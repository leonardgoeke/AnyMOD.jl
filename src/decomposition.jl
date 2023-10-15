
#region # * objects for decomposition

# ! struct for model results
mutable struct resData
	objVal::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	stLvl::Dict{Symbol,DataFrame}
	resData() = new(Inf,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}(),Dict{Symbol,DataFrame}())
end

# ! copy functions for model results
function copy(ben_obj::resData)
	out = resData()
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	out.stLvl = deepcopy(ben_obj.stLvl)
	return out
end

# structure managing stabilization method
mutable struct stabObj
	method::Array{Symbol,1} # array of method names used for stabilization
	methodOpt::Array{NamedTuple,1} # array of options for adjustment of stabilization parameters
	ruleSw::Union{NamedTuple{(), Tuple{}}, NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}}} # rule for switching between stabilization methods
	weight::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}} # weight of variables in stabilization
	actMet::Int # index of currently active stabilization method
	objVal::Float64 # array of objective value for current center
	dynPar::Array{Union{Dict,Float64},1} # array of dynamic parameters for each method
	var::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}} # variables subject to stabilization
	cns::ConstraintRef
	function stabObj(meth_tup::Tuple, ruleSw_ntup::NamedTuple,weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}},resData_obj::resData,lowBd_fl::Float64,top_m::anyModel)
		stab_obj = new()

		if !(isempty(ruleSw_ntup) || typeof(ruleSw_ntup) == NamedTuple{(:itr,:avgImp,:itrAvg),Tuple{Int64,Float64,Int64}})
			error("rule for switching stabilization method must be empty or have the fields 'itr', 'avgImp', and 'itrAvg'")
		end

		if !isempty(ruleSw_ntup) && ruleSw_ntup.itr < 2
			error("parameter 'itr' for  minimum iterations before switching stabilization method must be at least 2")
		end

		stab_obj.method, stab_obj.methodOpt, stab_obj.dynPar = writeStabOpt(meth_tup,lowBd_fl,resData_obj.objVal)
		
		# set other fields
		stab_obj.ruleSw = ruleSw_ntup
		stab_obj.weight = weight_ntup
		stab_obj.actMet = 1
		stab_obj.objVal = resData_obj.objVal
		stab_obj.var = filterStabVar(resData_obj.capa,resData_obj.stLvl,weight_ntup,top_m)
		
		# compute number of variables subject to stabilization
		stabCapa_arr = vcat(vcat(vcat(map(x -> stab_obj.var[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
		stLvl_arr = vcat(map(x -> stab_obj.var[:stLvl][x][!,:value],collect(keys(stab_obj.var[:stLvl])))...)
		stabExpr_arr = vcat(stabCapa_arr,stLvl_arr)

		return stab_obj, size(stabExpr_arr,1)
	end
end

# write options of stabilization method
function writeStabOpt(meth_tup::Tuple,lowBd_fl::Float64,upBd_fl::Float64)

	# set fields for name and options of method
	meth_arr = Symbol[]
	methOpt_arr = NamedTuple[]
	for (key,val) in meth_tup
		push!(meth_arr,key)
		push!(methOpt_arr,val)
		if key == :qtr && !isempty(setdiff(keys(val),(:start,:low,:thr,:fac)))
			error("options provided for trust-region do not match the defined options 'start', 'low', 'thr', and 'fac'")
		elseif key == :prx && !isempty(setdiff(keys(val),(:start,:min,:a)))
			error("options provided for proximal bundle do not match the defined options 'start', 'min', and 'a'")
		elseif key == :lvl && !isempty(setdiff(keys(val),(:lam,:myMax)))
			error("options provided for level bundle do not match the defined options 'lam', 'myMax'")
		elseif key == :box && !isempty(setdiff(keys(val),(:low,:up,:minUp)))
			error("options provided for trust-region do not match the defined options 'low', 'up', and 'minUp'")
		elseif key == :dsb && !isempty(setdiff(keys(val),(:start,:min,:lam,:myMax)))
			error("options provided for doubly stabilised bundle do not match the defined options 'start','min','lam', 'myMax'")
		end
	end

	if length(meth_arr) != length(unique(meth_arr)) error("stabilization methods must be unique") end

	# method specific adjustments (e.g. starting value for dynamic parameter, new variables for objective function)
	dynPar_arr = []
	for m in 1:size(meth_arr,1)
		if meth_arr[m] == :prx
			dynPar = Dict(:prx => methOpt_arr[m].start, :prxAux => methOpt_arr[m].start) # starting value for penalty
		elseif meth_arr[m] == :lvl
			dynPar = Dict(:yps => (1-methOpt_arr[m].lam)*(upBd_fl-lowBd_fl)/ top_m.options.scaFac.obj,:my => 0.0)
			if methOpt_arr[m].lam >= 1 || methOpt_arr[m].lam <= 0 
				error("lambda for level bundle must be strictly between 0 and 1")
			end
		elseif meth_arr[m] == :qtr
			dynPar = methOpt_arr[m].start # starting value for radius
		elseif meth_arr[m] == :box
			dynPar = 0.0 # dummy value since boxstep implementation does not have a dynamic parameter
		elseif meth_arr[m] == :dsb
			dynPar = Dict(:yps=>(1-methOpt_arr[m].lam)*(upBd_fl-lowBd_fl)/top_m.options.scaFac.obj,
			:prx => methOpt_arr[m].start,:my => 1.0)
		else
			error("unknown stabilization method provided, method must either be 'prx', 'lvl', 'qtr', or 'box'")
		end
		push!(dynPar_arr,dynPar)
	end
	
	return meth_arr, methOpt_arr, dynPar_arr
end

#endregion

#region # * heuristic solve and fix of capacities

# ! run heuristic and return exact capacities plus heuristic cut
function heuristicSolve(modOpt_tup::NamedTuple,redFac::Float64,t_int::Int,opt_obj::DataType;rtrnMod::Bool=true,solDet::Bool=false,fltSt::Bool=true)

	# create and solve model
	heu_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, objName = "heuristicModel_" * string(round(redFac,digits = 3)) * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 2, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, redStep = redFac, checkRng = (print = true, all = false), forceScr = solDet ? Symbol() : nothing)
	
	prepareMod!(heu_m,opt_obj,t_int)
	set_optimizer_attribute(heu_m.optModel, "Method", 2)
	set_optimizer_attribute(heu_m.optModel, "Crossover", 0)
	optimize!(heu_m.optModel)
	checkIIS(heu_m)

	# write results to benders object
	heuData_obj = resData()
	heuData_obj.objVal = sum(map(z -> sum(value.(heu_m.parts.cost.var[z][!,:var])), collect(filter(x -> any(occursin.(["costExp", "costOpr", "costMissCapa", "costRetro"],string(x))), keys(heu_m.parts.cost.var)))))
	heuData_obj.capa, ~ = writeResult(heu_m,[:capa,:exp,:mustCapa,:mustExp],fltSt = fltSt)
	
	if rtrnMod
		return heu_m, heuData_obj
	else
		return heuData_obj
	end
end

# ! evaluate results of heuristic solution to determine fixed and limited variables
function evaluateHeu(heu_m::anyModel,heuSca_obj::resData,heuCom_obj::resData,linPar_tup::NamedTuple,wrtCapa::Bool=false)

	# create empty dictionaries for limits and fixes
	fix_dic = Dict(:tech => Dict{Symbol,Dict{Symbol,DataFrame}}(),:exc => Dict{Symbol,Dict{Symbol,DataFrame}}())
	lim_dic = Dict(:tech => Dict{Symbol,Dict{Symbol,DataFrame}}(),:exc => Dict{Symbol,Dict{Symbol,DataFrame}}())
	cntHeu_arr = [0,0] # tracks number of variables fixed to zero, variable fixed to a non-zero value and limited variable

	# ! determine variables for fixing and limiting
	for sys in (:tech,:exc)
		part_dic = getfield(heu_m.parts,sys)
		for sSym in keys(heuSca_obj.capa[sys])
			fix_dic[sys][sSym] = Dict{Symbol,DataFrame}()
			lim_dic[sys][sSym] = Dict{Symbol,DataFrame}()

			relVar_arr = filter(x -> any(occursin.(part_dic[sSym].decomm == :none && !wrtCapa ? ["exp","mustCapa"] : ["capa","exp","mustCapa"],string(x))),collect(keys(part_dic[sSym].var)))
			relVarLim_arr = filter(x -> any(occursin.(part_dic[sSym].decomm == :none ? ["exp","mustCapa"] : ["capa","exp","mustCapa"],string(x))),collect(keys(part_dic[sSym].var)))

			for varSym in intersect(keys(heuSca_obj.capa[sys][sSym]),keys(heuCom_obj.capa[sys][sSym]),relVar_arr)
				must_boo = occursin("must",string(varSym))
				# match results from two different heuristic models
				bothCapa_df = rename(heuSca_obj.capa[sys][sSym][varSym],:value => :value_1) |> (x -> innerjoin(x, rename(heuCom_obj.capa[sys][sSym][varSym],:value => :value_2), on = intCol(x,:dir)))
				# determine cases for fix and limit
				bothCapa_df[!,:limVal], bothCapa_df[!,:limCns] = map(x -> getLinTrust(x.value_1,x.value_2,linPar_tup), eachrow(bothCapa_df)) |> (w -> map(x -> getindex.(w,x),[1,2]))
				bothCapa_df = flatten(select(bothCapa_df,Not([:value_1,:value_2])),[:limVal,:limCns])

				# ! store limited variables
				if varSym in relVarLim_arr
					lim_df = filter(x -> x.limCns != :Fix, bothCapa_df)
					if !isempty(lim_df)
						# removes storage variables controlled by ratio from further analysis
						if sys == :tech lim_df = removeFixStorage(varSym,lim_df,part_dic[sSym]) end
						if isempty(lim_df)
							continue
						else 
							lim_dic[sys][sSym][varSym] = lim_df
						end
						# reports on limited variables
						cntHeu_arr[2] = cntHeu_arr[2] + size(filter(x -> x.limCns == :Up,lim_df),1)
					end
				end
				
				# ! store fixed variables
				fix_df = select(filter(x -> x.limCns == :Fix, bothCapa_df),Not([:limCns]))
				if !isempty(fix_df)
					if sys == :tech fix_df = removeFixStorage(varSym,fix_df,part_dic[sSym]) end

					if isempty(fix_df)
						continue
					else 
						fix_dic[sys][sSym][varSym] = rename(fix_df,:limVal => :value)
					end
					
					# find related expansion variables and fix as well
					if !occursin("exp",lowercase(string(varSym)))
						for expVar in filter(x -> string(x) in replace.(string(varSym),must_boo ? ["Capa" => "Exp"] : ["capa" => "exp"]),keys(heuSca_obj.capa[sys][sSym]))
							# gets relevant expansion variables
							exp_df = heuSca_obj.capa[sys][sSym][expVar] |> (w -> innerjoin(w,select(part_dic[sSym].var[expVar],Not([:Ts_expSup,:var])), on = intCol(w)))
							# only fix expansion variables that relate to a fixed capacity
							relExp_df = unique(select(select(fix_df,Not(part_dic[sSym].decomm == :emerging ? [:limVal] : [:Ts_expSup,:limVal])) |> (w -> innerjoin(flatten(exp_df,:Ts_disSup),w, on = intCol(w))),Not([:Ts_disSup])))
							if !isempty(relExp_df) fix_dic[sys][sSym][expVar] = relExp_df end	
						end
					end
					# report on fixed variables
					cntHeu_arr[1] = cntHeu_arr[1] + size(fix_df,1)	
				end
			end
			# delete if nothing was written
			removeEmptyDic!(fix_dic[sys],sSym)
			removeEmptyDic!(lim_dic[sys],sSym)
		end
	end

	return fix_dic, lim_dic, cntHeu_arr
end

# ! returns a feasible solution as close as possible to the input dictionary
function getFeasResult(modOpt_tup::NamedTuple,fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},t_int::Int,zeroThrs_fl::Float64,opt_obj::DataType)

	# create top-problem
	topFeas_m = anyModel(modOpt_tup.inputDir,modOpt_tup.resultDir, objName = "feasModel" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 1, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, checkRng = (print = true, all = false), holdFixed = true)

	topFeas_m.subPro = tuple(0,0)
	prepareMod!(topFeas_m,opt_obj,t_int)

	# add limits to problem
	if !isempty(lim_dic) addLinearTrust!(topFeas_m,lim_dic) end

	# compute feasible capacites
	topFeas_m = computeFeas(topFeas_m,fix_dic,zeroThrs_fl,true);

    # return capacities and top problem (is sometimes used to compute costs of feasible solution afterward)
    return writeResult(topFeas_m,[:exp,:mustExp,:capa,:mustCapa]; fltSt = false) 
end

# ! runs top problem again with optimal results
function computeFeas(top_m::anyModel,var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},zeroThrs_fl::Float64;cutSmall_boo::Bool=false,wrtRes_boo::Bool=false)
	
	# create absolute value constraints for capacities or expansion variables
	for sys in (:tech,:exc)
		partTop_dic = getfield(top_m.parts,sys)
		for sSym in keys(var_dic[sys])
			part = partTop_dic[sSym]
			relVar_arr = filter(x -> any(occursin.(part.decomm == :none ? ["exp","mustCapa"] : ["capa","exp","mustCapa"],string(x))),collect(keys(var_dic[sys][sSym])))
			# create variables and writes constraints to minimize absolute value of capacity delta
			for varSym in relVar_arr
				var_df = part.var[varSym] |> (w -> occursin("exp",string(varSym)) ? collapseExp(w) : w)
				filter!(x -> !isempty(x.var.terms), var_df)
				if sys == :tech var_df = removeFixStorage(varSym,var_df,part) end # remove storage variables controlled by ratio
				if isempty(var_df) continue end
				# gets variable value and set variables below threshold to zero
				abs_df = deSelectSys(var_dic[sys][sSym][varSym]) |>  (z -> leftjoin(var_df,z,on = intersect(intCol(z,:dir),intCol(var_df,:dir)))) |> (y -> y[completecases(y),:])
				abs_df[!,:value] = map(x -> x.value < zeroThrs_fl ? 0.0 : x.value, eachrow(abs_df))
				# applies weights to make achieving zero values a priority 
				abs_df[!,:weight] .= 1.0
				# create variable for absolute value and connect with rest of dataframe again
				scaFac_fl = getfield(top_m.options.scaFac, occursin("exp",lowercase(string(varSym))) ? :insCapa : (occursin("StSize",string(varSym)) ? :capaStSize : :capa))
				part.var[Symbol(:abs,makeUp(varSym))] = createVar(select(abs_df,Not([:var,:value])), string(:abs,makeUp(varSym)),top_m.options.bound.capa,top_m.optModel, top_m.lock,top_m.sets,scaFac =scaFac_fl)
				abs_df[!,:varAbs] .= part.var[Symbol(:abs,makeUp(varSym))][!,:var] 
				# create constraints for absolute value
				abs_df[!,:absLow] = map(x -> x.varAbs - scaFac_fl * collect(keys(x.var.terms))[1] + x.value,eachrow(abs_df))
				abs_df[!,:absUp] = map(x -> x.varAbs  + scaFac_fl * collect(keys(x.var.terms))[1] - x.value,eachrow(abs_df)) 
				# scale and create absolute value constraints
				absLow_df = rename(orderDf(abs_df[!,[intCol(abs_df)...,:absLow]]),:absLow => :cnsExpr)
				absUp_df = rename(orderDf(abs_df[!,[intCol(abs_df)...,:absUp]]),:absUp => :cnsExpr)
				scaleCnsExpr!(absLow_df,top_m.options.coefRng,top_m.options.checkRng)
				scaleCnsExpr!(absUp_df,top_m.options.coefRng,top_m.options.checkRng)
				part.cns[Symbol(:absLow,makeUp(varSym))] = createCns(cnsCont(absLow_df,:greater),top_m.optModel,false)
				part.cns[Symbol(:absUp,makeUp(varSym))] = createCns(cnsCont(absUp_df,:greater),top_m.optModel,false)
				# create binary constraint to ensure zero values are either zero or above threshold
				if cutSmall_boo && 0.0 in abs_df[!,:value]
					cutSmall_df = rename(select(filter(x -> x.value == 0.0,abs_df),Not([:varAbs,:absLow,:absUp,:weight])),:var => :var_2)
					# create binary variable
					cutSmall_df = createVar(cutSmall_df, string("cutSmall",makeUp(varSym)),NaN,top_m.optModel, top_m.lock,top_m.sets,bi = true)
					# create constraints either enforcing zero or at least zero threshold
					cutSmall_df[!,:cutSmallZero] = map(x -> scaFac_fl * collect(keys(x.var_2.terms))[1] - (1 - x.var) * 10*zeroThrs_fl,eachrow(cutSmall_df))
					cutSmall_df[!,:cutSmallNonZero] = map(x -> scaFac_fl * collect(keys(x.var_2.terms))[1] - (1 - x.var) * zeroThrs_fl,eachrow(cutSmall_df))
					cutSmallZero_df = rename(orderDf(cutSmall_df[!,[intCol(cutSmall_df)...,:cutSmallZero]]),:cutSmallZero => :cnsExpr)
					cutSmallNonZero_df = rename(orderDf(cutSmall_df[!,[intCol(cutSmall_df)...,:cutSmallNonZero]]),:cutSmallNonZero => :cnsExpr)
					scaleCnsExpr!(cutSmallZero_df,top_m.options.coefRng,top_m.options.checkRng)
					scaleCnsExpr!(cutSmallNonZero_df,top_m.options.coefRng,top_m.options.checkRng)
					part.cns[Symbol(:cutSmallZero,makeUp(varSym))] = createCns(cnsCont(cutSmallZero_df,:smaller),top_m.optModel,false)
					part.cns[Symbol(:cutSmallNonZero,makeUp(varSym))] = createCns(cnsCont(cutSmallNonZero_df,:greater),top_m.optModel,false)
				end
			end
		end
	end
	
	# get sum of absolute values 
	absVar_arr = [:CapaConv,:CapaExc,:CapaStOut,:CapaStIn,:CapaStSize,:ExpConv,:ExpExc,:ExpStOut,:ExpStIn,:ExpStSize]
	absVal_expr = sum(map(x -> sum(getAllVariables(Symbol(:abs,x),top_m) |> (w -> isempty(w) ? [AffExpr()] : w[!,:var] .* w[!,:weight])),vcat(absVar_arr,Symbol.(:Must, absVar_arr))))
	# get sum of missing capacities and apply high weight 
	missCapa_expr = :missCapa in keys(top_m.parts.bal.var) ? sum(top_m.parts.bal.var[:missCapa][!,:var] ./ top_m.options.scaFac.insCapa .* 10) : AffExpr()
	objExpr_df = DataFrame(cnsExpr = [missCapa_expr + absVal_expr])
	scaleCnsExpr!(objExpr_df,top_m.options.coefRng,top_m.options.checkRng)
	
	# change objective of top problem to minimize absolute values and missing capacities
	@objective(top_m.optModel, Min, objExpr_df[1,:cnsExpr])
	# solve problem
	set_optimizer_attribute(top_m.optModel, "MIPGap", 0.001)
	set_optimizer_attribute(top_m.optModel, "SolutionLimit", 3600)
	
	optimize!(top_m.optModel)
	checkIIS(top_m)

	# write results into files (only used once optimum is obtained)
	if wrtRes_boo
		reportResults(:summary,top_m)
		reportResults(:cost,top_m)
		reportResults(:exchange,top_m)
	end

	return top_m
end

# ! find fixed variables and write to file
function writeFixToFiles(fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},feasFix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},temp_dir::String,res_m::anyModel; skipMustSt::Bool =false)
	rm(temp_dir; force = true, recursive = true)
	mkdir(temp_dir) # create directory for fixing files
	parFix_dic = defineParameter(res_m.options,res_m.report) # stores parameter info for fixing
	
	# loop over variables
	for sys in (:tech,:exc), sSym in keys(fix_dic[sys])
		hasMust_boo = sys == :tech && isdefined(res_m.parts.tech[sSym],:capaRestr) ? "must" in res_m.parts.tech[sSym].capaRestr[!,:cnstrType] : false
		for varSym in filter(x -> !skipMustSt || !hasMust_boo || x in (:capaConv, :expConv), keys(fix_dic[sys][sSym]))

			fix_df = feasFix_dic[sys][sSym][varSym] |> (w -> innerjoin(w,select(fix_dic[sys][sSym][varSym],Not([:value])), on = intersect(intCol(w,:dir),intCol(fix_dic[sys][sSym][varSym],:dir))))
			if isempty(fix_df) continue end
			# create file name
			par_sym = Symbol(varSym,:Fix)
			fileName_str = temp_dir * "/par_Fix" * string(makeUp(sys)) * "_" * string(sSym) * "_" * string(varSym)
			# set really small values due to remaining solver imprecisions to zero
			fix_df[!,:value] = map(x -> x < 1e-10 ? 0.0 : x,fix_df[!,:value])
			if occursin("capa",string(varSym)) && varSym in keys(getfield(res_m.parts,sys)[sSym].var) # adds residual capacities
				resVal_df = copy(getfield(res_m.parts,sys)[sSym].var[varSym])
				resVal_df[!,:resi] = map(x -> x.constant, resVal_df[!,:var])
				fix_df = joinMissing(fix_df,select(resVal_df,Not([:var])),intCol(fix_df,:dir),:left,Dict(:resi => 0.0))
				fix_df[!,:value] = fix_df[!,:value] .+ fix_df[!,:resi]
				select!(fix_df,Not([:resi]))
			end

			if varSym == :expExc && res_m.parts.exc[sSym].dir fix_df[!,:dir] .= true end
			# writes parameter file
			writeParameterFile!(res_m,fix_df,par_sym,parFix_dic[par_sym],fileName_str)
		end
	end
end

# ! get limits imposed on by linear trust region all limits are extended to avoid infeasbilites
function getLinTrust(val1_fl::Float64,val2_fl::Float64,linPar_tup::NamedTuple)

	if (val1_fl <= linPar_tup.thrsAbs && val2_fl <= linPar_tup.thrsAbs) || (any([val1_fl <= linPar_tup.thrsAbs,val2_fl <= linPar_tup.thrsAbs]) && abs(val1_fl - val2_fl) < linPar_tup.thrsAbs) # fix to zero, if both values are zero, or if one is zero and the other is very close to zero
		val_arr, cns_arr = [0.0], [:Fix]
	elseif val1_fl >= linPar_tup.thrsAbs && val2_fl <= linPar_tup.thrsAbs # set first value as upper limit, if other is zero
		val_arr, cns_arr = [val1_fl+linPar_tup.thrsAbs], [:Up]
	elseif val1_fl <= linPar_tup.thrsAbs && val2_fl >= linPar_tup.thrsAbs # set second value as upper limit, if other zero
		val_arr, cns_arr = [val2_fl+linPar_tup.thrsAbs], [:Up]
	elseif (abs(val1_fl/val2_fl-1) > linPar_tup.thrsRel) # enforce lower and upper limits, if difference does exceed threshold
		val_arr, cns_arr = sort([val1_fl,val2_fl]), [:Low,:Up]
		val_arr[2] = val_arr[2] + linPar_tup.thrsAbs
		if (val_arr[1] > linPar_tup.thrsAbs) 
			val_arr[1] = val_arr[1] - linPar_tup.thrsAbs
		else
			val_arr, cns_arr = [val_arr[2]], [:Up]
		end
	else 
		val_arr, cns_arr = [val1_fl], [:Fix] # set to mean, if difference does not exceed threshold
	end
		
	return val_arr, cns_arr
end

#endregion

#region # * basic benders algorithm

# ! set optimizer attributes and options, in case gurobi is used (recommended)
function prepareMod!(mod_m::anyModel,opt_obj::DataType, t_int::Int)
	# create optimization problem
	createOptModel!(mod_m)
	setObjective!(:cost,mod_m)
	# set optimizer and attributes
	set_optimizer(mod_m.optModel,opt_obj)
	set_optimizer_attribute(mod_m.optModel, "Threads", t_int)
	set_optimizer_attribute(mod_m.optModel, "QCPDual", 1)	
end

# ! run top-problem
function runTop(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},resData},stab_obj::Union{Nothing,stabObj},numFoc_int::Int,i::Int)

	resData_obj = resData()
	stabVar_obj = resData()

	# add cuts
	if !isempty(cutData_dic) addCuts!(top_m,cutData_dic,i) end
	# solve model
	@suppress begin
		set_optimizer_attribute(top_m.optModel, "Method", 2)
		set_optimizer_attribute(top_m.optModel, "Crossover", 0)
		set_optimizer_attribute(top_m.optModel, "NumericFocus", numFoc_int)
		optimize!(top_m.optModel)
	end
	
	# handle unsolved top problem
	if !isnothing(stab_obj)
		opt_tup = stab_obj.methodOpt[stab_obj.actMet]
		# if infeasible and level bundle stabilization, increase level until feasible
		while stab_obj.method[stab_obj.actMet] in (:lvl, :dsb) && termination_status(top_m.optModel) in (MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED)
			produceMessage(report_m.options,report_m.report, 1," - Empty level set", testErr = false, printErr = false)
			lowBd_fl = stab_obj.objVal/top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			stab_obj.dynPar[stab_obj.actMet][:yps] = (1-opt_tup.lam )* (stab_obj.objVal - lowBd_fl) / top_m.options.scaFac.obj
			ell_fl = stab_obj.objVal/top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			set_upper_bound(top_m.optModel[:r],ell_fl)
			optimize!(top_m.optModel)
		end

		# if no solution and proximal bundle stabilization, remove penalty term temporarily
		if stab_obj.method[stab_obj.actMet] == :prx && !(termination_status(top_m.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1])
			optimize!(top_m.optModel)
		end
	end
	checkIIS(top_m)

	# write technology capacites and level of capacity balance to benders object
	resData_obj.capa, resData_obj.stLvl = writeResult(top_m,[:capa,:mustCapa,:stLvl]; rmvFix = true)
	stabVar_obj.capa, stabVar_obj.stLvl = writeResult(top_m,[:capa,:exp,:stLvl]; rmvFix = true)

	# record level dual
	if !isnothing(stab_obj)
		if stab_obj.method[stab_obj.actMet] == :lvl
			levelDual_fl =  dual(UpperBoundRef(top_m.parts.obj.var[:obj][1,1]))
		elseif stab_obj.method[stab_obj.actMet] == :dsb
			levelDual_fl =  dual(UpperBoundRef(top_m.optModel[:r]))
		else
			levelDual_fl =  0.0
		end
	else
		level_dual_fl = 0.0
	end
	
	# get objective value of top problem
	topCost_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	estCost_fl = topCost_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])

	return resData_obj, stabVar_obj, topCost_fl, estCost_fl, levelDual_fl
end

# ! run sub-problem
function runSub(sub_m::anyModel,resData_obj::resData,sol_sym::Symbol,optTol_fl::Float64=1e-8,crs_boo::Bool=false,wrtRes_boo::Bool=false)

	# fixing capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(resData_obj.capa[sys])
			for capaSym in sort(filter(x -> occursin("capa",lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym]))),rev = true)
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], resData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(resData_obj.capa[sys][sSym][capaSym])
					delete!(resData_obj.capa[sys][sSym],capaSym)
				else
					resData_obj.capa[sys][sSym][capaSym] = limitVar!(resData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],sub_m)
				end
			end
			# remove system if no capacities exist
			removeEmptyDic!(resData_obj.capa[sys],sSym)
		end
	end

	# fixing storage levels
	if !isempty(resData_obj.stLvl)
		for sSym in keys(resData_obj.stLvl)
			if sSym in keys(sub_m.parts.tech)
				part_obj = sub_m.parts.tech[sSym]
				resData_obj.stLvl[sSym] = limitVar!(select(resData_obj.stLvl[sSym],Not([:scr])),select(part_obj.var[:stLvl],Not([:scr])),:stLvl,part_obj,sub_m)
				# remove system if no storage level exists
				removeEmptyDic!(resData_obj.stLvl,sSym)
			end
		end
	end

	# set optimizer attributes and solves
	@suppress begin
		if sol_sym == :barrier
			set_optimizer_attribute(sub_m.optModel, "Method", 2)
			set_optimizer_attribute(sub_m.optModel, "Crossover", crs_boo ? 1 : 0)
			set_optimizer_attribute(sub_m.optModel, "BarOrder", 1)
			set_optimizer_attribute(sub_m.optModel, "BarConvTol", optTol_fl)
		elseif sol_sym == :simplex
			set_optimizer_attribute(sub_m.optModel, "Method", 1)
			set_optimizer_attribute(sub_m.optModel, "Threads", 1)
			set_optimizer_attribute(sub_m.optModel, "OptimalityTol", optTol_fl)
			set_optimizer_attribute(sub_m.optModel, "Presolve", 2)
		end
		optimize!(sub_m.optModel)
	end
	checkIIS(sub_m)

	# write results into files (only used once optimum is obtained)
	if wrtRes_boo
		reportResults(:summary,sub_m)
		reportResults(:cost,sub_m)
		reportResults(:exchange,sub_m)
	end

	# get objective value
	scaObj_fl = sub_m.options.scaFac.obj
	resData_obj.objVal = value(sum(sub_m.parts.obj.var[:objVar][!,:var]))

	# get duals on capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(resData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa",lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym])))
				if Symbol(capaSym,:BendersFix) in keys(part_dic[sSym].cns)
					scaCapa_fl = getfield(sub_m.options.scaFac,occursin("StSize",string(capaSym)) ? :capaStSize : :capa)
					resData_obj.capa[sys][sSym][capaSym] = addDual(resData_obj.capa[sys][sSym][capaSym],part_dic[sSym].cns[Symbol(capaSym,:BendersFix)],scaObj_fl/scaCapa_fl)
					# remove capacity if none exists (again necessary because dual can be zero)
					removeEmptyDic!(resData_obj.capa[sys][sSym],capaSym)
				end
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			removeEmptyDic!(resData_obj.capa[sys],sSym)
		end
	end

	# get duals on storage levels
	if !isempty(resData_obj.stLvl)
		for sSym in keys(resData_obj.stLvl)
			if sSym in keys(sub_m.parts.tech)
				part_obj = sub_m.parts.tech[sSym]
				resData_obj.stLvl[sSym] = addDual(resData_obj.stLvl[sSym],part_obj.cns[:stLvlBendersFix],scaObj_fl/sub_m.options.scaFac.dispSt)
				removeEmptyDic!(resData_obj.stLvl,sSym)
			end
		end
	end

	return resData_obj
end

# ! add all cuts from input dictionary to top problem
function addCuts!(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},resData},i::Int)
	
	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_dis = Int[],scr = Int[], limCoef = Bool[], actItr = Int[], cnsExpr = AffExpr[])
	for sub in keys(cutData_dic)
		subCut = cutData_dic[sub]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		# compute cut element for each capacity
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(subCut.capa[sys]), capaSym in filter(x -> occursin("capa",lowercase(string(x))), collect(keys(subCut.capa[sys][sSym])))
				scaCapa_fl = getfield(top_m.options.scaFac,occursin("StSize",string(capaSym)) ? :capaStSize : :capa)
				push!(cutExpr_arr,getBendersCut(subCut.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],scaCapa_fl))
			end
		end

		# compute cut element for each storage level
		if !isempty(subCut.stLvl)
			for sSym in keys(subCut.stLvl)
				if sSym in keys(top_m.parts.tech)
					part_obj = top_m.parts.tech[sSym]
					push!(cutExpr_arr,getBendersCut(subCut.stLvl[sSym],part_obj.var[:stLvl],top_m.options.scaFac.dispSt))
				end
			end
		end
		
		# get cut variable and compute cut expression 
		cut_var = filter(x -> x.Ts_dis == sub[1] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var]
		cut_expr = @expression(top_m.optModel, subCut.objVal + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)))
		
		#region # * remove extremely small terms and limit the coefficient of extremely large terms
		limCoef_boo = false

		if typeof(cut_expr) == AffExpr && !isempty(cut_expr.terms)

			# ! ensure cut variable complies with limits on rhs
			cutFac_fl = abs(collect(values(cut_var.terms))[1]) # get scaling factor of cut variable
			scaRng_tup = top_m.options.coefRng.rhs ./ abs(cut_expr.constant) # get smallest and biggest scaling factors where rhs is still in range
				
			# adjust rhs to avoid violation of range only from cut variable and rhs
			if top_m.options.coefRng.mat[1]/cutFac_fl > scaRng_tup[2] 
				cut_expr.constant = top_m.options.coefRng.rhs[2] / (top_m.options.coefRng.mat[1]/cutFac_fl) # biggest rhs possible within range
				limCoef_boo = true
			elseif top_m.options.coefRng.mat[2]/cutFac_fl < scaRng_tup[1]
				cut_expr.constant = top_m.options.coefRng.rhs[1] / (top_m.options.coefRng.mat[2]/cutFac_fl) # smallest rhs possible within range
				limCoef_boo = true
			end
			
			# ! ensure factors remain within overall range
			maxRng_fl = top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1] # maximum range of coefficients
			facRng_fl = abs.(collect(values(cut_expr.terms))) |> (w -> (min(minimum(w),cutFac_fl),max(maximum(w),cutFac_fl))) # actual range of coefficients
			
			# manipulates factors to stay within range
			if maxRng_fl < facRng_fl[2]/facRng_fl[1]
				# compute maximum and minimum factors
				minFac_fl = facRng_fl[2]/maxRng_fl

				# removes small factors
				filter!(x -> abs(x[2]) > minFac_fl,cut_expr.terms)

				# check if the cut also would violate range, limit coefficients in this case
				if cutFac_fl < minFac_fl
					limCoef_boo = true
					maxCoef_fl = cutFac_fl * maxRng_fl # biggest possible coefficient, so cut variable is still in range
					foreach(x -> abs(cut_expr.terms[x]) > maxCoef_fl ? cut_expr.terms[x] = maxCoef_fl * sign(cut_expr.terms[x]) : nothing, collect(keys(cut_expr.terms))) # limits coefficients to maximum value
				end
			end

			# ! ensure scaling of factors does not move rhs out of range
			scaRng_tup = top_m.options.coefRng.rhs ./ abs(cut_expr.constant) # get smallest and biggest scaling factors where rhs is still in range

			for x in keys(cut_expr.terms)
				val_fl = abs(cut_expr.terms[x])
				if top_m.options.coefRng.mat[1]/val_fl > scaRng_tup[2] # factor requires more up-scaling than possible
					delete!(cut_expr.terms,x) # removes term
				elseif top_m.options.coefRng.mat[2]/val_fl < scaRng_tup[1] # factor requires more down-scaling than possible
					cut_expr.terms[x] = sign(cut_expr.terms[x]) * top_m.options.coefRng.mat[2]/scaRng_tup[1] # set to biggest factor possible within range
					limCoef_boo = true
				end
			end
		else # check if cut without variables can be scaled into range
			cutFac_fl = abs(collect(values(cut_var.terms))[1]) # get scaling factor of cut variable
			scaRng_tup = top_m.options.coefRng.rhs ./ cut_expr

			if top_m.options.coefRng.mat[1]/cutFac_fl > scaRng_tup[2] 
				cut_expr = top_m.options.coefRng.rhs[2] / (top_m.options.coefRng.mat[1]/cutFac_fl) # biggest rhs possible within range
				limCoef_boo = true
			elseif top_m.options.coefRng.mat[2]/cutFac_fl < scaRng_tup[1]
				cut_expr = top_m.options.coefRng.rhs[1] / (top_m.options.coefRng.mat[2]/cutFac_fl) # smallest rhs possible within range
				limCoef_boo = true
			end
		end

		#endregion

		# add benders variable to cut and push to dataframe of all cuts
		push!(cut_df,(i = i, Ts_dis = sub[1], scr = sub[2], limCoef = limCoef_boo, actItr = i, cnsExpr = cut_expr - cut_var))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df,top_m.options.coefRng,top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts] ,createCns(cnsCont(cut_df,:smaller),top_m.optModel,false))

end

#endregion

#region # * stabilization

# function to update the center of stabilization method
centerStab!(method::Symbol,stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel) = centerStab!(Val{method}(),stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)

# function for quadratic trust region
function centerStab!(method::Val{:qtr},stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)

	# match values with variables in model
	allVar_df = getStabDf(stab_obj,top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (2*maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*x.value*x.scaFac < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# absolute value for rhs of equation (sets default value of 15 to avoid zero radius if values are very small)
	abs_fl = sum(allVar_df[!,:value] .* sqrt.(allVar_df[!,:scaFac]) ) |> (x -> x < 0.01 * size(allVar_df,1) ? sum(allVar_df[!,:scaFac]) : x)
	
	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ abs((abs_fl * stab_obj.dynPar[stab_obj.actMet])^2 - sum(allVar_df[!,:scaFac] .* allVar_df[!,:value].^2))

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaEq_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# skips creation of trust-region, if rhs would substanitally violate rhs range
	rhs_fl = ((abs_fl * stab_obj.dynPar[stab_obj.actMet])^2 - capaSum_expr.aff.constant)  * scaEq_fl
 
	# create final constraint
	if top_m.options.coefRng.rhs[1] / addVio_fl < abs(rhs_fl) && top_m.options.coefRng.rhs[2] * addVio_fl > abs(rhs_fl)
		stab_obj.cns = @constraint(top_m.optModel,  capaSum_expr * scaEq_fl <= (abs_fl * stab_obj.dynPar[stab_obj.actMet])^2  * scaEq_fl)
	else
		if top_m.options.coefRng.rhs[2] * addVio_fl < abs(rhs_fl)
			stab_obj.cns = @constraint(top_m.optModel,  capaSum_expr * scaEq_fl <= top_m.options.coefRng.rhs[2]* addVio_fl + capaSum_expr.aff.constant * scaEq_fl)
		else 
			stab_obj.cns = @constraint(top_m.optModel,  capaSum_expr * scaEq_fl <= top_m.options.coefRng.rhs[1]* addVio_fl + capaSum_expr.aff.constant * scaEq_fl)
		end
		produceMessage(report_m.options,report_m.report, 1," - Adjusted radius of stabilization to prevent numerical problems", testErr = false, printErr = false)
	end
end

# function for proximal bundle method
function centerStab!(method::Val{:prx},stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj,top_m)

	pen_fl = stab_obj.dynPar[stab_obj.actMet][:prx]
	
	# sets values of variables that will violate range to zero
	minFac_fl = (2*pen_fl*maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*pen_fl*x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# adjust objective function
	@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] + 1/(2*pen_fl) * capaSum_expr  * scaFac_fl)

end

# function for level bundle method
function centerStab!(method::Val{:lvl},stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj,top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps] 

	# adjust objective function and level set
	@objective(top_m.optModel, Min, 0.5 * capaSum_expr  * scaFac_fl)
	set_upper_bound(top_m.parts.obj.var[:obj][1,1],ell_fl)
end

# function for box step method
function centerStab!(method::Val{:box},stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var,stab_obj.weightSt,top_m)
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value,:scaFac]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	allStLvl_df = vcat(map(x -> expExpr_dic[:stLvl][x],collect(keys(expExpr_dic[:stLvl])))...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)
	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df,allStLvl_df))

	# set lower and upper bound
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_lower_bound(z[1], x.value*(1-stab_obj.methodOpt[stab_obj.actMet].low) |> (y -> y < top_m.options.coefRng.rhs[1]/1e2 ? 0.0 : y))), eachrow(allVar_df))
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_upper_bound(z[1], max(stab_obj.methodOpt[stab_obj.actMet].minUp/z[2], x.value*(1+stab_obj.methodOpt[stab_obj.actMet].up)))), eachrow(allVar_df))

end

# function for doubly stabilised bundle method
function centerStab!(method::Val{:dsb},stab_obj::stabObj,addVio_fl::Float64,top_m::anyModel,report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj,top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]

	# compute penalty multiplier
	pen_fl = stab_obj.dynPar[stab_obj.actMet][:prx]

	# adjust objective function and level set
	@variable(top_m.optModel,r)
	@objective(top_m.optModel, Min, r + (1/2*pen_fl) * capaSum_expr  * scaFac_fl)
	@constraint(top_m.optModel,r_con,top_m.parts.obj.var[:obj][1,1] <= r)
	set_upper_bound(r,ell_fl)
end

# ! compute scaled l2 norm
function computeL2Norm(allVar_df::DataFrame,stab_obj::stabObj,scaRng_tup::Tuple,top_m::anyModel)

	# set values of variable to zero or biggest value possible without scaling violating rhs range
	for x in eachrow(allVar_df)	
		if top_m.options.coefRng.mat[1]/(x.value*x.scaFac*2) > scaRng_tup[2] # factor requires more up-scaling than possible
			x[:value] = 0 # set value to zero
		elseif top_m.options.coefRng.mat[2]/(x.value*x.scaFac*2) < scaRng_tup[1] # factor requires more down-scaling than possible
			x[:value] = top_m.options.coefRng.mat[2]/(scaRng_tup[1]*2) # set to biggest value possible within range
		end
	end

	# computes left hand side expression and scaling factor
	capaSum_expr = sum(map(x -> sum(collect(keys(x.var.terms))) |> (z -> x.scaFac * (z^2 - 2*x.value*z + x.value^2)),eachrow(allVar_df)))
	scaEq_fl = top_m.options.coefRng.mat[1]/minimum(abs.(vcat(collect(values(capaSum_expr.terms)),collect(values(capaSum_expr.aff.terms))) |> (z -> isempty(z) ? [1.0] : z)))

	return capaSum_expr, scaEq_fl
end

# ! update dynamic parameter of stabilization method
function adjustDynPar!(stab_obj::stabObj,top_m::anyModel,iUpd_int::Int,adjCtr_boo::Bool,adjCtr_count::Int,cntNull_int::Int,levelDual_fl::Float64,estCostNoStab_fl::Float64,estCost_fl::Float64,currentBest_fl::Float64,currentCost_fl::Float64,nearOpt_boo::Bool,report_m::anyModel)

	opt_tup = stab_obj.methodOpt[iUpd_int]
	if stab_obj.method[iUpd_int] == :qtr # adjust radius of quadratic trust-region
		if nearOpt_boo ? !adjCtr_boo : abs(1 - estCostNoStab_fl / estCost_fl) < opt_tup.thr && stab_obj.dynPar[iUpd_int] > opt_tup.low
			stab_obj.dynPar[iUpd_int] = max(opt_tup.low,stab_obj.dynPar[iUpd_int] / opt_tup.fac)
			produceMessage(report_m.options,report_m.report, 1," - Reduced quadratic trust-region!", testErr = false, printErr = false)	
		end
	elseif stab_obj.method[iUpd_int] == :prx # adjust penalty term of proximal term, implementation according to doi.org/10.1007/s10107-015-0873-6, section 5.1.2, only method 1 so far
		# compute τ_aux
		aux_fl =  (stab_obj.objVal - currentCost_fl)/(stab_obj.objVal - estCost_fl)
		#aux_fl = opt_tup.meth== "PBM-1" ? (stab_obj.objVal - currentCost_fl)/(stab_obj.objVal - estCost_fl) : 0
		stab_obj.dynPar[iUpd_int][:prxAux] = 2 * stab_obj.dynPar[iUpd_int][:prx] * (1+aux_fl)
		# check if serious step
		if adjCtr_boo
			# adjust τ_aux, if last 5 steps have been serious
			if adjCtr_count > 5
				stab_obj.dynPar[iUpd_int][:prxAux] = opt_tup.a * stab_obj.dynPar[iUpd_int][:prxAux]
			end
			# update proximal term
			stab_obj.dynPar[iUpd_int][:prx] = min(stab_obj.dynPar[iUpd_int][:prxAux],10 * stab_obj.dynPar[iUpd_int][:prx])
		else # if null-step
			if cntNull_int > 10
				stab_obj.dynPar[iUpd_int][:prx] = (opt_tup.a) * stab_obj.dynPar[iUpd_int][:prx]
			end
			stab_obj.dynPar[iUpd_int][:prx] = min(stab_obj.dynPar[iUpd_int][:prx],max(stab_obj.dynPar[iUpd_int][:prxAux],stab_obj.dynPar[iUpd_int][:prx]/opt_tup.a,opt_tup.min))
		end
	elseif stab_obj.method[iUpd_int] == :lvl # adjust level, implementation according to doi.org/10.1007/s10107-015-0873-6 
		stab_obj.dynPar[iUpd_int][:my] = 1-levelDual_fl
		if adjCtr_boo
			stab_obj.dynPar[iUpd_int][:yps] = min(stab_obj.dynPar[iUpd_int][:yps],(1-opt_tup.lam)*(currentBest_fl - estCostNoStab_fl) / top_m.options.scaFac.obj)
		else
			if stab_obj.dynPar[iUpd_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[iUpd_int][:yps] = opt_tup.lam*stab_obj.dynPar[iUpd_int][:yps]
			end
		end
	elseif stab_obj.method[iUpd_int] == :dsb # adjust doubly stabilised method, implementation according to doi.org/10.1007/s10107-015-0873-6
		stab_obj.dynPar[iUpd_int][:my] = 1-levelDual_fl
		if adjCtr_boo
			stab_obj.dynPar[iUpd_int][:prx] = (1+(1/1000)*(stab_obj.dynPar[iUpd_int][:my]-1))*stab_obj.dynPar[iUpd_int][:prx] # added a fixed scaler for the dual variable to avoid extremely large values for prx
			stab_obj.dynPar[iUpd_int][:yps] = min(stab_obj.dynPar[iUpd_int][:yps],(1-opt_tup.lam)*(currentBest_fl- estCostNoStab_fl) / top_m.options.scaFac.obj)
		else
			newPrx_fl = stab_obj.dynPar[iUpd_int][:prx]*(stab_obj.dynPar[iUpd_int][:yps]/((currentBest_fl - estCostNoStab_fl)/top_m.options.scaFac.obj))
			stab_obj.dynPar[iUpd_int][:prx] = max(opt_tup.min,newPrx_fl)
			if stab_obj.dynPar[iUpd_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[iUpd_int][:yps] = opt_tup.lam*stab_obj.dynPar[iUpd_int][:yps]
			end
		end
	end

end
 
# filter variables used for stabilization
function filterStabVar(capa_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},stLvl_dic::Dict{Symbol,DataFrame},weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}},top_m::anyModel)

	var_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()

	# write capacity values
	var_dic[:capa] = Dict(x => Dict{Symbol,Dict{Symbol,DataFrame}}() for x in [:tech,:exc])

	for sys in (:exc,:tech)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(capa_dic[sys])
			var_dic[:capa][sys][sSym] = Dict{Symbol,Dict{Symbol,DataFrame}}() # create empty dataframe for values
			
			# determine where using expansion rather than capacity is possible and more efficient
			varNum_dic = Dict(x => size(unique(getfield.(part_dic[sSym].var[x][!,:var],:terms)),1) for x in collect(keys(part_dic[sSym].var))) # number of unique variables
			trstVar_arr = map(filter(x -> occursin("capa",string(x)),collect(keys(part_dic[sSym].var)))) do x
				expVar_sym = Symbol(replace(string(x),"capa" => "exp"))
				return part_dic[sSym].decomm == :none && expVar_sym in keys(varNum_dic) && varNum_dic[expVar_sym] <= varNum_dic[x] ? expVar_sym : x
			end

			# filter capacities with weight of zero
			if weight_ntup.capa == 0.0 filter!(x -> (x in (:expStSize,:capaStSize)),trstVar_arr) end
			if weight_ntup.capaStSize == 0.0 filter!(x -> !(x in (:expStSize,:capaStSize)),trstVar_arr) end

			for trstSym in intersect(keys(capa_dic[sys][sSym]),trstVar_arr)
				var_df = capa_dic[sys][sSym][trstSym]
				if trstSym == :capaExc && !part_dic[sSym].dir filter!(x -> x.R_from < x.R_to,var_df) end # only get relevant capacity variables of exchange
				if sys == :tech var_df = removeFixStorage(trstSym,var_df,part_dic[sSym]) end # remove storage variables controlled by ratio
				# filter cases where actual variables are defined
				var_dic[:capa][sys][sSym][trstSym] = intCol(var_df) |> (w ->innerjoin(var_df,unique(select(filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym]),w)), on = w))
				# remove if no capacities remain
				removeEmptyDic!(var_dic[:capa][sys][sSym],trstSym)
			end
			
			# remove entire system if no capacities
			removeEmptyDic!(var_dic[:capa][sys],sSym)
		end
	end

	# write storage values
	var_dic[:stLvl] = Dict{Symbol,DataFrame}()

	if !isempty(stLvl_dic) && weight_ntup.stLvl != 0.0
		for sSym in keys(stLvl_dic)
			if sSym in keys(top_m.parts.tech)
				part_obj = top_m.parts.tech[sSym]
				var_df = stLvl_dic[sSym]
				var_dic[:stLvl][sSym] = intCol(var_df) |> (w ->innerjoin(var_df,unique(select(filter(x -> !isempty(x.var.terms), part_obj.var[:stLvl]),w)), on = w))
			end
		end
	end

	return var_dic
end

# ! solves top problem without trust region and obtains lower limits
function runTopWithoutStab(top_m::anyModel,stab_obj::stabObj)
	
	if stab_obj.method[stab_obj.actMet] == :qtr
		delete(top_m.optModel,stab_obj.cns) # remove trust-region
	elseif stab_obj.method[stab_obj.actMet] == :prx
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1]) # remove penalty form objective
	elseif stab_obj.method[stab_obj.actMet] == :lvl
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1])
		delete_upper_bound(top_m.parts.obj.var[:obj][1,1])
	elseif stab_obj.method[stab_obj.actMet] == :dsb
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1])
		delete(top_m.optModel,top_m.optModel[:r_con])
		unregister(top_m.optModel,:r_con)
		delete(top_m.optModel,top_m.optModel[:r])
		unregister(top_m.optModel,:r)
	elseif stab_obj.method[stab_obj.actMet] == :box
		stabVar_dic = matchValWithVar(stab_obj.var,stabObj.weightSt,top_m)
		for sys in keys(stabVar_dic), sSym in keys(stabVar_dic[sys]), capaSym in keys(stabVar_dic[sys][sSym])
			relVar_arr = map(x -> collect(x.terms)[1][1], stabVar_dic[sys][sSym][capaSym][!,:var])
			delete_lower_bound.(relVar_arr)
			set_lower_bound.(relVar_arr,0.0)
			delete_upper_bound.(relVar_arr)
		end
	end
	
	set_optimizer_attribute(top_m.optModel, "Method", 0)
	set_optimizer_attribute(top_m.optModel, "NumericFocus", 0)
	optimize!(top_m.optModel)

	# obtain different objective values
	topCost_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var])) # costs of unconstrained top-problem
	estCost_fl = topCost_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var]) # objective (incl. benders) of unconstrained top-problem

	return topCost_fl, estCost_fl
end

#endregion

#region # * near-optimal

# ! adapt top-problem for the computation of near-optimal solutions
function adaptNearOpt!(top_m::anyModel,nearOpt_ntup::NamedTuple,costOpt_fl::Float64,nOpt_int::Int)
	
	obj_arr = Pair[]
	for obj in nearOpt_ntup.obj[nOpt_int][2][2]
		# build filter function
		flt_tup = obj[2]
		te_boo = !(flt_tup.variable in (:capaExc,:expExc))
		exp_boo = flt_tup.variable in (:expConv,:expStIn,:expStOut,:expStSize,:expExc)
		flt_func = x -> (:system in keys(flt_tup) ? ((te_boo ? x.Te : x.Exc) in getDescFromName(flt_tup.system,top_m.sets[(te_boo ? :Te : :Exc)])) : true) && (:region in keys(flt_tup) ? (x.R_exp in getDescFromName(flt_tup.region,top_m.sets[:R])) : true) && (:region_from in keys(flt_tup) ? (x.R_from in getDescFromName(flt_tup.region_from,top_m.sets[:R])) : true) && (:region_to in keys(flt_tup) ? (x.R_to in getDescFromName(flt_tup.region_to,top_m.sets[:R])) : true) && (:timestep in keys(flt_tup) ? ((exp_boo ? x.Ts_exp : x.Ts_expSup) in getDescFromName(flt_tup.timestep,top_m.sets[:Ts])) : true)
		# write description of objective
		push!(obj_arr,(flt_tup.variable => (fac = obj[1],flt = flt_func)))
	end
	# change objective according to near-optimal
	objFunc_tup = tuple(vcat([:cost => (fac = 0.0,flt = x -> true)], obj_arr)...)
	@suppress setObjective!(objFunc_tup,top_m,nearOpt_ntup.obj[nOpt_int][2][1] == :min)
	
	# delete old restriction to near optimum
	if :nearOpt in keys(top_m.parts.obj.cns) delete(top_m.optModel,top_m.parts.obj.cns[:nearOpt][1,:cns]) end
	
	# restrict system costs to near-optimum
	cost_expr = sum(filter(x -> x.name in (:cost,:benders), top_m.parts.obj.var[:objVar])[!,:var])
	nearOpt_eqn = @constraint(top_m.optModel, costOpt_fl * (1 + nearOpt_ntup.optThres)  >= cost_expr)
	top_m.parts.obj.cns[:nearOpt] = DataFrame(cns = nearOpt_eqn)
end

# ! get capacity results for near optimal analysis
function getCapaResult(anyM::anyModel)

	# get capacities from summary file
	sum_df = rename(reportResults(:summary,anyM,rtnOpt = (:csvDf,)),:region_dispatch => :region, :technology => :system)
	sum_df = filter(x -> x.variable in (:capaStOut,:capaStIn,:capaStSize,:capaConv), sum_df)
	select!(sum_df, setdiff(namesSym(sum_df),[:scenario,:carrier,:objName]))

	# get capacity from exchange file
	exc_df = rename(filter(x -> x.variable == :capaExc, reportResults(:exchange,anyM,rtnOpt = (:csvDf,))), :exchange => :system)
	exc_df[!,:region] = string.(exc_df[!,:region_from]) .* " - " .* string.(exc_df[!,:region_to])
	exc_df[!,:id] .= ""
	select!(exc_df,setdiff(namesSym(exc_df),[:timestep_superordinate_expansion,:region_from,:region_to,:scenario,:directed,:carrier,:objName]))

	return rename(vcat(sum_df,exc_df),:timestep_superordinate_dispatch => :timestep,:variable => :capacity_variable,:value => :capacity_value)
end

#endregion

#region # * other refinements

# ! delete cuts that have not been binding for a while
function deleteCuts!(top_m::anyModel,delCut_int::Int,i::Int)
	if delCut_int < Inf
		# tracking latest binding iteration for cuts
		top_m.parts.obj.cns[:bendersCuts][!,:actItr] .= map(x -> abs(value(x.cns) / normalized_rhs(x.cns) - 1) < 1e-3 ? i : x.actItr, eachrow(top_m.parts.obj.cns[:bendersCuts]))
		# delete cuts that were not binding long enough
		delete.(top_m.optModel, filter(x -> x.actItr + delCut_int < i,top_m.parts.obj.cns[:bendersCuts])[!,:cns])
		filter!(x -> (x.actItr + delCut_int > i),top_m.parts.obj.cns[:bendersCuts])
	end
end

# ! computes convergence tolerance for subproblems
function getConvTol(gapCur_fl::Float64,gapEnd_fl::Float64,conSub_tup::NamedTuple{(:rng, :int, :crs), Tuple{Vector{Float64}, Symbol, Bool}})

	if conSub_tup.int == :lin
		m = (conSub_tup.rng[1] - conSub_tup.rng[2])/(1-gapEnd_fl)
		b = conSub_tup.rng[1] - m
		return b + m * gapCur_fl
	elseif conSub_tup.int == :exp
		m = log(conSub_tup.rng[1]/conSub_tup.rng[2])/(1-gapEnd_fl)
		b = log(conSub_tup.rng[1]) - m
		return exp(b + m * gapCur_fl)
	elseif conSub_tup.int == :log
		b = conSub_tup.rng[1]
		m = (conSub_tup.rng[2] - b ) / log(gapEnd_fl)
		return b + m * log(gapCur_fl)
	end
end

#endregion

#region # * data management

# merge all entries of dictionary used for capacity data into one dataframe for the specified columns
mergeVar(var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},outCol::Array{Symbol,1}) = vcat(vcat(vcat(map(x -> var_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,outCol],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)

# get dataframe with variables, values, and scaling factors for stabilization
function getStabDf(stab_obj::stabObj,top_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var,stab_obj.weight,top_m)
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value,:scaFac]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	allStLvl_df = vcat(map(x -> expExpr_dic[:stLvl][x],collect(keys(expExpr_dic[:stLvl])))...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)
	
	# filter zero scaling factors
	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df,allStLvl_df))
	
	# normalize scaling factors
	allVar_df[!,:scaFac] .= allVar_df[!,:scaFac] ./ minimum(allVar_df[!,:scaFac])

	return allVar_df
end

# ! matches values in dictionary with variables of provided problem
function matchValWithVar(var_dic::Dict{Symbol, Union{Dict{Symbol, Dict{Symbol, Dict{Symbol, DataFrame}}}, Dict{Symbol, DataFrame}}},weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}},mod_m::anyModel,prsvExp::Bool=false)
	
	expExpr_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()
	
	# match capacity values with variables
	expExpr_dic[:capa] = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		expExpr_dic[:capa][sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(mod_m.parts,sys)
		for sSym in keys(var_dic[:capa][sys])
			expExpr_dic[:capa][sys][sSym] = Dict{Symbol,DataFrame}()
			for varSym in keys(var_dic[:capa][sys][sSym])
				val_df = deSelectSys(var_dic[:capa][sys][sSym][varSym])
				# get scaling factor for variables (corrects scaling within model going back to standard units and apply weights defined for stabilization)
				modSca_fl =  getfield(mod_m.options.scaFac, occursin("exp",string(varSym)) ? :insCapa : (occursin("StSize",string(varSym)) ? :capaStSize : :capa))
				wgtSca_fl =  getfield(weight_ntup, occursin("exp",string(varSym)) ? :capa : (occursin("StSize",string(varSym)) ? :capaStSize : :capa))			
				val_df[!,:value] = val_df[!,:value] ./ modSca_fl # correct value with scaling factor for variable
				# join variables and values
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp,:var,:value] : [:Ts_disSup,:var,:value]) : [:var,:value]
				join_df = unique(select(innerjoin(deSelectSys(part_dic[sSym].var[varSym]),val_df, on = intCol(val_df,:dir)),sel_arr))			
				join_df[!,:scaFac] .=  wgtSca_fl^2	
				expExpr_dic[:capa][sys][sSym][varSym] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df,:Ts_exp => :Ts_disSup) : join_df
			end
		end
	end

	# match storage level values with variables
	expExpr_dic[:stLvl] = Dict{Symbol,DataFrame}()

	if !isempty(var_dic[:stLvl])
		for sSym in keys(var_dic[:stLvl])
			if sSym in keys(mod_m.parts.tech)
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp,:var,:value] : [:Ts_disSup,:var,:value]) : [:var,:value]
				val_df = var_dic[:stLvl][sSym] 
				val_df[!,:value] = val_df[!,:value] ./ mod_m.options.scaFac.dispSt
				join_df = select(innerjoin(val_df,mod_m.parts.tech[sSym].var[:stLvl], on  = intCol(var_dic[:stLvl][sSym])),sel_arr)
				join_df[!,:scaFac] .= weight_ntup.stLvl^2
				expExpr_dic[:stLvl][sSym] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df,:Ts_exp => :Ts_disSup) : join_df
			end
		end
	end

	return expExpr_dic
end

# ! write values of entire variables in input model to returned capacity dictionary
function writeResult(in_m::anyModel, var_arr::Array{Symbol,1};rmvFix::Bool = false, fltSt::Bool = true)
	
	# write expansion value
	capa_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	
	for sys in (:tech,:exc)
		capa_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts,sys)
		for sSym in filter(x -> part_dic[x].type in (:stock,:mature,:emerging),keys(part_dic))
			
			# continue in case of technology without changing capacites
			if part_dic[sSym].type == :stock &&  part_dic[sSym].decomm == :none continue end	

			varSym_arr = filter(x -> any(occursin.(string.(var_arr),string(x))), keys(part_dic[sSym].var))

			# get relevant capcities filtering fixed ones in case option is active
			capa_dic[sys][sSym] = Dict{Symbol, DataFrame}()
			for varSym in varSym_arr
				relVar_df = filter(x -> !isempty(x.var.terms), copy(part_dic[sSym].var[varSym]))
				if isempty(relVar_df) continue end
				capa_dic[sys][sSym][varSym] = getResult(relVar_df)
			end
			
			# check if storage expansion is fixed to storage output and removes variables in these cases
			if sys == :tech && fltSt
				for stVar in collect(keys(capa_dic[sys][sSym]))
					capa_dic[sys][sSym][stVar] = removeFixStorage(stVar,capa_dic[sys][sSym][stVar],part_dic[sSym])
					if isempty(capa_dic[sys][sSym][stVar]) delete!(capa_dic[sys][sSym],stVar) end
				end
			end

			# removes variables that are fixed from output
			if rmvFix
				for varSym in varSym_arr
					must_boo = occursin("must",string(varSym))
					fixVar_sym = part_dic[sSym].decomm == :none ? Symbol(replace(string(varSym),must_boo ? "Capa" => "Exp" : "capa" => "exp")) : varSym
					if Symbol(fixVar_sym,"BendersFix") in collect(keys(part_dic[sSym].cns))
						var_df = capa_dic[sys][sSym][varSym]
						expCns_df = select(part_dic[sSym].cns[Symbol(fixVar_sym,"BendersFix")],Not([:cns,:fac,]))
						# in case of no decommissioning capacites are fixed if all corresponding expansion variables are fixed
						if part_dic[sSym].decomm == :none	
							# get non-fixed cases for expansion variables
							expCnsY_df = antijoin(select(part_dic[sSym].var[fixVar_sym],Not([:var])),expCns_df, on = intCol(expCns_df))
							if !isempty(expCnsY_df)
								expCnsY_df = select(flatten(expCnsY_df,:Ts_disSup),Not(:Ts_exp))
								join_arr = part_dic[sSym].type == :emerging ? intCol(var_df) : filter(x -> x != :Ts_expSup,intCol(var_df))
								capa_dic[sys][sSym][varSym] = innerjoin(var_df,unique(select(expCnsY_df,join_arr)), on = join_arr)
							else
								capa_dic[sys][sSym][varSym] = DataFrame()
							end
						else
							capa_dic[sys][sSym][varSym] = innerjoin(var_df,expCns_df, on = intCol(var_df))
						end
						
					end
				end
			end

			# removes redundant varibles for undirected exchange capacity
			if sys == :exc && !part_dic[sSym].dir && :capaExc in keys(capa_dic[sys][sSym])
				filter!(x -> x.R_from < x.R_to,capa_dic[sys][sSym][:capaExc])
			end

			# remove empty fields
			filter!(x -> !isempty(x[2]),capa_dic[sys][sSym])
			removeEmptyDic!(capa_dic[sys],sSym)	
		end
	end

	# write storage levels in case of reduced foresight
	stLvl_dic = Dict{Symbol,DataFrame}()

	if :stLvl in var_arr && in_m.options.lvlFrs != 0
		for sSym in keys(in_m.parts.tech)
			if :stLvl in keys(in_m.parts.tech[sSym].var)
				stLvl_dic[sSym] = getResult(copy(in_m.parts.tech[sSym].var[:stLvl]))	
				removeEmptyDic!(stLvl_dic,sSym)
			end
		end
	end

	return capa_dic, stLvl_dic
end

# ! replaces the variable column with a column storing the value of the entire variable
function getResult(res_df::DataFrame)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		# aggregates expansion, if spread across different years
		res_df = combine(groupby(res_df,filter(x -> x != :Ts_expSup, intCol(res_df))), :var => (x -> x[1] * size(x,1)) => :var)
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms),res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> round(max(0,value(x) - x.constant), digits = 12),res_df[!,:var])

	return select(res_df,Not([:var]))
end

# ! create constraint fixing capacity (or setting a lower limits)
function limitVar!(value_df::DataFrame,var_df::DataFrame,var_sym::Symbol,part_obj::AbstractModelPart,fix_m::anyModel,lim_sym::Symbol=:Fix)

	# compute smallest and biggest capacity that can be enforced
	rngMat_tup = fix_m.options.coefRng.mat
	rngRhs_tup = fix_m.options.coefRng.rhs

	cns_sym = Symbol(:Benders,lim_sym)

	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(var_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])

	# correct values with scaling factor
	scaFac_sym = lowercase(string(var_sym)) |> (z -> occursin("stlvl",z) ? :dispSt : (occursin("exp",z) ? :insCapa : occursin("stsize",string(z)) ? :capaStSize : :capa))
	fix_df[!,:value]  = fix_df[!,:value] ./ getfield(fix_m.options.scaFac,scaFac_sym)
	
	# filter cases where no variable exists
	filter!(x -> !isempty(x.var.terms),fix_df)
	if isempty(fix_df) return value_df end

	# extract actual variable
	fix_df[!,:var] = map(x -> collect(keys(x.var.terms))[1], eachrow(fix_df))

	# find cases where capa cannot be set to zero, because it is linked to a non-zero mustCapa
	if occursin("capa",string(var_sym)) && Symbol(replace(string(var_sym),"capa" => "mustCapa"),"BendersFix") in keys(part_obj.cns)
		nonZero_df = part_obj.cns[Symbol(replace(string(var_sym),"capa" => "mustCapa"),"BendersFix")]
		nonZero_df = filter(x -> normalized_rhs(x.cns) != 0.0, nonZero_df)
		nonZero_df[!,:setZero] .= false
		fix_df = joinMissing(fix_df,select(nonZero_df,Not([:cns,:fac])), intCol(fix_df), :left,Dict(:setZero => :true))
	else
		fix_df[!,:setZero] .= true
	end

	# comptue factor and rhs, values below enforceable range are set to zero, values are above are set to largest value possible
	fix_df[!,:fac] = map(x -> x.value < rngRhs_tup[1] ? rngRhs_tup[1]/x.value : (x.value > rngRhs_tup[2] ? rngRhs_tup[2]/x.value : 1.0), eachrow(fix_df))
	fix_df[!,:rhs], fix_df[!,:fac] = map(x -> x.fac < rngMat_tup[1] ?  [rngRhs_tup[2],rngMat_tup[1]] : (x.fac > rngMat_tup[2] && x.setZero ? [0.0,1.0] : [x.value*x.fac,x.fac]) ,eachrow(fix_df)) |> (w  -> map(x -> getindex.(w,x),[1,2]))

	if !(Symbol(var_sym,cns_sym) in keys(part_obj.cns))
		# create actual constraint and attach to model part
		if lim_sym == :Fix
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac == x.rhs),eachrow(fix_df))
		elseif lim_sym == :Up
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac <= x.rhs),eachrow(fix_df))
		else lim_sym == :Low
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac >= x.rhs),eachrow(fix_df))
		end
	else
		# adjust rhs and factor of existing constraint
		fix_df = innerjoin(select(part_obj.cns[Symbol(var_sym,cns_sym)],Not([:fac])),fix_df,on = intCol(fix_df,:dir))
		set_normalized_rhs.(fix_df[!,:cns], fix_df[!,:rhs])
		set_normalized_coefficient.(fix_df[!,:cns], fix_df[!,:var], fix_df[!,:fac])	
	end
	
	part_obj.cns[Symbol(var_sym,cns_sym)] = select(fix_df,Not([:var,:value,:rhs,:setZero]))

	# correct value_df to values actually enforced
	value_df = innerjoin(select(value_df,Not([:value])), select(fix_df,Not([:var,:value,:cns,:setZero])), on = intCol(value_df,:dir))
	corSca_fl = getfield(fix_m.options.scaFac,var_sym == :stLvl ? :dispSt : (occursin("StSize",string(var_sym)) ? :capaStSize : :capa))
	value_df[!,:value] .=  value_df[!,:rhs] ./ value_df[!,:fac] .* corSca_fl
	select!(value_df,Not([:fac,:rhs]))

	return value_df
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,cns_df::DataFrame,scaFac_fl::Float64)
	new_df = deSelectSys(cns_df) |> (z -> innerjoin(dual_df,z, on = intCol(z,:dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac] .* scaFac_fl
	return select(filter(x -> x.dual != 0.0,new_df),Not([:cns,:fac]))
end

#getBendersCut(subCut.stLvl[sSym],part_obj.var[:stLvl],top_m.options.scaFac.dispSt)

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, var_df::DataFrame, scaFac_fl::Float64)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(var_df),z, on = intCol(z,:dir)))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual * scaFac_fl * (collect(keys(x.var.terms))[1] - x.value / scaFac_fl),eachrow(ben_df)))
end

# ! removes cases where storage variables are fixed by a ratio (e.g. storage energy capacity fixed by e/p ratio) 
function removeFixStorage(stVar_sym::Symbol,stVar_df::DataFrame,part_obj::TechPart)
	fixPar_dic = Dict(:expStSize => :sizeToStOutFixExp, :expStIn => :stInToConvFixExp, :expStOut => :stOutToStInFixExp, :capaStSize => :sizeToStOutFixCapa, :capaStIn => :stInToConvFixCapa, :capaStOut => :stOutToStInFixCapa)
	if stVar_sym in [:expStSize,:expStIn,:expStOut,:capaStSize,:capaStIn,:capaStOut] && fixPar_dic[stVar_sym] in collect(keys(part_obj.cns))
		fixCns_df = select(part_obj.cns[fixPar_dic[stVar_sym]],Not([:cns]))
		stVar_df = stVar_df |> (x -> antijoin(x,fixCns_df, on = intersect(intCol(x),intCol(fixCns_df))))
	end
	return stVar_df
end

#endregion

#region # * manage linear trust region 

# ! adds limits specified by dictionary to problem
function addLinearTrust!(top_m::anyModel,lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}})
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(lim_dic[sys])
			for trstSym in intersect(keys(lim_dic[sys][sSym]),keys(part_dic[sSym].var))
				# group limiting constraints
				grpBothCapa_arr = collect(groupby(lim_dic[sys][sSym][trstSym],:limCns))
				# get variables of top model
				trstVar_df = filter(x -> !isempty(x.var.terms),part_dic[sSym].var[trstSym])
				foreach(lim -> limitVar!(select(rename(lim,:limVal => :value),Not([:limCns])),trstVar_df,trstSym,part_dic[sSym],top_m,lim[1,:limCns]),grpBothCapa_arr)
			end
		end
	end
end

# ! check for binding limits
function checkLinearTrust(top_m::anyModel)
	binLim_boo = false
	# loop over limits to detect binding ones
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(part_dic)
			for limCns in filter(x -> any(occursin.(["BendersUp","BendersLow"],string(x))), keys(part_dic[sSym].cns))
				# move lower and upper bounds if they are binding
				lim_df = part_dic[sSym].cns[limCns]
				lim_df[!,:bind] = map(x -> dual(x.cns) != 0.0, eachrow(lim_df))
				binLim_boo = binLim_boo || any(lim_df[!,:bind])
			end
		end
	end
	return binLim_boo
end

# ! remove linear trust region
function deleteLinearTrust!(top_m::anyModel)
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(part_dic)
			for limCns in filter(x -> any(occursin.(["BendersUp","BendersLow"],string(x))), keys(part_dic[sSym].cns))
				delete.(top_m.optModel,part_dic[sSym].cns[limCns][!,:cns])
			end
		end
	end
end

#endregion

#region # * reporting

# write storage levels for case of reduced foresight
function writeStLvlRes(top_m::anyModel,sub_dic::Dict{Tuple{Int64, Int64},anyModel},sub_tup::Tuple,i::Int,stReport_df::DataFrame)

	# get levels from top-problem
	for x in filter(x -> :stLvl in keys(top_m.parts.tech[x].var),keys(top_m.parts.tech))
		data_df = printObject(top_m.parts.tech[x].var[:stLvl],top_m,rtnDf = (:csvDf,))
		data_df[!,:i] .= i
		append!(stReport_df,rename(data_df,:variable => :value))
	end

	# get levels from subproblems
	for s in collect(sub_tup)
		for x in filter(x -> :stLvl in keys(sub_dic[s].parts.tech[x].var),keys(sub_dic[s].parts.tech))
			data_df = printObject(sub_dic[s].parts.tech[x].var[:stLvl],sub_dic[s],rtnDf = (:csvDf,))
			data_df[!,:i] .= i
			append!(stReport_df,rename(data_df,:variable => :value))
		end
	end

	CSV.write(modOpt_tup.resultDir * "/stLvl_$(replace(top_m.options.objName,"topModel" => "")).csv",stReport_df)

	return stReport_df
end

# write capacity results for near optimal
function writeCapaRes(top_m::anyModel,sub_dic::Dict{Tuple{Int64, Int64},anyModel},sub_tup::Tuple,nearOpt_df::DataFrame,i::Int,nOpt_int::Int,nearOpt_ntup::NamedTuple,topCost_fl::Float64,subCost_fl::Float64,costOpt_fl::Float64,lssOpt_fl::Float64)
	if !isempty(nearOpt_ntup)
		lss_fl = sum(map(x -> sum(value.(sub_dic[x].parts.bal.var[:lss][!,:var])),sub_tup))
		if nOpt_int == 0 || ((topCost_fl + subCost_fl) <= costOpt_fl * (1 + nearOpt_ntup.cutThres) && lss_fl <= lssOpt_fl * (1 + nearOpt_ntup.lssThres))
			newRes_df = getCapaResult(top_m)
			newRes_df[!,:i] .= i
			newRes_df[!,:cost] .= topCost_fl + subCost_fl
			newRes_df[!,:lss] .= lss_fl
			if nOpt_int != 0 newRes_df[!,:thrs] .= (topCost_fl + subCost_fl)/costOpt_fl - 1 end
			append!(nearOpt_df,newRes_df)
		end
	end
	
	return nearOpt_df, lss_fl
end



#endregion