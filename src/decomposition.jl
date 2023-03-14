#region # * objects for decomposition

# ! struct for results of a sub-problem
mutable struct bendersData
	objVal::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}())
end

# ! copy functions for benders related results
function copy(ben_obj::bendersData)
	out = bendersData()
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	return out
end

# ! stores all information on current trust region 
mutable struct quadTrust
	objVal::Float64
	rad::Float64
	cns::ConstraintRef
	var::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}	
	opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:shrFac),Tuple{Float64,Float64,Float64,Float64}}	

	function quadTrust(exp_df::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trust_opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:shrFac),Tuple{Float64,Float64,Float64,Float64}})
		trustReg_obj = new()
		trustReg_obj.opt = trust_opt
		trustReg_obj.var = exp_df
		trustReg_obj.rad = trust_opt.startRad
		trstExpr_arr = vcat(vcat(vcat(map(x -> trustReg_obj.var[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
		return trustReg_obj, size(trstExpr_arr,1)
	end
end

#endregion

#region # * functions for heurstic

# ! run heuristic and return exact capacities plus heuristic cut
function heuristicSolve(modOpt_tup::NamedTuple,redFac::Float64,t_int::Int,opt_obj::DataType,rtrnMod_boo::Bool=true,solDet_boo::Bool=false)

	# create and solve model
	heu_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, objName = "heuristicModel_" * string(round(redFac,digits = 3)) * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 2, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, redStep = redFac, checkRng = (print = true, all = false), forceScr = solDet_boo ? Symbol() : nothing)
	
	prepareMod!(heu_m,opt_obj,t_int)
	set_optimizer_attribute(heu_m.optModel, "Method", 2)
	set_optimizer_attribute(heu_m.optModel, "Crossover", 0)
	optimize!(heu_m.optModel)
	checkIIS(heu_m)

	# write results to benders object
	heuData_obj = bendersData()
	heuData_obj.objVal = sum(map(z -> sum(value.(heu_m.parts.cost.var[z][!,:var])), collect(filter(x -> any(occursin.(["costExp", "costOpr", "costMissCapa", "costRetro"],string(x))), keys(heu_m.parts.cost.var)))))
	heuData_obj.capa = writeResult(heu_m,[:capa,:exp,:mustCapa,:mustExp])
	
	if rtrnMod_boo
		return heu_m, heuData_obj
	else
		return heuData_obj
	end
end

# ! evaluate results of heuristic solution to determine fixed and limited variables
function evaluateHeu(heu_m::anyModel,heuSca_obj::bendersData,heuCom_obj::bendersData,linPar::NamedTuple,wrtCapa::Bool=false)

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
				bothCapa_df[!,:limVal], bothCapa_df[!,:limCns] = map(x -> getLinTrust(x.value_1,x.value_2,linPar), eachrow(bothCapa_df)) |> (w -> map(x -> getindex.(w,x),[1,2]))
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

# ! get limits imposed on by linear trust region all limits are extended to avoid infeasbilites
function getLinTrust(val1_fl::Float64,val2_fl::Float64,linPar::NamedTuple)

	if (val1_fl <= linPar.thrsAbs && val2_fl <= linPar.thrsAbs) || (any([val1_fl <= linPar.thrsAbs,val2_fl <= linPar.thrsAbs]) && abs(val1_fl - val2_fl) < linPar.thrsAbs) # fix to zero, if both values are zero, or if one is zero and the other is very close to zero
		val_arr, cns_arr = [0.0], [:Fix]
	elseif val1_fl >= linPar.thrsAbs && val2_fl <= linPar.thrsAbs # set first value as upper limit, if other is zero
		val_arr, cns_arr = [val1_fl+linPar.thrsAbs], [:Up]
	elseif val1_fl <= linPar.thrsAbs && val2_fl >= linPar.thrsAbs # set second value as upper limit, if other zero
		val_arr, cns_arr = [val2_fl+linPar.thrsAbs], [:Up]
	elseif (abs(val1_fl/val2_fl-1) > linPar.thrsRel) # enforce lower and upper limits, if difference does exceed threshold
		val_arr, cns_arr = sort([val1_fl,val2_fl]), [:Low,:Up]
		val_arr[2] = val_arr[2] + linPar.thrsAbs
		if (val_arr[1] > linPar.thrsAbs) 
			val_arr[1] = val_arr[1] - linPar.thrsAbs
		else
			val_arr, cns_arr = [val_arr[2]], [:Up]
		end
	else 
		val_arr, cns_arr = [val1_fl], [:Fix] # set to mean, if difference does not exceed threshold
	end
		
	return val_arr, cns_arr
end

# ! returns a feasible as close as possible to the input dictionary
function getFeasResult(modOpt_tup::NamedTuple,fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},t_int::Int,zeroThrs_fl::Float64,opt_obj::DataType)

	# create top-problem
	topFeas_m = anyModel(modOpt_tup.inputDir,modOpt_tup.resultDir, objName = "feasModel" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 1, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, checkRng = (print = true, all = false))

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
function computeFeas(top_m::anyModel,var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},zeroThrs_fl::Float64,cutSmall::Bool=false)
	
	# create absolute value constraints for capacities or expansion variables
	for sys in (:tech,:exc)
		partTop_dic = getfield(top_m.parts,sys)
		for sSym in keys(var_dic[sys])
			part = partTop_dic[sSym]
			relVar_arr = filter(x -> any(occursin.(part.decomm == :none ? ["exp","mustCapa"] : ["capa","exp","mustCapa"],string(x))),collect(keys(var_dic[sys][sSym])))
			# create variabbles and writes constraints to minimize absolute value of capacity delta
			for varSym in relVar_arr
				var_df = part.var[varSym] |> (w -> occursin("exp",string(varSym)) ? collapseExp(w) : w)
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
				if cutSmall && 0.0 in abs_df[!,:value]
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

	return top_m
end

# ! find fixed variables and write to file
function writeFixToFiles(fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},feasFix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},temp_dir::String,heu_m::anyModel; skipMustSt::Bool =false)
	rm(temp_dir; force = true, recursive = true)
	mkdir(temp_dir) # create directory for fixing files
	parFix_dic = defineParameter(heu_m.options,heu_m.report) # stores parameter info for fixing
	
	# loop over variables
	for sys in (:tech,:exc), sSym in keys(fix_dic[sys])
		hasMust_boo = sys == :tech ? "must" in heu_m.parts.tech[sSym].capaRestr[!,:cnstrType] : false
		for varSym in filter(x -> !skipMustSt || !hasMust_boo || x in (:capaConv, :expConv), keys(fix_dic[sys][sSym]))
			fix_df = feasFix_dic[sys][sSym][varSym] |> (w -> innerjoin(w,select(fix_dic[sys][sSym][varSym],Not([:value])), on = intersect(intCol(w,:dir),intCol(fix_dic[sys][sSym][varSym],:dir))))
			# create file name
			par_sym = Symbol(varSym,:Fix)
			fileName_str = temp_dir * "/par_Fix" * string(makeUp(sys)) * "_" * string(sSym) * "_" * string(varSym)
			# set really small values due to remaining solver imprecisions to zero
			fix_df[!,:value] = map(x -> x < 1e-10 ? 0.0 : x,fix_df[!,:value])
			if occursin("capa",string(varSym)) # adds residual capacities
				resVal_df = copy(getfield(heu_m.parts,sys)[sSym].var[varSym])
				resVal_df[!,:resi] = map(x -> x.constant, resVal_df[!,:var])
				fix_df = innerjoin(fix_df,select(resVal_df,Not([:var])), on = intCol(fix_df,:dir))
				fix_df[!,:value] = fix_df[!,:value] .+ fix_df[!,:resi]
				select!(fix_df,Not([:resi]))	
			end

			if varSym == :expExc && heu_m.parts.exc[sSym].dir fix_df[!,:dir] .= true end
			# writes parameter file
			writeParameterFile!(heu_m,fix_df,par_sym,parFix_dic[par_sym],fileName_str)
		end
	end
end

# ! filter values relevant for quadratic trust region
function filterQtrVar(allVal_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel)

	var_dic = Dict(x => Dict{Symbol,Dict{Symbol,DataFrame}}() for x in [:tech,:exc])

	for sys in (:exc,:tech)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(allVal_dic[sys])
			var_dic[sys][sSym] = Dict{Symbol,Dict{Symbol,DataFrame}}() # create empty dataframe for values
			
			# determine where using expansion rather than capacity is possible and more efficient
			varNum_dic = Dict(x => size(unique(getfield.(part_dic[sSym].var[x][!,:var],:terms)),1) for x in collect(keys(part_dic[sSym].var))) # number of unique variables
			trstVar_arr = map(filter(x -> occursin("capa",string(x)),collect(keys(part_dic[sSym].var)))) do x
				expVar_sym = Symbol(replace(string(x),"capa" => "exp"))
				return part_dic[sSym].decomm == :none && expVar_sym in keys(varNum_dic) && varNum_dic[expVar_sym] <= varNum_dic[x] ? expVar_sym : x
			end

			for trstSym in trstVar_arr
				var_df = allVal_dic[sys][sSym][trstSym]
				if trstSym == :capaExc && !part_dic[sSym].dir filter!(x -> x.R_from < x.R_to,var_df) end # only get relevant capacity variables of exchange
				if sys == :tech var_df = removeFixStorage(trstSym,var_df,part_dic[sSym]) end # remove storage variables controlled by ratio
				# filter cases where acutal variables are defined
				var_dic[sys][sSym][trstSym] = intCol(var_df) |> (w ->innerjoin(var_df,unique(select(filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym]),w)), on = w))
				# remove if no capacities remain
				removeEmptyDic!(var_dic[sys][sSym],trstSym)
			end
			
			# remove entire system if not capacities
			removeEmptyDic!(var_dic[sys],sSym)
		end
	end
	return var_dic
end

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

# ! solves top problem without trust region and obtains lower limits
function runTopWithoutQuadTrust(top_m::anyModel,trustReg_obj::quadTrust)
	# solve top again with trust region and re-compute bound for soultion
	delete(top_m.optModel,trustReg_obj.cns)
	set_optimizer_attribute(top_m.optModel, "Method", 0)
	set_optimizer_attribute(top_m.optModel, "NumericFocus", 0)
	optimize!(top_m.optModel)

	# obtain different objective values
	objTop_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var])) # costs of unconstrained top-problem
	lowLim_fl = objTop_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var]) # objective (incl. benders) of unconstrained top-problem

	return objTop_fl, lowLim_fl
end

# ! dynamically adjusts the trust region
function adjustQuadTrust(top_m::anyModel,allVal_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trustReg_obj::quadTrust,objSub_fl::Float64,objTopTrust_fl::Float64,lowLim_fl::Float64,lowLimTrust_fl::Float64,report_m::anyModel)
	# re-create trust region
	if (objTopTrust_fl + objSub_fl) <= trustReg_obj.objVal # recenter trust region, if new best solution was obtained
		trustReg_obj.var = filterQtrVar(allVal_dic,top_m)
		trustReg_obj.cns  = centerQuadTrust(trustReg_obj.var,top_m,trustReg_obj.rad)
		trustReg_obj.objVal = objTopTrust_fl + objSub_fl
		produceMessage(report_m.options,report_m.report, 1," - Re-centered trust-region!", testErr = false, printErr = false)
	else
		if abs(1 - lowLim_fl / lowLimTrust_fl) < trustReg_obj.opt.shrThrs && trustReg_obj.rad > trustReg_obj.opt.lowRad # shrink trust region, if it does not constrain the top problem and the lower limit for its size is not yet reached
			trustReg_obj.rad = trustReg_obj.rad * trustReg_obj.opt.shrFac
			produceMessage(report_m.options,report_m.report, 1," - Shrunk trust-region!", testErr = false, printErr = false)	
		end
		trustReg_obj.cns  = centerQuadTrust(trustReg_obj.var,top_m,trustReg_obj.rad)
	end

	return trustReg_obj
end

# ! add trust region to objective part of model
function centerQuadTrust(var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel,rad_fl::Float64)

	# ! match values with variables
	expExpr_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		expExpr_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(var_dic[sys])
			expExpr_dic[sys][sSym] = Dict{Symbol,DataFrame}()
			for varSym in keys(var_dic[sys][sSym])
				val_df = deSelectSys(var_dic[sys][sSym][varSym])
				# correct scaling and storage values together with corresponding variables
				val_df[!,:value] = val_df[!,:value] ./ getfield(top_m.options.scaFac, occursin("exp",string(varSym)) ? :insCapa : (occursin("StSize",string(varSym)) ? :capaStSize : :capa))
				expExpr_dic[sys][sSym][varSym] = unique(select(innerjoin(deSelectSys(part_dic[sSym].var[varSym]),val_df, on = intCol(val_df,:dir)),[:var,:value]))
			end
		end
	end

	# ! write trust region constraint
	allVar_df = vcat(vcat(vcat(map(x -> expExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	
	# sets values of variables that will violate range to zero
	minFac_fl = (2*maximum(allVar_df[!,:value]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	abs_fl = sum(allVar_df[!,:value]) |> (x -> x < 0.01 * size(allVar_df,1) ? 10 * size(allVar_df,1) : x)
	scaRng_tup = top_m.options.coefRng.rhs ./ abs((abs_fl * rad_fl)^2 - sum(allVar_df[!,:value].^2))

	# set values of variable to zero or biggest value possible without scaling violating rhs range
	for x in eachrow(allVar_df)	
		if top_m.options.coefRng.mat[1]/(x.value*2) > scaRng_tup[2] # factor requires more up-scaling than possible
			x[:value] = 0 # set value to zero
		elseif top_m.options.coefRng.mat[2]/(x.value*2) < scaRng_tup[1] # factor requires more down-scaling than possible
			x[:value] = top_m.options.coefRng.mat[2]/(scaRng_tup[1]*2) # set to biggest value possible within range
		end
	end

	# computes left hand side expression and scaling factor
	capaSum_expr = sum(map(x -> collect(keys(x.var.terms))[1] |> (z -> z^2 - 2*x.value*z + x.value^2),eachrow(allVar_df)))
	scaFac_fl =  top_m.options.coefRng.mat[1]/minimum(abs.(collect(values(capaSum_expr.aff.terms)) |> (z -> isempty(z) ? [1.0] : z)))
	
	# create final constraint
	trustRegion_cns = @constraint(top_m.optModel,  capaSum_expr * scaFac_fl <= (abs_fl * rad_fl)^2  * scaFac_fl)

	return trustRegion_cns
end

#endregion

#region # * benders solution algorithm

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

# ! run sub-problem
function runSub(sub_m::anyModel,capaData_obj::bendersData,sol::Symbol,wrtRes::Bool=false)

	# fixing capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in sort(filter(x -> occursin("capa",lowercase(string(x))), collect(keys(capaData_obj.capa[sys][sSym]))),rev = true)
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], capaData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(capaData_obj.capa[sys][sSym][capaSym])
					delete!(capaData_obj.capa[sys][sSym],capaSym)
				else
					capaData_obj.capa[sys][sSym][capaSym] = limitVar!(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],sub_m)
				end
			end
			# remove system if no capacities exist
			removeEmptyDic!(capaData_obj.capa[sys],sSym)
		end
	end

	# set optimizer attributes and solves
	if sol == :barrier
		set_optimizer_attribute(sub_m.optModel, "Method", 2)
		set_optimizer_attribute(sub_m.optModel, "Crossover", 0)
		set_optimizer_attribute(sub_m.optModel, "BarOrder", 1)
		set_optimizer_attribute(sub_m.optModel, "BarConvTol", 1e-4)
	elseif sol == :simplex
		set_optimizer_attribute(sub_m.optModel, "Method", 1)
		set_optimizer_attribute(sub_m.optModel, "Threads", 1)
		set_optimizer_attribute(sub_m.optModel, "OptimalityTol", 1e-9)
		set_optimizer_attribute(sub_m.optModel, "Presolve", 2)
	end

	optimize!(sub_m.optModel)
	checkIIS(sub_m)

	# write results into files (only used once optimum is obtained)
	if wrtRes
		reportResults(:summary,sub_m)
		reportResults(:cost,sub_m)
	end

	# add duals and objective value to capacity data
	scaObj_fl = sub_m.options.scaFac.obj
	capaData_obj.objVal = value(sum(sub_m.parts.obj.var[:objVar][!,:var]))

	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa",lowercase(string(x))), collect(keys(capaData_obj.capa[sys][sSym])))
				if Symbol(capaSym,:BendersFix) in keys(part_dic[sSym].cns)
					scaCapa_fl = getfield(sub_m.options.scaFac,occursin("StSize",string(capaSym)) ? :capaStSize : :capa)
					capaData_obj.capa[sys][sSym][capaSym] = addDual(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].cns[Symbol(capaSym,:BendersFix)],scaObj_fl/scaCapa_fl)
					# remove capacity if none exists (again necessary because dual can be zero)
					removeEmptyDic!(capaData_obj.capa[sys][sSym],capaSym)
				end
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			removeEmptyDic!(capaData_obj.capa[sys],sSym)
		end
	end

	return capaData_obj
end

# ! run top-problem
function runTop(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	capaData_obj = bendersData()

	# add cuts
	if !isempty(cutData_dic) addCuts!(top_m,cutData_dic,i) end
	# solve model
	set_optimizer_attribute(top_m.optModel, "Method", 2)
	set_optimizer_attribute(top_m.optModel, "Crossover", 0)
	set_optimizer_attribute(top_m.optModel, "NumericFocus", 3)
	optimize!(top_m.optModel)
	checkIIS(top_m)

	# write technology capacites and level of capacity balance to benders object
	capaData_obj.capa, allVal_dic = [writeResult(top_m,x; rmvFix = true) for x in [[:capa,:mustCapa],[:capa,:exp]]] 
	
	# get objective value of top problem
	objTopTrust_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	lowLimTrust_fl = objTopTrust_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])

	return capaData_obj, allVal_dic, objTopTrust_fl, lowLimTrust_fl
end

# ! add all cuts from input dictionary to top problem
function addCuts!(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)
	
	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_disSup = Int[],scr = Int[], limCoef = Bool[], actItr = Int[], cnsExpr = AffExpr[])
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
		
		# get cut variable and compute cut expression 
		cut_var = filter(x -> x.Ts_disSup == top_m.supTs.step[sub[1]] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var]
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
		end

		#endregion

		# add benders variable to cut and push to dataframe of all cuts
		push!(cut_df,(i = i, Ts_disSup = top_m.supTs.step[sub[1]], scr = sub[2], limCoef = limCoef_boo, actItr = i, cnsExpr = cut_expr - cut_var))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df,top_m.options.coefRng,top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts] ,createCns(cnsCont(cut_df,:smaller),top_m.optModel,false))

end

# ! delete cuts that have not been binding for a while
function deleteCuts!(top_m::anyModel,delCut_int::Int,i::Int)
	# tracking latest binding iteration for cuts
	top_m.parts.obj.cns[:bendersCuts][!,:actItr] .= map(x -> dual(x.cns) != 0.0 ? i : x.actItr, eachrow(top_m.parts.obj.cns[:bendersCuts]))
	# delete cuts that were not binding long enough
	delete.(top_m.optModel, filter(x -> x.actItr + delCut_int < i,top_m.parts.obj.cns[:bendersCuts])[!,:cns])
	filter!(x -> (x.actItr + delCut_int > i),top_m.parts.obj.cns[:bendersCuts])
end

#endregion

#region # * transfer results between models

# ! write capacities or expansion in input model to returned capacity dictionary
function writeResult(in_m::anyModel, var_arr::Array{Symbol,1};rmvFix::Bool = false, fltSt::Bool = true, rmvCons::Bool = true)
	
	var_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	
	for sys in (:tech,:exc)
		var_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts,sys)
		for sSym in filter(x -> part_dic[x].type in (:stock,:mature,:emerging),keys(part_dic))
			
			# continue in case of technology without changing capacites
			if part_dic[sSym].type == :stock &&  part_dic[sSym].decomm == :none continue end	

			varSym_arr = filter(x -> any(occursin.(string.(var_arr),string(x))), keys(part_dic[sSym].var))
			var_dic[sys][sSym] = Dict(varSym => getResult(copy(part_dic[sSym].var[varSym]),rmvCons) for varSym in varSym_arr)

			# check if storage expansion is fixed to storage output and removes variables in these cases
			if sys == :Te && fltSt
				for stVar in collect(keys(var_dic[sys][sSym]))
					var_dic[sys][sSym][stVar] = removeFixStorage(stVar,var_dic[sys][sSym],part_dic[sSym])
				end
			end

			# removes variables that are fixed from output
			if rmvFix
				for varSym in varSym_arr
					must_boo = occursin("must",string(varSym))
					fixVar_sym = part_dic[sSym].decomm == :none ? Symbol(replace(string(varSym),must_boo ? "Capa" => "Exp" : "capa" => "exp")) : varSym
					if Symbol(fixVar_sym,"BendersFix") in collect(keys(part_dic[sSym].cns))
						var_df = var_dic[sys][sSym][varSym]
						expCns_df = select(part_dic[sSym].cns[Symbol(fixVar_sym,"BendersFix")],Not([:cns,:fac,]))
						# in case of no decommissioning capacites are fixed if all corresponding expansion variables are fixed
						if part_dic[sSym].decomm == :none	
							# get non-fixed cases for expansion variables
							expCnsY_df = antijoin(select(part_dic[sSym].var[fixVar_sym],Not([:var])),expCns_df, on = intCol(expCns_df))
							if !isempty(expCnsY_df)
								expCnsY_df = select(flatten(expCnsY_df,:Ts_disSup),Not(:Ts_exp))
								join_arr = part_dic[sSym].type == :emerging ? intCol(var_df) : filter(x -> x != :Ts_expSup,intCol(var_df))
								var_dic[sys][sSym][varSym] = innerjoin(var_df,unique(select(expCnsY_df,join_arr)), on = join_arr)
							else
								var_dic[sys][sSym][varSym] = DataFrame()
							end
						else
							var_dic[sys][sSym][varSym] = innerjoin(var_df,expCns_df, on = intCol(var_df))
						end
						
					end
				end
			end

			# removes redundant varibles for undirected exchange capacity
			if sys == :exc && !part_dic[sSym].dir && :capaExc in keys(var_dic[sys][sSym])
				filter!(x -> x.R_from < x.R_to,var_dic[sys][sSym][:capaExc])
			end

			# remove empty fields
			filter!(x -> !isempty(x[2]),var_dic[sys][sSym])
			removeEmptyDic!(var_dic[sys],sSym)	
		end
	end

	return var_dic
end

# ! replaces the variable column with a column storing the value of the variable
function getResult(res_df::DataFrame,rmvCons::Bool)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		# aggregates expansion, if spread across different years
		res_df = combine(groupby(res_df,filter(x -> x != :Ts_expSup, intCol(res_df))), :var => (x -> x[1] * size(x,1)) => :var)
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms),res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> round(max(0,value(x) - (rmvCons ? x.constant : 0.0)), digits = 12),res_df[!,:var])

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
	fix_df[!,:value]  = lowercase(string(var_sym)) |> (z -> fix_df[!,:value] ./ getfield(fix_m.options.scaFac,occursin("exp",z) ? :insCapa : occursin("stsize",string(z)) ? :capaStSize : :capa))
	
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
	value_df[!,:value] .=  value_df[!,:rhs] ./ value_df[!,:fac] .* getfield(fix_m.options.scaFac,occursin("StSize",string(var_sym)) ? :capaStSize : :capa)
	select!(value_df,Not([:fac,:rhs]))

	return value_df
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,cns_df::DataFrame,scaFac_fl::Float64)
	new_df = deSelectSys(cns_df) |> (z -> innerjoin(dual_df,z, on = intCol(z,:dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac] .* scaFac_fl
	return select(filter(x -> x.dual != 0.0,new_df),Not([:cns,:fac]))
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, var_df::DataFrame, scaCapa_fl::Float64)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(var_df),z, on = intCol(z,:dir)))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual * scaCapa_fl * (collect(keys(x.var.terms))[1] - x.value / scaCapa_fl),eachrow(ben_df)))
end

# ! removes cases where storage variables are fixed by a ratio (e.g. storage energy capacity fixed by e/p ratio) 
function removeFixStorage(stVar_sym::Symbol,stVar_df::DataFrame,part_obj::TechPart)
	fixPar_dic = Dict(:expStSize => :sizeToStOutFixExp, :expStIn => :stOutToStInFixExp, :capaStSize => :sizeToStOutFixCapa, :capaStIn => :stOutToStInFixCapa)
	if stVar_sym in [:expStSize,:expStIn,:capaStSize,:capaStIn] && fixPar_dic[stVar_sym] in collect(keys(part_obj.cns))
		fixCns_df = select(part_obj.cns[fixPar_dic[stVar_sym]],Not([:cns]))
		stVar_df = stVar_df |> (x -> antijoin(x,fixCns_df, on = intCol(x)))	
	end
	return stVar_df
end

#endregion