

#region # * heuristic solve and fix of capacities

# ! run heuristic and return exact capacities plus heuristic cut
function heuristicSolve(modOpt_tup::NamedTuple, t_int::Int, opt_obj::DataType; rtrnMod::Bool=true, solDet::Bool=false, fltSt::Bool=true)

	# create and solve model
	frsLvl_int = solDet ? 0 : modOpt_tup.frsLvl
	heu_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, holdFixed = true, objName = "heuristicModel_" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, repTsLvl = modOpt_tup.repTsLvl, frsLvl = frsLvl_int, reportLvl = 2, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, checkRng = (print = true, all = false), forceScr = solDet ? Symbol() : nothing)
	
	prepareMod!(heu_m, opt_obj, t_int)
	set_optimizer_attribute(heu_m.optModel, "Method", 2)
	set_optimizer_attribute(heu_m.optModel, "Crossover", 0)
	@suppress optimize!(heu_m.optModel)

	# write results to benders object
	heuData_obj = resData()
	heuData_obj.objVal = sum(map(z -> sum(value.(heu_m.parts.cost.var[z][!, :var])), collect(filter(x -> any(occursin.(["costExp", "costOpr", "costMissCapa", "costRetro"], string(x))), keys(heu_m.parts.cost.var)))))
	heuData_obj.capa, ~ = writeResult(heu_m, [:capa, :exp, :mustCapa, :mustExp], fltSt = fltSt)
	
	if rtrnMod
		return heu_m, heuData_obj
	else
		return heuData_obj
	end
end

# ! evaluate results of heuristic solution to determine fixed and limited variables
function evaluateHeu(heu_m::anyModel, heuSca_obj::resData, heuCom_obj::resData, linPar_tup::NamedTuple, wrtCapa::Bool=false)

	# create empty dictionaries for limits and fixes
	fix_dic = Dict(:tech => Dict{Symbol,Dict{Symbol,DataFrame}}(), :exc => Dict{Symbol,Dict{Symbol,DataFrame}}())
	lim_dic = Dict(:tech => Dict{Symbol,Dict{Symbol,DataFrame}}(), :exc => Dict{Symbol,Dict{Symbol,DataFrame}}())
	cntHeu_arr = [0, 0] # tracks number of variables fixed to zero, variable fixed to a non-zero value and limited variable

	# ! determine variables for fixing and limiting
	for sys in (:tech, :exc)
		part_dic = getfield(heu_m.parts, sys)
		for sSym in keys(heuSca_obj.capa[sys])
			fix_dic[sys][sSym] = Dict{Symbol,DataFrame}()
			lim_dic[sys][sSym] = Dict{Symbol,DataFrame}()

			relVar_arr = filter(x -> any(occursin.(part_dic[sSym].decomm == :none && !wrtCapa ? ["exp", "mustCapa"] : ["capa", "exp", "mustCapa"], string(x))), collect(keys(part_dic[sSym].var)))
			relVarLim_arr = filter(x -> any(occursin.(part_dic[sSym].decomm == :none ? ["exp", "mustCapa"] : ["capa", "exp", "mustCapa"], string(x))), collect(keys(part_dic[sSym].var)))

			for varSym in intersect(keys(heuSca_obj.capa[sys][sSym]), keys(heuCom_obj.capa[sys][sSym]), relVar_arr)
				must_boo = occursin("must", string(varSym))
				# match results from two different heuristic models
				bothCapa_df = rename(heuSca_obj.capa[sys][sSym][varSym], :value => :value_1) |> (x -> innerjoin(x, rename(heuCom_obj.capa[sys][sSym][varSym], :value => :value_2), on = intCol(x, :dir)))
				# determine cases for fix and limit
				bothCapa_df[!,:limVal], bothCapa_df[!,:limCns] = map(x -> getLinTrust(x.value_1, x.value_2, linPar_tup), eachrow(bothCapa_df)) |> (w -> map(x -> getindex.(w, x), [1, 2]))
				bothCapa_df = flatten(select(bothCapa_df, Not([:value_1, :value_2])), [:limVal, :limCns])

				# ! store limited variables
				if varSym in relVarLim_arr
					lim_df = filter(x -> x.limCns != :Fix, bothCapa_df)
					if !isempty(lim_df)
						# removes storage variables controlled by ratio from further analysis
						if sys == :tech lim_df = removeFixStorage(varSym, lim_df, part_dic[sSym]) end
						if isempty(lim_df)
							continue
						else 
							lim_dic[sys][sSym][varSym] = lim_df
						end
						# reports on limited variables
						cntHeu_arr[2] = cntHeu_arr[2] + size(filter(x -> x.limCns == :Up, lim_df), 1)
					end
				end
				
				# ! store fixed variables
				fix_df = select(filter(x -> x.limCns == :Fix, bothCapa_df), Not([:limCns]))
				if !isempty(fix_df)
					if sys == :tech fix_df = removeFixStorage(varSym, fix_df, part_dic[sSym]) end

					if isempty(fix_df)
						continue
					else 
						fix_dic[sys][sSym][varSym] = rename(fix_df, :limVal => :value)
					end
					
					# find related expansion variables and fix as well
					if !occursin("exp", lowercase(string(varSym)))
						for expVar in filter(x -> string(x) in replace.(string(varSym), must_boo ? ["Capa" => "Exp"] : ["capa" => "exp"]), keys(heuSca_obj.capa[sys][sSym]))
							# gets relevant expansion variables
							exp_df = heuSca_obj.capa[sys][sSym][expVar] |> (w -> innerjoin(w, select(part_dic[sSym].var[expVar], Not([:Ts_expSup, :var])), on = intCol(w)))
							# only fix expansion variables that relate to a fixed capacity
							relExp_df = unique(select(select(fix_df, Not(part_dic[sSym].decomm == :emerging ? [:limVal] : [:Ts_expSup, :limVal])) |> (w -> innerjoin(flatten(exp_df, :Ts_disSup), w, on = intCol(w))), Not([:Ts_disSup])))
							if !isempty(relExp_df) fix_dic[sys][sSym][expVar] = relExp_df end	
						end
					end
					# report on fixed variables
					cntHeu_arr[1] = cntHeu_arr[1] + size(fix_df, 1)	
				end
			end
			# delete if nothing was written
			removeEmptyDic!(fix_dic[sys], sSym)
			removeEmptyDic!(lim_dic[sys], sSym)
		end
	end

	return fix_dic, lim_dic, cntHeu_arr
end

# ! returns a feasible solution as close as possible to the input dictionary
function getFeasResult(modOpt_tup::NamedTuple, fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, t_int::Int, zeroThrs_fl::Float64, opt_obj::DataType; rngVio_fl::Float64 = 1e0, useVI::NamedTuple{(:bal, :st), Tuple{Bool, Bool}} = (bal = false, st = false), complCns = Dict{Tuple{Int64, Int64}, Dict{Symbol, DataFrame}}(), relVar = Vector{Symbol}(), resTup::Tuple = tuple())

	# create top-problem
	topFeas_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, objName = "feasModel" * modOpt_tup.suffix, frsLvl = modOpt_tup.frsLvl, supTsLvl = modOpt_tup.supTsLvl,  repTsLvl = modOpt_tup.repTsLvl, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, createVI = useVI, checkRng = (print = true, all = false), holdFixed = true)

	topFeas_m.subPro = tuple(0, 0)
	prepareMod!(topFeas_m, opt_obj, t_int)
    if !isempty(relVar) addComplCns!(topFeas_m, relVar, complCns) end

	# add limits to problem
	if !isempty(lim_dic) addLinearTrust!(topFeas_m, lim_dic, rngVio_fl) end

	# compute feasible capacites
	topFeas_m = computeFeas(topFeas_m, fix_dic, zeroThrs_fl, cutSmall = true);

    # return capacities and top problem (is sometimes used to compute costs of feasible solution afterward)
    return writeResult(topFeas_m, [:capa, :exp, :stLvl, :lim]; rmvFix = true), value(topFeas_m.parts.obj.var[:objVar][1,:var]), Dict(x => reportResults(x, topFeas_m, rtnOpt = (:csvDf,)) for x in resTup)
end

# ! runs top problem again with optimal results
function computeFeas(top_m::anyModel, var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, zeroThrs_fl::Float64; expExist::Bool = true, cutSmall::Bool=false, resultOpt::NamedTuple=NamedTuple())
	
	# create absolute value constraints for capacities or expansion variables
	for sys in (:tech, :exc)
		partTop_dic = getfield(top_m.parts, sys)
		for sSym in keys(var_dic[sys])
			part = partTop_dic[sSym]
			relVar_arr = filter(x -> any(occursin.(part.decomm == :none && expExist ? ["exp", "mustCapa"] : ["capa", "exp", "mustCapa"], string(x))), collect(keys(var_dic[sys][sSym])))
			# create variables and writes constraints to minimize absolute value of capacity delta
			for varSym in relVar_arr
				var_df = part.var[varSym] |> (w -> occursin("exp", string(varSym)) ? collapseExp(w) : w)
				filter!(x -> !isempty(x.var.terms), var_df)
				if sys == :tech var_df = removeFixStorage(varSym, var_df, part) end # remove storage variables controlled by ratio
				if isempty(var_df) continue end
				# gets variable value and set variables below threshold to zero
				abs_df = deSelect(var_dic[sys][sSym][varSym]) |>  (z -> leftjoin(var_df, z, on = intersect(intCol(z, :dir), intCol(var_df, :dir)))) |> (y -> y[completecases(y), :])
				abs_df[!,:value] = map(x -> abs(x.value) < zeroThrs_fl ? 0.0 : x.value, eachrow(abs_df))
				# applies weights to make achieving zero values a priority 
				abs_df[!,:weight] .= 1.0
				# create variable for absolute value and connect with rest of dataframe again
				scaFac_fl = getfield(top_m.options.scaFac, occursin("exp", lowercase(string(varSym))) ? :insCapa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))
				part.var[Symbol(:abs, makeUp(varSym))] = createVar(select(abs_df, Not([:var, :value])), string(:abs, makeUp(varSym)), top_m.options.bound.capa, top_m.optModel, top_m.lock, top_m.sets, scaFac = scaFac_fl)
				abs_df[!,:varAbs] .= part.var[Symbol(:abs, makeUp(varSym))][!,:var] 
				# create constraints for absolute value
				abs_df[!,:absLow] = map(x -> x.varAbs - scaFac_fl * collect(keys(x.var.terms))[1] + x.value, eachrow(abs_df))
				abs_df[!,:absUp] = map(x -> x.varAbs  + scaFac_fl * collect(keys(x.var.terms))[1] - x.value, eachrow(abs_df)) 
				# scale and create absolute value constraints
				absLow_df = rename(orderDf(abs_df[!,[intCol(abs_df)..., :absLow]]), :absLow => :cnsExpr)
				absUp_df = rename(orderDf(abs_df[!,[intCol(abs_df)..., :absUp]]), :absUp => :cnsExpr)
				scaleCnsExpr!(absLow_df, top_m.options.coefRng, top_m.options.checkRng)
				scaleCnsExpr!(absUp_df, top_m.options.coefRng, top_m.options.checkRng)
				part.cns[Symbol(:absLow,makeUp(varSym))] = createCns(cnsCont(absLow_df, :greater), top_m.optModel, false)
				part.cns[Symbol(:absUp,makeUp(varSym))] = createCns(cnsCont(absUp_df, :greater), top_m.optModel, false)
				# create binary constraint to ensure zero values are either zero or above threshold
				if cutSmall && 0.0 in abs_df[!,:value]
					cutSmall_df = rename(select(filter(x -> x.value == 0.0, abs_df), Not([:varAbs, :absLow, :absUp, :weight])), :var => :var_2)
					# create binary variable
					cutSmall_df = createVar(cutSmall_df, string("cutSmall", makeUp(varSym)), NaN, top_m.optModel, top_m.lock, top_m.sets, bi = true)
					# create constraints either enforcing zero or at least zero threshold
					cutSmall_df[!,:cutSmallZero] = map(x -> scaFac_fl * collect(keys(x.var_2.terms))[1] - (1 - x.var) * 10*zeroThrs_fl, eachrow(cutSmall_df))
					cutSmall_df[!,:cutSmallNonZero] = map(x -> scaFac_fl * collect(keys(x.var_2.terms))[1] - (1 - x.var) * zeroThrs_fl, eachrow(cutSmall_df))
					cutSmallZero_df = rename(orderDf(cutSmall_df[!,[intCol(cutSmall_df)..., :cutSmallZero]]), :cutSmallZero => :cnsExpr)
					cutSmallNonZero_df = rename(orderDf(cutSmall_df[!,[intCol(cutSmall_df)..., :cutSmallNonZero]]), :cutSmallNonZero => :cnsExpr)
					scaleCnsExpr!(cutSmallZero_df, top_m.options.coefRng, top_m.options.checkRng)
					scaleCnsExpr!(cutSmallNonZero_df, top_m.options.coefRng, top_m.options.checkRng)
					part.cns[Symbol(:cutSmallZero, makeUp(varSym))] = createCns(cnsCont(cutSmallZero_df, :smaller), top_m.optModel, false)
					part.cns[Symbol(:cutSmallNonZero, makeUp(varSym))] = createCns(cnsCont(cutSmallNonZero_df, :greater), top_m.optModel, false)
					part.var[Symbol(:cutSmall, makeUp(varSym))] = select(cutSmall_df, intCol(cutSmall_df, :var))
				end
			end
		end
	end
	
	# get sum of absolute values 
	absVar_arr = [:CapaConv, :CapaExc, :CapaStOut, :CapaStIn, :CapaStSize, :ExpConv, :ExpExc, :ExpStOut, :ExpStIn, :ExpStSize]
	absVal_expr = sum(map(x -> sum(getAllVariables(Symbol(:abs, x), top_m) |> (w -> isempty(w) ? [AffExpr()] : w[!,:var] .* w[!,:weight])), vcat(absVar_arr, Symbol.(:Must, absVar_arr))))
	# get sum of missing capacities and apply high weight 
	missCapa_expr = :missCapa in keys(top_m.parts.bal.var) ? sum(top_m.parts.bal.var[:missCapa][!,:var] ./ top_m.options.scaFac.insCapa .* 10) : AffExpr()
	objExpr_df = DataFrame(cnsExpr = [missCapa_expr + absVal_expr])
	scaleCnsExpr!(objExpr_df, top_m.options.coefRng, top_m.options.checkRng)
	
	# change objective of top problem to minimize absolute values and missing capacities
	@objective(top_m.optModel, Min, objExpr_df[1,:cnsExpr])
	# solve problem
	set_optimizer_attribute(top_m.optModel, "MIPGap", 0.001)
	set_optimizer_attribute(top_m.optModel, "SolutionLimit", 3600)
	solveModel!(top_m, top_m.optModel, [0,3], true, false)
	checkIIS(top_m)

	# write results into files (only used once optimum is obtained)
	writeAllResults!(top_m, resultOpt)

	return top_m
end

# ! find fixed variables and write to file
function writeFixToFiles(fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, feasFix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, temp_dir::String, res_m::anyModel; skipMustSt::Bool = false)
	rm(temp_dir; force = true, recursive = true)
	mkdir(temp_dir) # create directory for fixing files
	parFix_dic = defineParameter(res_m.options, res_m.report) # stores parameter info for fixing
	
	# loop over variables
	for sys in (:tech, :exc), sSym in keys(fix_dic[sys])
		hasMust_boo = sys == :tech && isdefined(res_m.parts.tech[sSym], :capaRestr) ? "must" in res_m.parts.tech[sSym].capaRestr[!,:cnstrType] : false
		for varSym in filter(x -> (!skipMustSt || !hasMust_boo || x in (:capaConv, :expConv)) && !(x in (:capaStSizeSeason,:capaStInter)), keys(fix_dic[sys][sSym]))

			fix_df = feasFix_dic[sys][sSym][varSym] |> (w -> innerjoin(w, select(fix_dic[sys][sSym][varSym], Not([:value])), on = intersect(intCol(w, :dir), intCol(fix_dic[sys][sSym][varSym], :dir))))
			if isempty(fix_df) continue end
			# create file name
			par_sym = Symbol(varSym, :Fix)
			fileName_str = temp_dir * "/par_Fix" * string(makeUp(sys)) * "_" * string(sSym) * "_" * string(varSym)
			# set really small values due to remaining solver imprecisions to zero
			fix_df[!,:value] = map(x -> x < 1e-10 ? 0.0 : x, fix_df[!,:value])
			if occursin("capa", string(varSym)) && varSym in keys(getfield(res_m.parts, sys)[sSym].var) # adds residual capacities
				resVal_df = copy(getfield(res_m.parts, sys)[sSym].var[varSym])
				resVal_df[!,:resi] = map(x -> x.constant, resVal_df[!,:var])
				fix_df = joinMissing(fix_df, select(resVal_df, Not([:var])), intCol(fix_df, :dir), :left, Dict(:resi => 0.0))
				fix_df[!,:value] = fix_df[!,:value] .+ fix_df[!,:resi]
				select!(fix_df, Not([:resi]))
			end

			if varSym == :expExc && res_m.parts.exc[sSym].dir fix_df[!,:dir] .= true end
			# writes parameter file
			writeParameterFile!(res_m, fix_df, par_sym, parFix_dic[par_sym], fileName_str)
		end
	end
end

# ! get limits imposed on by linear trust region all limits are extended to avoid infeasbilites
function getLinTrust(val1_fl::Float64, val2_fl::Float64, linPar_tup::NamedTuple)

	if (val1_fl <= linPar_tup.thrsAbs && val2_fl <= linPar_tup.thrsAbs) || (any([val1_fl <= linPar_tup.thrsAbs, val2_fl <= linPar_tup.thrsAbs]) && abs(val1_fl - val2_fl) < linPar_tup.thrsAbs) # fix to zero, if both values are zero, or if one is zero and the other is very close to zero
		val_arr, cns_arr = [0.0], [:Fix]
	elseif val1_fl >= linPar_tup.thrsAbs && val2_fl <= linPar_tup.thrsAbs # set first value as upper limit, if other is zero
		val_arr, cns_arr = [val1_fl+linPar_tup.thrsAbs], [:Up]
	elseif val1_fl <= linPar_tup.thrsAbs && val2_fl >= linPar_tup.thrsAbs # set second value as upper limit, if other zero
		val_arr, cns_arr = [val2_fl+linPar_tup.thrsAbs], [:Up]
	elseif (abs(val1_fl/val2_fl-1) > linPar_tup.thrsRel) # enforce lower and upper limits, if difference does exceed threshold
		val_arr, cns_arr = sort([val1_fl, val2_fl]), [:Low, :Up]
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

# build sub-problems
function buildSub(id::Int, subStr_tup::Tuple{String, String}, genSetup_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, inputFolderSub_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, scale_dic::Dict{Symbol,NamedTuple}, algOpt_obj::algSetup)
	# filter relevant input folders
	relIn_arr = filter(x -> (occursin("ini",x) && genSetup_ntup.frsLvl != 0 ? occursin(subStr_tup[1],x) : true) && (occursin("scr",x) ? occursin(subStr_tup[2],x) : true), inputFolderSub_ntup.in)
	# create sub-problems
	sub_m = anyModel(relIn_arr, inputFolderSub_ntup.results, checkRng = (print = true, all = false), objName = "subModel_" * string(id) * "_" * genSetup_ntup.name, frsLvl = genSetup_ntup.frsLvl, repTsLvl = genSetup_ntup.repTsLvl, holdFixed =true, supTsLvl = genSetup_ntup.supTsLvl, shortExp = genSetup_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facSub], dbInf = algOpt_obj.sub.dbInf, reportLvl = 1)
	sub_m.subPro = tuple(sort([(x.Ts_dis, x.scr) for x in eachrow(sub_m.parts.obj.par[:scrProb].data)])...)[id]
	prepareMod!(sub_m, algOpt_obj.opt, algOpt_obj.sub.threads)
	
	# set options
	@suppress  begin
		set_optimizer_attribute(sub_m.optModel, "Threads", algOpt_obj.sub.threads)
		if algOpt_obj.timeLim != 0.0 set_optimizer_attribute(sub_m.optModel, "TimeLimit", algOpt_obj.sub.timeLim * 60) end # in seconds
	end

	# collect complicating constraints
	comVar_dic = Dict{Symbol,DataFrame}()
	for comVa in filter(x -> occursin("BendersCom",string(x)), keys(sub_m.parts.lim.var))
		comVar_dic[comVa] = sub_m.parts.lim.var[comVa] |> (y -> select(y, filter(x -> x != :var, namesSym(y))))
	end

	return sub_m, comVar_dic
end

# ! set optimizer attributes and options, in case gurobi is used (recommended)
function prepareMod!(mod_m::anyModel, opt_obj::DataType, t_int::Int)
	# create optimization problem
	createOptModel!(mod_m)
	setObjective!(:cost, mod_m)
	# set optimizer and attributes
	set_optimizer(mod_m.optModel, opt_obj)
	set_optimizer_attribute(mod_m.optModel, "Threads", t_int)	
end

# ! run top-problem
function runTop(benders_obj::bendersObj)

	#region # * create cuts
	stab_obj = benders_obj.stab
	allAct_arr = map(x -> (x.i, x.Ts_dis, x.scr), eachrow(benders_obj.top.parts.obj.cns[:bendersCuts]))
	addCuts_arr = filter(x -> !(benders_obj.cuts.all[x][1] in allAct_arr), benders_obj.cuts.active)

	if !isempty(addCuts_arr) 
		# save values of previous cut for proximal method variation 2
		benders_obj.cuts.prev = !isnothing(stab_obj) && stab_obj.method[stab_obj.actMet] == :prx2 ? copy(benders_obj.cuts.active) : Int[]
		# add cuts and reset collecting array
		addCuts!(benders_obj.top, benders_obj.top.optModel, benders_obj.algOpt.rngVio.cut, benders_obj.cuts.all[addCuts_arr]) 
	end

	benders_obj.cuts.cnt = size(benders_obj.top.parts.obj.cns[:bendersCuts], 1)
	#endregion

	#region # * solve problem

	# create objects to store results
	resData_obj = resData()
	bestData_obj = resData()
	stabVar_obj = resData()

	# solve model
	@suppress begin 
		if benders_obj.algOpt.top.dnsThrs != 0 && benders_obj.algOpt.top.dnsThrs != 0.0
			set_optimizer_attribute(benders_obj.top.optModel, "GURO_PAR_BARDENSETHRESH", benders_obj.algOpt.top.dnsThrs)
		end
		# compute tolerances
		stabTol_fl = interItrPar(benders_obj.itr.gap, benders_obj.algOpt.gap, benders_obj.algOpt.top.stabTol[2], benders_obj.algOpt.top.stabTol[1])
		stabTolQ_fl = interItrPar(benders_obj.itr.gap, benders_obj.algOpt.gap, benders_obj.algOpt.top.stabTolQ[2], benders_obj.algOpt.top.stabTolQ[1])
		# set options
		set_optimizer_attribute(benders_obj.top.optModel, "Method", benders_obj.algOpt.top.stabMeth)
		set_optimizer_attribute(benders_obj.top.optModel, "BarQCPConvTol", max(stabTolQ_fl, benders_obj.algOpt.top.stabTolQ[2][2]))
		set_optimizer_attribute(benders_obj.top.optModel, "FeasibilityTol", max(stabTol_fl, benders_obj.algOpt.top.stabTol[2][2]))
		set_optimizer_attribute(benders_obj.top.optModel, "Crossover", benders_obj.algOpt.top.crs ? 1 : 0)
		set_optimizer_attribute(benders_obj.top.optModel, "NumericFocus", benders_obj.algOpt.top.numFoc[1])
		set_optimizer_attribute(benders_obj.top.optModel, "Threads", benders_obj.algOpt.top.threads)	
	end
	solveModel!(benders_obj.top, benders_obj.top.optModel, benders_obj.algOpt.top.numFoc[1:1], benders_obj.algOpt.top.check, false)
	
	# handle unsolved top problem
	if !isnothing(stab_obj)
		opt_tup = stab_obj.methodOpt[stab_obj.actMet]
		# if infeasible and level bundle stabilization, increase level until feasible
		while stab_obj.method[stab_obj.actMet] in (:lvl2, :dsb) && !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			produceMessage(report_m.options, report_m.report, 1, " - Empty level set", testErr = false, printErr = false)
			lowBd_fl = stab_obj.objVal / benders_obj.top.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			stab_obj.dynPar[stab_obj.actMet][:yps] = (1 - opt_tup.lam) * (stab_obj.objVal - lowBd_fl*benders_obj.top.options.scaFac.obj) / benders_obj.top.options.scaFac.obj
			ell_fl = stab_obj.objVal/benders_obj.top.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			set_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1], ell_fl)
			@suppress optimize!(benders_obj.top.optModel)
        end

		while stab_obj.method[stab_obj.actMet] == :lvl1 && !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			
			# increase upper bound
			lvl_fl = stab_obj.dynPar[stab_obj.actMet]
			stab_obj.dynPar[stab_obj.actMet] = opt_tup.lam * lvl_fl + (1 - opt_tup.lam) * stab_obj.objVal / benders_obj.top.options.scaFac.obj
			set_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1], lvl_fl)
			
            # remove stabilization if difference below optimality threshold
			if (stab_obj.objVal / benders_obj.top.options.scaFac.obj) /  lvl_fl - 1 < benders_obj.algOpt.gap
				@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1])
				delete_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
			end
			@suppress optimize!(benders_obj.top.optModel)
        end

		if stab_obj.method[stab_obj.actMet] == :qtrLvl && !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			
			# solve with greater numeric focus
			produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem reported infeasible - Increased numeric focus to $(benders_obj.algOpt.top.numFoc[2])" , testErr = false, printErr = false)
			solveModel!(benders_obj.top, benders_obj.top.optModel, benders_obj.algOpt.top.numFoc[2:2], benders_obj.algOpt.top.check, false)
	
			if !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))

				# increase level parameter almost until the upper bound
				low_fl = stab_obj.dynPar[stab_obj.actMet][:lvl]
				up_fl = stab_obj.objVal / benders_obj.top.options.scaFac.obj
				upRef_fl = low_fl + (up_fl - low_fl) * (1 - benders_obj.algOpt.gap)

				if upRef_fl - low_fl > 1e-4
					lvl1_arr = collect(low_fl:((upRef_fl - low_fl) / 2):upRef_fl)[2:end]
				else
					lvl1_arr = Float64[]
				end
				
				if !isempty(lvl1_arr) && up_fl - lvl1_arr[end] > 1e-4
					lvl2_arr = collect(lvl1_arr[end]:((up_fl - lvl1_arr[end]) / 3):up_fl)[2:end-1]
				else
					lvl2_arr = Float64[]
				end
				
				for lvl_fl in vcat(lvl1_arr, lvl2_arr)
					produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem reported infeasible - Increase level bound to $(lvl_fl)" , testErr = false, printErr = false)
					# increase level parameter
					stab_obj.dynPar[stab_obj.actMet][:lvl] = lvl_fl
					set_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1], lvl_fl)
					# try to re-solve
					@suppress optimize!(benders_obj.top.optModel)
					if termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED) break end
				end
				
				if !benders_obj.stab.crossNoStab && benders_obj.algOpt.gap * (1 + benders_obj.algOpt.gap) > benders_obj.itr.gap
					benders_obj.stab.crossNoStab = true
					produceMessage(report_m.options, report_m.report, 1, " - Activated crossover when solving without stabilization to verify convergence!", testErr = false, printErr = false)
				end

				if !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))

					# increase radius of trust-region to make feasible
					produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem reported infeasible - Increase radius to $(stab_obj.dynPar[stab_obj.actMet][:qtr])" , testErr = false, printErr = false)
					removeStab!(benders_obj)
					stab_obj.dynPar[stab_obj.actMet][:qtr] = stab_obj.dynPar[stab_obj.actMet][:qtr] * 1.5
					centerStab!(stab_obj.method[stab_obj.actMet], stab_obj, benders_obj.algOpt.rngVio.stab, benders_obj.top, benders_obj.report.mod; forceRad = true)
					@suppress optimize!(benders_obj.top.optModel)

					# other steps to create feasible model
					if !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))

						produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem reported infeasible - Deleted trust-region" , testErr = false, printErr = false)
						delete(benders_obj.top.optModel, stab_obj.cns)
						solveModel!(benders_obj.top, benders_obj.top.optModel, benders_obj.algOpt.top.numFoc[2:end], benders_obj.algOpt.top.check, false)

						# solve without stabilization as last resort
						if !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
							produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem reported infeasible - Removed all stabilization" , testErr = false, printErr = false)
							@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1])
							delete_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
							numFoc_int = solveModel!(benders_obj.top, benders_obj.top.optModel, benders_obj.algOpt.top.numFoc[2:end], false)
							if numFoc_int != benders_obj.algOpt.top.numFoc[2]
								produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Finally solved top problem without stabilization and numeric focus of $(numFoc_int)" , testErr = false, printErr = false)
							end
						end
					end
				end

			end
        end

		# if no solution and proximal bundle stabilization, remove penalty term temporarily
		if stab_obj.method[stab_obj.actMet] == :prx && !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1,1])
			@suppress optimize!(benders_obj.top.optModel)
		end

		# near-optimal can be infeasible with trust-region since near-optimum constraint cannot be fulfilled
		if stab_obj.method[stab_obj.actMet] == :qtr && is_valid(benders_obj.top.optModel, stab_obj.cns)

			topSolved_boo = checkTopStatus(benders_obj.top)
			termCnt_int = 0
			 
			# extend trust-region until problem is feasible or has mustCapa error
			while !topSolved_boo
				# delte old trust-region
				if is_valid(benders_obj.top.optModel, stab_obj.cns) delete(benders_obj.top.optModel, stab_obj.cns) end
				# double radius of trust-region and enforce again
				stab_obj.dynPar[stab_obj.actMet] = max(stab_obj.methodOpt[stab_obj.actMet].low, stab_obj.dynPar[stab_obj.actMet] * stab_obj.methodOpt[stab_obj.actMet].fac)
				centerStab!(stab_obj.method[stab_obj.actMet], stab_obj, benders_obj.algOpt.rngVio.stab, benders_obj.top, benders_obj.report.mod; forceRad = true)
				# solve again
				produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Extended quadratic trust-region to solve top-problem", testErr = false, printErr = false)
				@suppress optimize!(benders_obj.top.optModel)
				topSolved_boo = checkTopStatus(benders_obj.top)
				
				termCnt_int = termCnt_int + 1
				if termCnt_int > 2 # stop extension after three tries and use solution without any stabilization instead
					produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Stopped extension of radius to solve top-problem after three tries", testErr = false, printErr = false)
					stab_obj.dynPar[stab_obj.actMet] = stab_obj.methodOpt[stab_obj.actMet].start
					if is_valid(benders_obj.top.optModel, stab_obj.cns) delete(benders_obj.top.optModel, stab_obj.cns) end
					@suppress optimize!(benders_obj.top.optModel)
					break 
				elseif topSolved_boo # reset radius after frequent extension
					stab_obj.dynPar[stab_obj.actMet] = stab_obj.methodOpt[stab_obj.actMet].start
				end
			end
		end

	end
	checkIIS(benders_obj.top)

	#endregion

	#region # * write results

	# write technology capacites and level of capacity balance to benders object
	resData_obj.capa, resData_obj.stLvl, resData_obj.lim = writeResult(benders_obj.top, [:capa, :mustCapa, :stLvl, :lim]; rmvFix = true, fltSt = false)
	bestData_obj.capa, bestData_obj.stLvl, bestData_obj.lim = writeResult(benders_obj.top, [:capa, :mustCapa, :exp, :mustExp, :stLvl, :lim]; rmvFix = true, fltSt = false, filterExc = false)
	stabVar_obj.capa, stabVar_obj.stLvl, stabVar_obj.lim = writeResult(benders_obj.top, [:capa, :exp, :stLvl, :lim]; rmvFix = true)

	resData_obj = correctMustCapa(resData_obj)

	# record level dual
	if !isnothing(stab_obj)
		if stab_obj.method[stab_obj.actMet] == :lvl2
			levelDual_fl =  dual(UpperBoundRef(benders_obj.top.parts.obj.var[:obj][1,1]))
		elseif stab_obj.method[stab_obj.actMet] == :dsb
			levelDual_fl =  dual(UpperBoundRef(benders_obj.top.optModel[:r]))
		else
			levelDual_fl =  0.0
		end
	else
		levelDual_fl = 0.0
	end
	benders_obj.itr.res[:lvlDual] = levelDual_fl

	# get costs(!) of top-problem
	benders_obj.itr.res[:topCost] = value(sum(filter(x -> x.name == :cost, benders_obj.top.parts.obj.var[:objVar])[!,:var]))
	benders_obj.itr.res[:estSubCost] = value(filter(x -> x.name == :benders, benders_obj.top.parts.obj.var[:objVar])[1,:var])
	benders_obj.itr.res[:estTotCost] = benders_obj.itr.res[:topCost] + benders_obj.itr.res[:estSubCost]
	benders_obj.itr.res[:lowLimCost] = benders_obj.itr.res[:estTotCost]

	# get object for near-optimal case
	if benders_obj.nearOpt.cnt != 0 
		benders_obj.itr.res[:nearObj] = objective_value(benders_obj.top.optModel) 
		if !isnothing(benders_obj.stab) benders_obj.itr.res[:thrStab] = 1 - normalized_rhs(benders_obj.stab.cns) / value(benders_obj.stab.cns) end
	end

	# track cuts there were not binding for a certain number of iterations
	trackCuts(benders_obj)

	# write starting levels for storage
	stLvl_dic = Dict{Symbol,DataFrame}()
	for sSym in keys(benders_obj.top.parts.tech)
		if :startStLvl in keys(benders_obj.top.parts.tech[sSym].var)
			var_df = copy(benders_obj.top.parts.tech[sSym].var[:startStLvl])
			var_df[!,:value] = value.(var_df[!,:var])
			stLvl_dic[sSym] = select(var_df,Not([:var]))
		end
	end
		
	#endregion

	return resData_obj, bestData_obj, stabVar_obj, stLvl_dic
end

# ! run sub-problem
function runSub(sub_m::anyModel, resData_obj::resData, rngVio_fl::Float64, sol_sym::Symbol, optTol_fl::Float64=1e-8, crsOver_boo::Bool=false, check_boo::Bool = false, resultOpt::NamedTuple = NamedTuple())

	str_time = now()

	#region # * fix complicating variables

	# fixing capacity
	for sys in (:tech, :exc)
		part_dic = getfield(sub_m.parts, sys)
		for sSym in keys(resData_obj.capa[sys])
			for capaSym in sort(filter(x -> occursin("capa", lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym]))), rev = true)
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], resData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(resData_obj.capa[sys][sSym][capaSym])
					delete!(resData_obj.capa[sys][sSym], capaSym)
				else
					resData_obj.capa[sys][sSym][capaSym] = limitVar!(resData_obj.capa[sys][sSym][capaSym], part_dic[sSym].var[capaSym], capaSym, part_dic[sSym], rngVio_fl, sub_m)
				end
			end
			# remove system if no capacities exist
			removeEmptyDic!(resData_obj.capa[sys], sSym)
		end
	end

	# fixing storage levels
	if !isempty(resData_obj.stLvl)
		for sSym in keys(resData_obj.stLvl)
			if sSym in keys(sub_m.parts.tech)
				part_obj = sub_m.parts.tech[sSym]
				for stType in keys(resData_obj.stLvl[sSym])
					fix_df = select(filter(x -> stType == :stLvl ? true : x.scr == sub_m.subPro[2], resData_obj.stLvl[sSym][stType]), Not([:scr]))
					resData_obj.stLvl[sSym][stType] = limitVar!(fix_df, select(part_obj.var[stType], Not([:scr])), stType, part_obj, rngVio_fl, sub_m)
					removeEmptyDic!(resData_obj.stLvl[sSym], stType)
				end
				# remove system if no storage level exists
				removeEmptyDic!(resData_obj.stLvl, sSym)
			end
		end
	end

	# fixing limiting variables
	if !isempty(resData_obj.lim)
		for limSym in keys(resData_obj.lim)
			lim_df = select(filter(x -> x.sub == sub_m.subPro, resData_obj.lim[limSym]), Not([:sub]))
			if !isempty(lim_df)
				resData_obj.lim[limSym] = limitVar!(lim_df, sub_m.parts.lim.var[limSym], limSym, sub_m.parts.lim, rngVio_fl, sub_m)
				# remove system if no storage level exists
				removeEmptyDic!(resData_obj.lim, limSym)
			end
		end
	end
	
	#endregion

	#region # * solve problem

	# set optimizer attributes and solves
	@suppress begin
		if sol_sym == :barrier
			set_optimizer_attribute(sub_m.optModel, "Method", 2)
			set_optimizer_attribute(sub_m.optModel, "Crossover", crsOver_boo ? 1 : 0)
			set_optimizer_attribute(sub_m.optModel, "BarOrder", 1)
			set_optimizer_attribute(sub_m.optModel, "BarConvTol", optTol_fl)
		elseif sol_sym == :pdhg
			set_optimizer_attribute(sub_m.optModel, "Crossover", crsOver_boo ? 1 : 0)
			set_optimizer_attribute(sub_m.optModel, "Method", 6)
			set_optimizer_attribute(sub_m.optModel, "GURO_PAR_PDHGRELTOL", optTol_fl)
		elseif sol_sym == :simplex
			set_optimizer_attribute(sub_m.optModel, "Method", 1)
			set_optimizer_attribute(sub_m.optModel, "OptimalityTol", optTol_fl)
			set_optimizer_attribute(sub_m.optModel, "Presolve", 2)
		end
	end

	# increase numeric focus if model did not solve
	numFoc_int = solveModel!(sub_m, sub_m.optModel, [0,3], check_boo, check_boo)

	# write results into files (only used once optimum is obtained)
	writeAllResults!(sub_m, resultOpt, false)

	#endregion

	#region # * extract results

	if termination_status(sub_m.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED)
		# get objective value
		scaObj_fl = sub_m.options.scaFac.obj
		resData_obj.objVal = value(sum(sub_m.parts.obj.var[:objVar][!,:var]))

		# get duals on capacity
		for sys in (:tech, :exc)
			part_dic = getfield(sub_m.parts, sys)
			for sSym in keys(resData_obj.capa[sys])
				for capaSym in filter(x -> occursin("capa", lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym])))
					if Symbol(capaSym, :BendersFix) in keys(part_dic[sSym].cns)
						scaCapa_fl = getfield(sub_m.options.scaFac, occursin("StSize", string(capaSym)) ? :capaStSize : :capa)
						resData_obj.capa[sys][sSym][capaSym] = addDual(resData_obj.capa[sys][sSym][capaSym], part_dic[sSym].cns[Symbol(capaSym, :BendersFix)], scaObj_fl / scaCapa_fl)
						# remove capacity if none exists (again necessary because dual can be zero)
						removeEmptyDic!(resData_obj.capa[sys][sSym], capaSym)
					end
				end
				# remove system if no capacities exist (again necessary because dual can be zero)
				removeEmptyDic!(resData_obj.capa[sys], sSym)
			end
		end

		# get duals on storage levels
		if !isempty(resData_obj.stLvl)
			for sSym in keys(resData_obj.stLvl)
				if sSym in keys(sub_m.parts.tech)
					part_obj = sub_m.parts.tech[sSym]
					for stType in keys(resData_obj.stLvl[sSym])
						resData_obj.stLvl[sSym][stType] = addDual(resData_obj.stLvl[sSym][stType], part_obj.cns[Symbol(stType,:BendersFix)], scaObj_fl / sub_m.options.scaFac.dispSt)
						removeEmptyDic!(resData_obj.stLvl[sSym], stType)
					end
					removeEmptyDic!(resData_obj.stLvl, sSym)
				end
			end
		end

		# get duals on limits
		if !isempty(resData_obj.lim)
			for limSym in keys(resData_obj.lim)
				resData_obj.lim[limSym] = addDual(resData_obj.lim[limSym], sub_m.parts.lim.cns[Symbol(limSym,:BendersFix)], scaObj_fl / sub_m.options.scaFac.dispConv)
				removeEmptyDic!(resData_obj.lim, limSym)
			end
		end

		# probability weighted loss-of-load
		lssProb_df = matchSetParameter(sub_m.parts.bal.var[:lss], sub_m.parts.obj.par[:scrProb], sub_m.sets)
		lss_fl = sum(lssProb_df[!,:val] .* value.(lssProb_df[!,:var]))

	else
		lss_fl = 0.0
	end
	
	# elapsed time
	elpSub_time = now() - str_time

	#endregion

	return resData_obj, elpSub_time, lss_fl, numFoc_int
end

# ! solves a model increasing the numeric focus from starting value to maximum in infeasible
function solveModel!(mod_m::anyModel, opt_mod::Model, numFoc_arr::Array{Int, 1}, check_boo::Bool = true, iss_boo::Bool = false, noStab_ntup::Union{Nothing,NamedTuple{(:opt,:ref),Tuple{Union{Model,Nothing},Union{GenericReferenceMap,Nothing}}}} = nothing)

	numFoc_int = 1
	while true
		if !check_boo 
			@suppress set_optimizer_attribute(opt_mod, "NumericFocus", numFoc_arr[numFoc_int])
		else 
			set_optimizer_attribute(opt_mod, "NumericFocus", numFoc_arr[numFoc_int])
		end
		
		if !check_boo @suppress optimize!(opt_mod) else optimize!(opt_mod) end
		if termination_status(opt_mod) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED, MOI.TIME_LIMIT) || numFoc_int == length(numFoc_arr)
			if !(termination_status(opt_mod) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED, MOI.TIME_LIMIT)) # check infeasibility, if activated
				if iss_boo
					printIIS(mod_m, noStab_ntup)
				else
					if !check_boo @suppress optimize!(opt_mod) else optimize!(opt_mod) end
				end
			end
			break
		else
			numFoc_int = numFoc_int + 1
		end
	end

	return numFoc_arr[numFoc_int]

end

# ! run sub-problem on worker (sub_m is a global variable at package scope)
function runSub(resData_obj::resData, rngVio_fl::Float64, sol_sym::Symbol, optTol_fl::Float64 = 1e-8, crsOver_boo::Bool = false, check_boo::Bool = false, resultOpt::NamedTuple = NamedTuple())
	return runSub(sub_m, resData_obj, rngVio_fl, sol_sym, optTol_fl, crsOver_boo, check_boo, resultOpt)
end

getComVar() = comVar_dic
getSubString(res_sym::Symbol) = getSubStringWorker(res_sym::Symbol) 
getSubStringWorker(res_sym::Symbol) = "$(sub_m.options.outDir)/results_" * string(res_sym) * "_$(sub_m.options.outStamp).csv"

# ! add all cuts from input dictionary to top problem
function addCuts!(top_m::anyModel, opt_mod::Model, rngVio_fl::Float64, cuts_arr::Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}, noStab_boo::Bool = false)

	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_dis = Int[], scr = Int[], limCoef = Bool[], cnsExpr = AffExpr[])
	
	# add benders variable to cut and push to dataframe of all cuts
	for cut in cuts_arr	
		push!(cut_df, (i = cut[1][1], Ts_dis = cut[1][2], scr = cut[1][3], limCoef = cut[2][2], cnsExpr = cut[2][1]))
	end

	# scale cuts and add to dataframe of benders cuts in model
	coefRng_tup = (mat = (top_m.options.coefRng.mat[1], top_m.options.coefRng.mat[2] * rngVio_fl), rhs = (top_m.options.coefRng.rhs[1], top_m.options.coefRng.rhs[2] * rngVio_fl))
	scaleCnsExpr!(cut_df, coefRng_tup, top_m.options.checkRng)
	append!(top_m.parts.obj.cns[noStab_boo ? :bendersCutsNoStab : :bendersCuts], createCns(cnsCont(cut_df, :smaller), opt_mod, false))
end

# ! update results and stabilization
function updateIteration!(benders_obj::bendersObj, cutData_dic::Dict{Tuple{Int64,Int64},resData}, bestData_obj::resData, curRes_dic::Dict{Symbol,DataFrame}, stabVar_obj::resData, stLvl_dic::Dict{Symbol, DataFrame})

	itr_obj = benders_obj.itr
	best_obj = itr_obj.best
	nameStab_dic = Dict(:lvl1 => "level bundle", :lvl2 => "level bundle", :qtr => "quadratic trust-region", :prx => "proximal bundle", :box => "box-step", :qtrLvl => "level bundle with trust-region")

	# filter unsolved SPs
	infeasSub_arr = getindex.(filter(x -> x[2].objVal == Inf, collect(cutData_dic)),1)
	filter!(x -> !(x[1] in infeasSub_arr), cutData_dic)
	for sub in infeasSub_arr 
		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Could not solve sub-problem $sub and did not add a cut!", testErr = false, printErr = false) 
	end

	# create cuts for top problem with stabilization
	colCuts_arr = Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}() 
	exExpr_arr = getindex.(getindex.(benders_obj.cuts.all, 2), 1)
	for cut in collect(cutData_dic)
		cut_expr, limCoef_boo = createCutExpr(cut, benders_obj.top.optModel, benders_obj.algOpt.rngVio.cut, benders_obj.top)
		
		if isempty(findall(cut_expr .== exExpr_arr)) # add to overall cuts if unique
			push!(colCuts_arr, (benders_obj.itr.cnt.i, cut[1][1], cut[1][2])  => (cut_expr, limCoef_boo))
		else # otherwise remove to prevent addition problem without stabilization
			delete!(cutData_dic, cut[1])
		end
	end

	append!(benders_obj.cuts.slack, map(x -> Float64[], 1:length(colCuts_arr)))
	append!(benders_obj.cuts.active, collect(length(benders_obj.cuts.all) : length(benders_obj.cuts.all) + length(colCuts_arr) - 1) .+ 1)
	append!(benders_obj.cuts.all, colCuts_arr)

	# create and directly add cuts for top problem without stabilization
	if !isnothing(benders_obj.stab) 
		colCutsNoStab_arr = Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}() 
		for cut in collect(cutData_dic)
			cut_expr, limCoef_boo = createCutExpr(cut, benders_obj.topNoStab.opt, benders_obj.algOpt.rngVio.cut, benders_obj.top, benders_obj.topNoStab.ref)
			push!(colCutsNoStab_arr, (benders_obj.itr.cnt.i, cut[1][1], cut[1][2])  => (cut_expr, limCoef_boo))
		end
		addCuts!(benders_obj.top, benders_obj.topNoStab.opt, benders_obj.algOpt.rngVio.cut, colCutsNoStab_arr, true)
	end


	# get sub-results
	itr_obj.res[:actSubCost] = sum(map(x -> x.objVal, values(cutData_dic))) # objective of sub-problems
	itr_obj.res[:actTotCost] = itr_obj.res[:topCost] + itr_obj.res[:actSubCost]

	# update current best
	if benders_obj.nearOpt.cnt == 0 ? (itr_obj.res[:actTotCost] < best_obj.var.objVal) : (itr_obj.res[:nearObj] <= best_obj.var.objVal && itr_obj.gap <= benders_obj.algOpt.gap)
		best_obj.var.objVal = benders_obj.nearOpt.cnt == 0 ? itr_obj.res[:actTotCost] : itr_obj.res[:nearObj]
		best_obj.var.capa, best_obj.var.stLvl, best_obj.var.lim = map(x -> getfield(bestData_obj,x), [:capa, :stLvl, :lim])
		@suppress foreach(x -> best_obj.res[x] = curRes_dic[x], benders_obj.report.res.general)
		itr_obj.res[:curBest] = best_obj.var.objVal
		foreach(x -> best_obj.startLvl[x] = stLvl_dic[x], keys(stLvl_dic))
	end

	# computes optimality gap for cost minimization and feasibility gap for near-optimal
	itr_obj.gap = benders_obj.nearOpt.cnt == 0 ? (1 - itr_obj.res[:lowLimCost] / itr_obj.res[:curBest]) : abs((itr_obj.res[:actSubCost] - itr_obj.res[:estSubCost]) / itr_obj.res[:optCost])

	# adapt center and parameter for stabilization
	if !isnothing(benders_obj.stab)
		stab_obj = benders_obj.stab
		report_m = benders_obj.report.mod
		
		# determine if serious step 
		expStep_fl = best_obj.var.objVal - (benders_obj.nearOpt.cnt == 0 ? itr_obj.res[:estTotCost] : 0.0) # expected step size
		srsStep_boo = false

		if best_obj.var.objVal < stab_obj.objVal - stab_obj.srsThr * expStep_fl
			srsStep_boo = true
		end

		# initialize counters
		itr_obj.cnt.srs = srsStep_boo ? itr_obj.cnt.srs + 1 : 0
		itr_obj.cnt.null = srsStep_boo ? 0 : itr_obj.cnt.null + 1

		# adjust dynamic parameters of stabilization
		prx2Aux_fl = stab_obj.method[stab_obj.actMet] == :prx2 ? computePrx2Aux(benders_obj.cuts.allStab[benders_obj.cuts.prev]) : nothing
		foreach(x -> adjustDynPar!(x, benders_obj.stab, benders_obj.top, itr_obj, srsStep_boo, prx2Aux_fl, benders_obj.nearOpt.cnt != 0, benders_obj.algOpt.gap, benders_obj.report), 1:length(stab_obj.method))

		# update center of stabilization
		if srsStep_boo # update everything in case of serios step
			stab_obj.var = filterStabVar(stabVar_obj.capa, stabVar_obj.stLvl, stabVar_obj.lim, stab_obj.weight, benders_obj.top)
			stab_obj.objVal = best_obj.var.objVal
			produceMessage(report_m.options, report_m.report, 1, " - Updated reference point for stabilization!", testErr = false, printErr = false)
		end

		# switch stabilization method
		if !isnothing(benders_obj.stab)
			stab_obj = benders_obj.stab
			# switch stabilization method
			if !isempty(stab_obj.ruleSw) && itr_obj.cnt.i > stab_obj.ruleSw.itr && length(stab_obj.method) > 1
				if checkSwitch(stab_obj, itr_obj.cnt, benders_obj.report.itr)
					stab_obj.actMet = stab_obj.actMet + 1 |> (x -> length(stab_obj.method) < x ? 1 : x)
					produceMessage(report_m.options, report_m.report, 1, " - Switched stabilization to $(nameStab_dic[stab_obj.method[stab_obj.actMet]]) method!", testErr = false, printErr = false)
				end
			end
			
			# update stabilization method
			stabVio_df, benders_obj.cuts.qtrInfo = centerStab!(stab_obj.method[stab_obj.actMet], stab_obj, benders_obj.algOpt.rngVio.stab, benders_obj.top, report_m)
			stabVio_df[!,:i] .= itr_obj.cnt.i
			append!(benders_obj.report.stabVio, stabVio_df)
		end
	end
	
end

# ! check if algorithm converged and switch objective or terminate
function checkConvergence(benders_obj::bendersObj, lss_dic::Dict{Tuple{Int64,Int64},Float64})

	itr_obj = benders_obj.itr
	report_m = benders_obj.report.mod
	rtn_boo = false

	# check for termination
	if benders_obj.itr.gap < benders_obj.algOpt.gap
		# switch from cost minimization to near-optimal
		if !isnothing(benders_obj.nearOpt.setup) && benders_obj.nearOpt.cnt < length(benders_obj.nearOpt.setup.obj) 
			if benders_obj.nearOpt.cnt == 0
				# get characteristics of optimal solution
				itr_obj.res[:optCost] = itr_obj.best.objVal
				itr_obj.res[:optLss] = sum(collect(values(lss_dic)))
				# compute threshold for previous iterations
				thrs_df = filter(x -> x.variable == :cost, benders_obj.report.nearOpt)
				thrs_df[!,:value] = 1 .- itr_obj.res[:optCost] ./ thrs_df[!,:value]
				thrs_df[!,:variable] .= :thrs
				# add entries with threshold to overall dataframe
				append!(benders_obj.report.nearOpt, thrs_df)
				# check where costs and lss comply with near-optimal definition
				costNear_arr = filter(x -> x.variable == :thrs && x.value <= benders_obj.nearOpt.setup.cutThres, benders_obj.report.nearOpt)[!,:i]
				lssNear_arr = filter(x -> x.variable == :lss  && x.value <= itr_obj.res[:optLss] * (1 + benders_obj.nearOpt.setup.lssThres), benders_obj.report.nearOpt)[!,:i]
				# filter cases where both requirements are met
				filter!(x -> x.i in intersect(costNear_arr, lssNear_arr), benders_obj.report.nearOpt)
				# re-set gap
				benders_obj.algOpt.gap = benders_obj.nearOpt.setup.feasGap
			end
			# reset iteration variables
			itr_obj.gap = 1.0
			itr_obj.res[:nearObj] = Inf
			itr_obj.best.objVal = Inf

			# remove stabilization for near-optimum
			if !isnothing(benders_obj.stab) 
				removeStab!(benders_obj)
				benders_obj.stab = nothing 
			end 

			benders_obj.nearOpt.cnt = benders_obj.nearOpt.cnt + 1 # update near-opt counter
			# adapt the objective and constraint to near-optimal
			adaptNearOpt!(benders_obj.top, benders_obj.nearOpt.setup, itr_obj.res[:optCost], benders_obj.nearOpt.cnt)
			produceMessage(report_m.options, report_m.report, 1, " - Switched to near-optimal for $(benders_obj.nearOpt.setup.obj[benders_obj.nearOpt.cnt ][1])", testErr = false, printErr = false)
		# finish iteration
		else 
			if benders_obj.stab.crossNoStab
				rtn_boo = true
				produceMessage(report_m.options, report_m.report, 1, " - Finished iteration!", testErr = false, printErr = false)
			elseif !benders_obj.stab.crossNoStab
				benders_obj.stab.crossNoStab = true
				produceMessage(report_m.options, report_m.report, 1, " - Activated crossover when solving without stabilization to verify convergence!", testErr = false, printErr = false)
			end
		end
	elseif Dates.value(floor(now() - report_m.options.startTime, Dates.Minute(1))) > benders_obj.algOpt.timeLim
		rtn_boo = true
	else # reset option for solve without stabilization with crossover
		if !isnothing(benders_obj.stab) benders_obj.stab.crossNoStab = false end
	end

	return rtn_boo
end

# ! run iteration
function runIteration!(benders_obj::bendersObj, runSubDist::Function)

	allRes_df = DataFrame(i = Int[], Ts_expSup = Int[], Ts_disSup = Int[], Ts_dis = Int[], R_dis = Int[], R_exp = Int[], R_from = Int[], R_to = Int[], C = Int[], Te = Int[], Exc = Int[], M = Int[], scr = Int[], id = Int[], sub = Tuple[], variable = Symbol[], value = Float64[])

	while true

		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Started iteration $(benders_obj.itr.cnt.i)", testErr = false, printErr = false)
	
		#region # * solve top-problem and (start) sub-problems
		str_time = now()
		resData_obj, bestData_obj, stabVar_obj, stLvl_dic = runTop(benders_obj);
		elpTop_time = now() - str_time
	
		# start solving sub-problems
		cutData_dic = Dict{Tuple{Int64,Int64},resData}()
		timeSub_dic = Dict{Tuple{Int64,Int64},Millisecond}()
		lss_dic = Dict{Tuple{Int64,Int64},Float64}()
		numFoc_dic = Dict{Tuple{Int64,Int64},Int64}()
	
		acc_fl = interItrPar(benders_obj.itr.gap, benders_obj.algOpt.gap, benders_obj.algOpt.sub.rng, benders_obj.algOpt.sub.int)
	
		if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
		for (id,s) in enumerate(sort(collect(keys(benders_obj.sub))))
			if benders_obj.algOpt.dist # distributed case
				futData_dic[s] = runSubDist(id + 1, copy(resData_obj), benders_obj.algOpt.rngVio.fix, benders_obj.algOpt.sub.meth, acc_fl, benders_obj.algOpt.sub.crs, benders_obj.algOpt.sub.check)
			else # non-distributed case
				cutData_dic[s], timeSub_dic[s], lss_dic[s], numFoc_dic[s] = runSub(benders_obj.sub[s], copy(resData_obj), benders_obj.algOpt.rngVio.fix, benders_obj.algOpt.sub.meth, acc_fl, benders_obj.algOpt.sub.crs, benders_obj.algOpt.sub.check)
			end
		end

		# save current results
		curRes_dic = Dict(x => reportResults(x, benders_obj.top, rtnOpt = (:csvDf,), rmvZero = false) for x in benders_obj.report.res.general)
	
		# top-problem without stabilization
		strNoStab_time = now()
		if !isnothing(benders_obj.stab) 
			# remove stabilization from top problem (to be added again at the end of iteration)
			removeStab!(benders_obj)
			# check if top problem without stabilization should be solved again 
			if benders_obj.itr.cnt.i >= benders_obj.itr.cnt.nextNoStab || benders_obj.stab.crossNoStab
				runTopWithoutStab!(benders_obj)
				# compute next iteration to solve top problem
				par_ntup = benders_obj.stab.solveNoStab
				gap_fl = 1 - benders_obj.itr.res[:lowLimCost] / benders_obj.itr.res[:curBest]
				waitTopNoStab_int = max(1, Int(floor(interItrPar(gap_fl, benders_obj.algOpt.gap, [par_ntup.upper,1], par_ntup.inter, par_ntup.sub))))
				benders_obj.itr.cnt.nextNoStab = benders_obj.itr.cnt.i + waitTopNoStab_int
				# only report, if problem without stabilization is not solved again in the next iteration
				if waitTopNoStab_int != 1
					produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Solved top problem without stabilization. Next solve in iteration $(benders_obj.itr.cnt.nextNoStab)", testErr = false, printErr = false)
				end
			else
				# use results of last correct solve as lower bound
				benders_obj.itr.res[:lowLimCost] = benders_obj.itr.res[:estTotCostNoStab]
			end

		end
		elpNoStab_time = now() - strNoStab_time
	
		# get results of sub-problems
		if benders_obj.algOpt.dist
			wait.(collect(values(futData_dic)))
			for s in sort(collect(keys(benders_obj.sub)))
				cutData_dic[s], timeSub_dic[s], lss_dic[s], numFoc_dic[s] = fetch(futData_dic[s])
			end
		end
		
		#endregion
	
		#region # * analyse results and update refinements
	
		# update results and stabilization
		updateIteration!(benders_obj, cutData_dic, bestData_obj, curRes_dic, stabVar_obj, stLvl_dic)
		# report on iteration
		reportBenders!(benders_obj, resData_obj, elpTop_time, elpNoStab_time, timeSub_dic, lss_dic, numFoc_dic)
	
		# check convergence and finish
		rtn_boo = checkConvergence(benders_obj, lss_dic)

		# delete cuts that not were binding for the defined number of iterations
		deleteCuts!(benders_obj)

		# track capacity over iterations if activated
		if benders_obj.trackCapa reportComplVar!(allRes_df, resData_obj, benders_obj.itr.cnt.i) end

		#endregion

		benders_obj.itr.cnt.i = benders_obj.itr.cnt.i + 1
		if rtn_boo break end
		
	end

	# apply weights to tracked capacities
	if benders_obj.trackCapa
		w_dic = Dict(:capaConv => benders_obj.stab.weight.capa, :mustCapaConv => benders_obj.stab.weight.capa, :capaStSize => benders_obj.stab.weight.capaStSize, :capaStOut => benders_obj.stab.weight.capa, :capaStIn => benders_obj.stab.weight.capa, :capaExc => benders_obj.stab.weight.capa,
																																						:stLvl => benders_obj.stab.weight.stLvl, :stLvlInter => benders_obj.stab.weight.stLvl, :emissionBendersCom => benders_obj.stab.weight.lim)
		allRes_df[!,:value] = map(x -> x.value * w_dic[x.variable], eachrow(allRes_df))
	end

	return allRes_df

end

# ! prepare stabilization
function prepareStab!(benders_obj::bendersObj, stabSetup_obj::stabSetup, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, info_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, scale_dic::Dict{Symbol, NamedTuple}, runSubDist::Function)

	benders_obj.stab, curBest_tup = initializeStab!(benders_obj, stabSetup_obj, inputFolder_ntup, info_ntup, scale_dic, runSubDist)
	benders_obj.itr = itrStatus(curBest_tup, countItr(isempty(benders_obj.report.itr) ? 0 : maximum(benders_obj.report.itr[!,:i]) + 1, 0, 0, 0), 1.0, Dict{Symbol,Float64}())
	benders_obj.itr.res[:curBest] = curBest_tup.var.objVal
end

# ! write complicating constraints into top problem
function writeComplCons!(benders_obj::bendersObj)

	top_m = benders_obj.top
	report_m = benders_obj.report.mod
	complCns_dic = benders_obj.complVar
	relVar_arr = unique(vcat(filter(x -> !isempty(x), map(x -> collect(keys(complCns_dic[x])), collect(keys(complCns_dic))))...))

	# loop over types of complicating variables
	if !isempty(relVar_arr)
		addComplCns!(benders_obj.top, relVar_arr, complCns_dic)
		push!(top_m.report, (2, "limit", "", "enforced at least one limit across scenarios which creates a complicating constraint, Benders can not converge in case of overlapping complicating constraints (e.g., a national and system-wide emission limit)"))
		errorTest(unique(top_m.report), top_m.options, write = true)
		produceMessage(report_m.options, report_m.report, 1, " - Added complicating constraints to top-problem", testErr = false, printErr = false)
	end

end

# ! initialize reporting objects
function initializeReporting!(benders_obj::bendersObj, stabSetup_obj::stabSetup, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, info_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, resInfo::NamedTuple)

	# dataframe for reporting during iteration
	itrReport_df = DataFrame(i = Int[], lowCost = Float64[], bestObj = Float64[], gap = Float64[], curCost = Float64[], time_ges = Float64[], time_top = Float64[], time_waitNoStab = Float64[], time_subTot = Float64[], time_sub = Array{Float64,1}[], cntCuts = Int[], numFoc = Array{Int,1}[], objName = String[])
	nearOpt_df = DataFrame(i = Int[], timestep = String[], region = String[], system = String[], id = String[], variable = Symbol[], value = Float64[], objName = String[])
	stabVio_df = DataFrame(i = Int[], var = String[], fac = Float64[], type = Symbol[])

	# empty model just for reporting
	report_m = @suppress anyModel(String[], inputFolder_ntup.results, objName = "decomposition_" * info_ntup.name) 

	# add column for active stabilization method
	if !isempty(stabSetup_obj.method)
		itrReport_df[!,:actMethod] = fill(Symbol(), size(itrReport_df, 1))
		foreach(x -> itrReport_df[!,Symbol("dynPar_", x[1])] = Union{Float64,Vector{Float64}}[fill(Float64[], size(itrReport_df, 1))...], stabSetup_obj.method)
		select!(itrReport_df, vcat(filter(x -> x != :objName, namesSym(itrReport_df)), [:objName]))
	end

	# extend reporting dataframe in case of near-optimal
	if !isnothing(benders_obj.nearOpt.setup) itrReport_df[!,:objective] = fill("", size(itrReport_df, 1)) end

	benders_obj.report = (itr = itrReport_df, nearOpt = nearOpt_df, stabVio = stabVio_df, res = resInfo, mod = report_m)

end

#endregion

#region # * data management

# merge all entries of dictionary used for capacity data into one dataframe for the specified columns
mergeVar(var_dic::Dict{Symbol,Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}}, outCol::Array{Symbol,1}) = vcat(vcat(vcat(map(x -> var_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,outCol], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)

# get dataframe with variables, values, and scaling factors for stabilization
function getStabDf(stab_obj::stabObj, top_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(deepcopy(stab_obj.var), stab_obj.weight, top_m)
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var, :value, :scaFac]], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)
	allStLvl_df = vcat(vcat(map(x -> expExpr_dic[:stLvl][x] |> (u -> map(y -> u[y], collect(keys(u)))), collect(keys(expExpr_dic[:stLvl])))...)...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)
	allLim_df = vcat(map(x -> select(expExpr_dic[:lim][x], [:var, :value, :scaFac]), collect(keys(expExpr_dic[:lim])))...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)

	# filter zero scaling factors
	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df, allStLvl_df, allLim_df))
	
	# normalize scaling factors
	allVar_df[!,:scaFac] .= allVar_df[!,:scaFac] ./ maximum(allVar_df[!,:scaFac])

	return allVar_df
end

# ! matches values in dictionary with variables of provided problem
function matchValWithVar(var_dic::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}, weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl,:lim), NTuple{4, Float64}}, mod_m::anyModel, prsvExp::Bool=false)
	
	expExpr_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()
	
	# match capacity values with variables
	expExpr_dic[:capa] = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech, :exc)
		expExpr_dic[:capa][sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(mod_m.parts, sys)
		for sSym in keys(var_dic[:capa][sys])
			expExpr_dic[:capa][sys][sSym] = Dict{Symbol,DataFrame}()
			for varSym in keys(var_dic[:capa][sys][sSym])
				val_df = deSelect(var_dic[:capa][sys][sSym][varSym])
				# get scaling factor for variables (corrects scaling within model going back to standard units and apply weights defined for stabilization)
				modSca_fl =  getfield(mod_m.options.scaFac, occursin("exp", string(varSym)) ? :insCapa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))
				wgtSca_fl =  getfield(weight_ntup, occursin("exp", string(varSym)) ? :capa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))			
				val_df[!,:value] = val_df[!,:value] ./ modSca_fl # correct value with scaling factor for variable
				# join variables and values
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp, :var, :value] : [:Ts_disSup, :var, :value]) : [:var, :value]
				join_df = unique(select(innerjoin(deSelect(part_dic[sSym].var[varSym]), val_df, on = intCol(val_df, :dir)), sel_arr))			
				join_df[!,:scaFac] .=  wgtSca_fl^2	
				expExpr_dic[:capa][sys][sSym][varSym] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df, :Ts_exp => :Ts_disSup) : join_df
			end
		end
	end

	# match storage level values with variables
	expExpr_dic[:stLvl] = Dict{Symbol,Dict{Symbol,DataFrame}}()

	if !isempty(var_dic[:stLvl])
		for sSym in keys(var_dic[:stLvl])
			if sSym in keys(mod_m.parts.tech)
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp, :var, :value] : [:Ts_disSup, :var, :value]) : [:var, :value]
				expExpr_dic[:stLvl][sSym] = Dict{Symbol,DataFrame}() 
				for stType in keys(var_dic[:stLvl][sSym])
					val_df = var_dic[:stLvl][sSym][stType]
					val_df[!,:value] = val_df[!,:value] ./ mod_m.options.scaFac.dispSt
					join_df = select(innerjoin(val_df, mod_m.parts.tech[sSym].var[stType], on  = intCol(var_dic[:stLvl][sSym][stType])), sel_arr)
					join_df[!,:scaFac] .= weight_ntup.stLvl^2
					expExpr_dic[:stLvl][sSym][stType] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df, :Ts_exp => :Ts_disSup) : join_df
				end
			end
		end
	end

	# match limit values with variables
	expExpr_dic[:lim] = Dict{Symbol,DataFrame}()
	
	if !isempty(var_dic[:lim])
		for limSym in keys(var_dic[:lim])
			val_df = var_dic[:lim][limSym] 
			val_df[!,:value] = val_df[!,:value] ./ mod_m.options.scaFac.dispConv
			
			join_df = orderDf(select(innerjoin(val_df, mod_m.parts.lim.var[limSym], on  = intCol(var_dic[:lim][limSym], :sub)), vcat(intCol(val_df),[:var, :value])))
			join_df[!,:scaFac] .= weight_ntup.lim^2	
			
			expExpr_dic[:lim][limSym] = join_df
		end
	end

	return expExpr_dic
end

# ! write values of entire variables in input model to returned capacity dictionary
function writeResult(in_m::anyModel, var_arr::Array{Symbol,1}; rmvFix::Bool = false, fltSt::Bool = true, filterExc::Bool = true, roundDown::Int = 0)
	
	# write expansion value
	capa_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	
	for sys in (:tech, :exc)
		capa_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts, sys)
		for sSym in filter(x -> part_dic[x].type in (:stock, :mature, :emerging), keys(part_dic))
			
			# continue in case of technology without changing capacites
			if part_dic[sSym].type == :stock && part_dic[sSym].decomm == :none && !(:capaStSizeSeason in keys(part_dic[sSym].var)) continue end	

			varSym_arr = filter(x -> any(occursin.(string.(var_arr), string(x))) && !(x in (:stLvl, :stLvlInter, :capaStSizeInter)), keys(part_dic[sSym].var))

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
					capa_dic[sys][sSym][stVar] = removeFixStorage(stVar, capa_dic[sys][sSym][stVar], part_dic[sSym])
					if isempty(capa_dic[sys][sSym][stVar]) delete!(capa_dic[sys][sSym], stVar) end
				end
			end

			# removes variables that are fixed from output
			if rmvFix
				for varSym in varSym_arr
					must_boo = occursin("must", string(varSym))
					fixVar_sym = part_dic[sSym].decomm == :none ? Symbol(replace(string(varSym), must_boo ? "Capa" => "Exp" : "capa" => "exp")) : varSym
					if Symbol(fixVar_sym, "BendersFix") in collect(keys(part_dic[sSym].cns))
						var_df = capa_dic[sys][sSym][varSym]
						expCns_df = select(part_dic[sSym].cns[Symbol(fixVar_sym, "BendersFix")], Not([:cns, :fac]))
						# in case of no decommissioning capacites are fixed if all corresponding expansion variables are fixed
						if part_dic[sSym].decomm == :none	
							# get non-fixed cases for expansion variables
							expCnsY_df = antijoin(select(part_dic[sSym].var[fixVar_sym], Not([:var])), expCns_df, on = intCol(expCns_df))
							if !isempty(expCnsY_df)
								expCnsY_df = select(flatten(expCnsY_df, :Ts_disSup), Not(:Ts_exp))
								join_arr = part_dic[sSym].type == :emerging ? intCol(var_df) : filter(x -> x != :Ts_expSup, intCol(var_df))
								capa_dic[sys][sSym][varSym] = innerjoin(var_df, unique(select(expCnsY_df, join_arr)), on = join_arr)
							else
								capa_dic[sys][sSym][varSym] = DataFrame()
							end
						else
							capa_dic[sys][sSym][varSym] = innerjoin(var_df, expCns_df, on = intCol(var_df))
						end
						
					end
				end
			end

			# removes redundant variables for undirected exchange capacity
			if sys == :exc && !part_dic[sSym].dir && :capaExc in keys(capa_dic[sys][sSym]) && filterExc
				filter!(x -> x.R_from < x.R_to, capa_dic[sys][sSym][:capaExc])
			end

			if roundDown != 0
				for varSym in varSym_arr
					capa_dic[sys][sSym][varSym][!,:value] = floor.(capa_dic[sys][sSym][varSym][!,:value], digits = roundDown)
				end
			end

			# remove empty fields
			filter!(x -> !isempty(x[2]), capa_dic[sys][sSym])
			removeEmptyDic!(capa_dic[sys], sSym)	
		end
	end

	# write storage levels in case of reduced foresight
	stLvl_dic = Dict{Symbol,Dict{Symbol,DataFrame}}()

	if :stLvl in var_arr && in_m.options.frsLvl != 0
		for sSym in keys(in_m.parts.tech)
			stLvl_dic[sSym] = Dict{Symbol,DataFrame}()
			for stType in (:stLvl, :stLvlInter)
				if stType in keys(in_m.parts.tech[sSym].var)
					stLvl_dic[sSym][stType] = getResult(copy(in_m.parts.tech[sSym].var[stType]); pos_boo = stType == :stLvl)
					removeEmptyDic!(stLvl_dic[sSym], stType)
				end
			end
			removeEmptyDic!(stLvl_dic, sSym)
		end
		
	end

	comLim_dic = Dict{Symbol,DataFrame}()

	if :lim in var_arr
		for lim in filter(x -> occursin("BendersCom", string(x)), keys(in_m.parts.lim.var))
			comLim_dic[lim] = getResult(copy(in_m.parts.lim.var[lim]); pos_boo = false)
		end
	end

	return capa_dic, stLvl_dic, comLim_dic
end

# ! replaces the variable column with a column storing the value of the entire variable
function getResult(res_df::DataFrame; pos_boo::Bool = true)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		# aggregates expansion, if spread across different years
		res_df = combine(groupby(res_df, filter(x -> x != :Ts_expSup, intCol(res_df))), :var => (x -> x[1] * size(x, 1)) => :var)
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms), res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> (pos_boo ? max(0, value(x) - x.constant) : (value(x) - x.constant)) |> (y -> round(y, digits = 12)), res_df[!,:var])

	return select(res_df, Not([:var]))
end

# ! create constraint fixing capacity (or setting a lower limits)
function limitVar!(value_df::DataFrame, var_df::DataFrame, var_sym::Symbol, part_obj::AbstractModelPart, rngVio_fl::Float64, fix_m::anyModel, lim_sym::Symbol=:Fix)

	# compute smallest and biggest capacity that can be enforced
	rngMat_tup = fix_m.options.coefRng.mat
	rngRhs_tup = fix_m.options.coefRng.rhs

	cns_sym = Symbol(:Benders, lim_sym)

	# join variables with capacity values
	fix_df = deSelect(value_df) |>  (z -> leftjoin(var_df, z, on = intCol(z, :dir))) |> (y -> y[completecases(y), :])

	# correct values with scaling factor
	mapScaFac_arr = ["stlvl" => :dispSt, "exp" => :insCapa, "stsize" => :capaStSize, "benderscom" => :dispConv]
	scaFac_sym = occursin.(getindex.(mapScaFac_arr,1), lowercase(string(var_sym)))|> (z -> any(z) ? getindex.(mapScaFac_arr,2)[findall(z)[1]] : :capa)
	fix_df[!,:value]  = round.(fix_df[!,:value] ./ getfield(fix_m.options.scaFac, scaFac_sym), sigdigits = 10)
	
	# filter cases where no variable exists
	filter!(x -> !isempty(x.var.terms), fix_df)
	if isempty(fix_df) return value_df end

	# extract actual variable
	fix_df[!,:var] = map(x -> collect(keys(x.var.terms))[1], eachrow(fix_df))
	
	# find cases where capa cannot be set to zero, because it is linked to a non-zero mustCapa
	if occursin("capa", string(var_sym)) && Symbol(replace(string(var_sym), "capa" => "mustCapa"), "BendersFix") in keys(part_obj.cns)
		nonZero_df = part_obj.cns[Symbol(replace(string(var_sym), "capa" => "mustCapa"), "BendersFix")]
		nonZero_df = filter(x -> normalized_rhs(x.cns) != 0.0, nonZero_df)
		nonZero_df[!,:setZero] .= false
		fix_df = joinMissing(fix_df, select(nonZero_df, Not([:cns, :fac])), intCol(fix_df), :left, Dict(:setZero => :true))
	else
		fix_df[!,:setZero] .= true
	end

	# comptue factor and rhs, values below enforceable range are set to zero, values are above are set to largest value possible
	fix_df[!,:fac] = map(x -> abs(x.value) != 0.0 && abs(x.value) < rngRhs_tup[1] / rngVio_fl ? rngRhs_tup[1] / rngVio_fl / x.value : (abs(x.value) > rngRhs_tup[2] * rngVio_fl ? rngRhs_tup[2] * rngVio_fl / x.value : 1.0), eachrow(fix_df))
	fix_df[!,:rhs], fix_df[!,:fac] = map(x -> abs(x.fac) < rngMat_tup[1] / rngVio_fl ?  [rngRhs_tup[2] * rngVio_fl, rngMat_tup[1] / rngVio_fl] : (abs(x.fac) > rngMat_tup[2] * rngVio_fl && x.setZero ? [0.0, 1.0] : [x.value * x.fac, x.fac]), eachrow(fix_df)) |> (w  -> map(x -> getindex.(w, x), [1, 2]))

	if !(Symbol(var_sym, cns_sym) in keys(part_obj.cns))
		# create actual constraint and attach to model part
		if lim_sym == :Fix
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel, x.var * x.fac == x.rhs), eachrow(fix_df))
		elseif lim_sym == :Up
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel, x.var * x.fac <= x.rhs), eachrow(fix_df))
		else lim_sym == :Low
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel, x.var * x.fac >= x.rhs), eachrow(fix_df))
		end
	else
		# adjust rhs and factor of existing constraint
		fix_df = innerjoin(select(part_obj.cns[Symbol(var_sym, cns_sym)], Not([:fac])), select(fix_df, intersect(namesSym(fix_df),vcat(intCol(fix_df,:dir),[:var,:value,:setZero,:fac,:rhs]))), on = intCol(fix_df, :dir))
		set_normalized_rhs.(fix_df[!,:cns], fix_df[!,:rhs])
		set_normalized_coefficient.(fix_df[!,:cns], fix_df[!,:var], fix_df[!,:fac])	
	end
	
	part_obj.cns[Symbol(var_sym, cns_sym)] = select(fix_df, Not([:var, :value, :rhs, :setZero]))

	# correct value_df to values actually enforced
	value_df = innerjoin(select(value_df, Not([:value])), select(fix_df, Not([:var, :value, :cns, :setZero])), on = intCol(value_df, :dir))
	value_df[!,:value] .=  value_df[!,:rhs] ./ value_df[!,:fac] .* getfield(fix_m.options.scaFac, scaFac_sym)
	select!(value_df, Not([:fac, :rhs]))

	return value_df
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame, cns_df::DataFrame, scaFac_fl::Float64)
	new_df = deSelect(cns_df) |> (z -> innerjoin(dual_df, z, on = intCol(z, :dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac] .* scaFac_fl
	return select(filter(x -> x.dual != 0.0, new_df), Not([:cns,:fac]))
end

# ! generate cut expression
function createCutExpr(cut::Pair{Tuple{Int64,Int64},resData}, opt_mod::Model, rngVio_fl::Float64, top_m::anyModel, noStab_map::Union{Nothing,GenericReferenceMap} = nothing)

	subCut = cut[2]
	cutExpr_arr = Array{GenericAffExpr,1}()
	# compute cut element for each capacity
	for sys in (:tech, :exc)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(subCut.capa[sys]), capaSym in filter(x -> occursin("capa", lowercase(string(x))), collect(keys(subCut.capa[sys][sSym])))
			scaCapa_fl = getfield(top_m.options.scaFac, occursin("StSize", string(capaSym)) ? :capaStSize : :capa)
			push!(cutExpr_arr, getBendersCut(subCut.capa[sys][sSym][capaSym], part_dic[sSym].var[capaSym], scaCapa_fl, noStab_map))
		end
	end

	# compute cut element for each storage level
	if !isempty(subCut.stLvl)
		for sSym in keys(subCut.stLvl)
			if sSym in keys(top_m.parts.tech)
				part_obj = top_m.parts.tech[sSym]
				for stType in keys(subCut.stLvl[sSym])
					if stType == :stLvlInter
						var_df = filter(x -> x.scr == cut[1][2], part_obj.var[stType])
					else
						var_df = part_obj.var[stType]
					end
					push!(cutExpr_arr, getBendersCut(subCut.stLvl[sSym][stType], var_df, top_m.options.scaFac.dispSt, noStab_map))
				end
			end
		end
	end

	# compute cut element for each limit
	if !isempty(subCut.lim)
		for limSym in keys(subCut.lim)
			push!(cutExpr_arr, getBendersCut(subCut.lim[limSym], filter(x -> x.sub == cut[1], top_m.parts.lim.var[limSym]), top_m.options.scaFac.dispConv, noStab_map)) 
		end
	end

	# get cut variable and compute cut expression
	cut_var = filter(x -> x.Ts_dis == cut[1][1] && x.scr == cut[1][2], top_m.parts.obj.var[:cut])[1,:var]
	if !isnothing(noStab_map) cut_var = convertAffExpr(cut_var, noStab_map) end
	cut_expr = @expression(opt_mod, subCut.objVal + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)))

	#region # * remove extremely small terms and limit the coefficient of extremely large terms
	limCoef_boo = false

	if typeof(cut_expr) == AffExpr && !isempty(cut_expr.terms)

		# ! ensure cut variable complies with limits on rhs
		cutFac_fl = abs(collect(values(cut_var.terms))[1]) # get scaling factor of cut variable
		scaRng_tup = (top_m.options.coefRng.rhs[1], top_m.options.coefRng.rhs[2] * rngVio_fl) ./ abs(cut_expr.constant) # get smallest and biggest scaling factors where rhs is still in range
			
		# adjust rhs to avoid violation of range only from cut variable and rhs
		if top_m.options.coefRng.mat[2] / cutFac_fl < scaRng_tup[1]
			cut_expr.constant = top_m.options.coefRng.rhs[1] / (top_m.options.coefRng.mat[2]/cutFac_fl) # smallest rhs possible within range
			limCoef_boo = true
		end
		
		# ! ensure factors remain within overall range
		maxRng_fl = (top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1]) * rngVio_fl # maximum range of coefficients
		facRng_tup = abs.(collect(values(cut_expr.terms))) |> (w -> (min(minimum(w), cutFac_fl), max(maximum(w), cutFac_fl))) # actual range of coefficients
		
		# manipulates factors to stay within range
		if maxRng_fl < facRng_tup[2] / facRng_tup[1]
			# compute maximum and minimum factors
			minFac_fl = facRng_tup[2] / maxRng_fl

			# removes small factors
			filter!(x -> abs(x[2]) > minFac_fl, cut_expr.terms)

			# check if the cut also would violate range, limit coefficients in this case
			if cutFac_fl < minFac_fl
				limCoef_boo = true
				maxCoef_fl = cutFac_fl * maxRng_fl # biggest possible coefficient, so cut variable is still in range
				foreach(x -> abs(cut_expr.terms[x]) > maxCoef_fl ? cut_expr.terms[x] = maxCoef_fl * sign(cut_expr.terms[x]) : nothing, collect(keys(cut_expr.terms))) # limits coefficients to maximum value
			end
		end

		# ! adjust small rhs based on range of updated factors
		reqScaRhs_tup = top_m.options.coefRng.rhs ./ abs(cut_expr.constant) # range of scaling required to move rhs in range
		upFacRng_tup = abs.(collect(values(cut_expr.terms))) |> (w -> (min(minimum(w), cutFac_fl), max(maximum(w), cutFac_fl))) 
		posScaFac_tup = (top_m.options.coefRng.mat[1] / upFacRng_tup[1], top_m.options.coefRng.mat[2] / upFacRng_tup[2]) # possible range of scaling factors without moving factors out of range
		
		if reqScaRhs_tup[1] > posScaFac_tup[2] # factor requires more up-scaling than possible
			if abs(cut_expr.constant) < abs(cut_expr.constant - top_m.options.coefRng.rhs[1] / posScaFac_tup[2]) # setting to zero creates smaller error than smallest value in range
				cut_expr.constant = 0.0
			else
				cut_expr.constant = top_m.options.coefRng.rhs[1] / posScaFac_tup[2]
			end 
		end

		# ensure scaling of factors does not move rhs out of range
		if cut_expr.constant != 0.0
			scaRng_tup = (top_m.options.coefRng.rhs[1], top_m.options.coefRng.rhs[2] * rngVio_fl) ./ abs(cut_expr.constant) # get smallest and biggest scaling factors where rhs is still in range

			for x in keys(cut_expr.terms)
				val_fl = abs(cut_expr.terms[x])
				if top_m.options.coefRng.mat[1] / val_fl > scaRng_tup[2] # factor requires more up-scaling than possible
					delete!(cut_expr.terms, x) # removes term
				elseif top_m.options.coefRng.mat[2] * rngVio_fl / val_fl < scaRng_tup[1] # factor requires more down-scaling than possible
					cut_expr.terms[x] = sign(cut_expr.terms[x]) * top_m.options.coefRng.mat[2] / scaRng_tup[1] # set to biggest factor possible within range
					limCoef_boo = true
				end
			end
		end	

	else # check if cut without variables can be scaled into range
		cutFac_fl = abs(collect(values(cut_var.terms))[1]) # get scaling factor of cut variable
		scaRng_tup = (top_m.options.coefRng.mat[1], top_m.options.coefRng.mat[2] * rngVio_fl) ./ cutFac_fl
		negSign_boo = cut_expr < 0

		if cut_expr > top_m.options.coefRng.rhs[2] / scaRng_tup[1] 
			cut_expr = top_m.options.coefRng.rhs[2] / scaRng_tup[1] * (negSign_boo ? -1.0 : 1.0) # biggest rhs possible within range
			limCoef_boo = true
		elseif cut_expr < top_m.options.coefRng.rhs[1] / scaRng_tup[2]
			top_m.options.coefRng.mat[2] * rngVio_fl / cutFac_fl < scaRng_tup[1]
			minCut_expr = top_m.options.coefRng.rhs[1] / scaRng_tup[2] # smallest rhs possible within range
			# check if zero is not closer to acutal value than smallest value possible
			if abs(cut_expr - minCut_expr) > abs(cut_expr - 0)
				cut_expr = 0.0
			else
				cut_expr = minCut_expr * (negSign_boo ? -1.0 : 1.0)
			end	
			limCoef_boo = true
		end
	end

	#endregion

	return cut_expr - cut_var, limCoef_boo
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, var_df::DataFrame, scaFac_fl::Float64, noStab_map::Union{Nothing,GenericReferenceMap})
	ben_df = deSelect(sub_df) |> (z -> innerjoin(deSelect(var_df), z, on = intCol(z, :dir)))

	if !isnothing(noStab_map)
		return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual * scaFac_fl * (noStab_map[collect(keys(x.var.terms))[1]] - x.value / scaFac_fl), eachrow(ben_df)))
	else
		return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual * scaFac_fl * (collect(keys(x.var.terms))[1] - x.value / scaFac_fl), eachrow(ben_df)))
	end

	
end

# ! removes cases where storage variables are fixed by a ratio (e.g. storage energy capacity fixed by e/p ratio) 
function removeFixStorage(stVar_sym::Symbol, stVar_df::DataFrame, part_obj::TechPart)
	fixPar_dic = Dict(:expStSize => :sizeToStOutExpFix, :expStIn => :stInToConvExpFix, :expStOut => :stOutToStInExpFix, :capaStSize => :sizeToStOutCapaFix, :capaStIn => :stInToConvCapaFix, :capaStOut => :stOutToStInCapaFix)
	if stVar_sym in [:expStSize, :expStIn, :expStOut, :capaStSize, :capaStIn, :capaStOut] && fixPar_dic[stVar_sym] in collect(keys(part_obj.cns))
		fixCns_df = select(part_obj.cns[fixPar_dic[stVar_sym]], Not([:cns]))
		stVar_df = stVar_df |> (x -> antijoin(x, fixCns_df, on = intersect(intCol(x), intCol(fixCns_df))))
	end
	return stVar_df
end

# ! detect if anywhere mustCapaConv exceed capaConv (can occur with stabilization although constraint explicitly forbids it, will cause infeas SP)
function checkTopStatus(top_m::anyModel)

	if termination_status(top_m.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED)
		# get results
		checkData_obj = resData()
		checkData_obj.capa, ~, ~ = writeResult(top_m, [:capa, :mustCapa]; rmvFix = true, fltSt = false)

		# check for error
		for sSym in keys(checkData_obj.capa[:tech])
			if :capaConv in keys(checkData_obj.capa[:tech][sSym]) && :mustCapaConv in keys(checkData_obj.capa[:tech][sSym])
				joinCapa_df = checkData_obj.capa[:tech][sSym][:capaConv] |> (x -> innerjoin(rename(x, :value => :capaValue), checkData_obj.capa[:tech][sSym][:mustCapaConv], on = intCol(x), makeunique = true))
				if any((joinCapa_df[!,:capaValue] .+ 1e-5) .< joinCapa_df[!,:value])
					return false					
				end
			end
		end
		return true
	else
		return false
	end

end

# ! enforces capacities that are at least as high as must-run capacities
function correctMustCapa(resData_obj::resData)

	for sSym in keys(resData_obj.capa[:tech])
		if :capaConv in keys(resData_obj.capa[:tech][sSym]) && :mustCapaConv in keys(resData_obj.capa[:tech][sSym])
			joinCapa_df = resData_obj.capa[:tech][sSym][:capaConv] |> (x -> innerjoin(rename(x, :value => :capaValue), resData_obj.capa[:tech][sSym][:mustCapaConv], on = intCol(x), makeunique = true))
			joinCapa_df[!,:capaValue] = map(x -> x.capaValue < x.value ? x.value : x.capaValue, eachrow(joinCapa_df))
			resData_obj.capa[:tech][sSym][:capaConv] = rename(select(joinCapa_df, Not([:value])), :capaValue => :value)
		end
	end

	return resData_obj

end

# ! adds complicating constraints to a top-problem
function addComplCns!(top_m::anyModel, relVar_arr::Vector{Symbol}, complCns_dic::Dict{Tuple{Int,Int},Dict{Symbol,DataFrame}})
	
	for compl in relVar_arr
		allCompl_df = DataFrame()
		# loop over subproblems to get data
		for s in keys(complCns_dic)
			# get complicating variable and add subproblem info
			addCompl_df = copy(complCns_dic[s][compl])
			addCompl_df[!,:sub] .= fill(s, size(addCompl_df,1))
			# join to overall dataframe
			if isempty(allCompl_df)
				allCompl_df = addCompl_df
			else
				miss_arr = [intCol(allCompl_df), intCol(addCompl_df)] |> (y -> union(setdiff(y[1], y[2]), setdiff(y[2], y[1])))
				join_arr = intersect(namesSym(allCompl_df), namesSym(addCompl_df))
				allCompl_df = joinMissing(allCompl_df, addCompl_df, join_arr, :outer, merge(Dict(z => 0 for z in miss_arr), Dict(:Up => nothing, :Low => nothing, :Fix => nothing, :UpDir => nothing, :LowDir => nothing, :FixDir=> nothing)))
			end
		end
		# create complicating variables for benders
		top_m.parts.lim.var[compl] = createVar(select(allCompl_df, intCol(allCompl_df, :sub)), string(compl), NaN, top_m.optModel, top_m.lock, top_m.sets; scaFac = top_m.options.scaFac.dispConv, lowBd = compl == :emissionBendersCom ? NaN : 0.0)
		
		# create constraints using complicating variables
		topVar_df = copy(top_m.parts.lim.var[compl])
		topVar_df[!,:var] = map(x -> x.var * ((!(:scr in keys(x)) || x.scr == 0) ? top_m.scr.scrProb[x.sub] : 1.0), eachrow(topVar_df))
		allCompl_df = unique(select(allCompl_df, Not([:sub])))
		allCompl_df[!,:var] = aggDivVar(topVar_df, allCompl_df, tuple(intCol(allCompl_df)...), top_m.sets)

		cns_dic = Dict{Symbol,cnsCont}()
		cns_dic = createLimitCont(allCompl_df, compl, cns_dic, top_m)
		
		for cnsSym in keys(cns_dic)
			top_m.parts.lim.cns[cnsSym] = createCns(cns_dic[cnsSym], top_m.optModel, top_m.options.holdFixed)
		end
	end
end

#endregion

#region # * reporting

# ! report violation of range in quadratic trust-region
function reportRngViolations(qtrConsSca_expr::QuadExpr, matRng_tup::Tuple{Float64,Float64}, rngVio_fl::Float64, wrtRep::Bool)
	
	# create dataframe for reporting
	repVio_df = DataFrame(var = String[], fac = Float64[], type = Symbol[])

	if wrtRep

		# get factors that violate range
		trackVioSm_arr = Pair[]
		trackVioBg_arr = Pair[]

		for x in keys(qtrConsSca_expr.terms)
			if matRng_tup[1] / rngVio_fl > abs(qtrConsSca_expr.terms[x]) 
				push!(trackVioSm_arr, string(x.a) => abs(qtrConsSca_expr.terms[x])) 
			elseif matRng_tup[2] * rngVio_fl < abs(qtrConsSca_expr.terms[x]) 
				push!(trackVioBg_arr, string(x.a) => abs(qtrConsSca_expr.terms[x])) 
			end
		end
		
		for x in keys(qtrConsSca_expr.aff.terms) 
			if matRng_tup[1] / rngVio_fl > abs(qtrConsSca_expr.aff.terms[x])
				push!(trackVioSm_arr, string(x) => abs(qtrConsSca_expr.aff.terms[x])^0.5) 
			elseif matRng_tup[2] * rngVio_fl < abs(qtrConsSca_expr.aff.terms[x])
				push!(trackVioBg_arr, string(x) => abs(qtrConsSca_expr.aff.terms[x])^0.5) 
			end
		end	
		
		# add too small factors to reporting
		for x in unique(getindex.(trackVioSm_arr,1))
			# find greatest violation for each variable
			allRel_arr = filter(y -> y[1] == x, trackVioSm_arr)  
			min_fl = minimum(getindex.(allRel_arr,2))
			relEntr_pair = filter(y -> y[2] == min_fl, allRel_arr)[1]
			# add to overall dataframe
			push!(repVio_df, (var = relEntr_pair[1], fac = relEntr_pair[2], type = :tooSmall))
		end
		
		# add too large factors to reporting
		for x in unique(getindex.(trackVioBg_arr,1))
			# find greatest violation for each variable
			allRel_arr = filter(y -> y[1] == x, trackVioBg_arr)  
			max_fl = maximum(getindex.(allRel_arr,2))
			relEntr_pair = filter(y -> y[2] == max_fl, allRel_arr)[1]
			# add to overall dataframe
			push!(repVio_df, (var = relEntr_pair[1], fac = relEntr_pair[2], type = :tooBig))
		end

	end

	return repVio_df
end

# ! report on benders iteration
function reportBenders!(benders_obj::bendersObj, resData_obj::resData, elpTop_time::Millisecond, elpNoStab_time::Millisecond, timeSub_dic::Dict{Tuple{Int64,Int64},Millisecond}, lss_dic::Dict{Tuple{Int64,Int64},Float64}, numFoc_dic::Dict{Tuple{Int64,Int64},Int64})

	report_obj = benders_obj.report
	report_m = report_obj.mod
	itr_obj = benders_obj.itr

	#region # * reporting to terminal

	timeTop_fl = Dates.toms(elpTop_time) / Dates.toms(Second(1))
	timeSubTot_fl = (benders_obj.algOpt.dist ? maximum(collect(values(timeSub_dic))) : sum(collect(values(timeSub_dic)))) |> (ms -> Dates.toms(ms) / Dates.toms(Second(1)))
	timeWaitNoStab_fl = Dates.toms(elpNoStab_time) / Dates.toms(Second(1)) |> (x -> (benders_obj.algOpt.dist ? max(0, x - timeSubTot_fl) : x))
	timeSub_arr = round.(getindex.(sort(collect(timeSub_dic)),2) |> (ms -> Dates.toms.(ms) / Dates.toms(Second(1)) ./ 60) , sigdigits = 3)
	numFoc_arr = getindex.(sort(collect(numFoc_dic)),2)

	if benders_obj.nearOpt.cnt == 0
		produceMessage(report_obj.mod.options, report_obj.mod.report, 1, " - Lower: $(round(itr_obj.res[:lowLimCost], sigdigits = 8)), Upper: $(round(itr_obj.res[:curBest], sigdigits = 8)), Optimality gap: $(round(benders_obj.itr.gap, sigdigits = 4))", testErr = false, printErr = false)
	else
		produceMessage(report_obj.mod.options, report_obj.mod.report, 1, " - Objective: $(benders_obj.nearOpt.setup.obj[benders_obj.nearOpt.cnt][1]), Objective value: $(round(benders_obj.itr.res[:nearObj], sigdigits = 8)), Feasibility gap: $(round(benders_obj.itr.gap, sigdigits = 4))", testErr = false, printErr = false)
	end
	produceMessage(report_obj.mod.options, report_obj.mod.report, 1, " - Time for top: $timeTop_fl, Time for sub: $timeSubTot_fl, Waiting for top without stabilization: $timeWaitNoStab_fl", testErr = false, printErr = false)

	if Dates.value(floor(now() - report_m.options.startTime, Dates.Minute(1))) > benders_obj.algOpt.timeLim
		produceMessage(report_m.options, report_m.report, 1, " - Aborted due to time-limit!", testErr = false, printErr = false)
	end

	for sub in keys(numFoc_dic)
		if numFoc_dic[sub] != 0
			produceMessage(report_obj.mod.options, report_obj.mod.report, 1, " - Numeric focus of sub-problem $sub had to be increased to $(numFoc_dic[sub]) for the iteration", testErr = false, printErr = false)
		end
	end

	#endregion

	#region # * reporting to benders object

	# ! iteration reporting
	etr_arr = Pair{Symbol,Any}[:i => itr_obj.cnt.i, :lowCost => itr_obj.res[:lowLimCost], :bestObj => itr_obj.res[:curBest], :gap => benders_obj.itr.gap, :curCost => itr_obj.res[:actTotCost],
					:time_ges => Dates.value(floor(now() - report_obj.mod.options.startTime, Dates.Second(1)))/60, :time_top => timeTop_fl/60, :time_waitNoStab => timeWaitNoStab_fl/60, :time_subTot => timeSubTot_fl/60, :time_sub => timeSub_arr, :cntCuts => benders_obj.cuts.cnt, :numFoc => numFoc_arr, :objName => benders_obj.info.name]
	# add info about stabilization
	if !isnothing(benders_obj.stab) 
		stab_obj = benders_obj.stab
		push!(etr_arr, :actMethod => stab_obj.method[stab_obj.actMet])	
		append!(etr_arr, map(x -> Symbol("dynPar_", stab_obj.method[x]) => isa(stab_obj.dynPar[x], Dict) ? [round(stab_obj.dynPar[x][j], sigdigits = 5) for j in keys(stab_obj.dynPar[x])] : stab_obj.dynPar[x], 1:length(stab_obj.method)))
	elseif :actMethod in namesSym(report_obj.itr) # adds empty entries for near-opt case
		push!(etr_arr, :actMethod => :none)
		foreach(y -> push!(etr_arr, Symbol(y) => 0.0), filter(x -> occursin("dynPar",string(x)), namesSym(report_obj.itr)))
	end

	# add info about near-optimal
	if !isnothing(benders_obj.nearOpt.setup)
		push!(etr_arr, :objective => benders_obj.nearOpt.cnt != 0 ? benders_obj.nearOpt.setup.obj[benders_obj.nearOpt.cnt][1] : "cost")
	end

	push!(report_obj.itr, (;zip(getindex.(etr_arr, 1), getindex.(etr_arr, 2))...))

	# ! near-optimal reporting
	if !isnothing(benders_obj.nearOpt.setup)

		lss_fl = sum(collect(values(lss_dic)))

		if benders_obj.nearOpt.cnt == 0 || (itr_obj.res[:actTotCost] <= itr_obj.res[:optCost] * (1 + benders_obj.nearOpt.setup.cutThres) && lss_fl <= itr_obj.res[:optLss] * (1 + benders_obj.nearOpt.setup.lssThres))
				
			# get value of capacity variables 
			newRes_df = DataFrame(timestep = String[], region = String[], system = String[], id = String[], variable = Symbol[], value = Float64[])

			for sys in (:tech, :exc)
				part_dic = getfield(benders_obj.top.parts, sys)
				for sSym in keys(resData_obj.capa[sys]), capaSym in keys(resData_obj.capa[sys][sSym])

					# get capacity dataframe
					capa_df = printObject(resData_obj.capa[sys][sSym][capaSym], benders_obj.top, rtnDf = (:csvDf,))
					# merge into common format
					if capaSym != :capaExc
						capa_df = rename(select(capa_df, Not([:timestep_superordinate_expansion])), :timestep_superordinate_dispatch => :timestep, :region_expansion => :region, :technology => :system)
					else
						capa_df[!,:region] = capa_df[!,:region_from] .* " - " .* capa_df[!,:region_to]
						capa_df = rename(select(capa_df, Not([:timestep_superordinate_expansion, :region_from, :region_to, :directed])), :timestep_superordinate_dispatch => :timestep, :exchange => :system)
					end
					if capaSym in (:capaConv, :capaExc) capa_df[!,:id] .= "" end
					capa_df[!,:variable] .= capaSym
					# add to all capacities
					append!(newRes_df, capa_df)
				end
			end

			# add costs and loss of load
			push!(newRes_df, (timestep = "", region = "", system = "", id = "", variable = :cost, value = itr_obj.res[:actTotCost]))
			push!(newRes_df, (timestep = "", region = "", system = "", id = "", variable = :lss, value = lss_fl))

			# add relative difference to optimum
			if benders_obj.nearOpt.cnt != 0
				push!(newRes_df, (timestep = "", region = "", system = "", id = "", variable = :thrs, value = itr_obj.res[:actTotCost] / itr_obj.res[:optCost] - 1))
			end
			
			# add iteration counter and add
			newRes_df[!,:i] .= itr_obj.cnt.i
			newRes_df[!,:objName] .= benders_obj.info.name
			append!(report_obj.nearOpt, newRes_df)
		end
	end

	# ! write reports
	if itr_obj.cnt.i%benders_obj.algOpt.reportFreq == 0 
		CSV.write(report_obj.mod.options.outDir * "/iterationCuttingPlane_$(benders_obj.info.name).csv", report_obj.itr)
		if !isnothing(benders_obj.nearOpt.setup) CSV.write(report_obj.mod.options.outDir * "/nearOptSol_$(benders_obj.info.name).csv", report_obj.nearOpt) end
		if !isempty(report_obj.stabVio)  CSV.write(report_obj.mod.options.outDir * "/stabRngVio_$(benders_obj.info.name).csv", report_obj.stabVio) end
	end

	#endregion

end

# ! write results for overall algorithm
function writeBendersResults!(benders_obj::bendersObj, runSubDist::Function, getSubStringDist::Function)

	res_ntup = benders_obj.report.res
	# reporting on iteration
	CSV.write(benders_obj.report.mod.options.outDir * "/iterationCuttingPlane_$(benders_obj.info.name).csv", benders_obj.report.itr)
	
	# reporting on near-optimal
	if !isnothing(benders_obj.nearOpt.setup)
		# get pareto-efficient near-optimal solutions
		filterParetoEff!(benders_obj.report.nearOpt, benders_obj.nearOpt.setup)
		# write result file
		CSV.write(benders_obj.report.mod.options.outDir * "/nearOptSol_$(benders_obj.info.name).csv", benders_obj.report.nearOpt)
	end
	
	# run top-problem and sub-problems with optimal values fixed and write results
	foreach(x -> CSV.write("$(benders_obj.top.options.outDir)/results_" * string(x) * "_$(benders_obj.top.options.outStamp).csv", benders_obj.itr.best.res[x]), keys(benders_obj.itr.best.res))
	
	if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
	
	@suppress begin
		for (id,s) in enumerate(collect(keys(benders_obj.sub)))
			if benders_obj.algOpt.dist # distributed case
				futData_dic[s] = runSubDist(id + 1, copy(benders_obj.itr.best.var), benders_obj.algOpt.rngVio.fix, :barrier, 1e-8, false, benders_obj.algOpt.sub.check, res_ntup)
			else # non-distributed case
				runSub(benders_obj.sub[s], copy(benders_obj.itr.best.var), benders_obj.algOpt.rngVio.fix, :barrier, 1e-8, false, benders_obj.algOpt.sub.check, res_ntup)
			end
		end
	end
	
	if benders_obj.algOpt.dist wait.(collect(values(futData_dic))) end
	
	# merge general results into single files
	for res in res_ntup.general
		# get all relevant csv files
		mergFile_arr = ["$(benders_obj.top.options.outDir)/results_" * string(res) * "_$(benders_obj.top.options.outStamp).csv"]
		if benders_obj.algOpt.dist # get name for sub-problems from workers
			append!(mergFile_arr, map(x -> fetch(getSubStringDist(x + 1, res)), 1:length(benders_obj.sub)))
		else # get name directly
			append!(mergFile_arr, map(s -> benders_obj.sub[s] |> (z -> "$(z.options.outDir)/results_" * string(res) * "_$(z.options.outStamp).csv"), collect(keys(benders_obj.sub))))
		end
	
		# read in files and merge into one
		merged_df = CSV.read(mergFile_arr[1], DataFrame, stringtype = String)
	
		# add foresight column to costs if needed
		if res == :cost && benders_obj.top.scr.frsLvl != benders_obj.top.supTs.lvl
			merged_df[!,:timestep_foresight] .= "none"
		elseif "timestep_foresight" in names(merged_df)
			merged_df[!,:timestep_foresight] = replace.(merged_df[!,:timestep_foresight], " " => "")
		end
	
		for file in mergFile_arr[2:end]
			add_df = CSV.read(file, DataFrame, stringtype = String)
			# filter dispatch variables
			filter!(x -> !(x.variable in ("capaConv", "capaStIn", "capaStOut", "capaStSize", "capaExc", "capaStSizeSeason", "stInterDelta", "stLvl")), add_df)
			if isempty(add_df) continue end
			# makes adjustments to cost results
			if res == :cost
				sub_tup = sort(collect(keys(benders_obj.sub)))[parse(Int,split(add_df[1,:objName],"_")[2])]
				add_df[!,:scenario] .= benders_obj.top.sets[:scr].nodes[sub_tup[2]].val
				if benders_obj.top.scr.frsLvl != benders_obj.top.supTs.lvl
					add_df[!,:timestep_foresight] .= benders_obj.top.sets[:Ts].nodes[sub_tup[1]].val
				end
			end
			append!(merged_df, select(add_df, intersect(namesSym(merged_df), namesSym(add_df))))
		end
	
		merged_df[!,:objName] .= benders_obj.info.name 
	
		# write merged file and remove others
		CSV.write(benders_obj.report.mod.options.outDir * "/" * "results_" * string(res) * "_" * benders_obj.info.name * ".csv", orderDf(merged_df))
		rm.(mergFile_arr)
	
	end
	
end

# ! write capacity values as fixes
function writeResultsAsInputs!(benders_obj::bendersObj, outDir_str::String)

	# prepare inputs
	top_m = benders_obj.top
	parDef_dic = defineParameter(top_m.options, top_m.report)

	# create directory
	restDir!(outDir_str)
	
	# write capacity values
	for sys in (:tech, :exc)
		for sSym in keys(benders_obj.itr.best.var.capa[sys])
			for capaSym in filter(x -> !occursin("Season", string(x)), keys(benders_obj.itr.best.var.capa[sys][sSym]))
				if capaSym != :mustCapaConv
					# add residual capacities since not accounted for in variable
					var_df = benders_obj.itr.best.var.capa[sys][sSym][capaSym] |> (x -> copy(innerjoin(x, select(getfield(top_m.parts, sys)[sSym].var[capaSym], vcat(intCol(x),[:var])), on = intCol(x))))
					var_df[!,:value] = var_df[!,:value] .+ map(x -> x.constant, var_df[!,:var])
					select!(var_df, Not([:var]))
				else
					var_df = copy(benders_obj.itr.best.var.capa[sys][sSym][capaSym])
				end
				# add potentially missing dir column
				if sys == :exc
					if getindex(benders_obj.top.parts.exc,sSym).dir && !(:dir in namesSym(var_df))
						var_df[!, :dir] .= true
					end
				end
				# write parameter fle
				par_sym = Symbol(capaSym,"Fix")
				writeParameterFile!(top_m, var_df, par_sym, parDef_dic[par_sym], outDir_str * "par_" * string(sSym,"_",capaSym))
			end
		end
	end



	# write storage levels
	#=
	for sys in keys(benders_obj.itr.best.var.stLvl)
		par_sym = :stLvlFix
		writeParameterFile!(top_m, benders_obj.itr.best.var.stLvl[sys][:stLvl], par_sym, parDef_dic[par_sym], outDir_str * "par_stLvlFix_" * string(sys))
	end
	=#
end

# ! report value of complicating variables
function reportComplVar!(allRes_df::DataFrame, resData_obj::resData, i::Int64)

	# add capacity variables
	for sys in (:tech, :exc), sSym in keys(resData_obj.capa[sys]), capaSym in keys(resData_obj.capa[sys][sSym])
		add_df = copy(resData_obj.capa[sys][sSym][capaSym])
		if sys == :exc select!(add_df, Not([:dir])) end
		add_df[!,:i] .= i
		add_df[!,:variable] .= capaSym
		add_df[!,:sub] .= fill((0,0),size(add_df,1))
		foreach(x -> add_df[!,x] .= 0, setdiff(names(allRes_df),names(add_df)))
		append!(allRes_df, add_df)
	end

	# add storage levels
	for sSym in keys(resData_obj.stLvl), lvlSym in keys(resData_obj.stLvl[sSym])
		add_df = copy(resData_obj.stLvl[sSym][lvlSym])
		add_df[!,:i] .= i
		add_df[!,:variable] .= lvlSym
		add_df[!,:sub] .= fill((0,0),size(add_df,1))
		foreach(x -> add_df[!,x] .= 0, setdiff(names(allRes_df),names(add_df)))
		append!(allRes_df, add_df)
	end

	# add limits
	for x in keys(resData_obj.lim)
		add_df = copy(resData_obj.lim[x])
		add_df[!,:i] .= i
		add_df[!, :variable] .= x
		foreach(x -> add_df[!,x] .= 0, setdiff(names(allRes_df),names(add_df)))
		append!(allRes_df, add_df)
	end

end

# ! compute euclidean distance within the result dataframe
function computeDistance(allRes_df::DataFrame, i::Int, j::Int)
	i_df = select(filter(x -> x.i == i, allRes_df), Not([:i]))
	j_df = select(filter(x -> x.i == j, rename(allRes_df, :value => :value_2)), Not([:i]))
	join_df = innerjoin(i_df, j_df, on = filter(x -> !(x in ("value","i")), names(allRes_df)))
	return sqrt(sum((join_df.value .- join_df.value_2).^2))
end

# ! analysis of iterations until serious step
function analyseBlock(allRes_df::DataFrame, benders_obj::bendersObj)
	repBlocks_df = DataFrame(cntInt = Int[], relImp = Float64[], startGap = Float64[], intDis = Float64[], intCurDis = Float64[], intNxtDis = Float64[], curNxtDis = Float64[])

	# loop over all serious steps to write infos
	srsStep_arr = filter(x -> x.curCost == x.bestObj && x.i != 1, benders_obj.report.itr)[!,:i]

	for i in eachindex(srsStep_arr)
		# skip first step
		if i == 1 continue end

		# compute improvement and gap
		curStep_df = filter(x -> x.i == srsStep_arr[i], benders_obj.report.itr)
		preStep_df = filter(x -> x.i == srsStep_arr[i-1], benders_obj.report.itr)

		# compute reference distance
		if preStep_df[1,:actMethod] == :qtrLvl
			refDis_fl = preStep_df[1,:dynPar_qtrLvl][1] * sum(abs.(filter(x -> x.i == srsStep_arr[i-1], allRes_df)[!,:value]))
		else
			refDis_fl = preStep_df[1,:dynPar_qtr] * sum(abs.(filter(x -> x.i == srsStep_arr[i-1], allRes_df)[!,:value]))
		end

		# characterize serios step
		relImp_fl = curStep_df[1,:bestObj] / preStep_df[1,:bestObj]
		cntStep_int = srsStep_arr[i] - srsStep_arr[i-1]
		startGap_fl = preStep_df[1,:gap]
		
		# compute distances among iteration points
		intDis_arr = Float64[]
		for k in collect(srsStep_arr[i-1]+1:srsStep_arr[i]), l in collect(srsStep_arr[i-1]+1:srsStep_arr[i])
			if k >= l continue end
			push!(intDis_arr, computeDistance(allRes_df, k, l))
		end
		
		# compute distance between iteration points and current best
		curBest_arr = Float64[] 
		for k in collect(srsStep_arr[i-1]+1:srsStep_arr[i])
			push!(curBest_arr, computeDistance(allRes_df, k, srsStep_arr[i-1]))
		end
		
		# compute distance between iteration points and next best
		nxtBest_arr = Float64[]
		for k in collect(srsStep_arr[i-1]+1:srsStep_arr[i])
			push!(nxtBest_arr, computeDistance(allRes_df, k, srsStep_arr[i]))
		end
		
		# compute distance between current best and next best 
		bestDis_fl = computeDistance(allRes_df, srsStep_arr[i], srsStep_arr[i-1])

		# add result to data frame
		push!(repBlocks_df, (cntInt = cntStep_int, relImp = relImp_fl, startGap = startGap_fl, intDis = avg(intDis_arr) / refDis_fl, intCurDis = avg(curBest_arr) / refDis_fl, intNxtDis = avg(nxtBest_arr) / refDis_fl, curNxtDis = bestDis_fl / refDis_fl))
	end

	repBlocks_df[!,:objName] .= benders_obj.report.mod.options.objName
	return repBlocks_df
end

#endregion



