

#region # * heuristic solve and fix of capacities

# ! run heuristic and return exact capacities plus heuristic cut
function heuristicSolve(modOpt_tup::NamedTuple, redFac::Float64, t_int::Int, opt_obj::DataType; rtrnMod::Bool=true, solDet::Bool=false, fltSt::Bool=true)

	# create and solve model
	heu_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, objName = "heuristicModel_" * string(round(redFac, digits = 3)) * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 2, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, redStep = redFac, checkRng = (print = true, all = false), forceScr = solDet ? Symbol() : nothing)
	
	prepareMod!(heu_m, opt_obj, t_int)
	set_optimizer_attribute(heu_m.optModel, "Method", 2)
	set_optimizer_attribute(heu_m.optModel, "Crossover", 0)
	optimize!(heu_m.optModel)
	checkIIS(heu_m)

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
function getFeasResult(modOpt_tup::NamedTuple, fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, t_int::Int, zeroThrs_fl::Float64, opt_obj::DataType; roundDown::Int = 0)

	# create top-problem
	topFeas_m = anyModel(modOpt_tup.inputDir, modOpt_tup.resultDir, objName = "feasModel" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, reportLvl = 1, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, checkRng = (print = true, all = false), holdFixed = true)

	topFeas_m.subPro = tuple(0, 0)
	prepareMod!(topFeas_m, opt_obj, t_int)

	# add limits to problem
	if !isempty(lim_dic) addLinearTrust!(topFeas_m, lim_dic) end

	# compute feasible capacites
	topFeas_m = computeFeas(topFeas_m, fix_dic, zeroThrs_fl, cutSmall = true);

    # return capacities and top problem (is sometimes used to compute costs of feasible solution afterward)
    return writeResult(topFeas_m, [:exp, :mustExp, :capa, :mustCapa], fltSt = false, roundDown = roundDown)
end

# ! runs top problem again with optimal results
function computeFeas(top_m::anyModel, var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, zeroThrs_fl::Float64;cutSmall::Bool=false, wrtRes::Bool=false)
	
	# create absolute value constraints for capacities or expansion variables
	for sys in (:tech, :exc)
		partTop_dic = getfield(top_m.parts, sys)
		for sSym in keys(var_dic[sys])
			part = partTop_dic[sSym]
			relVar_arr = filter(x -> any(occursin.(part.decomm == :none ? ["exp", "mustCapa"] : ["capa", "exp", "mustCapa"], string(x))), collect(keys(var_dic[sys][sSym])))
			# create variables and writes constraints to minimize absolute value of capacity delta
			for varSym in relVar_arr
				var_df = part.var[varSym] |> (w -> occursin("exp", string(varSym)) ? collapseExp(w) : w)
				filter!(x -> !isempty(x.var.terms), var_df)
				if sys == :tech var_df = removeFixStorage(varSym, var_df, part) end # remove storage variables controlled by ratio
				if isempty(var_df) continue end
				# gets variable value and set variables below threshold to zero
				abs_df = deSelectSys(var_dic[sys][sSym][varSym]) |>  (z -> leftjoin(var_df, z, on = intersect(intCol(z, :dir), intCol(var_df, :dir)))) |> (y -> y[completecases(y), :])
				abs_df[!,:value] = map(x -> x.value < zeroThrs_fl ? 0.0 : x.value, eachrow(abs_df))
				# applies weights to make achieving zero values a priority 
				abs_df[!,:weight] .= 1.0
				# create variable for absolute value and connect with rest of dataframe again
				scaFac_fl = getfield(top_m.options.scaFac, occursin("exp", lowercase(string(varSym))) ? :insCapa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))
				part.var[Symbol(:abs, makeUp(varSym))] = createVar(select(abs_df, Not([:var, :value])), string(:abs, makeUp(varSym)), top_m.options.bound.capa, top_m.optModel, top_m.lock, top_m.sets, scaFac =scaFac_fl)
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
				end
			end
		end
	end
	
	# get sum of absolute values 
	absVar_arr = [:CapaConv, :CapaExc, :CapaStOut, :CapaStIn, :CapaStSize, :ExpConv, :ExpExc, :ExpStOut, :ExpStIn, :ExpStSize]
	absVal_expr = sum(map(x -> sum(getAllVariables(Symbol(:abs, x), top_m) |> (w -> isempty(w) ? [AffExpr()] : w[!,:var] .* w[!,:weight])),vcat(absVar_arr, Symbol.(:Must, absVar_arr))))
	# get sum of missing capacities and apply high weight 
	missCapa_expr = :missCapa in keys(top_m.parts.bal.var) ? sum(top_m.parts.bal.var[:missCapa][!,:var] ./ top_m.options.scaFac.insCapa .* 10) : AffExpr()
	objExpr_df = DataFrame(cnsExpr = [missCapa_expr + absVal_expr])
	scaleCnsExpr!(objExpr_df, top_m.options.coefRng, top_m.options.checkRng)
	
	# change objective of top problem to minimize absolute values and missing capacities
	@objective(top_m.optModel, Min, objExpr_df[1,:cnsExpr])
	# solve problem
	set_optimizer_attribute(top_m.optModel, "MIPGap", 0.001)
	set_optimizer_attribute(top_m.optModel, "SolutionLimit", 3600)
	
	optimize!(top_m.optModel)
	checkIIS(top_m)

	# write results into files (only used once optimum is obtained)
	if wrtRes
		reportResults(:summary, top_m)
		reportResults(:cost, top_m)
		reportResults(:exchange, top_m)
	end

	return top_m
end

# ! find fixed variables and write to file
function writeFixToFiles(fix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, feasFix_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, temp_dir::String, res_m::anyModel; skipMustSt::Bool =false)
	rm(temp_dir; force = true, recursive = true)
	mkdir(temp_dir) # create directory for fixing files
	parFix_dic = defineParameter(res_m.options, res_m.report) # stores parameter info for fixing
	
	# loop over variables
	for sys in (:tech, :exc), sSym in keys(fix_dic[sys])
		hasMust_boo = sys == :tech && isdefined(res_m.parts.tech[sSym], :capaRestr) ? "must" in res_m.parts.tech[sSym].capaRestr[!,:cnstrType] : false
		for varSym in filter(x -> !skipMustSt || !hasMust_boo || x in (:capaConv, :expConv), keys(fix_dic[sys][sSym]))

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
function buildSub(id::Int, genSetup_ntup::NamedTuple{(:name, :frs, :supTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64}}, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, scale_dic::Dict{Symbol,NamedTuple}, algOpt_obj::algSetup)
	# create sub-problems
	sub_m = @suppress anyModel(inputFolder_ntup.in, inputFolder_ntup.results, objName = "subModel_" * string(id) * genSetup_ntup.name, lvlFrs = genSetup_ntup.frs, supTsLvl = genSetup_ntup.supTsLvl, shortExp = genSetup_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facSub], dbInf = algOpt_obj.solOpt.dbInf, reportLvl = 1)
	sub_m.subPro = tuple([(x.Ts_dis, x.scr) for x in eachrow(sub_m.parts.obj.par[:scrProb].data)]...)[id]
	@suppress prepareMod!(sub_m, algOpt_obj.opt, algOpt_obj.threads)
	set_optimizer_attribute(sub_m.optModel, "Threads", algOpt_obj.threads)
	return sub_m
end

# ! set optimizer attributes and options, in case gurobi is used (recommended)
function prepareMod!(mod_m::anyModel, opt_obj::DataType, t_int::Int)
	# create optimization problem
	createOptModel!(mod_m)
	setObjective!(:cost, mod_m)
	# set optimizer and attributes
	set_optimizer(mod_m.optModel, opt_obj)
	set_optimizer_attribute(mod_m.optModel, "Threads", t_int)
	set_optimizer_attribute(mod_m.optModel, "QCPDual", 1)	
end

# ! run top-problem
function runTop(benders_obj::bendersObj)

	#region # * create cuts

	stab_obj = benders_obj.stab

	if !isempty(benders_obj.cuts) 
		# save values of previous cut for proximal method variation 2
		benders_obj.prevCuts = !isnothing(stab_obj) && stab_obj.method[stab_obj.actMet] == :prx2 ? copy(benders_obj.cuts) : Array{Pair{Tuple{Int,Int},Union{resData}},1}() 
		# add cuts and reset collecting array
		addCuts!(benders_obj.top, benders_obj.cuts, benders_obj.itr.cnt.i) 
		benders_obj.cuts = Array{Pair{Tuple{Int,Int},Union{resData}},1}()
	end

	#endregion

	#region # * solve problem

	# create objects to store results
	resData_obj = resData()
	stabVar_obj = resData()

	# solve model
	@suppress begin
		set_optimizer_attribute(benders_obj.top.optModel, "Method", 2)
		set_optimizer_attribute(benders_obj.top.optModel, "Crossover", 0)
		set_optimizer_attribute(benders_obj.top.optModel, "NumericFocus", benders_obj.algOpt.solOpt.numFoc)
		optimize!(benders_obj.top.optModel)
	end	
	
	# handle unsolved top problem
	if !isnothing(stab_obj)
		opt_tup = stab_obj.methodOpt[stab_obj.actMet]
		# if infeasible and level bundle stabilization, increase level until feasible
		while stab_obj.method[stab_obj.actMet] in (:lvl2, :dsb) && termination_status(benders_obj.top.optModel) in (MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED)
			produceMessage(report_m.options, report_m.report, 1, " - Empty level set", testErr = false, printErr = false)
			lowBd_fl = stab_obj.objVal / benders_obj.top.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			stab_obj.dynPar[stab_obj.actMet][:yps] = (1 - opt_tup.lam) * (stab_obj.objVal - lowBd_fl*benders_obj.top.options.scaFac.obj) / benders_obj.top.options.scaFac.obj
			ell_fl = stab_obj.objVal/benders_obj.top.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
			set_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1], ell_fl)
            optimize!(benders_obj.top.optModel)
        end

		while stab_obj.method[stab_obj.actMet] == :lvl1 && termination_status(benders_obj.top.optModel) in (MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED) 
            stab_obj.dynPar[stab_obj.actMet] = (opt_tup.lam * stab_obj.dynPar[stab_obj.actMet] * 1000  + (1 - opt_tup.lam) * stab_obj.objVal) / benders_obj.top.options.scaFac.obj
            set_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1], stab_obj.dynPar[stab_obj.actMet])
            optimize!(benders_obj.top.optModel)
        end

		# if no solution and proximal bundle stabilization, remove penalty term temporarily
		if stab_obj.method[stab_obj.actMet] == :prx && !(termination_status(benders_obj.top.optModel) in (MOI.OPTIMAL, MOI.LOCALLY_SOLVED))
			@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1,1])
			optimize!(benders_obj.top.optModel)
		end
	end
	checkIIS(benders_obj.top)

	# delete cuts that not were binding for the defined number of iterations
	deleteCuts!(benders_obj)

	#endregion

	#region # * write results

	# write technology capacites and level of capacity balance to benders object
	resData_obj.capa, resData_obj.stLvl = writeResult(benders_obj.top, [:capa, :mustCapa, :stLvl]; rmvFix = true)
	stabVar_obj.capa, stabVar_obj.stLvl = writeResult(benders_obj.top, [:capa, :exp, :stLvl]; rmvFix = true)

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
	if benders_obj.itr.cnt.nOpt != 0 benders_obj.itr.res[:nearObj] = objective_value(top_m.optModel) end
	
	#endregion

	return resData_obj, stabVar_obj
end

# ! run sub-problem
function runSub(sub_m::anyModel, resData_obj::resData, sol_sym::Symbol, optTol_fl::Float64=1e-8, crsOver_boo::Bool=false, wrtRes_boo::Bool=false)

	start_time = now()

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
					resData_obj.capa[sys][sSym][capaSym] = limitVar!(resData_obj.capa[sys][sSym][capaSym], part_dic[sSym].var[capaSym], capaSym, part_dic[sSym], sub_m)
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
				resData_obj.stLvl[sSym] = limitVar!(select(resData_obj.stLvl[sSym], Not([:scr])), select(part_obj.var[:stLvl], Not([:scr])), :stLvl, part_obj, sub_m)
				# remove system if no storage level exists
				removeEmptyDic!(resData_obj.stLvl, sSym)
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
		# write common results
		reportResults(:summary, sub_m)
		reportResults(:cost, sub_m)
		reportResults(:exchange, sub_m)
		#reportResults(:stLvl, sub_m)

		#=
		# write storage levels in case of reduced foresight
		for tSym in (:h2Cavern,:reservoir,:pumpedStorage,:redoxBattery,:lithiumBattery)
			stLvl_df = DataFrame(Ts_dis = Int[], scr = Int[], lvl = Float64[])
			for x in collect(sub_tup)
				append!(stLvl_df,combine(x -> (lvl = sum(value.(x.var)),), groupby(sub_dic[x].parts.tech[tSym].var[:stLvl],[:Ts_dis,:scr])))
			end
			stLvl_df = unstack(sort(unique(stLvl_df),:Ts_dis),:scr,:lvl)
			CSV.write(b * "results/stLvl_" * string(tSym) * "_" * suffix_str * ".csv",stLvl_df)
		end
		=#


	end

	#endregion

	#region # * extract data for cuts

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
					resData_obj.capa[sys][sSym][capaSym] = addDual(resData_obj.capa[sys][sSym][capaSym], part_dic[sSym].cns[Symbol(capaSym, :BendersFix)], scaObj_fl/scaCapa_fl)
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
				resData_obj.stLvl[sSym] = addDual(resData_obj.stLvl[sSym], part_obj.cns[:stLvlBendersFix], scaObj_fl/sub_m.options.scaFac.dispSt)
				removeEmptyDic!(resData_obj.stLvl, sSym)
			end
		end
	end

	#endregion

	elapsed_time = now() - start_time

	return elapsed_time, resData_obj
end

# ! add all cuts from input dictionary to top problem
function addCuts!(top_m::anyModel, cuts_arr::Array{Pair{Tuple{Int,Int},Union{resData}},1}, i::Int)
	
	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_dis = Int[], scr = Int[], limCoef = Bool[], actItr = Int[], cnsExpr = AffExpr[])
	for cut in cuts_arr
		subCut = cut[2]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		# compute cut element for each capacity
		for sys in (:tech, :exc)
			part_dic = getfield(top_m.parts, sys)
			for sSym in keys(subCut.capa[sys]), capaSym in filter(x -> occursin("capa", lowercase(string(x))), collect(keys(subCut.capa[sys][sSym])))
				scaCapa_fl = getfield(top_m.options.scaFac, occursin("StSize", string(capaSym)) ? :capaStSize : :capa)
				push!(cutExpr_arr, getBendersCut(subCut.capa[sys][sSym][capaSym], part_dic[sSym].var[capaSym], scaCapa_fl))
			end
		end

		# compute cut element for each storage level
		if !isempty(subCut.stLvl)
			for sSym in keys(subCut.stLvl)
				if sSym in keys(top_m.parts.tech)
					part_obj = top_m.parts.tech[sSym]
					push!(cutExpr_arr, getBendersCut(subCut.stLvl[sSym], part_obj.var[:stLvl], top_m.options.scaFac.dispSt))
				end
			end
		end
		
		# get cut variable and compute cut expression 
		cut_var = filter(x -> x.Ts_dis == cut[1][1] && x.scr == cut[1][2], top_m.parts.obj.var[:cut])[1,:var]
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
			facRng_fl = abs.(collect(values(cut_expr.terms))) |> (w -> (min(minimum(w), cutFac_fl), max(maximum(w), cutFac_fl))) # actual range of coefficients
			
			# manipulates factors to stay within range
			if maxRng_fl < facRng_fl[2]/facRng_fl[1]
				# compute maximum and minimum factors
				minFac_fl = facRng_fl[2]/maxRng_fl

				# removes small factors
				filter!(x -> abs(x[2]) > minFac_fl, cut_expr.terms)

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
					delete!(cut_expr.terms, x) # removes term
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
		push!(cut_df, (i = i, Ts_dis = cut[1][1], scr = cut[1][2], limCoef = limCoef_boo, actItr = i, cnsExpr = cut_expr - cut_var))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df, top_m.options.coefRng, top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts], createCns(cnsCont(cut_df, :smaller), top_m.optModel, false))

end

# ! update results and stabilization
function updateIteration!(benders_obj::bendersObj, cutData_dic::Dict{Tuple{Int64,Int64},resData}, stabVar_obj::resData)

	itr_obj = benders_obj.itr
	best_obj = itr_obj.best

	# store information for cuts
	benders_obj.cuts = collect(cutData_dic)

	# get sub-results
	itr_obj.res[:actSubCost] = sum(map(x -> x.objVal, values(cutData_dic))) # objective of sub-problems
	itr_obj.res[:actTotCost] = itr_obj.res[:topCost] + itr_obj.res[:actSubCost]

	# update current best
	if (itr_obj.cnt.nOpt == 0 ? (itr_obj.res[:actSubCost] - itr_obj.res[:estSubCost]) : itr_obj.res[:actTotCost]) < best_obj.objVal
		best_obj.objVal = itr_obj.cnt.nOpt == 0 ? itr_obj.res[:actTotCost] : (itr_obj.res[:actSubCost] - itr_obj.res[:estSubCost])
		best_obj.capa, best_obj.stLvl = writeResult(benders_obj.top, [:capa, :exp, :mustCapa, :stLvl]; rmvFix = true)	
		benders_obj.itr.res[:curBest] = best_obj.objVal	
	end

	# adapt center and parameter for stabilization
	if !isnothing(benders_obj.stab)
		stab_obj = benders_obj.stab
		report_m = benders_obj.report.mod
		
		# determine if serious step 
		expStep_fl = best_obj.objVal - (itr_obj.cnt.nOpt == 0 ? itr_obj.res[:estTotCost] : 0.0) # expected step size
		srsStep_boo = false
		if best_obj.objVal < stabVar_obj.objVal - stab_obj.srsThr * expStep_fl
			srsStep_boo = true
		end

		# initialize counters
		itr_obj.cnt.srs = srsStep_boo ? itr_obj.cnt.srs + 1 : 0
		itr_obj.cnt.null = srsStep_boo ? 0 : itr_obj.cnt.null + 1

		# adjust dynamic parameters of stabilization
		prx2Aux_fl = stab_obj.method[stab_obj.actMet] == :prx2 ? computePrx2Aux(benders_obj.cuts, benders_obj.prevCuts) : nothing
		foreach(x -> adjustDynPar!(x, benders_obj.stab, benders_obj.top, benders_obj.itr.res, itr_objc.cnt, srsStep_boo, prx2Aux_fl, benders_obj.itr.cnt.nOpt != 0, report_m), 1:length(stab_obj.method))

		# update center of stabilisation
		if srsStep_boo
			stab_obj.var = filterStabVar(stabVar_obj.capa, stabVar_obj.stLvl, stab_obj.weight, benders_obj.top)
			stab_obj.objVal = best_obj.objVal
			produceMessage(report_m.options,report_m.report, 1," - Updated reference point for stabilization!", testErr = false, printErr = false)
		end

		# switch quadratic stabilization method
		if !isnothing(benders_obj.stab)
			stab_obj = benders_obj.stab
			# switch stabilization method
			if !isempty(stab_obj.ruleSw) && i > stab_obj.ruleSw.itr && length(stab_obj.method) > 1
				min_boo = itrReport_df[i - stab_obj.ruleSw.itr,:actMethod] == stab_obj.method[stab_obj.actMet] # check if method as been used for the minimum number of iterations 
				pro_boo = itrReport_df[(i - min(i,stab_obj.ruleSw.itrAvg) + 1):end,:gap] |> (x -> (x[1]/x[end])^(1/(length(x) -1)) - 1 < stab_obj.ruleSw.avgImp) # check if progress in last iterations is below threshold
				if min_boo && pro_boo
					stab_obj.actMet = stab_obj.actMet + 1 |> (x -> length(stab_obj.method) < x ? 1 : x)
					produceMessage(report_m.options,report_m.report, 1," - Switched stabilization to $(nameStab_dic[stab_obj.method[stab_obj.actMet]]) method!", testErr = false, printErr = false)
				end
			end
			
			# update stabilization method
			centerStab!(stab_obj.method[stab_obj.actMet], benders_obj.algOpt.solOpt.addVio, benders_obj.top, report_m)
		end
	end
	
	# computes optimality gap for cost minimization and feasibility gap for near-optimal
	itr_obj.gap = itr_obj.cnt.nOpt == 0 ? (1 - benders_obj.itr.res[:lowLimCost] / benders_obj.itr.res[:curBest]) : abs(benders_obj.itr.res[:curBest] / benders_obj.itr.res[:optCost])

end

# ! check if algorithm converged and switch objective or terminate
function checkConvergence(benders_obj::bendersObj)

	itr_obj = benders_obj.itr
	report_m = benders_obj.report.mod
	rtn_boo = false

	# check for termination
	if benders_obj.itr.gap < benders_obj.algOpt.gap
		# switch from cost minimization to near-optimal
		if !isnothing(benders_obj.nearOpt.setup) && benders_obj.nearOpt.cnt < length(benders_obj.nearOpt.setup.obj) 
			if benders_obj.nearOpt.cnt == 0
				# get characteristics of optimal solution
				itr_obj.res[:optCost] = best_obj.objVal
				itr_obj.res[:optLss] = itr_obj.res[:nOptLss]
				# filter near-optimal solution already obtained
				benders_obj.report.nearOpt[!,:thrs] .= 1 .- itr_obj.res[:optCost] ./ benders_obj.report.nearOpt[!,:cost]
				filter!(x -> x.thrs <= benders_obj.nearOpt.setup.cutThres && x.lss <= itr_obj.res[:optLss] * (1 + benders_obj.nearOpt.setup.lssThres), benders_obj.report.nearOpt)
				# re-set gap
				benders_obj.algOpt.gap = benders_obj.nearOpt.setup.feasGap
			end
			# reset iteration variables
			itr_obj.gap = 1.0 
			itr_obj.res[:nearObj] = Inf
			itr_obj.best.objVal = Inf
			
			if !isempty(meth_tup) # reset current best tracking for stabilization
				benders_obj.stab.objVal = Inf
				benders_obj.stab.dynPar = computeDynPar(benders_obj.stab.method, benders_obj.stab.methodOpt, itr_obj.res[:estTotCost], itr_obj.res[:curBest], benders_obj.top)
			end 

			benders_obj.nearOpt.cnt = benders_obj.nearOpt.cnt + 1 # update near-opt counter
			# adapt the objective and constraint to near-optimal
			adaptNearOpt!(bender_obj.top, benders_obj.nearOpt.setup, itr_obj.res[:optCost], benders_obj.nearOpt.cnt)
			produceMessage(report_m.options,report_m.report, 1," - Switched to near-optimal for $(benders_obj.nearOpt.setup.obj[nOpt_int][1])", testErr = false, printErr = false)
			
		else
			produceMessage(report_m.options,report_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
			rtn_boo = true
		end
	end

	return rtn_boo
	
end

#endregion

#region # * data management

# merge all entries of dictionary used for capacity data into one dataframe for the specified columns
mergeVar(var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, outCol::Array{Symbol,1}) = vcat(vcat(vcat(map(x -> var_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,outCol], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)

# get dataframe with variables, values, and scaling factors for stabilization
function getStabDf(stab_obj::stabObj, top_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var, stab_obj.weight, top_m)
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var, :value, :scaFac]], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)
	allStLvl_df = vcat(map(x -> expExpr_dic[:stLvl][x], collect(keys(expExpr_dic[:stLvl])))...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)
	
	# filter zero scaling factors
	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df, allStLvl_df))
	
	# normalize scaling factors
	allVar_df[!,:scaFac] .= allVar_df[!,:scaFac] ./ minimum(allVar_df[!,:scaFac])

	return allVar_df
end

# ! matches values in dictionary with variables of provided problem
function matchValWithVar(var_dic::Dict{Symbol, Union{Dict{Symbol, Dict{Symbol, Dict{Symbol, DataFrame}}}, Dict{Symbol, DataFrame}}}, weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}}, mod_m::anyModel, prsvExp::Bool=false)
	
	expExpr_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()
	
	# match capacity values with variables
	expExpr_dic[:capa] = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech, :exc)
		expExpr_dic[:capa][sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(mod_m.parts, sys)
		for sSym in keys(var_dic[:capa][sys])
			expExpr_dic[:capa][sys][sSym] = Dict{Symbol,DataFrame}()
			for varSym in keys(var_dic[:capa][sys][sSym])
				val_df = deSelectSys(var_dic[:capa][sys][sSym][varSym])
				# get scaling factor for variables (corrects scaling within model going back to standard units and apply weights defined for stabilization)
				modSca_fl =  getfield(mod_m.options.scaFac, occursin("exp", string(varSym)) ? :insCapa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))
				wgtSca_fl =  getfield(weight_ntup, occursin("exp", string(varSym)) ? :capa : (occursin("StSize", string(varSym)) ? :capaStSize : :capa))			
				val_df[!,:value] = val_df[!,:value] ./ modSca_fl # correct value with scaling factor for variable
				# join variables and values
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp, :var, :value] : [:Ts_disSup, :var, :value]) : [:var, :value]
				join_df = unique(select(innerjoin(deSelectSys(part_dic[sSym].var[varSym]), val_df, on = intCol(val_df, :dir)), sel_arr))			
				join_df[!,:scaFac] .=  wgtSca_fl^2	
				expExpr_dic[:capa][sys][sSym][varSym] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df, :Ts_exp => :Ts_disSup) : join_df
			end
		end
	end

	# match storage level values with variables
	expExpr_dic[:stLvl] = Dict{Symbol,DataFrame}()

	if !isempty(var_dic[:stLvl])
		for sSym in keys(var_dic[:stLvl])
			if sSym in keys(mod_m.parts.tech)
				sel_arr = prsvExp ? (:Ts_exp in intCol(val_df) ? [:Ts_exp, :var, :value] : [:Ts_disSup, :var, :value]) : [:var, :value]
				val_df = var_dic[:stLvl][sSym] 
				val_df[!,:value] = val_df[!,:value] ./ mod_m.options.scaFac.dispSt
				join_df = select(innerjoin(val_df, mod_m.parts.tech[sSym].var[:stLvl], on  = intCol(var_dic[:stLvl][sSym])), sel_arr)
				join_df[!,:scaFac] .= weight_ntup.stLvl^2
				expExpr_dic[:stLvl][sSym] = prsvExp && :Ts_exp in intCol(val_df) ? rename(join_df, :Ts_exp => :Ts_disSup) : join_df
			end
		end
	end

	return expExpr_dic
end

# ! write values of entire variables in input model to returned capacity dictionary
function writeResult(in_m::anyModel, var_arr::Array{Symbol,1}; rmvFix::Bool = false, fltSt::Bool = true, roundDown::Int = 0)
	
	# write expansion value
	capa_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	
	for sys in (:tech, :exc)
		capa_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts, sys)
		for sSym in filter(x -> part_dic[x].type in (:stock, :mature, :emerging), keys(part_dic))
			
			# continue in case of technology without changing capacites
			if part_dic[sSym].type == :stock &&  part_dic[sSym].decomm == :none continue end	

			varSym_arr = filter(x -> any(occursin.(string.(var_arr), string(x))), keys(part_dic[sSym].var))

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

			# removes redundant varibles for undirected exchange capacity
			if sys == :exc && !part_dic[sSym].dir && :capaExc in keys(capa_dic[sys][sSym])
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
	stLvl_dic = Dict{Symbol,DataFrame}()

	if :stLvl in var_arr && in_m.options.lvlFrs != 0
		for sSym in keys(in_m.parts.tech)
			if :stLvl in keys(in_m.parts.tech[sSym].var)
				stLvl_dic[sSym] = getResult(copy(in_m.parts.tech[sSym].var[:stLvl]))	
				removeEmptyDic!(stLvl_dic, sSym)
			end
		end
	end

	return capa_dic, stLvl_dic
end

# ! replaces the variable column with a column storing the value of the entire variable
function getResult(res_df::DataFrame)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		# aggregates expansion, if spread across different years
		res_df = combine(groupby(res_df, filter(x -> x != :Ts_expSup, intCol(res_df))), :var => (x -> x[1] * size(x, 1)) => :var)
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms), res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> round(max(0, value(x) - x.constant), digits = 12), res_df[!,:var])

	return select(res_df, Not([:var]))
end

# ! create constraint fixing capacity (or setting a lower limits)
function limitVar!(value_df::DataFrame, var_df::DataFrame, var_sym::Symbol, part_obj::AbstractModelPart, fix_m::anyModel, lim_sym::Symbol=:Fix)

	# compute smallest and biggest capacity that can be enforced
	rngMat_tup = fix_m.options.coefRng.mat
	rngRhs_tup = fix_m.options.coefRng.rhs

	cns_sym = Symbol(:Benders, lim_sym)

	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(var_df, z, on = intCol(z, :dir))) |> (y -> y[completecases(y), :])

	# correct values with scaling factor
	scaFac_sym = lowercase(string(var_sym)) |> (z -> occursin("stlvl", z) ? :dispSt : (occursin("exp", z) ? :insCapa : occursin("stsize", string(z)) ? :capaStSize : :capa))
	fix_df[!,:value]  = fix_df[!,:value] ./ getfield(fix_m.options.scaFac, scaFac_sym)
	
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
	fix_df[!,:fac] = map(x -> x.value < rngRhs_tup[1] ? rngRhs_tup[1]/x.value : (x.value > rngRhs_tup[2] ? rngRhs_tup[2]/x.value : 1.0), eachrow(fix_df))
	fix_df[!,:rhs], fix_df[!,:fac] = map(x -> x.fac < rngMat_tup[1] ?  [rngRhs_tup[2], rngMat_tup[1]] : (x.fac > rngMat_tup[2] && x.setZero ? [0.0, 1.0] : [x.value*x.fac, x.fac]), eachrow(fix_df)) |> (w  -> map(x -> getindex.(w, x), [1, 2]))

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
		fix_df = innerjoin(select(part_obj.cns[Symbol(var_sym, cns_sym)], Not([:fac])), fix_df, on = intCol(fix_df, :dir))
		set_normalized_rhs.(fix_df[!,:cns], fix_df[!,:rhs])
		set_normalized_coefficient.(fix_df[!,:cns], fix_df[!,:var], fix_df[!,:fac])	
	end
	
	part_obj.cns[Symbol(var_sym, cns_sym)] = select(fix_df, Not([:var, :value, :rhs, :setZero]))

	# correct value_df to values actually enforced
	value_df = innerjoin(select(value_df, Not([:value])), select(fix_df, Not([:var, :value, :cns, :setZero])), on = intCol(value_df, :dir))
	corSca_fl = getfield(fix_m.options.scaFac, var_sym == :stLvl ? :dispSt : (occursin("StSize", string(var_sym)) ? :capaStSize : :capa))
	value_df[!,:value] .=  value_df[!,:rhs] ./ value_df[!,:fac] .* corSca_fl
	select!(value_df, Not([:fac, :rhs]))

	return value_df
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame, cns_df::DataFrame, scaFac_fl::Float64)
	new_df = deSelectSys(cns_df) |> (z -> innerjoin(dual_df, z, on = intCol(z, :dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac] .* scaFac_fl
	return select(filter(x -> x.dual != 0.0, new_df), Not([:cns,:fac]))
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, var_df::DataFrame, scaFac_fl::Float64)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(var_df), z, on = intCol(z, :dir)))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual * scaFac_fl * (collect(keys(x.var.terms))[1] - x.value / scaFac_fl), eachrow(ben_df)))
end

# ! removes cases where storage variables are fixed by a ratio (e.g. storage energy capacity fixed by e/p ratio) 
function removeFixStorage(stVar_sym::Symbol, stVar_df::DataFrame, part_obj::TechPart)
	fixPar_dic = Dict(:expStSize => :sizeToStOutFixExp, :expStIn => :stInToConvFixExp, :expStOut => :stOutToStInFixExp, :capaStSize => :sizeToStOutFixCapa, :capaStIn => :stInToConvFixCapa, :capaStOut => :stOutToStInFixCapa)
	if stVar_sym in [:expStSize, :expStIn, :expStOut, :capaStSize, :capaStIn, :capaStOut] && fixPar_dic[stVar_sym] in collect(keys(part_obj.cns))
		fixCns_df = select(part_obj.cns[fixPar_dic[stVar_sym]], Not([:cns]))
		stVar_df = stVar_df |> (x -> antijoin(x, fixCns_df, on = intersect(intCol(x), intCol(fixCns_df))))
	end
	return stVar_df
end

#endregion

#region # * reporting

# write storage levels for case of reduced foresight
function writeStLvlRes(top_m::anyModel, sub_dic::Dict{Tuple{Int64, Int64}, anyModel}, sub_tup::Tuple, i::Int, stReport_df::DataFrame)

	# get levels from top-problem
	for x in filter(x -> :stLvl in keys(top_m.parts.tech[x].var), keys(top_m.parts.tech))
		data_df = printObject(top_m.parts.tech[x].var[:stLvl], top_m, rtnDf = (:csvDf,))
		data_df[!,:i] .= i
		append!(stReport_df, rename(data_df, :variable => :value))
	end

	# get levels from subproblems
	for s in collect(sub_tup)
		for x in filter(x -> :stLvl in keys(sub_dic[s].parts.tech[x].var), keys(sub_dic[s].parts.tech))
			data_df = printObject(sub_dic[s].parts.tech[x].var[:stLvl], sub_dic[s], rtnDf = (:csvDf,))
			data_df[!,:i] .= i
			append!(stReport_df, rename(data_df, :variable => :value))
		end
	end

	CSV.write(modOpt_tup.resultDir * "/stLvl_$(replace(top_m.options.objName, "topModel" => "")).csv", stReport_df)

	return stReport_df
end

# write capacity results for near optimal
function writeCapaRes(top_m::anyModel, sub_dic::Dict{Tuple{Int64, Int64}, anyModel}, sub_tup::Tuple, nearOpt_df::DataFrame, i::Int, nOpt_int::Int, nearOpt_ntup::NamedTuple, topCost_fl::Float64, subCost_fl::Float64, costOpt_fl::Float64, lssOpt_fl::Float64)
	if !isempty(nearOpt_ntup)
		lss_fl = sum(map(x -> sum(value.(sub_dic[x].parts.bal.var[:lss][!,:var])), sub_tup))
		if nOpt_int == 0 || ((topCost_fl + subCost_fl) <= costOpt_fl * (1 + nearOpt_ntup.cutThres) && lss_fl <= lssOpt_fl * (1 + nearOpt_ntup.lssThres))
			newRes_df = getCapaResult(top_m)
			newRes_df[!,:i] .= i
			newRes_df[!,:cost] .= topCost_fl + subCost_fl
			newRes_df[!,:lss] .= lss_fl
			if nOpt_int != 0 newRes_df[!,:thrs] .= (topCost_fl + subCost_fl)/costOpt_fl - 1 end
			append!(nearOpt_df, newRes_df)
		end
	end
	
	return nearOpt_df, lss_fl
end

# report on benders iteration
function reportBenders!(benders_obj::bendersObj)

	report_m = benders_obj.report.mod

	itr_obj = benders_obj.cost
	nearOpt_obj = benders_obj.nearOpt

	timeTop_fl = Dates.toms(timeTop) / Dates.toms(Second(1))
	timeSub_fl = Dates.toms(timeSub) / Dates.toms(Second(1))
	if nearOpt_obj.cnt > 0
		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Objective: $(nearOpt_obj.setup.obj[nearOpt_obj.cnt][1]), Objective value: $(round(itr_obj.nearOpt, sigdigits = 8)), Feasibility gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
	else
		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Lower: $(round(itr_obj.costEst, sigdigits = 8)), Upper: $(round(itr_obj.best.objVal, sigdigits = 8)), Optimality gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
	end
	produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Time for top: $timeTop_fl Time for sub: $timeSub_fl", testErr = false, printErr = false)

	# write to reporting files
	etr_arr = Pair{Symbol,Any}[:i => i, :lowCost => itr_obj.costEst, :bestObj => itr_obj.best.objVal, :gap => gap_fl, :curCost => itr_obj.curObj,
					:time_ges => Dates.value(floor(now() - benders_obj.report.mod.options.startTime, Dates.Second(1)))/60, :time_top => timeTop_fl/60, :time_sub => timeSub_fl/60]
	
	if !isempty(meth_tup) # add info about stabilization
		stab_obj = benders_obj.stab
		push!(etr_arr, :actMethod => stab_obj.method[stab_obj.actMet])	
		append!(etr_arr, map(x -> Symbol("dynPar_", stab_obj.method[x]) => isa(stab_obj.dynPar[x], Dict) ? [round(stab_obj.dynPar[x][j], sigdigits = 2) for j in keys(stab_obj.dynPar[x])] : stab_obj.dynPar[x], 1:length(stab_obj.method)))
	end

	# add info about near-optimal
	if !isnothing(nearOpt_obj.setup)
		push!(etr_arr, :objective => nearOpt_obj.cnt > 0 ? nearOpt_obj.setup.obj[nearOpt_obj.cnt][1] : "cost") 
	end

	push!(itrReport_df, (;zip(getindex.(etr_arr, 1), getindex.(etr_arr, 2))...))
	if i%reportFreq == 0 
		CSV.write(benders_obj.report.mod.options.outDir * "/iterationCuttingPlane_$(benders_obj.info.name).csv", benders_obj.report.itr)
		if !isnothing(nearOpt_obj.setup) CSV.write(benders_obj.report.mod.options.outDir * "/nearOptSol_$(benders_obj.info.name).csv", benders_obj.report.nearOpt) end
	end

	if !isempty(nearOpt_ntup) nearOpt_df, itr_obj.res[:nOptLss] = writeCapaRes(top_m,sub_dic,sub_tup,nearOpt_df,i,nOpt_int,nearOpt_ntup,topCost_fl,subCost_fl,costOpt_fl,lssOpt_fl) end

	if Dates.value(floor(now() - report_m.options.startTime,Dates.Minute(1))) > timeLim
		produceMessage(report_m.options,report_m.report, 1," - Aborted due to time-limit!", testErr = false, printErr = false)
		break
	end

end

#endregion