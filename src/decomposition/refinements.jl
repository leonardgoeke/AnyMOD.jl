
#region # * stabilization

# initialize stabilization when creating benders object, returns the stabilization object
function initializeStab!(benders_obj::bendersObj, stabSetup_obj::stabSetup, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, info_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, scale_dic::Dict{Symbol,NamedTuple}, runSubDist::Function)

	report_m = benders_obj.report.mod

	if !isempty(stabSetup_obj.method)

		cutData_dic = Dict{Tuple{Int64,Int64},resData}()
		time_dic = Dict{Tuple{Int64,Int64},Millisecond}()
	
		#region # * compute heuristic solution
	
		heuOpt_ntup = (inputDir = inputFolder_ntup.heu, resultDir = inputFolder_ntup.results, suffix = info_ntup.name, supTsLvl = info_ntup.supTsLvl, repTsLvl = info_ntup.repTsLvl, frsLvl = info_ntup.frsLvl, shortExp = info_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facHeu])
		
		# ! get starting solution with heuristic solve or generic
		if stabSetup_obj.ini != :none
			
			produceMessage(report_m.options, report_m.report, 1, " - Started heuristic pre-solve for starting solution", testErr = false, printErr = false)
			# get heuristic solution and get a close feasible solution
			heu_m, heuSol_obj = heuristicSolve(heuOpt_ntup, benders_obj.algOpt.top.threads, benders_obj.algOpt.opt, rtrnMod = true, solDet = true, fltSt = false);
			top_m = benders_obj.top
			startSol_obj = resData()
			lowBd_fl = 0.0
				
			top_m = computeFeas(top_m, heuSol_obj.capa, 0.001, cutSmall = false);
		
			# write results for heuristic solution
			(startSol_obj.capa, startSol_obj.stLvl, startSol_obj.lim) = writeResult(top_m, [:capa, :exp, :mustCapa, :stLvl, :lim]; rmvFix = true)
			startSol_obj.objVal = value(top_m.parts.obj.var[:objVar][1,:var])
			startRes_dic = Dict(x => reportResults(x, top_m, rtnOpt = (:csvDf,)) for x in benders_obj.report.res.general)
 			
			# reset objective
			setObjective!(:cost, top_m)

			# remove fixing constraints and variables again
			for tSym in keys(top_m.parts.tech)
				# constraint deletion
				for c in filter(x -> any(occursin.(["absUp","absLow","cutSmall"], string(x))), collect(keys(top_m.parts.tech[tSym].cns)))
					delete.(top_m.optModel, top_m.parts.tech[tSym].cns[c][!,:cns])
					delete!(top_m.parts.tech[tSym].cns, c)
				end
				# variable deletion
				for v in filter(x -> any(occursin.(["cutSmall","abs"], string(x))), collect(keys(top_m.parts.tech[tSym].var)))
					foreach(x -> delete(top_m.optModel, collect(keys(x.terms))[1]), top_m.parts.tech[tSym].var[v][!,:var])
					foreach(x -> unregister(top_m.optModel, Symbol(collect(keys(x.terms))[1])), top_m.parts.tech[tSym].var[v][!,:var])
					delete!(top_m.parts.tech[tSym].var, v)
				end
			end
			
			for excSym in keys(top_m.parts.exc)
				for c in filter(x -> any(occursin.(["absUp","absLow","cutSmall"], string(x))), collect(keys(top_m.parts.exc[excSym].cns)))
					delete.(top_m.optModel, top_m.parts.exc[excSym].cns[c][!,:cns])
					delete!(top_m.parts.exc[excSym].cns, c)
				end
				# variable deletion
				for v in filter(x -> any(occursin.(["cutSmall","abs"], string(x))), collect(keys(top_m.parts.exc[excSym].var)))
					foreach(x -> delete(top_m.optModel, collect(keys(x.terms))[1]), top_m.parts.exc[excSym].var[v][!,:var])
					foreach(x -> unregister(top_m.optModel, Symbol(collect(keys(x.terms))[1])), top_m.parts.exc[excSym].var[v][!,:var])
					delete!(top_m.parts.exc[excSym].var, v)
				end
			end	
		else
			optimize!(benders_obj.top.optModel)
			startSol_obj = resData()
			startSol_obj.objVal = value(benders_obj.top.parts.obj.var[:objVar][1,:var])
			startSol_obj.capa, startSol_obj.stLvl, startSol_obj.lim  = writeResult(benders_obj.top, [:capa, :exp, :mustCapa, :stLvl, :lim]; rmvFix = true)
			lowBd_fl = startSol_obj.objVal

			startRes_dic = Dict(x => reportResults(x, benders_obj.top, rtnOpt = (:csvDf,)) for x in benders_obj.report.res.general)
		end

		# correct capacities, if mustCapa exceeds capa
		startSol_obj = correctMustCapa(startSol_obj)
	
		#endregion
	
		#region # * evaluate heuristic solution
	
		# first result for first iteration
		firstItr_df = DataFrame(i = 0, lowCost = 0, bestObj = Inf, gap = 1.0, curCost = Inf, time_ges = Dates.value(floor(now() - report_m.options.startTime, Dates.Second(1)))/60, time_top = 0, time_waitNoStab = 0, time_subTot = 0, cntCuts = 0, time_sub = Float64[], numFoc = Int[], objName = benders_obj.info.name)
		if !isnothing(benders_obj.nearOpt.setup) firstItr_df[!,:objective] .= "cost" end
		if !isempty(stabSetup_obj.method) 
			firstItr_df[!,:actMethod] .= Symbol()
			foreach(x -> firstItr_df[!, Symbol("dynPar_",x[1])] .= 0.0, stabSetup_obj.method)
		end
		append!(benders_obj.report.itr, firstItr_df)

		# create dictionaries to store results
		if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
		time_dic = Dict{Tuple{Int64,Int64},Millisecond}()
		numFoc_dic = Dict{Tuple{Int64,Int64},Int64}()
		
		# solve sub-problems
		for (id, s) in enumerate(sort(collect(keys(benders_obj.sub))))
			if benders_obj.algOpt.dist # distributed case
				futData_dic[s] = runSubDist(id + 1, copy(startSol_obj), benders_obj.algOpt.rngVio.fix, benders_obj.algOpt.sub.meth, 1e-8)
			else # non-distributed case
				cutData_dic[s], time_dic[s], ~, numFoc_dic[s] = runSub(benders_obj.sub[s], copy(startSol_obj), benders_obj.algOpt.rngVio.fix, benders_obj.algOpt.sub.meth, 1e-8)
			end
		end
		
		# get solutions
		if benders_obj.algOpt.dist
			wait.(collect(values(futData_dic)))
			for s in sort(collect(keys(benders_obj.sub)))
				cutData_dic[s], time_dic[s], ~, numFoc_dic[s] = fetch(futData_dic[s])
			end
		end

		# store information for cuts
		colCuts_arr = Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}()
		for cut in collect(cutData_dic)
			cut_expr, limCoef_boo = createCutExpr(cut, benders_obj.top.optModel, benders_obj.algOpt.rngVio.cut, benders_obj.top)
			push!(colCuts_arr, (1,cut[1][1], cut[1][2]) => (cut_expr, limCoef_boo))
		end

		benders_obj.cuts.all = colCuts_arr
		benders_obj.cuts.slack = map(x -> Float64[], 1:length(benders_obj.cuts.all))
		benders_obj.cuts.active = collect(1:length(benders_obj.cuts.all))

		# create copy for problem without stabilization
		noStab_opt, ref_refm = copy_model(benders_obj.top.optModel)
		set_optimizer(noStab_opt, benders_obj.algOpt.opt)
		set_optimizer_attribute(noStab_opt, "Threads", benders_obj.algOpt.top.threads)	
		benders_obj.topNoStab = (opt = noStab_opt, ref = ref_refm)


		# create and directly add cuts for top problem without stabilization
		colCutsNoStab_arr = Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}() 
		for cut in collect(cutData_dic)
			cut_expr, limCoef_boo = createCutExpr(cut, benders_obj.topNoStab.opt, benders_obj.algOpt.rngVio.cut, benders_obj.top, benders_obj.topNoStab.ref)
			push!(colCutsNoStab_arr, (1, cut[1][1], cut[1][2])  => (cut_expr, limCoef_boo))
		end
		addCuts!(benders_obj.top, benders_obj.topNoStab.opt, benders_obj.algOpt.rngVio.cut, colCutsNoStab_arr, true)


		# analyse results
		startSol_obj.objVal = startSol_obj.objVal + sum(map(x -> x.objVal, values(cutData_dic)))
		timeSubTot_fl = Dates.toms(benders_obj.algOpt.dist ? maximum(collect(values(time_dic))) : sum(collect(values(time_dic)))) / Dates.toms(Second(1))
		timeSub_arr = round.(getindex.(sort(collect(time_dic)),2) |> (ms -> Dates.toms.(ms) / Dates.toms(Second(1)) ./ 60) , sigdigits = 3)
		numFoc_arr = getindex.(sort(collect(numFoc_dic)),2)
		
		# write results for second iteration
		secItr_df = DataFrame(i = 1, lowCost = lowBd_fl, bestObj = startSol_obj.objVal, gap = 1 - lowBd_fl/startSol_obj.objVal, curCost = startSol_obj.objVal, time_ges = Dates.value(floor(now() - report_m.options.startTime, Dates.Second(1)))/60, time_top = 0, time_waitNoStab = 0, time_subTot = timeSubTot_fl/60, time_sub = [timeSub_arr], cntCuts = [0], numFoc = [numFoc_arr], objName = benders_obj.info.name)
		if !isnothing(benders_obj.nearOpt.setup) secItr_df[!,:objective] .= "cost" end
		if !isempty(stabSetup_obj.method) 
			secItr_df[!,:actMethod] .= Symbol()
			foreach(x -> secItr_df[!,Symbol("dynPar_",x[1])] .= 0.0, stabSetup_obj.method)
		end

		append!(benders_obj.report.itr, secItr_df)

		startSol_tup = (var = startSol_obj, res = startRes_dic, startLvl = Dict{Symbol, DataFrame}())

		#endregion

		#region # * initialize stabilization 

		stab_obj, eleNum_int = stabObj(stabSetup_obj.method, stabSetup_obj.srsThr, stabSetup_obj.lowLimVal, stabSetup_obj.switch, stabSetup_obj.weight, startSol_obj, lowBd_fl, stabSetup_obj.solveNoStab, stabSetup_obj.repVio, benders_obj.top);

		stabVio_df = centerStab!(stab_obj.method[stab_obj.actMet], stab_obj, benders_obj.algOpt.rngVio.stab, benders_obj.top, report_m);
		stabVio_df[!,:i] .= 0
		append!(benders_obj.report.stabVio, stabVio_df)

		conv_int = length(vcat(vcat(vcat(map(y -> stab_obj.var[:capa][:tech][y] |> (w -> map(z -> w[z][!,:value], filter(p -> occursin("Conv",string(p)), collect(keys(w))))), collect(keys(stab_obj.var[:capa][:tech]))))...)...))
		st_int = length(vcat(vcat(vcat(map(y -> stab_obj.var[:capa][:tech][y] |> (w -> map(z -> w[z][!,:value], filter(p -> !occursin("Conv",string(p)), collect(keys(w))))), collect(keys(stab_obj.var[:capa][:tech]))))...)...))
		exc_int = length(vcat(vcat(vcat(map(y -> stab_obj.var[:capa][:exc][y] |> (w -> map(z -> w[z][!,:value], collect(keys(w)))), collect(keys(stab_obj.var[:capa][:exc]))))...)...))
		stLvl_int = length(vcat(vcat(map(x -> stab_obj.var[:stLvl][x] |> (u -> map(y -> u[y][!,:value], collect(keys(u)))), collect(keys(stab_obj.var[:stLvl])))...)...))
		lim_int = length(vcat(map(x -> stab_obj.var[:lim][x][!,:value], collect(keys(stab_obj.var[:lim])))...))		

		#endregion

		produceMessage(report_m.options, report_m.report, 1, " - Initialized stabilization with $eleNum_int variables (conversion expansion $conv_int, storage expansion $st_int, exchange expansion $exc_int, storage level $stLvl_int, limits $lim_int)", testErr = false, printErr = false)
	else
		# create empty stabilization object
		stab_obj = nothing
		startSol_obj = resData()
		startSol_tup = (var = startSol_obj, res = Dict{Symbol,DataFrame}(), startLvl = Dict{Symbol, DataFrame}())
		# copy reference to non-statablized problem
		if !isempty(stabSetup_obj.method) benders_obj.topNoStab = (opt = nothing, ref = nothing) end
	end

	return stab_obj, startSol_tup

end

# write options of stabilization method
function writeStabOpt(meth_tup::Tuple, lowBd_fl::Float64, upBd_fl::Float64, top_m::anyModel)

	# set fields for name and options of method
	meth_arr = Symbol[]
	methOpt_arr = NamedTuple[]
	for (key, val) in meth_tup
		push!(meth_arr, key)
		push!(methOpt_arr, val)
		if key == :qtr && !isempty(setdiff(keys(val), (:start, :low, :thr, :fac)))
			error("options provided for trust-region do not match the defined options 'start', 'low', 'thr', and 'fac'")
		elseif key == :prx && !isempty(setdiff(keys(val), (:start, :min, :a)))
			error("options provided for proximal bundle do not match the defined options 'start', 'min' and 'a'")
		elseif key == :prx2 && !isempty(setdiff(keys(val), (:start, :min, :a)))
			error("options provided for proximal bundle do not match the defined options 'start', 'min' and 'a'")
		elseif key in (:lvl1,:lvl3) && !isempty(setdiff(keys(val), (:lam,)))
			error("options provided for level bundle do not match the defined option 'lam'")
		elseif key == :lvl2 && !isempty(setdiff(keys(val), (:lam, :myMax)))
			error("options provided for level bundle do not match the defined options 'lam', 'myMax'")
		elseif key == :qtrLvl && !isempty(setdiff(keys(val), (:startRad, :endRad, :inter, :lam)))
			error("options provided for trust-region level bundle do not match the defined options 'startRad', 'endRad', 'inter', and 'lam'")
		elseif key == :box && !isempty(setdiff(keys(val), (:low, :up, :minDelta, :thr, :fac, :scaLvl, :scaLim)))
			error("options provided for trust-region do not match the defined options 'low', 'up', 'minDelta', 'thr', 'fac', 'scaLvl', and 'scaLim'")
		elseif key == :dsb && !isempty(setdiff(keys(val), (:start, :min, :lam, :myMax)))
			error("options provided for doubly stabilized bundle do not match the defined options 'start', 'min', 'lam', 'myMax'")
		end
	end

	if length(meth_arr) != length(unique(meth_arr)) error("stabilization methods must be unique") end

	# method specific adjustments (e.g. starting value for dynamic parameter, new variables for objective function)
	dynPar_arr = computeDynPar(meth_arr, methOpt_arr, lowBd_fl, upBd_fl, top_m)

	return meth_arr, methOpt_arr, dynPar_arr
end

# compute dynamic parameter
function computeDynPar(meth_arr::Array{Symbol, 1}, methOpt_arr::Array{NamedTuple, 1}, lowBd_fl::Float64, upBd_fl::Float64, top_m::anyModel)
	
	dynPar_arr = []
	for m in 1:size(meth_arr, 1)
		if meth_arr[m] in (:prx1, :prx2)
			dynPar = Dict(:prx => methOpt_arr[m].start, :prxAux => methOpt_arr[m].start) # starting value for penalty
		elseif meth_arr[m] in (:lvl1, :lvl3)
			dynPar = (methOpt_arr[m].lam * lowBd_fl  + (1 - methOpt_arr[m].lam) * upBd_fl) / top_m.options.scaFac.obj # starting value for level
			if methOpt_arr[m].lam >= 1 || methOpt_arr[m].lam <= 0 
				error("lambda for level bundle must be strictly between 0 and 1")
			end
		elseif meth_arr[m] == :lvl2
			dynPar = Dict(:yps => (1 - methOpt_arr[m].lam) * (upBd_fl - lowBd_fl) / top_m.options.scaFac.obj, :my => 0.0)
			if methOpt_arr[m].lam >= 1 || methOpt_arr[m].lam <= 0 
				error("lambda for level bundle must be strictly between 0 and 1")
			end
		elseif meth_arr[m] == :qtr
			dynPar = methOpt_arr[m].start # starting value for radius
		elseif meth_arr[m] == :box
			dynPar = 1.0
		elseif meth_arr[m] == :qtrLvl
			dynPar = Dict(:lvl  => (methOpt_arr[m].lam * lowBd_fl  + (1 - methOpt_arr[m].lam) * upBd_fl) / top_m.options.scaFac.obj, :qtr => methOpt_arr[m].startRad)
		elseif meth_arr[m] == :dsb
			dynPar = Dict(:yps=>(1  -methOpt_arr[m].lam) * (upBd_fl - lowBd_fl) / top_m.options.scaFac.obj, :prx => methOpt_arr[m].start, :my => 1.0)
		else
			error("unknown stabilization method provided, method must either be 'prx', 'lvl', 'qtr', or 'box'")
		end
		push!(dynPar_arr, dynPar)
	end
	return dynPar_arr
end

# function to update the center of stabilization method
centerStab!(method::Symbol, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel; forceRad::Bool = false) = centerStab!(Val{method}(), stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)

# function for quadratic trust region
function centerStab!(method::Val{:qtr}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# create quadratic constraint
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl, relRhs = stab_obj.dynPar[stab_obj.actMet])
	stab_obj.cns = @constraint(top_m.optModel,  qtrConsSca_expr <= 0.0)

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df

end

# function for proximal bundle method
function centerStab!(method::Union{Val{:prx1},Val{:prx2}}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)

	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# create quadratic expression
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl, fac =  1/(2 * stab_obj.dynPar[stab_obj.actMet][:prx]))
	
	# adjust objective function
	@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] + qtrConsSca_expr)

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df
end

# functions for level bundle methods
function centerStab!(method::Val{:lvl1}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# create quadratic expression
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl)

	# adjust objective function and level set
	@objective(top_m.optModel, Min, qtrConsSca_expr)
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], stab_obj.dynPar[stab_obj.actMet])

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df
end

function centerStab!(method::Val{:lvl2}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# create quadratic expression
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl)

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps] 

	# adjust objective function and level set
	@objective(top_m.optModel, Min, qtrConsSca_expr)
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], ell_fl)

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df
end

function centerStab!(method::Val{:lvl3}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# adjust objective function and level set
	@objective(top_m.optModel, Min, 0.0)
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], stab_obj.dynPar[stab_obj.actMet])

	return repVio_df
end

# function for box step method
function centerStab!(method::Val{:box}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)

	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var, stab_obj.weight, top_m)
	
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!, [:var, :value, :scaFac]], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)
	allCapa_df[!,:negPos] .= false
	allCapa_df[!,:scalBox] .= 1.0 * stab_obj.dynPar[stab_obj.actMet]

	empty_df = DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[], negPos = Bool[])

	allStLvl_df = vcat(vcat(map(x -> expExpr_dic[:stLvl][x] |> (u -> map(y -> u[y], collect(keys(u)))), collect(keys(expExpr_dic[:stLvl])))...)...) |> (z -> isempty(z) ? empty_df : z)
	allStLvl_df[!,:negPos] .= occursin.("stLvlInter", string.(allStLvl_df[!,:var]))
	allStLvl_df[!,:scalBox] .=  stab_obj.methodOpt[stab_obj.actMet].scaLvl * stab_obj.dynPar[stab_obj.actMet]

	allLim_df = vcat(map(x -> expExpr_dic[:lim][x], collect(keys(expExpr_dic[:lim])))...) |> (z -> isempty(z) ? empty_df : select(z, [:var, :value, :scaFac]))
	allLim_df[!,:negPos] .= true
	allLim_df[!,:scalBox] .= stab_obj.methodOpt[stab_obj.actMet].scaLim * stab_obj.dynPar[stab_obj.actMet]

	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df, allStLvl_df, allLim_df))

	# set lower and upper bound
	minDelta_fl = stab_obj.methodOpt[stab_obj.actMet].minDelta
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_lower_bound(z[1], getLowerBound(x.value, minDelta_fl * x.scalBox, x.negPos, stab_obj.methodOpt[stab_obj.actMet].low * x.scalBox, top_m.options.coefRng.rhs[1]))), eachrow(allVar_df))
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_upper_bound(z[1], getUpperBound(x.value, minDelta_fl * x.scalBox, stab_obj.methodOpt[stab_obj.actMet].up * x.scalBox))), eachrow(allVar_df))

	return DataFrame(var = String[], fac = Float64[], type = Symbol[])

end

# ! get lower bound for boxstep
function getLowerBound(value_fl::Float64, minDelta_fl::Float64, negPos_boo::Bool, perLow_fl::Float64, lowerRng_fl::Float64)
    # get lower bound based on percentage
    corMin_fl = value_fl * (1 - perLow_fl * (value_fl >= 0.0 ? 1.0 : -1.0))
    # correct for minimum delta respecting potential lower bound of zero
	if value_fl - minDelta_fl < corMin_fl corMin_fl = value_fl - minDelta_fl end
	if !negPos_boo corMin_fl = max(corMin_fl, 0.0) end
	# correct to avoid range violation
    if abs(corMin_fl) < lowerRng_fl corMin_fl = 0.0 end
    return corMin_fl
end

# ! get upper bound for boxstep
function getUpperBound(value_fl::Float64, minDelta_fl::Float64, perUp_fl::Float64)
    # get upper bound based on percentage
    rel_fl = value_fl * (1 + perUp_fl * (value_fl >= 0.0 ? 1.0 : -1.0))
    # correct for minimum delta respecting potential
    corMax_fl = max(value_fl + minDelta_fl, rel_fl)
    return corMax_fl
end

# function for level paired with quadratic trust region 
function centerStab!(method::Val{:qtrLvl}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 0)

	# create quadratic constraint
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl, relRhs = stab_obj.dynPar[stab_obj.actMet][:qtr])
	stab_obj.cns = @constraint(top_m.optModel,  qtrConsSca_expr <= 0.0)

	# adjust objective function and level set
	if objective_sense(top_m.optModel) == MOI.MIN_SENSE 
		@objective(top_m.optModel, Max, 0.0)
	else
		@objective(top_m.optModel, Min, 0.0)
	end
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], stab_obj.dynPar[stab_obj.actMet][:lvl])

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df
end

# function for doubly stabilized bundle method
function centerStab!(method::Val{:dsb}, stab_obj::stabObj, rngVio_fl::Float64, top_m::anyModel, report_m::anyModel, forceRad::Bool)
	
	# set dual option according to demands of method 
	@suppress set_optimizer_attribute(top_m.optModel, "QCPDual", 1)

	# create quadratic expression
	qtrConsSca_expr = computeQuadExp(top_m, stab_obj, rngVio_fl, fac =  0.5 * stab_obj.dynPar[stab_obj.actMet][:prx])

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]
	
	# adjust objective function and level set
	stab_obj.helper_var = @variable(top_m.optModel, r)
	
	@objective(top_m.optModel, Min, r + qtrConsSca_expr)
	stab_obj.cns = @constraint(top_m.optModel, top_m.parts.obj.var[:obj][1, 1] <= r)
	set_upper_bound(stab_obj.helper_var, ell_fl)

	# report violation
	repVio_df = reportRngViolations(qtrConsSca_expr, top_m.options.coefRng.mat, rngVio_fl, stab_obj.repVio)

	return repVio_df
end

# compute quadratic expression for stabilization
function computeQuadExp(top_m::anyModel, stab_obj::stabObj, rngVio_fl::Float64; relRhs::Float64 = 0.0, fac::Float64 = 1.0)

	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# set small capacity values to zero or smallest possible value within range, whatever is more accurate
	lowerLimTrust_fl = stab_obj.lowLimVal
	allVar_df[!,:corValue] = map(x -> x != 0.0 && abs(x) < lowerLimTrust_fl ? (x < lowerLimTrust_fl / 2 ? 0.0 : lowerLimTrust_fl) : x, allVar_df[!,:value])
	
	# compute minimum size of rhs to ensure that correction of capacity does not exclude current best from the trust region
	delta_fl = sum((allVar_df[!,:value] - allVar_df[!,:corValue]).^2)

	# absolute value for rhs of equation
	abs_fl = sum(max.(allVar_df[!,:corValue], allVar_df[!,:scaFac]) .* allVar_df[!,:scaFac])	
	
	# computes constraint expression
	capaSum_expr = fac * sum(map(x -> sum(collect(keys(x.var.terms))) |> (z -> x.scaFac * (z^2 - 2 * x.corValue * z + x.corValue^2)), eachrow(allVar_df)))
	qtrCons_expr = capaSum_expr - (delta_fl + relRhs * abs_fl)
	
	# scaling factors
	coefRng_tup = (top_m.options.coefRng.mat[1], top_m.options.coefRng.mat[2] * rngVio_fl)
	matRng_tup = abs.(values(qtrCons_expr.aff.terms)) |> (y -> isempty(y) ? (1.0, 1.0) : (minimum(y), maximum(y)))
	qtrConsSca_expr = scaleRng([qtrCons_expr], [matRng_tup], coefRng_tup, false)[1]

	# scaling rhs
	rhsRange_arr = (top_m.options.coefRng.rhs[1], top_m.options.coefRng.rhs[2] * rngVio_fl)
	
	if abs(qtrConsSca_expr.aff.constant) < rhsRange_arr[1] # upscaling rhs
		qtrConsSca_expr = qtrConsSca_expr * rhsRange_arr[1] / abs(qtrConsSca_expr.aff.constant)
	elseif abs(qtrConsSca_expr.aff.constant) > rhsRange_arr[2] # downscaling rhs
		qtrConsSca_expr = qtrConsSca_expr * rhsRange_arr[2] / abs(qtrConsSca_expr.aff.constant)
	end

	return qtrConsSca_expr

end

# write function to compute poorman's Hessian auxilary scalar for prx_2
function computePrx2Aux(prevCuts_arr::Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1})

	diffVal_arr = Float64[]
	diffDual_arr = Float64[]
	
	for cut in prevCuts_arr # loop over cut data
		
		scr = (cut[1][2], cut[1][3])
		relCut_obj = cut[2][1]

		for sys in (:exc, :tech)
			
			allSys_arr = unique(union(keys(relCut_obj.capa[sys]), keys(cut[2].capa[sys])))
			
			for sSym in allSys_arr
				
				# get relevant dictionaries for systems (handles problem, if system only exits in current or previous)
			
				curCapa_dic = sSym in keys(relCut_obj.capa[sys]) ? relCut_obj.capa[sys][sSym] : Dict{Symbol, DataFrame}()
				prevCapa_dic = sSym in keys(cut[2].capa[sys]) ? cut[2].capa[sys][sSym] : Dict{Symbol, DataFrame}()

				allVar_arr = unique(union(keys(curCapa_dic), keys(prevCapa_dic)))
				
				for capaSym in filter(x-> occursin("capa", lowercase(string(x))), allVar_arr)
					
					# get current and previous values
					if capaSym in keys(curCapa_dic) 
						curCut_df = curCapa_dic[capaSym]
					else # case if capacity variable only exists in previous 
						curCut_df = filter(x -> false, copy(prevCapa_dic[capaSym]))
						end
					curCut_df = rename(curCut_df, [:value, :dual] .=> [:valueCur, :dualCur])

					if capaSym in keys(prevCapa_dic) 
						prevCut_df = prevCapa_dic[capaSym]
					else # case if capacity variable only exists in current 
						prevCut_df = filter(x -> false, copy(curCapa_dic[capaSym]))
					end
					prevCut_df = rename(prevCut_df, [:value, :dual] .=> [:valuePrev, :dualPrev])
					
					# join values for current and previous cut to compute difference
					join_df = joinMissing(curCut_df, prevCut_df, intCol(curCut_df, :dir), :outer, Dict(:valueCur => 0, :dualCur => 0, :valuePrev => 0, :dualPrev=>0))
					join_df[!,:valueDiff] = join_df[!,:valueCur] .- join_df[!,:valuePrev]
					join_df[!,:dualDiff] =  join_df[!,:dualCur] .- join_df[!,:dualPrev]
					# add difference to array
					append!(diffVal_arr, join_df[!,:valueDiff])
					append!(diffDual_arr, join_df[!,:dualDiff])
				end
			end
		end
	end
	return dot(diffVal_arr, diffDual_arr)/norm(diffDual_arr, 2)
end

# update dynamic parameter of stabilization method
function adjustDynPar!(x_int::Int, stab_obj::stabObj, top_m::anyModel, itr_obj::itrStatus, srsStep_boo::Bool, prx2Aux_fl::Union{Float64,Nothing}, nearOpt_boo::Bool, tarGap_fl::Float64, report_ntup::NamedTuple{(:itr,:nearOpt,:stabVio,:res,:mod),Tuple{DataFrame,DataFrame,DataFrame,NamedTuple,anyModel}})

	opt_tup = stab_obj.methodOpt[x_int]
	if stab_obj.method[x_int] == :qtr # adjust radius of quadratic trust-region
		# reduce radius when trust-region is not binding
		if (nearOpt_boo ? abs(1 - itr_obj.res[:nearObjNoStab] / itr_obj.res[:nearObj]) < opt_tup.thr : abs(1 - itr_obj.res[:estTotCostNoStab] / itr_obj.res[:estTotCost]) < opt_tup.thr) && stab_obj.dynPar[x_int] > opt_tup.low
			stab_obj.dynPar[x_int] = max(opt_tup.low, stab_obj.dynPar[x_int] / opt_tup.fac)
			stab_obj.lastSw = itr_obj.cnt.i
			produceMessage(report_ntup.mod.options, report_ntup.mod.report, 1, " - Reduced quadratic trust-region!", testErr = false, printErr = false)	
		# extend radius if algorithm "got stuck", applies same criterium as switching entire method
		elseif checkSwitch(stab_obj, itr_obj.cnt, report_ntup.itr)
			stab_obj.lastSw = itr_obj.cnt.i
			stab_obj.dynPar[x_int] = max(opt_tup.low, stab_obj.dynPar[x_int] * opt_tup.fac)
			produceMessage(report_ntup.mod.options, report_ntup.mod.report, 1, " - Extended quadratic trust-region!", testErr = false, printErr = false)
		end
	elseif stab_obj.method[x_int] in (:prx1, :prx2) # adjust penalty term of proximal term, implementation according to doi.org/10.1007/s10107-015-0873-6, section 5.1.2
		# compute τ_aux
		aux_fl = stab_obj.method[x_int] == :prx1 ? (stab_obj.objVal - itr_obj.res[:actTotCost])/(stab_obj.objVal - itr_obj.res[:estTotCost]) : prx2Aux_fl 
		# We introduce a safeguard ensuring that :prx is only updated if the numerator of the aux term is positive (see https://doi.org/10.1007/978-3-030-34910-3 Chapter 3 for a discussion)
		stab_obj.dynPar[x_int][:prxAux] = stab_obj.method[x_int] == :prx1 ? 2 * stab_obj.dynPar[x_int][:prx] * (1+aux_fl) : stab_obj.dynPar[x_int][:prx]*(1+max(aux_fl/1e3, 0))
		# check if serious step
		if srsStep_boo
			# adjust τ_aux, if last 5 steps have been serious
			if itr_obj.cnt.srs > 5
				stab_obj.dynPar[x_int][:prxAux] = opt_tup.a * stab_obj.dynPar[x_int][:prxAux]
			end
			# update proximal term
			stab_obj.dynPar[x_int][:prx] = min(stab_obj.dynPar[x_int][:prxAux], 10 * stab_obj.dynPar[x_int][:prx])
		else # if null-step
			if itr_obj.cnt.null > 10
				stab_obj.dynPar[x_int][:prx] = (opt_tup.a) * stab_obj.dynPar[x_int][:prx]
			end
			stab_obj.dynPar[x_int][:prx] = min(stab_obj.dynPar[x_int][:prx], max(stab_obj.dynPar[x_int][:prxAux], stab_obj.dynPar[x_int][:prx]/opt_tup.a, opt_tup.min))
		end
		# another safeguard preventing the proximal parameter to explode
		if stab_obj.method[x_int] == :prx2
			if stab_obj.dynPar[x_int][:prx]>1e6
				stab_obj.dynPar[x_int][:prx] = opt_tup.start
			end
		end
	elseif stab_obj.method[x_int] in (:lvl1,:lvl3) # adjust level, lvl3 uses no objective function as in https://ieeexplore.ieee.org/abstract/document/10829583
		stab_obj.dynPar[x_int] = (opt_tup.lam * itr_obj.res[:estTotCostNoStab]  + (1 - opt_tup.lam) * itr_obj.res[:curBest]) / top_m.options.scaFac.obj
	elseif stab_obj.method[x_int] == :lvl2 # adjust level, implementation according to doi.org/10.1007/s10107-015-0873-6 
		stab_obj.dynPar[x_int][:my] = 1 - itr_obj.res[:lvlDual]
		if srsStep_boo
			stab_obj.dynPar[x_int][:yps] = min(stab_obj.dynPar[x_int][:yps], (1-opt_tup.lam)*(itr_obj.res[:curBest] - itr_obj.res[:estTotCostNoStab]) / top_m.options.scaFac.obj)
		else
			if stab_obj.dynPar[x_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[x_int][:yps] = opt_tup.lam*stab_obj.dynPar[x_int][:yps]
			end
		end
	elseif stab_obj.method[x_int] == :qtrLvl
		
		# update level parameter and radius
		newLvl_fl = (opt_tup.lam * itr_obj.res[:estTotCostNoStab] + (1 - opt_tup.lam) * itr_obj.res[:curBest]) / top_m.options.scaFac.obj
		# avoid decreasing the level parameter at non-serious step to prevent infeasible top problem
		if srsStep_boo
			stab_obj.dynPar[x_int][:lvl] = newLvl_fl
		else
			stab_obj.dynPar[x_int][:lvl] = max(newLvl_fl, stab_obj.dynPar[x_int][:lvl])
		end
		stab_obj.dynPar[x_int][:qtr] = interItrPar(itr_obj.gap, tarGap_fl, [opt_tup.startRad, opt_tup.endRad], Symbol(opt_tup.inter))

	elseif stab_obj.method[x_int] == :dsb # adjust doubly stabilized method, implementation according to doi.org/10.1007/s10107-015-0873-6
		stab_obj.dynPar[x_int][:my] = min(1 - itr_obj.res[:lvlDual], opt_tup.myMax + 1.0)
		if srsStep_boo
			stab_obj.dynPar[x_int][:prx] = (stab_obj.dynPar[x_int][:my]) * stab_obj.dynPar[x_int][:prx] # added a fixed scaler for the dual variable to avoid extremely large values for prx
			stab_obj.dynPar[x_int][:yps] = min(stab_obj.dynPar[x_int][:yps], (1 - opt_tup.lam)*(itr_obj.res[:curBest]- itr_obj.res[:estTotCostNoStab]) / top_m.options.scaFac.obj)
		else
			newPrx_fl = stab_obj.dynPar[x_int][:prx] * (stab_obj.dynPar[x_int][:yps] / ((itr_obj.res[:curBest] - itr_obj.res[:estTotCostNoStab]) / top_m.options.scaFac.obj))
			stab_obj.dynPar[x_int][:prx] = max(opt_tup.min, newPrx_fl)
			if stab_obj.dynPar[x_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[x_int][:yps] = opt_tup.lam*stab_obj.dynPar[x_int][:yps]
			end
		end
		# reset prx parameter if it becomes too large
		if stab_obj.dynPar[x_int][:prx] > 1e6
			stab_obj.dynPar[x_int][:prx] = opt_tup.start
		end

	elseif stab_obj.method[x_int] == :box && itr_obj.gap < 0.9
		if abs(1 - itr_obj.res[:estTotCostNoStab] / itr_obj.res[:estTotCost]) <  opt_tup.thr
			stab_obj.dynPar[stab_obj.actMet] = stab_obj.dynPar[stab_obj.actMet] / opt_tup.fac
			stab_obj.lastSw = itr_obj.cnt.i
			produceMessage(report_ntup.mod.options, report_ntup.mod.report, 1, " - Reduced boxstep trust-region!", testErr = false, printErr = false)
		elseif checkSwitch(stab_obj, itr_obj.cnt, report_ntup.itr)
			stab_obj.dynPar[stab_obj.actMet] = stab_obj.dynPar[stab_obj.actMet] * opt_tup.fac
			stab_obj.lastSw = itr_obj.cnt.i
			produceMessage(report_ntup.mod.options, report_ntup.mod.report, 1, " - Extended boxstep trust-region!", testErr = false, printErr = false)
		end
	end

end
 
# filter variables used for stabilization
function filterStabVar(capa_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, stLvl_dic::Dict{Symbol,Dict{Symbol,DataFrame}}, lim_dic::Dict{Symbol,DataFrame}, weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl, :lim), NTuple{4, Float64}}, top_m::anyModel)

	var_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()

	# write capacity values
	var_dic[:capa] = Dict(x => Dict{Symbol,Dict{Symbol,DataFrame}}() for x in [:tech, :exc])

	for sys in (:exc, :tech)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(capa_dic[sys])
			var_dic[:capa][sys][sSym] = Dict{Symbol,Dict{Symbol,DataFrame}}() # create empty dataframe for values
			
			# determine where using expansion rather than capacity is possible and more efficient
			varNum_dic = Dict(x => size(unique(getfield.(part_dic[sSym].var[x][!,:var], :terms)), 1) for x in collect(keys(part_dic[sSym].var))) # number of unique variables
			trstVar_arr = map(filter(x -> occursin("capa", string(x)), collect(keys(part_dic[sSym].var)))) do x
				expVar_sym = Symbol(replace(string(x), "capa" => "exp"))
				return !(x in (:capaStOut, :capaStSize, :capaStIn)) && part_dic[sSym].decomm == :none && expVar_sym in keys(varNum_dic) && varNum_dic[expVar_sym] <= varNum_dic[x] ? expVar_sym : x
			end

			# filter capacities with weight of zero
			if weight_ntup.capa == 0.0 filter!(x -> (x in (:expStSize, :capaStSize)), trstVar_arr) end
			if weight_ntup.capaStSize == 0.0 filter!(x -> !(x in (:expStSize, :capaStSize)), trstVar_arr) end

			for trstSym in intersect(keys(capa_dic[sys][sSym]), trstVar_arr)
				var_df = capa_dic[sys][sSym][trstSym]
				if trstSym == :capaExc && !part_dic[sSym].dir filter!(x -> x.R_from < x.R_to, var_df) end # only get relevant capacity variables of exchange
				if sys == :tech var_df = removeFixStorage(trstSym, var_df, part_dic[sSym]) end # remove storage variables controlled by ratio
				# filter cases where actual variables are defined
				var_dic[:capa][sys][sSym][trstSym] = intCol(var_df) |> (w -> innerjoin(var_df, unique(select(filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym]), w)), on = w))
				# remove if no capacities remain
				removeEmptyDic!(var_dic[:capa][sys][sSym], trstSym)
			end
			
			# remove entire system if no capacities
			removeEmptyDic!(var_dic[:capa][sys], sSym)
		end
	end

	# write storage values
	var_dic[:stLvl] = Dict{Symbol,Dict{Symbol,DataFrame}}()

	if !isempty(stLvl_dic) && weight_ntup.stLvl != 0.0
		for sSym in keys(stLvl_dic)
			if sSym in keys(top_m.parts.tech) 
				part_obj = top_m.parts.tech[sSym]
				var_dic[:stLvl][sSym] = Dict{Symbol,DataFrame}()
				for stType in intersect(keys(part_obj.var), keys(stLvl_dic[sSym]))
					var_df = stLvl_dic[sSym][stType]

					var_dic[:stLvl][sSym][stType] = intCol(var_df) |> (w -> innerjoin(var_df, unique(select(filter(x -> !isempty(x.var.terms), part_obj.var[stType]), w)), on = w))
				end
				removeEmptyDic!(var_dic[:stLvl], sSym)
			end
		end
	end

	# write limit values
	var_dic[:lim] = Dict{Symbol,DataFrame}()

	if !isempty(lim_dic) && weight_ntup.lim != 0.0
		for limSym in keys(lim_dic)
			var_dic[:lim][limSym] = intCol(lim_dic[limSym]) |> (w -> innerjoin(lim_dic[limSym], unique(select(filter(x -> !isempty(x.var.terms), top_m.parts.lim.var[limSym]), w)), on = w))
		end
	end

	return var_dic
end

# solves top problem without trust region and obtains lower limits
function runTopWithoutStab!(benders_obj::bendersObj)

	# solve problem
	@suppress begin
		set_optimizer_attribute(benders_obj.topNoStab.opt, "Method", benders_obj.algOpt.top.noStabMeth)
		# solve only to optimality for fully accurate lower bound when close to optimum
		if benders_obj.stab.crossNoStab
			set_optimizer_attribute(benders_obj.topNoStab.opt, "Crossover", 1)
			set_optimizer_attribute(benders_obj.topNoStab.opt, "FeasibilityTol", 1e-6)
		else
			noStabTol_fl = interItrPar(benders_obj.itr.gap, benders_obj.algOpt.gap, benders_obj.algOpt.top.noStabTol[2], benders_obj.algOpt.top.noStabTol[1])
			set_optimizer_attribute(benders_obj.topNoStab.opt, "Crossover", 0)
			set_optimizer_attribute(benders_obj.topNoStab.opt, "FeasibilityTol", noStabTol_fl)
		end
	end
	numFoc_arr = [0, 2, 3]
	numFoc_int = solveModel!(benders_obj.top, benders_obj.topNoStab.opt, numFoc_arr, benders_obj.algOpt.top.check, false, benders_obj.topNoStab)

	if numFoc_int != numFoc_arr[1]
		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Top problem without stabilization solved by increasing numeric focus to $(numFoc_int)" , testErr = false, printErr = false)
	end

	# obtain different objective values
	benders_obj.itr.res[:topCostNoStab] = value(sum(convertAffExpr.(filter(x -> x.name == :cost, benders_obj.top.parts.obj.var[:objVar])[!,:var], benders_obj.topNoStab.ref))) # costs of unconstrained top-problem
	benders_obj.itr.res[:estTotCostNoStab] = benders_obj.itr.res[:topCostNoStab] + value(convertAffExpr(filter(x -> x.name == :benders, benders_obj.top.parts.obj.var[:objVar])[1,:var], benders_obj.topNoStab.ref)) # objective (incl. benders) of unconstrained top-problem
	benders_obj.itr.res[:lowLimCost] = benders_obj.itr.res[:estTotCostNoStab]
	
	if benders_obj.nearOpt.cnt != 0 benders_obj.itr.res[:nearObjNoStab] = objective_value(benders_obj.topNoStab.opt) end
	
end

# check if switching criterium is met
function checkSwitch(stab_obj::stabObj, cnt_obj::countItr, itr_df::DataFrame)
	# check if criterium is met
	min_boo = stab_obj.lastSw + stab_obj.ruleSw.itr < cnt_obj.i
	pro_boo = itr_df[(cnt_obj.i - min(cnt_obj.i, stab_obj.ruleSw.itrAvg) + 1):end, :gap] |> (x -> (x[1] / x[end])^(1 / (length(x) -1)) - 1 < stab_obj.ruleSw.avgImp) # check if progress in last iterations is below threshold
	# save info on last switch
	return min_boo && pro_boo
end

# remove stabilization from problem
function removeStab!(benders_obj::bendersObj)
	stab_obj = benders_obj.stab
	if stab_obj.method[stab_obj.actMet] == :qtr && is_valid(benders_obj.top.optModel, stab_obj.cns)
		delete(benders_obj.top.optModel, stab_obj.cns) # remove trust-region
	elseif stab_obj.method[stab_obj.actMet] in (:prx1, :prx2)
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1]) # remove penalty form objective
	elseif stab_obj.method[stab_obj.actMet] in (:lvl1, :lvl2, :lvl3) && has_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1])
		delete_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
	elseif stab_obj.method[stab_obj.actMet] == :qtrLvl
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1])
		# remove level bound
		if has_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
			delete_upper_bound(benders_obj.top.parts.obj.var[:obj][1, 1])
		end
		# remove trust-region
		if is_valid(benders_obj.top.optModel, stab_obj.cns) 
			delete(benders_obj.top.optModel, stab_obj.cns) 
		end
	elseif stab_obj.method[stab_obj.actMet] == :dsb
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1, 1])
		delete(benders_obj.top.optModel, stab_obj.cns)
		delete(benders_obj.top.optModel, stab_obj.helper_var)
		unregister(benders_obj.top.optModel, :r)
	elseif stab_obj.method[stab_obj.actMet] == :box
		stabVar_dic = matchValWithVar(stab_obj.var, stab_obj.weight, benders_obj.top)
	
		# delete limits on capacity
		for sys in keys(stabVar_dic[:capa]), sSym in keys(stabVar_dic[:capa][sys]), capaSym in keys(stabVar_dic[:capa][sys][sSym])
			rmvLim_arr = map(x -> collect(x.terms)[1][1], stabVar_dic[:capa][sys][sSym][capaSym][!,:var])
			delete_lower_bound.(rmvLim_arr)
			set_lower_bound.(rmvLim_arr, 0.0)
			delete_upper_bound.(rmvLim_arr)
		end
		
		# delete limits on storage level
		for sSym in keys(stabVar_dic[:stLvl]), stType in keys(stabVar_dic[:stLvl][sSym])
			rmvLim_arr = map(x -> collect(x.terms)[1][1], stabVar_dic[:stLvl][sSym][stType][!,:var])
			delete_lower_bound.(rmvLim_arr)
			if stType != :stLvlInter set_lower_bound.(rmvLim_arr, 0.0) end
			delete_upper_bound.(rmvLim_arr)
		end
		
		# delete limits on complicating limits
		for limSym in keys(stabVar_dic[:lim])
			rmvLim_arr = map(x -> collect(x.terms)[1][1], stabVar_dic[:lim][limSym][!,:var])
			delete_lower_bound.(rmvLim_arr)
			delete_upper_bound.(rmvLim_arr)
		end
	
	end
end

#endregion

#region # * near-optimal

# ! adapt top-problem for the computation of near-optimal solutions
function adaptNearOpt!(top_m::anyModel, nearOptSetup_obj::nearOptSetup, costOpt_fl::Float64, nOpt_int::Int)
	
	obj_arr = Pair[]
	for obj in nearOptSetup_obj.obj[nOpt_int][2][2]
		# build filter function
		flt_tup = obj[2]
		te_boo = !(flt_tup.variable in (:capaExc, :expExc))
		exp_boo = flt_tup.variable in (:expConv, :expStIn, :expStOut, :expStSize, :expExc)
		flt_func = x -> (:system in keys(flt_tup) ? ((te_boo ? x.Te : x.Exc) in getDescFromName(flt_tup.system, top_m.sets[(te_boo ? :Te : :Exc)])) : true) && (:region in keys(flt_tup) ? (x.R_exp in getDescFromName(flt_tup.region, top_m.sets[:R])) : true) && (:region_from in keys(flt_tup) ? (x.R_from in getDescFromName(flt_tup.region_from, top_m.sets[:R])) : true) && (:region_to in keys(flt_tup) ? (x.R_to in getDescFromName(flt_tup.region_to, top_m.sets[:R])) : true) && (:timestep in keys(flt_tup) ? ((exp_boo ? x.Ts_exp : x.Ts_expSup) in getDescFromName(flt_tup.timestep, top_m.sets[:Ts])) : true)
		# write description of objective
		push!(obj_arr, (flt_tup.variable => (fac = obj[1], flt = flt_func)))
	end
	# change objective according to near-optimal
	objFunc_tup = tuple(vcat([:cost => (fac = 0.0, flt = x -> true)], obj_arr)...)
	@suppress setObjective!(objFunc_tup, top_m, nearOptSetup_obj.obj[nOpt_int][2][1] == :min)
	
	# delete old restriction to near optimum
	if :nearOpt in keys(top_m.parts.obj.cns) delete(top_m.optModel, top_m.parts.obj.cns[:nearOpt][1,:cns]) end

	# restrict system costs to near-optimum
	cost_expr = sum(filter(x -> x.name in (:cost, :benders), top_m.parts.obj.var[:objVar])[!,:var])
	nearOpt_eqn = @constraint(top_m.optModel, costOpt_fl * (1 + nearOptSetup_obj.optThres)  >= cost_expr)
	top_m.parts.obj.cns[:nearOpt] = DataFrame(cns = nearOpt_eqn)
end

# ! filter pareto efficient near-optimal solutions
function filterParetoEff!(nearOpt_df::DataFrame, nearOptSetup_obj::nearOptSetup)

	filter!(x -> x.value > nearOptSetup_obj.parThres.zero || x.variable in (:lss, :cost, :thrs), nearOpt_df)
	allDom_arr = Array{Array{Int,1},1}()

	# loop over different objectives
	for obj in nearOptSetup_obj.obj
		
		# get cost and lss variables
		parRel_df = filter(x -> x.variable in (:cost, :lss), nearOpt_df)
		
		# get variables for specific objective and correct for direction
		min_boo = obj[2][1] == :min
		for fltObj in obj[2][2]
			# skip case, if weight is zero
			if fltObj[1] == 0.0 continue end
			# create array of functions to filter relevant rows and apply it
			relFlt_tup = fltObj[2]
			fltFunc_arr = map(z -> (y -> occursin(string(getfield(relFlt_tup, z)), string(y[z]))), collect(keys(relFlt_tup)))
			relNear_df = filter(x -> all(map(y -> y(x), fltFunc_arr)), nearOpt_df)
			# apply factor and orientation of optimization
			relNear_df[!,:value] = relNear_df[!,:value] .* (fltObj[1] > 0.0 ? 1.0 : -1.0) .* (min_boo ? 1.0 : -1.0)
			# add to overall data-DataFrame
			append!(parRel_df, relNear_df)
		end

		# check for dominated iterations
		i_arr = unique(parRel_df[!,:i])
		iDom_arr = Int[] # array of dominated iterations
		
		for i1 in i_arr
			# get rows relating to first iteration of check
			i1ParRel_df = select(rename(filter(x -> x.i == i1, parRel_df), :value => :value_1), Not([:i]))
			# loop over rows for second iteration of check
			for i2 in filter(x -> x > i1, i_arr)
				# get data and join to single dataframe
				i2ParRel_df = select(rename(filter(x -> x.i == i2, parRel_df), :value => :value_2), Not([:i]))
				joinParRel_df = joinMissing(i1ParRel_df, i2ParRel_df, [:timestep, :region, :system, :id, :variable], :left, Dict(:value_1 => 0.0, :value_2 => 0.0))
				
				# check for domination
				if all(joinParRel_df[!,:value_1] .<= joinParRel_df[!,:value_2] .* (1 + nearOptSetup_obj.parThres.dom)) # first dominating second
					push!(iDom_arr, i2)
				elseif all(joinParRel_df[!,:value_1] .* (1 + nearOptSetup_obj.parThres.dom) .>= joinParRel_df[!,:value_2]) # second dominating first
					push!(iDom_arr, i1)
					break
				end
			end
		end

		# filter unique dominated entries
		push!(allDom_arr, unique(sort(iDom_arr)))
	end

	# filter dominated cases across all objectives
	interDom_arr = intersect(allDom_arr...)
	filter!(x -> !(x.i in interDom_arr), nearOpt_df)
end

#endregion

#region # * other refinements

# ! track and delete cuts that were not binding for a certain number of iterations
function trackCuts(benders_obj::bendersObj)
	
	delCut_ntup = benders_obj.nearOpt.cnt == 0 ? benders_obj.algOpt.delCut : benders_obj.nearOpt.setup.delCut

	# add current slack to data
	# foreach(x ->  push!(benders_obj.cuts.slack[x[1]], abs(value(x[2][2][1]) / x[2][2][1].constant - 1)), enumerate(benders_obj.cuts.all))
	absGap_fl = benders_obj.itr.res[:curBest] - benders_obj.itr.res[:lowLimCost]
	foreach(x ->  push!(benders_obj.cuts.slack[x[1]], -value(x[2][2][1]) / absGap_fl), enumerate(benders_obj.cuts.all))

	# determine cuts that should be active
	rng_int = delCut_ntup.cnt
	thrs_fl = delCut_ntup.thres
	
	benders_obj.cuts.active = findall(map(x -> length(x) < rng_int || any(x[end-rng_int+1:end] .< thrs_fl), benders_obj.cuts.slack))

end

function deleteCuts!(benders_obj::bendersObj)
	
	top_m = benders_obj.top

	# filter cuts to be deleted
	allAct_arr = map(x -> benders_obj.cuts.all[x][1], benders_obj.cuts.active)
	delete.(top_m.optModel, filter(x -> !((x.i, x.Ts_dis, x.scr) in allAct_arr), top_m.parts.obj.cns[:bendersCuts])[!,:cns])
	filter!(x -> (x.i, x.Ts_dis, x.scr) in allAct_arr, top_m.parts.obj.cns[:bendersCuts])

end

# ! interpolate iteration parameter based on current gap (used for convergence tolerance of subproblems or radius in qtrLvl stabilization)
function interItrPar(gapCur_fl::Float64, gapEnd_fl::Float64, rng_arr::Union{Array{Float64, 1}, Array{Int, 1}}, int_sym::Union{Symbol,String}, cons_fl::Float64 = 0.0)

	int_sym = typeof(int_sym) == Symbol ? int_sym : Symbol(int_sym)

	if gapCur_fl < gapEnd_fl
		return rng_arr[2] - cons_fl
	else
		if int_sym == :lin
			m = (rng_arr[1] -rng_arr[2])/(1-gapEnd_fl)
			b =rng_arr[1] - m
			return b + m * gapCur_fl - cons_fl
		elseif int_sym == :exp
			m = log(rng_arr[1]/rng_arr[2])/(1-gapEnd_fl)
			b = log(rng_arr[1]) - m
			return exp(b + m * gapCur_fl) - cons_fl
		elseif int_sym == :log
			b = rng_arr[1]
			m = (rng_arr[2] - b ) / log(gapEnd_fl)
			return b + m * log(gapCur_fl) - cons_fl
		elseif int_sym == :none
			return rng_arr[2] - cons_fl
		end
	end
end

#endregion

#region # * manage linear trust region (same concept as box-step method, but here not implemented as a stabilization method for benders)

# ! adds limits specified by dictionary to problem
function addLinearTrust!(top_m::anyModel, lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, rngVio_fl::Float64)
	for sys in (:tech, :exc)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(lim_dic[sys])
			for trstSym in intersect(keys(lim_dic[sys][sSym]), keys(part_dic[sSym].var))
				# group limiting constraints
				grpBothCapa_arr = collect(groupby(lim_dic[sys][sSym][trstSym], :limCns))
				# get variables of top model
				trstVar_df = filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym])
				foreach(lim -> limitVar!(select(rename(lim, :limVal => :value), Not([:limCns])), trstVar_df, trstSym, part_dic[sSym], top_m, rngVio_fl, lim[1,:limCns]), grpBothCapa_arr)
			end
		end
	end
end

# ! check for binding limits
function checkLinearTrust(top_m::anyModel)
	binLim_boo = false
	# loop over limits to detect binding ones
	for sys in (:tech, :exc)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(part_dic)
			for limCns in filter(x -> any(occursin.(["BendersUp", "BendersLow"], string(x))), keys(part_dic[sSym].cns))
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
	for sys in (:tech, :exc)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(part_dic)
			for limCns in filter(x -> any(occursin.(["BendersUp", "BendersLow"], string(x))), keys(part_dic[sSym].cns))
				delete.(top_m.optModel, part_dic[sSym].cns[limCns][!,:cns])
			end
		end
	end
end

#endregion