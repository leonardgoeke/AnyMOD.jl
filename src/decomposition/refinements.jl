
#region # * stabilization

# initialize stabilization when creating benders object, returns the stabilization object
function initializeStab!(benders_obj::bendersObj, stabSetup_obj::stabSetup, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, info_ntup::NamedTuple{(:name, :frs, :supTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64}}, scale_dic::Dict{Symbol,NamedTuple}, runSubDist::Function)

	report_m = benders_obj.report.mod

	
	if !isempty(stabSetup_obj.method)

		cutData_dic = Dict{Tuple{Int64,Int64},resData}()
		time_dic = Dict{Tuple{Int64,Int64},Millisecond}()
	
		#region # * compute heuristic solution
	
		heuOpt_ntup = (inputDir = inputFolder_ntup.heu, resultDir = inputFolder_ntup.results, suffix = info_ntup.name, supTsLvl = info_ntup.supTsLvl, lvlFrs = info_ntup.frs, shortExp = info_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facHeu])
		
		# ! get starting solution with heuristic solve or generic
		if stabSetup_obj.ini.setup != :none
			produceMessage(report_m.options,report_m.report, 1," - Started heuristic pre-solve for starting solution", testErr = false, printErr = false)
			heu_m, startSol_obj =  @suppress heuristicSolve(heuOpt_ntup, 1.0, benders_obj.algOpt.threads, benders_obj.algOpt.opt, rtrnMod = true, solDet = stabSetup_obj.ini.det, fltSt = true);
			lowBd_fl = 0.0
		else
			@suppress optimize!(benders_obj.top.optModel)
			startSol_obj = resData()
			startSol_obj.objVal = value(benders_obj.top.parts.obj.var[:objVar][1,:var])
			startSol_obj.capa, startSol_obj.stLvl = writeResult(benders_obj.top,[:capa,:exp,:mustCapa,:mustExp,:stLvl])
			lowBd_fl = startSol_obj.objVal
		end
	
		#endregion
	
		#region # * evaluate heuristic solution
	
		# first result for first iteration
		firstItr_df = DataFrame(i = 0, lowCost = 0, bestObj = Inf, gap = 1.0, curCost = Inf, time_ges = Dates.value(floor(now() - report_m.options.startTime,Dates.Second(1)))/60, time_top = 0, time_sub = 0)
		if !isnothing(benders_obj.nearOpt) firstItr_df[!,:objective] .= "costs" end
		if !isempty(stabSetup_obj.method) 
			firstItr_df[!,:actMethod] .= Symbol()
			foreach(x -> firstItr_df[!,Symbol("dynPar_",x)] .= 0.0, stabSetup_obj.method)
		end
		append!(benders_obj.report.itr, firstItr_df)


		# create dictionaries to store results
		if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
		time_dic = Dict{Tuple{Int64,Int64},Millisecond}()
		
		# solve sub-problems
		for (id,s) in enumerate(collect(keys(benders_obj.sub)))
			if benders_obj.algOpt.dist # distributed case
				futData_dic[s] = runSubDist(id + 1, copy(startSol_obj), :barrier, 1e-8)
			else # non-distributed case
				time_dic[s], cutData_dic[s] = runSub(benders_obj.sub[s], copy(startSol_obj), :barrier, 1e-8)
			end
		end
		
		# get solutions
		if benders_obj.algOpt.dist
			wait.(collect(values(futData_dic)))
			for s in collect(keys(benders_obj.sub))
				time_dic[s], cutData_dic[s] = fetch(futData_dic[s])
			end
		end
	
		# store information for cuts
		benders_obj.cuts = collect(cutData_dic)

		# analyse results
		startSol_obj.objVal = startSol_obj.objVal + sum(map(x -> x.objVal, values(cutData_dic)))
		timeSub_fl = Dates.toms(benders_obj.algOpt.dist ? maximum(collect(values(time_dic))) : sum(collect(values(time_dic)))) / Dates.toms(Second(1))
		
		# write results for second iteration
		secItr_df = DataFrame(i = 1, lowCost = lowBd_fl, bestObj = startSol_obj.objVal, gap = 1 - lowBd_fl/startSol_obj.objVal, curCost = startSol_obj.objVal, time_ges = Dates.value(floor(now() - report_m.options.startTime,Dates.Second(1)))/60, time_top = 0, time_sub = timeSub_fl)
		if !isnothing(benders_obj.nearOpt) secItr_df[!,:objective] .= "costs" end
		if !isempty(stabSetup_obj.method) 
			secItr_df[!,:actMethod] .= Symbol()
			foreach(x -> secItr_df[!,Symbol("dynPar_",x)] .= 0.0, stabSetup_obj.method)
		end
		append!(benders_obj.report.itr, secItr_df)

		#endregion
	
		#region # * initialize stabilization 

		stab_obj, eleNum_int = stabObj(stabSetup_obj.method, stabSetup_obj.switch, stabSetup_obj.weight, startSol_obj, lowBd_fl, benders_obj.top);
		centerStab!(stab_obj.method[stab_obj.actMet], stab_obj, benders_obj.algOpt.solOpt.addVio, benders_obj.top, report_m);

		#endregion
		
		produceMessage(report_m.options,report_m.report, 1," - Initialized stabilization with $eleNum_int variables", testErr = false, printErr = false)
	else
		stab_obj = nothing
		startSol_obj = resData()
	end

	return stab_obj, startSol_obj
	
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
		elseif key == :lvl1 && !isempty(setdiff(keys(val), (:lam,)))
			error("options provided for level bundle do not match the defined option 'lam'")
		elseif key == :lvl2 && !isempty(setdiff(keys(val), (:lam, :myMax)))
			error("options provided for level bundle do not match the defined options 'lam', 'myMax'")
		elseif key == :box && !isempty(setdiff(keys(val), (:low, :up, :minUp)))
			error("options provided for trust-region do not match the defined options 'low', 'up', and 'minUp'")
		elseif key == :dsb && !isempty(setdiff(keys(val), (:start, :min, :lam, :myMax)))
			error("options provided for doubly stabilised bundle do not match the defined options 'start', 'min', 'lam', 'myMax'")
		end
	end

	if length(meth_arr) != length(unique(meth_arr)) error("stabilization methods must be unique") end

	# method specific adjustments (e.g. starting value for dynamic parameter, new variables for objective function)
	dynPar_arr = []
	for m in 1:size(meth_arr, 1)
		if meth_arr[m] in (:prx1, :prx2)
			dynPar = Dict(:prx => methOpt_arr[m].start, :prxAux => methOpt_arr[m].start) # starting value for penalty
		elseif meth_arr[m] == :lvl1
			dynPar = (methOpt_arr[m].lam * lowBd_fl  + (1 - methOpt_arr[m].lam) * upBd_fl) / top_m.options.scaFac.obj # starting value for level
			if methOpt_arr[m].lam >= 1 || methOpt_arr[m].lam <= 0 
				error("lambda for level bundle must be strictly between 0 and 1")
			end
		elseif meth_arr[m] == :lvl2
			dynPar = Dict(:yps => (1-methOpt_arr[m].lam)*(upBd_fl-lowBd_fl)/ top_m.options.scaFac.obj, :my => 0.0)
			if methOpt_arr[m].lam >= 1 || methOpt_arr[m].lam <= 0 
				error("lambda for level bundle must be strictly between 0 and 1")
			end
		elseif meth_arr[m] == :qtr
			dynPar = methOpt_arr[m].start # starting value for radius
		elseif meth_arr[m] == :box
			dynPar = 0.0 # dummy value since boxstep implementation does not have a dynamic parameter
		elseif meth_arr[m] == :dsb
			dynPar = Dict(:yps=>(1-methOpt_arr[m].lam)*(upBd_fl-lowBd_fl)/top_m.options.scaFac.obj,
			:prx => methOpt_arr[m].start, :my => 1.0)

		else
			error("unknown stabilization method provided, method must either be 'prx', 'lvl', 'qtr', or 'box'")
		end
		push!(dynPar_arr, dynPar)
	end
	
	return meth_arr, methOpt_arr, dynPar_arr
end

# function to update the center of stabilization method
centerStab!(method::Symbol, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel) = centerStab!(Val{method}(), stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)

# function for quadratic trust region
function centerStab!(method::Val{:qtr}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)

	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (2*maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*x.value*x.scaFac < minFac_fl ? 0.0 : x.value, eachrow(allVar_df))

	# absolute value for rhs of equation
	abs_fl = sum(allVar_df[!,:value] .* sqrt.(allVar_df[!,:scaFac]) ) |> (x -> x < 0.01 * size(allVar_df, 1) ? sum(allVar_df[!,:scaFac]) : x)
	
	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ abs((abs_fl * stab_obj.dynPar[stab_obj.actMet])^2 - sum(allVar_df[!,:scaFac] .* allVar_df[!,:value].^2))

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaEq_fl = computeL2Norm(allVar_df, stab_obj, scaRng_tup, top_m)

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
		produceMessage(report_m.options, report_m.report, 1, " - Adjusted radius of stabilization to prevent numerical problems", testErr = false, printErr = false)
	end
end

# function for proximal bundle method
function centerStab!(method::Union{Val{:prx1},Val{:prx2}}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)
	
	# get penalty factor
	pen_fl = 1/(2 * stab_obj.dynPar[stab_obj.actMet][:prx])

	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (2 * maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac])) / (top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*x.value*x.scaFac < minFac_fl ? 0.0 : x.value, eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ (sum(allVar_df[!,:value] .^ 2) |> (x -> x == 0.0 ? 1.0 : x))

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df, stab_obj, scaRng_tup, top_m)

	# current range of factors and value of constant
	fac_arr = abs.(vcat(collect(values(capaSum_expr.aff.terms)), collect(values(capaSum_expr.terms)))) |> (x -> scaFac_fl .* (minimum(x), maximum(x)))	
	const_fl = capaSum_expr.aff.constant * scaFac_fl |> (x -> x == 0.0 ? top_m.options.coefRng.rhs[1] : x)
	
	# maximum and minimum value for penalty
	maxPen_fl =  min(top_m.options.coefRng.mat[2]/fac_arr[2], top_m.options.coefRng.rhs[2]/const_fl) * addVio_fl	
	minPen_fl =  max(top_m.options.coefRng.mat[1]/fac_arr[1], top_m.options.coefRng.rhs[1]/const_fl) / addVio_fl
	
	# adjust objective function
	if pen_fl < maxPen_fl && pen_fl > minPen_fl
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] +  pen_fl * capaSum_expr  * scaFac_fl)
	else
		if pen_fl > maxPen_fl
			@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] +  maxPen_fl * capaSum_expr  * scaFac_fl)
		else
			@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] +  minPen_fl * capaSum_expr  * scaFac_fl)
		end
		produceMessage(report_m.options, report_m.report, 1, " - Adjusted proximal parameter to prevent numerical problems", testErr = false, printErr = false)
	
	end
end

# functions for level bundle methods
function centerStab!(method::Val{:lvl1}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (2 * maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2 * x.value < minFac_fl ? 0.0 : x.value, eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df, stab_obj, scaRng_tup, top_m)

	# adjust objective function and level set
	@objective(top_m.optModel, Min, 0.5 * capaSum_expr  * scaFac_fl)
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], stab_obj.dynPar[stab_obj.actMet])
end

function centerStab!(method::Val{:lvl2}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (2 * maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2 * x.value < minFac_fl ? 0.0 : x.value, eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df, stab_obj, scaRng_tup, top_m)

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps] 

	# adjust objective function and level set
	@objective(top_m.optModel, Min, 0.5 * capaSum_expr  * scaFac_fl)
	set_upper_bound(top_m.parts.obj.var[:obj][1, 1], ell_fl)
end

# function for box step method
function centerStab!(method::Val{:box}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var, stab_obj.weight, top_m)
	allCapa_df = vcat(vcat(vcat(map(x -> expExpr_dic[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var, :value, :scaFac]], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)
	allStLvl_df = vcat(map(x -> expExpr_dic[:stLvl][x], collect(keys(expExpr_dic[:stLvl])))...) |> (z -> isempty(z) ? DataFrame(var = AffExpr[], value = Float64[], scaFac = Float64[] ) : z)
	allVar_df = filter(x -> x.scaFac != 0.0, vcat(allCapa_df, allStLvl_df))

	# set lower and upper bound
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_lower_bound(z[1], x.value*(1-stab_obj.methodOpt[stab_obj.actMet].low) |> (y -> y < top_m.options.coefRng.rhs[1]/1e2 ? 0.0 : y))), eachrow(allVar_df))
	foreach(x -> collect(x.var.terms)[1] |> (z -> set_upper_bound(z[1], max(stab_obj.methodOpt[stab_obj.actMet].minUp/z[2], x.value*(1+stab_obj.methodOpt[stab_obj.actMet].up)))), eachrow(allVar_df))

end

# function for doubly stabilised bundle method
function centerStab!(method::Val{:dsb}, stab_obj::stabObj, addVio_fl::Float64, top_m::anyModel, report_m::anyModel)
	
	# match values with variables in model
	allVar_df = getStabDf(stab_obj, top_m)

	# sets values of variables that will violate range to zero
	minFac_fl = (maximum(allVar_df[!,:value] .* allVar_df[!,:scaFac]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> x.value < minFac_fl ? 0.0 : x.value, eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df, stab_obj, scaRng_tup, top_m)

	# compute level set constraint
	ell_fl = stab_obj.objVal/ top_m.options.scaFac.obj - stab_obj.dynPar[stab_obj.actMet][:yps]

	# compute penalty multiplier
	pen_fl = stab_obj.dynPar[stab_obj.actMet][:prx]

	# adjust objective function and level set
	stab_obj.helper_var = @variable(top_m.optModel, r)
	@objective(top_m.optModel, Min, r + (1/2*pen_fl) * capaSum_expr  * scaFac_fl)
	stab_obj.cns = @constraint(top_m.optModel, top_m.parts.obj.var[:obj][1, 1] <= r)
	set_upper_bound(stab_obj.helper_var, ell_fl)
end

# compute scaled l2 norm
function computeL2Norm(allVar_df::DataFrame, stab_obj::stabObj, scaRng_tup::Tuple, top_m::anyModel)

	# set values of variable to zero or biggest value possible without scaling violating rhs range
	for x in eachrow(allVar_df)	
		if top_m.options.coefRng.mat[1]/(x.value*x.scaFac*2) > scaRng_tup[2] # factor requires more up-scaling than possible
			x[:value] = 0 # set value to zero
		elseif top_m.options.coefRng.mat[2]/(x.value*x.scaFac*2) < scaRng_tup[1] # factor requires more down-scaling than possible
			x[:value] = top_m.options.coefRng.mat[2]/(scaRng_tup[1]*2) # set to biggest value possible within range
		end
	end

	# computes left hand side expression and scaling factor
	capaSum_expr = sum(map(x -> sum(collect(keys(x.var.terms))) |> (z -> x.scaFac * (z^2 - 2*x.value*z + x.value^2)), eachrow(allVar_df)))
	scaEq_fl = top_m.options.coefRng.mat[1]/minimum(abs.(vcat(collect(values(capaSum_expr.terms)), collect(values(capaSum_expr.aff.terms))) |> (z -> isempty(z) ? [1.0] : z)))

	return capaSum_expr, scaEq_fl
end

# write function to compute poorman's Hessian auxilary scalar for prx_2
function computePrx2Aux(cuts_arr::Array{Pair{Tuple{Int,Int},Union{resData}},1}, prevCuts_arr::Array{Pair{Tuple{Int,Int},Union{resData}},1})

	diffVal_arr = Float64[]
	diffDual_arr = Float64[]
	
	for cut in prevCuts_arr # loop over cut data
		
		scr = cut[1]
		relCut_obj = filter(x -> x[1] == scr, cuts_arr)[1][2]

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
function adjustDynPar!(stab_obj::stabObj, top_m::anyModel, iUpd_int::Int, adjCtr_boo::Bool, adjCtr_count::Int, cntNull_int::Int, levelDual_fl::Float64, prx2Aux_fl, estCostNoStab_fl::Float64, estCost_fl::Float64, currentBest_fl::Float64, currentCost_fl::Float64, nearOpt_boo::Bool, report_m::anyModel)

	opt_tup = stab_obj.methodOpt[iUpd_int]
	if stab_obj.method[iUpd_int] == :qtr # adjust radius of quadratic trust-region
		if nearOpt_boo ? !adjCtr_boo : abs(1 - estCostNoStab_fl / estCost_fl) < opt_tup.thr && stab_obj.dynPar[iUpd_int] > opt_tup.low
			stab_obj.dynPar[iUpd_int] = max(opt_tup.low, stab_obj.dynPar[iUpd_int] / opt_tup.fac)
			produceMessage(report_m.options, report_m.report, 1, " - Reduced quadratic trust-region!", testErr = false, printErr = false)	
		end
	elseif stab_obj.method[iUpd_int] in (:prx1, :prx2) # adjust penalty term of proximal term, implementation according to doi.org/10.1007/s10107-015-0873-6, section 5.1.2
		# compute τ_aux
		aux_fl = stab_obj.method[iUpd_int] == :prx1 ? (stab_obj.objVal - currentCost_fl)/(stab_obj.objVal - estCost_fl) : prx2Aux_fl 
		#aux_fl = opt_tup.meth== "PBM-1" ? (stab_obj.objVal - currentCost_fl)/(stab_obj.objVal - estCost_fl) : 0
		# We introduce a safeguard ensuring that :prx is only updated if the numerator of the aux term is positive (see https://doi.org/10.1007/978-3-030-34910-3 Chapter 3 for a discussion)
		stab_obj.dynPar[iUpd_int][:prxAux] = stab_obj.method[iUpd_int] == :prx1 ? 2 * stab_obj.dynPar[iUpd_int][:prx] * (1+aux_fl) : stab_obj.dynPar[iUpd_int][:prx]*(1+max(aux_fl/1e3, 0))
		# check if serious step
		if adjCtr_boo
			# adjust τ_aux, if last 5 steps have been serious
			if adjCtr_count > 5
				stab_obj.dynPar[iUpd_int][:prxAux] = opt_tup.a * stab_obj.dynPar[iUpd_int][:prxAux]
			end
			# update proximal term
			stab_obj.dynPar[iUpd_int][:prx] = min(stab_obj.dynPar[iUpd_int][:prxAux], 10 * stab_obj.dynPar[iUpd_int][:prx])
		else # if null-step
			if cntNull_int > 10
				stab_obj.dynPar[iUpd_int][:prx] = (opt_tup.a) * stab_obj.dynPar[iUpd_int][:prx]
			end
			stab_obj.dynPar[iUpd_int][:prx] = min(stab_obj.dynPar[iUpd_int][:prx], max(stab_obj.dynPar[iUpd_int][:prxAux], stab_obj.dynPar[iUpd_int][:prx]/opt_tup.a, opt_tup.min))
		end
		# another safeguard preventing the proximal parameter to explode
		if stab_obj.method[iUpd_int] == :prx2
			if stab_obj.dynPar[iUpd_int][:prx]>1e6
				stab_obj.dynPar[iUpd_int][:prx] = opt_tup.start
			end
		end
	elseif stab_obj.method[iUpd_int] == :lvl1 # adjust level
		stab_obj.dynPar[iUpd_int] = (opt_tup.lam * estCostNoStab_fl  + (1 - opt_tup.lam) * currentBest_fl) / top_m.options.scaFac.obj
	elseif stab_obj.method[iUpd_int] == :lvl2 # adjust level, implementation according to doi.org/10.1007/s10107-015-0873-6 
		stab_obj.dynPar[iUpd_int][:my] = 1-levelDual_fl
		if adjCtr_boo
			stab_obj.dynPar[iUpd_int][:yps] = min(stab_obj.dynPar[iUpd_int][:yps], (1-opt_tup.lam)*(currentBest_fl - estCostNoStab_fl) / top_m.options.scaFac.obj)
		else
			if stab_obj.dynPar[iUpd_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[iUpd_int][:yps] = opt_tup.lam*stab_obj.dynPar[iUpd_int][:yps]
			end
		end
	elseif stab_obj.method[iUpd_int] == :dsb # adjust doubly stabilised method, implementation according to doi.org/10.1007/s10107-015-0873-6
		stab_obj.dynPar[iUpd_int][:my] = min(1-levelDual_fl, opt_tup.myMax+1.0)
		if adjCtr_boo
			stab_obj.dynPar[iUpd_int][:prx] = (stab_obj.dynPar[iUpd_int][:my])*stab_obj.dynPar[iUpd_int][:prx] # added a fixed scaler for the dual variable to avoid extremely large values for prx
			stab_obj.dynPar[iUpd_int][:yps] = min(stab_obj.dynPar[iUpd_int][:yps], (1-opt_tup.lam)*(currentBest_fl- estCostNoStab_fl) / top_m.options.scaFac.obj)
		else
			newPrx_fl = stab_obj.dynPar[iUpd_int][:prx]*(stab_obj.dynPar[iUpd_int][:yps]/((currentBest_fl - estCostNoStab_fl)/top_m.options.scaFac.obj))
			stab_obj.dynPar[iUpd_int][:prx] = max(opt_tup.min, newPrx_fl)
			if stab_obj.dynPar[iUpd_int][:my] > opt_tup.myMax 
				stab_obj.dynPar[iUpd_int][:yps] = opt_tup.lam*stab_obj.dynPar[iUpd_int][:yps]
			end
		end
		# reset prx parameter if it becomes too large
		if stab_obj.dynPar[iUpd_int][:prx] > 1e6
			stab_obj.dynPar[iUpd_int][:prx] = opt_tup.start
		end
	end

end
 
# filter variables used for stabilization
function filterStabVar(capa_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}, stLvl_dic::Dict{Symbol,DataFrame}, weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}}, top_m::anyModel)

	var_dic = Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}}()

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
				return part_dic[sSym].decomm == :none && expVar_sym in keys(varNum_dic) && varNum_dic[expVar_sym] <= varNum_dic[x] ? expVar_sym : x
			end

			# filter capacities with weight of zero
			if weight_ntup.capa == 0.0 filter!(x -> (x in (:expStSize, :capaStSize)), trstVar_arr) end
			if weight_ntup.capaStSize == 0.0 filter!(x -> !(x in (:expStSize, :capaStSize)), trstVar_arr) end

			for trstSym in intersect(keys(capa_dic[sys][sSym]), trstVar_arr)
				var_df = capa_dic[sys][sSym][trstSym]
				if trstSym == :capaExc && !part_dic[sSym].dir filter!(x -> x.R_from < x.R_to, var_df) end # only get relevant capacity variables of exchange
				if sys == :tech var_df = removeFixStorage(trstSym, var_df, part_dic[sSym]) end # remove storage variables controlled by ratio
				# filter cases where actual variables are defined
				var_dic[:capa][sys][sSym][trstSym] = intCol(var_df) |> (w ->innerjoin(var_df, unique(select(filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym]), w)), on = w))
				# remove if no capacities remain
				removeEmptyDic!(var_dic[:capa][sys][sSym], trstSym)
			end
			
			# remove entire system if no capacities
			removeEmptyDic!(var_dic[:capa][sys], sSym)
		end
	end

	# write storage values
	var_dic[:stLvl] = Dict{Symbol,DataFrame}()

	if !isempty(stLvl_dic) && weight_ntup.stLvl != 0.0
		for sSym in keys(stLvl_dic)
			if sSym in keys(top_m.parts.tech)
				part_obj = top_m.parts.tech[sSym]
				var_df = stLvl_dic[sSym]
				var_dic[:stLvl][sSym] = intCol(var_df) |> (w ->innerjoin(var_df, unique(select(filter(x -> !isempty(x.var.terms), part_obj.var[:stLvl]), w)), on = w))
			end
		end
	end

	return var_dic
end

# solves top problem without trust region and obtains lower limits
function runTopWithoutStab(benders_obj::bendersObj)

	stab_obj = benders_obj.stab
	
	# remove stabilization
	if stab_obj.method[stab_obj.actMet] == :qtr
		delete(benders_obj.top.optModel, stab_obj.cns) # remove trust-region
	elseif stab_obj.method[stab_obj.actMet] in (:prx1, :prx2)
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1,1]) # remove penalty form objective
	elseif stab_obj.method[stab_obj.actMet] in (:lvl1, :lvl2)
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1,1])
		delete_upper_bound(benders_obj.top.parts.obj.var[:obj][1,1])
	elseif stab_obj.method[stab_obj.actMet] == :dsb
		@objective(benders_obj.top.optModel, Min, benders_obj.top.parts.obj.var[:obj][1,1])
		delete(benders_obj.top.optModel, stab_obj.cns)
		delete(benders_obj.top.optModel, stab_obj.helper_var)
		unregister(benders_obj.top.optModel, :r)
	elseif stab_obj.method[stab_obj.actMet] == :box
		stabVar_dic = matchValWithVar(stab_obj.var, stab_obj.weight, benders_obj.top)
		for grp in (:capa, :stLvl), sys in keys(stabVar_dic[grp]), sSym in keys(stabVar_dic[grp][sys]), capaSym in keys(stabVar_dic[grp][sys][sSym])
			relVar_arr = map(x -> collect(x.terms)[1][1], stabVar_dic[grp][sys][sSym][capaSym][!,:var])
			delete_lower_bound.(relVar_arr)
			set_lower_bound.(relVar_arr, 0.0)
			delete_upper_bound.(relVar_arr)
		end
	end
	
	# solve problem
	set_optimizer_attribute(benders_obj.top.optModel, "Method", 0)
	set_optimizer_attribute(benders_obj.top.optModel, "NumericFocus", benders_obj.algOpt.solOpt.numFoc)
	optimize!(benders_obj.top.optModel)
	checkIIS(benders_obj.top)

	# obtain different objective values
	topCost_fl = value(sum(filter(x -> x.name == :cost, benders_obj.top.parts.obj.var[:objVar])[!,:var])) # costs of unconstrained top-problem
	estCost_fl = topCost_fl + value(filter(x -> x.name == :benders, benders_obj.top.parts.obj.var[:objVar])[1,:var]) # objective (incl. benders) of unconstrained top-problem

	return topCost_fl, estCost_fl
end

#endregion

#region # * near-optimal

# ! adapt top-problem for the computation of near-optimal solutions
function adaptNearOpt!(top_m::anyModel, nearOpt_ntup::NamedTuple, costOpt_fl::Float64, nOpt_int::Int)
	
	obj_arr = Pair[]
	for obj in nearOpt_ntup.obj[nOpt_int][2][2]
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
	@suppress setObjective!(objFunc_tup, top_m, nearOpt_ntup.obj[nOpt_int][2][1] == :min)
	
	# delete old restriction to near optimum
	if :nearOpt in keys(top_m.parts.obj.cns) delete(top_m.optModel, top_m.parts.obj.cns[:nearOpt][1,:cns]) end
	
	# restrict system costs to near-optimum
	cost_expr = sum(filter(x -> x.name in (:cost, :benders), top_m.parts.obj.var[:objVar])[!,:var])
	nearOpt_eqn = @constraint(top_m.optModel, costOpt_fl * (1 + nearOpt_ntup.optThres)  >= cost_expr)
	top_m.parts.obj.cns[:nearOpt] = DataFrame(cns = nearOpt_eqn)
end

# ! get capacity results for near optimal analysis
function getCapaResult(anyM::anyModel)

	# get capacities from summary file
	sum_df = rename(reportResults(:summary, anyM, rtnOpt = (:csvDf,)), :region_dispatch => :region, :technology => :system)
	sum_df = filter(x -> x.variable in (:capaStOut, :capaStIn, :capaStSize, :capaConv), sum_df)
	select!(sum_df, setdiff(namesSym(sum_df), [:scenario, :carrier, :objName]))

	# get capacity from exchange file
	exc_df = rename(filter(x -> x.variable == :capaExc, reportResults(:exchange, anyM, rtnOpt = (:csvDf,))), :exchange => :system)
	exc_df[!,:region] = string.(exc_df[!,:region_from]) .* " - " .* string.(exc_df[!,:region_to])
	exc_df[!,:id] .= ""
	select!(exc_df, setdiff(namesSym(exc_df), [:timestep_superordinate_expansion, :region_from, :region_to, :scenario, :directed, :carrier, :objName]))

	return rename(vcat(sum_df, exc_df), :timestep_superordinate_dispatch => :timestep, :variable => :capacity_variable, :value => :capacity_value)
end

#endregion

#region # * other refinements

# ! delete cuts that have not been binding for a while
function deleteCuts!(benders_obj::bendersObj)
	
	top_m = benders_obj.top
	# numer of iterations after which unused cuts are delted
	delCut_int = benders_obj.itr.cnt.nOpt == 0 ? benders_obj.algOpt.delCut : benders_obj.nearOpt.setup.delCut
	
	if delCut_int < Inf
		# tracking latest binding iteration for cuts
		top_m.parts.obj.cns[:bendersCuts][!,:actItr] .= map(x -> abs(value(x.cns) / normalized_rhs(x.cns) - 1) < 1e-3 ? i : x.actItr, eachrow(top_m.parts.obj.cns[:bendersCuts]))
		# delete cuts that were not binding long enough
		delete.(top_m.optModel, filter(x -> x.actItr + delCut_int < i, top_m.parts.obj.cns[:bendersCuts])[!,:cns])
		filter!(x -> (x.actItr + delCut_int > benders_obj.itr.cnt.i), top_m.parts.obj.cns[:bendersCuts])
	end
end

# ! computes convergence tolerance for subproblems
function getConvTol(gapCur_fl::Float64, gapEnd_fl::Float64, conSub_tup::NamedTuple{(:rng, :int, :crs), Tuple{Vector{Float64}, Symbol, Bool}})

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


# ! WHAT OF THIS STILL RELEVANT???
#region # * manage linear trust region 

# ! adds limits specified by dictionary to problem
function addLinearTrust!(top_m::anyModel, lim_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}})
	for sys in (:tech, :exc)
		part_dic = getfield(top_m.parts, sys)
		for sSym in keys(lim_dic[sys])
			for trstSym in intersect(keys(lim_dic[sys][sSym]), keys(part_dic[sSym].var))
				# group limiting constraints
				grpBothCapa_arr = collect(groupby(lim_dic[sys][sSym][trstSym], :limCns))
				# get variables of top model
				trstVar_df = filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym])
				foreach(lim -> limitVar!(select(rename(lim, :limVal => :value), Not([:limCns])), trstVar_df, trstSym, part_dic[sSym], top_m, lim[1,:limCns]), grpBothCapa_arr)
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
