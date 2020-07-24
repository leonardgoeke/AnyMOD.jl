


function createTech2!(t::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

    cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(t,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
    modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

    # creates expansion and capacity variables (TODO reduktion auf capacity und nur wenn kein decomm)
    createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

    # connect capacity and expansion variables (TODO ganz weg)
    #if part.type != :stock createCapaCns!(part,prepTech_dic,cns_dic) end

    # create and control operated capacity variables
    if anyM.options.decomm != :none createOprVarCns!(part,cns_dic,anyM) end
    produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

    # create dispatch variables
    createDispVar!(part,modeDep_dic,ts_dic,r_dic,anyM)
    produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")

    # create conversion balance for conversion technologies
    if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn))))
        cns_dic[:convBal] = createConvBal(part,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared conversion balance for technology $(tech_str)")
    end

    # create storage balance for storage technologies
    if :stLvl in keys(part.var)
        cns_dic[:stBal] = createStBal(part,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared storage balance for technology $(tech_str)")
    end

    # create capacity restrictions
    createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM)
    produceMessage(anyM.options,anyM.report, 3," - Prepared capacity restrictions for technology $(tech_str)")

    # create ratio constraints
    if any(map(x -> occursin("ratioEner",string(x)), collectKeys(keys(part.par))))
        createRatioCns!(part,cns_dic,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared constraints controlling energy ratios for technology $(tech_str)")
    end

    # all constraints are scaled and then written into their respective array position
    foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

    produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

    return cns_dic
end


# XXX create constarints that enforce any type of limit (Up/Low/Fix) on any type of variable
function createLimitCns2!(techIdx_arr::Array{Int,1},partLim::OthPart,anyM::anyModel)

	parLim_arr = String.(collectKeys(keys(partLim.par)))
	techLim_arr = filter(x ->  any(map(y -> occursin(y,x),["Up","Low","Fix"])),parLim_arr)
	limVar_arr = map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2,map(y -> y => split(x,y),["Up","Low","Fix"])))[1], techLim_arr)
	varToPar_dic = Dict(y => getindex.(filter(z -> z[2] == y,limVar_arr),1) for y in unique(getindex.(limVar_arr,2)))

	# loop over all variables that are subject to any type of limit (except emissions)
    dispVar_arr = [:gen,:use,:emission] # TODO andere mögliche dispatch variablen hier ergänzen
	allKeys_arr = collect(keys(varToPar_dic))
	cns_dic = Dict{Symbol,cnsCont}()
	signLim_dic= Dict(:Up => :smaller, :Low => :greater, :Fix => :equal)

    #  XXX NEU FÜR STOCHASTIC, filter dispatchvariablen
    filter!(x -> x in dispVar_arr,allKeys_arr)
	@threads for va in allKeys_arr

		varToPart_dic = Dict(:exc => :exc, :crt => :bal,:trdSell => :trd, :trdBuy => :trd)

		# obtain all variables relevant for limits
		allVar_df = getAllVariables(va,anyM)

		# check if acutally any variables were obtained
		if isempty(allVar_df)
			lock(anyM.lock)
			push!(anyM.report,(2,"limit",string(va),"limits for variable provided, but none of these variables are actually created"))
			unlock(anyM.lock)
			continue
		end

		allLimit_df = DataFrame(var = AffExpr[])
		# XXX loop over respective type of limits to obtain data
		for lim in varToPar_dic[va]
			par_obj = copy(partLim.par[Symbol(va,lim)])
			if va in (:capaExc,:oprCapaExc) && :R_a in namesSym(par_obj.data) && :R_b in namesSym(par_obj.data)
				par_obj.data = vcat(par_obj.data,rename(par_obj.data,:R_a => :R_b,:R_b => :R_a))
			end
			agg_tup = tuple(intCol(par_obj.data)...)

			# aggregate search variables according to dimensions in limit parameter
			if isempty(agg_tup)
				grpVar_df = allVar_df
			else
				grpVar_df = combine(groupby(convertExcCol(allVar_df),collect(agg_tup)), :var => (x -> sum(x)) => :var)
			end

			# try to aggregate variables to limits directly provided via inputs
			limit_df = copy(par_obj.data)
			if size(limit_df,2) != 1
				limit_df[!,:var] = aggDivVar(grpVar_df, limit_df[!,Not(:val)], agg_tup, anyM.sets, aggFilt = agg_tup)
			else
				limit_df[!,:var] .= sum(grpVar_df[!,:var])
			end

			# gets provided limit parameters, that no variables could assigned to so far and tests if via inheritance any could be assigned
			mtcPar_arr, noMtcPar_arr  = findall(map(x -> x != AffExpr(),limit_df[!,:var])) |>  (x -> [x, setdiff(1:size(par_obj.data,1),x)])
			# removes entries with no parameter assigned from limits
			limit_df = limit_df[mtcPar_arr,:]

			if !isempty(noMtcPar_arr)
				# tries to inherit values to existing variables only for parameters without variables aggregated so far
				aggPar_obj = copy(par_obj,par_obj.data[noMtcPar_arr,:])
				aggPar_obj.data = matchSetParameter(grpVar_df[!,Not(:var)],aggPar_obj,anyM.sets, useNew = false)
				# again performs aggregation for inherited parameter data and merges if original limits
				aggLimit_df = copy(aggPar_obj.data)
				if !isempty(aggLimit_df)
					aggLimit_df[!,:var]  = aggDivVar(grpVar_df, aggLimit_df, agg_tup, anyM.sets, aggFilt = agg_tup)
					limit_df = vcat(limit_df,aggLimit_df)
				end
			end

			# merge limit constraint to other limits for the same variables
			limit_df = convertExcCol(rename(limit_df,:val => lim))
			join_arr = [intersect(intCol(allLimit_df),intCol(limit_df))...,:var]
			miss_arr = [intCol(allLimit_df),intCol(limit_df)] |> (y -> union(setdiff(y[1],y[2]), setdiff(y[2],y[1])))
			allLimit_df = joinMissing(allLimit_df, limit_df, join_arr, :outer, merge(Dict(z => 0 for z in miss_arr),Dict(:Up => nothing, :Low => nothing, :Fix => nothing)))
		end

		# XXX check for contradicting values
		colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in intCol(allLimit_df))
		limitCol_arr = intersect(namesSym(allLimit_df),(:Fix,:Up,:Low))
		entr_int = size(allLimit_df,1)
		if :Low in limitCol_arr || :Up in limitCol_arr
			# XXX Errors

			# upper and lower limit contradicting each other
			if :Low in limitCol_arr && :Up in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Low],nothing => 0.0) .>= replace(allLimit_df[!,:Up],nothing => Inf))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"contradicting values for upper and lower limit detected for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# fix and upper limit contradicting each other
			if :Fix in limitCol_arr && :Up in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Fix],nothing => 0.0) .>= replace(allLimit_df[!,:Up],nothing => Inf))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"fixed limit exceeds upper limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# fix and lower limit contradicting each other
			if :Fix in limitCol_arr && :Up in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Fix],nothing => Inf) .<= replace(allLimit_df[!,:Low],nothing => 0.0))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"fixed limit is smaller than lower limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# residual values already violate limits
			resiVal_arr = getfield.(allLimit_df[!,:var],:constant)
			if :Up in limitCol_arr
				for x in findall(resiVal_arr .>  replace(allLimit_df[!,:Up],nothing => Inf))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"residual values already exceed the upper limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# XXX warning
			# upper or lower limit of zero
			if !isempty(limitCol_arr |> (y -> filter(x -> collect(x[y]) |> (z -> any(isnothing.(z)) ? false : any(z .== 0)),allLimit_df))) && va != :emission
				lock(anyM.lock)
				push!(anyM.report,(2,"limit",string(va),"upper or lower limit of zero detected, please consider to use fix or omit limit instead"))
				unlock(anyM.lock)
				entr_int = size(allLimit_df,1)
			end

			# value is fixed, but still a upper a lower limit is provided
			if :Fix in limitCol_arr
				if !isempty(filter(x -> x != :Fix, limitCol_arr) |> (z -> filter(x -> all([!isnothing(x.Fix),any(.!isnothing.(collect(x[z])))]) ,eachrow(allLimit_df))))
					lock(anyM.lock)
					push!(anyM.report,(2,"limit",string(va),"upper and/or lower limit detected, although variable is already fixed"))
					unlock(anyM.lock)
				end
			end
		end

		# if installed capacities differ depending on the direction, because residual values were defined and at the same time fixed limits on the installed capacity were provided
		# an error will occur, because a value cannot be fixed but and the same time differ by direction, this is detected hier
		if :Fix in limitCol_arr && va == :capaExc

		end

		# XXX check for suspicious entries for capacity where limits are provided for the sum of capacity over several years
		if occursin("capa",string(va))
			if !(:Ts_disSup in namesSym(allLimit_df))
				lock(anyM.lock)
				push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the superordinate dispatch timestep, this means the sum of capacity over all superordinate timesteps was limited
																												(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
				unlock(anyM.lock)
			elseif 0 in unique(allLimit_df[!,:Ts_disSup])
				relEntr_df = filter(x -> x.Ts_disSup == 0, allLimit_df)
				if :Te in namesSym(relEntr_df)
					allTe_arr = unique(relEntr_df[!,:Te])
					for t in allTe_arr
						push!(anyM.report,(2,"limit","capacity","capacity limits were provided for $(createFullString(t,anyM.sets[:Te])) without specificing the superordinate dispatch timestep, this means the sum of capacity over all superordinate timesteps was limited
																						(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
					end
				else
					lock(anyM.lock)
					push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the superordinate dispatch timestep, this means the sum of capacity over all superordinate timesteps was limited
																												(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
					unlock(anyM.lock)
				end
			end
		end

		# XXX write constraint containers
		for lim in limitCol_arr
			# filter respective limits (low, fix or up) out of the entire dataframe
			relLim_df = filter(x -> !isnothing(x[lim]),allLimit_df[!,Not(filter(x -> x != lim,limitCol_arr))])
			relLim_df = filter(x -> x.var != AffExpr() || x.Fix != 0.0, relLim_df)
            if isempty(relLim_df) continue end
			rename!(relLim_df,lim => :Lim)

			# prepare, scale and save constraints to dictionary
			relLim_df[!,:cnsExpr] = map(x -> x.var - x.Lim, eachrow(relLim_df))
			relLim_df = orderDf(relLim_df[!,[intCol(relLim_df)...,:cnsExpr]])
			scaleCnsExpr!(relLim_df,anyM.options.coefRng,anyM.options.checkRng)
			cns_dic[Symbol(va,lim)] = cnsCont(relLim_df,signLim_dic[lim])

			produceMessage(anyM.options,anyM.report, 3," - Created constraints for $(lim == :Up ? "upper" : (lim == :Low ? "lower" : "fixed")) limit of variable $va")
		end
		typeLim_sym = va in (:emission,) ? "term" : "variable"
		produceMessage(anyM.options,anyM.report, 2," - Prepared constraints to limit $typeLim_sym $va")
	end

	# loops over stored constraints outside of threaded loop to create actual jump constraints
	for cnsSym in keys(cns_dic)
		partLim.cns[cnsSym] = createCns(cns_dic[cnsSym],anyM.optModel)
	end

	produceMessage(anyM.options,anyM.report, 1," - Created all limiting constraints")
end
