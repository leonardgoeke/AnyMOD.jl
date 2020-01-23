
# <editor-fold desc= create other elements of model"

# XXX create variables and capacity constraints for trade variables
function createTradeVarCns!(partTrd::OthPart,anyM::anyModel)
	for type in (:Buy, :Sell)
		trdPrc_sym = Symbol(:trd,type,:Prc)
		trd_sym = Symbol(:trd,type)
		if trdPrc_sym in keys(partTrd.par) && :C in names(partTrd.par[trdPrc_sym].data)

			# <editor-fold desc="create trade variables"
			c_arr = unique(partTrd.par[trdPrc_sym].data[!,:C])

			# create dataframe with all potential entries for trade/sell variable
			var_df = createPotDisp(c_arr,anyM)

			# match all potential variables with defined prices
			var_df = matchSetParameter(var_df,partTrd.par[trdPrc_sym],anyM.sets,anyM.report)[!,Not(:val)]

			var_df = createVar(var_df,string(:trd,type),getUpBound(var_df,anyM),anyM.optModel,anyM.lock,anyM.sets)
			partTrd.var[trd_sym] = orderDf(var_df)
			produceMessage(anyM.options,anyM.report, 3," - Created variables for $(type == :Buy ? "buying" : "selling") carriers")
			# </editor-fold>

			# <editor-fold desc="create capacity constraint on variable"
			trdCap_sym = Symbol(trd_sym,:Cap)
			if trdCap_sym in keys(partTrd.par)
				cns_df = matchSetParameter(var_df,partTrd.par[trdCap_sym],anyM.sets,anyM.report,newCol = :cap)
				sca_arr = getScale(cns_df,anyM.sets[:Ts],anyM.supTs)
				cns_df[!,:cap] = cns_df[!,:cap] .* sca_arr

				withlock(anyM.lock) do
					cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.var <= x.cap),eachrow(cns_df))
				end
				partTrd.cns[trdCap_sym] = orderDf(cns_df[!,[intCol(cns_df)...,:cns]])
				produceMessage(anyM.options,anyM.report, 3," - Created capacity restrictions for $(type == :Buy ? "buying" : "selling") carriers")
			end

			# </editor-fold>
		end
		produceMessage(anyM.options,anyM.report, 2," - Created variables and constraints for $(type == :Buy ? "buying" : "selling") carriers")
	end
	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for trade")
end

# XXX create all energy balances (and curtailment variables if required)
function createEnergyBal!(techIdx_arr::Array{Int,1},anyM::anyModel)

	partBal = anyM.parts.bal
	c_arr = filter(x -> x != 0,getfield.(values(anyM.sets[:C].nodes),:idx))
	allDim_df = createPotDisp(c_arr,anyM)
	bal_tup = (:C,:Ts_dis)
	agg_arr = [:Ts_dis, :R_dis, :C]

	# <editor-fold desc="create potential curtailment variables

	# get defined entries
	varCrt_df = DataFrame()
	for crtPar in intersect(keys(partBal.par),(:crtUp,:crtLow,:crtFix,:costCrt))
		append!(varCrt_df,matchSetParameter(allDim_df,partBal.par[crtPar],anyM.sets,anyM.report)[!,Not(:val)])
	end

	# obtain upper bound for variables and create them
	if !isempty(varCrt_df)
		partBal.var[:crt] = orderDf(createVar(varCrt_df,"crt",getUpBound(varCrt_df,anyM),anyM.optModel,anyM.lock,anyM.sets))
	end
	# </editor-fold>

	# <editor-fold desc="create actual balance"
	allC_arr = unique(allDim_df[!,:C])
	Threads.@threads for c in allC_arr
		relC_arr = unique([c,getDescendants(c,anyM.sets[:C])...])

		cRes_tup = anyM.cInfo[c] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl))

		# XXX add demand and scale it
		cns_df = matchSetParameter(filter(x -> x.C == c,allDim_df),partBal.par[:dem],anyM.sets,anyM.report)
		cns_df[!,:dem] = cns_df[!,:val] .* getScale(cns_df,anyM.sets[:Ts],anyM.supTs)
		select!(cns_df,Not(:val))

		# XXX get relevant variables
		src_df = cns_df[!,Not([:Ts_disSup,:dem])]

		# add tech variables
		cns_df[!,:techVar] = getTechEnerBal(c,relC_arr,src_df,techIdx_arr,anyM.parts.tech,anyM.sets)

		# add curtailment variables
		if :crt in keys(partBal.var)
			cns_df[!,:crtVar] = filterCarrier(partBal.var[:crt],relC_arr) |> (x -> aggUniVar(x,src_df,agg_arr, cRes_tup,anyM.sets))
		else
			cns_df[!,:crtVar] .= AffExpr()
		end

		# add trade variables
		if !isempty(anyM.parts.trd.var)
			cns_df[!,:trdVar] = sum([filterCarrier(anyM.parts.trd.var[trd],relC_arr) |> (x -> aggUniVar(x,src_df,agg_arr,cRes_tup,anyM.sets) |> (y -> trd != :trdSell ? y : -1.0 * y)) for trd in keys(anyM.parts.trd.var)])
		else
			cns_df[!,:trdVar] .= AffExpr()
		end

		# add exchange variables
		if !isempty(anyM.parts.exc.var)
			excVarTo_df = filterCarrier(anyM.parts.exc.var[:exc],relC_arr)
			excVarFrom_df = convertExcCol(copy(excVarTo_df))

			# apply loss values to from dataframe of from variables
			lossPar_obj = copy(anyM.parts.exc.par[:lossExc])
			lossPar_obj.data = lossPar_obj.data |> (x -> vcat(x,rename(x,:R_a => :R_b, :R_b => :R_a)))
			excVarFrom_df = matchSetParameter(excVarFrom_df,lossPar_obj,anyM.sets,anyM.report,newCol = :loss)

			# overwrite symmetric losses with any directed losses provided
			if :lossExcDir in keys(anyM.parts.exc.par)
				oprCol_arr = intCol(excVarFrom_df)
				dirLoss_df = matchSetParameter(excVarFrom_df[!,oprCol_arr],anyM.parts.exc.par[:lossExcDir],anyM.sets,anyM.report,newCol = :lossDir)
				excVarFrom_df = joinMissing(excVarFrom_df,dirLoss_df,oprCol_arr,:left,Dict(:lossDir => nothing))
				excVarFrom_df[!,:val] = map(x -> isnothing(x.lossDir) ? x.loss : x.lossDir,eachrow(excVarFrom_df[!,[:loss,:lossDir]]))
				select!(excVarFrom_df,Not(:lossDir))
			end

			# apply loss values to from variables
			excVarFrom_df[!,:var] = excVarFrom_df[!,:var] .* (1.0 .- excVarFrom_df[!,:loss])
			select!(excVarFrom_df,Not(:loss))

			balTo_tup, balFrom_tup = [tuple(replace(collect(bal_tup),:R_dis => x)...) for x in [:R_to, :R_from]]

			excFrom_arr = aggUniVar(convertExcCol(excVarFrom_df),rename(src_df,:R_dis => :R_to),[:Ts_dis,:R_to,:C],(Ts_dis = cRes_tup[1], R_to = cRes_tup[2], C = cRes_tup[3]),anyM.sets)
			excTo_arr  = aggUniVar(excVarTo_df,rename(src_df,:R_dis => :R_from),[:Ts_dis,:R_from,:C],(Ts_dis = cRes_tup[1], R_from = cRes_tup[2], C = cRes_tup[3]),anyM.sets)

			cns_df[!,:excVar] =  excFrom_arr .- excTo_arr
		end

		# XXX create final constaint depending on equality and non-equality cases
		withlock(anyM.lock) do
			if anyM.cInfo[c].eq
				cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, 0.1 * (x.techVar + x.excVar + x.trdVar) ==  0.1 * (x.dem + x.crtVar)),eachrow(cns_df))
			else
				cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, 0.1 *  (x.techVar + x.excVar + x.trdVar) >= 0.1 * (x.dem + x.crtVar)),eachrow(cns_df))
			end
		end

		# XXX writes constraint to object
		c_str = anyM.sets[:C].nodes[c].val
		produceMessage(anyM.options,anyM.report, 2," - Created energy balance for $(c_str)")
		partBal.cns[Symbol(c_str)] = orderDf(cns_df[!,[intCol(cns_df)...,:cns]])
	end

	produceMessage(anyM.options,anyM.report, 1," - Created energy balances for all carriers")
	# </editor-fold>
end

# XXX aggregate all technology variables for energy balance
function getTechEnerBal(cBal_int::Int,relC_arr::Array{Int,1},src_df::DataFrame,techIdx_arr::Array{Int,1},tech_dic::Dict{Int,TechPart},sets_dic::Dict{Symbol,Tree})
	techVar_arr = Array{Array{AffExpr,1}}(undef,length(relC_arr))

	# get temporal and spatial resolution for carrier being balanced
	cBalRes_tup = anyM.cInfo[cBal_int] |> (x -> (x.tsDis, x.rDis))

	# loops over all carriers relevant for respective energy balance
	for (idx,c) in enumerate(relC_arr)
		relTech_arr = vcat([intersect((:use,:gen,:stExtIn,:stExtOut),filter(y -> c in tech_dic[x].carrier[y], collect(keys(tech_dic[x].carrier)))) |> (y -> collect(zip(fill(x,length(y)),y))) for x in techIdx_arr]...)

		if isempty(relTech_arr)
			techVar_arr[idx]  = fill(AffExpr(),size(src_df,1))
			continue
		end

		# prepare loop over tech for c by creating empty dataframe and get temporal and spatial resolution for carrier being balanced
		allVar_df = DataFrame(Ts_dis = Int[], R_dis = Int[], var = AffExpr[])
		cRes_tup = anyM.cInfo[c] |> (x -> (x.tsDis, x.rDis))

		for x in relTech_arr
			add_df = select(filter(r -> r.C == c,tech_dic[x[1]].var[x[2]]),[:Ts_dis,:R_dis,:var])
			# gets resolution of technology being balanced from respective carrier and the information, if it is a disaggregated further or not
			tRes_tup = tech_dic[x[1]].disAgg ? (cRes_tup[1], anyM.cInfo[c].rExp) : cRes_tup

			# if dispatch regions for technology were disaggregated, replace the disaggregated with the ones relevant for the carrier
			for (idx,dim) in enumerate([:Ts_dis,:R_dis])
				if cBalRes_tup[idx] != tRes_tup[idx]
					set_sym = Symbol(split(string(dim),"_")[1])
					dim_dic = Dict(x => getindex(getAncestors(x,anyM.sets[set_sym],cBalRes_tup[idx])[end],1) for x in unique(add_df[!,dim]))
					add_df[!,dim] = map(x -> dim_dic[x],add_df[!,dim])
				end
			end

			add_df[!,:var] = add_df[!,:var] .* (x[2] in (:use,:stExtIn) ? -1.0 : 1.0)
			append!(allVar_df, add_df)
		end

		grpVar_df = by(allVar_df, [:Ts_dis, :R_dis], var = [:var] => x -> sum(x.var))

		techVar_arr[idx] = joinMissing(src_df,grpVar_df, [:Ts_dis, :R_dis], :left, Dict(:var => AffExpr()))[!,:var]
	end

	return map(x -> sum(x),eachrow(hcat(techVar_arr...)))
end

# XXX create constarints that enforce any type of limit (Up/Low/Fix) on any type of variable
function createLimitCns!(techIdx_arr::Array{Int,1},partLim::OthPart,anyM::anyModel)

	techLim_arr = filter(x ->  any(map(y -> occursin(string(y),string(x)),[:Up,:Low,:Fix])) ,string.(keys(partLim.par)))
	limVar_arr = map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2,map(y -> y => split(x,y),["Up","Low","Fix"])))[1], techLim_arr)
	varToPar_dic = Dict(y => getindex.(filter(z -> z[2] == y,limVar_arr),1) for y in unique(getindex.(limVar_arr,2)))

	# loop over all variables that are subject to any type of limit (except emissions)
	allKeys_arr = collect(keys(varToPar_dic))
	Threads.@threads for va in allKeys_arr
		varToPart_dic = Dict(:exc => :exc, :crt => :bal,:trdSell => :trd, :trdBuy => :trd)

		# obtain all variables relevant for limits
		allVar_df = getAllVariables(va,anyM)

		# check if acutally any variables were obtained
		if isempty(allVar_df)
			push!(anyM.report,(2,"limit",string(va),"limits for variable provided, but none of these variables are actually created"))
			continue
		end

		allLimit_df = DataFrame(var = AffExpr[])
		# XXX loop over respective type of limits to obtain data
		for lim in varToPar_dic[va]
			par_obj = copy(partLim.par[Symbol(va,lim)])
			agg_tup = tuple(intCol(par_obj.data)...)

			# aggregate search variables according to dimensions in limit parameter
			grpVar_df = by(convertExcCol(allVar_df),collect(agg_tup), var = [:var] => x -> sum(x.var))

			# try to aggregate variables to limits directly provided via inputs
			limit_df = copy(par_obj.data)
			limit_df[!,:var] = aggDivVar(grpVar_df, limit_df[!,Not(:val)], agg_tup, anyM.sets, aggFilt = agg_tup)

			# gets provided limit parameters, that no variables could assigned to so far and tests if via inheritance any could be assigned
			mtcPar_arr, noMtcPar_arr  = findall(map(x -> x != AffExpr(),limit_df[!,:var])) |>  (x -> [x, setdiff(1:size(par_obj.data,1),x)])
			# removes entries with no parameter assigned from limits
			limit_df = limit_df[mtcPar_arr,:]

			if !isempty(noMtcPar_arr)
				# tries to inherit values to existing variables only for parameters without variables aggregated so far
				aggPar_obj = copy(par_obj,par_obj.data[noMtcPar_arr,:])
				aggPar_obj.data = matchSetParameter(grpVar_df[!,Not(:var)],aggPar_obj,anyM.sets,anyM.report, useNew = false)
				# again performs aggregation for inherited parameter data and merges if original limits
				aggLimit_df = copy(aggPar_obj.data)
				aggLimit_df[!,:var]  = aggDivVar(grpVar_df, aggLimit_df, agg_tup, anyM.sets, aggFilt = agg_tup)
				limit_df = vcat(limit_df,aggLimit_df)
			end

			# merge limit constraint to other limits for the same variables
			join_arr = [intersect(intCol(allLimit_df),intCol(limit_df))...,:var]
			miss_arr = [intCol(allLimit_df),intCol(limit_df)] |> (y -> union(setdiff(y[1],y[2]), setdiff(y[2],y[1])))
			allLimit_df = joinMissing(allLimit_df, convertExcCol(rename(limit_df,:val => lim)), join_arr, :outer, merge(Dict(z => 0 for z in miss_arr),Dict(:Up => nothing, :Low => nothing, :Fix => nothing)))
		end

		# XXX check for contradicting values
		limitCol_arr = intersect(names(allLimit_df),(:Fix,:Up,:Low))
		entr_int = size(allLimit_df,1)
		if :Low in limitCol_arr || :Up in limitCol_arr
			# upper and lower limit contradicting each other
			if :Low in limitCol_arr && :Up in limitCol_arr
				filter!(x -> any(isnothing.([x.Low,x.Up])) ? true : x.Low < x.Up,allLimit_df)
				if entr_int != size(allLimit_df,1)
					push!(anyM.report,(2,"limit",string(va),"contradicting or equal values for upper and lower limit detected, both values were ignored in these cases"))
				end
			end
			# upper or lower limit of zero
			if !isempty(limitCol_arr |> (y -> filter(x -> collect(x[y]) |> (z -> any(isnothing.(z)) ? false : any(z .== 0)),allLimit_df))) && va != :emission
				push!(anyM.report,(2,"limit",string(va),"upper or lower limit of zero detected, please consider to use fix instead"))
				entr_int = size(allLimit_df,1)
			end
		end

		# value is fixed, but still a upper a lower limit is provided
		if :Fix in limitCol_arr && (:Low in limitCol_arr || :Up in limitCol_arr)
			if !isempty(limitCol_arr |> (z -> filter(x -> all([!isnothing(x.Fix),any(.!isnothing.(x[z]))]) ,allLimit_df)))
				push!(anyM.report,(2,"limit",string(va),"upper and/or lower limit detected, although variable is already fixed"))
			end
		end

		# XXX check for suspicious entries for capacity where limits are provided for the sum of capacity over several years
		if occursin("capa",string(va))
			if !(:Ts_disSup in names(allLimit_df))
				push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the supordinate dispatch timestep, this means the sum of capacity over all supordinate timesteps was limited
																												(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
			elseif 0 in unique(allLimit_df[!,:Ts_disSup])
				relEntr_df = filter(x -> x.Ts_disSup == 0, allLimit_df)
				if :Te in names(relEntr_df)
					allTe_arr = unique(relEntr_df[!,:Te])
					for t in allTe_arr
						push!(anyM.report,(2,"limit","capacity","capacity limits were provided for $(createFullString(t,anyM.sets[:Te])) without specificing the supordinate dispatch timestep, this means the sum of capacity over all supordinate timesteps was limited
																						(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
					end
				else
					push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the supordinate dispatch timestep, this means the sum of capacity over all supordinate timesteps was limited
																												(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
				end
			end
		end

		# XXX create final constraints
		for lim in limitCol_arr
			relLim_df = filter(x -> !isnothing(x[lim]),allLimit_df[!,Not(filter(x -> x != lim,limitCol_arr))])
            if isempty(relLim_df) continue end

			withlock(anyM.lock) do
				if lim == :Up
					relLim_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.var <=  x.Up),eachrow(relLim_df))
				elseif lim == :Low
					relLim_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.var >=  x.Low),eachrow(relLim_df))
				elseif lim == :Fix
					relLim_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.var ==  x.Fix),eachrow(relLim_df))
				end
			end

			partLim.cns[Symbol(va,lim)] = orderDf(relLim_df[!,[intCol(relLim_df)...,:cns]])
			produceMessage(anyM.options,anyM.report, 3," - Created constraints for $(lim == :Up ? "upper" : (lim == :Low ? "lower" : "fixed")) limit of variable $va")
		end
		produceMessage(anyM.options,anyM.report, 2," - Created constraints to limit variable $va")
	end
	produceMessage(anyM.options,anyM.report, 1," - Created all limiting constraints")
end

# </editor-fold>

# <editor-fold desc= utility"

# XXX connect capacity and expansion variables
function createCapaCns!(part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},anyM::anyModel)
    for capaVar in filter(x -> occursin("capa",string(x)),keys(prepTech_dic))

        index_arr = intCol(part.var[capaVar])
        join_arr = part.type != :mature ? index_arr : filter(x -> x != :Ts_expSup,collect(index_arr))

        # joins corresponding capacity and expansion variables together
		expVar_sym = Symbol(replace(string(capaVar),"capa" => "exp"))
		if !(expVar_sym in keys(part.var)) continue end
        expVar_df = flatten(part.var[expVar_sym],:Ts_disSup)
        cns_df = rename(join(part.var[capaVar],by(expVar_df,join_arr, exp = :var => x -> sum(x)); on = join_arr, kind = :inner),:var => :capa)

        # creates final equation
        withlock(anyM.lock) do
            cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.capa - x.capa.constant == x.exp),eachrow(cns_df))
        end
        part.cns[Symbol(capaVar)] = orderDf(cns_df[!,Not([:capa,:exp])])
    end
end

# XXX adds column with JuMP variable to dataframe
function createVar(setData_df::DataFrame,name_str::String,upBd_any::Union{Nothing,Float64,Array{Float64,1}},optModel::Model,lock::SpinLock,sets::Dict{Symbol,Tree})
	# adds an upper bound to all variables if provided within the options
	#if isempty(setData_df) return DataFrame(var = AffExpr[]) end
	arr_boo = typeof(upBd_any) <: Array
	if arr_boo
		info = VariableInfo.(true, 0.0, true, upBd_any, false, NaN, false, NaN, false, false)
		var_obj = JuMP.build_variable.(error, info)
	else
		info = VariableInfo(true, 0.0, false, isnothing(upBd_any) ? NaN : upBd_any, false, NaN, false, NaN, false, false)
		var_obj = JuMP.build_variable(error, info)
	end

	# writes full name for each variable
	setData_df = orderDf(setData_df)
	dim_arr = map(x -> Symbol(split(String(x),"_")[1]), filter(r -> r != :id,intCol(setData_df)))
	dim_int = length(dim_arr)
	setData_df[!,:name] = string.(name_str,"[",map(x -> join(map(y -> sets[dim_arr[y]].nodes[x[y]].val,1:dim_int),", "),eachrow(setData_df)),"]")

	withlock(lock) do
		if arr_boo
			setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, nameItr[1], nameItr[2]) => 1) for nameItr in zip(var_obj,setData_df[!,:name])]
		else
			setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, var_obj, nameItr) => 1) for nameItr in setData_df[!,:name]]
		end
	end

	return setData_df[!,Not(:name)]
end

# </editor-fold>
