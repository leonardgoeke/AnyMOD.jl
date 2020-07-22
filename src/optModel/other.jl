
# <editor-fold desc= create other elements of model"

# XXX create variables and capacity constraints for trade variables
function createTradeVarCns!(partTrd::OthPart,anyM::anyModel)
	for type in (:Buy, :Sell)
		trdPrc_sym = Symbol(:trd,type,:Prc)
		trd_sym = Symbol(:trd,type)
		if trdPrc_sym in keys(partTrd.par) && :C in namesSym(partTrd.par[trdPrc_sym].data)

			# <editor-fold desc="create trade variables"
			c_arr = unique(partTrd.par[trdPrc_sym].data[!,:C])

			# create dataframe with all potential entries for trade/sell variable
			var_df = createPotDisp(c_arr,anyM)

			# match all potential variables with defined prices
			var_df = matchSetParameter(var_df,partTrd.par[trdPrc_sym],anyM.sets)[!,Not(:val)]

			var_df = createVar(var_df,string(:trd,type),getUpBound(var_df,anyM.options.bound.disp / anyM.options.scaFac.dispTrd,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.dispTrd)
			partTrd.var[trd_sym] = orderDf(var_df)
			produceMessage(anyM.options,anyM.report, 3," - Created variables for $(type == :Buy ? "buying" : "selling") carriers")
			# </editor-fold>

			# <editor-fold desc="create capacity constraint on variable"
			trdCap_sym = Symbol(trd_sym,:Cap)
			if trdCap_sym in keys(partTrd.par)
				cns_df = matchSetParameter(var_df,partTrd.par[trdCap_sym],anyM.sets,newCol = :cap)
				sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
				cns_df[!,:cap] = cns_df[!,:cap] .* sca_arr

				# prepare, scale and create constraints
				cns_df[!,:cnsExpr] = map(x -> x.var - x.cap, eachrow(cns_df))
				scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
				partTrd.cns[trdCap_sym] = createCns(cnsCont(cns_df,:smaller),anyM.optModel)

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

	# <editor-fold desc="create potential curtailment and loss loss load variables

	for varType in (:crt,:lss)
		# get defined entries
		var_df = DataFrame()
		for par in intersect(keys(partBal.par),vcat(varType == :crt ? :costCrt : :costLss, Symbol.(varType,[:Up,:Low,:Fix])...))
			append!(var_df,matchSetParameter(allDim_df,partBal.par[par],anyM.sets)[!,Not(:val)])
		end

		# obtain upper bound for variables and create them
		if !isempty(var_df)
			partBal.var[varType] = orderDf(createVar(var_df,string(varType),getUpBound(var_df,anyM.options.bound.disp / anyM.options.scaFac.dispTrd,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.dispTrd))
		end
	end
	# </editor-fold>

	# <editor-fold desc="create actual balance"

	# finds all carriers that require an energy balance (not required for carriers that can only be shifted (temporal or spatial), e.g. that only have storage or exchange defined for them)
	relC_arr = Array{Int,1}()

	# if demand does not specific a carrier, it is assumed all carriers are relevant
	if !isempty(anyM.parts.bal.par[:dem].data)
		if :C in namesSym(anyM.parts.bal.par[:dem].data)
			append!(relC_arr,unique(anyM.parts.bal.par[:dem].data[!,:C]))
		else
			append!(relC_arr,c_arr)
		end
	end

	if :crt in keys(anyM.parts.bal.var) append!(relC_arr,unique(anyM.parts.bal.var[:crt][!,:C])) end
	if :lss in keys(anyM.parts.bal.var) append!(relC_arr,unique(anyM.parts.bal.var[:lss][!,:C])) end
	if :trdSell in keys(anyM.parts.trd.var) append!(relC_arr,unique(anyM.parts.trd.var[:trdSell][!,:C])) end
	if :trdBuy in keys(anyM.parts.trd.var) append!(relC_arr,unique(anyM.parts.trd.var[:trdBuy][!,:C])) end

	# add carriers beings generated or used
	append!(relC_arr,union(union(map(x -> anyM.parts.tech[x].carrier |> (y -> map(z -> getfield(y,z),intersect(keys(y),(:gen,:use)))),techIdx_arr)...)...))
	relC_arr = unique(relC_arr)

	# create object to write constraint data too
	cns_arr = Array{Pair{Symbol,cnsCont}}(undef,length(relC_arr))
	itrC_arr = collect(enumerate(relC_arr))

	@threads for (idx,c) in itrC_arr

		subC_arr = unique([c,getDescendants(c,anyM.sets[:C],true)...])
		cRes_tup = anyM.cInfo[c] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl))

		# XXX add demand and size it
		cns_df = matchSetParameter(filter(x -> x.C == c,allDim_df),partBal.par[:dem],anyM.sets)
		cns_df[!,:dem] = cns_df[!,:val] .* getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
		select!(cns_df,Not(:val))

		# XXX get relevant variables
		src_df = cns_df[!,Not([:Ts_disSup,:dem])]

		# add tech variables
		cns_df[!,:techVar] = getTechEnerBal(c,subC_arr,src_df,techIdx_arr,anyM.parts.tech,anyM.cInfo,anyM.sets)

		# add curtailment variables
		for varType in (:crt,:lss)
			if varType in keys(partBal.var)
				cns_df[!,Symbol(varType,:Var)] = filterCarrier(partBal.var[varType],subC_arr) |> (x -> aggUniVar(x,src_df,agg_arr, cRes_tup,anyM.sets))
			else
				cns_df[!,Symbol(varType,:Var)] .= AffExpr()
			end
		end

		# add trade variables
		if !isempty(anyM.parts.trd.var)
			cns_df[!,:trdVar] = sum([filterCarrier(anyM.parts.trd.var[trd],subC_arr) |> (x -> aggUniVar(x,src_df,agg_arr,cRes_tup,anyM.sets) |> (y -> trd != :trdSell ? y : -1.0 * y)) for trd in keys(anyM.parts.trd.var)])
		else
			cns_df[!,:trdVar] .= AffExpr()
		end

		# add exchange variables
		if !isempty(anyM.parts.exc.var)
			excVarTo_df = filterCarrier(anyM.parts.exc.var[:exc],subC_arr)
			excVarFrom_df = convertExcCol(copy(excVarTo_df))

			# get loss values and apply them to variables
			excVarFrom_df = getExcLosses(excVarFrom_df,anyM.parts.exc.par,anyM.sets)
			excVarFrom_df[!,:var] = excVarFrom_df[!,:var] .* (1.0 .- excVarFrom_df[!,:loss])
			select!(excVarFrom_df,Not(:loss))

			balTo_tup, balFrom_tup = [tuple(replace(collect(bal_tup),:R_dis => x)...) for x in [:R_to, :R_from]]

			excFrom_arr = aggUniVar(convertExcCol(excVarFrom_df),rename(src_df,:R_dis => :R_to),[:Ts_dis,:R_to,:C],(Ts_dis = cRes_tup[1], R_to = cRes_tup[2], C = cRes_tup[3]),anyM.sets)
			excTo_arr  = aggUniVar(excVarTo_df,rename(src_df,:R_dis => :R_from),[:Ts_dis,:R_from,:C],(Ts_dis = cRes_tup[1], R_from = cRes_tup[2], C = cRes_tup[3]),anyM.sets)

			cns_df[!,:excVar] =  excFrom_arr .- excTo_arr
		else
			cns_df[!,:excVar] .= AffExpr()
		end

		# prepare, scale and save constraints to dictionary
		c_str = Symbol(anyM.sets[:C].nodes[c].val)
		cns_df[!,:cnsExpr] = map(x -> x.techVar + x.excVar + x.trdVar + x.lssVar - x.dem - x.crtVar, eachrow(cns_df))
		cns_df = orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]])
		filter!(x -> x.cnsExpr != AffExpr(),cns_df)
		scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
		cns_arr[idx] = Symbol(c_str) => cnsCont(cns_df,anyM.cInfo[c].eq ? :equal : :greater)

		produceMessage(anyM.options,anyM.report, 2," - Prepared energy balance for $(c_str)")
	end

	# loops over stored constraints outside of threaded loop to create actual jump constraints
	for cns in cns_arr
		partBal.cns[cns[1]] = createCns(cns[2],anyM.optModel)
	end

	produceMessage(anyM.options,anyM.report, 1," - Created energy balances for all carriers")
	# </editor-fold>
end

# XXX aggregate all technology variables for energy balance
function getTechEnerBal(cBal_int::Int,subC_arr::Array{Int,1},src_df::DataFrame,techIdx_arr::Array{Int,1},tech_dic::Dict{Int,TechPart},
																				cInfo_dic::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:eq),Tuple{Int,Int,Int,Int,Bool}}},sets_dic::Dict{Symbol,Tree})
	techVar_arr = Array{Array{AffExpr,1}}(undef,length(subC_arr))

	# get temporal and spatial resolution for carrier being balanced
	cBalRes_tup = cInfo_dic[cBal_int] |> (x -> (x.tsDis, x.rDis))

	# loops over all carriers relevant for respective energy balance
	for (idx,c) in enumerate(subC_arr)

		# gets technologies relevant for respective filterCarrier
		relTech_arr = getRelTech(c,tech_dic,sets_dic[:C])

		# leaves loop for carrier, if no relevant technologies could be obtained
		if isempty(relTech_arr)
			techVar_arr[idx]  = fill(AffExpr(),size(src_df,1))
			continue
		end

		# prepare loop over tech for c by creating empty dataframe and get temporal and spatial resolution for carrier being balanced
		allVar_df = DataFrame(Ts_dis = Int[], R_dis = Int[], var = AffExpr[])
		cRes_tup = cInfo_dic[c] |> (x -> (x.tsDis, x.rDis))

		for x in relTech_arr
			# gets resolution and adjusts add_df in case of an agggregated technology
			add_df = select(filter(r -> r.C == c,tech_dic[x[1]].var[x[2]]),[:Ts_dis,:R_dis,:var])
			if isempty(add_df) continue end
			tRes_tup = tech_dic[x[1]].disAgg ? (cRes_tup[1], tech_dic[x[1]].balLvl.exp[2]) : cRes_tup
			checkTechReso!(tRes_tup,cBalRes_tup,add_df,sets_dic)

			# adds sign to variables and adds them to overall dataframe
			add_df[!,:var] = add_df[!,:var] .* (x[2] in (:use,:stExtIn) ? -1.0 : 1.0)
			append!(allVar_df, add_df)
		end

		# returns empty expression if no variales could be obtained
		if isempty(allVar_df)
			techVar_arr[idx]  = fill(AffExpr(),size(src_df,1))
		else
			grpVar_df = combine(groupby(allVar_df, [:Ts_dis, :R_dis]), :var => (x -> sum(x)) => :var)
			techVar_arr[idx] = joinMissing(src_df,grpVar_df, [:Ts_dis, :R_dis], :left, Dict(:var => AffExpr()))[!,:var]
		end
	end

	return map(x -> sum(x),eachrow(hcat(techVar_arr...)))
end

# XXX create constarints that enforce any type of limit (Up/Low/Fix) on any type of variable
function createLimitCns!(techIdx_arr::Array{Int,1},partLim::OthPart,anyM::anyModel)

	parLim_arr = String.(collectKeys(keys(partLim.par)))
	techLim_arr = filter(x ->  any(map(y -> occursin(y,x),["Up","Low","Fix"])),parLim_arr)
	limVar_arr = map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2,map(y -> y => split(x,y),["Up","Low","Fix"])))[1], techLim_arr)
	varToPar_dic = Dict(y => getindex.(filter(z -> z[2] == y,limVar_arr),1) for y in unique(getindex.(limVar_arr,2)))

	# loop over all variables that are subject to any type of limit (except emissions)
	allKeys_arr = collect(keys(varToPar_dic))
	cns_dic = Dict{Symbol,cnsCont}()
	signLim_dic= Dict(:Up => :smaller, :Low => :greater, :Fix => :equal)

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
			# upper and lower limit contradicting each other
			if :Low in limitCol_arr && :Up in limitCol_arr
				filter!(x -> any(isnothing.([x.Low,x.Up])) ? true : x.Low < x.Up,allLimit_df)
				if entr_int != size(allLimit_df,1)
					lock(anyM.lock)
					push!(anyM.report,(2,"limit",string(va),"contradicting or equal values for upper and lower limit detected, both values were ignored in these cases"))
					unlock(anyM.lock)
				end
			end
			# upper or lower limit of zero
			if !isempty(limitCol_arr |> (y -> filter(x -> collect(x[y]) |> (z -> any(isnothing.(z)) ? false : any(z .== 0)),allLimit_df))) && va != :emission
				lock(anyM.lock)
				push!(anyM.report,(2,"limit",string(va),"upper or lower limit of zero detected, please consider to use fix instead"))
				unlock(anyM.lock)
				entr_int = size(allLimit_df,1)
			end
			# residual values already violate limits
			resiVal_arr = getfield.(allLimit_df[!,:var],:constant)
			if :Up in limitCol_arr && any(resiVal_arr .>  allLimit_df[!,:Up])
				for x in findall(resiVal_arr .>  allLimit_df[!,:Up])
					dimStr_arr = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"residual values already exceed the upper limit for: " * dimStr_arr))
					unlock(anyM.lock)
				end
			end

		end





		# value is fixed, but still a upper a lower limit is provided
		if :Fix in limitCol_arr && (:Low in limitCol_arr || :Up in limitCol_arr)
			if !isempty(filter(x -> x != :Fix, limitCol_arr) |> (z -> filter(x -> all([!isnothing(x.Fix),any(.!isnothing.(collect(x[z])))]) ,eachrow(allLimit_df))))
				lock(anyM.lock)
				push!(anyM.report,(2,"limit",string(va),"upper and/or lower limit detected, although variable is already fixed"))
				unlock(anyM.lock)
			end
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

# </editor-fold>

# <editor-fold desc= utility functions"

# XXX connect capacity and expansion variables
function createCapaCns!(part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},cns_dic::Dict{Symbol,cnsCont})
    for capaVar in filter(x -> occursin("capa",string(x)),keys(prepTech_dic))

        index_arr = intCol(part.var[capaVar])
        join_arr = part.type != :mature ? index_arr : filter(x -> x != :Ts_expSup,collect(index_arr))

        # joins corresponding capacity and expansion variables together
		expVar_sym = Symbol(replace(string(capaVar),"capa" => "exp"))
		if !(expVar_sym in keys(part.var)) continue end
        expVar_df = flatten(part.var[expVar_sym],:Ts_disSup)
        cns_df = rename(innerjoin(part.var[capaVar],combine(groupby(expVar_df,join_arr), :var => (x -> sum(x)) => :exp); on = join_arr),:var => :capa)

        # creates final constraint object
		cns_df[!,:cnsExpr] = map(x -> x.capa - x.capa.constant - x.exp,eachrow(cns_df))
		cns_dic[Symbol(capaVar)] = cnsCont(select(cns_df,Not([:capa,:exp])),:equal)
    end
end

# XXX adds column with JuMP variable to dataframe
function createVar(setData_df::DataFrame,name_str::String,upBd_fl::Union{Float64,Array{Float64,1}},optModel::Model,lock_::ReentrantLock,sets::Dict{Symbol,Tree}; scaFac::Float64 = 1.0, lowBd::Float64 = 0.0)
	# adds an upper bound to all variables if provided within the options
	#if isempty(setData_df) return DataFrame(var = AffExpr[]) end
	arr_boo = typeof(upBd_fl) <: Array
	if arr_boo
		info = VariableInfo.(!isnan(lowBd), lowBd, .!isnan.(upBd_fl), upBd_fl, false, NaN, false, NaN, false, false)
		var_obj = JuMP.build_variable.(error, info)
	else
		info = VariableInfo(!isnan(lowBd), lowBd, !isnan(upBd_fl), upBd_fl, false, NaN, false, NaN, false, false)
		var_obj = JuMP.build_variable(error, info)
	end

	# writes full name for each variable
	setData_df = orderDf(setData_df)
	dim_arr = map(x -> Symbol(split(String(x),"_")[1]), filter(r -> r != :id,intCol(setData_df)))
	dim_int = length(dim_arr)
	setData_df[!,:name] = string.(name_str,"[",map(x -> join(map(y -> sets[dim_arr[y]].nodes[x[y]].val,1:dim_int),", "),eachrow(setData_df)),"]")

	lock(lock_)
	if arr_boo
		setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, nameItr[1], nameItr[2]) => scaFac) for nameItr in zip(var_obj,setData_df[!,:name])]
	else
		setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, var_obj, nameItr) => scaFac) for nameItr in setData_df[!,:name]]
	end
	unlock(lock_)

	return setData_df[!,Not(:name)]
end

# XXX scales expressions in the dataframe to be within the range defined within options
function scaleCnsExpr!(cnsExpr_df::DataFrame,coefRng::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Float64,Float64}}},checkRng_fl::Float64)

	if isempty(cnsExpr_df) return end

	if !all(isnan.(coefRng.rhs))
		# scale expression defining constraint so rhs coefficients are within desired range
		rhs_arr = abs.(getfield.(cnsExpr_df[!,:cnsExpr],:constant))
		findall(rhs_arr .!= 0.0) |> (x -> cnsExpr_df[x,:cnsExpr] = scaleRng(cnsExpr_df[x,:cnsExpr],rhs_arr[x],coefRng.rhs, true))
	end

	if !all(isnan.(coefRng.mat))
		# scale expression defining constraint so matrix coefficients are within desired range
		matRng_arr = map(x -> abs.(values(x.terms)) |> (y -> isempty(y) ? (0.0,0.0) : (minimum(y),maximum(y))), cnsExpr_df[!,:cnsExpr])
		findall(map(x -> x != (0.0,0.0),matRng_arr)) |> (x -> cnsExpr_df[x,:cnsExpr] = scaleRng(cnsExpr_df[x,:cnsExpr],matRng_arr[x],coefRng.mat,false))
	end

	if !isnan(checkRng_fl)
		checkExprRng(cnsExpr_df[:,:cnsExpr],checkRng_fl)
	end
end

# XXX used to perform scaling of expression array based on range of coefficients provided
function scaleRng(expr_arr::Array{AffExpr,1},rng_arr::Array,rng_tup::Tuple{Float64,Float64}, rhs_boo::Bool)
	scaRel_arr = rhs_boo ? union(findall(rng_arr .< rng_tup[1]), findall(rng_arr .> rng_tup[2])) : union(findall(getindex.(rng_arr,1) .< rng_tup[1]), findall(getindex.(rng_arr,2) .> rng_tup[2]))
	if !isempty(scaRel_arr)
		expr_arr[scaRel_arr] = map(x -> x[1] < rng_tup[1] ? rng_tup[1]/x[1] : rng_tup[2]/x[rhs_boo ? 1 : 2], rng_arr[scaRel_arr]) .* expr_arr[scaRel_arr]
	end
	return expr_arr
end

# XXX check range of coefficients in expressions within input array
function checkExprRng(expr_arr::Array{AffExpr,1},rngThres_fl::Float64)

	# obtains range of coefficients for matrix and rhs
	matRng_arr = map(x -> abs.(values(x.terms)) |> (y -> (minimum(y),maximum(y))), expr_arr)
	rhs_arr = abs.(getfield.(expr_arr,:constant))
	both_arr = max.(getindex.(matRng_arr,2),replace(rhs_arr,0.0 => -Inf)) ./ min.(getindex.(matRng_arr,1),replace(rhs_arr,0.0 => Inf))

	# filters rows where reange of coefficients is above threshold
	aboveThres_arr = findall(both_arr .> rngThres_fl)

	for expr in expr_arr[aboveThres_arr]
		println(expr)
	end
end

# XXX creates an actual jump constraint based on the constraint container provided
function createCns(cnsCont_obj::cnsCont,optModel::Model)
	cns_df = cnsCont_obj.data
	if cnsCont_obj.sign == :equal
		cns_df[!,:cns] = map(x -> @constraint(optModel, x.cnsExpr == 0),eachrow(cns_df))
	elseif cnsCont_obj.sign == :greater
		cns_df[!,:cns] = map(x -> @constraint(optModel, x.cnsExpr >= 0),eachrow(cns_df))
	elseif cnsCont_obj.sign == :smaller
		cns_df[!,:cns] = map(x -> @constraint(optModel, x.cnsExpr <= 0),eachrow(cns_df))
	end
	return select!(cns_df,Not(:cnsExpr))
end

# XXX adjusts resolution of var_df according to information in first two tuples
function checkTechReso!(tRes_tup::Tuple{Int,Int},cBalRes_tup::Tuple{Int,Int},var_df::DataFrame,sets_dic::Dict{Symbol,Tree})
	# if dispatch regions for technology were disaggregated, replace the disaggregated with the ones relevant for the carrier
	for (idx,dim) in enumerate([:Ts_dis,:R_dis])
		if cBalRes_tup[idx] != tRes_tup[idx]
			set_sym = Symbol(split(string(dim),"_")[1])
			dim_dic = Dict(x => getAncestors(x,sets_dic[set_sym],:int,cBalRes_tup[idx])[end] for x in unique(var_df[!,dim]))
			var_df[!,dim] = map(x -> dim_dic[x],var_df[!,dim])
		end
	end
end

# </editor-fold>
