
#region # * create other elements of model

# ! create variables and capacity constraints for trade variables
function createTradeVarCns!(partBal::OthPart,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)
	for type in (:Buy, :Sell)
		trdPrc_sym = Symbol(:trd,type,:Prc)
		trd_sym = Symbol(:trd,type)
		if trdPrc_sym in keys(partBal.par) && :C in namesSym(partBal.par[trdPrc_sym].data)

			#region # * create trade variables
			c_arr = unique(partBal.par[trdPrc_sym].data[!,:C])

			# create dataframe with all potential entries for trade/sell variable
			var_df = createPotDisp(c_arr,ts_dic,anyM)

			# match all potential variables with defined prices
			var_df = matchSetParameter(var_df,partBal.par[trdPrc_sym],anyM.sets)[!,Not(:val)]

			var_df = createVar(var_df,string(:trd,type),getUpBound(var_df,anyM.options.bound.disp / anyM.options.scaFac.dispTrd,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.dispTrd)
			partBal.var[trd_sym] = orderDf(var_df)
			produceMessage(anyM.options,anyM.report, 3," - Created variables for $(type == :Buy ? "buying" : "selling") carriers")
			#endregion

			#region # * create capacity constraint on variable
			trdCap_sym = Symbol(trd_sym,:Cap)
			if trdCap_sym in keys(partBal.par)
				cns_df = matchSetParameter(var_df,partBal.par[trdCap_sym],anyM.sets,newCol = :cap)
				sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
				cns_df[!,:cap] = cns_df[!,:cap] .* sca_arr

				# prepare, scale and create constraints
				cns_df[!,:cnsExpr] = map(x -> x.var - x.cap, eachrow(cns_df))
				scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
				partBal.cns[trdCap_sym] = createCns(cnsCont(cns_df,:smaller),anyM.optModel)

				produceMessage(anyM.options,anyM.report, 3," - Created capacity restrictions for $(type == :Buy ? "buying" : "selling") carriers")
			end

			#endregion
		end
		produceMessage(anyM.options,anyM.report, 2," - Created variables and constraints for $(type == :Buy ? "buying" : "selling") carriers")
	end
	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for trade")
end

# ! create all energy balances (and curtailment variables if required)
function createEnergyBal!(techSym_arr::Array{Symbol,1},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	partBal = anyM.parts.bal
	c_arr = filter(x -> x != 0,getfield.(values(anyM.sets[:C].nodes),:idx))
	allDim_df = createPotDisp(c_arr,ts_dic,anyM)
	bal_tup = (:C,:Ts_dis)
	agg_arr = [:Ts_dis, :R_dis, :C, :scr]

	#region # * create potential curtailment and loss loss load variables

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
	#endregion

	#region # * create actual balance

	# finds all carriers that require an energy balance (not required for carriers that can only be shifted (temporal or spatial), e.g. that only have storage or exchange defined for them)
	relC_arr = Array{Int,1}()

	# if demand does not specify a carrier, it is assumed all carriers are relevant
	if !isempty(anyM.parts.bal.par[:dem].data)
		if :C in namesSym(anyM.parts.bal.par[:dem].data)
			append!(relC_arr,unique(anyM.parts.bal.par[:dem].data[!,:C]))
		else
			append!(relC_arr,c_arr)
		end
	end

	for relVar in intersect([:crt,:lss,:trdSell,:trdBuy],collect(keys(anyM.parts.bal.var)))
		append!(relC_arr,unique(anyM.parts.bal.var[relVar][!,:C]))
	end

	# add carriers beings generated or used
	append!(relC_arr,union(union(map(x -> anyM.parts.tech[x].carrier |> (y -> map(z -> getfield(y,z),intersect(keys(y),(:gen,:use)))),techSym_arr)...)...))
	relC_arr = unique(filter(x -> anyM.cInfo[x].bal != :none,relC_arr))

	# create object to write constraint data too
	cns_arr = Array{Pair{Symbol,cnsCont}}(undef,length(relC_arr))
	itrC_arr = collect(enumerate(relC_arr))

	@threads for (idx,c) in itrC_arr

		subC_arr = unique([c,getDescendants(c,anyM.sets[:C],true)...])
		cRes_tup = anyM.cInfo[c] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl))

		# ! add and scale demand values
		cns_df = matchSetParameter(filter(x -> x.C == c,allDim_df),partBal.par[:dem],anyM.sets) # demand for carrier being balanced
		cns_df[!,:val] = cns_df[!,:val] .* getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
		cns_df = rename(cns_df,:val => :dem)
				
		# ! get relevant variables
		src_df = cns_df[!,Not([:Ts_disSup,:dem])]

		# add tech variables
		cns_df[!,:techVar], unEtr_arr = getTechEnerBal(c,subC_arr,src_df,anyM.parts.tech,anyM.cInfo,anyM.sets)
		# determine where an energy balance is required because a specific demand was defined
		unEtr_arr = map(x -> x == 0.0 ,cns_df[!,:dem]) .* unEtr_arr

		# add curtailment variables
		for varType in (:crt,:lss,:trdSell,:trdBuy)
			if varType in keys(partBal.var)
				cns_df[!,Symbol(varType,:Var)] = filterCarrier(partBal.var[varType],subC_arr) |> (x -> aggUniVar(x,src_df,agg_arr, cRes_tup,anyM.sets))
				# determine where an energy balance is required because a curtailment variable was defined
				if varType in (:crt,:trdSell)
					unEtr_arr = map(x -> x == AffExpr(),cns_df[!,Symbol(varType,:Var)]) .* unEtr_arr
				end
			end
		end

		# ! add exchange variables
		# get all relevant exchange variables and add losses 
		relExc_arr = collect(filter(x -> !isempty(intersect(subC_arr,anyM.parts.exc[x].carrier)) && :exc in keys(anyM.parts.exc[x].var), keys(anyM.parts.exc)))
		if !isempty(relExc_arr)
			excVarTo_df, excVarFrom_df = map(z -> anyM.parts.exc[z] |> (u -> filter(x -> x.C in subC_arr,u.var[:exc]) |> (v -> [v, addLossesExc(v,u,anyM.sets)])),relExc_arr) |> (w -> [vcat(getindex.(w,x)...) for x in [1,2]])

			# aggregate import (from) and export (to) variables
			excFrom_arr = aggUniVar(excVarFrom_df,rename(src_df,:R_dis => :R_to),[:Ts_dis,:R_to,:C,:scr],(Ts_dis = cRes_tup[1], R_to = cRes_tup[2], C = cRes_tup[3]),anyM.sets)
			excToMain_arr  = aggUniVar(filter(x -> x.C == c, excVarTo_df),rename(src_df,:R_dis => :R_from),[:Ts_dis,:R_from,:C,:scr],(Ts_dis = cRes_tup[1], R_from = cRes_tup[2], C = cRes_tup[3]),anyM.sets)
			excToDesc_arr  = aggUniVar(filter(x -> x.C != c, excVarTo_df),rename(src_df,:R_dis => :R_from),[:Ts_dis,:R_from,:C,:scr],(Ts_dis = cRes_tup[1], R_from = cRes_tup[2], C = cRes_tup[3]),anyM.sets)

			# determine where an energy balance is required because an export variable was defined
			unEtr_arr = map(x -> x == AffExpr(), excToMain_arr) .* unEtr_arr
			delete!(cns_df, unEtr_arr) # remove rows for unrequired energy balances	
			
			# create final column with import and export variables
			cns_df[!,:excVar] =  excFrom_arr[.!unEtr_arr] .- excToMain_arr[.!unEtr_arr] .- excToDesc_arr[.!unEtr_arr]
		end
		
		# ! add demand from descendant carriers
		for cSub in filter(x -> x != c, getDescendants(c,anyM.sets[:C],false)) # demand for descendant carriers that has to be aggregated
			demSub_df = filter(x -> x.val != 0.0, matchSetParameter(filter(x -> x.C == cSub,allDim_df),partBal.par[:dem],anyM.sets))
			if isempty(demSub_df) continue end
			if anyM.cInfo[c].tsDis == anyM.cInfo[cSub].tsDis # also scales demand to be aggregated if dispatch resolution is the same as for the ancestral carrier
				demSub_df[!,:val] = demSub_df[!,:val] .* getResize(demSub_df,anyM.sets[:Ts],anyM.supTs)
			end
			cns_df[!,:dem] = aggDivVar(vcat(demSub_df,rename(select(cns_df,intCol(cns_df,:dem)),:dem => :val)),select(cns_df,intCol(cns_df)),(:Ts_dis,:R_dis,:scr),anyM.sets)
		end

		# ! prepare, scale and save constraints to dictionary
		c_str = Symbol(anyM.sets[:C].nodes[c].val)

		negVar_arr = intersect([:crtVar,:trdSellVar,:dem],namesSym(cns_df))
		posVar_arr = intersect([:techVar,:excVar,:lssVar,:trdBuyVar],namesSym(cns_df))

		cns_df[!,:cnsExpr] = map(x -> (isempty(posVar_arr) ? AffExpr() : sum(getindex(x,posVar_arr))) - (isempty(negVar_arr) ? AffExpr() : sum(getindex(x,negVar_arr))), eachrow(cns_df))
		cns_df = orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]])
		filter!(x -> x.cnsExpr != AffExpr(),cns_df)
		scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
		cns_arr[idx] = Symbol(c_str) => cnsCont(cns_df,anyM.cInfo[c].bal == :eq ? :equal : :greater)

		produceMessage(anyM.options,anyM.report, 2," - Prepared energy balance for $(c_str)")
	end

	# loops over stored constraints outside of threaded loop to create actual jump constraints
	for cns in cns_arr
		partBal.cns[Symbol(:enBal,makeUp(cns[1]))] = createCns(cns[2],anyM.optModel)
	end

	produceMessage(anyM.options,anyM.report, 1," - Created energy balances for all carriers")
	#endregion
end

# ! aggregate all technology variables for energy balance
function getTechEnerBal(cBal_int::Int,subC_arr::Array{Int,1},src_df::DataFrame,tech_dic::Dict{Symbol,TechPart},
																				cInfo_dic::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:bal),Tuple{Int,Int,Int,Int,Symbol}}},sets_dic::Dict{Symbol,Tree})
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
		allVar_df = DataFrame(Ts_dis = Int[], R_dis = Int[], scr = Int[], var = AffExpr[])
		cRes_tup = cInfo_dic[c] |> (x -> (x.tsDis, x.rDis))

		for x in relTech_arr
			# gets resolution and adjusts add_df in case of an agggregated technology
			add_df = select(filter(r -> r.C == c,tech_dic[x[1]].var[x[2]]),[:Ts_dis,:R_dis,:scr,:var])
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
			grpVar_df = combine(groupby(allVar_df, [:Ts_dis, :R_dis, :scr]), :var => (x -> sum(x)) => :var)
			techVar_arr[idx] = joinMissing(src_df,grpVar_df, [:Ts_dis, :R_dis, :scr], :left, Dict(:var => AffExpr()))[!,:var]
		end
	end

	# check where tech variables are sinks for energy carriers, which means in this case an energy balance will be necessary 
	if cInfo_dic[cBal_int].bal == :ineq 
		if unique(techVar_arr[1]) != [AffExpr()] # sinks are terms with a negative sign
			unEtr_arr = map(x -> values(x.terms) |> (y -> isempty(y) ? true : minimum(collect(y)) >= 0.0), techVar_arr[1])
		else # if all expressions are empty there aren't any sinks
			unEtr_arr = fill(true, length(techVar_arr[1]))
		end
	else # for equality constraints the energy balance is always necessary, so checking for sinks is not required
		unEtr_arr = fill(false, length(techVar_arr[1]))
	end

	return map(x -> sum(x),eachrow(hcat(techVar_arr...))), unEtr_arr
end

# ! create balance on output capacity (in particular relevant in combination with must run)
function createCapaBal!(ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},yTs_dic::Dict{Int64,Int64},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)
	
	partBal = anyM.parts.bal
	par_obj = partBal.par[:capaDem]

	#region # * obtain and prepare capacity variables

	# ! get relevant capacity variables in right format
	conv_df, st_df = [getAllVariables(Symbol(:mustCapa,z),anyM) |> (x -> isempty(x) ? getAllVariables(Symbol(:capa,z),anyM) : vcat(x,antijoin(getAllVariables(Symbol(:capa,z),anyM),x,on = intCol(x)))) for z in (:Conv,:StOut)]
	conv_df[!,:id] .= 0

	# extends capacity with generated carriers and merges them
	conv_df[!,:C] = map(x -> collect(anyM.parts.tech[sysSym(x.Te,anyM.sets[:Te])].carrier.gen) , eachrow(conv_df))
	st_df[!,:C] = map(x -> collect(anyM.parts.tech[sysSym(x.Te,anyM.sets[:Te])].carrier.stExtOut[x.id]) , eachrow(st_df))

	allCapa_df = vcat(conv_df,st_df)
	allCapa_df = flatten(allCapa_df,:C)

	# filter capacity variables that will not be part due to carrier dimension
	if :C in namesSym(par_obj.data)
		c_arr = unique(par_obj.data[!,:C])
		filter!(x -> x.C in c_arr, allCapa_df)
	end


	# add dispatch regions according to output carriers
	allCapa_df[!,:R_dis] = map(x -> r_dic[(x.R_exp,anyM.cInfo[x.C].rDis)],eachrow(allCapa_df))
	allCapa_df = orderDf(flatten(select(allCapa_df,Not([:R_exp])),:R_dis))

	# ! add design factors to capacities

	# group by technologies 
	grpCapa_gdf = groupby(allCapa_df,:Te)
	allDesFac_arr = Array{DataFrame}(undef,length(grpCapa_gdf))

	# loop over technologies and write match of capacity and design factors to array
	capa_itr = collect(enumerate(grpCapa_gdf))
	@threads for (idx,teCapa) in capa_itr

		teCapa_df = DataFrame(teCapa)
		part = anyM.parts.tech[sysSym(teCapa_df[1,:Te],anyM.sets[:Te])]

		# if design factor already defined, match with it, otherwise create an empty parameter object for design factor
		if :desFac in keys(part.par)
			hasDesFac_df = matchSetParameter(teCapa_df,part.par[:desFac],anyM.sets; newCol = :desFac)
		else 
			parDef_ntup = (dim = (:Ts_expSup, :Ts_disSup, :R_dis, :C, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :C => :up, :Te => :up), part = :techConv)
			part.par[:desFac] = ParElement(DataFrame(),parDef_ntup,:desFac,anyM.report)
			part.par[:desFac].data = DataFrame(Ts_expSup = Int[],Ts_disSup = Int[],R_dis = Int[],C = Int[],Te = Int[], id = Int[], val = Float64[])
			hasDesFac_df = DataFrame(Ts_expSup = Int[],Ts_disSup = Int[],R_dis = Int[],C = Int[],Te = Int[], id = Int[], desFac = Float64[], var = AffExpr[])
		end

		# compute missing design factors, match with capacity and merge everything 
		noDesFac_df = antijoin(teCapa_df,hasDesFac_df,on = intCol(hasDesFac_df))
		if !isempty(noDesFac_df)
			computeDesFac!(part,yTs_dic,anyM,ts_dic,select(noDesFac_df,Not([:var])))
			allDesFac_arr[idx] = vcat(hasDesFac_df,matchSetParameter(noDesFac_df,part.par[:desFac],anyM.sets; newCol = :desFac))
		else
			allDesFac_arr[idx] = hasDesFac_df
		end
	end

	# merge all capacity variables with design factors
	allCapa_df = combine(x -> (var = sum(x.var .* x.desFac),), groupby(vcat(allDesFac_arr...),[:Ts_disSup,:R_dis,:C]) )

	#endregion

	#region # * create corresponding constraints

	# ! create variables for missing capacities
	if :costMissCapa in keys(partBal.par)
		# create missing capacity variable
		var_df = matchSetParameter(allCapa_df,partBal.par[:costMissCapa],anyM.sets)
		partBal.var[:missCapa] = orderDf(createVar(select(var_df,Not([:val])),"missCapa",anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.insCapa))
	end

	# ! match with capacity demand
	cns_df = matchLimitParameter(allCapa_df,par_obj,anyM)

	if !(:Ts_disSup in namesSym(cns_df))
		push!(anyM.report,(2,"capacity balance","","capacity demand was provided without specificing the superordinate dispatch timestep, this means the sum of capacity over all years was limited instead of enforcing the same limit for each year (see https://leonardgoeke.github.io/AnyMOD.jl/stable/parameter_list/#Limits-on-quantities-dispatched)"))
	end
	# add missing capacity variables
	if :missCapa in keys(partBal.var)
		cns_df[!,:var] = cns_df[!,:var] .+ aggDivVar(partBal.var[:missCapa], cns_df, tuple(intCol(cns_df)...), anyM.sets)
	end

	# ! create constraint
	cns_df[!,:cnsExpr] = map(x -> x.var - x.val, eachrow(cns_df))
	cns_df = orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]])
	scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)

	partBal.cns[:capaBal] = createCns(cnsCont(cns_df,:greater),anyM.optModel)

	produceMessage(anyM.options,anyM.report, 2," - Created capacity balances")

	#endregion

end

# ! create constarints that enforce any type of limit (Up/Low/Fix) on any type of variable
function createLimitCns!(partLim::OthPart,anyM::anyModel)

	parLim_arr = String.(collectKeys(keys(partLim.par)))
	techLim_arr = filter(x ->  any(map(y -> occursin(y,x),["Up","Low","Fix","UpDir","LowDir","FixDir"])),parLim_arr)
	limVar_arr = union(map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2,map(y -> y => split(x,y),["Up","Low","Fix","UpDir","LowDir","FixDir"]))), techLim_arr)...)
	varToPar_dic = Dict(y => getindex.(filter(z -> z[2] == y,limVar_arr),1) for y in unique(getindex.(limVar_arr,2)))

	# loop over all variables that are subject to any type of limit (except emissions)
	allKeys_arr = collect(keys(varToPar_dic))
	cns_dic = Dict{Symbol,cnsCont}()
	signLim_dic= Dict(:Up => :smaller, :Low => :greater, :Fix => :equal, :UpDir => :smaller, :LowDir => :greater, :FixDir => :equal)

	@threads for va in allKeys_arr

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
		# ! loop over respective type of limits to obtain data
		for lim in varToPar_dic[va]
			par_obj = copy(partLim.par[Symbol(va,lim)])

			if occursin("exc",lowercase(string(va))) && !occursin("Dir",string(lim)) && :R_from in namesSym(par_obj.data) && :R_to in namesSym(par_obj.data)
				par_obj.data = vcat(par_obj.data,rename(par_obj.data,:R_from => :R_to,:R_to => :R_from))
			end
			
			limit_df = matchLimitParameter(allVar_df,par_obj,anyM)

			# merge limit constraint to other limits for the same variables
			limit_df = rename(limit_df,:val => lim)
			join_arr = [intersect(intCol(allLimit_df),intCol(limit_df))...,:var]
			miss_arr = [intCol(allLimit_df),intCol(limit_df)] |> (y -> union(setdiff(y[1],y[2]), setdiff(y[2],y[1])))
			allLimit_df = joinMissing(allLimit_df, limit_df, join_arr, :outer, merge(Dict(z => 0 for z in miss_arr),Dict(:Up => nothing, :Low => nothing, :Fix => nothing,:UpDir => nothing, :LowDir => nothing, :FixDir=> nothing)))
		end

		# merge columns for for directed limits to rest
		for dirLim in intersect(namesSym(allLimit_df),(:FixDir,:UpDir,:LowDir))
			lim_sym = Symbol(replace(string(dirLim),"Dir" => ""))
			allLimit_df[!,lim_sym] = map(x -> isnothing(x[dirLim]) ? x[lim_sym] : x[dirLim],eachrow(allLimit_df))
			select!(allLimit_df,Not([dirLim]))
		end

		# ! check for contradicting values
		colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in intCol(allLimit_df))
		limitCol_arr = intersect(namesSym(allLimit_df),(:Fix,:Up,:Low))
		entr_int = size(allLimit_df,1)
		if :Low in limitCol_arr || :Up in limitCol_arr
			# ! errors
			# upper and lower limit contradicting each other
			if :Low in limitCol_arr && :Up in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Low],nothing => 0.0) .> replace(allLimit_df[!,:Up],nothing => Inf))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"contradicting values for upper and lower limit detected for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# fix and upper limit contradicting each other
			if :Fix in limitCol_arr && :Up in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Fix],nothing => 0.0) .> (replace(allLimit_df[!,:Up],nothing => Inf)))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"fixed limit exceeds upper limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# fix and lower limit contradicting each other
			if :Fix in limitCol_arr && :Low in limitCol_arr
				for x in findall(replace(allLimit_df[!,:Fix],nothing => Inf) .< replace(allLimit_df[!,:Low],nothing => 0.0))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(3,"limit",string(va),"fixed limit is smaller than lower limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# residual values already violate limits
			resiVal_arr = getfield.(allLimit_df[!,:var],:constant)
			if :Up in limitCol_arr
				for x in findall(resiVal_arr.>  replace(allLimit_df[!,:Up] ,nothing => Inf))
					dim_str = join(map(y -> allLimit_df[x,y] == 0 ?  "" : string(y,": ",join(getUniName(allLimit_df[x,y], anyM.sets[colSet_dic[y]])," < ")),intCol(allLimit_df)),"; ")
					lock(anyM.lock)
					push!(anyM.report,(2,"limit",string(va),"residual values already exceed the upper limit for: " * dim_str))
					unlock(anyM.lock)
				end
			end

			# ! warnings
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
		# an error will occur, because a value cannot be fixed but and the same time differ by direction, this is detected here
		if :Fix in limitCol_arr && va == :capaExc
			# filters fixed exchange capacities and extracts residual values
			fix_df = select(filter(x -> x.Fix != nothing, allLimit_df) ,intCol(allLimit_df,:var))
			fix_df[!,:resi] .= getfield.(fix_df[!,:var],:constant)
			# joins together capacities in both directions
			joinA_df = rename(select(fix_df,Not([:var])),:resi => :resiA)
			joinB_df = rename(joinA_df,:resiA => :resiB)
			comp_df = innerjoin(joinA_df,joinB_df, on = intCol(joinA_df) .=> replace(intCol(joinB_df),:R_from => :R_to,:R_to => :R_from))
			# finds cases that lead to contradiction and reports on them
			contraExc_df = filter(x -> x.resiA != x.resiB && x.R_from > x.R_to,comp_df)
			for x in eachrow(contraExc_df)
				dim_str = join(map(y -> x[y] == 0 ?  "" : string(y,": ",join(getUniName(x[y], anyM.sets[colSet_dic[y]])," < ")),intCol(contraExc_df)),"; ")
				lock(anyM.lock)
				push!(anyM.report,(3,"limit",string(va),"for the exchange capacity '" * dim_str * "' residual capacites differ by direction but at the same time the installed capacity in both directions is fixed to the same value by capaExcFix, this is a contradiction and  would lead to an infeasible model"))
				unlock(anyM.lock)
			end
		end

		# ! check for suspicious entries for capacity where limits are provided for the sum of capacity over several years
		if occursin("capa",string(va))
			if !(:Ts_disSup in namesSym(allLimit_df))
				lock(anyM.lock)
				push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the superordinate dispatch timestep, this means the sum of capacity over all years was limited instead of enforcing the same limit for each year (see https://leonardgoeke.github.io/AnyMOD.jl/stable/parameter_list/#Limits-on-quantities-dispatched)"))
				unlock(anyM.lock)
			elseif 0 in unique(allLimit_df[!,:Ts_disSup])
				relEntr_df = filter(x -> x.Ts_disSup == 0, allLimit_df)
				if :Te in namesSym(relEntr_df)
					allTe_arr = unique(relEntr_df[!,:Te])
					for tInt in allTe_arr
						push!(anyM.report,(2,"limit","capacity","capacity limits were provided for '$(string(sysSym(tInt,anyM.sets[:Te])))' without specificing the superordinate dispatch timestep, this means the sum of capacity over all superordinate timesteps was limited
																						(e.g. a limit on the sum of PV capacity across all years instead of the same limit for each of these years)"))
					end
				else
					lock(anyM.lock)
					push!(anyM.report,(2,"limit","capacity","capacity limits were provided without specificing the superordinate dispatch timestep, this means the sum of capacity over all years was limited instead of enforcing the same limit for each year
																													(see https://leonardgoeke.github.io/AnyMOD.jl/stable/parameter_list/#Limits-on-quantities-dispatched)"))
					unlock(anyM.lock)
				end
			end
		end

		# ! write constraint containers
		for lim in limitCol_arr
			# filter respective limits (low, fix or up) out of the entire dataframe
			relLim_df = filter(x -> !isnothing(x[lim]),allLimit_df[!,Not(filter(x -> x != lim,limitCol_arr))])
			relLim_df = filter(x -> x.var != AffExpr(), relLim_df)
			
            if isempty(relLim_df) continue end
			rename!(relLim_df,lim => :Lim)

			# prepare, scale and save constraints to dictionary
			relLim_df[!,:cnsExpr] = map(x -> x.var - x.Lim, eachrow(relLim_df))
			relLim_df = orderDf(relLim_df[!,[intCol(relLim_df)...,:cnsExpr]])
			scaleCnsExpr!(relLim_df,anyM.options.coefRng,anyM.options.checkRng)
			cns_dic[Symbol(va,lim)] = cnsCont(relLim_df,signLim_dic[lim])

			produceMessage(anyM.options,anyM.report, 3," - Created constraints for $(lim in (:Up,:UpDir) ? "upper" : (lim in (:Low,:LowDir) ? "lower" : "fixed")) limit of variable $va")
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

#endregion

#region # * utility functions

# ! adds column with JuMP variable to dataframe
function createVar(setData_df::DataFrame,name_str::String,upBd_fl::Union{Float64,Array{Float64,1}},optModel::Model,lock_::ReentrantLock,sets::Dict{Symbol,Tree}; scaFac::Float64 = 1.0, lowBd::Float64 = 0.0, bi::Bool = false)
	# adds an upper bound to all variables if provided within the options
	#if isempty(setData_df) return DataFrame(var = AffExpr[]) end
	arr_boo = typeof(upBd_fl) <: Array
	if arr_boo
		info = VariableInfo.(!isnan(lowBd), lowBd, .!isnan.(upBd_fl), upBd_fl, false, NaN, false, NaN, bi, false)
		var_obj = JuMP.build_variable.(error, info)
	else
		info = VariableInfo(!isnan(lowBd), lowBd, !isnan(upBd_fl), upBd_fl, false, NaN, false, NaN, bi, false)
		var_obj = JuMP.build_variable(error, info)
	end

	# writes full name for each variable
	setData_df = orderDf(setData_df)
	dim_int = length(intCol(setData_df))
	col_dic = Dict(x => Symbol(split(String(intCol(setData_df)[x]),"_")[1]) for x in 1:dim_int)
	setData_df[!,:name] = string.(name_str,"[",map(x -> join(map(y -> col_dic[y] != :id ? sets[col_dic[y]].nodes[x[y]].val : x[y],1:dim_int),", "),eachrow(setData_df)),"]")

	lock(lock_)
	if arr_boo
		setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, nameItr[1], nameItr[2]) => scaFac) for nameItr in zip(var_obj,setData_df[!,:name])]
	else
		setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, var_obj, nameItr) => scaFac) for nameItr in setData_df[!,:name]]
	end
	unlock(lock_)

	return setData_df[!,Not(:name)]
end

# ! scales expressions in the dataframe to be within the range defined within options
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

# ! used to perform scaling of expression array based on range of coefficients provided
function scaleRng(expr_arr::Array{AffExpr,1},rng_arr::Array,rng_tup::Tuple{Float64,Float64}, rhs_boo::Bool)
	scaRel_arr = rhs_boo ? union(findall(rng_arr .< rng_tup[1]), findall(rng_arr .> rng_tup[2])) : union(findall(getindex.(rng_arr,1) .< rng_tup[1]), findall(getindex.(rng_arr,2) .> rng_tup[2]))
	if !isempty(scaRel_arr)
		expr_arr[scaRel_arr] = map(x -> x[1] < rng_tup[1] ? rng_tup[1]/x[1] : rng_tup[2]/x[rhs_boo ? 1 : 2], rng_arr[scaRel_arr]) .* expr_arr[scaRel_arr]
	end
	return expr_arr
end

# ! check range of coefficients in expressions within input array
function checkExprRng(expr_arr::Array{AffExpr,1},rngThres_fl::Float64)
	# obtains range of coefficients for matrix and rhs
	matRng_arr = map(x -> abs.(values(x.terms)) |> (y -> isempty(y) ? (0.0,0.0) : (minimum(y),maximum(y))), expr_arr)
	rhs_arr = abs.(getfield.(expr_arr,:constant))
	both_arr = max.(getindex.(matRng_arr,2),replace(rhs_arr,0.0 => -Inf)) ./ min.(getindex.(matRng_arr,1),replace(rhs_arr,0.0 => Inf))

	# filters rows where reange of coefficients is above threshold
	aboveThres_arr = findall(both_arr .> rngThres_fl)

	for expr in expr_arr[aboveThres_arr]
		println(expr)
	end
end

# ! creates an actual jump constraint based on the constraint container provided
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

# ! adjusts resolution of var_df according to information in first two tuples
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

#endregion

