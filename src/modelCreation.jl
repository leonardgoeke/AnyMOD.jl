
# XXX create optimization model after anyModel has been initialized
function createOptModel!(anyM::anyModel)

	# <editor-fold desc="create technology related variables and constraints"

	techIdx_arr = collect(keys(anyM.parts.tech))
    parDef_dic = defineParameter(anyM.options,anyM.report)

    # XXX get dimension of expansion and capacity variables and mapping of capacity constraints
    tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
    prepVar_dic = Dict{Int,Dict{Symbol,NamedTuple}}()
    prepareTechs!(techIdx_arr,prepVar_dic,tsYear_dic,anyM)
    if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

    # remove technologies without any potential capacity variables
    techIdx_arr = collect(keys(prepVar_dic))
    foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techIdx_arr))

    # XXX create all technology related elements

    # creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
    ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

    # creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
    allLvlR_arr = union(getindex.(getfield.(getfield.(values(anyM.parts.tech),:balLvl),:exp),2),map(x -> x.rDis,values(anyM.cInfo)))

    allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
    r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl <= x[2] ? getDescendants(x[1], anyM.sets[:R],false,x[2]) : getAncestors(x[1],anyM.sets[:R],:int,x[2])[end]) |> (z -> typeof(z) <: Array ? z : [z]) for x in Iterators.product(allRExp_arr,allLvlR_arr))

    produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")

    # constraints for technologies are prepared in threaded loop and stored in an array of dictionaries
	techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techIdx_arr))
	tech_itr = collect(enumerate(techIdx_arr))

	@threads for (idx,t) in tech_itr
		println(t)
		techCnsDic_arr[idx] = createTech!(t,anyM.parts.tech[t],prepVar_dic[t],copy(parDef_dic),ts_dic,r_dic,anyM)
	end

    # loops over array of dictionary with constraint container for each technology to create actual jump constraints
    for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
        anyM.parts.tech[techIdx_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
    end
    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

	# </editor-fold>

	# <editor-fold desc="create exchange related variables and constraints"

	prepExc_dic = Dict{Symbol,NamedTuple}()
	partExc = anyM.parts.exc
	partLim = anyM.parts.lim

	# obtain dimensions of expansion variables for exchange
	potExc_df = prepareExcExpansion!(partExc,partLim,prepExc_dic,tsYear_dic,anyM)

	# obtain capacity dimensions solely based on expansion variables
	prepareCapacity!(partExc,prepExc_dic,prepExc_dic[:expExc].var,:capaExc,anyM)
	addResidualCapaExc!(partExc,prepExc_dic,potExc_df,anyM)

	if !all(map(x -> isempty(x),values(prepExc_dic[:capaExc])))
		# create expansion and capacity variables
		createExpCap!(partExc,prepExc_dic,anyM)
		# create capacity constraint
		createCapaExcCns!(partExc,anyM)
		produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints related to expansion and capacity for exchange")
		# create dispatch related variables
		createExcVar!(partExc,ts_dic,anyM)
		produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")
		# create capacity restrictions
		createRestrExc!(ts_dic,partExc,anyM)
		produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")
		produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")
	end

	# </editor-fold>

	createTradeVarCns!(anyM.parts.trd,anyM)
	createEnergyBal!(techIdx_arr,anyM)
	createLimitCns!(techIdx_arr,anyM.parts.lim,anyM)

	produceMessage(anyM.options,anyM.report, 1," - Completed model creation")
end
