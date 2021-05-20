
# ! create optimization model after anyModel has been initialized
"""
```julia
createOptModel!(model_object::anyModel)
```

Create all elements of the model's underlying optimization problem except for the objective function.
"""
function createOptModel!(anyM::anyModel)

	#region # * prepare dimensions of investment related variables
	parDef_dic = defineParameter(anyM.options,anyM.report)

    # ! gets dictionary with dimensions of expansion, retrofitting, and capacity variables
    tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
    prepSys_dic = Dict(sys => Dict{Symbol,Dict{Symbol,NamedTuple}}() for sys in (:Te,:Exc))
	prepareTechs!(collect(keys(anyM.parts.tech)),prepSys_dic[:Te],tsYear_dic,anyM)
	prepareExc!(collect(keys(anyM.parts.exc)),prepSys_dic[:Exc],tsYear_dic,anyM)

	allCapaDf_dic = addRetrofitting!(prepSys_dic,anyM)
	removeFixed!(prepSys_dic,allCapaDf_dic,anyM) # remove entries were capacities is fixed to zero
	addInsCapa!(prepSys_dic,anyM) # add entries for installed capacities

	# ! remove unrequired elements in case of distributed model creation
	if !isempty(anyM.subPro)
	    distributedMapping!(anyM,prepSys_dic)
	end
	# abort if there is already an error
    if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

	# remove systems without any potential capacity variables and reports this for exchange
	for excSym in setdiff(collect(keys(anyM.parts.exc)),collect(keys(prepSys_dic[:Exc])))
		push!(anyM.report,(2,"exchange mapping",string(excSym),"to allow for exchange between regions, residual capacities of any value (even zero) must be defined between them, but none were found"))
	end

	foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),collect(keys(prepSys_dic[:Te]))))
	foreach(x -> delete!(anyM.parts.exc, x),setdiff(collect(keys(anyM.parts.exc)),collect(keys(prepSys_dic[:Exc]))))

	#endregion

	#region # * create technology related variables and constraints

    # creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
	ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))
	
	# creates dictionary that assigns superordinate dispatch time-step to each dispatch time-step
	yTs_dic = Dict{Int,Int}()
	for x in collect(ts_dic), y in x[2] yTs_dic[y] = x[1][1] end

    # creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
    allLvlR_arr = union(getindex.(getfield.(getfield.(values(anyM.parts.tech),:balLvl),:exp),2),map(x -> x.rDis,values(anyM.cInfo)))

    allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
    r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl <= x[2] ? getDescendants(x[1], anyM.sets[:R],false,x[2]) : getAncestors(x[1],anyM.sets[:R],:int,x[2])[end]) |> (z -> typeof(z) <: Array ? z : [z]) for x in Iterators.product(allRExp_arr,allLvlR_arr))

    produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")

    # constraints for technologies are prepared in threaded loop and stored in an array of dictionaries
	techSym_arr = collect(keys(anyM.parts.tech))	
	techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techSym_arr))
	tech_itr = collect(enumerate(techSym_arr))

	@threads for (idx,tSym) in tech_itr
		techCnsDic_arr[idx] = createTech!(sysInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepSys_dic[:Te][tSym],copy(parDef_dic),ts_dic,yTs_dic,r_dic,anyM)
	end

	# connect retrofitting variables from the different technologies
	foreach(x -> createRetroConst!(x,techCnsDic_arr,tech_itr,anyM),[:Conv, :StIn, :StOut, :StSize])

    # loops over array of dictionary with constraint container for each technology to create actual jump constraints
    for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
        anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
	end

    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

	#endregion

	#region # * create exchange related variables and constraints

	# constraints for exchange are prepared in threaded loop and stored in an array of dictionaries as well
	excSym_arr = collect(keys(anyM.parts.exc))
	excCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(excSym_arr))
	excDir_arr = map(x -> sysInt(x,anyM.sets[:Exc]), filter(z -> anyM.parts.exc[z].dir, excSym_arr))
	exc_itr = collect(enumerate(excSym_arr))

	@threads for (idx,eSym) in exc_itr
		excCnsDic_arr[idx] = createExc!(sysInt(eSym,anyM.sets[:Exc]),anyM.parts.exc[eSym],prepSys_dic[:Exc][eSym],parDef_dic,ts_dic,r_dic,excDir_arr,anyM)
	end

	# connect retrofitting variables from the different exchange
	createRetroConst!(:Exc,excCnsDic_arr,exc_itr,anyM,excDir_arr)

	# loops over array of dictionary with constraint container for each exchange to create actual jump constraints
	for (idx,cnsDic) in enumerate(excCnsDic_arr), cnsSym in keys(cnsDic)
		anyM.parts.exc[excSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
	end

	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all exchange")

	#endregion

	#region # * create variables and constraints for trade, the energy balance and costs
	
	if isempty(anyM.subPro) || anyM.subPro != (0,0)
		createTradeVarCns!(anyM.parts.bal,ts_dic,anyM)
		createEnergyBal!(techSym_arr,ts_dic,anyM)
	end

	if :capaDem in keys(anyM.parts.bal.par) && (isempty(anyM.subPro) || anyM.subPro == (0,0))
		createCapaBal!(ts_dic,yTs_dic,r_dic,anyM)
	end
	
	createLimitCns!(anyM.parts.lim,anyM)
	createCost!(anyM.parts.cost,anyM)
	
	#endregion

	produceMessage(anyM.options,anyM.report, 1," - Completed model creation")
end









