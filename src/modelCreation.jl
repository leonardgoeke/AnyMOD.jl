
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

    # remove systems without any potential capacity variables
	foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),collect(keys(prepSys_dic[:Te]))))
	foreach(x -> delete!(anyM.parts.exc, x),setdiff(collect(keys(anyM.parts.exc)),collect(keys(prepSys_dic[:Exc]))))

	#endregion

	#region # * create technology related variables and constraints

    # creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
    ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

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
		println(tSym)
		techCnsDic_arr[idx] = createTech!(sysInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepSys_dic[:Te][tSym],copy(parDef_dic),ts_dic,r_dic,anyM)
	end

	# connect retrofitting variables from the different technologies (and therefore different parts)
	# TODO mache auch für exc später, überlege wie in allgemeine funktion überführbar
	for capaSym in (:Conv, :StIn, :StOut, :StSize)
	
		retro_df = getAllVariables(Symbol(:retro,capaSym),anyM)
		
		if isempty(retro_df) continue end

		# correct variables with retrofitting factor
		retro_df = matchSetParameter(retro_df,anyM.parts.obj.par[:facRetroConv],anyM.sets)
		retro_df[!,:var] = map(x -> x.start ? x.var * x.val : x.var,eachrow(retro_df))

		# aggregate retrofitting variables
		retro_df = combine(groupby(retro_df,intCol(retro_df)),:var => (x -> sum(x)) => :cnsExpr)
		
		# add to different cnsDic for target technology
		for t in unique(retro_df[!,:Te_j])
			techCnsDic_arr[filter(x -> x[2] == sysSym(t,anyM.sets[:Te]),tech_itr)[1][1]][:retroConv] = cnsCont(select(filter(x -> x.Te_j == t,retro_df),intCol(retro_df,:cnsExpr)),:equal)
		end

	end
	
    # loops over array of dictionary with constraint container for each technology to create actual jump constraints
    for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
        anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
	end
	



    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

	#endregion

	#region # * create exchange related variables and constraints

	# constraints for exchange are prepared in threaded loop and stored in an array of dictionaries
	excSym_arr = collect(keys(anyM.parts.exc))
	excCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(excSym_arr))
	exc_itr = collect(enumerate(excSym_arr))

	#@threads for (idx,excSym) in exc_itr
	#	excCnsDic_arr[idx] = 
	#end

	# TODO connecte retro für exc, beachte gerichtet/ungerichtet


	if !all(map(x -> isempty(x),values(prepExc_dic[:capaExc])))
		partExc = anyM.parts.exc
		# create expansion and capacity variables
		createExpCap!(partExc,prepExc_dic,anyM)
		# create capacity constraint
		if isempty(anyM.subPro) || anyM.subPro == (0,0)
			createCapaExcCns!(partExc,anyM)
		end
		produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints related to expansion and capacity for exchange")
		# create dispatch related exchange elements
		if isempty(anyM.subPro) || anyM.subPro != (0,0)
			# create dispatch variables
			createExcVar!(partExc,ts_dic,anyM)
			produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")
			# create capacity restrictions
			createRestrExc!(ts_dic,partExc,anyM)
			produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")
			produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")
		end
	end

	#endregion

	if isempty(anyM.subPro) || anyM.subPro != (0,0)
		createTradeVarCns!(anyM.parts.trd,ts_dic,anyM)
		createEnergyBal!(techSym_arr,ts_dic,anyM)
	end
	createLimitCns!(anyM.parts.lim,anyM)

	produceMessage(anyM.options,anyM.report, 1," - Completed model creation")
end