
# ! create optimization model after anyModel has been initialized
"""
```julia
createOptModel!(model_object::anyModel)
```

Create all elements of the model's underlying optimization problem except for the objective function.
"""
function createOptModel!(anyM::anyModel)

	#region # * create technology related variables and constraints
	parDef_dic = defineParameter(anyM.options,anyM.report)

    # ! gets dictionary with dimensions of expansion and capacity variables
    tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
    prepSys_dic = Dict(sys => Dict{Symbol,Dict{Symbol,NamedTuple}}() for sys in (:Te,:Exc))
	prepareTechs!(collect(keys(anyM.parts.tech)),prepSys_dic[:Te],tsYear_dic,anyM)
	prepareExc!(collect(keys(anyM.parts.exc)),prepSys_dic[:Exc],tsYear_dic,anyM)

	# TODO genau hier weiter machen

	# ! remove unrequired elements in case of distributed model creation
	if !isempty(anyM.subPro)
	    distributedMapping!(anyM,prepTech_dic,prepExc_dic)
	end
	# abort if there is already an error
    if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

    # remove technologies without any potential capacity variables
    techSym_arr = collect(keys(prepTech_dic))
    foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techSym_arr))

    # ! create all technology related elements

    # creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
    ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

    # creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
    allLvlR_arr = union(getindex.(getfield.(getfield.(values(anyM.parts.tech),:balLvl),:exp),2),map(x -> x.rDis,values(anyM.cInfo)))

    allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
    r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl <= x[2] ? getDescendants(x[1], anyM.sets[:R],false,x[2]) : getAncestors(x[1],anyM.sets[:R],:int,x[2])[end]) |> (z -> typeof(z) <: Array ? z : [z]) for x in Iterators.product(allRExp_arr,allLvlR_arr))

    produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")

    # constraints for technologies are prepared in threaded loop and stored in an array of dictionaries
	techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techSym_arr))
	tech_itr = collect(enumerate(techSym_arr))

	@threads for (idx,tSym) in tech_itr
		techCnsDic_arr[idx] = createTech!(sysInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepTech_dic[tSym],copy(parDef_dic),ts_dic,r_dic,anyM)
	end

    # loops over array of dictionary with constraint container for each technology to create actual jump constraints
    for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
        anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
    end
    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

	#endregion

	#region # * create exchange related variables and constraints


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



#=


# gather all existing capacity variables
blaConv_df = unique(vcat(filter(w -> !isempty(w), vcat(map(x -> :capaConv in keys(x) ? map(y -> getfield(x[:capaConv],y) |> (z -> select(z,intCol(z))),[:var,:resi]) : DataFrame[],values(prepSys_dic[:Te]))...))...))
blaConv_df = rename(blaConv_df,:Ts_expSup => :Ts_expSup_a,:Te => :Te_a,:R_exp => :R_exp_a)

# TODO was, wenn kapazitäten nur auf grund des umrüstens erst bestehen könnten? (gilt sowohl oben als auch unten, ich kann ja von a nach b nach c rüsten, also input für umrüstung ist selbst ein produkt der umrüstung)

tSym = :ccgtCHP

prepTech_dic = prepSys_dic[:Te][tSym]

part = anyM.parts.tech[tSym]
tInt = sysInt(tSym,anyM.sets[:Te])

# creata dataframe for potential retrofits
blub = rename(filter(x -> x.Te_a == tInt,blaConv_df),:Ts_expSup_a => :Ts_expSup_b, :Te_a => :Te_b, :R_exp_a => :R_exp_b)
bra = orderDf(rename(innerjoin(blub,blaConv_df,on = [:Ts_disSup]),:Ts_disSup => :Ts_expSup))
# filter all rows where regions dont match
relR_dic = Dict(x =>  vcat(getAncestors(x,anyM.sets[:R],:int)...,getDescendants(x,anyM.sets[:R])...) for x in unique(bra[!,:R_exp_b]))
filter!(x -> x.R_exp_a in relR_dic[x.R_exp_b],bra)

allRetro_df = matchSetParameter(bra,anyM.parts.obj.par[:costRetroConv],anyM.sets)

# TODO passe format an, sodass expansion enstpricht => :Ts_expSup um :Ts_exp erweitern und gruppieren
=#