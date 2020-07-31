

subPro = (2, 2)

#subPro = NamedTuple()

# TODO kontrollierte type of subPro (entweder leer oder 2 integer)

# <editor-fold desc="create technology related variables and constraints"

techSym_arr = collect(keys(anyM.parts.tech))
parDef_dic = defineParameter(anyM.options,anyM.report)

# XXX get dimension of expansion and capacity variables and mapping of capacity constraints
tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
prepVar_dic = Dict{Symbol,Dict{Symbol,NamedTuple}}()
prepareTechs!(techSym_arr,prepVar_dic,tsYear_dic,anyM)

# sets sub-problem field to function input
anyM.subPro = subPro

# XXX remove unrequired elements in case of distributed model creation
if !isempty(subPro)

    # get tuple of unrequired time-steps and scenarios
    rmvId_tup = (Ts_dis = union(map(x -> getDescendants(x,anyM.sets[:Ts],true),filter(x -> x != subPro[1],collect(anyM.supTs.step)))...),
                                                                        scr = filter(x -> x != subPro[2] && x != 0,getfield.(values(anyM.sets[:scr].nodes),:idx)))

    # remove unrequired nodes from trees of scenarios (cant remove time-steps, they needed for differentiation by expansion time-step)
    foreach(y ->  delete!(anyM.sets[:scr].nodes,y),rmvId_tup.scr)

    # rewrite information an superordinate time-steps
    anyM.supTs =  (lvl = anyM.supTs.lvl, step = (subPro[1],), sca = filter(x -> x[1][1] == subPro[1],anyM.supTs.sca), scr = Dict(subPro[1] => [subPro[2],]), scrProp = filter(x -> x[1] == (subPro[1],subPro[2]), anyM.supTs.scrProp))

    # adjust prepVar_dic
    for t in collect(keys(prepVar_dic))
        # delete fields for expansion variables
        foreach(y -> delete!(prepVar_dic[t],y), filter(x -> occursin("exp",string(x)),collectKeys(keys(prepVar_dic[t]))))
        # remove other superordinate dispatch timesteps from field for capacity variables
        for etr in collectKeys(keys(prepVar_dic[t]))
            var_df = filter(x -> x.Ts_disSup in anyM.supTs.step,prepVar_dic[t][etr].var)
            ratio_df = filter(x -> x.Ts_disSup in anyM.supTs,prepVar_dic[t][etr].ratio)
            resi_df = filter(x -> x.Ts_disSup in anyM.supTs,prepVar_dic[t][etr].resi)
            prepVar_dic[t][etr] = (var = var_df, ratio = ratio_df, resi = resi_df)
        end
    end

    # remove unrequired parameter data from tech objects
    for pName in collect(keys(anyM.parts.tech)), parName in collectKeys(keys(anyM.parts.tech[pName].par))
        parData_df = anyM.parts.tech[pName].par[parName].data
        rmv_arr = intersect(namesSym(parData_df),[:Ts_dis,:scr])
        if isempty(rmv_arr)
            continue
        else
            anyM.parts.tech[pName].par[parName].data  = filter(x -> !any(map(y -> x[y] in getfield(rmvId_tup,y),rmv_arr)),parData_df)
        end
    end

    # remove unrequired parameter data from all other objects
    for pName in (:trd,:exc,:bal,:lim,:obj), parName in collectKeys(keys(getfield(getfield(anyM.parts,pName),:par)))
        parData_df = getfield(anyM.parts,pName).par[parName].data
        rmv_arr = intersect(namesSym(parData_df),[:Ts_dis,:scr])
        if isempty(rmv_arr)
            continue
        else
            getfield(anyM.parts,pName).par[parName].data  = filter(x -> !any(map(y -> x[y] in getfield(rmvId_tup,y),rmv_arr)),parData_df)
        end
    end
end

if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

# remove technologies without any potential capacity variables
techSym_arr = collect(keys(prepVar_dic))
foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techSym_arr))

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
techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techSym_arr))
tech_itr = collect(enumerate(techSym_arr))

@threads for (idx,tSym) in tech_itr
    techCnsDic_arr[idx] = createTech2!(techInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepVar_dic[tSym],copy(parDef_dic),ts_dic,r_dic,anyM)
end

# loops over array of dictionary with constraint container for each technology to create actual jump constraints
for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
    anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
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

createTradeVarCns!(anyM.parts.trd,ts_dic,anyM)
createEnergyBal!(techSym_arr,ts_dic,anyM)
createLimitCns!(anyM.parts.lim,anyM)

produceMessage(anyM.options,anyM.report, 1," - Completed model creation")
