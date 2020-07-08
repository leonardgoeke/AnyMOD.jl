using Gurobi
function printIIS(anyM::anyModel)

    # computes iis
    Gurobi.compute_conflict(anyM.optModel.moi_backend.optimizer.model)

    if anyM.optModel.moi_backend.optimizer.model.inner.conflict != 0 return end
    # loops over constraint tables to find constraints within iis
    allCns_pair = vcat(collect.(vcat(anyM.parts.bal.cns, anyM.parts.trd.cns, anyM.parts.lim.cns, map(x -> x.cns,values(anyM.parts.tech))...))...)

    for cns in allCns_pair
        if cns[1] == :objEqn continue end

        allConstr_arr = findall(map(x -> MOI.get(anyM.optModel.moi_backend, Gurobi.ConstraintConflictStatus(), x.index),cns[2][!,:cns]))
        # prints constraints within iis
        if !isempty(allConstr_arr)
            println("$(length(allConstr_arr)) of IIS in $(cns[1]) constraints.")
            colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in intCol(cns[2]))
            for iisConstr in allConstr_arr
                row = cns[2][iisConstr,:]
                dimStr_arr = map(x -> row[x] == 0 ?  "" : string(x,": ",join(getUniName(row[x], anyM.sets[colSet_dic[x]])," < ")),collect(keys(colSet_dic)))
                println("$(join(filter(x -> x != "",dimStr_arr),", ")), constraint: $(row[:cns])")
            end
        end
    end
end
export printIIS
