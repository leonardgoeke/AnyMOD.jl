using Gurobi

# ! compute IIS and print constraints in it
function printIIS(anyM::anyModel, noStab_ntup::Union{Nothing,NamedTuple{(:opt,:ref),Tuple{Model,GenericReferenceMap}}} = nothing)

    # computes iis
    opt_mod = isnothing(noStab_ntup) ? anyM.optModel : noStab_ntup.opt
    compute_conflict!(opt_mod)

    if MOI.get(opt_mod, MOI.ConflictStatus()) != MOI.ConflictStatusCode(3) return end
    # loops over constraint tables to find constraints within iis
    allCns_pair = vcat(collect.(vcat(anyM.parts.obj.cns, anyM.parts.bal.cns, anyM.parts.cost.cns, anyM.parts.lim.cns, map(x -> x.cns, values(anyM.parts.exc))..., map(x -> x.cns, values(anyM.parts.tech))...))...)

    for cns in allCns_pair
        if cns[1] == :objEqn continue end

        # repalce constraints with constraints from copied model
        if isnothing(noStab_ntup)
            cns_arr = cns[2][!,:cns]
        else
            cns_arr = convertAffExpr.(cns[2][!,:cns], noStab_ntup.ref)
        end
       
        allConstr_arr = findall(map(x -> MOI.ConflictParticipationStatusCode(0) != MOI.get(opt_mod.moi_backend, MOI.ConstraintConflictStatus(), x.index), cns_arr))
        # prints constraints within iis
        if !isempty(allConstr_arr)
            println("$(length(allConstr_arr)) of IIS in $(cns[1]) constraints.")
            colSet_dic = Dict(x => Symbol(split(string(x), "_")[1]) for x in filter(x -> !(x in (:actItr,:limCoef)), intCol(cns[2])))
            for iisConstr in allConstr_arr
                row = cns[2][iisConstr,:]
                dimStr_arr = map(x -> row[x] == 0 ?  "" : x == :id ? string(row[x]) : string(x, ": ", join(getUniName(row[x], anyM.sets[colSet_dic[x]]), " < ")), collect(keys(colSet_dic)))
                println("$(join(filter(x -> x != "", dimStr_arr), ", ")), constraint: $(row[:cns])")
            end
        end
    end
end

export printIIS, checkIIS