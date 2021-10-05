using DataStructures

function get_tt(anyM::anyModel,setData_df::DataFrame)
    nodes_ordered = []
    for startNode_int in anyM.supTs.step
        append!(nodes_ordered, getDescendants(startNode_int,anyM.sets[:Ts]))
    end

    for row in eachrow(setData_df)
        delayTime_int = Int64(row.val)
        index_t = first(findall(x -> x == row.Ts_dis, nodes_ordered))
        ind = index_t-delayTime_int:index_t+ delayTime_int |> collect
        ind = collect(Iterators.dropwhile(<(1),ind))
        
        if last(ind) > last(1:length(nodes_ordered))
            index_f = Base.findall(x->x <=last(1:length(nodes_ordered)), ind)
            ind = ind[index_f]
        end
        row.Ts_dis2 = nodes_ordered[ind]
    end  
end

function getRecoveryTime(anyM::anyModel,cns_df::DataFrame)
    nodes_ordered = []
    for startNode_int in anyM.supTs.step
        append!(nodes_ordered, getDescendants(startNode_int,anyM.sets[:Ts]))
    end
    for row in eachrow(cns_df)
        index_t = first(findall(x -> x == row.Ts_dis, nodes_ordered))
        ind = index_t:index_t+ Int64(row.drRecoveryTime) -1 |> collect
        ind = collect(Iterators.dropwhile(<(1),ind))
        if last(ind) > last(1:length(nodes_ordered))
            index_f = Base.findall(x->x <=last(1:length(nodes_ordered)), ind)
            ind = ind[index_f]
        end
        row.Ts_dis2 = nodes_ordered[ind]
    end  
end



function createDrDoVar!(part::TechPart,anyM::anyModel)

    basis_df = orderDf(copy(part.var[:stExtIn][!,Not(:var)]))

    dim_int = length(intCol(basis_df))
    col_dic = Dict(x => Symbol(split(String(intCol(basis_df)[x]),"_")[1]) for x in 1:dim_int)
    basis_df[!,:name] = string.("dsmDo","[",map(x -> join(map(y -> col_dic[y] != :id ? anyM.sets[col_dic[y]].nodes[x[y]].val : x[y],1:dim_int),", "),eachrow(basis_df)),"]")

    basis_df = matchSetParameter(basis_df, part.par[:drTime], anyM.sets)
    basis_df = insertcols!(basis_df, :Ts_dis2 => Ref([]))
    get_tt(anyM,basis_df)
    basis_df = flatten(basis_df, :Ts_dis2)
    
    basis_df[!,:name] = map(x -> replace(x.name, "]" => string.(", ", anyM.sets[:Ts].nodes[x.Ts_dis2].val)), eachrow(basis_df))
    basis_df[!,:name] = map(x -> x.name* "]", eachrow(basis_df))


    setData_df = basis_df
    scaFac = anyM.options.scaFac.dispSt

    upBd_fl = getUpBound(basis_df,anyM.options.bound.disp / scaFac,anyM.supTs,anyM.sets[:Ts])
    lowBd = 0.0
    bi = false
    optModel = anyM.optModel
    lock_ = anyM.lock


    arr_boo = typeof(upBd_fl) <: Array
    if arr_boo
        info = VariableInfo.(!isnan(lowBd), lowBd, .!isnan.(upBd_fl), upBd_fl, false, NaN, false, NaN, bi, false)
        var_obj = JuMP.build_variable.(error, info)
    else
        info = VariableInfo(!isnan(lowBd), lowBd, !isnan(upBd_fl), upBd_fl, false, NaN, false, NaN, bi, false)
        var_obj = JuMP.build_variable(error, info)
    end

    lock(lock_)
    if arr_boo
        setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, nameItr[1], nameItr[2]) => scaFac) for nameItr in zip(var_obj,setData_df[!,:name])]
    else
        setData_df[!,:var] = [AffExpr(0,JuMP.add_variable(optModel, var_obj, nameItr) => scaFac) for nameItr in setData_df[!,:name]]
    end
    unlock(lock_)

    part.var[:dsmDo] = orderDf(setData_df[!,Not(:name)])

end


function createDrstExtOut!(part::TechPart,anyM::anyModel)
    setData_df = part.var[:stExtIn]
    groups = groupby(part.var[:dsmDo], filter(x -> x != :Ts_dis2, intCol(part.var[:dsmDo])))
    for i in 1:length(groups)
        setData_df[i,:var] = sum(groups[i].var)
    end
    part.var[:stExtOut] = orderDf(setData_df)
end



function createDrBalCns(part::TechPart,anyM::anyModel)
    # Zehrran constraint 7'
    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)

    cns_df[!,:stExtOut] .= rename(part.var[:stExtOut], :var => :stExtOut).stExtOut
    cns_df = matchSetParameter(cns_df, part.par[:effStIn], anyM.sets)
    cns_df = rename(cns_df, :val => :effStIn)

    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:effStIn].*cns_df[!,:stExtIn] .- cns_df[!,:stExtOut])
    cns_cont = cnsCont(orderDf(cns_df),:equal)
    part.cns[:drBal] = createCns(cns_cont,anyM.optModel)
end





function createDrCapExpBal(part::TechPart,anyM::anyModel)
    # Zehrran Constraint 8
    # DSM_up [t] - C_up <= 0  for t
    "constraint is equal to original part.cns[:stInRestr].cns"
    " make sure it is created!"

    # Zehrran constraint 9 
    
    cns_df = rename(part.var[:dsmDo],:var => :dsmDo)
    gr = groupby(cns_df, filter(x -> x != [:Ts_dis,:Ts_dis2], intCol(cns_df)))
    
    cns_df = rename(part.var[:stExtOut],:var => :dsmDo)
    
    for i in 1:length(gr)
        cns_df[i,:dsmDo] = sum(gr[i].dsmDo)
    end
        
    val_df = rename(part.var[:capaStOut],:R_exp => :R_dis)
    merg = innerjoin(cns_df, val_df, on=[:Ts_disSup, :R_dis, :Ts_expSup, :Te, :id])
    cns_df = rename(merg, :var => :capaStOut)
    
    sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
    cns_df[!,:capaStOut] = cns_df[!,:capaStOut] .* sca_arr
    
    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:dsmDo] .- cns_df[!,:capaStOut])
    cns_cont = cnsCont(orderDf(cns_df),:smaller)
    part.cns[:drCRed] = createCns(cns_cont,anyM.optModel)

    
    ### Zehrran constraint 10.
    cns_df = rename(part.var[:dsmDo],:var => :dsmDo)
    gr = groupby(cns_df, filter(x -> x != [:Ts_dis,:Ts_dis2], intCol(cns_df)))

    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)
    cns_df.stExtOut = rename(part.var[:stExtOut],:var => :stExtOut).stExtOut

    for i in 1:length(gr)
        cns_df[i,:stExtOut] = sum(gr[i].dsmDo)
    end

    cns_df = rename(cns_df,:stExtOut => :dsmDo)

    val_df = rename(part.var[:capaStOut],:R_exp => :R_dis)
    merg = innerjoin(cns_df, val_df, on = [:Ts_disSup, :R_dis, :Ts_expSup, :Te, :id])
    cns_df = rename(merg, :var => :capaStOut)
    
    val_df = rename(part.var[:capaStIn],:R_exp => :R_dis)
    merg = innerjoin(cns_df, val_df, on = [:Ts_disSup, :R_dis, :Ts_expSup, :Te, :id])
    cns_df = rename(merg, :var => :capaStIn)
    
    sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
    cns_df[!,:capaStOut] = cns_df[!,:capaStOut] .* sca_arr
    cns_df[!,:capaStIn] = cns_df[!,:capaStIn] .* sca_arr
    
    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:stExtIn] + cns_df[!,:dsmDo].- cns_df[!,:capaStIn])
    cns_cont = cnsCont(orderDf(cns_df),:smaller)
    part.cns[:drCMax] = createCns(cns_cont,anyM.optModel)
    
    "used capaStIn instead of max(capaStIn, capaStOut)"
    
end


function createDrRecoveryCns(part::TechPart,anyM::anyModel)
    ### Zehrran constraint 11

    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)
    cns_df = matchSetParameter(cns_df, part.par[:drTime], anyM.sets)
    cns_df = rename(cns_df,:val => :drTime)
    cns_df = matchSetParameter(cns_df, part.par[:drRecoveryTime], anyM.sets)
    cns_df = rename(cns_df,:val => :drRecoveryTime)
    
    if !iszero(cns_df.drRecoveryTime)
        val_df = rename(part.var[:capaStIn],:R_exp => :R_dis)
        merg = innerjoin(cns_df, val_df, on = [:Ts_disSup, :R_dis, :Ts_expSup, :Te, :id])
        cns_df = rename(merg, :var => :capaStIn)
        
        sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
        cns_df[!,:capaStIn] = cns_df[!,:capaStIn] .* sca_arr
        
        cns_df = insertcols!(cns_df, :Ts_dis2 => Ref([]))
        
        # sat it to 2 when testing, because it was not defined in input files
        # cns_df.drRecoveryTime .= 2 
        
        getRecoveryTime(anyM, cns_df)
        
        df_copy = cns_df
        for row in eachrow(cns_df)
            arr = row.Ts_dis2
            row.stExtIn = sum(cns_df[(df_copy[:R_dis].==row.R_dis).& ∈(arr).(df_copy.Ts_dis), :].stExtIn)
        end
        cns_df.stExtIn = df_copy.stExtIn
        
        cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:stExtIn] - cns_df[!,:capaStIn].*cns_df[!,:drTime])
        cns_cont = cnsCont(orderDf(cns_df),:smaller)
        part.cns[:drRecovery] = createCns(cns_cont,anyM.optModel)
    end
end    
