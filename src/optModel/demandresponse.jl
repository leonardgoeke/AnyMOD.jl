
function createDR(part::TechPart,anyM::anyModel)
    #region # * Create all related variables and constraints for demand response technologies

    # create DSM downward load shift variable
    createDrDoVar!(part,anyM)

    # create stExtOut variable as the sum of DSM downward load shift variable for every tt
    createDrstExtOut!(part,anyM)

    # create balance constraint of DSM downward and upward load shifting    
    createDrBalCns(part,anyM)
    
    # create a maximum constraint for DSM upward + sum of DSM downward for every tt
    createDrCapExpBal(part,anyM)
    
    # create recovery constraint for DSM upward if recovery time R is given as a parameter
    if !iszero(part.par[:drRecoveryTime].data.val)
        createDrRecoveryCns(part,anyM)
    end
    #endregion
end

function get_tt(anyM::anyModel,setData_df::DataFrame)
    for row in eachrow(setData_df)
        family = getDescendants(row.Ts_disSup,anyM.sets[:Ts])
        delayTime_int = Int64(row.val)

        index_t = first(findall(x -> x == row.Ts_dis, family))
        ind = index_t-delayTime_int:index_t+ delayTime_int |> collect

        # Begin at end of cycle
        if any(x->x < 1, ind)
            ind_pos = filter(x -> x >= 1, ind)
            ind_neg = length(family) .+ filter(x -> x < 1, ind)
            ind = vcat(ind_neg,ind_pos)
        end

        # Begin at start of cycle again
        if any(x->x > length(family), ind)
             ind = vcat(filter(x -> x <= length(family), ind),filter(x -> x > length(family), ind) .- length(family))
        end
        row.Ts_dis2 = family[ind]
    end  
end

function getRecoveryTime(anyM::anyModel,cns_df::DataFrame)
    for row in eachrow(cns_df)
        family = getDescendants(row.Ts_disSup,anyM.sets[:Ts])
    
        index_t = first(findall(x -> x == row.Ts_dis, family))
        ind = index_t:index_t+ Int64(row.drRecoveryTime) -1 |> collect

        # Begin at start of cycle again
        if any(x->x > length(family), ind)
            ind = vcat(filter(x -> x <= length(family), ind),filter(x -> x > length(family), ind) .- length(family))
        end

        row.Ts_dis2 = family[ind]
    end  
end

function createDrDoVar!(part::TechPart,anyM::anyModel)

    basis_df = orderDf(copy(part.var[:stExtIn][!,Not(:var)]))

    dim_int = length(intCol(basis_df))
    col_dic = Dict(x => Symbol(split(String(intCol(basis_df)[x]),"_")[1]) for x in 1:dim_int)
    basis_df[!,:name] = string.("dsmDo","[",map(x -> join(map(y -> col_dic[y] != :id ? anyM.sets[col_dic[y]].nodes[x[y]].val : x[y],1:dim_int),", "),eachrow(basis_df)),"]")

    basis_df = matchSetParameter(basis_df, part.par[:drTime], anyM.sets)
    basis_df = insertcols!(basis_df, :Ts_dis2 => Ref(Int[]))
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

function createDrBalCns(part::TechPart,anyM::anyModel)
    # Zerrahn constraint 7'
    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)
    cns_df.dsmDo = orderDf(combine(groupby(part.var[:dsmDo], filter(x -> x != :Ts_dis2, intCol(part.var[:dsmDo]))), :var => (x -> sum(x)) => :dsmDo)).dsmDo
    
    cns_df = matchSetParameter(cns_df, part.par[:effStIn], anyM.sets)
    cns_df = rename(cns_df, :val => :effStIn)

    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:effStIn].*cns_df[!,:stExtIn] .- cns_df[!,:dsmDo])
    cns_cont = cnsCont(orderDf(cns_df),:equal)
    part.cns[:drBal] = createCns(cns_cont,anyM.optModel)
end

function createDrCapExpBal(part::TechPart,anyM::anyModel)   
    # Zerrahn constraint 10.
    cns_df = rename(copy(part.var[:stExtOut]),:var => :stExtOut)
    cns_df = leftjoin(cns_df, rename(part.var[:stExtIn],:var => :stExtIn), on= intCol(cns_df))
    
    cns_df = rename(innerjoin(cns_df, rename(part.var[:capaStIn],:R_exp => :R_dis), on = intCol(rename(part.var[:capaStIn],:R_exp => :R_dis))), :var => :capaStIn)
    cns_df = rename(innerjoin(cns_df, rename(part.var[:capaStOut],:R_exp => :R_dis), on = intCol(rename(part.var[:capaStOut],:R_exp => :R_dis))), :var => :capaStOut)

    sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
    cns_df[!,:capaStIn] = cns_df[!,:capaStIn] .* sca_arr
    cns_df[!,:capaStOut] = cns_df[!,:capaStOut] .* sca_arr
    
    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:stExtIn] + cns_df[!,:stExtOut].- cns_df[!,:capaStIn])
    cns_cont = cnsCont(orderDf(cns_df),:smaller)
    part.cns[:drCMax] = createCns(cns_cont,anyM.optModel)
    #TODO fix the max in accordance to the original constraint 
    "used capaStIn instead of max(capaStIn, capaStOut)"
end


function createDrRecoveryCns(part::TechPart,anyM::anyModel)
    ### Zerrahn constraint 11

    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)
    cns_df = rename(matchSetParameter(cns_df, part.par[:drRecoveryTime], anyM.sets),:val => :drRecoveryTime)

    cns_df = insertcols!(cns_df, :Ts_dis2 => Ref(Int[]))
    getRecoveryTime(anyM, cns_df)
         
    cns_df[!,:Ts_dis3] = map(x -> unique(sort(filter(y -> x.Ts_dis in y.Ts_dis2, cns_df)[!,:Ts_dis])), eachrow(cns_df) )
    cns_df_fl = flatten(cns_df, :Ts_dis3)
    # TODO, check if the sum is correct
    grpData_df = orderDf(combine(groupby(cns_df_fl, filter(x -> x != :Ts_dis, intCol(cns_df_fl))), :stExtIn => (x -> sum(x)) => :stExtIn))   

    cns_df = rename(matchSetParameter(grpData_df, part.par[:drTime], anyM.sets),:val => :drTime)
    cns_df = rename(innerjoin(cns_df, rename(part.var[:capaStIn],:R_exp => :R_dis), on = intCol(rename(part.var[:capaStIn],:R_exp => :R_dis))), :var => :capaStIn)
    
    sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
    cns_df[!,:capaStIn] = cns_df[!,:capaStIn] .* sca_arr

    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:stExtIn] - cns_df[!,:capaStIn].*cns_df[!,:drTime])
    cns_cont = cnsCont(orderDf(cns_df),:smaller)
    part.cns[:drRecovery] = createCns(cns_cont,anyM.optModel)
end      

function createDrstExtOut!(part::TechPart,anyM::anyModel)
    grpData_df = rename(orderDf(combine(groupby(part.var[:dsmDo], filter(x -> x != :Ts_dis, intCol(part.var[:dsmDo]))), :var => (x -> sum(x)) => :var)),:Ts_dis2 => :Ts_dis)
    part.var[:stExtOut] = grpData_df
end