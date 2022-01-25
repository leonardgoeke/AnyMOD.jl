
function createDR(part::TechPart,anyM::anyModel)
    #region # * Create all related variables and constraints for demand response technologies

    # create DSM downward load shift variable
    createDrDoVar!(part,anyM)

    # create stExtOut variable as the sum of DSM downward load shift variable for every tt
    createDrstExtOut!(part)

    # create balance constraint of DSM downward and upward load shifting    
    createDrBalCns(part,anyM)
    
    # create a maximum constraint for DSM upward + sum of DSM downward for every tt
    createDrCapExpBal(part,anyM)
    
    # create recovery constraint for DSM upward if recovery time R is given as a parameter
    if :drRecoveryTime in collectKeys(keys(part.par))
        createDrRecoveryCns(part,anyM)
    end
    #endregion
end

function get_tt(anyM::anyModel,setData_df::DataFrame)

    setData_df[!,:fam] = map(x -> getDescendants(x,anyM.sets[:Ts]), setData_df.Ts_disSup)
    setData_df[!,:index_t] = map( (x,y) -> first(findall(z -> z == x, y)) , setData_df.Ts_dis, setData_df.fam)
    setData_df[!,:ind] = map((x,y) -> x-y:x+ y |> collect, 
                        setData_df.index_t, setData_df.val)
    setData_df[!,:ind] = map(x -> convert(Vector{Int64}, x), setData_df.ind)
    
    setData_df[!,:ind] =   map((x,z) -> any(y ->y <1, x) == true ?  
                            vcat(length(z) .+ filter(a -> a < 1, x), filter(a -> a >= 1, x)) : x , 
                            setData_df.ind, setData_df.fam)
        
    setData_df[!,:ind] = map((x,z) -> any(y -> y > length(z), x) == true ?
                            vcat(filter(y -> y <= length(z), x),filter(y -> y > length(z), x) .- length(z))
                            : x , setData_df.ind, setData_df.fam)

    setData_df[!,:Ts_dis2] = map((x,y) -> y[x], setData_df.ind, setData_df.fam)

    return setData_df[!,Not([:fam,:index_t,:ind])]  
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

    setData_df = orderDf(copy(part.var[:stExtIn][!,Not(:var)]))

    dim_int = length(intCol(setData_df))
    col_dic = Dict(x => Symbol(split(String(intCol(setData_df)[x]),"_")[1]) for x in 1:length(intCol(setData_df)))
    setData_df[!,:name] = string.("dsmDo","[",map(x -> join(map(y -> col_dic[y] != :id ? anyM.sets[col_dic[y]].nodes[x[y]].val : x[y],1:dim_int),", "),eachrow(setData_df)),"]")

    setData_df = matchSetParameter(setData_df, part.par[:drTime], anyM.sets)
    setData_df = insertcols!(setData_df, :Ts_dis2 => Ref(Int[]))
    setData_df = get_tt(anyM,setData_df)

    setData_df = flatten(setData_df, :Ts_dis2)
    
    setData_df[!,:name] = map(x -> replace(x.name, "]" => string.(", ", anyM.sets[:Ts].nodes[x.Ts_dis2].val)), eachrow(setData_df))
    setData_df[!,:name] = map(x -> x.name* "]", eachrow(setData_df))


    scaFac = anyM.options.scaFac.dispSt

    upBd_fl = getUpBound(setData_df,anyM.options.bound.disp / scaFac,anyM.supTs,anyM.sets[:Ts])
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

function createDrstExtOut!(part::TechPart)
    grpData_df = rename(orderDf(combine(groupby(part.var[:dsmDo], filter(x -> x != :Ts_dis, intCol(part.var[:dsmDo]))), :var => (x -> sum(x)) => :var)),:Ts_dis2 => :Ts_dis)
    part.var[:stExtOut] = grpData_df
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
end


function createDrRecoveryCns(part::TechPart,anyM::anyModel)
    ### Zerrahn constraint 11

    cns_df = rename(part.var[:stExtIn],:var => :stExtIn)
    cns_df = rename(matchSetParameter(cns_df, part.par[:drRecoveryTime], anyM.sets),:val => :drRecoveryTime)

    cns_df = insertcols!(cns_df, :Ts_dis2 => Ref(Int[]))
    getRecoveryTime(anyM, cns_df)

    cns_df[!,:sum_stExtIn] =  map(x -> (map(z -> cns_df[cns_df[:Ts_dis].== z, :stExtIn] , x)), cns_df.Ts_dis2)
    cns_df[!,:sum_stExtIn] = map(x -> sum(reduce(vcat, x)), cns_df.sum_stExtIn)

    cns_df = rename(matchSetParameter(cns_df, part.par[:drTime], anyM.sets),:val => :drTime)
    cns_df = rename(innerjoin(cns_df, rename(part.var[:capaStIn],:R_exp => :R_dis), on = intCol(rename(part.var[:capaStIn],:R_exp => :R_dis))), :var => :capaStIn)
    
    sca_arr = getResize(cns_df,anyM.sets[:Ts],anyM.supTs)
    cns_df[!,:capaStIn] = cns_df[!,:capaStIn] .* sca_arr

    cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:sum_stExtIn] .- cns_df[!,:capaStIn] .* cns_df[!,:drTime])

    cns_cont = cnsCont(orderDf(cns_df),:smaller)
    part.cns[:drRecovery] = createCns(cns_cont,anyM.optModel)
end     



function getRecoveryTime(anyM::anyModel,cns_df::DataFrame)
    cns_df[!,:fam] = map(x -> getDescendants(x,anyM.sets[:Ts]), cns_df.Ts_disSup)
    cns_df[!,:index_t] = map( (x,y) -> first(findall(z -> z == x, y)) , cns_df.Ts_dis, cns_df.fam)
    cns_df[!,:ind] = map((x,y) -> x : x + Int64(y) -1 |> collect, cns_df.index_t, cns_df.drRecoveryTime)
    cns_df[!,:ind] = map(x -> convert(Vector{Int64}, x), cns_df.ind)
    cns_df[!,:ind] =   map((x,z) -> any(y -> y > length(z), x) == true ?  
                        vcat(filter(a -> a <= length(z), x), filter(a -> a > length(z) , x) .- length(z)) : x , 
                        cns_df.ind, cns_df.fam)
    cns_df[!,:Ts_dis2] = map((x,y) -> y[x], cns_df.ind, cns_df.fam)
    return cns_df[!,Not([:fam,:index_t,:ind])]
end



# function getRecoveryTime(anyM::anyModel,cns_df::DataFrame)
#     for row in eachrow(cns_df)
#         family = getDescendants(row.Ts_disSup,anyM.sets[:Ts])
    
#         index_t = first(findall(x -> x == row.Ts_dis, family))
#         ind = index_t:index_t+ Int64(row.drRecoveryTime) -1 |> collect

#         # Begin at start of cycle again
#         if any(x->x > length(family), ind)
#             ind = vcat(filter(x -> x <= length(family), ind),filter(x -> x > length(family), ind) .- length(family))
#         end

#         row.Ts_dis2 = family[ind]
#     end  
# end

# function get_tt(anyM::anyModel,setData_df::DataFrame)
    # for row in eachrow(setData_df)
    #     family = getDescendants(row.Ts_disSup,anyM.sets[:Ts])
    #     delayTime_int = Int64(row.val)

    #     index_t = first(findall(x -> x == row.Ts_dis, family))
    #     ind = index_t-delayTime_int:index_t+ delayTime_int |> collect 

    #     # Begin at end of cycle
    #     if any(x->x < 1, ind)
    #         ind = vcat(length(family) .+ filter(x -> x < 1, ind),filter(x -> x >= 1, ind))
    #     end

    #     # Begin at start of cycle again
    #     if any(x->x > length(family), ind)
    #          ind = vcat(filter(x -> x <= length(family), ind),filter(x -> x > length(family), ind) .- length(family))
    #     end
    #     row.Ts_dis2 = family[ind]
    # end
# end