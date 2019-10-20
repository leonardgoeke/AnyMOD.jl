
function addConstraints!(anyM::anyModel)

    if !(isdefined(anyM,:variables)) error("Variables need to be defined before constraints are created!") end

    anyM.constraints = Dict{Symbol,CnsElement}()

    # XXX investment related constraints
    # all limits on variables (investment, capacity, and dispatch)
    createLimitConstraints!(anyM)
    # aggregate dispatch variables and save aggrations to dictionary
    createConstraint!(:agg,anyM)
    # definition of installed capacity
    controllCapaConstraints!(anyM)
    # controlls re- and de-commissioning
    if anyM.options.decomm != :none createConstraint!(:commission,anyM) end
    # ensures production quantities complies with parameter ratios
    createConstraint!(:ratioEner,anyM)
    produceMessage(anyM.options,anyM.report, 2," - Created all constraints controlling variables")

    # XXX distpach related constraints
    # create balancing constraints
    createConstraint!(:enerBal,anyM)
    createConstraint!(:convBal,anyM)
    createConstraint!(:stBal,anyM)
    produceMessage(anyM.options,anyM.report, 2," - Created all balancing constraints")

    # create capacity restrictions on dispatch variables
    createConstraint!(:capaRestr,anyM)
    # set capacity limit for trade capacities
    createConstraint!(:limitTrd,anyM)
    produceMessage(anyM.options,anyM.report, 2," - Created all capacity related constraints")


    produceMessage(anyM.options,anyM.report, 1," - Created all constraints")
end

createConstraint!(name::Symbol, anyM::anyModel) = createConstraint!(Val{name}(), anyM::anyModel)

createObjective(name::Symbol,AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}}, anyM::anyModel) = createObjective(Val{name}(), AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}}, anyM::anyModel)

# XXX create constraints controlling limits (fixed/lower/upper) on all variables
function createLimitConstraints!(anyM::anyModel)

    limitTab_dic = Dict{Symbol,IndexedTable}()
    stockTech_arr = DB.select(DB.filter(r -> r.type == 0,anyM.mapping[:TechData]),:Te)

    # XXX create table of limits and their respective variables (lower/upper/fixed)
    for varName in keys(anyM.variables)
        limitEntry_tab = collectLimitConstraints(anyM.variables[varName].data, varName, stockTech_arr, anyM)
        if !(isnothing(limitEntry_tab)) limitTab_dic[varName] = limitEntry_tab end
    end

    # XXX seperately creates emissions constraints
    allUse_tab = matchSetParameter(anyM.report,anyM.variables[:use].data,anyM.parameter[:emissionFac],anyM.sets,anyM.options.scale.compDig)
    allUseEm_tab = reindex(IT.transform(DB.select(allUse_tab,DB.Not(All(:val,:var,:Ts_supDis))),:var => DB.select(allUse_tab,(:var,:val) => x -> x.val * x.var / 1e6)), anyM.parameter[:emissionFac].dim)
    emEntry_tab = collectLimitConstraints(allUseEm_tab, :emission, Int32[], anyM)
    if !(isnothing(emEntry_tab)) limitTab_dic[:emission] = emEntry_tab end

    # XXX creates actual limiting constraints
    for varName in keys(limitTab_dic)
        # initializes variables
        eqnTable_dic = Dict{Symbol,IndexedTable}() # empty dictionary for limit constraints
        limitTab_tab = limitTab_dic[varName]
        crtLimits_tup = intersect(colnames(limitTab_tab),(:Low,:Up,:Fix))

        # creates a table per existing limit type and writes to an array
        for type in (:Low, :Up, :Fix)
            if type in crtLimits_tup
                subLim_tab = DB.dropmissing(DB.select(limitTab_tab,DB.Not(All(tuple(setdiff(crtLimits_tup,[type])...)))))
                if type == :Low
                    eqnTable_dic[type] = IT.transform(DB.select(subLim_tab, DB.Not(All(:var,type))),:eqn => DB.select(subLim_tab,(type,:var)
                                                                                                => x -> @constraint(anyM.optModel, x.Low * anyM.options.scale.limit <= sum(x.var) * anyM.options.scale.limit )))
                elseif type == :Up
                    eqnTable_dic[type] = IT.transform(DB.select(subLim_tab, DB.Not(All(:var,type))),:eqn => DB.select(subLim_tab,(type,:var)
                                                                                                => x -> @constraint(anyM.optModel, x.Up * anyM.options.scale.limit >= sum(x.var) * anyM.options.scale.limit)))
                else
                    eqnTable_dic[type] = IT.transform(DB.select(subLim_tab, DB.Not(All(:var,type))),:eqn => DB.select(subLim_tab,(type,:var)
                                                                                                => x -> @constraint(anyM.optModel, x.Fix * anyM.options.scale.limit == sum(x.var) * anyM.options.scale.limit)))
                end
            end
        end

        # merges all limit constraints to one table
        limit_tab = mergeDicTable(eqnTable_dic, false)

        # adds table, if existing, to dictionary of constraints if existing
        eqnName_sym = Symbol(varName, :Limit)
        dim_tup = tuple(filter(x -> x != :eqn,collect(colnames(limit_tab)))...)
        anyM.constraints[eqnName_sym] = CnsElement(eqnName_sym, dim_tup, reindex(limit_tab,tuple(filter(r -> r != :eqn,collect(colnames(limit_tab)))...)))
        produceMessage(anyM.options,anyM.report, 3," - Created constraints limiting $varName")
    end

    produceMessage(anyM.options,anyM.report, 2," - Created all limiting constraints")
end

# XXX aggregates all dispatch variables
function createConstraint!(name::Val{:agg},anyM::anyModel)

    # loops over aggregation dictionaries written earlier and creates constraints accordingly
    for varName in keys(anyM.variables)

        varData_obj = anyM.variables[varName]
        agg_dic = anyM.aggVar[varName]
        if isempty(agg_dic) continue end # continues if no aggregations were written earlier

        # XXX creates aggregation constraint based on keys/values in dictionary
        var_arr = DB.select(varData_obj.data,:var)
        aggEqn_dic = Dict{Int64,ConstraintRef}()

        for aggRow in keys(agg_dic)
            aggEqn_dic[aggRow] = @constraint(anyM.optModel, var_arr[aggRow] == sum(var_arr[collect(agg_dic[aggRow])]))
        end
        aggEqnSort_dic = sort(aggEqn_dic)

        # XXX writes aggregation to constraint object
        dimIdx_tab = DB.select(varData_obj.data,DB.Not(All(:var)))
        eqnName_sym = Symbol(:agg,uppercase(String(varName)[1]),String(varName)[2:end])
        anyM.constraints[eqnName_sym] = CnsElement(eqnName_sym, varData_obj.dim, IT.transform(dimIdx_tab[collect(keys(aggEqnSort_dic))],:eqn => collect(values(aggEqnSort_dic))))
        produceMessage(anyM.options,anyM.report, 3," - Created constraints to aggregate $varName variables")
    end

    produceMessage(anyM.options,anyM.report, 2," - Created constraints to aggregate variables")
end

# XXX creates constraints defining the installed capacity depending on investment and residual capacities
function controllCapaConstraints!(anyM::anyModel)

    # assigns the year to each investment timestep starting with the subordinate level
    tsYear_dic = Dict(zip(anyM.supDis.step,convert(Array{Int32,1},collect(0:anyM.options.shortInvest:(length(anyM.supDis.step)-1)*anyM.options.shortInvest))))

    allEqn_tab = Dict{Symbol,IndexedTable}()
    capa_tup = (:Conv, :StIn, :StOut, :StSize, :Exc)

    for capaItr in capa_tup
        # <editor-fold desc="XXX prepares investment variables"
        invVar = anyM.variables[Symbol(:inv,capaItr)]
        groupCol_tup = tuple(replace(collect(invVar.dim),:Ts_inv => :Ts_supDis)...)

        # extends the investment variable table to include dispatch timesteps within lifetime of the respective expansion
        lftm_tab = reindex(matchSetParameter(anyM.report,invVar.data,anyM.parameter[Symbol(:life,capaItr)],anyM.sets,anyM.options.scale.compDig,:life),invVar.dim)
        invTsup_arr = DB.select(lftm_tab,(:Ts_inv,:life) => x -> filter(y -> (tsYear_dic[y] > tsYear_dic[x.Ts_inv]-anyM.options.shortInvest) && (tsYear_dic[y] <= tsYear_dic[x.Ts_inv]+x.life),collect(anyM.supDis.step)))

        invVarExt_tab = DB.rename(DB.flatten(IT.transform(invVar.data,:Ts_supDis => invTsup_arr)),:var => :inv)

        # groups investment by investment period
        invVarExtType2_tab = DB.groupreduce((inv = +,), invVarExt_tab, groupCol_tup; select = :inv)

        # </editor-fold>

        # <editor-fold desc="XXX joins investment to capacity variables"

        capaVar_obj = anyM.variables[Symbol(:capa,capaItr)]
        if isempty(capaVar_obj.data) continue end
        # gets all actual variables of type 2 (mature) and 3 (emerging)
        if typeof(DB.select(capaVar_obj.data,:var)) == Array{VariableRef,1}
            capaVar_tab = capaVar_obj.data
        else
            capaVar_tab = DB.filter(r -> r.var.constant == 0,capaVar_obj.data)
        end

        # joins investment and capacity into one table, different join for exchange capacity (because no differentiation between stock/non-stock)
        if capaItr != :Exc
            # filters type 2 variables and joins investment variables
            capaVarType2_tab = DB.filter(r -> r.Ts_inv == 0,capaVar_tab)
            capaVarType2Exp_tab = rmvDummyCol(DB.join(addDummyCol(capaVarType2_tab),invVarExtType2_tab; lkey = groupCol_tup, rkey = groupCol_tup, how = :inner))
            #  filters type 3 variables and joins investment variables
            capaVarType3_tab = DB.filter(r -> r.Ts_inv != 0,capaVar_tab)
            capaVarType3Exp_tab = DB.join(capaVarType3_tab,invVarExt_tab; lkey = capaVar_obj.dim, rkey = capaVar_obj.dim, how = :inner)
            # merge both tables, add residutal capacities to table and write constraints
            capaVarAll_tab = DB.merge(capaVarType2Exp_tab,capaVarType3Exp_tab)
        else
            capaVarAll_tab = DB.join(capaVar_tab, invVarExtType2_tab; lkey = capaVar_obj.dim, rkey = capaVar_obj.dim, how = :inner)
        end

        # joins residual capacities to table and write constraint
        if Symbol(:capa,capaItr,:Resi) in keys(anyM.parameter)
            capaVarFull_tab = joinMissing(capaVarAll_tab, matchSetParameter(anyM.report,DB.select(capaVarAll_tab,groupCol_tup),anyM.parameter[Symbol(:capa,capaItr,:Resi)],anyM.sets,anyM.options.scale.compDig,:resi),groupCol_tup,groupCol_tup,:left,(0,GenericAffExpr{Float64,VariableRef}(),GenericAffExpr{Float64,VariableRef}(),0.0))
        else
            capaVarFull_tab = IT.transform(capaVarAll_tab,:resi => fill(0.0,length(capaVarAll_tab)))
        end

        capaVarFull_tab = DB.reindex(capaVarFull_tab,capaVar_obj.dim)
        groupColPlusTsInv_tup = capaItr != :Exc ? tuple(vcat(:Ts_inv,groupCol_tup...)...) : groupCol_tup

        allEqn_tab[capaItr] = reindex(IT.transform(DB.select(capaVarFull_tab,groupColPlusTsInv_tup ),:eqn =>
                                                                            DB.select(capaVarFull_tab,(:var,:inv,:resi) => x -> @constraint(anyM.optModel, x.var == x.inv + x.resi))),groupColPlusTsInv_tup)


        # </editor-fold>
    end

    for capaItr in keys(allEqn_tab)
        anyM.constraints[capaItr] = CnsElement(capaItr, tuple(filter(x -> x != :eqn,collect(colnames(allEqn_tab[capaItr])))...), allEqn_tab[capaItr])
    end
    produceMessage(anyM.options,anyM.report, 3," - Created constraints defining installed capacity")
end

# XXX creates constraints controlling the de- and re-commissioning of capacities
function createConstraint!(name::Val{:commission},anyM::anyModel)
    dimConv_tup, dimSt_tup, dimExc_tup = [anyM.variables[Symbol(:capa,x)].dim for x in (:Conv,:StIn,:Exc)]

    # ensures commissioned capacity does not exceed installed capacity
    for type in (:Conv, :StIn, :StOut, :StSize, :Exc)

        # initializes symbols and tuples
        instCapa_sym = Symbol(:capa,type)
        commCapa_sym = Symbol(:capaComm,type)
        dim_tup = type in (:Conv,:Exc) ? (type == :Conv ? dimConv_tup : dimExc_tup) : dimSt_tup

        # XXX constraints to limit commissioned capacity to installed capacity
        # assigns installed to commissioned variable, writes constraint and creates objects
        eqnComm_tab = DB.join((l,r) -> (eqn = @constraint(anyM.optModel, l.var >= r.comm),),
                                                            anyM.variables[instCapa_sym].data,DB.rename(anyM.variables[commCapa_sym].data,:var => :comm); rkey = dim_tup, lkey = dim_tup, how = :inner)

        anyM.constraints[Symbol(:comm,type)] = CnsElement(Symbol(:comm,type), dim_tup, eqnComm_tab)

        # XXX creates constraints to not allow for re-commissioning
        if anyM.options.decomm == :decomm
            # determines the first dispatch year within data
            startDisp_int = minimum(DB.select(anyM.variables[commCapa_sym].data,:Ts_supDis))

            dimPrev_tup = tuple(replace(collect(dim_tup), :Ts_supDis => :Ts_supDisPrev)...)
            dimNoTsInv_tup = tuple(filter(x -> x != :Ts_inv,collect(dim_tup))...)

            # filter first, determine previous year for other and joins correspondig commissioning variable
            commVarFilt_tab = DB.filter(r -> r.Ts_supDis != startDisp_int,anyM.variables[commCapa_sym].data)
            commVar_tab = DB.rename(IT.transform(commVarFilt_tab,:Ts_supDisPrev => DB.select(commVarFilt_tab,:Ts_supDis => r -> r-1)),:var => :commPrev)

            commPreVar_tab = DB.rename(DB.join(commVar_tab, anyM.variables[commCapa_sym].data; lkey = dimPrev_tup, rkey = dim_tup, how = :inner),:var => :commNow)

            # joins investment variable for current year
            commInvVar_tab = DB.rename(joinMissing(commPreVar_tab, anyM.variables[Symbol(:inv,type)].data, dimNoTsInv_tup,  anyM.variables[Symbol(:inv,type)].dim, :left, (GenericAffExpr{Float64,VariableRef}(),)),:var => :invNow)

            # add any increase of residual capacities
            resiPar_sym = Symbol(:capa,type,:Resi)
            if resiPar_sym in keys(anyM.parameter)
                # adds residual capacities for current and previous year to table
                commJoinResi_tab = DB.select(commInvVar_tab,DB.Not(All(:commPrev,:commNow,:invNow)))
                commResiNow_tab = matchSetParameter(anyM.report,commJoinResi_tab,anyM.parameter[resiPar_sym],anyM.sets,anyM.options.scale.compDig,:valNow)
                commResiPrev_tab = matchSetParameter(anyM.report,DB.rename(DB.select(commJoinResi_tab,DB.Not(All(:Ts_supDis))),:Ts_supDisPrev => :Ts_supDis),anyM.parameter[resiPar_sym],anyM.sets,anyM.options.scale.compDig,:valPrev)
                # filter entries where residual increase and adds to them to table
                commResiBoth_tab = DB.join((l,r) ->  (Ts_supDis = l.Ts_supDis, deltaResi = l.valNow > r.valPrev ? (l.valNow - r.valPrev) : 0.0),commResiNow_tab,commResiPrev_tab;lkey = dimPrev_tup, rkey = dim_tup, how = :inner)
                commResiDelta_tab = DB.filter(r -> r.deltaResi != 0.0, commResiBoth_tab)
            else
                commResiDelta_tab = table(fill(Int32[],length(dim_tup)+1)...,names = vcat(:deltaResi,dim_tup...))
            end

            # creates actual constraint and object
            commDecommEqn_tab = join((l,r) -> (eqn =  @constraint(anyM.optModel, l.commNow <= l.commPrev + l.invNow - coalesce(r.deltaResi,0.0)),),
                                                                    DB.select(commInvVar_tab,DB.Not(All(:Ts_supDisPrev))),commResiDelta_tab; lkey = dim_tup, rkey = dim_tup,how = :left)

            anyM.constraints[Symbol(:decomm,type)] = CnsElement(Symbol(:decomm,type) ,dim_tup, commDecommEqn_tab)
        end
    end
    produceMessage(anyM.options,anyM.report, 3," - Created constraints defining commissioned capacity")
end

# XXX creates energy balance
function createConstraint!(name::Val{:enerBal},anyM::anyModel)

    # <editor-fold desc="adds variables from conversion and storage"
    # adds positive variables
    posVar_tab = DB.merge(DB.filter(r -> r.Te == 0 && r.Ts_supDis in anyM.supDis.step, anyM.variables[:gen].data), DB.filter(r -> r.Te == 0 && r.Ts_supDis in anyM.supDis.step, anyM.variables[:stExtOut].data))
    posVarGrp_tab = JuliaDB.groupby(posVar_tab, (:Ts_supDis, :Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
        NamedTuple{(:posVar,)}(tuple(Array(y)))
    end

    # adds negative variables
    negVar_tab = DB.merge(DB.filter(r -> r.Te == 0 && r.Ts_supDis in anyM.supDis.step, anyM.variables[:use].data), DB.filter(r -> r.Te == 0 && r.Ts_supDis in anyM.supDis.step,anyM.variables[:stExtIn].data))
    negVarGrp_tab = JuliaDB.groupby(negVar_tab, (:Ts_supDis, :Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
        NamedTuple{(:negVar,)}(tuple(Array(y)))
    end

    # joins positive and negative variables from conversion and storage
    joinMiss_arr = convert(Array{Union{Array{VariableRef,1},Float64,GenericAffExpr},1},[Array{VariableRef,1}(),Array{VariableRef,1}()])
    eqnVar1_tab = joinMissing(posVarGrp_tab,negVarGrp_tab,(:Ts_supDis, :Ts_dis, :R_dis, :C),(:Ts_supDis, :Ts_dis, :R_dis, :C),:outer,tuple(joinMiss_arr...))
    # </editor-fold>

    # <editor-fold desc="adds variables from exchange"
    if :exchange in keys(anyM.variables)
        if :lossExc in keys(anyM.parameter)
            # find entries with exchange losses, repeat matches with switched to/from and joint to variable table
            excLoss_tab = matchSetParameter(anyM.report,DB.rename(DB.select(anyM.variables[:exchange].data,DB.Not(All(:var))),:R_to => :R_a,:R_from => :R_b),anyM.parameter[:lossExc],anyM.sets,anyM.options.scale.compDig,:val,false)
            excLossTwice_tab = DB.merge(DB.rename(excLoss_tab,:R_b => :R_from,:R_a => :R_to),DB.rename(excLoss_tab,:R_a => :R_from,:R_b => :R_to))

            excVar_tab = joinMissing(anyM.variables[:exchange].data,excLossTwice_tab,(:Ts_supDis,:Ts_dis,:R_from,:R_to,:C), (:Ts_supDis,:Ts_dis,:R_from,:R_to,:C), :left, (0.0,))

            # groups exchange variables by from regions and includes losses (= exchange requires more input in source region than is transferred to the taraget region)
            excFromGrp_tab = JuliaDB.groupby(excVar_tab, (:Ts_supDis, :Ts_dis, :R_from, :C), usekey = false; select = (:var,:val)) do y
                NamedTuple{(:excFrom,)}(tuple(sum(dot(y.var,  round.(1 ./ (1 .- y.val), digits = anyM.options.scale.compDig )))))
            end
        else
            # groups exchange variables by from regions
            # groups exchange variables by from regions
            excFromGrp_tab = JuliaDB.groupby(anyM.variables[:exchange].data, (:Ts_supDis, :Ts_dis, :R_from, :C), usekey = false; select = :var) do y
                NamedTuple{(:excFrom,)}(tuple(sum(y)))
            end
        end

        # groups exchange variables by to regions
        excToGrp_tab = JuliaDB.groupby(anyM.variables[:exchange].data, (:Ts_supDis, :Ts_dis, :R_to, :C), usekey = false; select = :var) do y
            NamedTuple{(:excTo,)}(tuple(sum(y)))
        end
        push!(joinMiss_arr,GenericAffExpr{Float64,VariableRef}())
        eqnVarExcFrom_tab = joinMissing(eqnVar1_tab,excFromGrp_tab, (:Ts_supDis, :Ts_dis, :R_dis, :C), (:Ts_supDis, :Ts_dis, :R_from, :C), :outer, tuple(joinMiss_arr...))
        push!(joinMiss_arr,GenericAffExpr{Float64,VariableRef}())
        eqnVar1_tab = joinMissing(eqnVarExcFrom_tab,excToGrp_tab, (:Ts_supDis, :Ts_dis, :R_dis, :C), (:Ts_supDis, :Ts_dis, :R_to, :C), :outer, tuple(joinMiss_arr...))
    end
    # </editor-fold>

    # <editor-fold desc="adds trade variables"
    for typ in (:Buy,:Sell)
        trdName_sym = Symbol(:trade,typ)
        if trdName_sym in keys(anyM.variables)
            trdVar_tab = DB.groupby(anyM.variables[trdName_sym].data, (:Ts_supDis, :Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
                NamedTuple{(Symbol(:trd,typ),)}(tuple(Array(y)))
            end
            push!(joinMiss_arr,Array{VariableRef,1}())
            eqnVar1_tab = joinMissing(eqnVar1_tab,trdVar_tab,(:Ts_supDis, :Ts_dis, :R_dis, :C), (:Ts_supDis, :Ts_dis, :R_dis, :C), :outer, tuple(joinMiss_arr...))
        end
    end
    # </editor-fold>

    # <editor-fold desc="adds parameter"
    # adds demand parameter, since these are provided in MW they are scaled to MWh
    demand_tab = matchSetParameter(anyM.report,DB.select(eqnVar1_tab,(:Ts_supDis,:Ts_dis,:R_dis,:C)),anyM.parameter[:demand],anyM.sets,anyM.options.scale.compDig,:dem)
    demandScaled_tab = addScaling(demand_tab,:dem,anyM.sets[:Ts],anyM.supDis)

    push!(joinMiss_arr,0.0)
    eqnVar2_tab = joinMissing(eqnVar1_tab,demandScaled_tab,(:Ts_supDis,:Ts_dis, :R_dis, :C), (:Ts_supDis,:Ts_dis, :R_dis, :C), :left, tuple(joinMiss_arr...))
    # </editor-fold>

    # <editor-fold desc="write final constraints"

    # determines rows where carrier is on the first level, because in these cases the energy balance is a equality constraint
    firstLvlC_tup = tuple(anyM.sets[:C][ anyM.sets[:C][:,:lvl] .== 1,:idx]...)
    eqnVar3_tab = IT.transform(eqnVar2_tab,:parentC => map(x -> x in firstLvlC_tup,DB.select(eqnVar2_tab,:C)))

    blub = IT.transform(eqnVar3_tab,:id => collect(1:length(eqnVar3_tab)))

    eqnCol_tup = colnames(eqnVar3_tab)
    # creates constraints differently depending on how exchange and trade are applied
    if :exchange in keys(anyM.variables)
        if :trdBuy in eqnCol_tup && :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:excTo,:excFrom,:dem,:trdBuy,:trdSell) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) == sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) >= sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem))
        elseif :trdBuy in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:excTo,:excFrom,:dem,:trdBuy) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) == sum(x.negVar)+sum(x.excFrom)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) >= sum(x.negVar)+sum(x.excFrom)+x.dem))
        elseif :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:excTo,:excFrom,:dem,:trdSell) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo) == sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo) >= sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem))
        else
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:excTo,:excFrom,:dem) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo) == sum(x.negVar)+sum(x.excFrom)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.excTo) >= sum(x.negVar)+sum(x.excFrom)+x.dem))
        end
    else
        if :trdBuy in eqnCol_tup && :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:dem,:trdBuy,:trdSell) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.trdBuy) == sum(x.negVar)+sum(x.trdSell)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.trdBuy) >= sum(x.negVar)+sum(x.trdSell)+x.dem))
        elseif :trdBuy in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:dem,:trdBuy) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar)+sum(x.trdBuy) == sum(x.negVar)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar)+sum(x.trdBuy) >= sum(x.negVar)+x.dem))
        elseif :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:dem,:trdSell) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar) == sum(x.negVar)+sum(x.trdSell)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar) >= sum(x.negVar)+sum(x.trdSell)+x.dem))
        else
            eqn_arr = DB.select(eqnVar3_tab,(:parentC,:posVar,:negVar,:dem) =>
                            x -> x.parentC ? @constraint(anyM.optModel, sum(x.posVar) == sum(x.negVar)+x.dem) :
                                                @constraint(anyM.optModel, sum(x.posVar) >= sum(x.negVar)+x.dem))
        end
    end

    anyM.constraints[:enerBal] = CnsElement(:enerBal, (:Ts_dis, :R_dis, :C), IT.transform(DB.select(eqnVar3_tab,(:Ts_supDis,:Ts_dis, :R_dis, :C)),:eqn => eqn_arr))
    # </editor-fold>
    produceMessage(anyM.options,anyM.report, 3," - Created energy balances")
end

# XXX creates conversion balance
function createConstraint!(name::Val{:convBal},anyM::anyModel)
    joinKey_tup = (:Ts_inv, :Ts_dis, :R_dis, :Te)
    joinKeyM_tup = (:Ts_inv, :Ts_dis, :R_dis, :Te, :M)

    # XXX determines relevant dimensions for conversion balance
    convBalTech_tab = DB.select(DB.filter(r -> r.refLvl != nothing && ((:use in keys(r.allCar) && :gen in keys(r.allCar)) || :stIntIn in keys(r.allCar) || :stIntOut in keys(r.allCar)),
                                                                                                                    DB.select(anyM.mapping[:TechInfo],(:Te,:refLvl,:allCar))),DB.Not(All(:allCar)))

    allCapaConv_tab = table(DB.unique(rows(DB.select(anyM.variables[:capaConv].data,(:Ts_inv,:Te)))))

    # gets levels in conversion balance and expands to actual regions and timesteps
    convBalDim_tab = DB.select(IT.transform(convBalTech_tab,:Ts_dis => map(x -> x.Ts, DB.select(convBalTech_tab,:refLvl)),:R_dis => map(x -> x.R, DB.select(convBalTech_tab,:refLvl))),DB.Not(All(:refLvl)))
    convBal_tab = expandSetColumns(DB.join(convBalDim_tab,allCapaConv_tab;lkey = :Te, rkey = :Te, how = :inner),(:Ts_dis,:R_dis), anyM.sets)

    # joins conversion efficiencies, defines also entries where mode need to be specified explicitly
    convBalEff_tab = matchSetParameter(anyM.report,convBal_tab,anyM.parameter[:effConv],anyM.sets,anyM.options.scale.compDig,:eff,false)

    # adds empty mode column if non-existing within efficiencies
    if !(:M in colnames(convBalEff_tab)) convBalEff_tab = IT.transform(convBalEff_tab,:M => fill(convert(Int32,0),length(convBalEff_tab))) end

    # gets alls modes that require a seperate conversion balance
    modeConv_arr = sort(unique(DB.select(convBalEff_tab,:M)))
    # removes mode column to perform correct join with dispach variables below
    convBalEffNoM_tab = table(DB.unique(DB.select(convBalEff_tab,DB.Not(All(:M,:eff)))))

    varInOut_dic = Dict{Symbol,IndexedTable}()

    # XXX writes grouped input and output related variables
    for type in (:in => (:use,:stIntOut), :out => (:gen,:stIntIn))
        var_tab = DB.merge(DB.join(anyM.variables[type[2][1]].data,addDummyCol(convBalEffNoM_tab); lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), rselect = DB.Not(All(:dummy)), how = :inner),
                                DB.join(anyM.variables[type[2][2]].data,addDummyCol(convBalEffNoM_tab); lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), rselect = DB.Not(All(:dummy)), how = :inner))
        # where input variables are mode dependant, but the conversion balance does not need to be specified for this mode, mode value is re-written to zero to groupy correctly
        modeVar_arr = unique(DB.select(var_tab,:M))
        if  modeVar_arr != modeConv_arr
            modeVar_dic = Dict(x => x in modeConv_arr ? x : 0 for x in modeVar_arr)
            varInOut_dic[type[1]] = DB.groupby(IT.transform(var_tab,:M => map(x -> modeVar_dic[x], DB.select(var_tab,:M))), joinKeyM_tup, usekey = false, select = :var) do x
                NamedTuple{(Symbol(type[1],:Var),)}(tuple(sum(x)))
            end
        else
            varInOut_dic[type[1]] = DB.groupby(var_tab, joinKeyM_tup, usekey = false, select = :var) do x
                NamedTuple{(Symbol(type[1],:Var),)}(tuple(sum(x)))
            end
        end
    end

    # XXX merge efficiencies and variables and creates constraint
    convBalInOutEff_tab = join(convBalEff_tab,join(varInOut_dic[:in],varInOut_dic[:out]; lkey = joinKeyM_tup, rkey =joinKeyM_tup, how = :inner); lkey = joinKeyM_tup, rkey = joinKeyM_tup, how = :inner)
    eqn_arr = DB.select(convBalInOutEff_tab,(:inVar,:outVar,:eff) => x -> @constraint(anyM.optModel, sum(x.inVar)*x.eff ==  sum(x.outVar)))

    anyM.constraints[:convBal] = CnsElement(:convBal, joinKey_tup, IT.transform(DB.select(convBalInOutEff_tab,DB.Not(All(:inVar, :outVar, :eff))),:eqn => eqn_arr))
    produceMessage(anyM.options,anyM.report, 3," - Created conversion balances")
end

# XXX create storage balance
function createConstraint!(name::Val{:stBal},anyM::anyModel)

    joinKey_tup = (:Ts_supDis, :Ts_inv, :Ts_dis, :R_dis, :C, :Te)
    joinKeyM_tup = (:Ts_supDis, :Ts_inv, :Ts_dis, :R_dis, :C, :Te, :M)
    sizeVar_tab = DB.rename(anyM.variables[:stSize].data,:var => :size)

    # <editor-fold desc="joins variable with current storage level (=stSize) to level of next periode"
    # get supordinate dispatch timesteps

    # creates dictionary that assigns last to first dispatch period within a supordinate dispatch timestep
    tsChildren_dic = Dict((x,y) => getChildren(x,anyM.sets[:Ts],false,y) for x in anyM.supDis.step, y in unique(anyM.sets[:Ts][DB.select(sizeVar_tab,:Ts_dis),:lvl]))
    lastFirstTs_dic = Dict(maximum(tsChildren_dic[z]) => minimum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
    lastTs_arr = collect(keys(lastFirstTs_dic))

    # filters next sizes and adds them to table
    nextTs_tab = DB.rename(IT.transform(sizeVar_tab,:Ts_nextDis => DB.select(anyM.variables[:stSize].data,:Ts_dis => x -> x in lastTs_arr ? lastFirstTs_dic[x] : x+1)),:size => :nextSize)

    sizeBothVar_tab = DB.reindex(DB.join(sizeVar_tab,nextTs_tab; lkey = joinKeyM_tup, rkey = (:Ts_supDis, :Ts_inv, :Ts_nextDis, :R_dis, :C, :Te, :M), how = :inner),joinKeyM_tup)
    # saves all carrier/mode combinations where size is mode specific
    modeCarSize_arr = convert(Array{Tuple{Int32,Int32},1},values.(sort(unique(DB.select(sizeBothVar_tab,(:C,:M))))))

    varInOut_dic = Dict{Symbol,IndexedTable}()
    sizeVarNoM_tab = table(DB.unique(DB.select(sizeVar_tab,DB.Not(All(:M,:size)))))
    # </editor-fold>

    # <editor-fold desc="adds variables for storage in and output to variables for storage level"
    for type in (:In,:Out)
        # find all relevant variables
        allVar_tab = DB.merge(DB.join(anyM.variables[Symbol(:stExt,type)].data,sizeVarNoM_tab; lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), how = :inner),
                                            DB.join(anyM.variables[Symbol(:stInt,type)].data,sizeVarNoM_tab; lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), how = :inner))

        # joins efficiency parameter and creates expression
        allVarEff_tab = matchSetParameter(anyM.report,allVar_tab,anyM.parameter[Symbol(:effSt,type)],anyM.sets,anyM.options.scale.compDig,:eff,false)

        allVarExpr_tab = sort(IT.transform(DB.select(allVarEff_tab,DB.Not(All(:eff,:var))),:var => type == :In ? DB.select(allVarEff_tab,(:eff,:var) => x -> x.eff*x.var)
                                                                                                               : DB.select(allVarEff_tab,(:eff,:var) => x -> x.var*round(1/x.eff,digits = anyM.options.scale.compDig))))
        # replaces mode value with zero where size is not mode controlled
        if !isempty(anyM.mapping[:modeCases])
            modeSizeInOut_dic = convert(Dict{Tuple{Int32,Int32},Int32}, Dict((x.C, x.M) => (x.C, x.M) in modeCarSize_arr ? x.M : 0 for x in unique(DB.select(allVar_tab,(:C,:M)))))
            allVarExpr_tab = IT.transform(DB.select(allVarExpr_tab,DB.Not(All(:M))),:M => map(x -> modeSizeInOut_dic[(x.C,x.M)],DB.select(allVarExpr_tab,(:C,:M))))
        end
        # groups variables to join with size variables next
        varInOut_dic[type] = IT.rename(DB.groupreduce((var = :var  => +,), allVarExpr_tab, joinKeyM_tup; select = (:var,)),:var => Symbol(:var,type))
    end

    # merges variables for input and output to size variables
    allVar_tab = joinMissing(varInOut_dic[:In],varInOut_dic[:Out], joinKeyM_tup, joinKeyM_tup, :outer, (0.0,0.0))
    sizeAllVarEff_tab = DB.join(table(rows(sizeBothVar_tab)),allVar_tab; lkey = joinKeyM_tup, rkey = joinKeyM_tup, how = :inner)
    # </editor-fold>

    # <editor-fold desc="adds additional parameters and create constraint"
    # adds discharge rate and inflows if they are defined
    disDef_boo = (:stDis) in keys(anyM.parameter)
    infDef_boo = (:stInflow) in keys(anyM.parameter)

    # adds discharge factor on nextSize Variable, since values are provided per hour factors need to be scaled accordingly
    if disDef_boo
        sizeAllVarDis_tab = joinMissing(sizeAllVarEff_tab,matchSetParameter(anyM.report,DB.select(sizeVar_tab,DB.Not(All(:size))),anyM.parameter[:stDis],anyM.sets,anyM.options.scale.compDig,:stDis,false), joinKeyM_tup, joinKeyM_tup, :left,(0.0,))
        sizeAllVarScale_tab = addScaling(IT.transform(sizeAllVarDis_tab,:scale => fill(1.0,length(sizeAllVarDis_tab))),:scale,anyM.sets[:Ts],anyM.supDis)
        sizeAllVarFac_tab = IT.transform(sizeAllVarScale_tab,:nextSize => DB.select(sizeAllVarScale_tab,(:nextSize, :stDis, :scale) => x -> x.nextSize * round(1/(1-x.stDis^x.scale),digits = anyM.options.scale.compDig)))
        sizeAllVarEff_tab = DB.select(sizeAllVarFac_tab,DB.Not(All(:stDis,:scale)))
    end

    # adds inflows parameter, since values are provided per hour factors need to be scaled accordingly
    if infDef_boo
        sizeAllVarInf_tab = joinMissing(sizeAllVarEff_tab,matchSetParameter(anyM.report,DB.select(sizeVar_tab,DB.Not(All(:size))),anyM.parameter[:stInflow],anyM.sets,anyM.options.scale.compDig,:stInf,false), joinKeyM_tup, joinKeyM_tup, :left,(0.0,))
        sizeAllVarEff_tab = addScaling(sizeAllVarInf_tab,:stInf,anyM.sets[:Ts],anyM.supDis) # adds scaling since inflows are provided in MW
    end

    # XXX create constraints and final object

    if infDef_boo
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut,:stInf)
                                                            => x -> @constraint(anyM.optModel, x.nextSize == x.size + x.stInf + sum(x.varIn) - sum(x.varOut)))
    else
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut)
                                                            => x -> @constraint(anyM.optModel, x.nextSize == x.size + sum(x.varIn) - sum(x.varOut)))
    end

    anyM.constraints[:stBal] = CnsElement(:stBal, joinKey_tup, IT.transform(DB.select(sizeAllVarEff_tab,joinKeyM_tup),:eqn => eqn_arr))
    # </editor-fold>
    produceMessage(anyM.options,anyM.report, 3," - Created storage balances")
end

# XXX creates capacity restrictions on dispatch variables
function createConstraint!(name::Val{:capaRestr},anyM::anyModel)

    # XXX capacity restrctions on conversion and storage capacites

    # adds a id column and filters restsrictions where actual capacity exists
    restrId_tab = IT.transform(anyM.mapping[:capaDispRestr],:id => convert(Array{Int32,1},collect(1:length(anyM.mapping[:capaDispRestr]))))
    restrIdConv_tab = DB.join(restrId_tab,table(DB.unique(DB.select(anyM.mapping[:capaConv],(:Ts_inv,:Te)))); lkey = :Te, rkey = :Te, rselect = :Ts_inv, how = :inner)
    restrIdSt_tab = DB.join(restrId_tab,table(DB.unique(DB.select(anyM.mapping[:capaSt],(:Ts_inv,:Te)))); lkey = :Te, rkey = :Te, rselect = :Ts_inv, how = :inner)

    # creates named tuple for every type of restriction to be created
    cnstrType_tup = ((type = :out, disVar = (:gen, :stIntIn), capaVar = :Conv), (type = :in, disVar = (:use,:stIntOut), capaVar = :Conv), (type = :stIn, disVar = (:stExtIn, :stIntIn), capaVar = :StIn),
                                                                (type = :stOut, disVar = (:stExtOut, :stIntOut), capaVar = :StOut), (type = :stSize,disVar = (:stSize,), capaVar = :StSize))


    [createRestr!(cnstr::NamedTuple,DB.filter(r -> r.cnstrType == cnstr.type, cnstr.type in (:in,:out) ? restrIdConv_tab : restrIdSt_tab)::IndexedTable,anyM) for cnstr in cnstrType_tup]

    # XXX capacity restrctions for exchange capacities

    # joins capacity variable to dispatch variable
    if :exchange in keys(anyM.variables)
        excDisVar_tab = DB.rename(anyM.variables[:exchange].data,:var => :excDisp)

        excVarFull_tab =  DB.reindex(DB.merge([DB.select(DB.join(excDisVar_tab,anyM.variables[:capaExc].data; lkey = (:Ts_supDis, x, :C), rkey = (:Ts_supDis, :R_a, :C), rselect = DB.Not(All(:R_b)) ,how = :inner),DB.Not(All(:R_a)))
                                                                                                                                                                    for x in (:R_from,:R_to)]...),(:Ts_dis,:R_from,:R_to,:C))
        # adds availability of exchange capacity (matchSetParameter has to be called twice, because both "sides", from and to need to be checked)
        excAvaOnce_tab = matchSetParameter(anyM.report,DB.rename(excVarFull_tab,:R_to => :R_a,:R_from => :R_b),anyM.parameter[:avaExc],anyM.sets,anyM.options.scale.compDig,:ava,false)
        excAva_tab = DB.merge(DB.rename(excAvaOnce_tab,:R_b => :R_from,:R_a => :R_to),DB.rename(excAvaOnce_tab,:R_a => :R_from,:R_b => :R_to))

        # since exchange capacities are provided in MW, but energy in MWh, scaling factor is added to exchange capacity
        excScaled_tab = addScaling(excAva_tab,:var,anyM.sets[:Ts],anyM.supDis)

        excVar_tab = DB.reindex(DB.rename(excScaled_tab,:var => :excCapa),(:Ts_dis, :R_from, :R_to, :C))

        eqn_arr = DB.select(excVar_tab,(:excDisp, :excCapa, :ava) => x -> @constraint(anyM.optModel, x.excDisp <=  x.excCapa*x.ava))
        anyM.constraints[:capaRestrExc] = CnsElement(:capaRestrExc, (:Ts_dis,:R_from,:R_tom,:C),IT.transform(DB.select(excVar_tab,(:Ts_dis,:R_from,:R_to,:C)),:eqn => eqn_arr))
        produceMessage(anyM.options,anyM.report, 3," - Created all capacity restrictions for exchange")
    end
    produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions")
end

# XXX sets capacity limits for buy and sell capacity, if any are defined
function createConstraint!(name::Val{:limitTrd},anyM::anyModel)
    for type in (:Buy, :Sell)
        if Symbol(:trd,type,:Cap) in keys(anyM.parameter)
            eqn_tab = matchSetParameter(anyM.report,anyM.variables[Symbol(:trade,type)].data, anyM.parameter[Symbol(:trd,type,:Cap)], anyM.sets, anyM.options.scale.compDig, :cap, false)

            # adds scaling to trade capacities
            eqnScaled_tab = addScaling(eqn_tab,:cap,anyM.sets[:Ts],anyM.supDis)

            eqnFull_tab = IT.transform(DB.select(eqnScaled_tab,DB.Not(All(:cap,:var))),:eqn => DB.select(eqn_tab,(:var,:cap) => x -> @constraint(anyM.optModel, x.var <=  x.cap)))
            anyM.constraints[Symbol(:limit,type)] = CnsElement(Symbol(:limit,type),(:Ts_dis, :R_dis, :C, :id),eqnFull_tab)
            produceMessage(anyM.options,anyM.report, 3," - Created constraints defining limits on $(type) capacity")
        end
    end
end

# XXX controls ratios of energy in- and output of conversion according to ratio parameters,
function createConstraint!(name::Val{:ratioEner},anyM::anyModel)

    joinKey1_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M)
    joinKey2_tup = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M)

    ratioEnerConst_dic = Dict{Symbol,IndexedTable}()
    # collects all tables for equations
    for type in (:Use, :Gen), limit in (:Up, :Low, :Fix)
        ratioName_sym = Symbol(:ratioEner,type,limit)

        if !(ratioName_sym in keys(anyM.parameter)) continue end
        # obtain variable name and parameter data
        varName_sym = Symbol(lowercase(string(type)))
        allPar_data = anyM.parameter[ratioName_sym].data

        # joins parameter data with ratio controlled variable and all variables
        parVar_data = IT.rename(DB.join(allPar_data, anyM.variables[varName_sym].data; lkey = joinKey1_tup, rkey = joinKey1_tup, how = :inner),:var => :varRatio)
        allVar_data = DB.join(DB.select(parVar_data, DB.Not(All(:val,:varRatio,:Ts_supDis))), anyM.variables[varName_sym].data; lkey = joinKey2_tup, rkey = joinKey2_tup, how = :left)

        # groups and creates equations object
        allVarGrp_tab = JuliaDB.groupby(allVar_data, joinKey2_tup, usekey = false; select = :var) do y
            NamedTuple{(:var,)}(tuple(sum(y),))
        end

        ratioEnerConst_dic[ratioName_sym] = DB.join(allVarGrp_tab, parVar_data ; lkey = joinKey2_tup, rkey = joinKey2_tup, how = :inner)
    end

    # creats constraints for each entry in dictionary
    for limit in keys(ratioEnerConst_dic)

        limitData_tab = ratioEnerConst_dic[limit]
        if occursin("Fix",string(limit))
            ratioEqn_tab = IT.transform(DB.select(limitData_tab,DB.Not(All(:varRatio,:var,:val))),:eqn => DB.select(limitData_tab,(:varRatio,:var,:val) => x -> @constraint(anyM.optModel, x.var * x.val == x.varRatio)))
        elseif occursin("Low",string(limit))
            ratioEqn_tab = IT.transform(DB.select(limitData_tab,DB.Not(All(:varRatio,:var,:val))),:eqn => DB.select(limitData_tab,(:varRatio,:var,:val) => x -> @constraint(anyM.optModel, x.var * x.val <= x.varRatio)))
        else
            ratioEqn_tab = IT.transform(DB.select(limitData_tab,DB.Not(All(:varRatio,:var,:val))),:eqn => DB.select(limitData_tab,(:varRatio,:var,:val) => x -> @constraint(anyM.optModel, x.var * x.val >= x.varRatio)))
        end
        produceMessage(anyM.options,anyM.report, 3," - Created constraints to control energy ratio $limit")
        anyM.constraints[limit] = CnsElement(limit, joinKey1_tup, DB.reindex(ratioEqn_tab,joinKey1_tup))
    end
    produceMessage(anyM.options,anyM.report, 2," - Created all constraints to control energy ratios")
end

# <editor-fold desc="collection of other subfunctions"

# XXX writes table with all information regarding limit constraints for variables in input table
function collectLimitConstraints(varData_tab::IndexedTable, limName_sym::Symbol, stockTech_arr::Array{Int32,1}, anyM::anyModel)
    # gets dimensions of search variable
    varDim_tup = pkeynames(varData_tab)
    investLimit_boo = any(occursin.(("inv","capa"),string(limName_sym)))
    actualVar_boo = limName_sym in keys(anyM.variables)

    # <editor-fold desc="match limit parameters with variables"
    # returns limits on existing variables and a table with all parameter limits provided
    limExtVar_tab, allLimPar_tab = matchLimitParameter(varData_tab, limName_sym, anyM)

    # defines grouping tuple for aggregation call
    if investLimit_boo
        grpInter_tup = tuple()
    else
        grpInter_tup = occursin("trade",string(limName_sym)) ? ((:Ts_dis, :R_dis), (:id, :C)) : limName_sym == :exchange ? ((:R_from, :R_to), (:Ts_dis, :C)) : ((:C, :Ts_dis), (:Ts_inv, :Te, :M, :R_dis) => (:Ts_inv, :Te, :M))
    end
    # gets limits on aggregation of variables, special case for trade due to "id" column
    if occursin("trade",string(limName_sym))
        # creates a dummy tree dataframe for ids so aggregation can be performed correctly
        id_arr = convert(Array{Int32,1},unique(DB.select(varData_tab,:id)))
        topNodeId_df = DataFrame(idx = Int32[0], val = String["topNode"], lvl = Int32[0], pare = Int32[0], sub_id = Int32[1], children = [id_arr])
        restNodesId_arr = [(idx = convert(Int32,cnt), val = string(id), lvl = convert(Int32,1), pare = convert(Int32,0), sub_id = convert(Int32,cnt),children = Int32[]) for (cnt,id) in enumerate(id_arr)]
        dmySet_dic = Dict(:id => append!(topNodeId_df,DataFrame(restNodesId_arr)))

        agg_dic, limAggVar_tab = aggregateSetTable(varData_tab, varDim_tup, grpInter_tup, merge(dmySet_dic,anyM.sets), allLimPar_tab)
    else
        agg_dic, limAggVar_tab = aggregateSetTable(varData_tab, varDim_tup, grpInter_tup, anyM.sets, allLimPar_tab)
    end

    # if the limit name directly related to a variable (for example useUp, but not emissionUp) aggregations are saved for later writing
    if actualVar_boo anyM.aggVar[limName_sym] = agg_dic end

    if all(isnothing.((limExtVar_tab, limAggVar_tab))) return nothing end

    # merges limits on existing and aggregated variables to one table
    if isnothing(limAggVar_tab) || isempty(limAggVar_tab) || limName_sym == :gen
        allLim_tab = limExtVar_tab
    elseif isnothing(limExtVar_tab)
        var_arr = DB.select(varData_tab, :var)
        allLim_tab = IT.transform(DB.select(limAggVar_tab,DB.Not(All(:aggVar))), :var => map(x -> sum(var_arr[collect(x)]), DB.select(limAggVar_tab,:aggVar)))
    else
        var_arr = DB.select(varData_tab, :var)
        limAggVar_tab = IT.transform(DB.select(limAggVar_tab,DB.Not(All(:aggVar))), :var => map(x -> sum(var_arr[collect(x)]), DB.select(limAggVar_tab,:aggVar)))

        # adds columns not appearing in limExtVar_tab, but in limAggVar_tab, to limExtVar_tab
        for addCol in setdiff(colnames(limAggVar_tab), colnames(limExtVar_tab)) limExtVar_tab = IT.transform(limExtVar_tab,addCol => fill(missing,length(limExtVar_tab))) end
        # merges limits on existing and aggregated variables to one table
        allLim_tab = merge(limExtVar_tab, limAggVar_tab)
    end

    if isnothing(allLim_tab) return allLim_tab end
    # </editor-fold>

    # <editor-fold desc="checks limits for errors"

    # excludes limits on stock technologies and reports this (only for capacity and investment for conversion and storage)
    if investLimit_boo && :Te in varDim_tup
        for i in intersect(stockTech_arr,DB.select(allLim_tab,:Te))
            push!(anyM.report,(2,:limit,limName_sym,"$(createFullString(i,anyM.sets[:Te])) is a technology without investment, but limits were provided, these values were ignored"))
        end
        allLim_tab = DB.filter(r -> !(r.Te in stockTech_arr),allLim_tab)
    end

    # reports on capacity constraints that limit the sum of capacity over several years
    if occursin("capa",string(limName_sym)) && 0 in unique(DB.select(allLim_tab,:Ts_supDis))
        for te in unique(DB.select(filter(r -> r.Ts_supDis == 0,varLim_tab),:Te))
            push!(anyM.report,(2,:limit,limName_sym,"for an $(createFullString(te,anyM.sets[:Te])) a capacity limit was provided, which time dimension is either not specified at all or specified above the level of investment decisions, in this case the limit applies to the total sum of capacities for the entire time, this might be unintended and can be fixed by adding temporal column with keyword all (for example not the maximum installable capacity of PV in each single year is limited, but the overall sum of capacity installed among years)"))
        end
    end
    # </editor-fold>

    return allLim_tab
end

# XXX returns limit parameter that could directly be related to input variables and a table of all other limit parameters
function matchLimitParameter(varData_tab::IndexedTable, limName_sym::Symbol, anyM::anyModel)

    # <editor-fold desc="collects matching limit parameters"
    # initializes variables
    defPar_tup = tuple(collect(keys(anyM.parameter))...)

    matchedPar_dic = Dict{Symbol,IndexedTable}()
    allPar_dic = Dict{Symbol,IndexedTable}()

    search_tab = varData_tab
    searchCol_tup = pkeynames(varData_tab)

    # searches for fixed, upper and lower and saves matched parameter as well as all parameter in dictionary
    for consType in (:Fix,:Up,:Low)
        searchPar_sym = Symbol(limName_sym,consType)
        if searchPar_sym in defPar_tup
            allPar_tab = DB.rename(anyM.parameter[searchPar_sym].data, :val => consType)
            matchedPar_tab = matchSetParameter(anyM.report, search_tab, anyM.parameter[searchPar_sym], anyM.sets, anyM.options.scale.compDig)
            if !isempty(matchedPar_tab)
                matchedPar_dic[consType] = DB.rename(DB.join(addDummyCol(search_tab),matchedPar_tab; lkey = searchCol_tup, rkey = searchCol_tup, lselect = searchCol_tup, rselect = :val, how = :inner), :val => consType)
                allPar_dic[consType] = DB.join(allPar_tab, matchedPar_dic[consType]; lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
            else
                allPar_dic[consType] = allPar_tab
            end
        end
    end

    # merges both tables for matched and all parameter
    matchedPar_tab = mergeDicTable(matchedPar_dic)
    allPar_tab = mergeDicTable(allPar_dic)

    if all(isnothing.((matchedPar_tab,allPar_tab))) return nothing, nothing end

    fullPar_tab = isnothing(matchedPar_tab) ? allPar_tab : DB.join(matchedPar_tab, allPar_tab; lkey = searchCol_tup, rkey = searchCol_tup, how = :outer)
    allTypes_arr = collect(colnames(fullPar_tab))

    # </editor-fold>

    # <editor-fold desc="checks for contradicting or poorly defined entries"
    # checks for conflicts between lower and upper limits
    if :Low in allTypes_arr || :Up in allTypes_arr
        if :Low in allTypes_arr && :Up in allTypes_arr
            errorContra_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : x[1] > x[2]))]
            errorSame_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : x[1] == x[2]))]
            errorUpperZero_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : (x[1] == 0) || (x[2] == 0)))]

            if !isempty(errorContra_tab)
                uniContra_arr = unique(DB.select(errorContra_tab,:Te))
                for i in uniContra_arr
                    push!(anyM.report,(2,:limit,limName_sym,"$(createFullString(i,anyM.sets[:Te])) got contradicting values for upper and lower limit, these values were ignored"))
                end
                fullPar_tab = DB.join(fullPar_tab, errorContra_tab;lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
            end

            if !isempty(errorSame_tab)
                uniSame_arr = unique(DB.select(errorContra_tab,:Te))
                for i in uniSame_arr
                    push!(anyM.report,(2,:limit,limName_sym,"$(createFullString(i,anyM.sets[:Te])) same values for upper and lower limits detected, use fix instead"))
                end
            end
        else
            errorUpperZero_tab = fullPar_tab[findall(DB.select(fullPar_tab,intersect((:Low,:Up),allTypes_arr)[1] => x -> ismissing(x) ? false : (x == 0)))]
        end

        if !isempty(errorUpperZero_tab) && limName_sym != :emissionUp
            uniUpperZero_arr = unique(DB.select(errorUpperZero_tab,:Te))
            for i in uniUpperZero_arr
                push!(anyM.report,(2,:limit,limName_sym,"$(createFullString(i,anyM.sets[:Te])) got upper or/and lower limit of zero, these values were ignored"))
            end
            fullPar_tab = DB.join(fullPar_tab, errorUpperZero_tab; lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
        end
    end

    # checks if additional limits were set if value is already fixed
    if :Fix in allTypes_arr && (:Low in allTypes_arr || :Up in allTypes_arr)
        if :Low in allTypes_arr && :Up in allTypes_arr
            errorFixLimit_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Fix,:Low,:Up) => x -> !ismissing(x[1]) && (!ismissing(x[2]) || !ismissing(x[3]))))]
        elseif :Low in allTypes_arr
            errorFixLimit_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Fix,:Low) => x -> !ismissing(x[1]) && !ismissing(x[2])))]
        else
            errorFixLimit_tab = fullPar_tab[findall(DB.select(fullPar_tab,(:Fix,:Up) => x -> !ismissing(x[1]) && !ismissing(x[2])))]
        end

        if !isempty(errorUpperZero_tab)
            uniFixLimit_arr = unique(DB.select(errorFixLimit_tab,:Te))
            for i in uniFixLimit_arr
                push!(anyM.report,(2,:limit,limName_sym,"$(createFullString(i,anyM.sets[:Te])) got fixed value and limits (upper/lower) set, limits were ignored"))
            end
            fullPar_tab = DB.join(fullPar_tab, uniFixLimit_arr; lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
        end
    end
    # </editor-fold>

    if isnothing(matchedPar_tab)
        return nothing, allPar_tab
    else
        return DB.join(matchedPar_tab, varData_tab; lkey = searchCol_tup, rkey = searchCol_tup, how = :inner), allPar_tab
    end
end

# XXX creates capacity constraint for specific type (:out,:in,:stOut,...)
function createRestr!(cnstr_ntup::NamedTuple,cnstr_tab::IndexedTable,anyM::anyModel)

    # <editor-fold desc="initializes dispatch and capacity variables"
    # checks if some kind de-commissioning is enabled and uses actually commissioned capacities in that case
    capaVar_sym = anyM.options.decomm != :none ? :capaComm : :capa

    # create tuples for joining capacity and dispatch variables
    joinCapa_tup = (:Ts_supDis,:Ts_inv,:R_inv,:Te)
    joinDis_tup = (:Ts_dis,:Ts_inv,:R_dis,:C,:Te)

    # adds investment regions and supordinate dispatch timesteps to obtain full investment table
    rLvlInv_arr = map(x -> x.R,DB.select(cnstr_tab,:Te => z ->DB.select(anyM.mapping[:TechInfo],:invLvl)[findall(DB.select(anyM.mapping[:TechInfo],:Te) .== z)[1]]))
    cnstrCapa_tab = expandSetColumns(IT.transform(DB.select(cnstr_tab,DB.Not(All(:cnstrType))),:R_inv => rLvlInv_arr),(:R_inv,),anyM.sets)
    capaDim_tab = DB.rename(DB.flatten(IT.transform(cnstrCapa_tab,:Ts_supDis => fill(anyM.supDis.step,length(cnstrCapa_tab))),:Ts_supDis),:car => :C)

    # joins capacity variables
    capaVar_tab = DB.rename(DB.join(capaDim_tab,table(rows(anyM.variables[Symbol(capaVar_sym,cnstr_ntup.capaVar)].data)); lkey = joinCapa_tup, rkey = joinCapa_tup, how = :inner),:var => :capaVar)

    # adds dispatch regions to table and groups to obtain capacity variables
    lvlRDis_dic = Dict((x.lvlR,x.R_inv) => anyM.sets[:R][x.R_inv,:lvl] == x.lvlR ? x.R_inv : convert(Int32,getHeritanceLine(x.R_inv,anyM.sets[:R],x.lvlR)) for x in DB.unique(DB.select(capaVar_tab,(:lvlR,:R_inv))))
    capaVarR_tab = IT.transform(DB.select(capaVar_tab,DB.Not(All(:lvlR))),:R_dis => DB.select(capaVar_tab,(:lvlR,:R_inv) => x -> lvlRDis_dic[(x.lvlR,x.R_inv)]))
    capaVarGrp_tab = DB.groupby(capaVarR_tab,(:Ts_supDis,:Ts_inv,:R_dis,:Te,:id,:lvlTs,:C), usekey = false, select = :capaVar) do x
        NamedTuple{(:capaVar,)}(tuple(sum(x)))
    end

    # since all capacities except for storage size are provided in MW, but energy in MWh, scaling factor is added to capacity variables
    if cnstr_ntup.type != :StSize
        capaVarGrp_tab = IT.transform(capaVarGrp_tab,:scale => DB.select(capaVarGrp_tab,(:capaVar,:Ts_supDis,:lvlTs) => x -> anyM.supDis.dic[(x.Ts_supDis,x.lvlTs)]))
    end

    # adds dispatch timesteps to table
    lvlTsStep_dic = Dict((x,y) => x == anyM.supDis.lvl ? [y] : getChildren(y,anyM.sets[:Ts],false,x) for x in unique(DB.select(capaVarGrp_tab,(:lvlTs))), y in anyM.supDis.step)
    disDim_tab = DB.flatten(DB.flatten(IT.transform(DB.select(capaVarGrp_tab,DB.Not(All(:lvlTs,:capaVar,:scale))),:Ts_dis => DB.select(capaVarGrp_tab,(:lvlTs,:Ts_supDis) => x -> lvlTsStep_dic[(x.lvlTs,x.Ts_supDis)])),:Ts_dis),:C)

    # joins dispatach variables depending on how many types of dispatch variables are to be joined
    if length(cnstr_ntup.disVar) == 1
        disVar_tab = DB.rename(DB.join(disDim_tab,anyM.variables[cnstr_ntup.disVar[1]].data; lkey = joinDis_tup, rkey = joinDis_tup, how = :inner),:var => :disVar)
    else
        mergeDis_tab = DB.merge(anyM.variables[cnstr_ntup.disVar[1]].data,anyM.variables[cnstr_ntup.disVar[2]].data)
        disVar_tab = DB.rename(DB.join(disDim_tab,mergeDis_tab; lkey = joinDis_tup, rkey = joinDis_tup, how = :inner),:var => :disVar)
    end
    # </editor-fold>

    # <editor-fold desc="joins parameters to dispatch variables"
    # determines final dimension and name of availability parameter for further use
    if cnstr_ntup.type in (:in,:out)
        parDim_tup = (:Ts_dis,:Ts_inv,:R_dis,:Te,:M); avaName_sym = :avaConv
    else
        parDim_tup = (:Ts_dis,:Ts_inv,:R_dis,:C,:Te,:M); avaName_sym = Symbol(:ava,uppercase(string(cnstr_ntup.type)[1]),string(cnstr_ntup.type)[2:end])
    end

    # adds availability parameter
    disVar2_tab = reindex(disVar_tab,parDim_tup)
    varAva_tab = IT.transform(disVar2_tab,:ava => DB.select(matchSetParameter(anyM.report,DB.select(disVar2_tab,parDim_tup),anyM.parameter[avaName_sym],anyM.sets,anyM.options.scale.compDig),:val))

    # adds efficiency parameter in case capacity constraints addresses generation
    if cnstr_ntup.type == :out
        varAva_tab = IT.transform(varAva_tab,:ava => DB.select(varAva_tab,:ava).*DB.select(matchSetParameter(anyM.report,DB.select(varAva_tab,parDim_tup),anyM.parameter[:effConv],anyM.sets,anyM.options.scale.compDig),:val))
    end
    # </editor-fold>


    # <editor-fold desc="groups dispatch varialbles and joins capacity variables"
    # groups table to obtain dispatch variables
    grpDim_tup = tuple(filter(x -> x != :M,vcat(:Ts_supDis,parDim_tup...))...)
    grpDim2_tup = tuple(filter(x -> !(x in (:M,:Ts_dis)),vcat(:Ts_supDis,parDim_tup...))...)
    # smallest availability among dispatch variables is obtained too expand the the final capacity constraint to avoid numerical trouble
    disVarGrp1_tab = DB.groupby(varAva_tab,tuple(vcat(grpDim_tup...,:id)...), usekey = false, select = (:disVar,:ava)) do x
        NamedTuple{(:disVar,:ava,:maxAva)}(tuple(Array(x.disVar),Array(x.ava),maximum(x.ava)))
    end
    rSelNot_tup = cnstr_ntup.type in (:in,:out) ? (:C,:lvlTs,:id) : (:lvlTs,:id)

    # joins capacity variables
    allVarFinal_tab = DB.join(disVarGrp1_tab,capaVarGrp_tab; lkey = grpDim2_tup, rkey = grpDim2_tup, lselect = DB.Not(All(:id)), rselect = DB.Not(All(rSelNot_tup)), how = :inner)
    allVarFinalRe_tab = DB.reindex(allVarFinal_tab,grpDim_tup)

    dispVarComp_tab = IT.transform(DB.select(allVarFinalRe_tab,DB.Not(All(:disVar,:ava,:maxAva,:capaVar,:scale))),
                                                    :disp => DB.select(allVarFinalRe_tab,(:disVar,:ava,:maxAva) => x -> dot(x.disVar,round.((1/x.ava), digits = anyM.options.scale.compDig))),
                                                    :capa => DB.select(allVarFinalRe_tab,(:capaVar,:scale,:maxAva) => x -> x.capaVar*round(x.scale, digits = anyM.options.scale.compDig)))

    # create final constraints and write object
    fullData_tab = IT.transform(DB.select(dispVarComp_tab,grpDim_tup), :eqn => DB.select(dispVarComp_tab,(:disp,:capa) => x -> @constraint(anyM.optModel, x.disp <= x.capa)))
    eqnNam_sym = Symbol(:capaRestr,uppercase(String(cnstr_ntup.type)[1]),String(cnstr_ntup.type)[2:end])
    anyM.constraints[eqnNam_sym] = CnsElement(eqnNam_sym, tuple(filter(x -> x != :M,collect(parDim_tup))...),fullData_tab)
    produceMessage(anyM.options,anyM.report, 3," - Created all capacity restrictions for $(cnstr_ntup.type)")
    # </editor-fold>
end

# </editor-fold>
