# XXX sets the objective function according to the arguments provided in obj_dic
function setObjective!(obj_dic::Union{Dict{Symbol,Float64},Symbol},anyM::anyModel,minimize::Bool=true)

    # XXX converts input into dictionary, if only a symbol was provided, if :none keyword was provided returns a dummy objective function
    if typeof(obj_dic) == Symbol
        if obj_dic == :none @objective(anyM.optModel, Min, 1); return end
        obj_dic = Dict(obj_dic => 1.0)
    end

    # XXX create empty variables table for objective variables
    anyM.variables[:objVar] = VarElement(:objVar,tuple(),table(Symbol[], Symbol[], VariableRef[]; names = (:group,:name,:var)))
    anyM.constraints[:objEqn] = CnsElement(:objEqn,tuple(),table(Symbol[], Symbol[], ConstraintRef[]; names = (:group,:name,:eqn)))


    # XXX create variables and constraints required for specified objectives
    for objGrp in keys(obj_dic)
        createObjective!(objGrp,anyM)
    end

    # XXX sets overall objective variable according to weights provided in dictionary
    if minimize
        @objective(anyM.optModel, Min, sum(map(x -> sum(DB.select(filter(r -> r.group == x,anyM.variables[:objVar].data),:var))*obj_dic[x], collect(keys(obj_dic)))))
    else
        @objective(anyM.optModel, Max, sum(map(x -> sum(DB.select(filter(r -> r.group == x,anyM.variables[:objVar].data),:var))*obj_dic[x], collect(keys(obj_dic)))))
    end

    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for cost objective and set the objective accordingly")
end

createObjective!(objGrp::Symbol, anyM::anyModel) = createObjective!(Val{objGrp}(), anyM::anyModel)

# XXX functions to define and create variables relating to cost objective
function createObjective!(objGrp::Val{:costs},anyM::anyModel)
    exprCost_dic = Dict{Symbol,Array{GenericAffExpr{Float64,VariableRef},1}}()

    # <editor-fold desc="compute discount factors"
    # gets all relevant supordinate dispatch timesteps and regions and creates a table of them
    allRInv_arr = vcat(map(x -> anyM.sets[:R][anyM.sets[:R][:,:lvl] .== x,:idx],unique(DB.select(anyM.mapping[:C_lvl],:lvlRInv)))...)
    tsR_tab = DB.flatten(DB.flatten(table([anyM.supDis.step],[allRInv_arr];names = (:Ts_supDis,:R_inv)),:Ts_supDis),:R_inv)

    # looks up  created table with discount rates and computes full discount factors from that
    rateVal_tab = reindex(matchSetParameter(anyM.report,tsR_tab,anyM.parameter[:rateDisc],anyM.sets,anyM.options.digits.comp,:itrRate),(:Ts_supDis,:R_inv))
    val_arr = map(allRInv_arr) do y
        discFc_arr = vcat(1,(1 ./ (1 .+ DB.select(DB.filter(r -> r.R_inv == y,rateVal_tab),:itrRate)[2:end])).^anyM.options.shortInvest)
        return map(x -> prod(discFc_arr[1:x]), 1:length(discFc_arr))
    end

    anyM.parameter[:discFac] = ParElement(DataFrame(Ts = repeat(collect(anyM.supDis.step),length(allRInv_arr)), R = sort(repeat(allRInv_arr,length(anyM.supDis.step))), val = vcat(val_arr...)),
                                                                                                        (name = :discFac, dim = (:Ts_supDis, :R_inv), default_val = nothing, inherit = (:Ts_supDis => :up, :R_inv => :up), grp = :invest))
    # </editor-fold>

    # <editor-fold desc="add array for investment costs expression to dictionary"
    costInvExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    for (idx,capaItr) in enumerate((:Conv,:StIn,:StOut,:StSize,:Exc))
        if Symbol(:costInv,capaItr) in keys(anyM.parameter)
            invVar_tab = anyM.variables[Symbol(:inv,capaItr)].data
            dim_tup = tuple(filter(x -> x != :var, collect(colnames(invVar_tab)))...)

            tsYear_dic = Dict(zip(anyM.supDis.step,convert(Array{Int32,1},collect(0:anyM.options.shortInvest:(length(anyM.supDis.step)-1)*anyM.options.shortInvest))))

            # adds lifetime (if an economic lifetime is defined, this is used)
            if Symbol(:lifeEco,capaItr) in keys(anyM.parameter)
                lftm_tab = matchSetParameter(anyM.report, invVar_tab, anyM.parameter[Symbol(:lifeEco,capaItr)], anyM.sets, anyM.options.digits.comp, :life)
                if length(lftm_tab) < length(invVar_tab)
                    newSearch_tab = DB.join(invVar_tab,lftm_tab; lkey = dim_tup, rkey = dim_tup, how = :anti)
                    lftm_tab = DB.merge(matchSetParameter(anyM.report, newSearch_tab, anyM.parameter[Symbol(:life,capaItr)], anyM.sets, anyM.options.digits.comp, :life), lftm_tab)
                end
            else
                lftm_tab = matchSetParameter(anyM.report, invVar_tab, anyM.parameter[Symbol(:life,capaItr)], anyM.sets, anyM.options.digits.comp, :life)
            end

            # adds investment cost and respective interest rate
            invCost_tab = matchSetParameter(anyM.report, lftm_tab, anyM.parameter[Symbol(:costInv,capaItr)], anyM.sets, anyM.options.digits.comp, :invCost)
            itrRate_tab = matchSetParameter(anyM.report, invCost_tab, anyM.parameter[Symbol(:rateInv,capaItr)], anyM.sets, anyM.options.digits.comp, :itrRate)

            # compute annuity costs and periods where it is payed
            annCost_arr = DB.select(itrRate_tab,:invCost) .* ((1 .+ DB.select(itrRate_tab,:itrRate)) .^ DB.select(itrRate_tab,:life) .* DB.select(itrRate_tab,:itrRate)) ./ ((1 .+ DB.select(itrRate_tab,:itrRate)) .^ DB.select(itrRate_tab,:life) .- 1)
            annCost_tab = IT.transform(DB.select(itrRate_tab,DB.Not(All(:itrRate,:invCost))),:costAnn => annCost_arr)
            invTsup_arr = DB.select(annCost_tab,(:Ts_inv,:life) => x -> filter(y -> (tsYear_dic[y] > tsYear_dic[x.Ts_inv]-anyM.options.shortInvest) && (tsYear_dic[y] <= tsYear_dic[x.Ts_inv]+x.life),collect(anyM.supDis.step)))
            annCost2_tab = DB.flatten(IT.transform(DB.select(annCost_tab,DB.Not(All(:life))),:Ts_supDis => invTsup_arr),:Ts_supDis)

            # joins discount factors, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = DB.rename(DB.join(annCost2_tab,anyM.parameter[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFac)
                push!(costInvExpr_arr,dot(DB.select(discFac_tab,:var), round.(DB.select(discFac_tab,:disFac) .* DB.select(discFac_tab,:costAnn), digits = anyM.options.digits.comp)))
            else
                discFac1_tab = DB.rename(DB.join(annCost2_tab,anyM.parameter[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacA)
                discFac_tab = DB.rename(DB.join(discFac1_tab,anyM.parameter[:discFac].data; lkey = (:R_b, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacB)
                push!(costInvExpr_arr,dot(DB.select(discFac_tab,:var), round.((0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:oprCost), digits = anyM.options.digits.comp)))
            end
        end
    end
    if !isempty(costInvExpr_arr) exprCost_dic[:totCostInv] = costInvExpr_arr end
    produceMessage(anyM.options,anyM.report, 3," - Created variables and constraints for investment costs")
    # </editor-fold>

    # <editor-fold desc="add array for operation costs expression to dictionary"
    capaTyp_sym =  anyM.options.decomm != :none ? :capaComm : :capa
    costOprExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    for capaItr in (:Conv,:StIn,:StOut,:StSize,:Exc)
        if Symbol(:costOpr,capaItr) in keys(anyM.parameter)
            # matches variables with cost parameter
            oprCost_tab = matchSetParameter(anyM.report, anyM.variables[Symbol(capaTyp_sym,capaItr)].data, anyM.parameter[Symbol(:costOpr,capaItr)], anyM.sets, anyM.options.digits.comp, :oprCost)
            # joins discount factors, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = DB.rename(DB.join(oprCost_tab,anyM.parameter[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFac)
                push!(costOprExpr_arr, dot(DB.select(discFac_tab,:var), round.(DB.select(discFac_tab,:disFac) .* DB.select(discFac_tab,:oprCost) , digits = anyM.options.digits.comp)))
            else
                discFac1_tab = DB.rename(DB.join(oprCost_tab,anyM.parameter[:discFac].data; lkey = (:R_a, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacA)
                discFac_tab = DB.rename(DB.join(discFac1_tab,anyM.parameter[:discFac].data; lkey = (:R_b, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacB)
                push!(costOprExpr_arr, dot(DB.select(discFac_tab,:var), round.((0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:oprCost) , digits = anyM.options.digits.comp)))
            end
        end
    end

    if !isempty(costOprExpr_arr) exprCost_dic[:totCostOpr] = costOprExpr_arr end
    produceMessage(anyM.options,anyM.report, 3," - Created variables and constraints for operational costs")
    # </editor-fold>

    # <editor-fold desc="add array for variable costs expression to dictionary"
    costGenExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()
    for capaItr in (:Use,:Gen,:StIn,:StOut,:StSize,:Exc)
        if Symbol(:costVar,capaItr) in keys(anyM.parameter)
            varNam_sym = Symbol(lowercase(String(capaItr)[1]),String(capaItr)[2:end])
            varDim_tup = tuple(filter(x -> !(x in (:Ts_supDis, :var)), collect(colnames(anyM.variables[varNam_sym].data)))...)

            # XXX check if variable cost can be assigned to aggregated variables first
            # changes inheritance rules to assign a value to the upper node, if it is the same for all lower nodes
            anyM.parameter[Symbol(:costVar,capaItr)].inherit = (:Ts_inv => :uni_full, :Ts_dis => :uni_full, :R_dis => :uni_full, :C  => :uni_full, :Te => :uni_full)
            if Symbol(:agg,capaItr) in keys(anyM.constraints)
                # tries to assign parameters on variables that are defined via aggregation (dimensions of aggregation constraint = dimensions of aggregated variables)
                aggVar_tab = matchSetParameter(anyM.report, DB.select(anyM.constraints[Symbol(:agg,capaItr)].data,DB.Not(All(:eqn))), anyM.parameter[Symbol(:costVar,capaItr)], anyM.sets, anyM.options.digits.comp, :costVar)
                # finally joins variables and paramters in cases where assignment to aggregated variables was successfull
                aggFull_tab = DB.join(aggVar_tab,anyM.variables[varNam_sym].data; lkey = varDim_tup, rkey = varDim_tup, how = :inner)

                # XXX removes variables that wer included via the aggregation from further analysis
                if !(isempty(aggVar_tab))
                    # gets all rows of aggregation table with variables, that variable costs were successfully assigned to
                    varLen_arr = collect(1:length(anyM.variables[varNam_sym].data))
                    varId_tab = IT.transform(anyM.variables[varNam_sym].data,:id => varLen_arr)
                    aggRow_arr = DB.select(DB.join(aggVar_tab,varId_tab; lkey = varDim_tup, rkey = varDim_tup, how = :inner),:id)
                    # use aggregation dictionary to get actual rows of variables in the variables table, that are covered via assiging variables costs to aggregated variables
                    varRowViaAgg_arr = map(x -> anyM.aggVar[Symbol(lowercase(string(capaItr)))][x],aggRow_arr)
                    # obtains remaining variables that are not coverd via assigning values to aggregated variables
                    varLenBit_arr = BitSet(varLen_arr); aggRowBit_arr = BitSet(aggRow_arr)
                    noAggVar_tab = anyM.variables[varNam_sym].data[1 .* setdiff(varLenBit_arr,union(aggRowBit_arr,varRowViaAgg_arr...))]
                else
                    noAggVar_tab = anyM.variables[varNam_sym].data
                end
            else
                noAggVar_tab = anyM.variables[varNam_sym].data
                aggFull_tab = nothing
            end

            # XXX perform ordinary join for whatever remains and merges results
            anyM.parameter[Symbol(:costVar,capaItr)].inherit = (:R_dis => :avg_any,:Ts_dis => :avg_any)
            noAggFull_tab = matchSetParameter(anyM.report,noAggVar_tab,anyM.parameter[Symbol(:costVar,capaItr)],anyM.sets,anyM.options.digits.comp,:costVar)

            if aggFull_tab == nothing
                varCostFull_tab = noAggFull_tab
            else
                varCostFull_tab = DB.merge(aggFull_tab,noAggFull_tab)
            end

            # XXX joins discount factors to variable costs, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = matchSetParameter(anyM.report,DB.rename(varCostFull_tab,:R_dis => :R_inv),anyM.parameter[:discFac],anyM.sets,anyM.options.digits.comp,:discFac)
                push!(costGenExpr_arr,dot(DB.select(discFac_tab,:var), round.(DB.select(discFac_tab,:discFac) .* DB.select(discFac_tab,:costVar)/1000, sigdigits = anyM.options.digits.comp)))
            else
                discFac1_tab = DB.rename(DB.join(varCostFull_tab,anyM.parameter[:discFac].data; lkey = (:R_a, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacA)
                discFac_tab = DB.rename(DB.join(discFac1_tab,anyM.parameter[:discFac].data; lkey = (:R_b, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacB)
                push!(costGenExpr_arr,dot(DB.select(discFac_tab,:var), round.((0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:costVar)/1000, sigdigits = anyM.options.digits.comp)))
            end
        end
    end
    if !isempty(costGenExpr_arr) exprCost_dic[:totCostVar] = costGenExpr_arr end
    produceMessage(anyM.options,anyM.report, 3," - Created variables and constraints for variable costs")
    # </editor-fold>

    # <editor-fold desc="add array for trade costs expression to dictionary"
    costTrdExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    for trdItr in (:Buy, :Sell)
        if Symbol(:trd,trdItr,:Prc) in keys(anyM.parameter)
            joinDim_tup = tuple(intersect((:Ts_dis,:R_dis,:C,:id),colnames(anyM.variables[Symbol(:trade,trdItr)].data))...)
            # adds trade variables with prices
            trdPrc_tab = DB.join(anyM.variables[Symbol(:trade,trdItr)].data,anyM.parameter[Symbol(:trd,trdItr,:Prc)].data;lkey = joinDim_tup,rkey = joinDim_tup, how = :inner)
            # adds discount factors
            discFac_tab = matchSetParameter(anyM.report,DB.rename(trdPrc_tab,:val => :prc,:R_dis => :R_inv),anyM.parameter[:discFac],anyM.sets,anyM.options.digits.comp,:discFac)
            push!(costTrdExpr_arr,dot(DB.select(discFac_tab,:var), round.(DB.select(discFac_tab,:discFac) .* DB.select(discFac_tab,:prc) /1000 * (trdItr == :Sell ? -1 : 1), sigdigits = anyM.options.digits.comp)))
        end
    end
    if !isempty(costTrdExpr_arr) exprCost_dic[:totCostTrd] = costTrdExpr_arr end
    produceMessage(anyM.options,anyM.report, 3," - Created variables and constraints for trade related costs (and revenues)")
    # </editor-fold>

    # <editor-fold desc="creates cost variables and constraints to define them"

    # XXX create costs variables and adds them table of object variables
    if isnothing(anyM.options.bound.cost)
        info = VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)
        infoTrd = VariableInfo(false, NaN, false, NaN, false, NaN, false, NaN, false, false)
    else
        costBound_flt = anyM.options.bound.cost * length(anyM.supDis.step) * size(anyM.sets[:R],2) * anyM.options.scale.cost
        info = VariableInfo(true, 0.0, true, costBound_flt, false, NaN, false, NaN, false, false)
        infoTrd = VariableInfo(true, - costBound_flt, true, costBound_flt, false, NaN, false, NaN, false, false) # trade costs do not have a lower limit of zero, because sell revenues can exceed costs
    end

    objVar_arr = [JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, name == :totCostTrd ? infoTrd : info),string(name)) for name in keys(exprCost_dic)]
    append!(rows(anyM.variables[:objVar].data),(group = fill(:costs,length(objVar_arr)), name = collect(keys(exprCost_dic)), var = objVar_arr))

    # XXX creates constraints defining cost variables and adds them to table of object constraints
    objEqn_arr = [@constraint(anyM.optModel, objVar_arr[idx] == sum(exprCost_dic[name]) * anyM.options.scale.cost) for (idx, name) in enumerate(keys(exprCost_dic))]
    append!(rows(anyM.constraints[:objEqn].data),(group = fill(:costs,length(objEqn_arr)), name = collect(keys(exprCost_dic)), eqn = objEqn_arr))

    produceMessage(anyM.options,anyM.report, 2," - Created variables and constraints for cost objective")

    # </editor-fold>
end
