
function createAllEquations(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}} = Dict{Symbol,Dict{Tuple,Array}}())

    Equation_dic = Dict{Symbol,EqnElement}()

    # XXX investment related constraints
    # limits on investment and capacity
    createAllLimitConstraints(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}, Equation_dic::Dict{Symbol,EqnElement})
    # definition of installed capacity
    controllCapaConstraints(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}, Equation_dic::Dict{Symbol,EqnElement})
    # controlls re- and de-commissioning
    if DecommMethod != :none
        createConstraint(:commission,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    end

    # XXX distpach related constraints
    # create balancing equations
    createConstraint(:enerBal,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    createConstraint(:convBal,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    createConstraint(:stBal,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    # create capacity restrictions on dispatch variables
    createConstraint(:capaRestr,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    # set capacity limit for trade capacities
    createConstraint(:limitTrd,Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    # aggregate dispatch variables and save aggrations to dictionary
    AggDis_dic = createConstraint(:agg,Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    createObjective(:costs,AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    return Equation_dic
end

# XXX create equation object
struct EqnElement
    name::Symbol
    dim::Tuple
    grp::Tuple
    data::IndexedTable

    EqnElement(name::Symbol; kwargs...) = EqnElement(Val{name}(), name; kwargs...)
    function EqnElement(name::Symbol, dim::Tuple, grp::Tuple, data::IndexedTable)
        return new(name,dim,grp,data)
    end
end

createConstraint(name::Symbol,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement}) =
                                        createConstraint(Val{name}(),Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
createConstraint(name::Symbol,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement}) =
                                        createConstraint(Val{name}(),Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
createConstraint(name::Symbol,Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement}) =
                                        createConstraint(Val{name}(),Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

createObjective(name::Symbol,AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement}) =
                                        createObjective(Val{name}(),AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

# XXX create constraints controlling limits (fixed/lower/upper) on investment and capacity variables
function createAllLimitConstraints(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}, Equation_dic::Dict{Symbol,EqnElement})

    stockTech_arr = DB.select(DB.filter(r -> r.type == 0,Mapping_dic[:TechData]),:Te)

    # XXX create equations controlling limits (lower/upper/fixed) on investment variables
    for invVar in (:invConv, :invStIn, :invStOut, :invStSize, :invExc)
        # creates table with constraints
        limit_tab = createLimitConstraints(Variable_dic[invVar].data, invVar, stockTech_arr, Set_dic, Parameter_dic)
        if limit_tab != nothing
            # adds table to dictionary of equations if existing
            eqnName_sym = Symbol(invVar,:Limit)
            dim_tup = tuple(filter(x -> x != :eqn,collect(colnames(limit_tab)))...)
            Equation_dic[eqnName_sym] = EqnElement(eqnName_sym, dim_tup, (:invest,), limit_tab)
        end
    end

    # XXX create equations controlling limits (lower/upper/fixed) on capacity variables (and commissioned capacity variables if used)
    for capVar in union((:capaConv, :capaStIn, :capaStOut, :capaStSize, :capaExc), DecommMethod != :none ? (:capaCommConv, :capaCommStIn, :capaCommStOut, :capaCommStSize, :capaCommExc) : ())
        # groups the input variable by construction timestep before obtaining table of constraints

        varData_tab = Variable_dic[capVar].data
        if isempty(varData_tab) continue end
        groupbyCol_tup = tuple(filter(x -> !(x in (:var,:Ts_inv)),collect(colnames(varData_tab)))...)
        varDataFilt_tab = DB.groupreduce((var = +,), table(rows(Variable_dic[capVar].data)), groupbyCol_tup; select = :var)

        limit_tab = createLimitConstraints(varDataFilt_tab, capVar, stockTech_arr, Set_dic, Parameter_dic)

        if limit_tab != nothing
            eqnName_sym = Symbol(capVar,:Limit)
            dim_tup = tuple(filter(x -> x != :eqn,collect(colnames(limit_tab)))...)

            # reports, if both the installed and the commissioned capacity are constraints, this can easy lead to contradicting constraints causing an infeasible model
            if DecommMethod != :none && occursin("Comm",string(capVar))
                if Symbol(replace(string(x),"Comm" => "")) in keys(Equation_dic)
                    push!(Report_df,(2,:limit,capVar,"there are limits on the installed and the commssioned capacity at the same time, this is not advised"))
                end
            end

            Equation_dic[eqnName_sym] = EqnElement(eqnName_sym, dim_tup, (:invest,), limit_tab)
        end
    end
end

# XXX creates constraints defining the installed capacity depending on investment and residual capacities
function controllCapaConstraints(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}, Equation_dic::Dict{Symbol,EqnElement})

    # supordinate timestep of dispatch calculatins
    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]
    cntSupDis_int = length(supDis_arr)

    # assigns the year to each investment timestep starting with the subordinate level
    tsYear_dic = Dict(zip(supDis_arr,convert(Array{Int16,1},collect(0:ShortInvest_int:(length(supDis_arr)-1)*ShortInvest_int))))

    allEqn_tab = Dict{Symbol,IndexedTable}()
    capa_tup = (:Conv, :StIn, :StOut, :StSize, :Exc)

    for capaItr in capa_tup
        # <editor-fold desc="XXX prepares investment variables"
        invVar = Variable_dic[Symbol(:inv,capaItr)]
        groupCol_tup = tuple(replace(collect(invVar.dim),:Ts_inv => :Ts_supDis)...)

        # extends the investment variable table to include dispatch timesteps within lifetime of the respective expansion
        lftm_tab = reindex(matchSetParameter(invVar.data,Parameter_dic[Symbol(:life,capaItr)],Set_dic,:life),invVar.dim)
        invTsup_arr = DB.select(lftm_tab,(:Ts_inv,:life) => x -> filter(y -> (tsYear_dic[y] > tsYear_dic[x.Ts_inv]-ShortInvest_int) && (tsYear_dic[y] <= tsYear_dic[x.Ts_inv]+x.life),supDis_arr))


        invVarExt_tab = DB.rename(DB.flatten(IT.transform(invVar.data,:Ts_supDis => invTsup_arr)),:var => :inv)

        # groups investment by investment period
        invVarExtType2_tab = DB.groupreduce((inv = +,), invVarExt_tab, groupCol_tup; select = :inv)

        # </editor-fold>

        # <editor-fold desc="XXX joins investment to capacity variables"

        capaVar_obj = Variable_dic[Symbol(:capa,capaItr)]
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
            capaVarType2Exp_tab = DB.join(capaVarType2_tab,invVarExtType2_tab; lkey = groupCol_tup, rkey = groupCol_tup, how = :inner)
            #  filters type 3 variables and joins investment variables
            capaVarType3_tab = DB.filter(r -> r.Ts_inv != 0,capaVar_tab)
            capaVarType3Exp_tab = DB.join(capaVarType3_tab,invVarExt_tab; lkey = capaVar_obj.dim, rkey = capaVar_obj.dim, how = :inner)
            # merge both tables, add residutal capacities to table and write equations
            capaVarAll_tab = DB.merge(capaVarType2Exp_tab,capaVarType3Exp_tab)
        else
            capaVarAll_tab = DB.join(capaVar_tab, invVarExtType2_tab; lkey = capaVar_obj.dim, rkey = capaVar_obj.dim, how = :inner)
        end

        # joins residual capacities to table and write equation
        if Symbol(:capa,capaItr,:Resi) in keys(Parameter_dic)
            capaVarFull_tab = joinMissing(capaVarAll_tab, matchSetParameter(DB.select(capaVarAll_tab,groupCol_tup),Parameter_dic[Symbol(:capa,capaItr,:Resi)],Set_dic,:resi),groupCol_tup,groupCol_tup,:left,(0,GenericAffExpr{Float64,VariableRef}(),GenericAffExpr{Float64,VariableRef}(),0.0))
        else
            capaVarFull_tab = IT.transform(capaVarAll_tab,:resi => fill(0.0,length(capaVarAll_tab)))
        end

        capaVarFull_tab = DB.reindex(capaVarFull_tab,capaVar_obj.dim)
        groupColPlusTsInv_tup = capaItr != :Exc ? tuple(vcat(:Ts_inv,groupCol_tup...)...) : groupCol_tup

        allEqn_tab[capaItr] = reindex(IT.transform(DB.select(capaVarFull_tab,groupColPlusTsInv_tup ),:eqn =>
                                                                            DB.select(capaVarFull_tab,(:var,:inv,:resi) => x -> @constraint(global_mod, x.var == x.inv + x.resi))),groupColPlusTsInv_tup)


        # </editor-fold>
    end

    for capaItr in keys(allEqn_tab)
        Equation_dic[capaItr] = EqnElement(capaItr, tuple(filter(x -> x != :eqn,collect(colnames(allEqn_tab[capaItr])))...), (:invest,), allEqn_tab[capaItr])
    end
end

# XXX creates constraints controlling the de- and re-commissioning of capacities
function createConstraint(name::Val{:commission},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    dimConv_tup, dimSt_tup, dimExc_tup = [Variable_dic[Symbol(:capa,x)].dim for x in (:Conv,:StIn,:Exc)]

    # ensures commissioned capacity does not exceed installed capacity
    for type in (:Conv, :StIn, :StOut, :StSize, :Exc)

        # initializes symbols and tuples
        instCapa_sym = Symbol(:capa,type)
        commCapa_sym = Symbol(:capaComm,type)
        dim_tup = type in (:Conv,:Exc) ? (type == :Conv ? dimConv_tup : dimExc_tup) : dimSt_tup

        # XXX constraints to limit commissioned capacity to installed capacity
        # assigns installed to commissioned variable, writes equation and creates objects
        eqnComm_tab = DB.join((l,r) -> (eqn = @constraint(global_mod, l.var >= r.comm),),
                                                            Variable_dic[instCapa_sym].data,DB.rename(Variable_dic[commCapa_sym].data,:var => :comm); rkey = dim_tup, lkey = dim_tup, how = :inner)

        Equation_dic[Symbol(:comm,type)] = EqnElement(Symbol(:comm,type), dim_tup, (:invest,), eqnComm_tab)

        # XXX creates constraints to not allow for re-commissioning
        if DecommMethod == :decomm
            # determines the first dispatch year within data
            startDisp_int = minimum(DB.select(Variable_dic[commCapa_sym].data,:Ts_supDis))

            dimPrev_tup = tuple(replace(collect(dim_tup), :Ts_supDis => :Ts_supDisPrev)...)
            dimNoTsInv_tup = tuple(filter(x -> x != :Ts_inv,collect(dim_tup))...)

            # filter first, determine previous year for other and joins correspondig commissioning variable
            commVarFilt_tab = DB.filter(r -> r.Ts_supDis != startDisp_int,Variable_dic[commCapa_sym].data)
            commVar_tab = DB.rename(IT.transform(commVarFilt_tab,:Ts_supDisPrev => DB.select(commVarFilt_tab,:Ts_supDis => r -> r-1)),:var => :commPrev)

            commPreVar_tab = DB.rename(DB.join(commVar_tab, Variable_dic[commCapa_sym].data; lkey = dimPrev_tup, rkey = dim_tup, how = :inner),:var => :commNow)

            # joins investment variable for current year
            commInvVar_tab = DB.rename(joinMissing(commPreVar_tab, Variable_dic[Symbol(:inv,type)].data, dimNoTsInv_tup,  Variable_dic[Symbol(:inv,type)].dim, :left, (GenericAffExpr{Float64,VariableRef}(),)),:var => :invNow)

            # add any increase of residual capacities
            resiPar_sym = Symbol(:capa,type,:Resi)
            if resiPar_sym in keys(Parameter_dic)
                # adds residual capacities for current and previous year to table
                commJoinResi_tab = DB.select(commInvVar_tab,DB.Not(All(:commPrev,:commNow,:invNow)))
                commResiNow_tab = matchSetParameter(commJoinResi_tab,Parameter_dic[resiPar_sym],Set_dic,:valNow)
                commResiPrev_tab = matchSetParameter(DB.rename(DB.select(commJoinResi_tab,DB.Not(All(:Ts_supDis))),:Ts_supDisPrev => :Ts_supDis),Parameter_dic[resiPar_sym],Set_dic,:valPrev)
                # filter entries where residual increase and adds to them to table
                commResiBoth_tab = DB.join((l,r) ->  (Ts_supDis = l.Ts_supDis, deltaResi = l.valNow > r.valPrev ? (l.valNow - r.valPrev) : 0.0),commResiNow_tab,commResiPrev_tab;lkey = dimPrev_tup, rkey = dim_tup, how = :inner)
                commResiDelta_tab = DB.filter(r -> r.deltaResi != 0.0, commResiBoth_tab)
            else
                commResiDelta_tab = table(fill(Int16[],length(dim_tup)+1)...,names = vcat(:deltaResi,dim_tup...))
            end

            # creates actual constraint and object
            commDecommEqn_tab = join((l,r) -> (eqn =  @constraint(global_mod, l.commNow <= l.commPrev + l.invNow - coalesce(r.deltaResi,0.0)),),
                                                                    DB.select(commInvVar_tab,DB.Not(All(:Ts_supDisPrev))),commResiDelta_tab; lkey = dim_tup, rkey = dim_tup,how = :left)

            Equation_dic[Symbol(:decomm,type)] = EqnElement(Symbol(:decomm,type) ,dim_tup, (:invest,),commDecommEqn_tab)
        end
    end
end

# XXX creates energy balance
function createConstraint(name::Val{:enerBal},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    # <editor-fold desc="adds variables from conversion and storage"
    # adds positive variables
    posVar_tab = filterBal(DB.merge(Variable_dic[:gen].data, Variable_dic[:stExtOut].data),Set_dic,Mapping_dic)
    posVarGrp_tab = JuliaDB.groupby(posVar_tab, (:Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
        NamedTuple{(:posVar,)}(tuple(Array(y)))
    end

    # adds negative variables
    negVar_tab = filterBal(DB.merge(Variable_dic[:use].data, Variable_dic[:stExtIn].data),Set_dic,Mapping_dic)
    negVarGrp_tab = JuliaDB.groupby(negVar_tab, (:Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
        NamedTuple{(:negVar,)}(tuple(Array(y)))
    end

    # joins positive and negative variables from conversion and storage
    joinMiss_arr::Array{Union{Array{VariableRef,1},Float64,GenericAffExpr},1} = [Array{VariableRef,1}(),Array{VariableRef,1}()]
    eqnVar1_tab = joinMissing(posVarGrp_tab,negVarGrp_tab,(:Ts_dis, :R_dis, :C),(:Ts_dis, :R_dis, :C),:outer,tuple(joinMiss_arr...))
    # </editor-fold>

    # <editor-fold desc="adds variables from exchange"
    if :exchange in keys(Variable_dic)
        if :lossExc in keys(Parameter_dic)
            # find entries with exchange losses, repeat matches with switched to/from and joint to variable table
            excLoss_tab = matchSetParameter(DB.rename(DB.select(Variable_dic[:exchange].data,DB.Not(All(:var))),:R_to => :R_a,:R_from => :R_b),Parameter_dic[:lossExc],Set_dic,:val,false)
            excLossTwice_tab = DB.merge(DB.rename(excLoss_tab,:R_b => :R_from,:R_a => :R_to),DB.rename(excLoss_tab,:R_a => :R_from,:R_b => :R_to))

            excVar_tab = joinMissing(DB.select(Variable_dic[:exchange].data,DB.Not(All(:Ts_supDis))),DB.select(excLossTwice_tab,DB.Not(All(:Ts_supDis))),(:Ts_dis,:R_from,:R_to,:C), (:Ts_dis,:R_from,:R_to,:C), :left, (0.0,))

            # groups exchange variables by from regions and includes losses (= exchange requires more input in source region than is transferred to the taraget region)
            excFromGrp_tab = JuliaDB.groupby(excVar_tab, (:Ts_dis, :R_from, :C), usekey = false; select = (:var,:val)) do y
                NamedTuple{(:excFrom,)}(tuple(sum(dot(y.var, 1 ./ (1 .- y.val)))))
            end
        else
            # groups exchange variables by from regions
            # groups exchange variables by from regions
            excFromGrp_tab = JuliaDB.groupby(Variable_dic[:exchange].data, (:Ts_dis, :R_from, :C), usekey = false; select = :var) do y
                NamedTuple{(:excFrom,)}(tuple(sum(y)))
            end
        end

        # groups exchange variables by to regions
        excToGrp_tab = JuliaDB.groupby(Variable_dic[:exchange].data, (:Ts_dis, :R_to, :C), usekey = false; select = :var) do y
            NamedTuple{(:excTo,)}(tuple(sum(y)))
        end
        push!(joinMiss_arr,GenericAffExpr{Float64,VariableRef}())
        eqnVarExcFrom_tab = joinMissing(eqnVar1_tab,excFromGrp_tab, (:Ts_dis, :R_dis, :C), (:Ts_dis, :R_from, :C), :outer, tuple(joinMiss_arr...))
        push!(joinMiss_arr,GenericAffExpr{Float64,VariableRef}())
        eqnVar1_tab = joinMissing(eqnVarExcFrom_tab,excToGrp_tab, (:Ts_dis, :R_dis, :C), (:Ts_dis, :R_to, :C), :outer, tuple(joinMiss_arr...))
    end
    # </editor-fold>

    # <editor-fold desc="adds trade variables"
    for typ in (:Buy,:Sell)
        trdName_sym = Symbol(:trade,typ)
        if trdName_sym in keys(Variable_dic)
            trdVar_tab = DB.groupby(Variable_dic[trdName_sym].data, (:Ts_dis, :R_dis, :C), usekey = false; select = :var) do y
                NamedTuple{(Symbol(:trd,typ),)}(tuple(Array(y)))
            end
            push!(joinMiss_arr,Array{VariableRef,1}())
            eqnVar1_tab = joinMissing(eqnVar1_tab,trdVar_tab,(:Ts_dis, :R_dis, :C), (:Ts_dis, :R_dis, :C), :outer, tuple(joinMiss_arr...))
        end
    end
    # </editor-fold>

    # <editor-fold desc="adds parameter"
    # adds demand
    demand_tab = matchSetParameter(DB.select(eqnVar1_tab,(:Ts_dis,:R_dis,:C)),Parameter_dic[:demand],Set_dic,:dem)
    push!(joinMiss_arr,0.0)
    eqnVar2_tab = joinMissing(eqnVar1_tab,demand_tab,(:Ts_dis, :R_dis, :C), (:Ts_dis, :R_dis, :C), :left, tuple(joinMiss_arr...))

    # </editor-fold>

    # <editor-fold desc="write final equations"
    eqnCol_tup = colnames(eqnVar2_tab)
    # creates equations differently depending on how exchange and trade are applied
    if :exchange in keys(Variable_dic)
        if :trdBuy in eqnCol_tup && :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:excTo,:excFrom,:dem,:trdBuy,:trdSell) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) == sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem))
        elseif :trdBuy in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:excTo,:excFrom,:dem,:trdBuy) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.excTo)+sum(x.trdBuy) == sum(x.negVar)+sum(x.excFrom)+x.dem))
        elseif :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:excTo,:excFrom,:dem,:trdSell) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.excTo) == sum(x.negVar)+sum(x.excFrom)+sum(x.trdSell)+x.dem))
        else
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:excTo,:excFrom,:dem) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.excTo) == sum(x.negVar)+sum(x.excFrom)+x.dem))
        end
    else
        if :trdBuy in eqnCol_tup && :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:dem,:trdBuy,:trdSell) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.trdBuy) == sum(x.negVar)+sum(x.trdSell)+x.dem))
        elseif :trdBuy in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:dem,:trdBuy) =>
                            x -> @constraint(global_mod, sum(x.posVar)+sum(x.trdBuy) == sum(x.negVar)++x.dem))
        elseif :trdSell in eqnCol_tup
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:dem,:trdSell) =>
                            x -> @constraint(global_mod, sum(x.posVar) == sum(x.negVar)+sum(x.trdSell)+x.dem))
        else
            eqn_arr = DB.select(eqnVar2_tab,(:posVar,:negVar,:dem) =>
                            x -> @constraint(global_mod, sum(x.posVar) == sum(x.negVar)+x.dem))
        end
    end

    Equation_dic[:enerBal] = EqnElement(:enerBal, (:Ts_dis, :R_dis, :C), (:dispatch,),IT.transform(DB.select(eqnVar2_tab,(:Ts_dis, :R_dis, :C)),:eqn => eqn_arr))
    # </editor-fold>
end

# XXX creates conversion balance
function createConstraint(name::Val{:convBal},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    joinKey_tup = (:Ts_inv, :Ts_dis, :R_dis, :Te)
    joinKeyM_tup = (:Ts_inv, :Ts_dis, :R_dis, :Te, :M)

    # XXX determines relevant dimensions for conversion balance
    convBalTech_tab = DB.select(DB.filter(r -> r.refLvl != nothing && :use in keys(r.allCar) && :gen  in keys(r.allCar),DB.select(Mapping_dic[:TechInfo],(:Te,:refLvl,:allCar))),DB.Not(All(:allCar)))
    allCapaConv_tab = table(DB.unique(rows(DB.select(Variable_dic[:capaConv].data,(:Ts_inv,:Te)))))

    # gets levels in conversion balance and expands to actual regions and timesteps
    convBalDim_tab = DB.select(IT.transform(convBalTech_tab,:Ts_dis => map(x -> x.Ts, DB.select(convBalTech_tab,:refLvl)),:R_dis => map(x -> x.R, DB.select(convBalTech_tab,:refLvl))),DB.Not(All(:refLvl)))
    convBal_tab = expandSetColumns(DB.join(convBalDim_tab,allCapaConv_tab;lkey = :Te, rkey = :Te, how = :inner),(:Ts_dis,:R_dis), Set_dic)

    # joins conversion efficiencies, defines also entries where mode need to be specified explicitly
    convBalEff_tab = matchSetParameter(convBal_tab,Parameter_dic[:effConv],Set_dic,:eff,false)

    # gets alls modes that require a seperate conversion balance
    modeConv_arr = sort(unique(DB.select(convBalEff_tab,:M)))
    # removes mode column to perform correct join with dispach variables below
    convBalEffNoM_tab = table(DB.unique(DB.select(convBalEff_tab,DB.Not(All(:M,:eff)))))

    varInOut_dic = Dict{Symbol,IndexedTable}()

    # XXX writes grouped input and output related variables
    for type in (:in => (:use,:stIntOut), :out => (:gen,:stIntIn))
        var_tab = DB.merge(DB.join(Variable_dic[type[2][1]].data,addDummyCol(convBalEffNoM_tab); lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), rselect = DB.Not(All(:dummy)), how = :inner),
                                DB.join(Variable_dic[type[2][2]].data,addDummyCol(convBalEffNoM_tab); lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), rselect = DB.Not(All(:dummy)), how = :inner))
        # where input variables are mode dependant, but the conversion balance does not need to be specified for this mode, mode value is re-written to zero to groupy correctly
        modeVar_arr = unique(DB.select(var_tab,:M))
        if  modeVar_arr != modeConv_arr
            modeVar_dic = Dict(x => x in modeConv_arr ? x : 0 for x in modeVar_arr)
            varInOut_dic[type[1]] = IT.rename(DB.groupreduce((var = +,), IT.transform(var_tab,:M => map(x -> modeVar_dic[x], DB.select(var_tab,:M))), joinKeyM_tup; select = :var),:var => Symbol(type[1],:Var))
        else
            varInOut_dic[type[1]] = IT.rename(DB.groupreduce((var = +,), var_tab, joinKeyM_tup; select = :var),:var => Symbol(type[1],:Var))
        end
    end

    # XXX merge efficiencies and variables and creates constraint
    convBalInOutEff_tab = join(convBalEff_tab,joinMissing(varInOut_dic[:in],varInOut_dic[:out], joinKeyM_tup, joinKeyM_tup, :outer, (0.0,0.0)); lkey = joinKeyM_tup, rkey = joinKeyM_tup, how = :inner)
    eqn_arr = DB.select(convBalInOutEff_tab,(:inVar,:outVar,:eff) => x -> @constraint(global_mod, sum(x.inVar)*x.eff ==  sum(x.outVar)))

    Equation_dic[:convBal] = EqnElement(:convBal, joinKey_tup, (:dispatch,), IT.transform(DB.select(convBalInOutEff_tab,DB.Not(All(:inVar, :outVar, :eff))),:eqn => eqn_arr))
end

# XXX create storage balance
function createConstraint(name::Val{:stBal},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    joinKey_tup = (:Ts_inv, :Ts_dis, :R_dis, :C, :Te)
    joinKeyM_tup = (:Ts_inv, :Ts_dis, :R_dis, :C, :Te, :M)
    sizeVar_tab = DB.rename(DB.select(Variable_dic[:stSize].data,DB.Not(All(:Ts_supDis))),:var => :size)

    # <editor-fold desc="joins variable with current storage level (=stSize) to level of next periode"
    # get supordinate dispatch timesteps
    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]

    # creates dictionary that assigns last to first dispatch period within a supordinate dispatch timestep
    tsChildren_dic = Dict((x,y) => getChildren(x,Set_dic[:Ts],false,y) for x in supDis_arr, y in unique(Set_dic[:Ts][DB.select(sizeVar_tab,:Ts_dis),:lvl]))
    lastFirstTs_dic = Dict(maximum(tsChildren_dic[z]) => minimum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
    lastTs_arr = collect(keys(lastFirstTs_dic))

    # filters next sizes and adds them to table
    nextTs_tab = DB.rename(IT.transform(sizeVar_tab,:Ts_nextDis => DB.select(Variable_dic[:stSize].data,:Ts_dis => x -> x in lastTs_arr ? lastFirstTs_dic[x] : x+1)),:size => :nextSize)

    sizeBothVar_tab = DB.reindex(DB.join(sizeVar_tab,nextTs_tab; lkey = joinKeyM_tup, rkey = (:Ts_inv, :Ts_nextDis, :R_dis, :C, :Te, :M), how = :inner),joinKeyM_tup)
    # saves all carrier/mode combinations where size is mode specific
    modeCarSize_arr::Array{Tuple{Int16,Int16},1} = values.(sort(unique(DB.select(sizeBothVar_tab,(:C,:M)))))

    varInOut_dic = Dict{Symbol,IndexedTable}()
    sizeVarNoM_tab = table(DB.unique(DB.select(sizeVar_tab,DB.Not(All(:M,:size)))))
    # </editor-fold>

    # <editor-fold desc="adds variables for storage in and output to variables for storage level"
    for type in (:In,:Out)
        # find all relevant variables
        allVar_tab = DB.merge(DB.join(Variable_dic[Symbol(:stExt,type)].data,sizeVarNoM_tab; lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), how = :inner),
                                            DB.join(Variable_dic[Symbol(:stInt,type)].data,sizeVarNoM_tab; lkey = joinKey_tup, rkey = joinKey_tup, lselect = DB.Not(All(:Ts_supDis)), how = :inner))

        # joins efficiency parameter and creates expression
        allVarEff_tab = matchSetParameter(allVar_tab,Parameter_dic[Symbol(:effSt,type)],Set_dic,:eff,false)

        allVarExpr_tab = sort(IT.transform(DB.select(allVarEff_tab,DB.Not(All(:eff,:var))),:var => type == :In ? DB.select(allVarEff_tab,(:eff,:var) => x -> x.eff*x.var)
                                                                                                                : DB.select(allVarEff_tab,(:eff,:var) => x -> x.var/x.eff)))
        # replaces mode value with zero where size is not mode controlled
        if !isempty(Mapping_dic[:modeCases])
            modeSizeInOut_dic::Dict{Tuple{Int16,Int16},Int16} = Dict((x.C, x.M) => (x.C, x.M) in modeCarSize_arr ? x.M : 0 for x in unique(DB.select(allVar_tab,(:C,:M))))
            allVarExpr_tab = IT.transform(DB.select(allVarExpr_tab,DB.Not(All(:M))),:M => map(x -> modeSizeInOut_dic[(x.C,x.M)],DB.select(allVarExpr_tab,(:C,:M))))
        end
        # groups variables to join with size variables next
        varInOut_dic[type] = IT.rename(DB.groupreduce((var = :var  => +,), allVarExpr_tab, joinKeyM_tup; select = (:var,)),:var => Symbol(:var,type))
    end

    # merges variables for input and output to size variables
    allVar_tab = joinMissing(varInOut_dic[:In],varInOut_dic[:Out], joinKeyM_tup, joinKeyM_tup, :outer, (0.0,0.0))
    sizeAllVarEff_tab = DB.join(table(rows(sizeBothVar_tab)),allVar_tab; lkey = joinKeyM_tup, rkey = joinKeyM_tup, how = :inner)
    # </editor-fold>

    # <editor-fold desc="adds additional parameters and create equation"
    # adds discharge rate and inflows if they are defined
    disDef_boo = (:stDis) in keys(Parameter_dic)
    infDef_boo = (:stInflow) in keys(Parameter_dic)

    if disDef_boo
        sizeAllVarEff_tab = joinMissing(sizeAllVarEff_tab,matchSetParameter(DB.select(sizeVar_tab,DB.Not(All(:size))),Parameter_dic[:stDis],Set_dic,:stDis,false), joinKeyM_tup, joinKeyM_tup, :left,(0.0,))
    end

    if infDef_boo
        sizeAllVarEff_tab = joinMissing(sizeAllVarEff_tab,matchSetParameter(DB.select(sizeVar_tab,DB.Not(All(:size))),Parameter_dic[:stInflow],Set_dic,:stInf,false), joinKeyM_tup, joinKeyM_tup, :left,(0.0,))
    end

    # XXX create equations and final object
    if disDef_boo && infDef_boo
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut,:stDis,:stInf)
                                                            => x -> @constraint(global_mod, x.nextSize == (1-x.stDis) * (x.size + x.stInf + sum(x.varIn) - sum(x.varOut))))
    elseif !disDef_boo && infDef_boo
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut,:stInf)
                                                            => x -> @constraint(global_mod, x.nextSize == x.size + x.stInf + sum(x.varIn) - sum(x.varOut)))
    elseif disDef_boo && !infDef_boo
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut,:stDis)
                                                            => x -> @constraint(global_mod, x.nextSize == (1-x.stDis) * (x.size + sum(x.varIn) - sum(x.varOut))))
    elseif !disDef_boo && !infDef_boo
        eqn_arr = DB.select(sizeAllVarEff_tab,(:size,:nextSize,:varIn,:varOut)
                                                            => x -> @constraint(global_mod, x.nextSize == x.size + sum(x.varIn) - sum(x.varOut)))
    end

    Equation_dic[:stBal] = EqnElement(:stBal, joinKey_tup, (:dispatch,), IT.transform(DB.select(sizeAllVarEff_tab,joinKeyM_tup),:eqn => eqn_arr))
    # </editor-fold>
end

# XXX creates capacity restrictions on dispatch variables
function createConstraint(name::Val{:capaRestr},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    # XXX capacity restrctions on conversion and storage capacites

    # adds a id column and filters restsrictions where actual capacity exists
    restrId_tab = IT.transform(Mapping_dic[:capaDispRestr],:id => convert(Array{Int16,1},collect(1:length(Mapping_dic[:capaDispRestr]))))
    restrIdConv_tab = DB.join(restrId_tab,table(DB.unique(DB.select(Mapping_dic[:capaConv],(:Ts_inv,:Te)))); lkey = :Te, rkey = :Te, rselect = :Ts_inv, how = :inner)
    restrIdSt_tab = DB.join(restrId_tab,table(DB.unique(DB.select(Mapping_dic[:capaSt],(:Ts_inv,:Te)))); lkey = :Te, rkey = :Te, rselect = :Ts_inv, how = :inner)

    # creates named tuple for every type of restriction to be created
    cnstrType_tup = ((type = :out, disVar = (:gen,), capaVar = :Conv), (type = :in, disVar = (:use,), capaVar = :Conv), (type = :stIn, disVar = (:stExtIn, :stIntIn), capaVar = :StIn),
                                                                (type = :stOut, disVar = (:stExtOut, :stIntOut), capaVar = :StOut), (type = :stSize,disVar = (:stSize,), capaVar = :StSize))

    [createRestr(cnstr::NamedTuple,DB.filter(r -> r.cnstrType == cnstr.type, cnstr.type in (:in,:out) ? restrIdConv_tab : restrIdSt_tab)::IndexedTable,
                Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
                                                                                                                                                                                for cnstr in cnstrType_tup]

    # XXX capacity restrctions for exchange capacities

    # joins capacity variable to dispatch variable
    excDisVar_tab = DB.rename(Variable_dic[:exchange].data,:var => :excDisp)

    excVarFull_tab =  DB.reindex(DB.merge([DB.select(DB.join(excDisVar_tab,Variable_dic[:capaExc].data; lkey = (:Ts_supDis, x, :C), rkey = (:Ts_supDis, :R_a, :C), rselect = DB.Not(All(:R_b)) ,how = :inner),DB.Not(All(:R_a,:Ts_supDis)))
                                                                                                                                                                for x in (:R_from,:R_to)]...),(:Ts_dis,:R_from,:R_to,:C))
    # adds availability of exchange capacity (matchSetParameter has to be called twice, because both "sides", from and to need to be checked)
    excAvaOnce_tab = matchSetParameter(DB.rename(excVarFull_tab,:R_to => :R_a,:R_from => :R_b),Parameter_dic[:avaExc],Set_dic,:ava,false)
    excAva_tab = DB.merge(DB.rename(excAvaOnce_tab,:R_b => :R_from,:R_a => :R_to),DB.rename(excAvaOnce_tab,:R_a => :R_from,:R_b => :R_to))

    excVar_tab = DB.reindex(DB.rename(excAva_tab,:var => :excCapa),(:Ts_dis, :R_from, :R_to, :C))

    eqn_arr = DB.select(excVar_tab,(:excDisp, :excCapa, :ava) => x -> @constraint(global_mod, x.excDisp <=  x.excCapa*x.ava))
    Equation_dic[:capaRestrExc] = EqnElement(:capaRestrExc, (:Ts_dis,:R_from,:R_tom,:C),(:dispatch,),IT.transform(DB.select(excVar_tab,(:Ts_dis,:R_from,:R_to,:C)),:eqn => eqn_arr))
end

# XXX sets capacity limits for buy and sell capacity, if any are defined
function createConstraint(name::Val{:limitTrd},Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    for typ in (:Buy, :Sell)
        if Symbol(:trd,typ,:Cap) in keys(Parameter_dic)
            eqn_tab = matchSetParameter(Variable_dic[Symbol(:trade,typ)].data,Parameter_dic[Symbol(:trd,typ,:Cap)],Set_dic,:cap,false)
            eqnFull_tab = IT.transform(DB.select(eqn_tab,DB.Not(All(:cap,:var))),:eqn => DB.select(eqn_tab,(:var,:cap) => x -> @constraint(global_mod, x.var <=  x.cap)))
            Equation_dic[Symbol(:limit,typ)] = EqnElement(Symbol(:limit,typ),(:Ts_dis, :R_dis, :C, :id),(:dispatch,),eqnFull_tab)
        end
    end
end

# XXX aggregates all dispatch variables
function createConstraint(name::Val{:agg},Set_dic::Dict{Symbol,DataFrame},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    AggDis_dic = Dict{Symbol,Dict{Int64,BitSet}}()
    for dispVar in (:gen,:use,:stIntIn,:stIntOut,:stExtIn,:stExtOut)

        # initalizes data and calls aggregation function
        aggData_tab = Variable_dic[dispVar].data
        aggCol_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M)
        relAggRmv_dic = aggregateSetTable(DB.select(aggData_tab,DB.Not(All(:Ts_supDis,:var))),aggCol_tup, (((:Ts_inv, :Te, :M, :Ts_dis) => (:Ts_inv, :Te, :M)), (:C, :R_dis)),Set_dic)

        # creates aggregation constraint and equation object
        var_arr = DB.select(aggData_tab,:var)

        aggEqn_dic = Dict{Int32,ConstraintRef}()
        for aggRow in keys(relAggRmv_dic)
            aggEqn_dic[aggRow] = @constraint(global_mod, var_arr[aggRow] == sum(var_arr[collect(relAggRmv_dic[aggRow])]))
        end

        dimIdx_tab = DB.select(aggData_tab,aggCol_tup)

        aggEqnSort_dic = sort(aggEqn_dic)
        eqnName_sym = Symbol(:agg,uppercase(String(dispVar)[1]),String(dispVar)[2:end])
        Equation_dic[eqnName_sym] = EqnElement(eqnName_sym, aggCol_tup, (:dispatch,),IT.transform(dimIdx_tab[collect(keys(aggEqnSort_dic))],:eqn => collect(values(aggEqnSort_dic))))
        AggDis_dic[eqnName_sym] = relAggRmv_dic
    end
    return AggDis_dic
end

# <editor-fold desc="collection of other subfunctions"

# writes limit constraints for all input variables
function createLimitConstraints(varData_tab::IndexedTable,varName_sym::Symbol,stockTech_arr::Array{Int16,1},Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement})
        # gets dimensions of search variable
        varDim_tup = tuple(filter(x -> x != :var,collect(colnames(varData_tab)))...)

        # TODO limit constraints, die oberhalb einzelner kapazitätsvariablen liegen werde nicht berücksichtigt (z.B. summe windkraft)
        # matches variables with defined parameter values
        varLim_tab = matchLimitParameter(varData_tab,varName_sym,Set_dic,Parameter_dic)
        crtLimits_tup = intersect(colnames(varLim_tab),(:Low,:Up,:Fix))

        if isempty(crtLimits_tup) return nothing end

        # excludes limits on stock technologies and reports this (only for conversion and storage)
        if :Te in varDim_tup
            for i in intersect(stockTech_arr,DB.select(varLim_tab,:Te))
                push!(Report_df,(2,:limit,varName_sym,"$(createFullString(i,Set_dic[:Te])) is a technology without investment, but limits were provided, these values were ignored"))
            end
            varLim_tab = DB.filter(r -> !(r.Te in stockTech_arr),varLim_tab)
        end

        # excludes cases

        table_arr = IndexedTable[]

        # creates a table per existing limit type and writes to an array
        for type in (:Low, :Up, :Fix)
            if type in crtLimits_tup
                subLim_tab = DB.dropmissing(DB.select(varLim_tab,tuple(vcat(varDim_tup...,(type, :var)...)...)))
                if type == :Low
                    push!(table_arr, IT.transform(DB.select(subLim_tab,varDim_tup),:eqn => DB.select(subLim_tab,(type,:var) => x -> @constraint(global_mod, x.Low <= sum(x.var)))))
                elseif type == :Up
                    push!(table_arr, IT.transform(DB.select(subLim_tab,varDim_tup),:eqn => DB.select(subLim_tab,(type,:var) => x -> @constraint(global_mod, x.Up >= sum(x.var)))))
                else
                    push!(table_arr, IT.transform(DB.select(subLim_tab,varDim_tup),:eqn => DB.select(subLim_tab,(type,:var) => x -> @constraint(global_mod, x.Fix == sum(x.var)))))
                end
            end
        end

        # merges all arrays depending on the length
        tableLength_int = length(table_arr)
        if tableLength_int == 0
            limitEqn_tab = nothing
        elseif tableLength_int == 1
            limitEqn_tab = table_arr[1]
        else
            limitEqn_tab = merge(table_arr[1], tableLength_int == 2 ? table_arr[2] : merge(table_arr[2],table_arr[3]))
        end

        return limitEqn_tab
end

# finds parameters constraining defining limit constraints (fixed/lower/upper)
function matchLimitParameter(varData_tab::IndexedTable,varName_sym::Symbol,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},returnIn_boo::Bool=true)

    # initializes variables
    defPar_tup = tuple(collect(keys(Parameter_dic))...)
    matchedPar_dic = Dict{Symbol,IndexedTable}()

    search_tab = DB.select(varData_tab,DB.Not(All(:var)))
    searchCol_tup = colnames(search_tab)

    # searches for fixed, upper and lower and saves matched parameter in dictionary
    for consType in (:Fix,:Up,:Low)
        searchPar_sym = Symbol(varName_sym,consType)
        if searchPar_sym in defPar_tup
            matchedPar_tab = matchSetParameter(search_tab, Parameter_dic[searchPar_sym], Set_dic)
            if !isempty(matchedPar_tab)
                matchedPar_dic[consType] = DB.rename(DB.join(addDummyCol(search_tab),matchedPar_tab; lkey = searchCol_tup, rkey = searchCol_tup, lselect = searchCol_tup, rselect = :val, how = :inner), :val => consType)
            end
        end
    end

    usedTypes_arr = collect(keys(matchedPar_dic))

    # returns input data if nothing was matches
    if isempty(usedTypes_arr) returnIn_boo ? (return varData_tab) : (return nothing) end

    # merges matched tables depending on the number of matches
    if length(usedTypes_arr) == 1
        matched_tab= matchedPar_dic[usedTypes_arr[1]]
    else
        matched_tab = DB.join(matchedPar_dic[usedTypes_arr[1]],matchedPar_dic[usedTypes_arr[2]];lkey = searchCol_tup, rkey = searchCol_tup, how = :outer)
        if length(usedTypes_arr) == 3
            matched_tab = DB.join(matched_tab,matchedPar_dic[usedTypes_arr[3]];lkey = searchCol_tup, rkey = searchCol_tup, how = :outer)
        end
    end


    # checks for conflicts between lower and upper limits
    if :Low in usedTypes_arr || :Up in usedTypes_arr
        if :Low in usedTypes_arr && :Up in usedTypes_arr
            errorContra_tab = matched_tab[findall(DB.select(matched_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : x[1] > x[2]))]
            errorSame_tab = matched_tab[findall(DB.select(matched_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : x[1] == x[2]))]
            errorUpperZero_tab = matched_tab[findall(DB.select(matched_tab,(:Low,:Up) => x -> ismissing(x[1]) || ismissing(x[2]) ? false : (x[1] == 0) || (x[2] == 0)))]

            if !isempty(errorContra_tab)
                uniContra_arr = unique(DB.select(errorContra_tab,:Te))
                for i in uniContra_arr
                    push!(Report_df,(2,:eqn,varName_sym,"$(createFullString(i,Set_dic[:Te])) got contradicting values for upper and lower limit, these values were ignored"))
                end
                matched_tab = DB.join(matched_tab, errorContra_tab;lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
            end

            if !isempty(errorSame_tab)
                uniSame_arr = unique(DB.select(errorContra_tab,:Te))
                for i in uniSame_arr
                    push!(Report_df,(2,:eqn,varName_sym,"$(createFullString(i,Set_dic[:Te])) got same values for upper and lower limits detected, use fix instead"))
                end
            end
        else
            errorUpperZero_tab = matched_tab[findall(DB.select(matched_tab,intersect((:Low,:Up),usedTypes_arr)[1] => x -> ismissing(x) ? false : (x == 0)))]
        end

        if !isempty(errorUpperZero_tab)
            uniUpperZero_arr = unique(DB.select(errorUpperZero_tab,:Te))
            for i in uniUpperZero_arr
                push!(Report_df,(2,:eqn,varName_sym,"$(createFullString(i,Set_dic[:Te])) got upper or/and lower limit of zero, these values were ignored"))
            end
            matched_tab = DB.join(matched_tab, errorUpperZero_tab; lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
        end
    end

    # checks if additional limits were set if value is already fixed
    if :Fix in usedTypes_arr && (:Low in usedTypes_arr || :Up in usedTypes_arr)
        if :Low in usedTypes_arr && :Up in usedTypes_arr
            errorFixLimit_tab = matched_tab[findall(DB.select(matched_tab,(:Fix,:Low,:Up) => x -> !ismissing(x[1]) && (!ismissing(x[2]) || !ismissing(x[3]))))]
        elseif :Low in usedTypes_arr
            errorFixLimit_tab = matched_tab[findall(DB.select(matched_tab,(:Fix,:Low) => x -> !ismissing(x[1]) && !ismissing(x[2])))]
        else
            errorFixLimit_tab = matched_tab[findall(DB.select(matched_tab,(:Fix,:Up) => x -> !ismissing(x[1]) && !ismissing(x[2])))]
        end

        if !isempty(errorUpperZero_tab)
            uniFixLimit_arr = unique(DB.select(errorFixLimit_tab,:Te))
            for i in uniFixLimit_arr
                push!(Report_df,(2,:eqn,varName_sym,"$(createFullString(i,Set_dic[:Te])) got fixed value and limits (upper/lower) set, limits were ignored"))
            end
            matched_tab = DB.join(matched_tab, uniFixLimit_arr; lkey = searchCol_tup, rkey = searchCol_tup, how = :anti)
        end
    end

    return DB.join(matched_tab,varData_tab;lkey = searchCol_tup, rkey = searchCol_tup, how = :left)
end

# creates capacity constraint for specific type (:out,:in,:stOut,...)
function createRestr(cnstr_ntup::NamedTuple,cnstr_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Mapping_dic::Dict{Symbol,IndexedTable},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})

    # <editor-fold desc="initalizes dispatch and capacity variables"
    # checks if some kind de-commissioning is enabled and uses actually commissioned capacities in that case
    capaVar_sym = DecommMethod != :none ? :capaComm : :capa

    # create tuples for joining capacity and dispatch variables
    joinCapa_tup = (:Ts_supDis,:Ts_inv,:R_inv,:Te)
    joinDis_tup = (:Ts_dis,:Ts_inv,:R_dis,:C,:Te)

    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]

    # adds investment regions and supordinate dispatch timesteps to obtain full investment table
    rLvlInv_arr = map(x -> x.R,DB.select(cnstr_tab,:Te => z ->DB.select(Mapping_dic[:TechInfo],:invLvl)[findall(DB.select(Mapping_dic[:TechInfo],:Te) .== z)[1]]))
    cnstrCapa_tab = expandSetColumns(IT.transform(DB.select(cnstr_tab,DB.Not(All(:cnstrType))),:R_inv => rLvlInv_arr),(:R_inv,),Set_dic)
    capaDim_tab = DB.rename(DB.flatten(IT.transform(cnstrCapa_tab,:Ts_supDis => fill(supDis_arr,length(cnstrCapa_tab))),:Ts_supDis),:car => :C)

    # joins capacity variables
    capaVar_tab = DB.rename(DB.join(capaDim_tab,Variable_dic[Symbol(capaVar_sym,cnstr_ntup.capaVar)].data; lkey = joinCapa_tup, rkey = joinCapa_tup, how = :inner),:var => :capaVar)

    # adds dispatch regions to table and groups to obtain capacity variables
    lvlRDis_dic = Dict((x.lvlR,x.R_inv) => Set_dic[:R][x.R_inv,:lvl] == x.lvlR ? x.R_inv : convert(Int16,getHeritanceLine(x.R_inv,Set_dic[:R],x.lvlR)) for x in DB.unique(DB.select(capaVar_tab,(:lvlR,:R_inv))))
    capaVarR_tab = IT.transform(DB.select(capaVar_tab,DB.Not(All(:lvlR))),:R_dis => DB.select(capaVar_tab,(:lvlR,:R_inv) => x -> lvlRDis_dic[(x.lvlR,x.R_inv)]))
    capaVarGrp_tab = DB.groupby(capaVarR_tab,(:Ts_supDis,:Ts_inv,:R_dis,:Te,:id,:lvlTs,:C), usekey = false, select = :capaVar) do x
        NamedTuple{(:capaVar,)}(tuple(sum(x)))
    end

    # adds dispatch timesteps to table
    lvlTsStep_dic = Dict((x,y) => x == lvlSupDis_int ? [y] : getChildren(y,Set_dic[:Ts],false,x) for x in unique(DB.select(capaVarGrp_tab,(:lvlTs))), y in supDis_arr)
    disDim_tab = DB.flatten(DB.flatten(IT.transform(DB.select(capaVarGrp_tab,DB.Not(All(:lvlTs,:capaVar))),:Ts_dis => DB.select(capaVarGrp_tab,(:lvlTs,:Ts_supDis) => x -> lvlTsStep_dic[(x.lvlTs,x.Ts_supDis)])),:Ts_dis),:C)

    # joins dispatach variables depending on how many types of dispatch variables are to be joined
    if length(cnstr_ntup.disVar) == 1
        disVar_tab = DB.rename(DB.join(disDim_tab,Variable_dic[cnstr_ntup.disVar[1]].data; lkey = joinDis_tup, rkey = joinDis_tup, how = :inner),:var => :disVar)
    else
        mergeDis_tab = DB.merge(Variable_dic[cnstr_ntup.disVar[1]].data,Variable_dic[cnstr_ntup.disVar[2]].data)
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
    varAva_tab = IT.transform(disVar2_tab,:disVar => DB.select(disVar2_tab,:disVar)./DB.select(matchSetParameter(DB.select(disVar2_tab,parDim_tup),Parameter_dic[avaName_sym],Set_dic),:val))

    # adds efficiency parameter in case capacity constraints addresses generation
    if cnstr_ntup.type == :out
        varAva_tab = IT.transform(varAva_tab,:disVar => DB.select(varAva_tab,:disVar)./DB.select(matchSetParameter(DB.select(varAva_tab,parDim_tup),Parameter_dic[:effConv],Set_dic),:val))
    end
    # </editor-fold>


    # <editor-fold desc="groups dispatch varialbles and joins capacity variables"
    # groups table to obtain dispatch variables and joins capacity variables
    grpDim_tup = tuple(filter(x -> x != :M,vcat(:Ts_supDis,parDim_tup...))...)
    grpDim2_tup = tuple(filter(x -> !(x in (:M,:Ts_dis)),vcat(:Ts_supDis,parDim_tup...))...)

    disVarGrp1_tab = DB.groupby(varAva_tab,tuple(vcat(grpDim_tup...,:id)...), usekey = false, select = :disVar) do x
        NamedTuple{(:disVar,)}(tuple(sum(x)))
    end
    rSelNot_tup = cnstr_ntup.type in (:in,:out) ? (:C,:lvlTs,:id) : (:lvlTs,:id)

    allVarFinal_tab = DB.join(disVarGrp1_tab,capaVarGrp_tab; lkey = grpDim2_tup, rkey = grpDim2_tup, lselect = DB.Not(All(:id)), rselect = DB.Not(All(rSelNot_tup)), how = :inner)
    allVarFinalRe_tab = DB.reindex(allVarFinal_tab,grpDim_tup)

    # create final equations and write object
    fullData_tab = IT.transform(DB.select(allVarFinalRe_tab,grpDim_tup),:eqn => DB.select(allVarFinalRe_tab,(:disVar,:capaVar) => x -> @constraint(global_mod, sum(x.disVar) <= sum(x.capaVar))))
    eqnNam_sym = Symbol(:capaRestr,uppercase(String(cnstr_ntup.type)[1]),String(cnstr_ntup.type)[2:end])
    Equation_dic[eqnNam_sym] = EqnElement(eqnNam_sym, tuple(filter(x -> x != :M,collect(parDim_tup))...),(:dispatch,),fullData_tab)
    # </editor-fold>
end

# filters all entries from dispatch variables that relate to the energy balance
function filterBal(dispVar_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame},Mapping_dic::Dict{Symbol,IndexedTable})

    tsDis_arr, rDis_arr, c_arr = [convert(Array{Int16,1},unique(DB.select(dispVar_tab,dim))) for dim in (:Ts_dis,:R_dis,:C)]
    carBalLvl_dic = Dict(x => values(DB.select(DB.filter(r -> r.C == x,Mapping_dic[:C_lvl]),(:lvlTsDis, :lvlRDis))[1]) for x in c_arr)
    tsLvl_dic  = Dict(x => Set_dic[:Ts][x,:lvl] for x in tsDis_arr)
    rLvl_dic = Dict(x => Set_dic[:R][x,:lvl] for x in rDis_arr)
    return DB.filter(r -> carBalLvl_dic[r.C] == (tsLvl_dic[r.Ts_dis],rLvl_dic[r.R_dis]) ,dispVar_tab)
end

# </editor-fold>

# XXX function to create all objectives
function createObjective(name::Val{:costs},AggDis_dic::Dict{Symbol,Dict{Int64,BitSet}},Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},Variable_dic::Dict{Symbol,VarElement},Equation_dic::Dict{Symbol,EqnElement})
    exprCost_dic = Dict{Symbol,Array{GenericAffExpr{Float64,VariableRef},1}}()

    # <editor-fold desc="compute discount factors"
    # gets all relevant supordinate dispatch timesteps and regions and creates a table of them
    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]
    allRInv_arr = vcat(map(x -> Set_dic[:R][Set_dic[:R][:,:lvl] .== x,:idx],unique(DB.select(Mapping_dic[:C_lvl],:lvlRInv)))...)
    tsR_tab = DB.flatten(DB.flatten(table([supDis_arr],[allRInv_arr];names = (:Ts_supDis,:R_inv)),:Ts_supDis),:R_inv)

    # looks up  created table with discount rates and computes full discount factors from that
    rateVal_tab = reindex(matchSetParameter(tsR_tab,Parameter_dic[:rateDisc],Set_dic,:itrRate),(:Ts_supDis,:R_inv))
    val_arr = map(allRInv_arr) do y
        discFc_arr = vcat(1,(1 ./ (1 .+ DB.select(DB.filter(r -> r.R_inv == y,rateVal_tab),:itrRate)[2:end])).^ShortInvest_int)
        return map(x -> prod(discFc_arr[1:x]),1:length(discFc_arr))
    end

    Parameter_dic[:discFac] = ParElement(DataFrame(Ts = repeat(supDis_arr,length(allRInv_arr)), R = sort(repeat(allRInv_arr,length(supDis_arr))), val = vcat(val_arr...)),
                                                                                                        (name = :discFac, dim = (:Ts_supDis, :R_inv), default_val = nothing, inherit = (:Ts_supDis => :up, :R_inv => :up), grp = :invest))
    # </editor-fold>

    # <editor-fold desc="add array for investment costs expression to dictionary"
    costInvExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    for (idx,capaItr) in enumerate((:Conv,:StIn,:StOut,:StSize,:))
        if Symbol(:costInv,capaItr) in keys(Parameter_dic)
            invVar_tab = Variable_dic[Symbol(:inv,capaItr)].data
            dim_tup = tuple(filter(x -> x != :var, collect(colnames(invVar_tab)))...)

            tsYear_dic = Dict(zip(supDis_arr,convert(Array{Int16,1},collect(0:ShortInvest_int:(length(supDis_arr)-1)*ShortInvest_int))))

            # adds lifetime (if an economic lifetime is defined, this is used)
            if Symbol(:lifeEco,capaItr) in keys(Parameter_dic)
                lftm_tab = matchSetParameter(invVar_tab,Parameter_dic[Symbol(:lifeEco,capaItr)],Set_dic,:life)
                if length(lftm_tab) < length(invVar_tab)
                    newSearch_tab = DB.join(invVar_tab,lftm_tab; lkey = dim_tup, rkey = dim_tup, how = :anti)
                    lftm_tab = DB.merge(matchSetParameter(newSearch_tab,Parameter_dic[Symbol(:life,capaItr)],Set_dic,:life),lftm_tab)
                end
            else
                lftm_tab = matchSetParameter(invVar_tab,Parameter_dic[Symbol(:life,capaItr)],Set_dic,:life)
            end

            # adds investment cost and respective interest rate
            invCost_tab = matchSetParameter(lftm_tab,Parameter_dic[Symbol(:costInv,capaItr)],Set_dic,:invCost)
            itrRate_tab = matchSetParameter(invCost_tab,Parameter_dic[Symbol(:rateInv,capaItr)],Set_dic,:itrRate)

            # compute annuity costs and periods where it is payed
            annCost_arr = DB.select(itrRate_tab,:invCost) .* ((1 .+ DB.select(itrRate_tab,:itrRate)) .^ DB.select(itrRate_tab,:life) .* DB.select(itrRate_tab,:itrRate)) ./ ((1 .+ DB.select(itrRate_tab,:itrRate)) .^ DB.select(itrRate_tab,:life) .- 1)
            annCost_tab = IT.transform(DB.select(itrRate_tab,DB.Not(All(:itrRate,:invCost))),:costAnn => annCost_arr)
            invTsup_arr = DB.select(annCost_tab,(:Ts_inv,:life) => x -> filter(y -> (tsYear_dic[y] > tsYear_dic[x.Ts_inv]-ShortInvest_int) && (tsYear_dic[y] <= tsYear_dic[x.Ts_inv]+x.life),supDis_arr))
            annCost2_tab = DB.flatten(IT.transform(DB.select(annCost_tab,DB.Not(All(:life))),:Ts_supDis => invTsup_arr),:Ts_supDis)

            # joins discount factors, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = DB.rename(DB.join(annCost2_tab,Parameter_dic[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFac)
                push!(costInvExpr_arr,dot(DB.select(discFac_tab,:var),DB.select(discFac_tab,:disFac) .* DB.select(discFac_tab,:costAnn)))
            else
                discFac1_tab = DB.rename(DB.join(annCost2_tab,Parameter_dic[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacA)
                discFac_tab = DB.rename(DB.join(discFac1_tab,Parameter_dic[:discFac].data; lkey = (:R_b, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacB)
                push!(costInvExpr_arr,dot(DB.select(discFac_tab,:var),(0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:oprCost)))
            end
        end
    end
    if !isempty(costInvExpr_arr) exprCost_dic[:totCostInv] = costInvExpr_arr end
    # </editor-fold>

    # <editor-fold desc="add array for operation costs expression to dictionary"
    capaTyp_sym =  DecommMethod != :none ? :capaComm : :capa
    costOprExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    for capaItr in (:Conv,:StIn,:StOut,:StSize,:Exc)
        if Symbol(:costOpr,capaItr) in keys(Parameter_dic)
            # matches variables with cost parameter
            oprCost_tab = matchSetParameter(Variable_dic[Symbol(capaTyp_sym,capaItr)].data,Parameter_dic[Symbol(:costOpr,capaItr)],Set_dic,:oprCost)
            # joins discount factors, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = DB.rename(DB.join(oprCost_tab,Parameter_dic[:discFac].data; lkey = (:R_inv, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFac)
                push!(costOprExpr_arr,dot(DB.select(discFac_tab,:var),DB.select(discFac_tab,:disFac) .* DB.select(discFac_tab,:oprCost)))
            else
                discFac1_tab = DB.rename(DB.join(oprCost_tab,Parameter_dic[:discFac].data; lkey = (:R_a, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacA)
                discFac_tab = DB.rename(DB.join(discFac1_tab,Parameter_dic[:discFac].data; lkey = (:R_b, :Ts_supDis), rkey = (:R_inv, :Ts_supDis), how = :inner),:val => :disFacB)
                push!(costOprExpr_arr,dot(DB.select(discFac_tab,:var),(0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:oprCost)))
            end
        end
    end

    if !isempty(costOprExpr_arr) exprCost_dic[:totCostOpr] = costOprExpr_arr end

    # </editor-fold>
    # TODO speed up durch sort, nochmal durchgehen/überdenken, zu ende kommentieren
    # <editor-fold desc="add array for variable costs expression to dictionary"
    #=costGenExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()
    for capaItr in (:Use,:Gen,:StIn,:StOut,:StSize,:Exc)
        if Symbol(:costVar,capaItr) in keys(Parameter_dic)
            varNam_sym = Symbol(lowercase(String(capaItr)[1]),String(capaItr)[2:end])
            varDim_tup = tuple(filter(x -> !(x in (:Ts_supDis, :var)), collect(colnames(Variable_dic[varNam_sym].data)))...)

            if Symbol(:agg,capaItr) in keys(Equation_dic)
                # XXX assigns variable and variable cost to each other
                # changes inheritance rules and matches variables being aggregated with parameter costs
                Parameter_dic[Symbol(:costVar,capaItr)].inherit = (:Ts_inv => :uni_full, :Ts_dis => :uni_full, :R_dis => :uni_full, :C  => :uni_full, :Te => :uni_full)
                aggVar_tab = matchSetParameter(DB.select(Equation_dic[Symbol(:agg,capaItr)].data,DB.Not(All(:eqn))),Parameter_dic[Symbol(:costVar,capaItr)],Set_dic,:costVar)

                # kann nicht leer sein
                aggFull_tab = DB.join(aggVar_tab,Variable_dic[varNam_sym].data; lkey = varDim_tup, rkey = varDim_tup, how = :inner)

                # identify variables that are not included via aggregation above by matching values with aggregation dictionaries
                varLen_arr = collect(1:length(Variable_dic[varNam_sym].data))
                varId_tab = IT.transform(Variable_dic[varNam_sym].data,:id => varLen_arr)
                aggRow_arr = DB.select(DB.join(aggVar_tab,varId_tab; lkey = varDim_tup, rkey = varDim_tup, how = :inner),:id)

                bla = map(x -> AggDis_dic[Symbol(:agg,capaItr)][x],aggRow_arr)

                noAggVar_tab = Variable_dic[varNam_sym].data[setdiff(varLen_arr,union(aggRow_arr,bla...))]

                # joint zeug, das nicht über agg geht
                Parameter_dic[Symbol(:costVar,capaItr)].inherit = (:R_dis => :avg_any,:Ts_dis => :avg_any)
                noAggFull_tab = matchSetParameter(noAggVar_tab,Parameter_dic[Symbol(:costVar,capaItr)],Set_dic,:costVar)
                varCostFull_tab = DB.merge(aggFull_tab,noAggFull_tab)
            else
                varCostFull_tab = matchSetParameter(Variable_dic[varNam_sym].data,Parameter_dic[Symbol(:costVar,capaItr)],Set_dic)
            end

            # joins discount factors, in case of exchange costs the average of both regions is used, computes expression based on it
            if capaItr != :Exc
                discFac_tab = matchSetParameter(DB.rename(varCostFull_tab,:R_dis => :R_inv),Parameter_dic[:discFac],Set_dic,:discFac)
                push!(costGenExpr_arr,dot(DB.select(discFac_tab,:var),DB.select(discFac_tab,:discFac) .* DB.select(discFac_tab,:costVar)))
            else
                discFac1_tab = DB.rename(matchSetParameter(DB.rename(varCostFull_tab,:R_a => :R_inv),Parameter_dic[:discFac],Set_dic,:discFacA),:R_inv => :R_a)
                discFac_tab = matchSetParameter(DB.rename(varCostFull_tab,:R_b => :R_inv),Parameter_dic[:discFac],Set_dic,:discFacA)
                push!(costOprExpr_arr,dot(DB.select(discFac_tab,:var),(0.5*DB.select(discFac_tab,:disFacA) .+  0.5*DB.select(discFac_tab,:disFacA)) .* DB.select(discFac_tab,:costVar)))
            end
        end
    end
    if !isempty(costGenExpr_arr) exprCost_dic[:totCostVar] = costGenExpr_arr end=#

    # </editor-fold>

    # <editor-fold desc="add array for trade costs expression to dictionary"
    costTrdExpr_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

    #for trdItr in (:Buy, :Sell)
    #    if Symbol(:trd,trdItr,:Prc) in keys(Parameter_dic)
    #        joinDim_tup = tuple(intersect((:Ts_dis,:R_dis,:C,:id),colnames(Variable_dic[Symbol(:trade,trdItr)].data))...)
    #        # adds trade variables with prices
    #        trdPrc_tab = DB.join(Variable_dic[Symbol(:trade,trdItr)].data,Parameter_dic[Symbol(:trd,trdItr,:Prc)].data;lkey = joinDim_tup,rkey = joinDim_tup, how = :inner)
    #        # adds discount factors
    #        discFac_tab = matchSetParameter(DB.rename(trdPrc_tab,:val => :prc, :Ts_dis => :Ts_supDis,:R_dis => :R_inv),Parameter_dic[:discFac],Set_dic,:discFac)
    #        push!(costTrdExpr_arr,dot(DB.select(discFac_tab,:var),DB.select(discFac_tab,:discFac) .* DB.select(discFac_tab,:prc)) * (trdItr == :Sell ? -1 : 1))
    #    end
    #end
    if !isempty(costTrdExpr_arr) exprCost_dic[:totCostTrd] = costTrdExpr_arr end
    # </editor-fold>

    # <editor-fold desc="creates cost variables and equations to define them"
    # create costs variable object
    info = VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)
    infoTrd = VariableInfo(false, NaN, false, NaN, false, NaN, false, NaN, false, false) # trade costs do not have a lower limit of zero, because sell reveneus can exceed costs
    objVar_arr = [JuMP.add_variable(global_mod, JuMP.build_variable(error, name == :totCostTrd ? infoTrd : info),string(name)) for name in keys(exprCost_dic)]
    Variable_dic[:objVar] = VarElement(:objVar,tuple(),tuple(),table(objVar_arr; names = (:var,)))

    # create cost equations
    objEqn_arr = [@constraint(global_mod, objVar_arr[idx]== sum(exprCost_dic[key])) for (idx,key) in enumerate(keys(exprCost_dic))]
    Equation_dic[:objEqn] = EqnElement(:objEqn,tuple(),tuple(),table(objEqn_arr; names = (:eqn,)))
    # </editor-fold>
end
