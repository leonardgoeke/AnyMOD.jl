
function createAllVariables(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable})

    Variable_dic = Dict{Symbol,VarElement}()

    # <editor-fold desc="investment variables

    # create invest and capacity variables
    createVariable(:invest, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})
    createVariable(:capacity, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    # creates variables for actually commissioned capacities in case decommissioning is enabled
    if DecommMethod != :none createVariable(:commissioned, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement}) end

    # </editor-fold>

    # <editor-fold desc="dispatch variables"

    # XXX pre-sets parameter on dispatch level (in the course, mode variables are created and written into parameter dictionary)
    presetDispatchParameter(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    # XXX creates dispatch variables for technologies, trade and exchange
    createVariable(:dispatch, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})
    createVariable(:exchange, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})
    createVariable(:trade, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement})

    # </editor-fold>

    return Variable_dic, Parameter_dic
end

# XXX create variables
struct VarElement
    name::Symbol
    dim::Tuple
    grp::Tuple
    data::IndexedTable

    function VarElement(name::Symbol, dim::Tuple, grp::Tuple, data::IndexedTable)
        return new(name,dim,grp,data)
    end
end

createVariable(name::Symbol, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement}) =
                                        createVariable(Val{name}(), Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})
createVariable(name::Symbol, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement}) =
                                        createVariable(Val{name}(), Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement})
createVariable(name::Symbol, Variable_dic::Dict{Symbol,VarElement}) = createVariable(Val{name}(), Variable_dic::Dict{Symbol,VarElement})

# <editor-fold desc="creation of investment variables"
# XXX creates investment variables,
# variables are not created if invest is limited to zero or if invest depends on other variable (this can be the case for storage output and size, then the relation is directly written into the table)
function createVariable(name::Val{:invest}, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    # <editor-fold desc="XXX initialization"
    # reads data from inputs

    # initializes assignments and data
    invTypes_dic = Dict(:invConv => :invConv, :invStIn => :invSt, :invStOut => :invSt, :invStSize => :invSt, :invExc => :invExc)
    invTypeRatio_tup = (:invStIn => :stInToConv, :invStOut => :stOutToStIn, :invStSize => :stSizeToStIn)

    dimConv_tup = colnames(Mapping_dic[:invConv])
    dimSt_tup   = colnames(Mapping_dic[:invSt])

    defPar_tup = tuple(collect(keys(Parameter_dic))...)
    removeTab_dic = Dict{Symbol,Array{IndexedTable,1}}(x => [] for x in keys(invTypes_dic))

    # </editor-fold>

    # <editor-fold desc="XXX identify cases of restricted invesment"
    # filters and saves invest variables that are fixed to zero
    # filters cases where expansion is fixed to zero
    for invVar in keys(invTypes_dic)
        varFix_sym = Symbol(invVar,:Fix)
        if varFix_sym in defPar_tup
            push!(removeTab_dic[invVar], DB.select(filter(r -> r.val == 0, matchSetParameter(Mapping_dic[invTypes_dic[invVar]], Parameter_dic[varFix_sym], Set_dic)),DB.Not(All(:val))))
        end
    end

    # filters cases where storage investment is controlled via a fixed ratio
    stRatio_dic = Dict{Symbol,IndexedTable}()
    for invRatio in invTypeRatio_tup
        if invRatio[2] in defPar_tup
            rmvTab_tab = filter(r -> r.val != 0, matchSetParameter(Mapping_dic[invTypes_dic[invRatio[1]]], Parameter_dic[invRatio[2]], Set_dic))
            push!(removeTab_dic[invRatio[1]], DB.select(rmvTab_tab,DB.Not(All(:val))))
            stRatio_dic[invRatio[1]] = rmvTab_tab

            # writes a report, if limits (upper/lower/fixed) on the storage expansion were ignored due to the ratios provided
            if !isempty(rmvTab_tab)
                limVal_tab = matchLimitParameter(IT.transform(rmvTab_tab,:var => fill(nothing,length(rmvTab_tab))::Array{Nothing,1}),invRatio[1],Set_dic,Parameter_dic,false)
                if limVal_tab != nothing
                    uniOver_arr = unique(DB.select(limVal_tab,:Te))
                    for i in uniOver_arr
                        if invRatio[1] == :invStIn
                            push!(Report_df,(1,:var,:invest,"in at least one case for $(createFullString(i,Set_dic[:Te])) limits (fixed/lower and/or upper) for storage input were ignored since an conversion/storage input ratio was provided"))
                        elseif invRatio[1] == :invStOut
                            push!(Report_df,(2,:var,:invest,"in at least one case for $(createFullString(i,Set_dic[:Te])) limits (fixed/lower and/or upper) for storage output were ignored since an input/output ratio was provided"))
                        elseif invRatio[1] == :invStSizeFix
                            push!(Report_df,(1,:var,:invest,"in at least one case for $(createFullString(i,Set_dic[:Te])) limits (fixed/lower and/or upper) for storage size were ignored since an input/size ratio was provided"))
                        end
                    end
                end
            end
        end
    end
    # </editor-fold>

    # <editor-fold desc="XXX creates variables and adds expressions for dependant investment"
    # removes all entries filtered earlier and create all variables for table
    invVar_tab = Dict(x => createVarTable(removeEntries(removeTab_dic[x],Mapping_dic[invTypes_dic[x]]),nothing,string(x)) for x in keys(invTypes_dic))

    # XXX add entries where investment is defined via a ratio, (loop sorted to start with input, because it affects subsequents entries)
    for rmvRatio in sort(collect(keys(stRatio_dic)))
        if rmvRatio == :invStIn
            join_tab = DB.join(stRatio_dic[rmvRatio],invVar_tab[:invConv]; lkey = dimConv_tup, rkey = dimConv_tup, how = :inner)
        else
            join_tab = DB.join(stRatio_dic[rmvRatio],invVar_tab[:invStIn]; lkey = dimSt_tup, rkey = dimSt_tup, how = :inner)
        end
        invVar_tab[rmvRatio] = DB.merge(invVar_tab[rmvRatio],IT.transform(DB.select(join_tab,DB.Not(All(:val,:var))),:var => DB.select(join_tab,(:val,:var) => x ->x.val*x.var)::Array{GenericAffExpr{Float64,VariableRef},1}))
    end

    # XXX add entries where investment is interpolated between periods, because some investments level are above supordinate dispatch level
    # gets supordinate dispatch timesteps
    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]

    # creates dictionary of all timesteps used for investment and their "year" starting at zero
    tsYear_dic = Dict(zip(supDis_arr,collect(0:ShortInvest_int:(length(supDis_arr)-1)*ShortInvest_int)))
    remInvTs_arr = vcat(map(x -> Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== x,:idx],filter(x -> x != lvlSupDis_int,unique(Mapping_dic[:C_lvl].columns.lvlTsInv)))...)

    # adds other investment timesteps to dictionary
    for invIdx in remInvTs_arr
        tsYear_dic[invIdx] = tsYear_dic[getChildren(invIdx,Set_dic[:Ts],false,lvlSupDis_int)[1]]
    end

    # filters technologies with investment timesteps above supordinate dispatch level
    otherTech_arr = DB.select(filter(r -> r.invLvl.Ts != lvlSupDis_int,Mapping_dic[:TechInfo]),(:Te))
    # assigns supordinate dispatch timesteps to higher investment steps
    invSupTs_dic = Dict(x => (InterCapaMethod == :linear ?
                                filter(y -> tsYear_dic[y] == tsYear_dic[x] || (Set_dic[:Ts][x,:sub_id] == 1 ? false : (tsYear_dic[y] > tsYear_dic[x-1] && tsYear_dic[y] <= tsYear_dic[x])),supDis_arr)
                                        : filter(y -> tsYear_dic[y] == tsYear_dic[x],supDis_arr)) for x in remInvTs_arr)

    # assigns number of assigned supordinate dispatch timesteps to higher investment steps
    cntInvSupTs_dic = Dict(x => convert(Int16,length(invSupTs_dic[x])) for x in keys(invSupTs_dic))

    for invVar in keys(invTypes_dic)
        if invVar != :invExc    filtInvVar_tab = filter(r -> r.Te in otherTech_arr,invVar_tab[invVar])
        else                    filtInvVar_tab = filter(s -> DB.select(filter(r -> r.C == s.C,Mapping_dic[:C_lvl]),:lvlTsInv)[1] != lvlSupDis_int,invVar_tab[invVar]) end

        if isempty(filtInvVar_tab) continue end
        # gets array of supordinate dispatch timestep and its length for each row
        subDisTs_arr::Union{Array{Int16,1},Array{Array{Int16,1},1}} = DB.select(filtInvVar_tab,:Ts_inv => x -> invSupTs_dic[x])
        cntSubDisTs_arr::Array{Float64,1} = DB.select(filtInvVar_tab,:Ts_inv => x -> cntInvSupTs_dic[x])
        # adds arrays to table and uses them to finally comput investment expression
        joinVar_tab  = DB.flatten(IT.transform(filtInvVar_tab,:Ts_inv2 => subDisTs_arr,:factor => cntSubDisTs_arr),:Ts_inv2)
        joinVarNew_tab = DB.rename(DB.select(joinVar_tab,DB.Not(All(:Ts_inv))),:Ts_inv2 => :Ts_inv)
        finalVar_tab = IT.transform(DB.select(joinVarNew_tab,DB.Not(All(:var,:factor))), :var => DB.select(joinVarNew_tab,(:var, :factor) => x -> x.var/x.factor)::Array{GenericAffExpr{Float64,VariableRef},1})

        # removes manipulated entries from original table and merges remaing files with new entries for final table
        colName_tup = colnames(Mapping_dic[invTypes_dic[invVar]])
        invVar_tab[invVar] = DB.merge(finalVar_tab, DB.join(invVar_tab[invVar], table(rows(filtInvVar_tab)); lkey = colName_tup, rkey = colName_tup, how = :anti))
    end

    # XXX finally adds data to variable dictionary
    for invVar in keys(invTypes_dic)
        dim_tup = colnames(Mapping_dic[invTypes_dic[invVar]])
        Variable_dic[invVar] = VarElement(invVar, dim_tup, (:invest,), reindex(invVar_tab[invVar],dim_tup))
    end
    # </editor-fold>
end

# XXX creates variables for capacities installed
# variables are only created if capacity is not limited to zero and, in case of non-investment technologies, if residual capacities are specified
function createVariable(name::Val{:capacity}, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    # <editor-fold desc="XXX initialization"
    # reads data from inputs

    # initializes assignments and data
    capaTypes_dic = Dict(:capaConv => :capaConv, :capaStIn => :capaSt, :capaStOut => :capaSt, :capaStSize => :capaSt, :capaExc => :capaExc)
    capaTypeRatio_tup = (:capaStIn => :stInToConv, :capaStOut => :stOutToStIn, :capaStSize => :stSizeToStIn)

    dimSt_tup = colnames(Mapping_dic[:capaSt])

    defPar_tup = tuple(collect(keys(Parameter_dic))...)
    removeTab_dic = Dict{Symbol,Array{IndexedTable,1}}(x => [] for x in keys(capaTypes_dic))

    # seperates capacity variables relating to stock and non-stock technologies
    stockTech_arr = DB.select(filter(r -> r.type == 0,Mapping_dic[:TechData]),:Te)
    stockConv_tab = filter(r -> r.Te in stockTech_arr, Mapping_dic[:capaConv])
    stockSt_tab = filter(r -> r.Te in stockTech_arr, Mapping_dic[:capaSt])

    noStock_dic = Dict(x => filter(r -> !(r.Te in stockTech_arr), Mapping_dic[x]) for x in (:capaConv,:capaSt))
    noStock_dic[:capaExc] = Mapping_dic[:capaExc]
    # </editor-fold>

    # <editor-fold desc="XXX handling of stock technologies"

    # filters stock technologies with residual values provided and saves these in dictionary
    stockData_dic::Dict{Symbol,Union{Nothing,IndexedTable}} = Dict(x => checkResiCapa(Symbol(x,:Resi),occursin("Conv",string(x)) ? stockConv_tab : stockSt_tab,defPar_tup, Set_dic ,Parameter_dic)
                                                                                                                                    for x in filter(x -> x != :capaExc,collect(keys(capaTypes_dic))))
    stockData_dic[:capaExc] = nothing
    # reports if storage input to conversion ratio was provided for stock technologies
    if :stInToConv in defPar_tup
        for i in intersect(stockTech_arr,DB.select(Parameter_dic[:stInToConv].data,:Te))
            push!(Report_df,(2,:var,:stockStorage,"$(createFullString(i,Set_dic[:Te])) is a technology without investment and a ratio for conversion to input storage capacity was provided, this is not supported and values were ignored"))
        end
    end

    # report on stock technologies with specified output and/or size but no input
    for cap in intersect(keys(stockData_dic),(:capaStOut,:capaStSize))
        stockTab_tab = stockData_dic[cap]
        if stockTab_tab == nothing continue end
        if !isempty(stockTab_tab)
            withoutIn_tab = DB.join(stockTab_tab,stockData_dic[:capaStIn]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti)
            for i in unique(DB.select(withoutIn_tab,:Te))
                push!(Report_df,(2,:var,:stockStorage,"$(createFullString(i,Set_dic[:Te])) is a technology with storage, but without investment and has residual capacities for storage output and/or size, but not for storage input, it was ignored as a result"))
            end
        end
    end

    # search for ratios provided for storage of stock technologies and report on their usage (similar to hardcoded example above for the storage input to conversion ratio)
    if stockData_dic[:capaStIn] != nothing
        fltStockStIn_tab = DB.select(stockData_dic[:capaStIn],DB.Not(All(:val)))
        unmatchedTech_dic = Dict{Symbol,Array{Int16,1}}()
        for stockRatio in (:stOutToStIn => :capaStOut, :sizeToStIn => :capaStSize)
            if stockRatio[1] in defPar_tup
                # checks if input ratio was provided for stock technologies
                ratioVal_tab = DB.join((l,r) -> (val = l.var * r.val,),DB.rename(stockData_dic[:capaStIn],:val => :var),
                                            filter(r -> r.val != 0,matchSetParameter(fltStockStIn_tab,Parameter_dic[stockRatio[1]],Set_dic)); lkey = dimSt_tup, rkey = dimSt_tup,how =:inner)

                # uses ratios where no stock data was provided and reports on it
                ratioUsed_tab = DB.join(ratioVal_tab,stockData_dic[stockRatio[2]]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti)
                stockData_dic[stockRatio[2]] = DB.merge(ratioUsed_tab,stockData_dic[stockRatio[2]])
                unmatchedTech_dic[stockRatio[2]] = unique(DB.select(DB.join(stockData_dic[:capaStIn],stockData_dic[stockRatio[2]]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti),:Te))

                # reports the number of cases where a  ratio was provided bot not used in the end
                unusedRatio_int = length(ratioVal_tab) - length(ratioUsed_tab)
                if unusedRatio_int != 0
                    reportCase_str = occursin("Out",string(stockRatio[1])) ? "output capacity" : "size"
                    push!(Report_df,(2,:var,:stockStorage,"in $(unusedRatio_int) case(s) a ratio and a residual capacity were provided to control the storage $reportCase_str of a technology with storage, but without investment, in these case the residual capacity is used"))
                end
            end
        end

        # reports on cases where a stock technology had a storage input but output and/or size could not be assigned
        for tech in keys(unmatchedTech_dic)
            typeError_str = (tech == :stOutToStIn) ? "size" : "output capacity"
            for i in unmatchedTech_dic[tech]
                push!(Report_df,(2,:var,:stockStorage,"$(createFullString(i,Set_dic[:Te])) is technology with storage, but without investment, but storage $typeError_str were not specified in any way, hence the technology was ignored"))
            end
        end
    end
    # </editor-fold>

    # <editor-fold desc="XXX handling of investment technologies"
    # filters and removes non-stock variables that are fixed to zero, if any storage type (input,output,size) is fixed to zero remove whole storage

    # filters cases where expansion is fixed to zero
    for capVar in keys(capaTypes_dic)
        varFix_sym = Symbol(capVar,:Fix)
        if varFix_sym in defPar_tup push!(removeTab_dic[capVar], DB.select(filter(r -> r.val == 0, matchSetParameter(Mapping_dic[capaTypes_dic[capVar]], Parameter_dic[varFix_sym], Set_dic)),DB.Not(All(:val)))) end
    end

    # </editor-fold>

    # creates final variable table by merging data for stock and non-stock values and add to dictionary afterwards
    capaVar_dic = Dict(x => reindex(createVarTable(removeEntries(removeTab_dic[x],noStock_dic[capaTypes_dic[x]]),stockData_dic[x],string(x)),colnames(Mapping_dic[capaTypes_dic[x]])) for x in keys(capaTypes_dic))
    for capaVar in keys(capaTypes_dic)
        dim_tup = colnames(Mapping_dic[capaTypes_dic[capaVar]])
        Variable_dic[capaVar] = VarElement(capaVar, dim_tup, (:invest,), reindex(capaVar_dic[capaVar],dim_tup))
    end
end

# XXX creates new variable every invesment variable to reflect commissioned value
function createVariable(name::Val{:commissioned}, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement})

    dimConv_tup, dimSt_tup, dimExc_tup = [Variable_dic[x].dim for x in (:capaConv, :capaStIn, :capaExc)]

    defPar_tup = tuple(collect(keys(Parameter_dic))...)

    # XXX creates commissioned capacity variables for conversion and exchange
    Variable_dic[:capaCommConv] = VarElement(:capaCommConv, dimConv_tup, (:invest,), createVarTable(DB.select(Variable_dic[:capaConv].data,DB.Not(All(:var))),nothing,string(:capaCommConv)))
    Variable_dic[:capaCommExc]  = VarElement(:capaCommExc,  dimExc_tup, (:invest,),  createVarTable(DB.select(Variable_dic[:capaExc].data,DB.Not(All(:var))),nothing,string(:capaCommExc)))

    # XXX creates commissioned capacity variables for storage
    # if storage investment is controlle via a ratio, the same needs to apply for decommissioning, dic defines how this is done
    contrRatio_dic = Dict(:StIn => (ratio = :stInToConv, contr = :Conv), :StOut => (ratio = :stOutToStIn, contr = :StIn), :StSize => (ratio = :sizeToStIn, contr = :StIn))

    for typ in sort(collect(keys(contrRatio_dic)))
        capaVar_tab = DB.select(Variable_dic[Symbol(:capa,typ)].data,DB.Not(All(:var)))
        varName_sym = Symbol(:capaComm,typ)

        # checks if parameter ratio is defined controlling the investment for specific storage type
        if contrRatio_dic[typ].ratio in defPar_tup
            # finds cases were storage investment is controlled via ratio and creates functions based on ratio value and controll variable
            relJoin_tup = typ == :StIn ? dimConv_tup : dimSt_tup
            capaVarRatioVal_tab = matchSetParameter(capaVar_tab, Parameter_dic[contrRatio_dic[typ].ratio], Set_dic)
            if !(:C in relJoin_tup)
                capaVarRatio_tab = DB.join((l,r) -> (C = l.C, var = l.val * r.var),capaVarRatioVal_tab,Variable_dic[Symbol(:capaComm,contrRatio_dic[typ].contr)].data; lkey = relJoin_tup, rkey = relJoin_tup, how = :inner)
            else
                capaVarRatio_tab = DB.join((l,r) -> (var = l.val * r.var,),capaVarRatioVal_tab,Variable_dic[Symbol(:capaComm,contrRatio_dic[typ].contr)].data; lkey = relJoin_tup, rkey = relJoin_tup, how = :inner)
            end

            capaVarNoRatio_tab = createVarTable(rmvDummyCol(DB.join(addDummyCol(capaVar_tab),capaVarRatio_tab; lkey = dimSt_tup, rkey = dimSt_tup, how = :anti)),nothing,string(varName_sym))

            Variable_dic[varName_sym] = VarElement(varName_sym,dimSt_tup,(:dispatch,), DB.reindex(DB.merge(capaVarNoRatio_tab,capaVarRatio_tab),dimSt_tup))
        else
            Variable_dic[varName_sym] = VarElement(varName_sym,dimSt_tup,(:dispatch,), createVarTable(capaVar_tab,nothing,string("capaComm",typ)))
        end
    end
end
# </editor-fold>

# <editor-fold desc="pre-setting and mode related stuff"
# XXX replaces parameter values orginally read-in with fixed value on potential dispatch levels and identifies cases where modes are used
# done to avoid potential inconsistencies (for example different values efficiency on hourly and daily level)
function presetDispatchParameter(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})


    # <editor-fold desc="initialize and define variables"
    # maps parameters to pre-setting types
    preTypePar_dic = Dict(y => filter(x -> isdefined(Parameter_dic[x],:presetLvl) && Parameter_dic[x].presetLvl == y, keys(Parameter_dic)) for y in (:reference, :lowest, :highest, :carrier, :exchange, :trade))

    # creates dictionaries with carriers and their temporal and spatial dispatch level
    cLvlTs_dic, cLvlR_dic = [Dict(zip(DB.select(Mapping_dic[:C_lvl],:C),DB.select(Mapping_dic[:C_lvl],dim))) for dim in (:lvlTsDis, :lvlRDis)]

    # counts number of modes per tech to identify ill defined values later
    techM_tab = filter(r -> r.M != [0],Mapping_dic[:TechInfo])
    techCntM_dic::Dict{Int16,Int16} = Dict(x => length(DB.select(filter(y -> y.Te == x,techM_tab),:M)[1]) for x in unique(DB.select(techM_tab,:Te)))

    # creates dictionary that assigns combination of supordinate dispatch timestep and dispatch level to dispatch timesteps
    supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== maximum(Mapping_dic[:C_lvl].columns.lvlTsInv) ,:idx]
    allLvlTsDis_arr = unique(DB.select(Mapping_dic[:C_lvl],:lvlTsDis))
    tsSupDisLvl_dic = Dict((x[1], x[2]) => convert(Array{Int16,1},Set_dic[:Ts][x[1],:lvl] == x[2] ? [x[1]] : getChildren(x[1],Set_dic[:Ts],false,x[2])) for x in Iterators.product(supDis_arr,allLvlTsDis_arr))

    # creates dictionary that assigns combination of investment region and dispatch level to dispatch region
    allRInv_arr = unique(union(DB.select(Mapping_dic[:capaConv],(:R_inv)),DB.select(Mapping_dic[:capaSt],(:R_inv))))
    allLvlR_arr = unique(union(DB.select(Mapping_dic[:C_lvl],:lvlRDis),DB.select(Mapping_dic[:C_lvl],:lvlRInv)))
    rInvLvl_dic = Dict((x[1], x[2]) => convert(Int16,Set_dic[:R][x[1],:lvl] <= x[2] ? x[1] : getHeritanceLine(x[1],Set_dic[:R],x[2])) for x in Iterators.product(allRInv_arr,allLvlR_arr))

    # filter relevant technology data and dictionary to save mode related entries
    techAll_tab = DB.select(Mapping_dic[:TechInfo],(:refLvl,:allCar,:M,:Te))
    ModeParameter_dic = Dict{Symbol,IndexedTable}()

    # </editor-fold>

    # <editor-fold desc="pre-setting for exchange and trade parameters"
    if !isempty(preTypePar_dic[:exchange])
        # find all carriers that can be exchanged and adds their dispatch level to capacity table
        lvlTsDisC_dic = Dict(car => DB.select(filter(r -> r.C == car,Mapping_dic[:C_lvl]),:lvlTsDis)[1] for car in unique(DB.select(Variable_dic[:capaExc].data,:C)))
        capaDispLvl_tab = IT.transform(DB.select(Variable_dic[:capaExc].data,DB.Not(All(:var))),:lvlTs => DB.select(Variable_dic[:capaExc].data,:C => r -> lvlTsDisC_dic[r]))

        # flattens table based on this supordinate dispatch timesteps
        exc_tab = DB.flatten(DB.select(IT.transform(DB.select(capaDispLvl_tab,DB.Not(All(:lvlTs))),:Ts_dis => DB.select(capaDispLvl_tab,(:Ts_supDis,:lvlTs) => x -> tsSupDisLvl_dic[(x.Ts_supDis, x.lvlTs)])),DB.Not(All(:Ts_supDis))),:Ts_dis)

        foreach(x -> resetParameter(exc_tab,x,Set_dic,Parameter_dic),preTypePar_dic[:exchange])
    end

    if !isempty(preTypePar_dic[:trade])
        trd_tab = rmvDummyCol(reindex(addDummyCol(expandSetColumns(DB.rename(DB.select(Mapping_dic[:C_lvl],(:C,:lvlTsDis,:lvlRDis)),:lvlTsDis => :Ts_dis,:lvlRDis => :R_dis),(:Ts_dis,:R_dis),Set_dic)),(:Ts_dis,:R_dis,:C)))
        foreach(x -> resetParameter(trd_tab,x,Set_dic,Parameter_dic),preTypePar_dic[:trade])
    end
    # </editor-fold>

    # <editor-fold desc=" pre-setting for technology parameters"

    # XXX pre-setting on reference level
    if !isempty(preTypePar_dic[:reference])
        # gets reference levels of all technologies that actually convert something
        techRef_tab = DB.select(filter(r -> :use in keys(r.allCar) && :gen in keys(r.allCar), techAll_tab),DB.Not(All(:allCar)))
        techRefJoinLvl_tab = DB.select(IT.transform(techRef_tab,:lvlTs => map(x -> x.Ts, DB.select(techRef_tab,:refLvl)),:lvlR => map(x -> x.R, DB.select(techRef_tab,:refLvl))),DB.Not(All(:refLvl)))
        # expands to all actual dispatch timesteps on reference level with and without mode dimension
        techRefDisp_tab = expandInvestToDisp(DB.join(techRefJoinLvl_tab,Variable_dic[:capaConv].data; lkey = :Te, rkey = :Te, rselect = DB.Not(All(:var)), how = :inner),tsSupDisLvl_dic,rInvLvl_dic,false)
        refM_tab, refNoM_tab = [DB.flatten(techRefDisp_tab,:M), DB.select(techRefDisp_tab,DB.Not(All(:M)))]

        for x in preTypePar_dic[:reference]
            resetParameter((:M in colnames(Parameter_dic[x].data)) ? refM_tab : refNoM_tab, x , Set_dic, Parameter_dic, ModeParameter_dic, techCntM_dic, (:Ts_dis => :up, :R_dis => :up))
        end
    end

    # XXX pre-setting on highest or lowest level across all conversion parameters
    if !isempty(preTypePar_dic[:lowest])
        # gets lowest dispatch levels for technlogy and joins with acutal capacity data
    	techConvAll_tab = filter(r -> !isempty(intersect(keys(r.allCar),(:use,:gen))),techAll_tab)
        tsLvl_arr, rLvl_arr = [DB.select(techConvAll_tab,:allCar => r -> maximum(map(z -> dic[z],vcat(map(x -> getproperty(r,x),intersect(keys(r),(:gen,:use)))...)))) for dic in (cLvlTs_dic,cLvlR_dic)]
        invLowDisLvl_tab = DB.join(IT.transform(DB.select(techConvAll_tab,DB.Not(All(:allCar,:refLvl))),:lvlTs => tsLvl_arr, :lvlR => rLvl_arr),Variable_dic[:capaConv].data; lkey = :Te, rkey = :Te, rselect = DB.Not(All(:var)), how = :inner)
        # expands to all actual dispatch timesteps on reference level with and without mode dimension
        dispVar_tab = expandInvestToDisp(invLowDisLvl_tab,tsSupDisLvl_dic,rInvLvl_dic)
        lowM_tab, lowNoM_tab = [DB.flatten(dispVar_tab,:M), DB.select(dispVar_tab,DB.Not(All(:M)))]

        for x in preTypePar_dic[:lowest]
            resetParameter((:M in colnames(Parameter_dic[x].data)) ? lowM_tab : lowNoM_tab, x, Set_dic, Parameter_dic, ModeParameter_dic, techCntM_dic,(:Ts_dis => :avg_any, :R_dis => :avg_any))
        end
    end

    # XXX pre-setting on the respective level of each carrier
    if !isempty(preTypePar_dic[:carrier])
        # seperates between storage parameters and non-storage parameters
        for str in (true, false)
            carType_tup = str ? (:stExtIn, :stExtOut) : (:gen,:use) # differentiation between storage and no-storage, because different carriers are relevant
            techCar_tab =  IT.transform(DB.select(techAll_tab,DB.Not(All(:allCar))),:C => DB.select(techAll_tab,:allCar => r -> unique(vcat(map(x -> getproperty(r,x),intersect(keys(r),carType_tup))...))))
            techCarFlat_tab = DB.flatten(techCar_tab,:C)
            # create table of all combinations on carrier level, but at least on reference level (other conflicts emerge when writing equations)
            techCarLvl_tab = IT.transform(techCarFlat_tab, :lvlTs    => DB.select(techCarFlat_tab,(:C,:refLvl) => r -> convert(Int16,max(r.refLvl == nothing ? 0 : r.refLvl[:Ts],cLvlTs_dic[r.C]))),
                                                                      :lvlR     => DB.select(techCarFlat_tab,(:C,:refLvl) => r -> convert(Int16,max(r.refLvl == nothing ? 0 : r.refLvl[:R],cLvlR_dic[r.C]))))

            # expands to all actual dispatch timesteps on reference level with and without mode dimension
    		join_tup = str ? (:C,:Te) : (:Te,)
            dispVar_tab =  expandInvestToDisp(DB.join(DB.select(techCarLvl_tab,DB.Not(All(:refLvl))),Variable_dic[str ? :capaStIn : :capaConv].data; lkey = join_tup, rkey = join_tup, rselect = DB.Not(All(:var)), how = :inner),tsSupDisLvl_dic,rInvLvl_dic)
            carM_tab, carNoM_tab = [DB.flatten(dispVar_tab,:M), DB.select(dispVar_tab,DB.Not(All(:M)))]

            for x in filter(x -> !str || (occursin("St",string(x)) || string(x)[1:2] == "st"), preTypePar_dic[:carrier])
                resetParameter((:M in colnames(Parameter_dic[x].data)) ? carM_tab : carNoM_tab, x, Set_dic, Parameter_dic, ModeParameter_dic, techCntM_dic)
            end
        end
    end
    # </editor-fold>

    # <editor-fold desc="merges all modes cases to one table"
    # filter tech/mode combinations from mapping
    techWithMode_tab = DB.flatten(DB.select(filter(r -> r.M != Int16[0],Mapping_dic[:TechInfo]),(:M,:Te)),:M)
    # assinges dispatch variables to groups of capacity constraints
    assConvRestr_dic = Dict(:in => :use, :out => :gen)

    # merges entries of all parameter entrie with modes specified into one mapping table, that is used to create dispatch variables later
    modDim_tup = (:Ts_dis, :R_dis, :C, :Te, :M, :cnstrType)
    allModesMerged_tab = table(Int16[], Int16[], Int16[], Int16[], Int16[], Symbol[], names = modDim_tup)
    modeDim_tup = (:Ts_dis, :R_dis, :C, :Te)
    for parMode in keys(ModeParameter_dic)
        # adds all respective carriers, if parameter itself is not mode dependant (case for availabity for example)
        if !(:C in Parameter_dic[parMode].dim)
            # assigns combination of technology and in/out to array of relevant carrier
            relTe_tab = DB.select(DB.filter(r -> r.Te in unique(DB.select(ModeParameter_dic[parMode],:Te)),Mapping_dic[:TechInfo]),(:Te,:allCar))
            teCar_dic = Dict((assConvRestr_dic[b] in keys(a.allCar)) ? (a.Te,b) => getproperty(a.allCar,assConvRestr_dic[b]) : nothing => nothing
                                                                                                            for a = rows(relTe_tab), b = intersect((:in,:out),Parameter_dic[parMode].modeDep))
            # extends table according to dictionary written earlier
            ModeParameter_dic[parMode] = DB.flatten(IT.transform(ModeParameter_dic[parMode],:C => map(x -> teCar_dic[(x.Te, x.cnstrType)],DB.select(ModeParameter_dic[parMode],(:Te,:cnstrType)))),:C)
        end

        allModesMerged_tab = DB.select(joinMissing(allModesMerged_tab,ModeParameter_dic[parMode],modDim_tup,modDim_tup,:outer,(convert(Int16,0),)),DB.Not(All(:Ts_inv)))
    end

    Mapping_dic[:modeCases] = allModesMerged_tab
    # </editor-fold>
end

# XXX sets parameter data to values that could be matched with input table
function resetParameter(newData_tab::IndexedTable,par_sym::Symbol,Set_dic::Dict{Symbol,DataFrame},Parameter_dic::Dict{Symbol,ParElement},
                                                ModeParameter_dic::Union{Nothing,Dict{Symbol,IndexedTable}}=nothing,techCntM_dic::Union{Nothing,Dict{Int16,Int16}}=nothing, newInherit_tup::Tuple = ())


    # if, inherit is already set to new value, this indicates parameter was already reset and function can be skipped
    if Parameter_dic[par_sym].inherit != newInherit_tup

        # gets dimension of search tables and parameter without mode
        dimNoM_tup = tuple(filter(x -> x != :M ,collect(Parameter_dic[par_sym].dim))...)

        if !(:M in colnames(newData_tab)) || unique(DB.select(newData_tab,:M)) == Int16[0]
            # in case modes are not being searched for just directly set data
            matchData_tab = matchSetParameter(newData_tab,Parameter_dic[par_sym],Set_dic)
            Parameter_dic[par_sym].data = reindex(matchData_tab,tuple(intersect(colnames(matchData_tab),dimNoM_tup)...))
        else

            # looks up original table without applying default values
            matchData1_tab = matchSetParameter(newData_tab,Parameter_dic[par_sym],Set_dic,:val,false)

            # filter returned table by weather a mode was specified
            noMode_tab = DB.filter(r -> r.M == 0,matchData1_tab)
            mode_tab = DB.filter(r -> r.M != 0,matchData1_tab)

            # groups mode related data for further analysis
            resDim_tup = tuple(filter(x -> x != :M ,intersect(Parameter_dic[par_sym].dim,colnames(matchData1_tab)))...)

            modeGrp_tab = DB.groupby(mode_tab, resDim_tup, usekey = false; select = (:val, :M, :Te)) do y
                NamedTuple{(:M,:val,:cntM)}(length(unique(y.val)) == 1 ? tuple(Array(y.M),y.val[1],length(y.M)) : tuple(Array(y.M),Array(y.val),length(y.M)))
            end

            # removes entries where there is no parameter value for every mode and reports on it
            modeGrpDef_tab = filter(r -> techCntM_dic[r.Te] == r.cntM ,modeGrp_tab)
            if length(modeGrp_tab) > length(modeGrpDef_tab)
                missTechM_tab = DB.groupby(length,DB.join(modeGrp_tab,modeGrpDef_tab; lkey = resDim_tup, rkey = resDim_tup, how = :anti),:Te)
                for row in rows(missTechM_tab)
                    push!(Report_df,(2, :modes, par_sym, "parameter data was not specified for all modes in $(row.length) cases for $(createFullString(row.Te,Set_dic[:Te],true)), existing values were ignored"))
                end
            end

            # filters entries where mode values are not distinct, reports on it and uses these entries as non-mode specific data
            noModeDis_tab = filter(r -> typeof(r.val) == Array ,DB.select(modeGrpDef_tab,DB.Not(All(:cntM))))
            if !isempty(noModeDis_tab)
                disTechM_tab = DB.groupby(length,DB.join(modeGrpDef_tab,noModeDis_tab; lkey = resDim_tup, rkey = resDim_tup, how = :anti),:Te)
                for row in rows(disTechM_tab)
                    push!(Report_df,(2, :modes, par_sym, "parameter data was the same for all modes in $(row.length) cases for $(createFullString(row.Te,Set_dic[:Te],true)), no differentiation between modes was applied in these cases"))
                end
                noMode_tab = DB.merge(noMode_tab,noModeDis_tab)
            end

            # filters data where distinct mode data is provided for all modes and expends resulting table again
            finalModeGrp_tab = sort(DB.filter(r -> techCntM_dic[r.Te] == r.cntM, modeGrp_tab))
            leng_arr = DB.select(finalModeGrp_tab,:cntM)
            finalMode_tab = IT.transform(table(vcat(fill.(DB.select(finalModeGrp_tab,DB.Not(All(:cntM,:M,:val))),leng_arr)...)),:M => vcat(DB.select(finalModeGrp_tab,:M)...),:val => vcat(DB.select(finalModeGrp_tab,:val)...))

            # gets all data, where no values where obtained successfully yet and look them up again applying the default value and not specifing the mode anymore
            # (hence now non mode-specific parameter values for technologies with modes are taken into account => mode-specific parameter values overwrite non-mode specific parameter values)
            newSearch_tab = table(DB.unique(DB.join(newData_tab,!isempty(finalMode_tab) ? DB.merge(DB.select(noMode_tab,DB.Not(All(:val))),DB.select(finalMode_tab,DB.Not(All(:val)))) : DB.select(noMode_tab,DB.Not(All(:val)));
                                                                                                                                lkey = resDim_tup, rkey = resDim_tup, lselect = resDim_tup, how = :anti)))
            if !isempty(newSearch_tab)
                matchData2_tab = matchSetParameter(IT.transform(newSearch_tab,:M => fill(0,length(newSearch_tab))),Parameter_dic[par_sym],Set_dic)
                if !isempty(matchData2_tab) noMode_tab = DB.merge(matchData2_tab,noMode_tab) end
            end

            # returns tables with and without mode data to respective dictionaries, if there is no data for both deletes element
            if isempty(noMode_tab) && isempty(finalMode_tab)
                 delete!(Parameter_dic,par_sym); return
            else
                Parameter_dic[par_sym].data = reindex(merge(noMode_tab,finalMode_tab),resDim_tup)
            end
            # saves all dimensions with a mode to seperate dictionary for later use
            if !isempty(finalMode_tab)
                # assinges dispatch variables to a group of dispatch variables
                assDisGrp_dic = Dict(:gen => (:out,), :use => (:in,), :stIntIn => (:stIn,:stSize), :stExtIn => (:stIn,:stSize), :stIntOut => (:stOut,:stSize), :stExtOut => (:stOut,:stSize))
                # identifies what kind of capacity restrictions (in,out,stIn,...) apply for each occuring technology to expand table accordingly
                capaRestr_dic = Dict(x.Te => intersect(Parameter_dic[par_sym].modeDep,union(map(y -> assDisGrp_dic[y],keys(x.allCar))...))
                                                                                            for x in DB.filter(r -> r.Te in unique(DB.select(finalMode_tab,:Te)),Mapping_dic[:TechInfo]))

                ModeParameter_dic[par_sym] = DB.flatten(IT.transform(DB.select(finalMode_tab,DB.Not(All(:val))),:cnstrType => map(x -> capaRestr_dic[x], DB.select(finalMode_tab,:Te))),:cnstrType)
            end
        end
        # sets new inherit rules and default value
        Parameter_dic[par_sym].dim = dimNoM_tup
        Parameter_dic[par_sym].inherit = newInherit_tup
    end
end
# </editor-fold>

# <editor-fold desc="creation of dispatch variables"
# XXX creates all dispatch variables
function createVariable(name::Val{:dispatch},Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    # assignes capacity to dispatch variables
    assCapaDis_dic = Dict(:capaConv => (:use, :gen), :capaStIn => (:stIntIn, :stExtIn), :capaStOut => (:stIntOut, :stExtOut), :capaStSize => (:stSize,))
    # assinges dispatch variables to a group of dispatch variables
    assDisGrp_dic = Dict(:gen => :out, :use => :in, :stIntIn => :stIn, :stExtIn => :stIn, :stIntOut => :stOut, :stExtOut => :stOut, :stSize => :stSize)

    # gets table of capacity variables
    dispVarTab_dic = Dict(x => filter(r -> r.varType in assCapaDis_dic[x],Mapping_dic[:dispVar]) for x in keys(assCapaDis_dic))
    lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
    dim_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te)

    for varGrp in keys(dispVarTab_dic)

        # creates empty variable object if no capacity variables of this group exist
        if isempty(Variable_dic[varGrp].data)
            for var_ in assCapaDis_dic[varGrp] VarElement(var_, dim_tup, (:dispatch,), table()) end
            continue
        end

        # joins capacity variables with dispatch levels
        key_tup = varGrp == :capaConv ?  (:Te,) : (:C, :Te)
        varLvl_tab = DB.join(Variable_dic[varGrp].data,table(rows(dispVarTab_dic[varGrp])); lkey = key_tup, rkey = key_tup, lselect = DB.Not(All(:var)), how = :inner)

        # cheks for cases where storage balance is created on the supordiante dispatch level (like one equation per each year)
        stOnSupDisp = filter(r -> r.lvlTs == lvlSupDis_int,varLvl_tab)
        if !isempty(stOnSupDisp)
            for te in unique(DB.select(stOnSupDisp,:Te))
                push!(Report_df,(2,:eqn,:stBal,"for technology $(createFullString(te,Set_dic[:Te])) storage balance is created on the supordinate dispatch level, i.e. no actual storage can take place"))
            end
        end

        # assignes combination of investment or supordinate dispatch index and level to the relevant dispatch timeteps
        tsSupDisLvl_dic::Dict{Tuple{Int16,Int16},Array{Int16,1}} = Dict((x.Ts_supDis, x.lvlTs) => Set_dic[:Ts][x.Ts_supDis,:lvl] == x.lvlTs ? [x.Ts_supDis] : getChildren(x.Ts_supDis,Set_dic[:Ts],false,x.lvlTs) for x in DB.unique(DB.select(varLvl_tab,(:Ts_supDis, :lvlTs))))
        rInvLvl_dic::Dict{Tuple{Int16,Int16},Int16}     = Dict((x.R_inv, x.lvlR) => Set_dic[:R][x.R_inv,:lvl] == x.lvlR ? x.R_inv : getHeritanceLine(x.R_inv,Set_dic[:R],x.lvlR) for x in DB.unique(DB.select(varLvl_tab,(:R_inv, :lvlR))))

        for var_ in assCapaDis_dic[varGrp]

            varLvlSub_tab = DB.select(filter(r -> r.varType == var_, varLvl_tab),DB.Not(All(:varType)))

            # adds temporal and regional dispatch timesteps
            fullVar_tab = reindex(expandInvestToDisp(IT.transform(varLvlSub_tab,:M  => convert(Array{Int16,1},fill(0,length(varLvlSub_tab)))),tsSupDisLvl_dic,rInvLvl_dic,true),dim_tup)

            # filters rows where availability is zero and removes them from table to not generate these constraints
            rmv_tup = varGrp == :capaConv ? (:Ts_supDis,:C) : (:Ts_supDis,)
            join_tup = tuple(setdiff(colnames(fullVar_tab),rmv_tup)...)
            paraAva_sym = Symbol(replace(string(varGrp),"capa" => "ava"))
            fullVar_tab = DB.join(fullVar_tab,filter(r -> r.val == 0, matchSetParameter(DB.select(fullVar_tab,DB.Not(All(rmv_tup))),Parameter_dic[paraAva_sym],Set_dic)); lkey = join_tup, rkey = join_tup, how = :anti)


            # finds cases, where dispatch variables need to be mode specific and replaces them within table
            if !isempty(Mapping_dic[:modeCases])
                join_tup = (:Ts_dis, :R_dis, :C, :Te)
                # filters all modes cases relevant for the respective dispatch variables
                relModeVar_tab = DB.select(table(rows(DB.filter(r -> r.cnstrType == assDisGrp_dic[var_], Mapping_dic[:modeCases]))),DB.Not(All(:cnstrType)))
                # identifies relevant modes cases, adds them to all variables table while removing respective old entries without model
                modeCase_tab = join(fullVar_tab,relModeVar_tab; lkey = join_tup, rkey = join_tup ,how = :inner)
                fullVar_tab = merge(modeCase_tab,DB.join(fullVar_tab,modeCase_tab; lkey = join_tup, rkey = join_tup, how = :anti))
            end

            Variable_dic[var_] = VarElement(var_, dim_tup, (:dispatch,), createVarTable(fullVar_tab,nothing,String(var_)))
        end
    end
end

# XXX creates all exchange variables
function createVariable(name::Val{:exchange}, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Mapping_dic::Dict{Symbol,IndexedTable}, Variable_dic::Dict{Symbol,VarElement})

    excDim_tup = (:Ts_dis, :R_a, :R_b, :C)

    # find all carriers that can be exchanged and adds their dispatch level to capacity table
    lvlTsDisC_dic = Dict(car => DB.select(filter(r -> r.C == car,Mapping_dic[:C_lvl]),:lvlTsDis)[1] for car in unique(DB.select(Variable_dic[:capaExc].data,:C)))
    capaDispLvl_tab = IT.transform(DB.select(Variable_dic[:capaExc].data,DB.Not(All(:var))),:lvlTs => DB.select(Variable_dic[:capaExc].data,:C => r -> lvlTsDisC_dic[r]))

    # creates dictionary that assigns all actual dispatch timestep to combination of supordinate dispatch timesteps and dispatch level, expand and flattens variable table based on this
    tempSupDisLvl_dic = Dict((x.Ts_supDis, x.lvlTs) => Set_dic[:Ts][x.Ts_supDis,:lvl] == x.lvlTs ? [x.Ts_supDis] : getChildren(x.Ts_supDis,Set_dic[:Ts],false,x.lvlTs) for x in DB.unique(DB.select(capaDispLvl_tab,(:Ts_supDis, :lvlTs))))
    exc_tab = DB.flatten(IT.transform(DB.select(capaDispLvl_tab,DB.Not(All(:lvlTs))),:Ts_dis => DB.select(capaDispLvl_tab,(:Ts_supDis,:lvlTs) => x -> tempSupDisLvl_dic[(x.Ts_supDis, x.lvlTs)])),:Ts_dis)

    excVar_tab = reindex(createVarTable(DB.merge(DB.rename(exc_tab,:R_a => :R_from,:R_b => :R_to),DB.rename(exc_tab,:R_b => :R_from,:R_a => :R_to)),nothing,"exchange"),(:Ts_dis,:R_from,:R_to,:C))

    Variable_dic[:exchange] = VarElement(:exchange,(:Ts_dis,:R_from,:R_to,:C),(:dispatch,),excVar_tab)
end

# XXX creates all trade variables
function createVariable(name::Val{:trade}, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, Variable_dic::Dict{Symbol,VarElement})
    for typ in (:Buy, :Sell)
        if Symbol(:trd,typ,:Prc) in keys(Parameter_dic)
            Variable_dic[Symbol(:trade,typ)] = VarElement(Symbol(:trade,typ),(:Ts_dis, :R_dis, :C, :id) ,(:dispatch,),createVarTable(DB.select(Parameter_dic[Symbol(:trd,typ,:Prc)].data,DB.Not(All(:val))),nothing,string("trd",typ)))
        end
    end
end

# </editor-fold>

# <editor-fold desc="collection of subfunctions"

# XXX checks if residual capacities are provided for the stock technologies in allStockCapa_table
function checkResiCapa(resiPar_sym::Symbol, stockCapa_tab::IndexedTable, allPar_tup::Tuple, Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement})
    if resiPar_sym in allPar_tup
        # search for defined residual values
        stock_tab = filter(r -> r.val != 0, matchSetParameter(stockCapa_tab, Parameter_dic[resiPar_sym], Set_dic))
        if isempty(stock_tab)
            return nothing
        else
            return stock_tab
        end
    else
        # directly filter all if the the search parameter is not even defined
        return nothing
    end
end

# XXX merges variables and fixed residual capacities for stock technologies together into data table
function createVarTable(setData_tab::IndexedTable,resiData_tab::Union{IndexedTable,Nothing},name_str::String,lowerBound_flt::Float64 = 0.0)
    info::VariableInfo{Float64,Float64,Float64,Float64} = VariableInfo(true, lowerBound_flt, false, NaN, false, NaN, false, NaN, false, false)
    buildVar_svar = JuMP.build_variable(error, info::VariableInfo{Float64,Float64,Float64,Float64})
    if resiData_tab != nothing
        var_tab = IT.transform(setData_tab, :var => [JuMP.add_variable(global_mod::Model, JuMP.build_variable(error, info::VariableInfo{Float64,Float64,Float64,Float64}),name_str::String) for i = 1:length(setData_tab)]::Array{VariableRef,1})
        return DB.merge(DB.rename(resiData_tab, :val => :var),var_tab)
    else
        return IT.transform(setData_tab, :var => [JuMP.add_variable(global_mod::Model, buildVar_svar, name_str::String) for i = 1:length(setData_tab)]::Array{VariableRef,1})
    end
end
# </editor-fold>
