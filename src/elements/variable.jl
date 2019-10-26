
function addVariables!(anyM::anyModel)

    anyM.variables = Dict{Symbol,VarElement}()

    # <editor-fold desc="investment variables>
    # XXX prepares limit parameters by adding zero columns
    prepareLimitParameter!(anyM)

    # XXX create invest and capacity variables
    createVariable!(:invest,   anyM)
    createVariable!(:capacity, anyM)

    # creates variables for actually commissioned capacities in case decommissioning is enabled
    if anyM.options.decomm != :none createVariable!(:commissioned, anyM) end
    produceMessage(anyM.options,anyM.report, 2," - Created investment related variables")
    # </editor-fold>

    # <editor-fold desc="dispatch variables"

    # XXX pre-sets parameter on dispatch level (in the course, mode variables are created and written into parameter dictionary)
    prepareDispatchParameter!(anyM)
    produceMessage(anyM.options,anyM.report, 2," - Performed pre-setting of dispatch related parameters")

    # XXX creates dispatch variables for technologies, trade and exchange
    createVariable!(:techDispatch, anyM)
    createVariable!(:exchange, anyM)
    createVariable!(:trade, anyM)
    produceMessage(anyM.options,anyM.report, 2," - Created dispatch related variables")
    # </editor-fold>

    produceMessage(anyM.options,anyM.report, 1," - Created all variables")
end

createVariable!(name::Symbol, anyM::anyModel) = createVariable!(Val{name}(), anyM::anyModel)

# <editor-fold desc="pre-setting and mode related stuff"

# XXX extends limits parameters with zero values for all unset columns, necessary to set limits right
function prepareLimitParameter!(anyM::anyModel)
    # filter limit constraints and loops
    for limitPar in filter(x -> any(occursin.(("Low", "Up", "Fix"),string(x))), collect(keys(anyM.parameter)))
        parameter_obj = anyM.parameter[limitPar]
        # creates an empty dummy table and defines an array of zeros as a missing values
        empty_tab = table(fill(Int32[],length(parameter_obj.dim))... ; names = parameter_obj.dim)
        missVal_tup = tuple(fill(convert(Int32,0), length(parameter_obj.dim))...)
        # performs joint to add zero columns
        join_tup = tuple(filter(x -> x != :val, collect(colnames(parameter_obj.data)))...)
        anyM.parameter[limitPar].data = reindex(joinMissing(parameter_obj.data, empty_tab, join_tup, join_tup, :left, missVal_tup),parameter_obj.dim)
    end
end

# XXX replaces anyM.parameter values orginally read-in with fixed value on potential dispatch levels and identifies cases where modes are used
# done to avoid potential inconsistencies (for example different values efficiency on hourly and daily level)
function prepareDispatchParameter!(anyM::anyModel)

    # <editor-fold desc="initialize and define variables"
    # maps parameters to pre-setting types
    preTypePar_dic = Dict(y => filter(x -> isdefined(anyM.parameter[x],:presetLvl) && anyM.parameter[x].presetLvl == y, keys(anyM.parameter)) for y in (:reference, :lowest, :highest, :carrier, :exchange, :trade))

    # creates dictionaries with carriers and their temporal and spatial dispatch level
    cLvlTs_dic, cLvlR_dic = [Dict(zip(DB.select(anyM.mapping[:C_lvl],:C),DB.select(anyM.mapping[:C_lvl],dim))) for dim in (:lvlTsDis, :lvlRDis)]

    # counts number of modes per tech to identify ill defined values later
    techM_tab = filter(r -> r.M != [0],anyM.mapping[:TechInfo])
    techCntM_dic = convert(Dict{Int32,Int32},Dict(x => length(DB.select(filter(y -> y.Te == x,techM_tab),:M)[1]) for x in unique(DB.select(techM_tab,:Te))))

    # creates dictionary that assigns combination of supordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(DB.select(anyM.mapping[:C_lvl],:lvlTsDis))
    tsSupDisLvl_dic = Dict((x[1], x[2]) => convert(Array{Int32,1},anyM.sets[:Ts][x[1],:lvl] == x[2] ? [x[1]] : getChildren(x[1],anyM.sets[:Ts],false,x[2])) for x in Iterators.product(anyM.supDis.step,allLvlTsDis_arr))

    # creates dictionary that assigns combination of investment region and dispatch level to dispatch region
    allRInv_arr = unique(union(DB.select(anyM.mapping[:capaConv],(:R_inv)),DB.select(anyM.mapping[:capaSt],(:R_inv))))
    allLvlR_arr = unique(union(DB.select(anyM.mapping[:C_lvl],:lvlRDis),DB.select(anyM.mapping[:C_lvl],:lvlRInv)))
    rInvLvl_dic = Dict((x[1], x[2]) => convert(Int32,anyM.sets[:R][x[1],:lvl] <= x[2] ? x[1] : getHeritanceLine(x[1],anyM.sets[:R],x[2])) for x in Iterators.product(allRInv_arr,allLvlR_arr))

    # filter relevant technology data and dictionary to save mode related entries
    techAll_tab = DB.select(anyM.mapping[:TechInfo],(:refLvl,:allCar,:M,:Te))
    # </editor-fold>

    # <editor-fold desc="creates dimensions relevant for pre-setting depending on the used preset levels"
    presetDim_dic = Dict{Symbol,IndexedTable}()
    newHerit_dic = Dict{Symbol,Tuple}()
    # assignes type of presetting level to each used parameter
    parPreset_dic = Dict(x => anyM.parameter[x].presetLvl for x in filter(x -> isdefined(anyM.parameter[x],:presetLvl),keys(anyM.parameter)))

    # dimensions for all exchange variables
    if :exchange in values(parPreset_dic)
        # find all carriers that can be exchanged and adds their dispatch level to capacity table
        lvlTsDisC_dic = Dict(car => DB.select(filter(r -> r.C == car,anyM.mapping[:C_lvl]),:lvlTsDis)[1] for car in unique(DB.select(anyM.variables[:capaExc].data,:C)))
        capaDispLvl_tab = IT.transform(DB.select(anyM.variables[:capaExc].data,DB.Not(All(:var))), :lvlTs => DB.select(anyM.variables[:capaExc].data, :C => r -> lvlTsDisC_dic[r]))

        # flattens table based on this supordinate dispatch timesteps
        presetDim_dic[:exchange] = DB.flatten(DB.select(IT.transform(DB.select(capaDispLvl_tab,DB.Not(All(:lvlTs))),:Ts_dis => DB.select(capaDispLvl_tab,(:Ts_supDis,:lvlTs) => x -> tsSupDisLvl_dic[(x.Ts_supDis, x.lvlTs)])),DB.Not(All(:Ts_supDis))),:Ts_dis)
    end

    # dimensions for all trade variables
    if :trade in values(parPreset_dic)
        presetDim_dic[:trade] = rmvDummyCol(reindex(addDummyCol(expandSetColumns(DB.rename(DB.select(anyM.mapping[:C_lvl],(:C,:lvlTsDis,:lvlRDis)),:lvlTsDis => :Ts_dis,:lvlRDis => :R_dis),(:Ts_dis,:R_dis),anyM.sets)),(:Ts_dis,:R_dis,:C)))
    end

    # dimensions for technology variables on reference level
    if :reference in values(parPreset_dic)
        # gets reference levels of all technologies that actually convert something
        techRef_tab = DB.select(filter(r -> (:use in keys(r.allCar) && :gen in keys(r.allCar))  || :stIntIn in keys(r.allCar) || :stIntOut in keys(r.allCar), techAll_tab),DB.Not(All(:allCar)))
        techRefJoinLvl_tab = DB.select(IT.transform(techRef_tab,:lvlTs => map(x -> x.Ts, DB.select(techRef_tab,:refLvl)),:lvlR => map(x -> x.R, DB.select(techRef_tab,:refLvl))),DB.Not(All(:refLvl)))
        # expands to all actual dispatch timesteps on reference level with and without mode dimension
        techRefDisp_tab = expandInvestToDisp(DB.join(techRefJoinLvl_tab,anyM.variables[:capaConv].data; lkey = :Te, rkey = :Te, rselect = DB.Not(All(:var)), how = :inner),tsSupDisLvl_dic,rInvLvl_dic,false)

        presetDim_dic[:reference_mode], presetDim_dic[:reference] = [DB.flatten(techRefDisp_tab,:M), DB.select(techRefDisp_tab,DB.Not(All(:M)))]
        newHerit_dic[:reference] = (:Ts_dis => :up, :R_dis => :up)

        for type in (:use, :gen)
            teC_tab = flatten(IT.transform(DB.select(anyM.mapping[:TechInfo],(:Te,)),:C => DB.select(anyM.mapping[:TechInfo],:allCar => x -> type in keys(x) ? getproperty(x,type) : Int32[])),:C)
            presetDim_dic[Symbol(:reference_mode_,type)], presetDim_dic[Symbol(:reference_,type)] = [join(presetDim_dic[Symbol(:reference,x)],teC_tab; lkey = (:Te,), rkey = (:Te,), how = :left) for x in ("_mode", "")]
        end
    end

    # dimensions for technology variables on the lowest level (spatial and temporal) any variable is defined
    if :lowest in values(parPreset_dic)
        # gets lowest dispatch levels for technlogy and joins with acutal capacity data
        techConvAll_tab = filter(r -> !isempty(intersect(keys(r.allCar),(:use,:gen))),techAll_tab)
        # finds smallest temporal and spatial resolution among generated and used carriers
        tsLvl_arr, rLvl_arr = [DB.select(techConvAll_tab,:allCar => r -> maximum(map(z -> dic[z],vcat(map(x -> getproperty(r,x),intersect(keys(r),(:gen,:use)))...)))) for dic in (cLvlTs_dic,cLvlR_dic)]
        # ensures resolution found above is a least as detailed as the reference level
        tsLvl_arr, rLvl_arr = [max.(map(x -> x.Ts, DB.select(techConvAll_tab,:refLvl)),tsLvl_arr), max.(map(x -> x.R, DB.select(techConvAll_tab,:refLvl)),rLvl_arr)]

        invLowDisLvl_tab = DB.join(IT.transform(DB.select(techConvAll_tab,DB.Not(All(:allCar,:refLvl))),:lvlTs => tsLvl_arr, :lvlR => rLvl_arr),anyM.variables[:capaConv].data; lkey = :Te, rkey = :Te, rselect = DB.Not(All(:var)), how = :inner)
        # expands to all actual dispatch timesteps on reference level with and without mode dimension
        dispVar_tab = expandInvestToDisp(invLowDisLvl_tab,tsSupDisLvl_dic,rInvLvl_dic)
        presetDim_dic[:lowest_mode], presetDim_dic[:lowest] = [DB.flatten(dispVar_tab,:M), DB.select(dispVar_tab,DB.Not(All(:M)))]
        newHerit_dic[:lowest] = (:Ts_dis => :avg_any, :R_dis => :avg_any)
    end

    # dimensions for technology variables on the level of each individual carrier
    if :carrier in values(parPreset_dic)
        # seperates between storage parameters and non-storage parameters
        for str in (true, false)
            carType_tup = str ? (:stExtIn, :stExtOut) : (:gen,:use) # differentiation between storage and no-storage, because different carriers are relevant
            techCar_tab =  IT.transform(DB.select(techAll_tab,DB.Not(All(:allCar))),:C => DB.select(techAll_tab,:allCar => r -> unique(vcat(map(x -> getproperty(r,x),intersect(keys(r),carType_tup))...))))
            techCarFlat_tab = DB.flatten(techCar_tab,:C)
            # create table of all combinations on carrier level, but at least on reference level (other conflicts emerge when writing constraints)
            techCarLvl_tab = IT.transform(techCarFlat_tab, :lvlTs    => DB.select(techCarFlat_tab,(:C,:refLvl) => r -> convert(Int32,max(r.refLvl == nothing ? 0 : r.refLvl[:Ts],cLvlTs_dic[r.C]))),
                                                                      :lvlR     => DB.select(techCarFlat_tab,(:C,:refLvl) => r -> convert(Int32,max(r.refLvl == nothing ? 0 : r.refLvl[:R],cLvlR_dic[r.C]))))

            # expands to all actual dispatch timesteps on reference level with and without mode dimension
            join_tup = str ? (:C,:Te) : (:Te,)
            dispVar_tab =  expandInvestToDisp(DB.join(DB.select(techCarLvl_tab,DB.Not(All(:refLvl))),anyM.variables[str ? :capaStIn : :capaConv].data;
                                                                                                lkey = join_tup, rkey = join_tup, rselect = DB.Not(All(:var)), how = :inner),tsSupDisLvl_dic,rInvLvl_dic)
            presetDim_dic[Symbol(:carrier_mode,str ? :_st : "")], presetDim_dic[Symbol(:carrier,str ? :_st : "")] = [DB.flatten(dispVar_tab,:M), DB.select(dispVar_tab,DB.Not(All(:M)))]
        end
        newHerit_dic[:carrier] = tuple()
    end

    # dimensions for technology variables on the finest level on the use or generation side suitable
    for type in (:gen, :use)
        presetLvl_sym = Symbol(:all, uppercase(string(type)[1]), string(type)[2:end])
        if presetLvl_sym in values(parPreset_dic)
            # filter technologies with multiple fuels
            teC_tab = filter(r -> type in keys(r.allCar) && length(r.allCar[type]) > 1,anyM.mapping[:TechInfo]) |> y -> IT.transform(DB.select(y,(:Te,:invLvl,:M)),:C => map(x -> sort(x[type]), DB.select(y,:allCar)))
            teC2_tab = IT.transform(DB.select(teC_tab,DB.Not(All(:invLvl))),:lvlR_inv => map(x -> x.R, DB.select(teC_tab,:invLvl)))

            # get dispatch resolution per technology defined for all carriers
            srtDispVar_tab = DB.groupby(DB.filter(r -> r.varType == type, anyM.mapping[:dispVar]), (:lvlTs, :lvlR, :Te), usekey = false; select = :C) do y
                NamedTuple{(:C,)}(tuple(Array(sort(y))))
            end

            # filters technologies with multiple fuels
            dispAllC_tab = DB.join(teC2_tab, srtDispVar_tab, ; lkey = (:Te,:C), rkey = (:Te,:C), how = :inner)
            dispMergeAllC_tab  = IT.transform(DB.select(dispAllC_tab,DB.Not(All(:lvlTs,:lvlR))),:lvl => map(z -> (z.lvlTs, z.lvlR), rows(DB.select(dispAllC_tab,(:lvlTs, :lvlR)))))

            # finds finest resolution suited (e.g. no "crossing")
            finReso_tab = DB.groupby(dispMergeAllC_tab, (:C,:Te, :lvlR_inv, :M), usekey = false; select = :lvl) do y
                filt1_arr = filter(x -> !(any(map(z -> (x[1] > z[1] && x[2] < z[2]) || (x[2] > z[2] && x[1] < z[1]), y))),y) # filters cases where resolutions "cross"
                filt2_arr = filter(x -> !(any(map(z -> (x[1] <= z[1] && x[2] < z[2]) || (x[1] < z[1] && x[2] <= z[2]), filt1_arr))), filt1_arr) # filters dominated cases from remaining entries
                return NamedTuple{(:lvlTs,:lvlR)}(tuple(filt2_arr[1][1], filt2_arr[1][2]))
            end

            # adds investment region and timesteps to table
            invReso_tab = IT.transform(DB.select(finReso_tab,DB.Not(All(:lvlR_inv))),:R_inv => DB.select(finReso_tab,(:lvlR_inv) => x -> anyM.sets[:R][anyM.sets[:R][:,:lvl] .== x,:idx]),
                                                                                                                                            :Ts_supDis => fill(anyM.supDis.step ,length(finReso_tab)))
            # adds investment timesteps to table and expands
            addTsInv_tab = join(flatten(flatten(invReso_tab,:R_inv),:Ts_supDis),DB.select(anyM.variables[:capaConv].data,DB.Not(All(:var))); lkey = (:Ts_supDis, :R_inv, :Te), rkey = (:Ts_supDis, :R_inv, :Te), how = :inner)
            techRefDisp_tab = flatten(expandInvestToDisp(reindex(addTsInv_tab,(:Te,)), tsSupDisLvl_dic, rInvLvl_dic, false),:C)

            presetDim_dic[Symbol(presetLvl_sym,:_mode)], presetDim_dic[presetLvl_sym] = [DB.flatten(techRefDisp_tab,:M), DB.select(techRefDisp_tab,DB.Not(All(:M)))]
            newHerit_dic[presetLvl_sym] = tuple()
        end
    end

    # </editor-fold>

    # <editor-fold desc="excute sub-processes to preset"
    # initializes dictionaries for pre-setting process
    modeParameter_dic = Dict{Symbol,Union{Nothing,IndexedTable}}() # saves all mode dependant dimensions
    report_dic = Dict{Symbol,DataFrame}() # stores reporting of individual processes

    # performs parallel pre-setting process fo each paramter
    for par in keys(parPreset_dic)
        preKey_sym = (:M in colnames(anyM.parameter[par].data)) ?  Symbol(parPreset_dic[par],:_mode) : parPreset_dic[par] # adds "_mode" to key being looked up in presetDim_dic, if case is mode dependant
        if parPreset_dic[par] == :exchange  # in case of trade and exchange reset gets called without tech/mode dictionary
            newParameter, modeParameter_dic[par], report_dic[par] =  resetParameter(presetDim_dic[preKey_sym], anyM.parameter[par], anyM.sets, anyM.mapping[:TechInfo], anyM.options)
        elseif parPreset_dic[par] == :trade
            # adds id column, if non existing so far or rewrites values, to avoid zeroes outside of aggregated variables in this column
            if :id in colnames(anyM.parameter[par].data)
                # creates an default value for all entries without unspecified ids to avoid problems latter in code when aggregating trade variables
                idCol_arr = DB.select(anyM.parameter[par].data,:id)
                newIdCol_arr = convert(Array{Int32,1},map(x -> x != 0 ? x : maximum(idCol_arr) +1, DB.select(anyM.parameter[par].data,:id)))
            else
                newIdCol_arr = convert(Array{Int32,1},fill(1,length(anyM.parameter[par].data)))
            end
            anyM.parameter[par].data = IT.transform(anyM.parameter[par].data, :id => newIdCol_arr)
            newParameter, modeParameter_dic[par], report_dic[par] =  resetParameter(presetDim_dic[preKey_sym], anyM.parameter[par], anyM.sets, anyM.mapping[:TechInfo], anyM.options)
        else
            if parPreset_dic[par] == :carrier  # in case of carrier preset dim can differ depending on if it is a storage/non-storage case
                preKey_sym = occursin("St", String(par)) || par in (:stInflow,:stDis) ? Symbol(preKey_sym,:_st) : preKey_sym
                newParameter, modeParameter_dic[par], report_dic[par] =  resetParameter(presetDim_dic[preKey_sym], anyM.parameter[par], anyM.sets, anyM.mapping[:TechInfo], anyM.options, techCntM_dic)
            elseif parPreset_dic[par] == :reference # in case of reference preset, dim can differ depending on if parameter addresses gen/use side and is therefore dependant on carrier (like ratios) or not (like efficiency)
                preKey_sym = :C in anyM.parameter[par].dim ? Symbol(preKey_sym, occursin("Gen", String(par)) ? :_gen : use ) : preKey_sym
                newParameter, modeParameter_dic[par], report_dic[par] =
                                            resetParameter(presetDim_dic[preKey_sym], anyM.parameter[par], anyM.sets, anyM.mapping[:TechInfo], anyM.options, techCntM_dic, newHerit_dic[parPreset_dic[par]])
            else
                newParameter, modeParameter_dic[par], report_dic[par] =
                                            resetParameter(presetDim_dic[preKey_sym], anyM.parameter[par], anyM.sets, anyM.mapping[:TechInfo], anyM.options, techCntM_dic, newHerit_dic[parPreset_dic[par]])
            end
        end
        # either deletes parameter completely, if resetParameter returned no newParameter or replaces object entirely if something was returned
        isnothing(newParameter) ?  Base.delete!(anyM.parameter,x) : anyM.parameter[par] = newParameter
        produceMessage(anyM.options,anyM.report, 3," - Performed pre-setting of $(par) parameter")
    end

    # merges different reports of subprocesses into main report again
    anyM.report = vcat(anyM.report,values(report_dic)...)
    # </editor-fold>

    # <editor-fold desc="merges all modes cases to one table"
    # filter tech/mode combinations from mapping
    techWithMode_tab = DB.flatten(DB.select(filter(r -> r.M != Int32[0],anyM.mapping[:TechInfo]),(:M,:Te)),:M)
    # assinges dispatch variables to groups of capacity constraints
    assConvRestr_dic = Dict(:in => :use, :out => :gen)

    # merges entries of all parameter entries with modes specified into one mapping table, that is used to create dispatch variables later
    modDim_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M, :cnstrType)
    allModesMerged_tab = table(Int32[], Int32[], Int32[], Int32[], Int32[], Int32[], Symbol[], names = modDim_tup)
    for parMode in keys(modeParameter_dic)
        if !isnothing(modeParameter_dic[parMode])
            # adds all respective carriers, if parameter itself is not mode dependant (case for availabity for example)
            if !(:C in anyM.parameter[parMode].dim)
                # assigns combination of technology and in/out to array of relevant carrier
                relTe_tab = DB.select(DB.filter(r -> r.Te in unique(DB.select(modeParameter_dic[parMode],:Te)),anyM.mapping[:TechInfo]),(:Te,:allCar))
                teCar_dic = Dict((assConvRestr_dic[b] in keys(a.allCar)) ? (a.Te,b) => getproperty(a.allCar,assConvRestr_dic[b]) : nothing => nothing                                                                                    for a = rows(relTe_tab), b = intersect((:in,:out),anyM.parameter[parMode].modeDep))
                # extends table according to dictionary written earlier
                modeParameter_dic[parMode] = DB.flatten(IT.transform(modeParameter_dic[parMode],:C => map(x -> teCar_dic[(x.Te, x.cnstrType)],DB.select(modeParameter_dic[parMode],(:Te,:cnstrType)))),:C)
            end
            allModesMerged_tab = joinMissing(allModesMerged_tab,modeParameter_dic[parMode],modDim_tup,modDim_tup,:outer,(convert(Int32,0),))
        end
    end

    anyM.mapping[:modeCases] = allModesMerged_tab
    # </editor-fold>
end
# </editor-fold>

# <editor-fold desc="creation of investment variables"
# XXX creates investment variables,
# variables are not created if invest is limited to zero or if invest depends on other variable (this can be the case for storage output and size, then the relation is directly written into the table)
function createVariable!(name::Val{:invest}, anyM::anyModel)

    # <editor-fold desc="initialization"
    # reads data from inputs

    # initializes assignments and data
    invTypes_dic = Dict(:invConv => :invConv, :invStIn => :invSt, :invStOut => :invSt, :invStSize => :invSt, :invExc => :invExc)
    invTypeRatio_tup = (:invStIn => :stInToConv, :invStOut => :stOutToStIn, :invStSize => :sizeToStIn)

    dimConv_tup = colnames(anyM.mapping[:invConv])
    dimSt_tup   = colnames(anyM.mapping[:invSt])

    defPar_tup = tuple(collect(keys(anyM.parameter))...)
    removeTab_dic = Dict{Symbol,Array{IndexedTable,1}}(x => [] for x in keys(invTypes_dic))

    # </editor-fold>

    # <editor-fold desc="identify cases of restricted invesment"e
    # filters cases where investment is fixed to zero
    for invVar in keys(invTypes_dic)
        varFix_sym = Symbol(invVar,:Fix)
        if varFix_sym in defPar_tup
            push!(removeTab_dic[invVar], DB.select(filter(r -> r.val == 0, matchSetParameter(anyM.report,anyM.mapping[invTypes_dic[invVar]], anyM.parameter[varFix_sym], anyM.sets,anyM.options.digits.comp)),DB.Not(All(:val))))
        end
    end

    # filters cases where storage investment is controlled via a fixed ratio
    stRatio_dic = Dict{Symbol,IndexedTable}()
    for invRatio in invTypeRatio_tup
        if invRatio[2] in defPar_tup
            rmvTab_tab = filter(r -> r.val != 0, matchSetParameter(anyM.report,anyM.mapping[invTypes_dic[invRatio[1]]], anyM.parameter[invRatio[2]], anyM.sets,anyM.options.digits.comp))
            push!(removeTab_dic[invRatio[1]], DB.select(rmvTab_tab,DB.Not(All(:val))))
            stRatio_dic[invRatio[1]] = rmvTab_tab

            # writes a report, if limits (upper/lower/fixed) on the storage expansion were ignored due to the ratios provided
            if !isempty(rmvTab_tab)
                limVal_tab, dmy = matchLimitParameter(IT.transform(rmvTab_tab,:var => fill(nothing,length(rmvTab_tab))::Array{Nothing,1}),invRatio[1],anyM)
                if limVal_tab != nothing
                    uniOver_arr = unique(DB.select(limVal_tab,:Te))
                    for i in uniOver_arr
                        if invRatio[1] == :invStIn
                            push!(anyM.report,(1,:variable,:investment,"in at least one case for $(createFullString(i,anyM.sets[:Te])) limits (fixed/lower and/or upper) for storage input were ignored since an conversion/storage input ratio was provided"))
                        elseif invRatio[1] == :invStOut
                            push!(anyM.report,(2,:variable,:investment,"in at least one case for $(createFullString(i,anyM.sets[:Te])) limits (fixed/lower and/or upper) for storage output were ignored since an input/output ratio was provided"))
                        elseif invRatio[1] == :invStSizeFix
                            push!(anyM.report,(1,:variable,:investment,"in at least one case for $(createFullString(i,anyM.sets[:Te])) limits (fixed/lower and/or upper) for storage size were ignored since an input/size ratio was provided"))
                        end
                    end
                end
            end
        end
    end
    # </editor-fold>

    # <editor-fold desc="creates variables and adds expressions for dependant investment"
    # removes all entries filtered earlier and create all variables for table
    invVar_tab = Dict(x => createInvVar(anyM.optModel,removeEntries(removeTab_dic[x],anyM.mapping[invTypes_dic[x]]),nothing,string(x),anyM.options.bound.inv) for x in keys(invTypes_dic))

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

    # creates dictionary of all timesteps used for investment and their "year" starting at zero
    tsYear_dic = Dict(zip(anyM.supDis.step,collect(0:anyM.options.shortInvest:(length(anyM.supDis.step)-1)*anyM.options.shortInvest)))
    remInvTs_arr = vcat(map(x -> anyM.sets[:Ts][anyM.sets[:Ts][:,:lvl] .== x,:idx],filter(x -> x != anyM.supDis.lvl,unique(anyM.mapping[:C_lvl].columns.lvlTsInv)))...)

    # adds other investment timesteps to dictionary
    for invIdx in remInvTs_arr
        tsYear_dic[invIdx] = tsYear_dic[getChildren(invIdx,anyM.sets[:Ts],false,anyM.supDis.lvl)[1]]
    end

    otherTech_arr = DB.select(filter(r -> r.invLvl.Ts != anyM.supDis.lvl,anyM.mapping[:TechInfo]),(:Te)) # filters technologies with investment timesteps above supordinate dispatch level
    # assigns supordinate dispatch timesteps to higher investment steps
    invSupTs_dic = Dict(x => (anyM.options.interCapa == :linear ? #
                                filter(y -> tsYear_dic[y] == tsYear_dic[x] || (anyM.sets[:Ts][x,:sub_id] == 1 ? false : (tsYear_dic[y] > tsYear_dic[x-1] && tsYear_dic[y] <= tsYear_dic[x])),collect(anyM.supDis.step))
                                        : filter(y -> tsYear_dic[y] == tsYear_dic[x],collect(anyM.supDis.step))) for x in remInvTs_arr)
    cntInvSupTs_dic = Dict(x => convert(Int32,length(invSupTs_dic[x])) for x in keys(invSupTs_dic)) # assign number of assigned supordinate dispatch timesteps to higher investment steps

    # performs actaul interpolation
    for invVar in keys(invTypes_dic)
        if invVar != :invExc    filtInvVar_tab = filter(r -> r.Te in otherTech_arr,invVar_tab[invVar])
        else                    filtInvVar_tab = filter(s -> DB.select(filter(r -> r.C == s.C,anyM.mapping[:C_lvl]),:lvlTsInv)[1] != anyM.supDis.lvl,invVar_tab[invVar]) end

        if isempty(filtInvVar_tab) continue end
        # gets array of supordinate dispatch timestep and its length for each row
        subDisTs_arr = convert(Union{Array{Int32,1},Array{Array{Int32,1},1}}, DB.select(filtInvVar_tab,:Ts_inv => x -> invSupTs_dic[x]))
        cntSubDisTs_arr = convert(Array{Float64,1}, DB.select(filtInvVar_tab,:Ts_inv => x -> cntInvSupTs_dic[x]))
        # adds arrays to table and uses them to finally comput investment expression
        joinVar_tab  = DB.flatten(IT.transform(filtInvVar_tab,:Ts_inv2 => subDisTs_arr,:factor => cntSubDisTs_arr),:Ts_inv2)
        joinVarNew_tab = DB.rename(DB.select(joinVar_tab,DB.Not(All(:Ts_inv))),:Ts_inv2 => :Ts_inv)
        finalVar_tab = IT.transform(DB.select(joinVarNew_tab,DB.Not(All(:var,:factor))), :var => DB.select(joinVarNew_tab,(:var, :factor) => x -> x.var*round(1/x.factor,digits = anyM.options.digits.comp)))

        # removes manipulated entries from original table and merges remaing files with new entries for final table
        colName_tup = colnames(anyM.mapping[invTypes_dic[invVar]])
        invVar_tab[invVar] = DB.merge(finalVar_tab, DB.join(invVar_tab[invVar], table(rows(filtInvVar_tab)); lkey = colName_tup, rkey = colName_tup, how = :anti))
    end

    # </editor-fold>

    # <editor-fold desc="finds required aggregations and creates variable objects"
    for invVar in keys(invTypes_dic)
        invData_tab = invVar_tab[invVar]
        dim_tup = colnames(anyM.mapping[invTypes_dic[invVar]])
        anyM.variables[invVar] = VarElement(invVar, dim_tup, reindex(invData_tab,dim_tup))
    end
    # </editor-fold>

    produceMessage(anyM.options,anyM.report, 3," - Created investment variables")
end

# XXX creates variables for capacities installed
# variables are only created if capacity is not limited to zero and, in case of non-investment technologies, if residual capacities are specified
function createVariable!(name::Val{:capacity}, anyM::anyModel)

    # <editor-fold desc="initialization"
    capaTypes_dic = Dict(:capaConv => :capaConv, :capaStIn => :capaSt, :capaStOut => :capaSt, :capaStSize => :capaSt, :capaExc => :capaExc)
    capaTypeRatio_tup = (:capaStIn => :stInToConv, :capaStOut => :stOutToStIn, :capaStSize => :sizeToStIn)
    dimSt_tup = colnames(anyM.mapping[:capaSt])

    defPar_tup = tuple(collect(keys(anyM.parameter))...)
    removeTab_dic = Dict{Symbol,Array{IndexedTable,1}}(x => [] for x in keys(capaTypes_dic))

    # seperates capacity variables relating to stock and non-stock technologies
    stockTech_arr = DB.select(filter(r -> r.type == 0,anyM.mapping[:TechData]),:Te)
    stockConv_tab = filter(r -> r.Te in stockTech_arr, anyM.mapping[:capaConv])
    stockSt_tab = filter(r -> r.Te in stockTech_arr, anyM.mapping[:capaSt])

    noStock_dic = Dict(x => filter(r -> !(r.Te in stockTech_arr), anyM.mapping[x]) for x in (:capaConv,:capaSt))
    noStock_dic[:capaExc] = anyM.mapping[:capaExc]
    # </editor-fold>

    # <editor-fold desc="handling of stock technologies"

    # filters stock technologies with residual values provided and saves these in dictionary
    stockData_dic = convert(Dict{Symbol,Union{Nothing,IndexedTable}},Dict(x => checkResiCapa(Symbol(x,:Resi),occursin("Conv",string(x)) ? stockConv_tab : stockSt_tab,defPar_tup, anyM)
                                                                                                                                    for x in filter(x -> x != :capaExc,collect(keys(capaTypes_dic)))))
    stockData_dic[:capaExc] = nothing
    # reports if storage input to conversion ratio was provided for stock technologies
    if :stInToConv in defPar_tup
        for i in intersect(stockTech_arr,DB.select(anyM.parameter[:stInToConv].data,:Te))
            push!(anyM.report,(2,:variable,:stockStorage,"$(createFullString(i,anyM.sets[:Te])) is a technology without investment and a ratio for conversion to input storage capacity was provided, this is not supported and values were ignored"))
        end
    end

    # report on stock technologies with specified output and/or size but no input
    for cap in intersect(keys(stockData_dic),(:capaStOut,:capaStSize))
        stockTab_tab = stockData_dic[cap]
        if stockTab_tab == nothing continue end
        if !isempty(stockTab_tab)
            withoutIn_tab = DB.join(stockTab_tab,stockData_dic[:capaStIn]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti)
            for i in unique(DB.select(withoutIn_tab,:Te))
                push!(anyM.report,(2,:variable,:stockStorage,"$(createFullString(i,anyM.sets[:Te])) is a technology with storage, but without investment and has residual capacities for storage output and/or size, but not for storage input, it was ignored as a result"))
            end
        end
    end

    # search for ratios provided for storage of stock technologies and report on their usage (similar to hardcoded example above for the storage input to conversion ratio)
    if stockData_dic[:capaStIn] != nothing
        fltStockStIn_tab = DB.select(stockData_dic[:capaStIn],DB.Not(All(:val)))
        unmatchedTech_dic = Dict{Symbol,Array{Int32,1}}()
        for stockRatio in (:stOutToStIn => :capaStOut, :sizeToStIn => :capaStSize)
            if stockRatio[1] in defPar_tup
                # checks if input ratio was provided for stock technologies
                ratioVal_tab = DB.join((l,r) -> (val = l.var * r.val,),DB.rename(stockData_dic[:capaStIn],:val => :var),
                                    filter(r -> r.val != 0,matchSetParameter(anyM.report, fltStockStIn_tab, anyM.parameter[stockRatio[1]], anyM.sets,anyM.options.digits.comp)); lkey = dimSt_tup, rkey = dimSt_tup,how =:inner)

                # uses ratios where no stock data was provided and reports on it
                ratioUsed_tab = DB.join(ratioVal_tab,stockData_dic[stockRatio[2]]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti)
                stockData_dic[stockRatio[2]] = DB.merge(ratioUsed_tab,stockData_dic[stockRatio[2]])
                unmatchedTech_dic[stockRatio[2]] = unique(DB.select(DB.join(stockData_dic[:capaStIn],stockData_dic[stockRatio[2]]; lkey = dimSt_tup, rkey = dimSt_tup,how =:anti),:Te))

                # reports the number of cases where a  ratio was provided bot not used in the end
                unusedRatio_int = length(ratioVal_tab) - length(ratioUsed_tab)
                if unusedRatio_int != 0
                    reportCase_str = occursin("Out",string(stockRatio[1])) ? "output capacity" : "size"
                    push!(anyM.report,(2,:variable,:stockStorage,"in $(unusedRatio_int) case(s) a ratio and a residual capacity were provided to control the storage $reportCase_str of a technology with storage, but without investment, in these cases the residual capacity is used"))
                end
            end
        end

        # reports on cases where a stock technology had a storage input but output and/or size could not be assigned
        for tech in keys(unmatchedTech_dic)
            typeError_str = (tech == :stOutToStIn) ? "size" : "output capacity"
            for i in unmatchedTech_dic[tech]
                push!(anyM.report,(2,:variable,:stockStorage,"$(createFullString(i,anyM.sets[:Te])) is technology with storage, but without investment, but storage $typeError_str were not specified in any way, hence the technology was ignored"))
            end
        end
    end
    # </editor-fold>

    # <editor-fold desc="handling of investment technologies"
    # filters and removes non-stock variables that are fixed to zero, if any storage type (input,output,size) is fixed to zero remove whole storage

    # filters cases where expansion is fixed to zero
    for capVar in keys(capaTypes_dic)
        varFix_sym = Symbol(capVar,:Fix)
        if varFix_sym in defPar_tup
            push!(removeTab_dic[capVar], DB.select(filter(r -> r.val == 0, matchSetParameter(anyM.report, anyM.mapping[capaTypes_dic[capVar]], anyM.parameter[varFix_sym], anyM.sets,anyM.options.digits.comp)),DB.Not(All(:val))))
        end
    end
    # </editor-fold>

    # <editor-fold desc="finds required aggregations and creates variable objects"
    # creates final variable table by merging data for stock and non-stock values and add to dictionary afterwards
    capaVar_dic = Dict(x => reindex(createInvVar(anyM.optModel,removeEntries(removeTab_dic[x],noStock_dic[capaTypes_dic[x]]),stockData_dic[x],string(x),anyM.options.bound.inv),colnames(anyM.mapping[capaTypes_dic[x]]))
                                                                                                                                                                                                    for x in keys(capaTypes_dic))
    for capaVar in keys(capaVar_dic)
        capaData_tab = capaVar_dic[capaVar]
        dim_tup = colnames(capaData_tab,DB.Not(:var))
        anyM.variables[capaVar] = VarElement(capaVar, dim_tup, reindex(capaData_tab,dim_tup))
    end
    # </editor-fold>
    produceMessage(anyM.options,anyM.report, 3," - Created capacity variables")
end

# XXX creates new variable every invesment variable to reflect commissioned value
function createVariable!(name::Val{:commissioned}, anyM::anyModel)

    # specific definitons required for cases where ratio among capacities of installed capacities is to be preserved within commissioned capacities
    dimConv_tup, dimSt_tup, dimExc_tup = [anyM.variables[x].dim for x in (:capaConv, :capaStIn, :capaExc)]
    defPar_tup = tuple(collect(keys(anyM.parameter))...)
    contrRatio_dic = Dict(:capaStIn => (ratio = :stInToConv, contr = :Conv), :capaStOut => (ratio = :stOutToStIn, contr = :StIn), :capaStSize => (ratio = :sizeToStIn, contr = :StIn))

    for capaVar in (:capaExc, :capaConv, :capaStIn, :capaStOut, :capaStSize)

        if !(capaVar in keys(anyM.variables)) continue end
        comm_sym = Symbol(replace(string(capaVar),"capa" => "capaComm"))

        # filter installed capacites that are only created to control a limit on installed capacities and create commissioning variable for remainging
        commCapa_tab = anyM.variables[capaVar].data[setdiff(collect(1:length(anyM.variables[capaVar].data)),collect(keys(anyM.aggVar[capaVar])))]

        # <editor-fold desc="filter cases were capacity is controlled by ratio"
        # e.g. if installed capacity of stOut is controlled by stIn relation must be preserved within commissioned capacities
        if capaVar in (keys(contrRatio_dic))
            # checks if anyM.parameter ratio is defined controlling the investment for specific storage type
            if contrRatio_dic[capaVar].ratio in defPar_tup
                # finds cases were storage investment is controlled via ratio and creates functions based on ratio value and controll variable
                relJoin_tup = capaVar == :capaStIn ? dimConv_tup : dimSt_tup
                capaVarRatioVal_tab = matchSetParameter(anyM.report,commCapa_tab, anyM.parameter[contrRatio_dic[capaVar].ratio], anyM.sets, anyM.options.digits.comp)
                if !(:C in relJoin_tup)
                    capaVarRatio_tab = DB.join((l,r) -> (C = l.C, var = l.val * r.var),capaVarRatioVal_tab,anyM.variables[Symbol(:capaComm,contrRatio_dic[capaVar].contr)].data; lkey = relJoin_tup, rkey = relJoin_tup, how = :inner)
                else
                    capaVarRatio_tab = DB.join((l,r) -> (var = l.val * r.var,),capaVarRatioVal_tab,anyM.variables[Symbol(:capaComm,contrRatio_dic[capaVar].contr)].data; lkey = relJoin_tup, rkey = relJoin_tup, how = :inner)
                end
                # replaces ratio controlled entries with original entries
                commCapa_tab = rmvDummyCol(DB.join(addDummyCol(commCapa_tab),capaVarRatio_tab; lkey = dimSt_tup, rkey = dimSt_tup, how = :anti))
            end
        end
        # </editor-fold>

        # <editor-fold desc="finds required aggregations due to limit constraints"
        dim_tup = colnames(commCapa_tab,DB.Not(:var))
        # finds provided limit constraints that can not be related to an existing variable
        relPar_arr = collect(intersect(keys(anyM.parameter), map(x -> Symbol(comm_sym,x),(:Fix, :Low, :Up))))
        noVarYet_tab = missDimLimits(DB.select(commCapa_tab,DB.Not(All(:var))),relPar_arr,anyM)
        # performs aggregation among all variable dimensions, entries for limit constraints that would not be controlled via aggregation are removed
        newVar_tab, anyM.aggVar[comm_sym] = aggregateSetTable(DB.select(commCapa_tab,DB.Not(All(:var))), dim_tup, tuple(), anyM.sets, noVarYet_tab, true)

        # merges any variables that have to be additionaly created for limit constraints
        if !(isnothing(newVar_tab))
            commCapa_tab = DB.merge(commCapa_tab, createInvVar(anyM.optModel, newVar_tab, nothing, String(comm_sym), anyM.options.bound.inv))
        end
        # </editor-fold>

        # XXX creates commissioned capacity variables for conversion and exchange
        varComm_tab = reindex(createInvVar(anyM.optModel,commCapa_tab,nothing,string(comm_sym),anyM.options.bound.inv),dim_tup)
        anyM.variables[comm_sym] = VarElement(comm_sym, dim_tup, varComm_tab)
    end

    produceMessage(anyM.options,anyM.report, 3," - Created commissioned variables")
end
# </editor-fold>

# <editor-fold desc="creation of dispatch variables"
# XXX creates all dispatch variables

# XXX create dispatch variables of technologies
function createVariable!(name::Val{:techDispatch}, anyM::anyModel)

    # XXX initializes variables
    # assigns capacity to types of dispatch variables
    capaType_dic = Dict(:capaConv => (:use, :gen), :capaStIn => (:stIntIn, :stExtIn), :capaStOut => (:stIntOut, :stExtOut), :capaStSize => (:stSize,))
    # assigns types of dispatch variables to group
    typeGrp_dic = Dict(:gen => :out, :use => :in, :stIntIn => :stIn, :stExtIn => :stIn, :stIntOut => :stOut, :stExtOut => :stOut, :stSize => :stSize)
    allDim_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M)

	varTabDic_dic = Dict{Symbol,Dict{Symbol,IndexedTable}}() # stores dictionaries of dimension tables returned from subfunction
	aggDicDic_dic = Dict{Symbol,Dict{Symbol,Dict{Int64,BitSet}}}() # stores dictionaries of aggregation dictionaries returned from subfunction
	report_dic = Dict{Symbol,DataFrame}() # stores reporting of individual function calls

	# XXX obtains tables of required dispatch entries and dictionaries of aggregations
	for capa in keys(capaType_dic)
		if isempty(anyM.variables[capa].data) continue end
		capaMapping_tab = filter(r -> r.varType in capaType_dic[capa],anyM.mapping[:dispVar])
		varTabDic_dic[capa], report_dic[capa] = getTechByCapa(capa, capaType_dic[capa], anyM.variables[capa].data, capaMapping_tab, anyM)
	end

	# XXX writes actual variable objects and saves aggregations to model structure
	for grp in keys(varTabDic_dic), type in keys(varTabDic_dic[grp])

        # internal storage variables should only be created, if a corresponding gen/use variable exists
        if type == :stIntIn
            varType_tab = join(varTabDic_dic[grp][type], varTabDic_dic[:capaConv][:gen]; lkey = allDim_tup, rkey = allDim_tup, how = :inner)
        elseif type == :stIntOut
            varType_tab = join(varTabDic_dic[grp][type], varTabDic_dic[:capaConv][:use]; lkey = allDim_tup, rkey = allDim_tup, how = :inner)
        else
            varType_tab = varTabDic_dic[grp][type]
        end

		var_tab = createDispVar(anyM.optModel, varType_tab, String(type), anyM.sets[:Ts], anyM.supDis, anyM.options.bound.disp, anyM.options.digits.comp)
		anyM.variables[type] = VarElement(type, allDim_tup, var_tab)
		produceMessage(anyM.options,anyM.report, 3," - Created dispatch variables for $(type)")
    end
end

# XXX creates all exchange variables
function createVariable!(name::Val{:exchange}, anyM::anyModel)

    # XXX initializes variables
    if isempty(anyM.variables[:capaExc].data) return end
    excDim_tup = (:Ts_dis, :R_a, :R_b, :C)

    # XXX create entries for original exchange variables
    # find all carriers that can be exchanged and adds their dispatch level to capacity table
    lvlTsDisC_dic = Dict(car => DB.select(filter(r -> r.C == car,anyM.mapping[:C_lvl]),:lvlTsDis)[1] for car in unique(DB.select(anyM.variables[:capaExc].data,:C)))
    capaDispLvl_tab = IT.reindex(IT.transform(DB.select(anyM.variables[:capaExc].data,DB.Not(All(:var))),:lvlTs => DB.select(anyM.variables[:capaExc].data,:C => r -> lvlTsDisC_dic[r])),(:R_a, :R_b))

    # creates dictionary that assigns all actual dispatch timestep to combination of supordinate dispatch timesteps and dispatch level, expand and flattens variable table based on this
    tempSupDisLvl_dic = Dict((x.Ts_supDis, x.lvlTs) => anyM.sets[:Ts][x.Ts_supDis,:lvl] == x.lvlTs ? [x.Ts_supDis] : getChildren(x.Ts_supDis,anyM.sets[:Ts],false,x.lvlTs) for x in DB.unique(DB.select(capaDispLvl_tab,(:Ts_supDis, :lvlTs))))
    exc_tab = DB.flatten(IT.transform(DB.select(capaDispLvl_tab,DB.Not(All(:Ts_supDis,:lvlTs))),:Ts_dis => DB.select(capaDispLvl_tab,(:Ts_supDis,:lvlTs) => x -> tempSupDisLvl_dic[(x.Ts_supDis, x.lvlTs)])),:Ts_dis)
    excFilter_tab = filter(r -> r.R_a != r.R_b,exc_tab)
    excRename_tab = DB.merge(DB.rename(excFilter_tab,:R_a => :R_from,:R_b => :R_to),DB.rename(excFilter_tab,:R_b => :R_from,:R_a => :R_to))

    # XXX adds further entries for aggregation and limits
    # adds entries to be used for aggregations (e.g. synthGas can be exchanged => creates aggregated variables on gas level, so this exchange is accounted for in the gas level)
    excMerg_tab = addAggVar(excRename_tab, anyM.sets, anyM.mapping)

    assSupDis_dic = assignSupDis(unique(DB.select(excRename_tab,:Ts_dis)),anyM.sets[:Ts],anyM.supDis.lvl)
    excFullAddSup_tab = IT.transform(excMerg_tab,:Ts_supDis => DB.select(excRename_tab,:Ts_dis => x -> assSupDis_dic[x]))

    # XXX create actual variable object
    excVar_tab = reindex(createDispVar(anyM.optModel,excFullAddSup_tab,"exchange",anyM.sets[:Ts],anyM.supDis,anyM.options.bound.disp,anyM.options.digits.comp),(:Ts_dis, :R_from, :R_to, :C))
    anyM.variables[:exchange] = VarElement(:exchange,(:Ts_dis,:R_from,:R_to,:C),excVar_tab)
    produceMessage(anyM.options,anyM.report, 3," - Created exchange variables")
end

# XXX creates all trade variables
function createVariable!(name::Val{:trade}, anyM::anyModel)
    for type in (:Buy, :Sell)
        if Symbol(:trd,type,:Prc) in keys(anyM.parameter)
            #  XXX gets initial entries from checking where a price for buying or selling is defined
            trdVar_tab = DB.select(anyM.parameter[Symbol(:trd,type,:Prc)].data,DB.Not(All(:val)))
            getSupDis_dic = assignSupDis(unique(DB.select(trdVar_tab,:Ts_dis)),anyM.sets[:Ts],anyM.supDis.lvl) # adds supordinate dispatch timestep to trade entries

            # XXX adds entries to be used for aggregations (e.g. synthGas can be bought/sold => creates aggregated variables on gas level, so this is accounted for in the gas level)
            trdFull_tab =  addAggVar(trdVar_tab, anyM.sets, anyM.mapping)

            # adds supordinate dispatch timesteps again to table
            assSupDis_dic = assignSupDis(unique(DB.select(trdFull_tab,:Ts_dis)),anyM.sets[:Ts],anyM.supDis.lvl)
            dim_tup = tuple(filter(r -> r != :var,collect(colnames(trdFull_tab)))...)
            trdFullAddSup_tab = reindex(IT.transform(trdFull_tab,:Ts_supDis => DB.select(trdFull_tab,:Ts_dis => x -> assSupDis_dic[x])),dim_tup)

            # XXX create actual variable object
            anyM.variables[Symbol(:trade,type)] =
                VarElement(Symbol(:trade,type),(:Ts_dis, :R_dis, :C, :id),createDispVar(anyM.optModel,trdFullAddSup_tab,string("trd",type),anyM.sets[:Ts],anyM.supDis,anyM.options.bound.disp,anyM.options.digits.comp))
        end
    end
    produceMessage(anyM.options,anyM.report, 3," - Created trade variables")
end

# </editor-fold>

# <editor-fold desc="collection of subfunctions"
# XXX sets parameter data to values that could be matched with input table
function resetParameter(newData_tab::IndexedTable, parameter::ParElement, sets::Dict{Symbol,DataFrame}, techInfo::IndexedTable, options::modOptions, techCntM_dic::Union{Nothing,Dict{Int32,Int32}} = nothing, newInherit_tup::Tuple = ())
    # gets dimension of search tables and parameter without mode
    dimNoM_tup = tuple(filter(x -> x != :M ,collect(parameter.dim))...)
    modeParameter = nothing
    # creates empty report, that entries are written to within subprocess
    report = DataFrame(type = Int32[], group = Symbol[], instance = Symbol[],  message = String[])

    if !(:M in colnames(newData_tab)) || unique(DB.select(newData_tab,:M)) == Int32[0]
        # in case modes are not being searched for just directly set data
        matchData_tab = matchSetParameter(report, newData_tab, parameter, sets, options.digits.comp)
        parameter.data = reindex(matchData_tab,tuple(intersect(colnames(matchData_tab),dimNoM_tup)...))
    else
        # looks up original table without applying default values
        matchData1_tab = matchSetParameter(report,newData_tab,parameter,sets, options.digits.comp,:val,false)

        # filter returned table by weather a mode was specified
        noMode_tab = DB.filter(r -> r.M == 0,matchData1_tab)
        mode_tab = DB.filter(r -> r.M != 0,matchData1_tab)

        # groups mode related data for further analysis
        resDim_tup = tuple(filter(x -> x != :M ,intersect(parameter.dim,colnames(matchData1_tab)))...)

        if !isempty(mode_tab)

            modeGrp_tab = DB.groupby(mode_tab, resDim_tup, usekey = false; select = (:val, :M, :Te)) do y
                NamedTuple{(:M,:val,:cntM)}(length(unique(y.val)) == 1 ? tuple(Array(y.M),y.val[1],length(y.M)) : tuple(Array(y.M),Array(y.val),length(y.M)))
            end

            # removes entries where there is no parameter value for every mode and reports on it
            modeGrpDef_tab = filter(r -> techCntM_dic[r.Te] == r.cntM ,modeGrp_tab)
            if length(modeGrp_tab) > length(modeGrpDef_tab)
                missTechM_tab = DB.groupby(length,DB.join(modeGrp_tab,modeGrpDef_tab; lkey = resDim_tup, rkey = resDim_tup, how = :anti),:Te)
                for row in rows(missTechM_tab)
                    push!(report,(2, :modes, parameter.name, "parameter data was not specified for all modes in $(row.length) cases for $(createFullString(row.Te,sets[:Te],true)), existing values were ignored"))
                end
            end

            # filters entries where mode values are not distinct, reports on it and uses these entries as non-mode specific data
            noModeDis_tab = filter(r -> typeof(r.val) == Array ,DB.select(modeGrpDef_tab,DB.Not(All(:cntM))))
            if !isempty(noModeDis_tab)
                disTechM_tab = DB.groupby(length,DB.join(modeGrpDef_tab,noModeDis_tab; lkey = resDim_tup, rkey = resDim_tup, how = :anti),:Te)
                for row in rows(disTechM_tab)
                    push!(report,(2, :modes, parameter.name, "parameter data was the same for all modes in $(row.length) cases for $(createFullString(row.Te,sets[:Te],true)), no differentiation between modes was applied in these cases"))
                end
                noMode_tab = DB.merge(noMode_tab,noModeDis_tab)
            end

            # filters data where distinct mode data is provided for all modes and expends resulting table again
            finalModeGrp_tab = sort(DB.filter(r -> techCntM_dic[r.Te] == r.cntM, modeGrp_tab))
            leng_arr = DB.select(finalModeGrp_tab,:cntM)
            finalMode_tab = IT.transform(table(vcat(fill.(DB.select(finalModeGrp_tab,DB.Not(All(:cntM,:M,:val))),leng_arr)...)),:M => vcat(DB.select(finalModeGrp_tab,:M)...),:val => vcat(DB.select(finalModeGrp_tab,:val)...))
        else
            finalMode_tab = mode_tab
        end

        # gets all data, where no values where obtained successfully yet and look them up again applying the default value and not specifing the mode anymore
        # (hence now non mode-specific parameter values for technologies with modes are taken into account => mode-specific parameter values generally overwrite non-mode specific parameter values)
        newSearch_tab = table(DB.unique(DB.join(newData_tab,!isempty(finalMode_tab) ? DB.merge(DB.select(noMode_tab,DB.Not(All(:val))),DB.select(finalMode_tab,DB.Not(All(:val)))) : DB.select(noMode_tab,DB.Not(All(:val)));
                                                                                                                            lkey = resDim_tup, rkey = resDim_tup, lselect = resDim_tup, how = :anti)))
        if !isempty(newSearch_tab)
            matchData2_tab = matchSetParameter(report,IT.transform(newSearch_tab,:M => convert(Array{Int32,1},fill(0,length(newSearch_tab)))),parameter,sets, options.digits.comp)
            if !isempty(matchData2_tab) noMode_tab = DB.merge(matchData2_tab,noMode_tab) end
        end

        # returns tables with and without mode data to respective dictionaries, if there is no data for both deletes element
        if isempty(noMode_tab) && isempty(finalMode_tab)
            return nothing, modeParameter, report
        else
            parameter.data = reindex(merge(noMode_tab,finalMode_tab),resDim_tup)
        end
        # saves all dimensions with a mode to seperate dictionary for later use
        if !isempty(finalMode_tab)
            # assinges dispatch variables to a group of dispatch variables
            assDisGrp_dic = Dict(:gen => (:out,), :use => (:in,), :stIntIn => (:stIn,:stSize), :stExtIn => (:stIn,:stSize), :stIntOut => (:stOut,:stSize), :stExtOut => (:stOut,:stSize))
            # identifies what kind of capacity restrictions (in,out,stIn,...) apply for each occuring technology to expand table accordingly
            capaRestr_dic = Dict(x.Te => intersect(parameter.modeDep,union(map(y -> assDisGrp_dic[y],keys(x.allCar))...))
                                                                                        for x in DB.filter(r -> r.Te in unique(DB.select(finalMode_tab,:Te)),techInfo))

            modeParameter = DB.flatten(IT.transform(DB.select(finalMode_tab,DB.Not(All(:val))),:cnstrType => map(x -> capaRestr_dic[x], DB.select(finalMode_tab,:Te))),:cnstrType)
        end
    end
    # sets new inherit rules and default value
    parameter.dim = dimNoM_tup
    parameter.inherit = newInherit_tup

    return parameter, modeParameter, report
end

# XXX obtains residual capacities for technologies
function checkResiCapa(resiPar_sym::Symbol, stockCapa_tab::IndexedTable, allPar_tup::Tuple, anyM::anyModel)
    if resiPar_sym in allPar_tup
        # search for defined residual values
        stock_tab = filter(r -> r.val != 0, matchSetParameter(anyM.report, stockCapa_tab, anyM.parameter[resiPar_sym], anyM.sets, anyM.options.digits.comp))
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

# XXX determines relevant dimensions for a group of dispatch variables from capacity variables and computes dimension for each group
function getTechByCapa(capa::Symbol, dispGrp_tup::Tuple, capaData_tab::IndexedTable, capaMapping_tab::IndexedTable, anyM::anyModel)

	dim_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te)

	# creates empty report, that entries are written to within subfunction
    report = DataFrame(type = Int32[], group = Symbol[], instance = Symbol[],  message = String[])
	# empty dictionary for all variable table and aggregations written in subfunction
	varTab_dic = Dict{Symbol,IndexedTable}()
	aggDic_dic = Dict{Symbol,Dict{Int64,BitSet}}()

	# <editor-fold desc= XXX creates relevant dimension of dispatch variables from existing capacity variables>
	# joins capacity variables with dispatch levels to get dispatch variables needed for technologies
	key_tup = capa == :capaConv ?  (:Te,) : (:C, :Te)
	varLvlTech_tab = DB.join(capaData_tab, table(rows(capaMapping_tab)); lkey = key_tup, rkey = key_tup, lselect = DB.Not(All(:var)), how = :inner)

	# gets dispatch variables irrespective of technology created for the energy balance
	noTech_tab = filter(r -> r.Te == 0, capaMapping_tab)
	uniReg_arr = Dict(x => anyM.sets[:R][anyM.sets[:R][:,:lvl] .== x, :idx] for x in unique(DB.select(noTech_tab,:lvlR)))
	varLvlEnerGrp_tab = IT.transform(noTech_tab,:Ts_supDis => fill(anyM.supDis.step,length(noTech_tab)),
																					:Ts_inv => fill(convert(Int32,0),length(noTech_tab)),
																						:R_inv => map(x -> uniReg_arr[x],DB.select(noTech_tab,:lvlR)))

	# filters out dispatch variables for energy balances, that are superfluous, because no capacities can be related to these
	# (e.g. there is a carrier that can only be stored by a stock technology and capacities only exist in certain regions, creating a variable for regions without capacity would lead to an uncontrolled variable)
	varLvlEnerFlat_tab = flatten(flatten(varLvlEnerGrp_tab,:Ts_supDis),:R_inv)
	uniqueTech_tab = unique(DB.select(varLvlTech_tab,(:C,:Ts_supDis,:R_inv)))

	varLvlEner_tab = DB.filter(r -> !isempty(intersect(map(x -> (C = r.C, Ts_supDis = r.Ts_supDis, R_inv = x),vcat(r.R_inv, getChildren(r.R_inv,anyM.sets[:R],true))),uniqueTech_tab)), varLvlEnerFlat_tab)

	# merges both tables of dispatch variables
	varLvl_tab = DB.merge(varLvlTech_tab,varLvlEner_tab)

	# cheks for cases where storage balance is created on the subordinate dispatch level (like one constraint per each year)
	stOnSupDisp = filter(r -> r.lvlTs == anyM.supDis.lvl,varLvl_tab)
	if capa == :capaStSize && !isempty(stOnSupDisp)
		for te in unique(DB.select(stOnSupDisp,:Te))
			push!(report,(2,:constraint,:storageBalance,"for technology $(createFullString(te,anyM.sets[:Te])) storage balance will be created on the supordinate dispatch level, i.e. no actual storage can take place"))
		end
	end
	# </editor-fold>

	# <editor-fold desc= XXX creates table of actual dispatch variables for each group>
	# assignes combination of investment or supordinate dispatch index and level to the relevant dispatch timeteps
	unconvDic = Dict((x.Ts_supDis, x.lvlTs) => anyM.sets[:Ts][x.Ts_supDis,:lvl] == x.lvlTs ? [x.Ts_supDis] : getChildren(x.Ts_supDis,anyM.sets[:Ts],false,x.lvlTs) for x in DB.unique(DB.select(varLvl_tab,(:Ts_supDis, :lvlTs))))
	tsSupDisLvl_dic = convert(Dict{Tuple{Int32,Int32},Array{Int32,1}},unconvDic)
	unconvDic2 = Dict((x.R_inv, x.lvlR) => anyM.sets[:R][x.R_inv,:lvl] == x.lvlR ? x.R_inv : getHeritanceLine(x.R_inv,anyM.sets[:R],x.lvlR) for x in DB.unique(DB.select(varLvl_tab,(:R_inv, :lvlR))))
	rInvLvl_dic = convert(Dict{Tuple{Int32,Int32},Int32},unconvDic2)

	for type in dispGrp_tup
		varTab_dic[type] = getTechByGrp(capa, type, varLvl_tab, tsSupDisLvl_dic, rInvLvl_dic, anyM)
	end
	# </editor-fold>

	return varTab_dic, report
end

# XXX determines relevant dimensions for a specific type of dispatch variables
function getTechByGrp(capa::Symbol, type::Symbol, varLvl_tab::IndexedTable ,tsSupDisLvl_dic::Dict{Tuple{Int32,Int32},Array{Int32,1}}, rInvLvl_dic::Dict{Tuple{Int32,Int32},Int32}, anyM::anyModel)

	report = DataFrame(type = Int32[], group = Symbol[], instance = Symbol[],  message = String[])
	dim_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te)
	varLvlSub_tab = DB.select(filter(r -> r.varType == type, varLvl_tab),DB.Not(All(:varType)))
    typeGrp_dic = Dict(:gen => :out, :use => :in, :stIntIn => :stIn, :stExtIn => :stIn, :stIntOut => :stOut, :stExtOut => :stOut, :stSize => :stSize)

	# <editor-fold desc= XXX extends table and filters were availability is zero>
	# adds temporal and regional dispatch timesteps
	fullVar_tab = expandInvestToDisp(IT.transform(varLvlSub_tab,:M  => convert(Array{Int32,1},fill(0,length(varLvlSub_tab)))),tsSupDisLvl_dic,rInvLvl_dic,false)

	# filters rows where availability is zero and removes them from table to not generate these constraints
	rmv_tup = capa == :capaConv ? (:C,) : tuple()
	join_tup = tuple(setdiff(colnames(fullVar_tab),rmv_tup)...)
	paraAva_sym = Symbol(replace(string(capa),"capa" => "ava"))
	filtVar_tab = rmvDummyCol(DB.join(addDummyCol(fullVar_tab),filter(r -> r.val == 0.0,
		matchSetParameter(report,DB.select(fullVar_tab,DB.Not(All(rmv_tup))),anyM.parameter[paraAva_sym],anyM.sets, anyM.options.digits.comp)); lkey = join_tup, rkey = join_tup, how = :anti))
	# </editor-fold>

	# <editor-fold desc= XXX finds cases, where dispatch variables need to be mode specific and replaces them within table>
	if !isempty(anyM.mapping[:modeCases])
        join_tup = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te)

        # filters all modes cases relevant for the respective dispatch variables
        relModeVar_tab = DB.select(table(rows(DB.filter(r -> r.cnstrType == typeGrp_dic[type], anyM.mapping[:modeCases]))),DB.Not(All(:cnstrType)))
        # identifies relevant modes cases, adds them to all variables table while removing respective old entries without model
        modeCase_tab = DB.join(filtVar_tab, relModeVar_tab; lkey = join_tup, rkey = join_tup ,how = :inner)

        # in case of conversion variables (use and gen) dispatch variables exist on many temporal and spatial levels, therefore, if a variable on a upper level is mode-dependant, so most be those on lower levels
        if  capa == :capaConv
        	# filters entries, that might be mode controlled, because a variable on different temporal/spatial level is mode dependant
        	relEntr_tab = rows(DB.select(modeCase_tab,(:Ts_inv, :C, :Te))) |> (y -> filter(r -> (Ts_inv = r.Ts_inv, C= r.C, Te= r.Te) in y, filtVar_tab))

            # goes step-by-step over variables below mode controlled levels and ensures they are mode depedant as well
            while true
                # filters relevant entries that are not yet mode controlled
                maybeModeCase_tab = rmvDummyCol(DB.join(addDummyCol(relEntr_tab), modeCase_tab; lkey = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te), rkey = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te), how = :anti))
                if isempty(maybeModeCase_tab) break end

                # determines which mode dependant variables should actually be controlled as the sum of mode dependant variables on lower levels
                grpInter_tup = (((:Ts_inv, :Te, :Ts_dis) => (:Ts_inv, :Te)), (:C, :R_dis))
                dmy, addModes_tab = aggregateSetTable(maybeModeCase_tab, (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), grpInter_tup, anyM.sets, modeCase_tab)
                if isempty(addModes_tab) break end

                grpNewMode_tab = DB.groupby(flatten(DB.select(addModes_tab, (:M,:aggVar)),:aggVar),(:aggVar), usekey = false, select = :M) do x
        				NamedTuple{(:M,)}(tuple(Array(x)))
                end
        		newModeCase_tab = DB.select(flatten(join(IT.transform(maybeModeCase_tab,:aggVar => collect(1:length(maybeModeCase_tab))),grpNewMode_tab; lkey = :aggVar,rkey = :aggVar, how = :inner),:M),DB.Not(All(:aggVar)))
        		modeCase_tab = DB.merge(newModeCase_tab,modeCase_tab)
        	end
        end
        # adds mode relevant cases to all variables table while removing respective old entries without model
        filtVar_tab = DB.merge(modeCase_tab,DB.join(filtVar_tab,modeCase_tab; lkey = join_tup, rkey = join_tup, how = :anti))
	end
	# </editor-fold>

	# <editor-fold desc= XXX adds entries for limit constraints and peforms aggregation>
	# filters all limit parameters of relevance for the respective type of dispatch variable

    # adds supordinate dispatch timesteps again to table
    assSupDis_dic = assignSupDis(unique(DB.select(filtVar_tab,:Ts_dis)),anyM.sets[:Ts],anyM.supDis.lvl)
    fitlVarAddSup_tab = DB.reindex(IT.transform(filtVar_tab,:Ts_supDis => DB.select(filtVar_tab,:Ts_dis => x -> assSupDis_dic[x])),(:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M))

	# </editor-fold>
	return fitlVarAddSup_tab
end

# XXX creates a investment variable for all rows in setData_tab, rows that also appear in resiData_tab are merge to the resulting table, but instead of variable these rows a fixed number
function createInvVar(optModel::Model,setData_tab::IndexedTable,resiData_tab::Union{IndexedTable,Nothing},name_str::String,upBd_flt::Union{Nothing,Float64})

    # adds an upper bound to all variables if provided within the options
    if isnothing(upBd_flt)
        info = VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)
    else
        info = VariableInfo(true, 0.0, true, upBd_flt, false, NaN, false, NaN, false, false)
    end

    buildVar_svar = JuMP.build_variable(error, info)

    # creates variables and might merge residual values
    if resiData_tab != nothing
        var_tab = IT.transform(setData_tab, :var => [JuMP.add_variable(optModel::Model, JuMP.build_variable(error, info),name_str) for i = 1:length(setData_tab)])
        return DB.merge(DB.rename(resiData_tab, :val => :var),var_tab)
    else
        return IT.transform(setData_tab, :var => [JuMP.add_variable(optModel::Model, buildVar_svar, name_str::String) for i = 1:length(setData_tab)])
    end
end

# XXX creates all dispatch variables, if a upper bound for dispatch variables is provided these are scaled
function createDispVar(optModel::Model,setData_tab::IndexedTable,name_str::String,timeTree_df::DataFrame,supDis::NamedTuple{(:lvl,:step,:dic),Tuple{Int32,Tuple{Vararg{Int32,N} where N},Dict{Tuple{Int32,Int32},Float64}}},upBd_flt::Union{Nothing,Float64},rdDig_int::Int64)

    if isnothing(upBd_flt) # creates all variables without any upper bound
        buildVar_svar = JuMP.build_variable(error, VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false))
        return IT.transform(setData_tab, :var => [JuMP.add_variable(optModel, buildVar_svar, name_str::String) for i = 1:length(setData_tab)])
    else # scales the provided upper bound and creates variables
        upBb_arr = round.(DB.select(addScaling(IT.transform(setData_tab,:upBd => fill(upBd_flt,length(setData_tab))),:upBd,timeTree_df,supDis),:upBd);digits = rdDig_int)
        return IT.transform(setData_tab, :var => [JuMP.add_variable(optModel, JuMP.build_variable(error, VariableInfo(true, 0.0, true, upBb_arr[i], false, NaN, false, NaN, false, false)), name_str) for i = 1:length(setData_tab)])
    end
end

# XXX adds additional dispatch for trade and exchange variables
function addAggVar(var_tab::IndexedTable,sets::Dict{Symbol,DataFrame},mapping::Dict{Symbol,IndexedTable})

    uniC_arr = unique(DB.select(var_tab,:C))

    # assigns to each carriers an array of carriers, that require seperate aggregation variables
    parC_dic = Dict{Int32,Array{Int32,1}}()
    for x in uniC_arr
        if isempty(values(parC_dic))
            parC_dic[x] = filter(y -> !(y in uniC_arr),getindex.(getHeritanceLine(x,sets[:C]),1))
        else # ensures a carrier is only assigned once
            parC_dic[x] = filter(y -> !(y in union(uniC_arr,values(parC_dic)...)),getindex.(getHeritanceLine(x,sets[:C]),1))
        end
    end

    if !isempty(union(values(parC_dic)...))
        # extends orginal table by carriers to aggregate and their respective spatial and temporal level
        lvlTs_dic, lvlR_dic = [Dict(x => getproperty.(filter(r -> r.C == x,mapping[:C_lvl]),type)[1] for x in union(values(parC_dic)...)) for type in (:lvlTsDis, :lvlRDis)]
        varAgg_tab = flatten(IT.transform(var_tab,:C => map(x -> parC_dic[x],DB.select(var_tab,:C))),:C)
        varAggLvl_tab = IT.transform(varAgg_tab,:lvlTs => map(x -> lvlTs_dic[x], DB.select(varAgg_tab,:C)),:lvlR => map(x -> lvlR_dic[x], DB.select(varAgg_tab,:C)))

        # rewrites columns to values for carrier being aggregated
        for col in intersect(colnames(varAggLvl_tab),(:Ts_dis,:R_from,:R_to))
            setName_sym = Symbol(split(String(col),"_")[1])
            set_df = sets[setName_sym]
            lvl = Symbol(:lvl,setName_sym)
            varAggLvl_tab = IT.transform(varAggLvl_tab,
                                col => DB.select(varAggLvl_tab,(col,lvl) => x -> set_df[getproperty(x,col),:lvl] == getproperty(x,lvl) ? getproperty(x,col) : getindex.(getHeritanceLine(getproperty(x,col),set_df,getproperty(x,lvl)),1)))
        end

        return merge(var_tab,DB.select(varAggLvl_tab,DB.Not(All(intersect(colnames(varAggLvl_tab),(:lvlTs,:lvlR))...))))
    else
        return var_tab
    end
end
# </editor-fold>
