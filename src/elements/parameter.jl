mutable struct ParElement
    name::Symbol
    dim::Tuple
    default_val::Union{Nothing,Float64}
    inherit::Tuple
    data::Union{Nothing,IndexedTable}
    grp::Symbol
    presetLvl::Symbol
    modeDep::Tuple

    function ParElement(paraData_df::Union{Nothing,DataFrame},paraDef_ntup::NamedTuple)

        SetLongShort_dic = Dict(:Ts => :timestep, :R => :region, :C => :carrier, :Te => :technology, :M => :mode)

        # immediately creates object without data if provoided input paraData is nothing
        if paraData_df == nothing return new(paraDef_ntup.name,paraDef_ntup.dim,paraDef_ntup.default_val,paraDef_ntup.inherit,nothing,paraDef_ntup.grp) end

        # XXX check consistency of rows in input dataframe and definition of set and rename columns according to set defintion
        # assigns array of used suffixes according to parameter defintion to each set
        SplitDim_arr = map(x -> map(y -> Symbol(y), split(String(x),"_")),collect(paraDef_ntup.dim))
        SetSuf_dic = Dict(x => map(y -> length(y) == 1 ? nothing : y[end],filter(z -> z[1] == x,SplitDim_arr)) for x in unique(map(x -> x[1],SplitDim_arr)))

        # loops over set columns in input dataframe and assigns them to the sets defined for the parameter
        NewCol_dic = Dict(:val => :val)
        SufNum_dic = Dict(:b => 2, :c => 3, :d => 4, :e => 5, :f => 6, :g => 7)
        for colNam in setdiff(names(paraData_df),[:val])
            colNam_arr = split(String(colNam),"_")
            setNam = Symbol(colNam_arr[1])

            if !(setNam in keys(SetSuf_dic)) # parameter provided for a set not appearing in definition (e.g. demand depending on the technology)
                push!(Report_df,(2, :par, paraDef_ntup.name, "parameter data was specified for $(SetLongShort_dic[setNam]) set, but it is not defined to depend on this set"))
                continue
            elseif length(SetSuf_dic[setNam]) == 1 && length(colNam_arr) > 1 # they are several instances of the set provided, but it only depends on one instance (e.g. two region sets for efficiency)
                push!(Report_df,(2, :par, paraDef_ntup.name, "parameter data was specified for several instances of $(SetLongShort_dic[setNam]) set, but it is defined to depend only on one instance, additonal instances were ignored"))
                continue
            elseif SetSuf_dic[setNam][1] == nothing # set has only one instance and no suffix => no change when converting from read-in dataframe to parameter element
                NewCol_dic[colNam] = colNam
            elseif length(SetSuf_dic[setNam]) == 1 || length(colNam_arr) == 1 # column name in dataframe has no underscore, but defintion of parameter element has one
                NewCol_dic[colNam] = Symbol(setNam, "_", SetSuf_dic[setNam][1])
            else
                cntRep_int = SufNum_dic[Symbol(colNam_arr[2])] # set defined for several instances
                NewCol_dic[colNam] = Symbol(setNam, "_", SetSuf_dic[setNam][cntRep_int])
            end
        end

        # filters only used coulumns, renames them accordingly and converts to table
        paraData_df = paraData_df[:,collect(keys(NewCol_dic))]
        DataFrames.rename!(paraData_df,NewCol_dic)
        writeData_tab = JuliaDB.table(paraData_df)

        new_obj = new(paraDef_ntup.name,paraDef_ntup.dim,paraDef_ntup.default_val,paraDef_ntup.inherit,writeData_tab,paraDef_ntup.grp)
        # defines on which level parameter is presetted and which capacity restrictions are affected by different modes for all dispatch parameters, where this is specified
        if :presetLvl in keys(paraDef_ntup) new_obj.presetLvl = paraDef_ntup.presetLvl end
        if :modeDep   in keys(paraDef_ntup) new_obj.modeDep   = paraDef_ntup.modeDep   end
        return new_obj
    end
end

# XXX defines all existing parameters
function defineParameter()
    ParDef_dic = Dict{Symbol, NamedTuple}()

    # <editor-fold desc="XXX investment parameters"

    # XXX general investment

    ParDef_dic[:rateDisc]  =   (name = :rateDisc,  dim = (:Ts_supDis, :R_inv), default_val = 0.02, inherit = (:Ts_supDis => :up, :R_inv => :up, :R_inv => :avg_any, :Ts_supDis => :avg_any), grp = :invest)

    # XXX technology and exchange investment

    ParDef_dic[:stInToConv]  =   (name = :stInToConv,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:stOutToStIn] =   (name = :stOutToStIn, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:sizeToStIn]  =   (name = :sizeToStIn,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)

    ParDef_dic[:lifeConv]    =   (name = :lifeConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeStIn]    =   (name = :lifeStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeStOut]   =   (name = :lifeStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeStSize]  =   (name = :lifeStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeExc]     =   (name = :lifeExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = 50, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), grp = :invest)

    ParDef_dic[:lifeEcoConv]   =   (name = :lifeEcoConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeEcoStIn]   =   (name = :lifeEcoStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeEcoStOut]  =   (name = :lifeEcoStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeEcoStSize] =   (name = :lifeEcoStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:lifeEcoExc]    =   (name = :lifeEcoExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any), grp = :invest)

    ParDef_dic[:costInvConv]   =   (name = :costInvConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costInvStIn]   =   (name = :costInvStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costInvStOut]  =   (name = :costInvStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costInvStSize] =   (name = :costInvStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costInvExc]    =   (name = :costInvExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), grp = :invest)

    ParDef_dic[:rateInvConv]   =   (name = :rateInvConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:rateInvStIn]   =   (name = :rateInvStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:rateInvStOut]  =   (name = :rateInvStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:rateInvStSize] =   (name = :rateInvStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:rateInvExc]    =   (name = :rateInvExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = 0.03, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), grp = :invest)

    ParDef_dic[:costOprConv]   =   (name = :costOprConv,   dim = (:Ts_inv, :Ts_supDis, :R_inv, :Te),     default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costOprStIn]   =   (name = :costOprStIn,   dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costOprStOut]  =   (name = :costOprStOut,  dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costOprStSize] =   (name = :costOprStSize, dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up), grp = :invest)
    ParDef_dic[:costOprExc]    =   (name = :costOprExc,    dim = (:Ts_inv, :Ts_supDis, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), grp = :invest)


    # XXX parameters regarding limits on technology and exchange investment and capacity

    ParDef_dic[:invConvUp]      =   (name = :invConvUp,      dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_inv => :sum_any, :R_inv => :sum_full,  :Te => :sum_any), grp = :invest)
    ParDef_dic[:invConvLow]     =   (name = :invConvLow,     dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_inv => :sum_any, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)
    ParDef_dic[:invConvFix]     =   (name = :invConvFix,     dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_inv => :sum_any, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)

    ParDef_dic[:invStInUp]      =   (name = :invStInUp,      dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_full, :Te => :sum_any),  grp = :invest)
    ParDef_dic[:invStInLow]     =   (name = :invStInLow,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)
    ParDef_dic[:invStInFix]     =   (name = :invStInFix,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)

    ParDef_dic[:invStOutUp]     =   (name = :invStOutUp,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_full,  :Te => :sum_any), grp = :invest)
    ParDef_dic[:invStOutLow]    =   (name = :invStOutLow,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)
    ParDef_dic[:invStOutFix]    =   (name = :invStOutFix,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)

    ParDef_dic[:invStSizeUp]    =   (name = :invStSizeUp,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_full,  :Te => :sum_any), grp = :invest)
    ParDef_dic[:invStSizeLow]   =   (name = :invStSizeLow,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)
    ParDef_dic[:invStSizeFix]   =   (name = :invStSizeFix,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :Ts_inv => :up, :R_inv => :sum_any,  :Te => :sum_any),  grp = :invest)

    ParDef_dic[:invExcUp]       =   (name = :invExcUp,      dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any, :R_b => :sum_any), grp = :invest)
    ParDef_dic[:invExcLow]      =   (name = :invExcLow,     dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any, :R_b => :sum_any), grp = :invest)
    ParDef_dic[:invExcFix]      =   (name = :invExcFix,     dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any, :R_b => :sum_any), grp = :invest)

    ParDef_dic[:capaConvUp]     =   (name = :capaConvUp,     dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaConvLow]    =   (name = :capaConvLow,    dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaConvFix]    =   (name = :capaConvFix,    dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaConvResi]   =   (name = :capaConvResi,   dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

    ParDef_dic[:capaStInUp]     =   (name = :capaStInUp,     dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStInLow]    =   (name = :capaStInLow,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStInFix]    =   (name = :capaStInFix,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStInResi]   =   (name = :capaStInResi,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

    ParDef_dic[:capaStOutUp]    =   (name = :capaStOutUp,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStOutLow]   =   (name = :capaStOutLow,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStOutFix]   =   (name = :capaStOutFix,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStOutResi]  =   (name = :capaStOutResi,  dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

    ParDef_dic[:capaStSizeUp]   =   (name = :capaStSizeUp,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStSizeLow]  =   (name = :capaStSizeLow,  dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStSizeFix]  =   (name = :capaStSizeFix,  dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
    ParDef_dic[:capaStSizeResi] =   (name = :capaStSizeResi, dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

    ParDef_dic[:capaExcUp]   =   (name = :capaExcUp,   dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
    ParDef_dic[:capaExcLow]  =   (name = :capaExcLow,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
    ParDef_dic[:capaExcFix]  =   (name = :capaExcFix,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
    ParDef_dic[:capaExcResi] =   (name = :capaExcResi, dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)

    if DecommMethod != :none
        ParDef_dic[:capaCommConvCUp]    =   (name = :capaCommConvUp,     dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommConvLow]    =   (name = :capaCommConvLow,    dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommConvFix]    =   (name = :capaCommConvFix,    dim = (:Ts_supDis, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

        ParDef_dic[:capaCommStInUp]     =   (name = :capaCommStInUp,     dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStInLow]    =   (name = :capaCommStInLow,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStInFix]    =   (name = :capaCommStInFix,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

        ParDef_dic[:capaCommStOutUp]    =   (name = :capaCommStOutUp,    dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStOutLow]   =   (name = :capaCommStOutLow,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStOutFix]   =   (name = :capaCommStOutFix,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

        ParDef_dic[:capaCommStSizeUp]   =   (name = :capaCommStSizeUp,   dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStSizeLow]  =   (name = :capaCommStSizeLow,  dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)
        ParDef_dic[:capaCommStSizeFix]  =   (name = :capaCommStSizeFix,  dim = (:Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:C => :up, :R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :Ts_supDis => :up), grp = :invest)

        ParDef_dic[:capaExcUp]   =   (name = :capaExcUp,   dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
        ParDef_dic[:capaExcLow]  =   (name = :capaExcLow,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
        ParDef_dic[:capaExcFix]  =   (name = :capaExcFix,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
        ParDef_dic[:capaExcResi] =   (name = :capaExcResi, dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :up, :R_a => :sum_any,  :R_b => :sum_any), grp = :invest)
    end

    # </editor-fold>

    # <editor-fold desc="XXX dispatch parameters"

    # XXX technology dispatch properties

    # availability parameters
    ParDef_dic[:avaConv]      =   (name = :avaConv,   dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up,            :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in, :out))
    ParDef_dic[:avaStIn]      =   (name = :avaStIn,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,))
    ParDef_dic[:avaStOut]     =   (name = :avaStOut,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stOut,))
    ParDef_dic[:avaStSize]    =   (name = :avaStSize, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    # must-run and fix-run (if availability provides an upper level for the share of capacity used at a certain timepoint, these provide a lower and fixed share)
    ParDef_dic[:mrunConv]      =   (name = :mrunConv,   dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up,            :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in, :out))
    ParDef_dic[:mrunStIn]      =   (name = :mrunStIn,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,))
    ParDef_dic[:mrunStOut]     =   (name = :mrunStOut,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stOut,))
    ParDef_dic[:mrunStSize]    =   (name = :mrunStSize, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    ParDef_dic[:frunConv]      =   (name = :frunConv,   dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up,            :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in, :out))
    ParDef_dic[:frunStIn]      =   (name = :frunStIn,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,))
    ParDef_dic[:frunStOut]     =   (name = :frunStOut,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stOut,))
    ParDef_dic[:frunStSize]    =   (name = :frunStSize, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    # efficiency parameters
    ParDef_dic[:effConv]      =   (name = :effConv,  dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :Te => :up),            grp = :dispatch, presetLvl = :reference, modeDep = (:in, :out))
    ParDef_dic[:effStIn]      =   (name = :effStIn,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up), grp = :dispatch, presetLvl = :carrier,   modeDep = (:stIn,))
    ParDef_dic[:effStOut]     =   (name = :effStOut, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up), grp = :dispatch, presetLvl = :carrier,   modeDep = (:stOut,))

    # specific storage parameters
    ParDef_dic[:stDis]     =   (name = :stDis,    dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up),           grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))
    ParDef_dic[:stInflow]  =   (name = :stInflow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :C => :up, :Ts_dis => :sum_any, :R_dis => :sum_any, :Te => :up), grp = :dispatch, presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    # variable costs
    ParDef_dic[:costVarUse]   =   (name = :costVarUse,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:in,))
    ParDef_dic[:costVarGen]   =   (name = :costVarGen,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:out,))
    ParDef_dic[:costVarStIn]  =   (name = :costVarStIn,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:stIn,))
    ParDef_dic[:costVarStOut] =   (name = :costVarStOut, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:stOut,))

    # capacity related ratios (x% of conversion capacity is used for carrier y)
    ParDef_dic[:ratioCapaUseUp]    =   (name = :ratioCapaUseUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in,))
    ParDef_dic[:ratioCapaUseLow]   =   (name = :ratioCapaUseLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in,))
    ParDef_dic[:ratioCapaUseFix]   =   (name = :ratioCapaUseFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:in,))

    ParDef_dic[:ratioCapaGenUp]    =   (name = :ratioCapaGenUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:out,))
    ParDef_dic[:ratioCapaGenLow]   =   (name = :ratioCapaGenLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:out,))
    ParDef_dic[:ratioCapaGenFix]   =   (name = :ratioCapaGenFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :lowest,  modeDep = (:out,))

    # energy related ratios (x% of energy from/to technology has to be carrier y)
    ParDef_dic[:ratioEnerUseUp]    =   (name = :ratioCapaUseUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:in,))
    ParDef_dic[:ratioEnerUseLow]   =   (name = :ratioCapaUseLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:in,))
    ParDef_dic[:ratioEnerUseFix]   =   (name = :ratioCapaUseFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:in,))

    ParDef_dic[:ratioEnerGenUp]    =   (name = :ratioCapaGenUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:out,))
    ParDef_dic[:ratioEnerGenLow]   =   (name = :ratioCapaGenLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:out,))
    ParDef_dic[:ratioEnerGenFix]   =   (name = :ratioCapaGenFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up), grp = :dispatch, presetLvl = :carrier,  modeDep = (:out,))


    # XXX further dispatch properties
    ParDef_dic[:demand]    =   (name = :demand, dim = (:Ts_dis, :R_dis, :C), default_val = 0.0, inherit = (:Ts_dis => :sum_any, :R_dis  => :sum_any, :C => :sum_any), grp = :dispatch)

    # trade (=sell or buy to an external market) parameters
    ParDef_dic[:trdBuyPrc]   =   (name = :trdBuyPrc,  dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any), grp = :dispatch, presetLvl = :trade)
    ParDef_dic[:trdSellPrc]  =   (name = :trdSellPrc, dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any), grp = :dispatch, presetLvl = :trade)
    ParDef_dic[:trdBuyCap]   =   (name = :trdBuyPrc,  dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any), grp = :dispatch, presetLvl = :trade)
    ParDef_dic[:trdSellCap]  =   (name = :trdSellPrc, dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any), grp = :dispatch, presetLvl = :trade)

    # exchange (=exchange between explicit regions) parameters
    ParDef_dic[:avaExc]     =   (name = :avaExc,  dim = (:Ts_dis, :R_a, :R_b, :C), default_val = 1.0,     inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any), grp = :dispatch, presetLvl = :exchange)
    ParDef_dic[:lossExc]     =  (name = :lossExc, dim = (:Ts_dis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any), grp = :dispatch, presetLvl = :exchange)
    ParDef_dic[:costExc]    =   (name = :costExc, dim = (:Ts_dis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any), grp = :dispatch, presetLvl = :exchange)
    ParDef_dic[:costVarExc]   =   (name = :costVarExc,   dim = (:Ts_dis, :R_a, :R_b, :C),               default_val = nothing, inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any),              grp = :dispatch, presetLvl = :carrier)

    # </editor-fold>

    # check if sets are illdefined with respect to inheritance
    InheritRules_tup = (:sum_full, :sum_any, :avg_full, :avg_any, :uni_full, :uni_any, :up)
    WrongSetHerit_arr = filter(x -> !all(map(y -> y[1] in ParDef_dic[x].dim,ParDef_dic[x].inherit)),collect(keys(ParDef_dic)))
    WrongRulesHerit_arr = filter(x -> !all(map(y -> y[2] in InheritRules_tup,ParDef_dic[x].inherit)),collect(keys(ParDef_dic)))

    if !isempty(WrongSetHerit_arr)
        push!(Report_df,(3, :par, :definition, "inheritance rule for a set not defined as a possible dimension in $(join(WrongSetHerit_arr,","))"))
    end

    if !isempty(WrongRulesHerit_arr)
        push!(Report_df,(3, :par, :definition, "invalid inheritance rule for $(join(WrongRulesHerit_arr,","))"))
    end

    return ParDef_dic
end

# XXX matches set with input parameters, uses inheritance rules for unmatched cases
function matchSetParameter(srcSetIn_tab::IndexedTable,parameter_obj::ParElement,Set_dic::Dict{Symbol,DataFrame},newCol_sym::Symbol = :val,useDefault_boo::Bool = true)

    # directly returns default values if no data was provided for the parameter
    if parameter_obj.data == nothing
        return IT.transform(srcSetIn_tab,newCol_sym => fill(parameter_obj.default_val,length(srcSetIn_tab)))
    end

    searchCol_tup = colnames(srcSetIn_tab)
    paraData_tab = parameter_obj.data

    # removes sets the parameter is not specified for from search table and condenses search table accordingly
    redunSrc_tup = setdiff(searchCol_tup,colnames(paraData_tab))
    searchSet_tab = isempty(redunSrc_tup) ? srcSetIn_tab : table(DB.unique(DB.select(srcSetIn_tab,DB.Not(All(redunSrc_tup...)))))

    srcCol_tup = colnames(searchSet_tab)
    searchSet_tab = addDummyCol(searchSet_tab)
    # workaround for "join" bug, does not work if all columns of the first table are used
    cntSearch_int = length(searchSet_tab)

    # searches for matches in original data
    paraMatch_tab = rmvDummyCol(DB.join(searchSet_tab, paraData_tab; lkey = srcCol_tup, rkey = srcCol_tup, how=:inner))

    # boolean that switches to true if all values were matched via inheritance
    allMatch_boo = false

    # checks if there are actually unmatched values before startin inheritance process
    noMatch_tab = DB.join(searchSet_tab, paraData_tab; lkey = srcCol_tup, rkey = srcCol_tup, how=:anti)
    if !isempty(noMatch_tab)

        for herit in parameter_obj.inherit
            if herit[1] in srcCol_tup

                # inherit new values and check for additional matches
                unmatch_arr::Array{Int16,1} = unique(DB.select(noMatch_tab,herit[1]))
                newData_tab = inheritParameter(herit,unmatch_arr,paraData_tab,Set_dic)
                if JuliaDB.isempty(newData_tab) continue end

                newMatch_tab = rmvDummyCol(DB.join(noMatch_tab, newData_tab; lkey = srcCol_tup, rkey = srcCol_tup, how=:inner))

                # report on inheritace
                cntNewData_int = length(newData_tab)
                cntMatch_int = length(newMatch_tab)

                push!(Report_df,(1, :inheri, parameter_obj.name, "inheritance via $(herit) created $(cntNewData_int) new data row(s) and assigned $(cntMatch_int) new value(s)"))

                # add new rows to both table with matches and parameter data
                paraData_tab = DB.merge(paraData_tab,newData_tab)
                paraMatch_tab = DB.merge(paraMatch_tab,newMatch_tab)

                # removes newly matched values and leaves loop if everything is matched now
                if cntMatch_int == length(noMatch_tab)
                    allMatch_boo = true
                    break
                else
                    noMatch_tab = DB.join(noMatch_tab, newMatch_tab; lkey = srcCol_tup, rkey = srcCol_tup, how=:anti)
                end
            end
        end

        # writes default values for remaining unmatched values
        cntNoMatch_int = length(noMatch_tab)
        if !allMatch_boo && parameter_obj.default_val != nothing && useDefault_boo
            defaultMatch_tab = IT.transform(noMatch_tab,:val => fill(parameter_obj.default_val,cntNoMatch_int))
            paraMatch_tab = isempty(paraMatch_tab) ? rmvDummyCol(defaultMatch_tab) : DB.merge(paraMatch_tab,rmvDummyCol(defaultMatch_tab))
            printVal = parameter_obj.default_val == nothing ? "nothing" : parameter_obj.default_val
            push!(Report_df,(1, :inheri, parameter_obj.name, "assigned default value $(printVal) for $(length(defaultMatch_tab)) entrie(s)"))
        end
    end

    # expands table again by rows
    if !isempty(redunSrc_tup)
        paraMatch_tab = DB.join(paraMatch_tab, table(rows(srcSetIn_tab)); lkey = srcCol_tup, rkey = srcCol_tup, how =:inner)
    end

    return DB.rename(paraMatch_tab,:val => newCol_sym)
end

inheritParameter(herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int16,1},paraData_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame}) = inheritParameter(Val{herit_par[2]}(),herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int16,1},paraData_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame})

# <editor-fold desc="collection of subfunctions"

# XXX covers all inheritance from nodes below unmatched nodes
function inheritParameter(heritType::Union{Val{:sum_full},Val{:sum_any},Val{:avg_full},Val{:avg_any},Val{:uni_full},Val{:uni_any}},herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int16,1},paraDataIn_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame})

    # XXX reads out specific inheritance options
    heritSet_sym = herit_par[1]
    heritSetShort_sym = Symbol(split(String(heritSet_sym),"_")[1])

    splHerit_arr = split(String(herit_par[2]),"_")
    heritAgg_sym = Symbol(splHerit_arr[1])
    heritFull_boo = Symbol(splHerit_arr[2]) == :full

    # XXX initialize values for loop (removes and add val again to control its position)
    colNam_tup = tuple(vcat(filter(x -> x != :val,collect(colnames(paraDataIn_tab)))...,:val)...)

    # dimensions not involved in inheritance propcess
    noHeritSet_tup = tuple(setdiff(collect(colNam_tup),[:val,herit_par[1]])...)

    newData_tab = table(NamedTuple{colNam_tup}((fill(Int16[],length(colNam_tup)-1)...,Float64[])), pkey = vcat(heritSet_sym,noHeritSet_tup...))

    # gets all children of unmatched ids to filter relevant part of tree
    childrenUnmatch_arr = unique(vcat(map(x -> getChildren(x,Set_dic[heritSetShort_sym],true),unmatch_arr)...))

    paraDataFilt_tab = DB.filter(r -> getproperty(r,heritSet_sym) in childrenUnmatch_arr,paraDataIn_tab)
    if isempty(paraDataFilt_tab) return newData_tab end

    paraDataSrc_tab = IT.transform(paraDataFilt_tab, :pare => DB.select(paraDataFilt_tab,heritSet_sym => x -> Set_dic[heritSetShort_sym][x,:pare]))

    # loops going upwards within tree trying to obtain new values
    newVal_boo = true

    while newVal_boo
        # saves all children of parents currently used in the grouped table within a dictionary to use below within loop over rows
        if heritFull_boo
            childPar_dic = Dict(x => getChildren(x,Set_dic[heritSetShort_sym], false, x != 0 ? Set_dic[heritSetShort_sym][x,:lvl]+1 : 1) for x in unique(DB.select(paraDataSrc_tab,:pare)))
        end

        # groups table by parents and not used dimensions according to aggregation rule
        if heritAgg_sym == :sum
            paraDataGrp_tab = JuliaDB.groupby(paraDataSrc_tab, Tuple(vcat(noHeritSet_tup...,:pare)), usekey = false, select = (heritSet_sym,:val)) do y
               NamedTuple{(heritSet_sym,:valAgg)}(tuple(Array(getproperty(y,heritSet_sym)), sum(y.val)))
            end
        elseif heritAgg_sym == :avg
            paraDataGrp_tab = JuliaDB.groupby(paraDataSrc_tab, Tuple(vcat(noHeritSet_tup...,:pare)), usekey = false, select = (heritSet_sym,:val)) do y
               NamedTuple{(heritSet_sym,:valAgg)}(tuple(Array(getproperty(y,heritSet_sym)), Statistics.mean(y.val)))
            end
        else
            paraDataGrp_tab = DB.filter(r -> r.valAgg != nothing, JuliaDB.groupby(paraDataSrc_tab, Tuple(vcat(noHeritSet_tup...,:pare)), usekey = false, select = (heritSet_sym,:val)) do y
               NamedTuple{(heritSet_sym,:valAgg)}(tuple(Array(getproperty(y,heritSet_sym)), length(unique(y.val)) == 1 ? y.val[1] : nothing))
           end )
        end

        existKey_tab = DB.select(newData_tab,Keys())
        # loops through rows of grouped table to see if any row can be coverted into new data
        newVal_boo = false
        for row in rows(paraDataGrp_tab)
            # if option "full" is used, gets all direct children of parent and compare children aggregated in respective row
            if heritFull_boo checkExis_arr = map(x -> x in getproperty(row,heritSet_sym),childPar_dic[row.pare]) end
            # if option "full" is used or all children were found in row
            if !heritFull_boo || all(checkExis_arr)
                checkNew_tup = NamedTuple{Tuple(vcat(heritSet_sym,noHeritSet_tup...))}((row.pare,map(x -> getproperty(row,x),noHeritSet_tup)...))
                # writes values if non-existing in table so far
                if !(checkNew_tup in existKey_tab)
                    newEntry_tup = NamedTuple{Tuple(vcat(heritSet_sym,:val,noHeritSet_tup...))}((row.pare,row.valAgg,map(x -> getproperty(row,x),noHeritSet_tup)...))
                    newVal_boo = true
                    push!(rows(newData_tab),newEntry_tup)
                end
            end
        end
        # add parent column to new data and filters values to consider for further inheritance (= only children of unmatched values)
        paraDataSrc_tab = DB.filter(r -> getproperty(r,heritSet_sym) in childrenUnmatch_arr,newData_tab)
        paraDataSrc_tab = IT.transform(paraDataSrc_tab, :pare => DB.select(paraDataSrc_tab,heritSet_sym => x -> Set_dic[heritSetShort_sym][x,:pare]))
    end

    return newData_tab
end

# XXX covers direct inheritance from upper nodes
function inheritParameter(heritType::Val{:up},herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int16,1},paraDataIn_tab::IndexedTable,Set_dic::Dict{Symbol,DataFrame})

    heritSetShort_sym = Symbol(split(String(herit_par[1]),"_")[1])

    unmatchChild_dic = Dict(x => intersect(unmatch_arr,getChildren(x,Set_dic[heritSetShort_sym],true)) for x in unique(DB.select(paraDataIn_tab, herit_par[1])))

    paraData_tab = DB.filter(r -> !isempty(unmatchChild_dic[getproperty(r,herit_par[1])]),paraDataIn_tab)
    paraDataChild_tab = DB.flatten(IT.transform(paraData_tab,:child => DB.select(paraData_tab,herit_par[1] => x -> unmatchChild_dic[x])))

    # determines all columns for groupby statement
    filtNam_tup = filter(x -> !(x in [:val,herit_par[1]]),collect(colnames(paraData_tab)))
    grpBy_tup = tuple(vcat(:child,filtNam_tup...)...)
    grpBy2_tup = tuple(vcat(:maximum,filtNam_tup...)...)
    grpBy3_tup = tuple(vcat(herit_par[1],filtNam_tup...)...)

    paraNewData_tab = JuliaDB.groupby(maximum,paraDataChild_tab, grpBy_tup, usekey = false, select = herit_par[1])
    if !isempty(paraNewData_tab)
        # add dummy column to allow for later join commands
        paraData_tab = addDummyCol(paraData_tab)
        paraNewData_tab = join(paraNewData_tab,paraData_tab; lkey = grpBy2_tup, rkey = grpBy3_tup, how = :inner, lselect =grpBy_tup, rselect = (:val))
        paraNewData_tab = DB.select(JuliaDB.rename(paraNewData_tab, :child => herit_par[1]),DB.Not(:maximum))
    end

    return paraNewData_tab
end

# </editor-fold>
