
# XXX defines all existing parameters
function defineParameter(options::modOptions,report::DataFrame)
    ParDef_dic = Dict{Symbol, NamedTuple}()

    # <editor-fold desc="XXX investment parameters"

    # XXX general investment

    ParDef_dic[:rateDisc]  =   (name = :rateDisc,  dim = (:Ts_supDis, :R_inv), default_val = 0.02, inherit = (:Ts_supDis => :up, :R_inv => :up, :R_inv => :avg_any, :Ts_supDis => :avg_any))

    # XXX technology and exchange investment

    ParDef_dic[:stInToConv]  =   (name = :stInToConv,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:stOutToStIn] =   (name = :stOutToStIn, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:sizeToStIn]  =   (name = :sizeToStIn,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))

    ParDef_dic[:lifeConv]    =   (name = :lifeConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeStIn]    =   (name = :lifeStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeStOut]   =   (name = :lifeStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeStSize]  =   (name = :lifeStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 20, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeExc]     =   (name = :lifeExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = 50, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up))

    ParDef_dic[:lifeEcoConv]   =   (name = :lifeEcoConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeEcoStIn]   =   (name = :lifeEcoStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeEcoStOut]  =   (name = :lifeEcoStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeEcoStSize] =   (name = :lifeEcoStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:lifeEcoExc]    =   (name = :lifeEcoExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any))

    ParDef_dic[:costInvConv]   =   (name = :costInvConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costInvStIn]   =   (name = :costInvStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costInvStOut]  =   (name = :costInvStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costInvStSize] =   (name = :costInvStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costInvExc]    =   (name = :costInvExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up))

    ParDef_dic[:rateInvConv]   =   (name = :rateInvConv,   dim = (:Ts_inv, :R_inv, :Te),     default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:rateInvStIn]   =   (name = :rateInvStIn,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:rateInvStOut]  =   (name = :rateInvStOut,  dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:rateInvStSize] =   (name = :rateInvStSize, dim = (:Ts_inv, :R_inv, :C, :Te), default_val = 0.03, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:rateInvExc]    =   (name = :rateInvExc,    dim = (:Ts_inv, :R_a, :R_b, :C),  default_val = 0.03, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up))

    ParDef_dic[:costOprConv]   =   (name = :costOprConv,   dim = (:Ts_inv, :Ts_supDis, :R_inv, :Te),     default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costOprStIn]   =   (name = :costOprStIn,   dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costOprStOut]  =   (name = :costOprStOut,  dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costOprStSize] =   (name = :costOprStSize, dim = (:Ts_inv, :Ts_supDis, :R_inv, :C, :Te), default_val = nothing, inherit = (:Te => :up, :Ts_inv => :up, :R_inv => :up))
    ParDef_dic[:costOprExc]    =   (name = :costOprExc,    dim = (:Ts_inv, :Ts_supDis, :R_a, :R_b, :C),  default_val = nothing, inherit = (:Ts_inv => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up))


    # XXX parameters regarding limits on technology and exchange investment and capacity

    # investment limits and residual investment on conversion, storage and exchange
    ParDef_dic[:invConvUp]      =   (name = :invConvUp,      dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_full, :R_inv => :sum_full, :Te => :sum_full))
    ParDef_dic[:invConvLow]     =   (name = :invConvLow,     dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any))
    ParDef_dic[:invConvFix]     =   (name = :invConvFix,     dim = (:Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any))

    ParDef_dic[:invStInUp]      =   (name = :invStInUp,      dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_full, :R_inv => :sum_full, :Te => :sum_full, :C => :sum_full))
    ParDef_dic[:invStInLow]     =   (name = :invStInLow,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))
    ParDef_dic[:invStInFix]     =   (name = :invStInFix,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))

    ParDef_dic[:invStOutUp]     =   (name = :invStOutUp,     dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_full, :R_inv => :sum_full, :Te => :sum_full, :C => :sum_full))
    ParDef_dic[:invStOutLow]    =   (name = :invStOutLow,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))
    ParDef_dic[:invStOutFix]    =   (name = :invStOutFix,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))

    ParDef_dic[:invStSizeUp]    =   (name = :invStSizeUp,    dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_full, :R_inv => :sum_full, :Te => :sum_full))
    ParDef_dic[:invStSizeLow]   =   (name = :invStSizeLow,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))
    ParDef_dic[:invStSizeFix]   =   (name = :invStSizeFix,   dim = (:Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_inv => :sum_any,  :Te => :sum_any,  :C => :sum_any))

    ParDef_dic[:invExcUp]       =   (name = :invExcUp,      dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :sum_full, :R_a => :sum_full, :R_b => :sum_full, :C => :sum_full))
    ParDef_dic[:invExcLow]      =   (name = :invExcLow,     dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_a => :sum_any,  :R_b => :sum_any,  :C => :sum_any))
    ParDef_dic[:invExcFix]      =   (name = :invExcFix,     dim = (:Ts_inv, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_inv => :sum_any,  :R_a => :sum_any,  :R_b => :sum_any,  :C => :sum_any))

    # installed capacity limits and residual capacities on conversion, storage and exchange
    ParDef_dic[:capaConvUp]     =   (name = :capaConvUp,     dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any))
    ParDef_dic[:capaConvLow]    =   (name = :capaConvLow,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any))
    ParDef_dic[:capaConvFix]    =   (name = :capaConvFix,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any))
    ParDef_dic[:capaConvResi]   =   (name = :capaConvResi,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any))

    ParDef_dic[:capaStInUp]     =   (name = :capaStInUp,     dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
    ParDef_dic[:capaStInLow]    =   (name = :capaStInLow,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStInFix]    =   (name = :capaStInFix,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStInResi]   =   (name = :capaStInResi,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

    ParDef_dic[:capaStOutUp]    =   (name = :capaStOutUp,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
    ParDef_dic[:capaStOutLow]   =   (name = :capaStOutLow,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStOutFix]   =   (name = :capaStOutFix,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStOutResi]  =   (name = :capaStOutResi,  dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

    ParDef_dic[:capaStSizeUp]   =   (name = :capaStSizeUp,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
    ParDef_dic[:capaStSizeLow]  =   (name = :capaStSizeLow,  dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStSizeFix]  =   (name = :capaStSizeFix,  dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
    ParDef_dic[:capaStSizeResi] =   (name = :capaStSizeResi, dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

    ParDef_dic[:capaExcUp]   =   (name = :capaExcUp,   dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_full))
    ParDef_dic[:capaExcLow]  =   (name = :capaExcLow,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))
    ParDef_dic[:capaExcFix]  =   (name = :capaExcFix,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))
    ParDef_dic[:capaExcResi] =   (name = :capaExcResi, dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))

    # commssioned capacity limits on conversion, storage and exchange
    if options.decomm != :none
        ParDef_dic[:capaCommConvCUp]    =   (name = :capaCommConvUp,     dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any))
        ParDef_dic[:capaCommConvLow]    =   (name = :capaCommConvLow,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any))
        ParDef_dic[:capaCommConvFix]    =   (name = :capaCommConvFix,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any))

        ParDef_dic[:capaCommStInUp]     =   (name = :capaCommStInUp,     dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
        ParDef_dic[:capaCommStInLow]    =   (name = :capaCommStInLow,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
        ParDef_dic[:capaCommStInFix]    =   (name = :capaCommStInFix,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

        ParDef_dic[:capaCommStOutUp]    =   (name = :capaCommStOutUp,    dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
        ParDef_dic[:capaCommStOutLow]   =   (name = :capaCommStOutLow,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
        ParDef_dic[:capaCommStOutFix]   =   (name = :capaCommStOutFix,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

        ParDef_dic[:capaCommStSizeUp]   =   (name = :capaCommStSizeUp,   dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_full, :Te => :sum_full, :Ts_supDis => :avg_any, :C => :sum_full))
        ParDef_dic[:capaCommStSizeLow]  =   (name = :capaCommStSizeLow,  dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))
        ParDef_dic[:capaCommStSizeFix]  =   (name = :capaCommStSizeFix,  dim = (:Ts_supDis, :Ts_inv, :R_inv, :C, :Te), default_val = nothing, inherit = (:R_inv => :sum_any,  :Te => :sum_any,  :Ts_supDis => :avg_any, :C => :sum_any))

        ParDef_dic[:capaCommExcUp]   =   (name = :capaCommExcUp,   dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_full))
        ParDef_dic[:capaCommExcLow]  =   (name = :capaCommExcLow,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))
        ParDef_dic[:capaCommExcFix]  =   (name = :capaCommExcFix,  dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))
        ParDef_dic[:capaCommExcResi] =   (name = :capaCommExcResi, dim = (:Ts_supDis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_supDis => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any))
    end

    # XXX limits on quantites (including emissions and emission factors)

    upHerit_tup = (:Ts_dis => :sum_full, :Ts_inv => :sum_full, :R_dis => :sum_full, :C => :sum_full, :Te => :sum_full, :M => :sum_full)
    ofHerit_tup = (:Ts_dis => :sum_any,  :Ts_inv => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :Te => :sum_any,  :M => :sum_any)

    # actual energy limits on use, generation and storage
    ParDef_dic[:useUp]   =  (name = :useUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = upHerit_tup)
    ParDef_dic[:useLow]  =  (name = :useLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = ofHerit_tup)
    ParDef_dic[:useFix]  =  (name = :useFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = ofHerit_tup)

    ParDef_dic[:genUp]   =  (name = :genUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = upHerit_tup)
    ParDef_dic[:genLow]  =  (name = :genLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = ofHerit_tup)
    ParDef_dic[:genFix]  =  (name = :genFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = ofHerit_tup)

    ParDef_dic[:exchangeUp]   =  (name = :excUp,  dim = (:Ts_dis, :R_from, :R_to, :C), default_val = nothing, inherit = (:Ts_dis => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :C => :sum_full))
    ParDef_dic[:exchangeLow]  =  (name = :excLow, dim = (:Ts_dis, :R_from, :R_to, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any))
    ParDef_dic[:exchangeFix]  =  (name = :excFix, dim = (:Ts_dis, :R_from, :R_to, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any))

    ParDef_dic[:tradeBuyUp]   =  (name = :trdBuyUp,  dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full))
    ParDef_dic[:tradeBuyLow]  =  (name = :trdBuyLow, dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any))
    ParDef_dic[:tradeBuyFix]  =  (name = :trdBuyFix, dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any))

    ParDef_dic[:tradeSellUp]   =  (name = :trdBuyUp,  dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full))
    ParDef_dic[:tradeSellLow]  =  (name = :trdBuyLow, dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any))
    ParDef_dic[:tradeSellFix]  =  (name = :trdBuyFix, dim = (:Ts_dis, :R_dis, :C), default_val = nothing, inherit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any))

    # emission limits and factors (are computed as net values of trade and exchange)
    ParDef_dic[:emissionUp]    =  (name = :emissionLimit,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = upHerit_tup)
    ParDef_dic[:emissionFac]   =  (name = :emissionFactor, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = tuple(:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :Te => :up, :M => :up))

    # </editor-fold>

    # <editor-fold desc="XXX dispatch parameters"

    # XXX technology dispatch properties

    # availability parameters
    ParDef_dic[:avaConv]      =   (name = :avaConv,   dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up,            :Te => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :lowest,  modeDep = (:in, :out))
    ParDef_dic[:avaStIn]      =   (name = :avaStIn,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :carrier, modeDep = (:stIn,))
    ParDef_dic[:avaStOut]     =   (name = :avaStOut,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :carrier, modeDep = (:stOut,))
    ParDef_dic[:avaStSize]    =   (name = :avaStSize, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    # efficiency parameters
    ParDef_dic[:effConv]      =   (name = :effConv,  dim = (:Ts_dis, :Ts_inv, :R_dis, :Te, :M),     default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any),            presetLvl = :reference, modeDep = (:in, :out))
    ParDef_dic[:effStIn]      =   (name = :effStIn,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,   modeDep = (:stIn,))
    ParDef_dic[:effStOut]     =   (name = :effStOut, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = 1.0, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,   modeDep = (:stOut,))

    # specific storage parameters
    ParDef_dic[:stDis]     =   (name = :stDis,    dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any),           presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))
    ParDef_dic[:stInflow]  =   (name = :stInflow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :C => :up, :Ts_dis => :sum_any, :R_dis => :sum_any, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier, modeDep = (:stIn,:stOut,:stSize))

    # variable costs
    ParDef_dic[:costVarUse]   =   (name = :costVarUse,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,  modeDep = (:in,))
    ParDef_dic[:costVarGen]   =   (name = :costVarGen,   dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,  modeDep = (:out,))
    ParDef_dic[:costVarStIn]  =   (name = :costVarStIn,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,  modeDep = (:stIn,))
    ParDef_dic[:costVarStOut] =   (name = :costVarStOut, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), presetLvl = :carrier,  modeDep = (:stOut,))

    # energy related ratios (x% of energy from/to technology has to be carrier y)
    ParDef_dic[:ratioEnerUseUp]    =   (name = :ratioEnerUseUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allUse, modeDep = (:in,))
    ParDef_dic[:ratioEnerUseLow]   =   (name = :ratioEnerUseLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allUse, modeDep = (:in,))
    ParDef_dic[:ratioEnerUseFix]   =   (name = :ratioEnerUseFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allUse, modeDep = (:in,))

    ParDef_dic[:ratioEnerGenUp]    =   (name = :ratioEnerGenUp,  dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allGen, modeDep = (:out,))
    ParDef_dic[:ratioEnerGenLow]   =   (name = :ratioEnerGenLow, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allGen, modeDep = (:out,))
    ParDef_dic[:ratioEnerGenFix]   =   (name = :ratioEnerGenFix, dim = (:Ts_dis, :Ts_inv, :R_dis, :C, :Te, :M), default_val = nothing, inherit = (:Ts_inv => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), presetLvl = :allGen, modeDep = (:out,))


    # XXX further dispatch properties
    ParDef_dic[:demand]    =   (name = :demand, dim = (:Ts_dis, :R_dis, :C), default_val = 0.0, inherit = (:Ts_dis => :avg_any, :R_dis  => :sum_any), grp = :dispatch)

    # trade (=sell or buy to an external market) parameters
    ParDef_dic[:trdBuyPrc]   =   (name = :trdBuyPrc,  dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :trade)
    ParDef_dic[:trdSellPrc]  =   (name = :trdSellPrc, dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :trade)
    ParDef_dic[:trdBuyCap]   =   (name = :trdBuyPrc,  dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :trade)
    ParDef_dic[:trdSellCap]  =   (name = :trdSellPrc, dim = (:Ts_dis, :R_dis, :C, :id), default_val = nothing, inherit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), presetLvl = :trade)

    # exchange (=exchange between explicit regions) parameters
    ParDef_dic[:avaExc]     =   (name = :avaExc,  dim = (:Ts_dis, :R_a, :R_b, :C),      default_val = 1.0,     inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any), presetLvl = :exchange)
    ParDef_dic[:lossExc]    =   (name = :lossExc, dim = (:Ts_dis, :R_a, :R_b, :C),      default_val = nothing, inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any), presetLvl = :exchange)
    ParDef_dic[:costVarExc] =   (name = :costVarExc,   dim = (:Ts_dis, :R_a, :R_b, :C), default_val = nothing, inherit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any), presetLvl = :exchange)

    # </editor-fold>

    # check if sets are illdefined with respect to inheritance
    InheritRules_tup = (:sum_full, :sum_any, :avg_full, :avg_any, :uni_full, :uni_any, :up)
    WrongSetHerit_arr = filter(x -> !all(map(y -> y[1] in ParDef_dic[x].dim,ParDef_dic[x].inherit)),collect(keys(ParDef_dic)))
    WrongRulesHerit_arr = filter(x -> !all(map(y -> y[2] in InheritRules_tup,ParDef_dic[x].inherit)),collect(keys(ParDef_dic)))

    if !isempty(WrongSetHerit_arr)
        push!(report,(3, :par, :definition, "inheritance rule for a set not defined as a possible dimension: $(join(WrongSetHerit_arr,", "))"))
    end

    if !isempty(WrongRulesHerit_arr)
        push!(report,(3, :par, :definition, "invalid inheritance rule: $(join(WrongRulesHerit_arr,", "))"))
    end

    return ParDef_dic
end

# XXX matches set with input parameters, uses inheritance rules for unmatched cases
function matchSetParameter(report::DataFrame, srcSetIn_tab::IndexedTable, parameter_obj::ParElement, sets::Dict{Symbol,DataFrame}, readDig::Int64, newCol_sym::Symbol =:val, useDefault_boo::Bool = true)

    # directly returns default values if no data was provided for the parameter
    if parameter_obj.data == nothing || length(colnames(parameter_obj.data)) == 1
        return IT.transform(srcSetIn_tab,newCol_sym => fill(parameter_obj.default_val,length(srcSetIn_tab)))
    end

    searchCol_tup = colnames(srcSetIn_tab)
    paraData_tab = parameter_obj.data

    # removes sets the parameter is not specified for from search table and condenses search table accordingly
    redunSrc_tup = setdiff(searchCol_tup,colnames(paraData_tab))
    searchSet_tab = isempty(redunSrc_tup) ? srcSetIn_tab : IT.table(DB.unique(DB.select(srcSetIn_tab,DB.Not(All(redunSrc_tup...)))))

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
                unmatch_arr::Array{Int32,1} = unique(DB.select(noMatch_tab,herit[1]))
                newData_tab = inheritParameter(herit,unmatch_arr,paraData_tab,sets,readDig)
                if JuliaDB.isempty(newData_tab) continue end

                newMatch_tab = rmvDummyCol(DB.join(noMatch_tab, newData_tab; lkey = srcCol_tup, rkey = srcCol_tup, how=:inner))

                # report on inheritace
                cntNewData_int = length(newData_tab)
                cntMatch_int = length(newMatch_tab)

                push!(report,(1, :inheritance, parameter_obj.name, "inheritance via $(herit) created $(cntNewData_int) new data row(s) and assigned $(cntMatch_int) new value(s)"))

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
            push!(report,(1, :inheritance, parameter_obj.name, "assigned default value $(printVal) for $(length(defaultMatch_tab)) entrie(s)"))
        end
    end

    # expands table again by rows
    if !isempty(redunSrc_tup)
        paraMatch_tab = DB.join(paraMatch_tab, IT.table(rows(srcSetIn_tab)); lkey = srcCol_tup, rkey = srcCol_tup, how =:inner)
    end

    return DB.rename(paraMatch_tab,:val => newCol_sym)
end

inheritParameter(herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int32,1},paraData_tab::IndexedTable,sets::Dict{Symbol,DataFrame},readDig::Int64) =
                                    inheritParameter(Val{herit_par[2]}(),herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int32,1},paraData_tab::IndexedTable,sets::Dict{Symbol,DataFrame},readDig::Int64)


# <editor-fold desc="collection of subfunctions"

# XXX covers all inheritance from nodes below unmatched nodes
function inheritParameter(heritType::Union{Val{:sum_full},Val{:sum_any},Val{:avg_full},Val{:avg_any},Val{:uni_full},Val{:uni_any}},herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int32,1},paraDataIn_tab::IndexedTable,sets::Dict{Symbol,DataFrame},readDig::Int64)

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

    newData_tab = IT.table(NamedTuple{colNam_tup}((fill(Int32[],length(colNam_tup)-1)...,Float64[])), pkey = vcat(heritSet_sym,noHeritSet_tup...))

    # gets all children of unmatched ids to filter relevant part of tree
    childrenUnmatch_arr = unique(vcat(map(x -> getChildren(x,sets[heritSetShort_sym],true),unmatch_arr)...))

    paraDataFilt_tab = DB.filter(r -> getproperty(r,heritSet_sym) in childrenUnmatch_arr,paraDataIn_tab)
    if isempty(paraDataFilt_tab) return newData_tab end

    paraDataSrc_tab = IT.transform(paraDataFilt_tab, :pare => DB.select(paraDataFilt_tab,heritSet_sym => x -> sets[heritSetShort_sym][x,:pare]))

    # loops going upwards within tree trying to obtain new values
    newVal_boo = true

    while newVal_boo
        # saves all children of parents currently used in the grouped table within a dictionary to use below within loop over rows
        if heritFull_boo
            childPar_dic = Dict(x => getChildren(x,sets[heritSetShort_sym], false, x != 0 ? sets[heritSetShort_sym][x,:lvl]+1 : 1) for x in unique(DB.select(paraDataSrc_tab,:pare)))
        end

        # groups table by parents and not used dimensions according to aggregation rule (sum, any, or unique)
        if heritAgg_sym == :sum
            paraDataGrp_tab = JuliaDB.groupby(paraDataSrc_tab, Tuple(vcat(noHeritSet_tup...,:pare)), usekey = false, select = (heritSet_sym,:val)) do y
               NamedTuple{(heritSet_sym,:valAgg)}(tuple(Array(getproperty(y,heritSet_sym)), round(sum(y.val),digits = readDig)))
            end
        elseif heritAgg_sym == :avg
            paraDataGrp_tab = JuliaDB.groupby(paraDataSrc_tab, Tuple(vcat(noHeritSet_tup...,:pare)), usekey = false, select = (heritSet_sym,:val)) do y
               NamedTuple{(heritSet_sym,:valAgg)}(tuple(Array(getproperty(y,heritSet_sym)), round(Statistics.mean(y.val),digits = readDig)))
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
        paraDataSrc_tab = IT.transform(paraDataSrc_tab, :pare => DB.select(paraDataSrc_tab,heritSet_sym => x -> sets[heritSetShort_sym][x,:pare]))
    end

    return newData_tab
end

# XXX covers direct inheritance from upper nodes
function inheritParameter(heritType::Val{:up},herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int32,1},paraDataIn_tab::IndexedTable,sets::Dict{Symbol,DataFrame},readDig::Int64)

    heritSetShort_sym = Symbol(split(String(herit_par[1]),"_")[1])

    unmatchChild_dic = Dict(x => intersect(unmatch_arr,getChildren(x,sets[heritSetShort_sym],true)) for x in unique(DB.select(paraDataIn_tab, herit_par[1])))

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
