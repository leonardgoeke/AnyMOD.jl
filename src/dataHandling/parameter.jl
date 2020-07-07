
# <editor-fold desc="definition and handling of parameters"

# XXX defines all existing parameters
function defineParameter(options::modOptions,report::Array{Tuple,1})
    parDef_dic = Dict{Symbol, NamedTuple}()

    # <editor-fold desc="XXX expansion parameters"

    # XXX general expansion
    parDef_dic[:rateDisc]  =   (dim = (:Ts_disSup, :R_exp), defVal = 0.02, herit = (:Ts_disSup => :up, :R_exp => :up, :R_exp => :avg_any, :Ts_disSup => :avg_any), part = :obj)

    # XXX technology and exchange expansion

    parDef_dic[:stInToConv]   = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:stOutToStIn]  = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:sizeToStIn]   = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)

    parDef_dic[:delConv]   = (dim = (:Ts_expSup, :R_exp, :Te),     defVal = 0, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techConv)
    parDef_dic[:delStIn]   = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 0, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:delStOut]  = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 0, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:delStSize] = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 0, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:delExc]    = (dim = (:Ts_expSup, :R_a, :R_b, :C),  defVal = 0, herit = (:Ts_expSup => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), part = :exc)

    parDef_dic[:lifeConv]    = (dim = (:Ts_expSup, :R_exp, :Te),     defVal = 20, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techConv)
    parDef_dic[:lifeStIn]    = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 20, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:lifeStOut]   = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 20, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:lifeStSize]  = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = 20, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:lifeExc]     = (dim = (:Ts_expSup, :R_a, :R_b, :C),  defVal = 50, herit = (:Ts_expSup => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), part = :exc)

    parDef_dic[:lifeEcoConv]   = (dim = (:Ts_expSup, :R_exp, :Te),     defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:lifeEcoStIn]   = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:lifeEcoStOut]  = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:lifeEcoStSize] = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:lifeEcoExc]    = (dim = (:Ts_expSup, :R_a, :R_b, :C),  defVal = nothing, herit = (:Ts_expSup => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), part = :obj)

    parDef_dic[:costExpConv]   = (dim = (:Ts_expSup, :R_exp, :Te),     defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costExpStIn]   = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costExpStOut]  = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costExpStSize] = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costExpExc]    = (dim = (:Ts_expSup, :R_a, :R_b, :C),  defVal = nothing, herit = (:Ts_expSup => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), part = :obj)

    parDef_dic[:rateExpConv]   = (dim = (:Ts_expSup, :R_exp, :Te),     defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:rateExpStIn]   = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:rateExpStOut]  = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:rateExpStSize] = (dim = (:Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:rateExpExc]    = (dim = (:Ts_expSup, :R_a, :R_b, :C),  defVal = nothing, herit = (:Ts_expSup => :up, :R_a => :avg_any, :R_b => :avg_any, :C => :up), part = :obj)

    parDef_dic[:costOprConv]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te),     defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costOprStIn]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costOprStOut]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costOprStSize] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :R_exp => :up), part = :obj)
    parDef_dic[:costOprExc]    = (dim = (:Ts_disSup, :R_a, :R_b, :C),  defVal = nothing, herit = (:Ts_disSup => :up, :R_a => :avg_any, :R_b => :avg_any, :R_a => :up, :R_b => :up, :C => :up), part = :obj)


    # XXX parameters regarding limits on technology and exchange expansion and capacity

    # expansion limits on conversion, storage and exchange
    parDef_dic[:expConvUp]  = (dim = (:Ts_exp, :R_exp, :Te), defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:expConvLow] = (dim = (:Ts_exp, :R_exp, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:expConvFix] = (dim = (:Ts_exp, :R_exp, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:expStInUp]  = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:expStInLow] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:expStInFix] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:expStOutUp]  = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:expStOutLow] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:expStOutFix] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:expStSizeUp]  = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:expStSizeLow] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:expStSizeFix] = (dim = (:Ts_exp, :R_exp, :C, :Te), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:expExcUp]  = (dim = (:Ts_exp, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_exp => :sum_full, :R_a => :sum_full, :R_b => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:expExcLow] = (dim = (:Ts_exp, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_a => :sum_any,  :R_b => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:expExcFix] = (dim = (:Ts_exp, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_a => :sum_any,  :R_b => :sum_any,  :C => :sum_any),  part = :lim)

    # installed capacity limits and residual capacities on conversion, storage and exchange
    parDef_dic[:capaConvUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaConvLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaConvFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStInUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStInLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStInFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStOutUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStOutLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStOutFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStSizeUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStSizeLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStSizeFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaExcUp]  = (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_full),                   part = :lim)
    parDef_dic[:capaExcLow] = (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any),                    part = :lim)
    parDef_dic[:capaExcFix] = (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any),                    part = :lim)

    parDef_dic[:capaConvResi]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te),     defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any, :Ts_disSup => :up),                  part = :techConv)
    parDef_dic[:capaStInResi]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)
    parDef_dic[:capaStOutResi]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)
    parDef_dic[:capaStSizeResi] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)

    parDef_dic[:capaExcResi]    = (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :Ts_disSup => :up), part = :exc)
    parDef_dic[:capaExcResiDir] = (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :Ts_disSup => :up), part = :exc)

    # commssioned capacity limits on conversion, storage and exchange
    if options.decomm != :none
        parDef_dic[:oprCapaConvUp]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any,  :Ts_expSup => :sum_full), part = :lim)
        parDef_dic[:oprCapaConvLow]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)
        parDef_dic[:oprCapaConvFix]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)

        parDef_dic[:oprCapaStInUp]     =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
        parDef_dic[:oprCapaStInLow]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
        parDef_dic[:oprCapaStInFix]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

        parDef_dic[:oprCapaStOutUp]    =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
        parDef_dic[:oprCapaStOutLow]   =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
        parDef_dic[:oprCapaStOutFix]   =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

        parDef_dic[:oprCapaStSizeUp]   =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :C => :sum_full, :Ts_expSup => :sum_full), part = :lim)
        parDef_dic[:oprCapaStSizeLow]  =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
        parDef_dic[:oprCapaStSizeFix]  =   (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :C, :Te), defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :C => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

        parDef_dic[:oprCapaExcUp]   =   (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_full), part = :lim)
        parDef_dic[:oprCapaExcLow]  =   (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any),  part = :lim)
        parDef_dic[:oprCapaExcFix]  =   (dim = (:Ts_disSup, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_a => :sum_any,  :R_b => :sum_any, :C => :sum_any),  part = :lim)
    end

    # XXX limits on quantites (including emissions and emission factors)

    upHerit_tup = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :Te => :sum_full, :M => :sum_full)
    ofHerit_tup = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :Te => :sum_any,  :M => :sum_any)

    # actual energy limits on use, generation and storage
    parDef_dic[:useUp]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:useLow]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:useFix]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:genUp]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:genLow]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:genFix]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:stOutUp]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:stOutLow]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:stOutFix]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:stInUp]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:stInLow]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:stInFix]  =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:excUp]   =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:excLow]  =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:excFix]  =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:excDirUp]   =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:excDirLow]  =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:excDirFix]  =  (dim = (:Ts_dis, :R_from, :R_to, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:crtUp]   =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:crtLow]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:crtFix]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)

    parDef_dic[:lssUp]   =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:lssLow]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:lssFix]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)

    parDef_dic[:trdBuyUp]   =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:trdBuyLow]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:trdBuyFix]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any),  part = :lim)

    parDef_dic[:trdSellUp]   =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full), part = :lim)
    parDef_dic[:trdSellLow]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any),  part = :lim)
    parDef_dic[:trdSellFix]  =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any),  part = :lim)

    # emission limits and factors (are computed as net values of trade and exchange)
    parDef_dic[:emissionUp]    =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:emissionFac]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = tuple(:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :M => :up), part = :lim)
    parDef_dic[:emissionPrc]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = tuple(:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :M => :up), part = :obj)

    # </editor-fold>

    # <editor-fold desc="XXX dispatch parameters"

    # XXX technology dispatch properties

    # availability parameters
    parDef_dic[:avaConv]      =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :Te, :M),     defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up,            :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techConv, techPre = (preset = :lowest,    mode = (:in, :out)))
    parDef_dic[:avaStIn]      =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:avaStOut]     =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:avaStSize]    =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))

    # efficiency parameters
    parDef_dic[:effConv]      =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :Te, :M),     defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any),            part = :techConv, techPre = (preset = :reference, mode = (:in, :out)))
    parDef_dic[:effStIn]      =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:effStOut]     =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))

    # specific storage parameters
    parDef_dic[:stDis]     =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any),           part = :techSt, techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:stInflow]  =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te),     defVal = nothing, herit = (:Ts_expSup => :up, :C => :up, :Ts_dis => :sum_any, :R_dis => :sum_any, :Te => :up), part = :techSt, techPre = (preset = :carrierSt, mode = tuple()))

    # variable costs
    parDef_dic[:costVarUse]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :obj, techPre = (preset = :carrierIn,  mode = (:in,)))
    parDef_dic[:costVarGen]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :obj, techPre = (preset = :carrierOut, mode = (:out,)))
    parDef_dic[:costVarStIn]  =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :obj, techPre = (preset = :carrierSt,  mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:costVarStOut] =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :obj, techPre = (preset = :carrierSt,  mode = (:stIn,:stOut,:stLvl)))

    # energy related ratios (x% of energy from/to technology has to be carrier y)
    parDef_dic[:ratioEnerUseUp]    =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minUse, mode = (:in,)))
    parDef_dic[:ratioEnerUseLow]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minUse, mode = (:in,)))
    parDef_dic[:ratioEnerUseFix]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minUse, mode = (:in,)))

    parDef_dic[:ratioEnerGenUp]    =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minGen, mode = (:out,)))
    parDef_dic[:ratioEnerGenLow]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minGen, mode = (:out,)))
    parDef_dic[:ratioEnerGenFix]   =   (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M), defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up), part = :techConv, techPre = (preset = :minGen, mode = (:out,)))

    # XXX further dispatch properties
    parDef_dic[:dem]     =  (dim = (:Ts_dis, :R_dis, :C), defVal = 0.0, herit = (:Ts_dis => :avg_any, :R_dis  => :sum_any), part = :bal)
    parDef_dic[:costCrt] =  (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :bal)
    parDef_dic[:costLss] = (dim = (:Ts_dis, :R_dis, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :bal)

    # trade (=sell or buy to an external market) parameters
    parDef_dic[:trdBuyPrc]   =   (dim = (:Ts_dis, :R_dis, :C, :id), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :trd)
    parDef_dic[:trdSellPrc]  =   (dim = (:Ts_dis, :R_dis, :C, :id), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :trd)
    parDef_dic[:trdBuyCap]   =   (dim = (:Ts_dis, :R_dis, :C, :id), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :trd)
    parDef_dic[:trdSellCap]  =   (dim = (:Ts_dis, :R_dis, :C, :id), defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :trd)

    # exchange (=exchange between explicit regions) parameters
    parDef_dic[:avaExc]     =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = 1.0,     herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :exc)
    parDef_dic[:lossExc]    =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = 0.0,     herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :exc)
    parDef_dic[:costVarExc] =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :obj)

    parDef_dic[:avaExcDir]     =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :exc)
    parDef_dic[:lossExcDir]    =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :exc)
    parDef_dic[:costVarExcDir] =   (dim = (:Ts_dis, :R_a, :R_b, :C), defVal = nothing, herit = (:Ts_dis => :up, :R_a => :up, :R_b => :up, :R_a => :avg_any, :R_b => :avg_any, :Ts_dis => :avg_any, :C => :up), part = :obj)

    # </editor-fold>

    # check if sets are illdefined with respect to inheritance
    heritRules_tup = (:sum_full, :sum_any, :avg_full, :avg_any, :uni_full, :uni_any, :up)
    wrongSetHerit_arr = filter(x -> !all(map(y -> y[1] in parDef_dic[x].dim,parDef_dic[x].herit)),collectKeys(keys(parDef_dic)))
    wrongRulesHerit_arr = filter(x -> !all(map(y -> y[2] in heritRules_tup,parDef_dic[x].herit)),collectKeys(keys(parDef_dic)))

    if !isempty(wrongSetHerit_arr)
        push!(report,(3, "parameter definition", "", "inheritance rule for a set not defined as a possible dimension: $(join(wrongSetHerit_arr,", "))"))
    end

    if !isempty(wrongRulesHerit_arr)
        push!(report,(3, "parameter read-in", "definition", "invalid inheritance rule: $(join(wrongRulesHerit_arr,", "))"))
    end

    return parDef_dic
end

# XXX assign parameter to model parts
function parameterToParts!(paraTemp_dic::Dict{String,Dict{Symbol,DataFrame}}, techIdx_arr::Array{Int,1}, anyM::anyModel)

    # parameter defined within input data
    allPar_arr = unique(vcat(collectKeys.(keys.(values(paraTemp_dic)))...))
     # parameter actually used in the model (difference are the exchange related parameters, that can be provided both directed and symmetric, but within the model only directed values are being used)
    parToFile_dic = Dict(x => collectKeys(keys(paraTemp_dic[x])) for x in keys(paraTemp_dic))

    # gets defintion of parameters and checks, if all input parameters are defined
    parDef_dic = defineParameter(anyM.options,anyM.report)
    undefinedPar_arr = setdiff(unique(vcat(values(parToFile_dic)...)),keys(parDef_dic))
    if !isempty(undefinedPar_arr)
        for undefined in undefinedPar_arr push!(anyM.report,(3,"parameter read-in","definition","parameter with the name $(string(undefined)) does not exist")) end
        print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options)
    end

    # maps potential nodes for inheritance from technology tree to "actual" technologies
    techToPar_dic = Dict{Symbol,Dict{Int32,Array{Int32,1}}}()
    techToPar_dic[:up] = Dict(x => vcat(x,getAncestors(x,anyM.sets[:Te],:int)...) for x in techIdx_arr)
    techToPar_dic[:down] = Dict(x => vcat(x,getDescendants(x,anyM.sets[:Te],true)...) for x in techIdx_arr)
    techToPar_dic[:both] = Dict(x => union(techToPar_dic[:up][x],techToPar_dic[:down][x]) for x in techIdx_arr)
    techToPar_dic[:none] = Dict(x => [x] for x in techIdx_arr)

    convTechIdx_arr = filter(r -> !isempty(intersect((:gen,:use),keys(anyM.parts.tech[r].carrier))),techIdx_arr)
    stTechIdx_arr   = filter(r -> !isempty(intersect((:stExtIn,:stExtOut,:stIntIn,:stIntOut),keys(anyM.parts.tech[r].carrier))),techIdx_arr)

    # XXX loop over all actual parameters to assign them to parts of the model
    @threads for parIt in allPar_arr

        # ensures all dataframes with data from single files have the same columns so they can be merged
        relFiles_arr = collect(filter(y -> parIt in parToFile_dic[y],keys(paraTemp_dic)))
        allCol_arr = unique(vcat(map(x -> namesSym(paraTemp_dic[x][parIt]), relFiles_arr)...))
        for parFile in relFiles_arr
            misCol_arr = setdiff(allCol_arr,namesSym(paraTemp_dic[parFile][parIt]))
            for mis in misCol_arr
                paraTemp_dic[parFile][parIt][!,mis] = fill(convert(Int32,0),nrow(paraTemp_dic[parFile][parIt]))
            end
        end
        # actually merge data frames
        allParData_df = vcat(map(x -> paraTemp_dic[x][parIt],relFiles_arr)...)

        # order regions in ascending order so regions are not ambivalent anymore and duplicates can be identified
        if :R_b in namesSym(allParData_df) && !(occursin("Dir",string(parIt)))
            sortR_mat = sort(hcat([allParData_df[!,x] for x in (:R,:R_b)]...);dims = 2)
            for (index,col) in enumerate((:R,:R_b)) allParData_df[!,col] = sortR_mat[:,index] end
        end

        # XXX checks for duplicates and removes them in case
        nonUnique_bool = nonunique(allParData_df)
        if any(nonUnique_bool)
            push!(anyM.report,(1,"parameter read-in","validity check","non-unique entries discovered for $(string(parIt))"))
            delete!(allParData_df,nonUnique_bool)
        end

        # XXX checks for contradicting values
        rmvVal_df = removeVal(allParData_df)
        if !isempty(rmvVal_df)
            contradic_bool = nonunique(allParData_df[:,rmvVal_df])
            if any(contradic_bool)
                 push!(anyM.report,(3,"parameter read-in","validity check","contradicting entries discovered for $(string(parIt))"))
            end
        end

        # XXX assign parameters to parts
        parDef_tup = parDef_dic[parIt]
        parPart_sym =parDef_tup.part

        if parPart_sym != :techSt && parPart_sym != :techConv
            # adds parameter to non-technology parts
            getfield(anyM.parts,parPart_sym).par[parIt] = ParElement(allParData_df,parDef_tup,parIt,anyM.report)
        else
            allParTech_arr = :Te in namesSym(allParData_df) ? unique(allParData_df[!,:Te]) : [0]
            # determines how technology might inherit from other technology nodes (not at all, by going up, by going down or both)
            heritRules_arr = map(x -> x[2],filter(x -> x[1] == :Te, collect(parDef_tup.herit)))
            if isempty(heritRules_arr)
                herit_sym = :none
            else
                if unique(heritRules_arr) == [:up]
                    herit_sym = :up
                elseif :up in heritRules_arr
                    herit_sym = :both
                else
                    herit_sym = :down
                end
            end
            for relTech in filter(x -> !isempty(intersect(allParTech_arr,techToPar_dic[herit_sym][x])), parPart_sym == :techSt ? stTechIdx_arr : convTechIdx_arr)
                # filters all entries of possible inheritance for each technology
                filtParData_df = :Te in namesSym(allParData_df) ? filter(row -> row.Te in techToPar_dic[herit_sym][relTech], allParData_df) : allParData_df
                # removes potential zero columns from data being actually written to part
                rmvZeroParData_df = filtParData_df[!,filter(x -> unique(filtParData_df[!,x]) != [0] || x == :val,namesSym(filtParData_df))]
                anyM.parts.tech[relTech].par[parIt] = ParElement(rmvZeroParData_df,parDef_tup,parIt,anyM.report)
            end
        end
    end

    # XXX adds parameter object for parameters where no explicit values where provided, but a default value exists
    for parUndef in keys(filter(r -> r[2].defVal != nothing,parDef_dic))
        parPart_sym = parDef_dic[parUndef].part
        if parPart_sym != :techSt && parPart_sym != :techConv
            if !haskey(getfield(anyM.parts,parPart_sym).par,parUndef)
                getfield(anyM.parts,parPart_sym).par[parUndef] = ParElement(DataFrame(),parDef_dic[parUndef],parUndef,anyM.report)
            end
        else
            for relTech in filter(x -> !haskey(anyM.parts.tech[x].par,parUndef),parPart_sym == :techSt ? stTechIdx_arr : convTechIdx_arr)
                anyM.parts.tech[relTech].par[parUndef] = ParElement(DataFrame(),parDef_dic[parUndef],parUndef,anyM.report)
            end
        end
    end

    return parDef_dic
end

# XXX perform pre-setting of dispatch parameters for all technologies
function presetDispatchParameter!(part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},newHerit_dic::Dict{Symbol,Tuple{Pair{Symbol,Symbol},Pair{Symbol,Symbol}}},
                                                                                                ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	relPar_arr = filter(x -> :techPre in keys(parDef_dic[x]) && (!(isempty(part.par[x].data)) || occursin("eff",string(x))), collectKeys(keys(part.par)))
	parPre_dic = Dict(x => parDef_dic[x].techPre.preset for x in relPar_arr)
	preType_arr = union(values(parPre_dic))

    typeVar_dic = Dict(:out => [:gen, :stIntIn], :in => [:use,:stIntOut], :stIn => [:stExtIn, :stOut], :stOut => [:stExtOut, :stIntOut], :stLvl => [:stLvl])
    modeDep_dic = Dict(x => DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[]) for x in union(values(typeVar_dic)...))

	for preType in preType_arr
		# get all relevant carriers
		specMode_boo = !isempty(part.modes) && !isempty(filter(y -> :M in namesSym(part.par[y].data), keys(filter(x -> x[2] == preType,parPre_dic))))

        # creates table of relevant capacity resolutions and the level of pre-setting
        capaLvl_df = unique(vcat(map(x -> select(x,intCol(x)),values(prepTech_dic[preType != :carrierSt ? :capaConv : :capaStSize]))...)) |> (x -> select(copy(x),intCol(x)))

        # creates dataframe depending on the respective pre-set mode
		if preType == :lowest
			carConv_arr = union(map(x -> getfield(part.carrier,x), intersect((:gen,:use),keys(part.carrier)))...)
			lowest_tup = map(x -> anyM.cInfo[x],carConv_arr) |> (y -> [maximum(getfield.(y,:tsDis)), part.disAgg ? part.balLvl.exp[2] : maximum(getfield.(y,:rDis))])
            capaLvl_df[!,:lvlTs] .= lowest_tup[1]; capaLvl_df[!,:lvlR] .= lowest_tup[2]
		elseif preType == :reference
			ref_tup = part.balLvl.ref
			if isempty(ref_tup) continue end
            capaLvl_df[!,:lvlTs] .= ref_tup[1]; capaLvl_df[!,:lvlR] .= ref_tup[2];
		elseif preType == :carrierIn || preType == :carrierOut || preType == :carrierSt

			if preType == :carrierIn || preType == :carrierOut
				car_arr = (preType == :carrierIn ? :use : :gen) |> (y -> haskey(part.carrier,y) ? collect(getfield(part.carrier,y)) : Int[])
				if isempty(car_arr) continue end
                capaLvl_df[!,:C] .= car_arr
                capaLvl_df = flatten(capaLvl_df,:C)
			else
                # filter carriers that are can be actively stored, although they got descendants
                intC_arr = union(collect(part.actSt),map(y -> part.carrier[y],filter(x -> x in keys(part.carrier),[:stIntIn,:stIntOut])) |> (y -> isempty(y) ? Int[] : union(y...)))
                capaLvl_df = replCarLeafs(capaLvl_df,anyM.sets[:C],noLeaf = intC_arr)
                # filter entries that are already descendants of carrier being actively stored
                unique(vcat(map(x -> filter(y -> x != y,getDescendants(x,anyM.sets[:C],true)),unique(capaLvl_df[!,:C]))...)) |> (z -> filter!(x -> !(x.C in z) || x.C in intC_arr,capaLvl_df))
                car_arr = unique(capaLvl_df[!,:C])
			end
			resC_dic = Dict(x => anyM.cInfo[x] |> (y -> [getfield(y,:tsDis), part.disAgg ? part.balLvl.exp[2] : getfield(y,:rDis)]) for x in car_arr)
            capaLvl_df = combine(x -> resC_dic[x.C[1]] |> (y -> (lvlTs = y[1], lvlR = y[2])), groupby(capaLvl_df, namesSym(capaLvl_df)))
		elseif preType == :minUse || preType == :minGen
			car_arr = (preType == :minUse ? :use : :gen) |> (y -> haskey(part.carrier,y) ? collect(getfield(part.carrier,y)) : Int[])
			if isempty(car_arr) continue end

            insertcols!(capaLvl_df,1,:C => fill(car_arr,size(capaLvl_df,1)))
            capaLvl_df = flatten(capaLvl_df,:C)

            reso_tup = map(x -> anyM.cInfo[x],car_arr) |> (y -> [minimum(getfield.(y,:tsDis)), part.disAgg ? part.balLvl.exp[2] : minimum(getfield.(y,:rDis))])
            capaLvl_df[!,:lvlTs] .= reso_tup[1]; capaLvl_df[!,:lvlR] .= reso_tup[2];
		end

        # expand based on code above to full table for pre-setting of dispatch paramters
		dispReso_df = expandExpToDisp(capaLvl_df,ts_dic,r_dic)

		# additionally creates mode specific table in case different modes exists for technology
		if specMode_boo
            dispResoM_df = copy(dispReso_df)
            insertcols!(dispResoM_df,1, :M =>  fill(part.modes,size(dispReso_df,1)))
            dispResoM_df = flatten(dispResoM_df,:M)
		end

        # loops over all parameters of specific pre-setting type
		for parItr in keys(filter(x -> x[2] == preType,parPre_dic))
            parPef_ntup = parDef_dic[parItr]
			newPar_obj, report = resetParameter(:M in namesSym(part.par[parItr].data) ? dispResoM_df : dispReso_df, part.par[parItr], anyM.sets, anyM.options, length(part.modes), haskey(newHerit_dic,preType) ? newHerit_dic[preType] : tuple())

            if :M in namesSym(newPar_obj.data)
                mode_df = unique(filter(x -> x.M != 0, newPar_obj.data)[!,Not([:val,:M])])

                # loops over all types of relevant variables (:gen, :use etc.) that have to be mode specific
                for va in intersect(union(map(x -> typeVar_dic[x], parPef_ntup.techPre.mode)...),keys(part.carrier) |> (y -> isempty(intersect(y,(:stExtIn,:stIntIn))) ? y : [:stLvl,y...]))
                    modeItr_df = copy(mode_df)
                    # determines relevant carriers of variables
                    if :C in parPef_ntup.dim # carrier is already specified within parameter data
                        car_arr = unique(modeItr_df[!,:C])
                    else # carrier is not in parameter data, all possible carriers of respective variable need to be obtained
                        car_arr = collect(getfield(part.carrier,va))
                        modeItr_df[!,:C] .= [car_arr]
                        modeItr_df = flatten(modeItr_df,:C)
                    end

                    # adds temporal and spatial level to dataframe
                    cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[x].rDis) for x in car_arr)
                    modeItr_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],modeItr_df[!,:C])
                    modeItr_df[!,:lvlR] = map(x -> cToLvl_dic[x][2],modeItr_df[!,:C])

                    # expands dataframe along spatial and temporal level according to resolution of respective carriers
                    for dim in (:R,:Ts)
                        dimCol = Symbol(dim,:_dis); lvl = Symbol(:lvl,dim)
                        dim_dic = Dict((x[dimCol],x[lvl]) =>  getDescendants(x[dimCol], anyM.sets[dim],false,x[lvl]) |> (y -> isempty(y) ? getAncestors(x[dimCol],anyM.sets[dim],:int,x[lvl])[end] : y)
                                                                                                                                                        for x in eachrow(unique(modeItr_df[!,[dimCol,lvl]])))
                        modeItr_df[!,dimCol] = map(x -> dim_dic[(x[dimCol],x[lvl])],eachrow(modeItr_df[!,[dimCol,lvl]]))
                        modeItr_df = flatten(modeItr_df[!,Not(lvl)],dimCol)
                    end

                    modeDep_dic[va] = unique(vcat(modeDep_dic[va],modeItr_df))
                end
            end
            # set lower limit for availabilities to avoid really small but non-zero values
            if parItr in (:avaConv, :avaStIn, :avaStOut, :avaStSize)
                lowVal_arr = newPar_obj.data[!,:val] .< anyM.options.avaMin
                newPar_obj.data[lowVal_arr,:val] .= 0.0
            end

            part.par[parItr] = newPar_obj
        end
	end

    return modeDep_dic
end

# XXX pre-sets specific dispatch parameter
function resetParameter(newData_df::DataFrame, par_obj::ParElement, sets::Dict{Symbol,Tree}, options::modOptions, cntM_int::Int = 0, newHerit_tup::Tuple = ())
    # gets dimension of search tables and parameter without mode
    newData_df = select(newData_df,intersect(namesSym(newData_df),par_obj.dim))
    # creates empty report, that entries are written to within subprocess
    report = Array{Tuple,1}()

    if !(:M in namesSym(newData_df))
        # in case modes are not being searched for just directly set data
        par_obj.data = matchSetParameter(newData_df,par_obj,sets) |> (x -> select(x,orderDim(namesSym(x))))
    else
        # looks up original table without applying default values
        matchData1_df = matchSetParameter(newData_df,par_obj,sets,newCol = :val, useDef = false)

        # filter returned table by weather a mode was specified
        noMode_df = filter(r -> r.M == 0,matchData1_df)
        mode_df = filter(r -> r.M != 0,matchData1_df)

        # groups mode related data for further analysis
        resDim_arr = filter(x -> x != :M ,intersect(par_obj.dim,namesSym(matchData1_df)))

        if !isempty(mode_df)

            # filteres entries were there is not one value for each mode
            modeGrp_gdf = groupby(mode_df, resDim_arr)
            modeGrpDef_arr = filter(r -> cntM_int == size(r,1), collect(modeGrp_gdf))

            if length(modeGrp_gdf.ends) > length(modeGrpDef_arr)
                push!(report,(2, "parameter pre-setting", string(par_obj.name), "parameter data was not specified for all modes in some cases for $(createFullString(t,sets[:Te])), existing values were ignored"))
            end

            # filters entries where mode values are not distinct, reports on it and uses these entries as non-mode specific data
            disMode_arr = filter(r -> length(unique(r[!,:val])) != 1, modeGrpDef_arr)
            if length(modeGrpDef_arr) > length(disMode_arr)
                push!(report,(2, "parameter pre-setting", string(par_obj.name), "parameter data was the same for all modes in some cases for $(createFullString(t,sets[:Te])), no differentiation between modes was applied in these cases"))
                noMode_df = vcat(noMode_df, vcat(filter(r -> length(unique(r[!,:val])) == 1, modeGrpDef_arr)...) )
            end

            # filters data where distinct mode data is provided for all modes and expends resulting table again
            finalMode_df =  vcat(disMode_arr...)
        else
            finalMode_df = mode_df
        end

        # gets all data, where no values where obtained successfully yet and look them up again applying the default value and not specifing the mode anymore
        # (hence now non mode-specific parameter values for technologies with modes are taken into account => mode-specific parameter values generally overwrite non-mode specific parameter values)
        newSearch_df = unique(antijoin(newData_df[!,resDim_arr],  vcat(finalMode_df, noMode_df)[!,Not(:val)], on = resDim_arr))

        if !isempty(newSearch_df)
            newSearch_df[!,:M] .= 0
            matchData2_df = matchSetParameter(newSearch_df,par_obj,sets)
            noMode_df = vcat(matchData2_df,noMode_df)
        end

        # returns tables with and without mode data to parameter object
        par_obj.data = vcat(noMode_df,finalMode_df) |> (x -> select(x,orderDim(namesSym(x))))
    end
    # sets new inherit rules and default value
    par_obj.herit = newHerit_tup

    return par_obj, report
end

# XXX creates new parameter objects for discount factors from discount rates provided
function computeDisFac!(partObj::OthPart,anyM::anyModel)

	# XXX discount factor for technologies
	rExp_arr = union(map(x -> getfield.(getNodesLvl(anyM.sets[:R],x),:idx), unique(getfield.(values(anyM.cInfo),:rExp)))...)
	discR_df = matchSetParameter(flatten(flatten(DataFrame(Ts_disSup = anyM.supTs.step, R_exp = rExp_arr),:Ts_disSup),:R_exp),partObj.par[:rateDisc],anyM.sets)

	discR_df[!,:disFac] = 1 ./ (1 .+ discR_df[!,:val]).^anyM.options.shortExp
	discR_df[!,:disFac] = map(x -> filter(y -> y < x.Ts_disSup ,collect(anyM.supTs.step)) |> (z -> prod(filter(y -> y.R_exp == x.R_exp && y.Ts_disSup in z, discR_df)[!,:disFac])*x.disFac),eachrow(discR_df))
	select!(discR_df,Not(:val))

	discPar_obj = copy(partObj.par[:rateDisc],rename(discR_df,:disFac => :val))
	discPar_obj.name = :discFac
	discPar_obj.defVal = nothing
	partObj.par[:disFac] = discPar_obj

	# XXX discount factor for exchange (average of from and to region)
	discRExc_df = rename(copy(discR_df),:R_exp => :R_from,:disFac => :disFacFrom)
	discRExc_df[!,:R_to] .= [unique(discRExc_df[!,:R_from])]
	discRExc_df = flatten(discRExc_df,:R_to)

	discRExc_df = innerjoin(discRExc_df,discR_df, on = [:Ts_disSup,:R_to] .=> [:Ts_disSup,:R_exp])
	discRExc_df[!,:disFac] = (discRExc_df[!,:disFac] + discRExc_df[!,:disFacFrom]) * 0.5
	select!(discRExc_df,Not(:disFacFrom))

	discPar_obj = copy(partObj.par[:rateDisc],rename(discRExc_df,:disFac => :val))
	discPar_obj.name = :disFacExc
	discPar_obj.defVal = nothing
	discPar_obj.dim = (:Ts_dis, :R_from, :R_to)
	discPar_obj.herit = (:Ts_dis => :up, :R_from => :up, :R_to => :up, :Ts_dis => :avg_any, :R_from => :avg_any, :R_to => :avg_any)
	partObj.par[:disFacExc] = discPar_obj
end

# XXX extract specified limit parameter from the limit part of the model
function getLimPar(partLim::OthPart,par_sym::Symbol, tech_tr::Tree; tech::Int = 0)

	if par_sym in keys(partLim.par)
		parLim_obj = copy(partLim.par[par_sym])
		if :Te in namesSym(parLim_obj.data) # case for technology limits with values differentiated by tech
			parLim_obj.data = filter(x -> x.Te in [[tech];getAncestors(tech,tech_tr,:int,0)], parLim_obj.data)
			if isempty(parLim_obj.data)
				parLim_obj = ParElement()
			end
		end
	else
		parLim_obj = ParElement()
	end

	return parLim_obj
end

# </editor-fold>

# <editor-fold desc="perform match between dimension tables and parameter data"

# XXX matches set with input parameters, uses inheritance rules for unmatched cases
function matchSetParameter(srcSetIn_df::DataFrame, par_obj::ParElement, sets::Dict{Symbol,Tree}; newCol::Symbol =:val, useDef::Bool = true, useNew::Bool = true)

     # directly return search dataframes with added empty column if it is empty itself
    if isempty(srcSetIn_df)
        paraMatch_df = copy(srcSetIn_df)
        paraMatch_df[!,newCol]  = Float64[]
        return paraMatch_df
    end

    # directly returns default values if no data was provided for the parameter
    if isempty(par_obj.data) || length(namesSym(par_obj.data)) == 1
        paraMatch_df = copy(srcSetIn_df)
        paraMatch_df[!,newCol] = fill(isempty(par_obj.data) ? par_obj.defVal : par_obj.data[1,:val],size(paraMatch_df,1))
        return paraMatch_df
    end

    searchCol_arr = namesSym(srcSetIn_df)
    paraData_df = par_obj.data

    # removes sets the parameter is not specified for from search table and condenses search table accordingly
    redunSrc_arr = setdiff(searchCol_arr,namesSym(paraData_df))
    searchSet_df = isempty(redunSrc_arr) ? srcSetIn_df : unique(srcSetIn_df[!,Not(redunSrc_arr)])
    srcCol_arr = namesSym(searchSet_df)

    # searches for matches in original data
    paraMatch_df = innerjoin(searchSet_df, paraData_df; on = srcCol_arr)

    # boolean that switches to true if all values were matched via inheritance
    allMatch_boo = false

    # checks if there are actually unmatched values before startin inheritance process
    if size(searchSet_df,1) != size(paraMatch_df,1)
        noMatch_df = antijoin(searchSet_df, paraData_df; on = srcCol_arr)
        if !isempty(noMatch_df)

            for herit in filter(x -> x[1] in srcCol_arr, collect(par_obj.herit))

                # inherit new values and check for additional matches
                unmatch_arr = unique(noMatch_df[!,herit[1]])
                if herit[2] == :up
                    newData_df = heritParameter_up(herit,unmatch_arr,paraData_df,sets)
                else
                    newData_df = heritParameter_rest(herit,unmatch_arr,paraData_df,sets)
                end
                if isempty(newData_df) continue end

                newMatch_df = innerjoin(noMatch_df, newData_df; on = srcCol_arr)

                # report on inheritance
                cntNewData_int = size(newData_df,1)
                cntMatch_int = size(newMatch_df,1)

                # add new rows to both table with matches and parameter data
                paraData_df = vcat(paraData_df, useNew ? newData_df : antijoin(newData_df,newMatch_df, on = srcCol_arr))
                paraMatch_df = vcat(paraMatch_df,newMatch_df)

                # removes newly matched values from search and leaves loop if everything is matched now
                if cntMatch_int == size(noMatch_df,1)
                    allMatch_boo = true
                    break
                else
                    noMatch_df = antijoin(noMatch_df, newMatch_df; on = srcCol_arr )
                end
            end

            # writes default values for remaining unmatched values
            cntNoMatch_int = size(noMatch_df,1)
            if !allMatch_boo && par_obj.defVal != nothing && useDef
                defaultMatch_df = noMatch_df
                defaultMatch_df[!,:val] = fill(par_obj.defVal,cntNoMatch_int)
                paraMatch_df = isempty(paraMatch_df) ? defaultMatch_df : vcat(paraMatch_df,defaultMatch_df)
            end
        end
    end

    # expands table again by rows
    if !isempty(redunSrc_arr)
        paraMatch_df = innerjoin(paraMatch_df, srcSetIn_df; on = srcCol_arr)
    end

    rename!(paraMatch_df,:val => newCol)

    return paraMatch_df
end

# XXX covers direct inheritance from upper nodes
function heritParameter_up(herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int,1},paraData_df::DataFrame,sets::Dict{Symbol,Tree})

    heritSetShort_sym = Symbol(split(String(herit_par[1]),"_")[1])

    unmatch_set = BitSet(unmatch_arr)
    unmatchChild_dic = Dict(x => intersect(unmatch_set,BitSet(getDescendants(x,sets[heritSetShort_sym],true))) for x in unique(paraData_df[!, herit_par[1]]))

    # adds children where their data is missing to provided parameter data
    paraDataIn_df = filter(r -> !isempty(unmatchChild_dic[getproperty(r,herit_par[1])]),paraData_df)
    if isempty(paraDataIn_df) return paraDataIn_df end
    paraDataIn_df[!,:child] = map(x -> unmatchChild_dic[x],paraDataIn_df[!,herit_par[1]])
    paraDataIn_df = flatten(paraDataIn_df,:child)

    # determines all columns for groupby statement
    grpBy_arr = filter(x -> !(x in [:val,herit_par[1]]),namesSym(paraDataIn_df))

    # uses closest child with value as new data
    newData_df = combine(x -> maximum(x[!,herit_par[1]]) |> (z -> NamedTuple{(herit_par[1],:val)}(tuple(z,x.val[findall(x[!,herit_par[1]] .== z)][1]))), groupby(paraDataIn_df, grpBy_arr))

    select!(newData_df, Not(herit_par[1]))
    rename!(newData_df,:child => herit_par[1])

    return newData_df
end

# XXX covers all inheritance from nodes below unmatched nodes
function heritParameter_rest(herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int,1},paraData_df::DataFrame,sets::Dict{Symbol,Tree})

    # XXX reads out specific inheritance options
    heritSet_sym = herit_par[1]
    heritSetShort_sym = Symbol(split(String(heritSet_sym),"_")[1])

    splHerit_arr = split(String(herit_par[2]),"_")
    heritAgg_sym = Symbol(splHerit_arr[1])
    heritFull_boo = Symbol(splHerit_arr[2]) == :full

    # XXX initialize values for loop (removes and add val again to control its position)

    # dimensions not involved in inheritance propcess
    noHeritSet_tup = tuple(setdiff(namesSym(paraData_df),[:val,herit_par[1]])...)
    colName_tup = tuple(herit_par[1],noHeritSet_tup...)
    newData_df = vcat(colName_tup...,:val) |> (y -> select(DataFrame(Dict(x => x != :val ? Int[] : Float64[] for x in y)),y))

    # gets all children of unmatched ids to filter relevant part of tree
    childrenUnmatch_arr = union(map(x -> BitSet(getDescendants(x,sets[heritSetShort_sym],true)),unmatch_arr)...)

    paraDataFilt_df = filter(r -> getproperty(r,heritSet_sym) in childrenUnmatch_arr,paraData_df)
    if isempty(paraDataFilt_df) return newData_df end

    paraDataSrc_df = paraDataFilt_df

    # loops going upwards within tree trying to obtain new values
    newVal_boo = true

    while newVal_boo
        paraDataSrc_df[!,:pare] =  map(x -> sets[heritSetShort_sym].up[x],paraDataSrc_df[!,heritSet_sym])
        # saves all children of parents currently used in the grouped table within a dictionary to use below within loop over rows
        if heritFull_boo
            childPar_dic = Dict(x => getDescendants(x,sets[heritSetShort_sym], false, x != 0 ? sets[heritSetShort_sym].nodes[x].lvl+1 : 1) for x in unique(paraDataSrc_df[!,:pare]))
        end

        # groups table by parents and not used dimensions according to aggregation rule (sum, any, or unique)
        if heritAgg_sym == :sum
            paraDataGrp_df = combine(groupby(paraDataSrc_df, vcat(noHeritSet_tup...,:pare)), :val => (x -> sum(x)) => :valAgg)
        elseif heritAgg_sym == :avg
            paraDataGrp_df = combine(groupby(paraDataSrc_df, vcat(noHeritSet_tup...,:pare)), :val => (x -> Statistics.mean(x)) => :valAgg)
        else
            paraDataGrp_df = dropmissing(combine(groupby(paraDataSrc_df, vcat(noHeritSet_tup...,:pare)), :val => (x -> length(unique(x)) == 1 ? x[1] : missing) => :valAgg))
        end

        existKey_arr = Tuple.(eachrow(newData_df[!,Not(:val)]))

        if heritFull_boo
            full_dic = groupby(paraDataSrc_df,vcat(noHeritSet_tup...,:pare)) |> (y -> Dict(x.pare[1] => collect(x[!,heritSet_sym]) for x in y))
        end

        # loops through rows of grouped table to see if any row can be coverted into new data
        newVal_boo = false
        for row in eachrow(paraDataGrp_df)
            # either "full" is not used or for all children an initial value was provided
            if !heritFull_boo || isempty(setdiff(childPar_dic[row.pare],full_dic[row.pare]))
                #checkNew_tup = NamedTuple{Tuple(vcat(heritSet_sym,noHeritSet_tup...))}((row.pare,map(x -> getproperty(row,x),noHeritSet_tup)...))
                checkNew_tup = (row.pare,map(x -> getproperty(row,x),noHeritSet_tup)...)
                # writes values if non-existing in table so far
                if !(checkNew_tup in existKey_arr)
                    newEntry_tup = (row.pare,map(x -> getproperty(row,x),noHeritSet_tup)...,row.valAgg)
                    newVal_boo = true
                    push!(newData_df,newEntry_tup)
                end
            end
        end
        # add parent column to new data and filters values to consider for further inheritance (= only children of unmatched values)
        paraDataSrc_df = filter(r -> getproperty(r,heritSet_sym) in childrenUnmatch_arr,newData_df)
    end

    return newData_df
end

# </editor-fold>
