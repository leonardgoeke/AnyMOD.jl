
#region # * definition and handling of parameters

# ! defines all existing parameters
function defineParameter(options::modOptions,report::Array{Tuple,1})
    parDef_dic = Dict{Symbol, NamedTuple}()

    #region # * expansion and retrofit parameters

    # ! general discount rate
    parDef_dic[:rateDisc]  = (dim = (:Ts_disSup, :R_exp), problem = :both, defVal = 0.02, herit = (:Ts_disSup => :up, :R_exp => :up, :R_exp => :avg_any, :Ts_disSup => :avg_any), part = :cost)

    # ! technology and exchange expansion
    convExp_tup = (:Te => :up, :Ts_expSup => :up, :R_exp => :up, :Ts_expSup => :avg_any, :R_exp => :avg_any)
    stExp_tup = (:Te => :up, :Ts_expSup => :up, :id => :up, :R_exp => :up,  :Ts_expSup => :avg_any, :R_exp => :avg_any)
    excExp_tup = (:Exc => :up, :Ts_expSup => :up, :R_from => :up, :R_to => :up, :Ts_expSup => :avg_any, :R_from => :avg_any, :R_to => :avg_any)

    parDef_dic[:delConv]   = (dim = (:Ts_expSup, :R_exp, :Te),      problem = :top, defVal = 0.0, herit = convExp_tup, part = :techConv)
    parDef_dic[:delStIn]   = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 0.0, herit = stExp_tup, part = :techSt)
    parDef_dic[:delStOut]  = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 0.0, herit = stExp_tup, part = :techSt)
    parDef_dic[:delStSize] = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 0.0, herit = stExp_tup, part = :techSt)
    parDef_dic[:delExc]    = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = 0.0, herit = excExp_tup, part = :exc)
    parDef_dic[:delExcDir] = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = 0.0, herit = excExp_tup, part = :exc)

    parDef_dic[:lifeConv]   = (dim = (:Ts_expSup, :R_exp, :Te),      problem = :top, defVal = 20, herit = convExp_tup, part = :techConv)
    parDef_dic[:lifeStIn]   = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 20, herit = stExp_tup, part = :techSt)
    parDef_dic[:lifeStOut]  = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 20, herit = stExp_tup, part = :techSt)
    parDef_dic[:lifeStSize] = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = 20, herit = stExp_tup, part = :techSt)
    parDef_dic[:lifeExc]    = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = 50, herit = excExp_tup, part = :exc)
    parDef_dic[:lifeExcDir] = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = 50, herit = excExp_tup, part = :exc)

    parDef_dic[:lifeEcoConv]   = (dim = (:Ts_expSup, :R_exp, :Te),      problem = :top, defVal = nothing, herit = convExp_tup, part = :cost)
    parDef_dic[:lifeEcoStIn]   = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:lifeEcoStOut]  = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:lifeEcoStSize] = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:lifeEcoExc]    = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)
    parDef_dic[:lifeEcoExcDir] = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)

    parDef_dic[:costExpConv]   = (dim = (:Ts_expSup, :R_exp, :Te),      problem = :top, defVal = nothing, herit = convExp_tup, part = :cost)
    parDef_dic[:costExpStIn]   = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:costExpStOut]  = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:costExpStSize] = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:costExpExc]    = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)
    parDef_dic[:costExpExcDir] = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)

    parDef_dic[:rateExpConv]   = (dim = (:Ts_expSup, :R_exp, :Te),      problem = :top, defVal = nothing, herit = convExp_tup, part = :cost)
    parDef_dic[:rateExpStIn]   = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:rateExpStOut]  = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:rateExpStSize] = (dim = (:Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = stExp_tup, part = :cost)
    parDef_dic[:rateExpExc]    = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)
    parDef_dic[:rateExpExcDir] = (dim = (:Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = excExp_tup, part = :cost)

    parDef_dic[:costOprConv]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te),      problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :R_exp => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_exp => :avg_any), part = :cost)
    parDef_dic[:costOprStIn]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :id => :up, :R_exp => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_exp => :avg_any), part = :cost)
    parDef_dic[:costOprStOut]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :id => :up, :R_exp => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_exp => :avg_any), part = :cost)
    parDef_dic[:costOprStSize] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Te => :up, :Ts_expSup => :up, :id => :up, :R_exp => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_exp => :avg_any), part = :cost)
    parDef_dic[:costOprExc]    = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Exc => :up, :Ts_expSup => :up, :R_from => :up, :R_to => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_from => :avg_any, :R_to => :avg_any), part = :cost)
    parDef_dic[:costOprExcDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :up, :Exc => :up, :Ts_expSup => :up, :R_from => :up, :R_to => :up, :Ts_disSup => :avg_any, :Ts_expSup => :avg_any, :R_from => :avg_any, :R_to => :avg_any), part = :cost)

    # ! technology and exchange retrofit
    convRetro_tup = (:Ts_retro => :up, :R_exp => :up, :Ts_expSup_i => :up, :Te_i => :up, :Ts_expSup_j => :up, :Te_j => :up, :Ts_retro => :avg_any, :R_exp => :avg_any, :Ts_expSup_i => :avg_any, :Te_i => :avg_any, :Ts_expSup_j => :avg_any, :Te_j => :avg_any)
    stRetro_tup = (:Ts_retro => :up, :R_exp => :up, :id_i => :up, :Ts_expSup_i => :up, :Te_i => :up, :id_j => :up, :Ts_expSup_j => :up, :Te_j => :up, :Ts_retro => :avg_any, :R_exp => :avg_any, :id_i => :avg_any, :Ts_expSup_i => :avg_any, :Te_i => :avg_any, :id_j => :avg_any, :Ts_expSup_j => :avg_any, :Te_j => :avg_any)
    excRetro_tup = (:Ts_retro => :up, :R_from => :up, :R_to => :up, :Ts_expSup_i => :up, :Exc_i => :up, :Ts_expSup_j => :up, :Exc_j => :up, :Ts_retro => :avg_any, :R_from => :avg_any, :R_to => :avg_any, :Ts_expSup_i => :avg_any, :Exc_i => :avg_any, :Ts_expSup_j => :avg_any, :Exc_j => :avg_any)

    parDef_dic[:lifeRetroConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = 20.0, herit = convRetro_tup, part = :techConv)
    parDef_dic[:lifeRetroStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 20.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:lifeRetroStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 20.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:lifeRetroStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 20.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:lifeRetroExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = 50.0, herit = excRetro_tup,   part = :exc)

    parDef_dic[:creditRetroConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = 1.0, herit = convRetro_tup, part = :techConv)
    parDef_dic[:creditRetroStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 1.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:creditRetroStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 1.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:creditRetroStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 1.0, herit = stRetro_tup,   part = :techSt)
    parDef_dic[:creditRetroExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = 1.0, herit = excRetro_tup,   part = :exc)

    parDef_dic[:lifeRetroEcoConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = nothing, herit = convRetro_tup, part = :cost)
    parDef_dic[:lifeRetroEcoStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup,   part = :cost)
    parDef_dic[:lifeRetroEcoStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup,   part = :cost)
    parDef_dic[:lifeRetroEcoStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup,   part = :cost)
    parDef_dic[:lifeRetroEcoExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = nothing, herit = excRetro_tup,  part = :cost)

    parDef_dic[:costRetroConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = nothing, herit = convRetro_tup, part = :cost)
    parDef_dic[:costRetroStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:costRetroStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:costRetroStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:costRetroExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = nothing, herit = excRetro_tup, part = :cost)

    parDef_dic[:rateRetroConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = nothing, herit = convRetro_tup, part = :cost)
    parDef_dic[:rateRetroStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:rateRetroStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:rateRetroStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = stRetro_tup, part = :cost)
    parDef_dic[:rateRetroExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = nothing, herit = excRetro_tup, part = :cost)

    parDef_dic[:facRetroConv]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j),               problem = :top, defVal = 0.0, herit = convRetro_tup, part = :cost)
    parDef_dic[:facRetroStIn]   = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 0.0, herit = stRetro_tup, part = :cost)
    parDef_dic[:facRetroStOut]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 0.0, herit = stRetro_tup, part = :cost)
    parDef_dic[:facRetroStSize] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = 0.0, herit = stRetro_tup, part = :cost)
    parDef_dic[:facRetroExc]    = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j),     problem = :top, defVal = 0.0, herit = excRetro_tup, part = :cost)

    #endregion

    #region # * limit parameters on capacity

    # ! parameters regarding limits on technology and exchange expansion and capacity

    # expansion limits on conversion, storage and exchange
    parDef_dic[:expConvUp]  = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:expConvLow] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:expConvFix] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:expStInUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :id => :sum_full), part = :lim)
    parDef_dic[:expStInLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)
    parDef_dic[:expStInFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)

    parDef_dic[:expStOutUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :id => :sum_full), part = :lim)
    parDef_dic[:expStOutLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)
    parDef_dic[:expStOutFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)

    parDef_dic[:expStSizeUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full, :id => :sum_full), part = :lim)
    parDef_dic[:expStSizeLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)
    parDef_dic[:expStSizeFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any,  :id => :sum_any),  part = :lim)

    parDef_dic[:expExcUp]  = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :Exc => :sum_full), part = :lim)
    parDef_dic[:expExcLow] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)
    parDef_dic[:expExcFix] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)

    parDef_dic[:expExcUpDir]  = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :Exc => :sum_full), part = :lim)
    parDef_dic[:expExcLowDir] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)
    parDef_dic[:expExcFixDir] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)

    # retrofitting limits on conversion, storage and exchange
    parDef_dic[:retroConvUp]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_full, :R_exp => :sum_full, :Ts_expSup_i => :sum_full, :Te_i => :sum_full, :Ts_expSup_j => :sum_full, :Te_j => :sum_full), part = :lim)
    parDef_dic[:retroConvLow] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any,  :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any), part = :lim)
    parDef_dic[:retroConvFix] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any,  :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any), part = :lim)

    parDef_dic[:retroStInUp]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_full, :R_exp => :sum_full, :id_i => :sum_full, :Ts_expSup_i => :sum_full, :Te_i => :sum_full, :id_j => :sum_full, :Ts_expSup_j => :sum_full, :Te_j => :sum_full), part = :lim)
    parDef_dic[:retroStInLow] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)
    parDef_dic[:retroStInFix] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)

    parDef_dic[:retroStOutUp]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_full, :R_exp => :sum_full, :id_i => :sum_full, :Ts_expSup_i => :sum_full, :Te_i => :sum_full, :id_j => :sum_full, :Ts_expSup_j => :sum_full, :Te_j => :sum_full), part = :lim)
    parDef_dic[:retroStOutLow] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)
    parDef_dic[:retroStOutFix] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)

    parDef_dic[:retroStSizeUp]  = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_full, :R_exp => :sum_full, :id_i => :sum_full, :Ts_expSup_i => :sum_full, :Te_i => :sum_full, :id_j => :sum_full, :Ts_expSup_j => :sum_full, :Te_j => :sum_full), part = :lim)
    parDef_dic[:retroStSizeLow] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)
    parDef_dic[:retroStSizeFix] = (dim = (:Ts_retro, :R_exp, :Ts_expSup_i, :Te_i, :Ts_expSup_j, :Te_j, :id_i, :id_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_exp => :sum_any, :id_i => :sum_any, :Ts_expSup_i => :sum_any, :Te_i => :sum_any, :id_j => :sum_any, :Ts_expSup_j => :sum_any, :Te_j => :sum_any),  part = :lim)

    parDef_dic[:retroExcUp]  = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :Ts_expSup_i => :sum_full, :Exc_i => :sum_full, :Ts_expSup_j => :sum_full, :Exc_j => :sum_full), part = :lim)
    parDef_dic[:retroExcLow] = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Ts_expSup_i => :sum_any, :Exc_i => :sum_any, :Ts_expSup_j => :sum_any, :Exc_j => :sum_any),  part = :lim)
    parDef_dic[:retroExcFix] = (dim = (:Ts_retro, :R_from, :R_to, :Ts_expSup_i, :Exc_i, :Ts_expSup_j, :Exc_j), problem = :top, defVal = nothing, herit = (:Ts_retro => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Ts_expSup_i => :sum_any, :Exc_i => :sum_any, :Ts_expSup_j => :sum_any, :Exc_j => :sum_any),  part = :lim)

    # installed capacity limits and residual capacities on conversion, storage and exchange
    parDef_dic[:capaConvUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaConvLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaConvFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStInUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStInLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStInFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStOutUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStOutLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStOutFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaStSizeUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaStSizeLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:capaStSizeFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:capaExcUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_disSup => :avg_any, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaExcLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any), part = :lim)
    parDef_dic[:capaExcFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any), part = :lim)

    parDef_dic[:capaExcUpDir]  = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_disSup => :avg_any, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:capaExcLowDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any), part = :lim)
    parDef_dic[:capaExcFixDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any), part = :lim)

    # ! residual capacities

    parDef_dic[:capaConvResi]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te),      problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any, :Ts_disSup => :up),                  part = :techConv)
    parDef_dic[:capaStInResi]   = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)
    parDef_dic[:capaStOutResi]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)
    parDef_dic[:capaStSizeResi] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :techSt)

    parDef_dic[:capaExcResi]    = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:R_from => :sum_any,  :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :exc)
    parDef_dic[:capaExcResiDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:R_from => :sum_any,  :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any, :Ts_expSup => :sum_any, :Ts_disSup => :up), part = :exc)

    # ! capacity ratios

    parDef_dic[:stInToConvUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:stInToConvLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:stInToConvFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)

    parDef_dic[:stOutToStInUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:stOutToStInLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:stOutToStInFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)

    parDef_dic[:sizeToStInUp]  = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:sizeToStInLow] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)
    parDef_dic[:sizeToStInFix] = (dim = (:Ts_exp, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:Te => :up, :Ts_exp => :up, :R_exp => :up), part = :techSt)

    # commssioned capacity limits on conversion, storage and exchange
    parDef_dic[:insCapaConvUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te),  problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any,  :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaConvLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaConvFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:insCapaStInUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaStInLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaStInFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:insCapaStOutUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaStOutLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaStOutFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:insCapaStSizeUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any, :id => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaStSizeLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaStSizeFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_exp, :Te, :id), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any, :id => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:insCapaExcUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaExcLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaExcFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    parDef_dic[:insCapaExcUpDir]  = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_expSup => :sum_full), part = :lim)
    parDef_dic[:insCapaExcLowDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)
    parDef_dic[:insCapaExcFixDir] = (dim = (:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_disSup => :avg_any, :R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any,  :Ts_expSup => :sum_any),  part = :lim)

    #endregion

    #region # * limit parameters on dispatch

    upHerit_tup = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :Te => :sum_full, :M => :sum_full, :scr => :up)
    ofHerit_tup = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :Te => :sum_any,  :M => :sum_any,  :scr => :up)
    upHeritSt_tup = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :Te => :sum_full, :M => :sum_full, :scr => :up, :id => :sum_full)
    ofHeritSt_tup = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :Te => :sum_any,  :M => :sum_any,  :scr => :up, :id => :sum_any)

    # ! limits on technology dispatch

    parDef_dic[:useUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:useLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:useFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:genUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:genLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:genFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:stExtOutUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHeritSt_tup, part = :lim)
    parDef_dic[:stExtOutLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stExtOutFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    parDef_dic[:stExtInUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHeritSt_tup, part = :lim)
    parDef_dic[:stExtInLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stExtInFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    parDef_dic[:stIntOutUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHeritSt_tup, part = :lim)
    parDef_dic[:stIntOutLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stIntOutFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    parDef_dic[:stIntInUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:stIntInLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stIntInFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    parDef_dic[:convInUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:convInLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:convInFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:convOutUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = upHerit_tup, part = :lim)
    parDef_dic[:convOutLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)
    parDef_dic[:convOutFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = ofHerit_tup, part = :lim)

    parDef_dic[:stOutUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHeritSt_tup, part = :lim)
    parDef_dic[:stOutLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stOutFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    parDef_dic[:stInUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = upHeritSt_tup, part = :lim)
    parDef_dic[:stInLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)
    parDef_dic[:stInFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = ofHeritSt_tup, part = :lim)

    # ! limits on other dispatch variables

    parDef_dic[:excUp]   =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :C => :sum_full, :Exc => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:excLow]  =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any, :Exc => :sum_any, :scr => :up),  part = :lim)
    parDef_dic[:excFix]  =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any, :Exc => :sum_any, :scr => :up),  part = :lim)

    parDef_dic[:excUpDir]   =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :C => :sum_full, :Exc => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:excLowDir]  =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any, :Exc => :sum_any, :scr => :up),  part = :lim)
    parDef_dic[:excFixDir]  =  (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :Ts_expSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :C => :sum_any, :Exc => :sum_any, :scr => :up),  part = :lim)

    parDef_dic[:crtUp]   =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:crtLow]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:crtFix]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)

    parDef_dic[:lssUp]   =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:lssLow]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:lssFix]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)

    parDef_dic[:trdBuyUp]   =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:trdBuyLow]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :scr => :up),  part = :lim)
    parDef_dic[:trdBuyFix]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any,  :scr => :up),  part = :lim)

    parDef_dic[:trdSellUp]   =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:trdSellLow]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any, :scr => :up),  part = :lim)
    parDef_dic[:trdSellFix]  =  (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_any,  :R_dis => :sum_any,  :C => :sum_any, :scr => :up),  part = :lim)

    # emission limits and factors (are computed as net values of trade and exchange)
    parDef_dic[:emissionUp]    =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :Exc, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :sum_full, :Ts_expSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :Te => :sum_full, :Exc => :sum_full, :M => :sum_full, :id => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:emissionFac]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :Exc, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Exc => :up, :M => :up, :id => :up, :scr => :up), part = :lim)
    parDef_dic[:emissionPrc]   =  (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :Exc, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :C => :up, :Te => :up, :Exc => :up, :M => :up, :id => :up, :scr => :up), part = :cost)

    #endregion

    #region # * limit parameters on costs

    # ! total costs

    parDef_dic[:costUp]  = (dim = (:Ts, :R, :Te, :Exc), problem = :top, defVal = nothing, herit = (:Ts => :sum_full, :R => :sum_full, :Te => :sum_full, :Exc => :sum_full), part = :lim)
    parDef_dic[:costLow] = (dim = (:Ts, :R, :Te, :Exc), problem = :top, defVal = nothing, herit = (:Ts => :sum_any,  :R => :sum_any,  :Te => :sum_any, :Exc => :sum_any),  part = :lim)
    parDef_dic[:costFix] = (dim = (:Ts, :R, :Te, :Exc), problem = :top, defVal = nothing, herit = (:Ts => :sum_any,  :R => :sum_any,  :Te => :sum_any, :Exc => :sum_any),  part = :lim)

    # ! expansion costs
   
    parDef_dic[:costExpConvUp]  = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:costExpConvLow] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:costExpConvFix] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:costExpStInUp]  = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:costExpStInLow] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:costExpStInFix] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:costExpStOutUp]  = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:costExpStOutLow] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:costExpStOutFix] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:costExpStSizeUp]  = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_exp => :sum_full, :Te => :sum_full), part = :lim)
    parDef_dic[:costExpStSizeLow] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)
    parDef_dic[:costExpStSizeFix] = (dim = (:Ts_exp, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_exp => :sum_any,  :Te => :sum_any),  part = :lim)

    parDef_dic[:costExpExcUp]  = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :Exc => :sum_full), part = :lim)
    parDef_dic[:costExpExcLow] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)
    parDef_dic[:costExpExcFix] = (dim = (:Ts_exp, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:Ts_exp => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any),  part = :lim)

    # ! operating costs

    parDef_dic[:costOprConvUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprConvLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costOprConvFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costOprStInUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprStInLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costOprStInFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costOprStOutUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprStOutLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costOprStOutFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costOprStSizeUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprStSizeLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costOprStSizeFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costOprExcUp]  = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprExcLow] = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costOprExcFix] = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), defVal = nothing, problem = :top, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any), part = :lim)

    # ! retrofitting costs

    parDef_dic[:costRetroConvUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroConvLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroConvFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any), part = :lim)

    parDef_dic[:costRetroStInUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroStInLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costRetroStInFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costRetroStOutUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroStOutLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costRetroStOutFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costRetroStSizeUp]  = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_full, :Te => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroStSizeLow] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costRetroStSizeFix] = (dim = (:Ts_disSup, :R_exp, :Te), problem = :top, defVal = nothing, herit = (:R_exp => :sum_any,  :Te => :sum_any,  :Ts_disSup => :avg_any),  part = :lim)

    parDef_dic[:costRetroExcUp]  = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_full, :Ts_disSup => :avg_any), part = :lim)
    parDef_dic[:costRetroExcLow] = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any),  part = :lim)
    parDef_dic[:costRetroExcFix] = (dim = (:Ts_disSup, :R_from, :R_to, :Exc), problem = :top, defVal = nothing, herit = (:R_from => :sum_any, :R_to => :sum_any, :Exc => :sum_any, :Ts_disSup => :avg_any),  part = :lim)

    # ! variable system costs

    parDef_dic[:costVarUseUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarUseLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarUseFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarGenUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarGenLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarGenFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarStInUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarStInLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarStInFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarStOutUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarStOutLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarStOutFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarConvInUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarConvInLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarConvInFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarConvOutUp]  = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :Te => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarConvOutLow] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costVarConvOutFix] = (dim = (:Ts_disSup, :R_dis, :Te, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :Te => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costVarExcUp]   =  (dim = (:Ts_disSup, :R_from, :R_to, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_from => :sum_full, :R_to => :sum_full, :Exc => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costVarExcLow]  =  (dim = (:Ts_disSup, :R_from, :R_to, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any, :scr => :up),  part = :lim)
    parDef_dic[:costVarExcFix]  =  (dim = (:Ts_disSup, :R_from, :R_to, :Exc, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any,  :R_from => :sum_any,  :R_to => :sum_any,  :Exc => :sum_any, :scr => :up),  part = :lim)

    # ! other variable costs

    parDef_dic[:costEmUp]  = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costEmLow] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costEmFix] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costCrtUp]  = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costCrtLow] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costCrtFix] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costLssUp]  = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costLssLow] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costLssFix] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costTrdBuyUp]  = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costTrdBuyLow] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costTrdBuyFix] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)

    parDef_dic[:costTrdSellUp]  = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_full, :R_dis => :sum_full, :C => :sum_full, :scr => :up), part = :lim)
    parDef_dic[:costTrdSellLow] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)
    parDef_dic[:costTrdSellFix] = (dim = (:Ts_disSup, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_disSup => :sum_any, :R_dis => :sum_any, :C => :sum_any, :scr => :up), part = :lim)

    #endregion

    #region # * dispatch parameters"

    # ! technology dispatch properties

    # availability parameters
    parDef_dic[:avaConv]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :Te, :M, :scr), problem = :both, defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techConv, techPre = (preset = :lowest,    mode = (:convIn, :convOut)))
    parDef_dic[:avaStIn]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub,  defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :C => :up, :id => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:avaStOut]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :both, defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :C => :up, :id => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:avaStSize] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub,  defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :C => :up, :id => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))

    # efficiency parameters
    parDef_dic[:effConv]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :Te, :M, :scr), problem = :both, defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :Te => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techConv, techPre = (preset = :reference, mode = (:convIn, :convOut)))
    parDef_dic[:effStIn]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub,  defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :id => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any, :C => :up), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:effStOut] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :both, defVal = 1.0, herit = (:Ts_expSup => :up, :Ts_dis => :up, :id => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any, :C => :up), part = :techSt,   techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))

    # energy content of carrier
    parDef_dic[:enCont]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :Te => :up, :C => :up, :C => :avg_any, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techConv, techPre = (preset = :reference, mode = (:convIn, :convOut)))

    # specific storage parameters
    parDef_dic[:stDis]    = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :C => :up, :R_dis => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any, :scr => :up, :id => :up), part = :techSt, techPre = (preset = :carrierSt, mode = (:stIn,:stOut,:stLvl)))
    parDef_dic[:stInflow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :id, :scr),     problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :C => :up, :Ts_dis => :avg_any, :R_dis => :sum_any, :Te => :up, :scr => :up, :id => :up),                                part = :techSt, techPre = (preset = :carrierSt, mode = tuple()))

    # variable costs
    parDef_dic[:costVarUse]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)
    parDef_dic[:costVarGen]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)
    parDef_dic[:costVarConvIn]    = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)
    parDef_dic[:costVarConvOut]   = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)
    parDef_dic[:costVarStIn]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :id => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)
    parDef_dic[:costVarStOut] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :C => :up, :id => :up, :Te => :up, :Ts_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :cost)

    # energy related ratios (x% of energy from/to technology has to be carrier y)
    parDef_dic[:ratioConvInUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minUse, mode = (:convIn,)))
    parDef_dic[:ratioConvInLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minUse, mode = (:convIn,)))
    parDef_dic[:ratioConvInFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minUse, mode = (:convIn,)))

    parDef_dic[:ratioConvOutUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub,  defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minGen, mode = (:convOut,)))
    parDef_dic[:ratioConvOutLow] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub,  defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minGen, mode = (:convOut,)))
    parDef_dic[:ratioConvOutFix] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :both, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv, techPre = (preset = :minGen, mode = (:convOut,)))
    
    # restrictions on deployment
    parDef_dic[:flhConvUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techConv)
    parDef_dic[:flhConvLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techConv)
    parDef_dic[:flhConvFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techConv)

    parDef_dic[:flhStInUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:flhStInLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:flhStInFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)

    parDef_dic[:flhStOutUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:flhStOutLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:flhStOutFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)

    parDef_dic[:cycStInUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:cycStInLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:cycStInFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)

    parDef_dic[:cycStOutUp]  = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:cycStOutLow] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)
    parDef_dic[:cycStOutFix] = (dim = (:Ts_disSup, :Ts_expSup, :R_dis, :Te, :M, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :scr => :up, :id => :up, :Te => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any), part = :techSt)

    # parameters to enforce a fixed production profile
    parDef_dic[:mustOut] = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :Te, :C, :scr),   problem = :both,  defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :up, :R_dis => :up, :scr => :up, :Te => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :techConv, techPre = (preset = :carrierOut, mode = (:convOut,)))

    # ! further dispatch parameters

    # probability of dispatch scenarios
    parDef_dic[:scrProp] = (dim = (:Ts_sup, :scr), problem = :sub, defVal = nothing, herit = (:scr => :up, :Ts_sup => :up, :Ts_sup => :avg_any), part = :obj)

    # demand related parameters
    parDef_dic[:capaDem] =      (dim = (:Ts_disSup, :R_dis, :C), problem = :both, defVal = nothing, herit = (:R_dis => :sum_any, :Ts_disSup => :avg_any), part = :bal)
    parDef_dic[:costMissCapa] = (dim = (:Ts_disSup, :R_dis, :C), problem = :both, defVal = nothing, herit = (:Ts_disSup => :up, :R_dis => :up, :C => :up, :Ts_disSup => :avg_any, :R_dis => :avg_any, :C => :avg_any), part = :bal)

    parDef_dic[:dem]     = (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = 0.0,     herit = (:Ts_dis => :avg_any, :R_dis  => :sum_any, :scr => :up),             					part = :bal)
    parDef_dic[:costCrt] = (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :bal)
    parDef_dic[:costLss] = (dim = (:Ts_dis, :R_dis, :C, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :Ts_dis => :avg_any, :R_dis => :avg_any), part = :bal)

    # trade (=sell or buy to an external market) parameters
    parDef_dic[:trdBuyPrc]  = (dim = (:Ts_dis, :R_dis, :C, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :bal)
    parDef_dic[:trdSellPrc] = (dim = (:Ts_dis, :R_dis, :C, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :bal)
    parDef_dic[:trdBuyCap]  = (dim = (:Ts_dis, :R_dis, :C, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :bal)
    parDef_dic[:trdSellCap] = (dim = (:Ts_dis, :R_dis, :C, :id, :scr), problem = :sub, defVal = nothing, herit = (:Ts_dis => :up, :R_dis => :up, :scr => :up, :R_dis => :avg_any, :Ts_dis => :avg_any), part = :bal)

    # exchange (=exchange between explicit regions) parameters
    excHerit_tup = (:Ts_expSup => :up, :Ts_dis => :up, :R_from => :up, :R_to => :up, :scr => :up, :R_from => :avg_any, :R_to => :avg_any, :Exc => :up, :Ts_dis => :avg_any, :C => :up)
    
    parDef_dic[:avaExc]     = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = 1.0,     herit = excHerit_tup, part = :exc)
    parDef_dic[:lossExc]    = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = 0.0,     herit = excHerit_tup, part = :exc)
    parDef_dic[:costVarExc] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :cost)

    parDef_dic[:avaExcDir]     = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:lossExcDir]    = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:costVarExcDir] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :cost)

    parDef_dic[:ratioExcUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:ratioExcLow] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:ratioExcFix] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)

    parDef_dic[:ratioExcUpDir]  = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:ratioExcLowDir] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:ratioExcFixDir] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)

    parDef_dic[:flhExcUp]  = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:flhExcLow] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:flhExcFix] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)

    parDef_dic[:flhExcUpDir]  = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:flhExcLowDir] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)
    parDef_dic[:flhExcFixDir] = (dim = (:Ts_dis, :Ts_expSup, :R_from, :R_to, :C, :Exc, :scr), problem = :sub, defVal = nothing, herit = excHerit_tup, part = :exc)

    #endregion

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

# ! assign parameter to model parts
function parameterToParts!(paraTemp_dic::Dict{String,Dict{Symbol,DataFrame}}, sysArr_dic::Dict{Symbol,Array{Int64,1}}, anyM::anyModel)

    # parameter defined within input data
    allPar_arr = unique(vcat(collectKeys.(keys.(values(paraTemp_dic)))...))

     # parameter actually used in the model (difference are the exchange related parameters, that can be provided both directed and symmetric, but within the model only directed values are being used)
    parToFile_dic = Dict(x => collectKeys(keys(paraTemp_dic[x])) for x in keys(paraTemp_dic))

    # gets defintion of parameters and checks, if all input parameters are defined
    parDef_dic = defineParameter(anyM.options,anyM.report)
    undefinedPar_arr = setdiff(unique(vcat(values(parToFile_dic)...)),keys(parDef_dic))
    if !isempty(undefinedPar_arr)
        for undefined in undefinedPar_arr push!(anyM.report,(3,"parameter read-in","definition","parameter with the name '$(string(undefined))' does not exist")) end
        printstyled(anyM.options.objName; color = :underline); print(" ",getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options)
    end

    # maps potential nodes for inheritance from technology tree to "actual" technologies
    sysToPar_dic = Dict(x => Dict{Symbol,Dict{Int32,Array{Int32,1}}}() for x in collect(keys(sysArr_dic)))
    for sys in collect(keys(sysToPar_dic))
        sysToPar_dic[sys][:up] = Dict(x => vcat(x,getAncestors(x,anyM.sets[sys],:int)...) for x in sysArr_dic[sys])
        sysToPar_dic[sys][:down] = Dict(x => vcat(x,getDescendants(x,anyM.sets[sys],true)...) for x in sysArr_dic[sys])
        sysToPar_dic[sys][:both] = Dict(x => union(sysToPar_dic[sys][:up][x],sysToPar_dic[sys][:down][x]) for x in sysArr_dic[sys])
        sysToPar_dic[sys][:none] = Dict(x => [x] for x in sysArr_dic[sys])
    end

    sysPart_dic = Dict{Symbol,Array{Int64,1}}()
    sysPart_dic[:techConv] = filter(r -> !isempty(intersect((:gen,:use),keys(anyM.parts.tech[sysSym(r,anyM.sets[:Te])].carrier))),sysArr_dic[:Te])
    sysPart_dic[:techSt] = filter(r -> !isempty(intersect((:stExtIn,:stExtOut,:stIntIn,:stIntOut),keys(anyM.parts.tech[sysSym(r,anyM.sets[:Te])].carrier))),sysArr_dic[:Te])
    sysPart_dic[:exc] = sysArr_dic[:Exc]

    # ! loop over all actual parameters to assign them to parts of the model
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

        # ! checks for duplicates and removes them in case
        nonUnique_bool = nonunique(allParData_df)
        if any(nonUnique_bool)
            push!(anyM.report,(1,"parameter read-in","validity check","non-unique entries discovered for '$(string(parIt))'"))
            delete!(allParData_df,nonUnique_bool)
        end

        # ! checks for contradicting values
        rmvVal_df = removeVal(allParData_df)
        if !isempty(rmvVal_df)
            contradic_bool = nonunique(allParData_df[:,rmvVal_df])
            if any(contradic_bool)
                 push!(anyM.report,(3,"parameter read-in","validity check","contradicting entries discovered for '$(string(parIt))'"))
            end
        end
        
        # ! assign parameters to parts
        paraDef_ntup = parDef_dic[parIt]
        parPart_sym = paraDef_ntup.part

        if !(parPart_sym in (:techSt, :techConv, :exc))
            # adds parameter to non-system parts
            getfield(anyM.parts,parPart_sym).par[parIt] = ParElement(allParData_df,paraDef_ntup,parIt,anyM.report)
        else
            sym = parPart_sym in (:techSt, :techConv) ? :Te : :Exc

            allParSys_arr = sym in namesSym(allParData_df) ? unique(allParData_df[!,sym]) : [0]
            # determines how technology might inherit from other technology nodes (not at all, by going up, by going down or both)
            heritRules_arr = map(x -> x[2],filter(x -> x[1] == sym, collect(paraDef_ntup.herit)))
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
            # add respecitve parameter data to each relevant system
            for relSys in filter(x -> !isempty(intersect(allParSys_arr,sysToPar_dic[sym][herit_sym][x])), sysPart_dic[parPart_sym])
                # filters all entries of possible inheritance for each technology
                filtParData_df = sym in namesSym(allParData_df) ? filter(row -> row[sym] in sysToPar_dic[sym][herit_sym][relSys], allParData_df) : allParData_df

                # removes potential zero columns from data being actually written to part
                rmvZeroParData_df = filtParData_df[!,filter(x -> unique(filtParData_df[!,x]) != [0] || x == :val,namesSym(filtParData_df))]

                # gets corresponding part
                part_obj = getfield(anyM.parts,sym == :Te ? :tech : :exc)[sysSym(relSys,anyM.sets[sym])]
                # continues in case of must run parameters that will not be relevant, because no such carrier is generated
                out_arr = intersect((:gen, :stExtOut),keys(part_obj.carrier))
                if parIt == :mustOut && (isempty(out_arr) || (:C in namesSym(rmvZeroParData_df) && isempty(intersect(union(map(x -> part_obj.carrier[x],out_arr)...),rmvZeroParData_df[!,:C]))))
                    continue
                end
                # creates new parameter element
                part_obj.par[parIt] = ParElement(rmvZeroParData_df,paraDef_ntup,parIt,anyM.report)
            end
        end
    end

    # ! adds parameter object for parameters where no explicit values where provided, but a default value exists
    for parUndef in keys(filter(r -> r[2].defVal != nothing,parDef_dic))
        parPart_sym = parDef_dic[parUndef].part
        if !(parPart_sym in (:techSt, :techConv, :exc))
            if !haskey(getfield(anyM.parts,parPart_sym).par,parUndef)
                getfield(anyM.parts,parPart_sym).par[parUndef] = ParElement(DataFrame(),parDef_dic[parUndef],parUndef,anyM.report)
            end
        else
            sym = parPart_sym in (:techSt, :techConv) ? :Te : :Exc
            for relSys in filter(x -> !haskey(getfield(anyM.parts,sym == :Te ? :tech : :exc)[sysSym(x,anyM.sets[sym])].par,parUndef),sysPart_dic[parPart_sym])
                getfield(anyM.parts,sym == :Te ? :tech : :exc)[sysSym(relSys,anyM.sets[sym])].par[parUndef] = ParElement(DataFrame(),parDef_dic[parUndef],parUndef,anyM.report)
            end
        end
    end

    # ! plausibility checks of provided input data
    checkPlaus!(anyM)

    return parDef_dic
end

# ! perform pre-setting of dispatch parameters for all technologies
function presetDispatchParameter!(part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},newHerit_dic::Dict{Symbol,Tuple{Pair{Symbol,Symbol},Pair{Symbol,Symbol}}},
                                                                                                ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	relPar_arr = filter(x -> :techPre in keys(parDef_dic[x]) && (!(isempty(part.par[x].data)) || occursin("eff",string(x))), collectKeys(keys(part.par)))
	parPre_dic = Dict(x => parDef_dic[x].techPre.preset for x in relPar_arr)
	preType_arr = union(values(parPre_dic))

    typeVar_dic = Dict(:convOut => [:gen, :stIntIn], :convIn => [:use,:stIntOut], :stIn => [:stExtIn, :stOut], :stOut => [:stExtOut, :stIntOut], :stLvl => [:stLvl])
    modeDep_dic = Dict(x => DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], scr = Int[]) for x in union(values(typeVar_dic)...))

    for preType in preType_arr

		# get all relevant carriers
		specMode_boo = !isempty(part.modes) && !isempty(filter(y -> :M in namesSym(part.par[y].data), keys(filter(x -> x[2] == preType,parPre_dic))))

        # creates table of relevant capacity resolutions and the level of pre-setting
        preCapa_sym = preType != :carrierSt ? :capaConv : :capaStSize
        if preCapa_sym in keys(prepTech_dic)
            capaLvl_df = unique(vcat(map(x -> select(x,intCol(x)),values(prepTech_dic[preCapa_sym]))...)) |> (x -> select(copy(x),intCol(x)))
        else # do not no any presetting, if no capacities relevant for parameter will exist (example: stock technology with conversion part, but not conversion capacities are defined)
            continue
        end

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
                capaLvl_df[!,:C] = map(x -> car_arr, 1:size(capaLvl_df,1))
                capaLvl_df = flatten(capaLvl_df,:C)
            else
                # gets array of carriers defined for each group of storage
                subField_arr = intersect((:stExtIn,:stExtOut,:stIntIn,:stIntOut),keys(part.carrier))
                idC_dic = Dict(y => union(map(x -> getfield(part.carrier,x)[y],subField_arr)...) for y in 1:length(part.carrier[subField_arr[1]]))
                # expands capacities according to carriers
                capaLvl_df[!,:C] .= map(x -> idC_dic[x],capaLvl_df[!,:id])
                capaLvl_df = flatten(capaLvl_df,:C)
                car_arr = union(values(idC_dic)...)
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
		dispReso_df = expandExpToDisp(capaLvl_df,ts_dic,r_dic,anyM.supTs.scr)

		# additionally creates mode specific table in case different modes exists for technology
		if specMode_boo
            dispResoM_df = copy(dispReso_df)
            insertcols!(dispResoM_df,1, :M =>  fill(part.modes,size(dispReso_df,1)))
            dispResoM_df = flatten(dispResoM_df,:M)
		end

        # loops over all parameters of specific pre-setting type
        for parItr in keys(filter(x -> x[2] == preType,parPre_dic))
            parPef_ntup = parDef_dic[parItr]
			newPar_obj = resetParameter(:M in namesSym(part.par[parItr].data) ? dispResoM_df : dispReso_df, part.par[parItr], part.name[end], anyM, length(part.modes), haskey(newHerit_dic,preType) ? newHerit_dic[preType] : tuple())

            # skips parameter if no values are remaining after resetting, in case of mode dependant parameter writes the dimensions that have to be made mode specific
            if isempty(newPar_obj.data)
                delete!(part.par, parItr)
                continue
            elseif :M in namesSym(newPar_obj.data)
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

# ! pre-sets specific dispatch parameter
function resetParameter(newData_df::DataFrame, par_obj::ParElement, tStr::String, anyM::anyModel, cntM_int::Int = 0, newHerit_tup::Tuple = ())
    # gets dimension of search tables and parameter without mode
    newData_df = select(newData_df,intersect(namesSym(newData_df),par_obj.dim))
    # creates empty report, that entries are written to within subprocess
    report = Array{Tuple,1}()

    if !(:M in namesSym(newData_df))
        # in case modes are not being searched for just directly set data
        par_obj.data = matchSetParameter(newData_df,par_obj,anyM.sets) |> (x -> select(x,orderDim(namesSym(x))))
    else
        # looks up original table without applying default values
        matchData1_df = matchSetParameter(newData_df,par_obj,anyM.sets,newCol = :val, useDef = false)

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
                push!(anyM.report,(2, "parameter pre-setting", string(par_obj.name), "parameter data was not specified for all modes in some cases for '$tStr', existing values were ignored"))
            end

            # filters entries where mode values are not distinct, reports on it and uses these entries as non-mode specific data
            disMode_arr = filter(r -> length(unique(r[!,:val])) != 1, modeGrpDef_arr)
            if length(modeGrpDef_arr) > length(disMode_arr)
                push!(anyM.report,(2, "parameter pre-setting", string(par_obj.name), "parameter data was the same for all modes in some cases for '$tStr', no differentiation between modes was applied in these cases"))
                noMode_df = vcat(noMode_df, vcat(filter(r -> length(unique(r[!,:val])) == 1, modeGrpDef_arr)...) )
            end

            # filters data where distinct mode data is provided for all modes and expends resulting table again
            finalMode_df =  isempty(disMode_arr) ? filter(x -> false, matchData1_df) : vcat(disMode_arr...)
        else
            finalMode_df = mode_df
        end

        # gets all data, where no values where obtained successfully yet and look them up again applying the default value and not specifing the mode anymore
        # (hence now non mode-specific parameter values for technologies with modes are taken into account => mode-specific parameter values generally overwrite non-mode specific parameter values)
        newSearch_df = unique(antijoin(newData_df[!,resDim_arr],  vcat(finalMode_df, noMode_df)[!,Not(:val)], on = resDim_arr))

        if !isempty(newSearch_df)
            newSearch_df[!,:M] .= 0
            matchData2_df = matchSetParameter(newSearch_df,par_obj,anyM.sets)
            noMode_df = vcat(matchData2_df,noMode_df)
        end

        # returns tables with and without mode data to parameter object
        par_obj.data = vcat(noMode_df,finalMode_df) |> (x -> select(x,orderDim(namesSym(x))))
    end
    # sets new inherit rules and default value
    par_obj.herit = newHerit_tup

    return par_obj
end

# ! extract specified limit parameter from the limit part of the model
function getLimPar(partLim::OthPart,par_sym::Symbol, sys_tr::Tree; sys::Int = 0)

	if par_sym in keys(partLim.par)
		parLim_obj = copy(partLim.par[par_sym])
        if :Te in namesSym(parLim_obj.data) || :Exc in namesSym(parLim_obj.data) # case for limits differentiated by technology or exchange
            sym = intersect([:Te,:Exc],namesSym(parLim_obj.data))[1]
			parLim_obj.data = filter(x -> x[sym] in [[sys];getAncestors(sys,sys_tr,:int,0)], parLim_obj.data)
			if isempty(parLim_obj.data)
				parLim_obj = ParElement()
			end
		end
	else
		parLim_obj = ParElement()
	end

	return parLim_obj
end

# ! check plausibility of several input parameters
function checkPlaus!(anyM::anyModel)

    # get array of all stock systems for storage and exchange
    stock_dic = Dict(x => map(z -> sysInt(Symbol(z[end]),anyM.sets[x]),getfield.(filter(y -> y.type == :stock,collect(values(getfield(anyM.parts,x == :Te ? :tech : :exc)))),:name)) for x in (:Te,:Exc))
    
    # report on retrofitting
    for retroPar in filter(x -> occursin("costRetro",String(x)),collect(keys(anyM.parts.cost.par)))
        exc_boo = retroPar == :costRetroExc
        parData_df = anyM.parts.cost.par[retroPar].data

        if all([(exc_boo ? Symbol(:Exc_,b) : Symbol(:Te_,b)) in namesSym(parData_df) for b in (:j,:i)]) # reports on retrofitting costs defined for stock technologies
            for s in intersect(parData_df[!,exc_boo ? :Exc_j : :Te_j],stock_dic[exc_boo ? :Exc : :Te])
                push!(anyM.report,(2,"parameter data",String(retroPar),"retrofitting costs were defined with stock $(exc_boo ? :exchange : :technology) $(sysSym(s,anyM.sets[exc_boo ? :Exc : :Te])) as a target, entries are ignored"))
            end

            # reports on retrofitting without a specific target technology
            if any([0 in parData_df[!,exc_boo ? Symbol(:Exc_,b) : Symbol(:Te_,b)] for b in (:j,:i)])  
                push!(anyM.report,(2,"parameter data",String(retroPar),"some retrofitting costs were defined without a specific start or target $(exc_boo ? :exchange : :technology), this implies any capacity could be used for or build by retrofitting")) 
            end
        else
            # reports on retrofitting without a specific target technology
            push!(anyM.report,(2,"parameter data",String(retroPar),"some retrofitting costs were defined without a specific start or target $(exc_boo ? :exchange : :technology), this implies any capacity could be used for or build by retrofitting"))
        end
    end

end

#endregion

#region # * perform match between dimension tables and parameter data

# ! matches set with input parameters, uses inheritance rules for unmatched cases
function matchSetParameter(srcSetIn_df::DataFrame, par_obj::ParElement, sets::Dict{Symbol,Tree}; newCol::Symbol =:val, useDef::Bool = true, useNew::Bool = true, defVal = nothing)

    # check if default value of parameter object is overwritten
    defVal_fl = isnothing(defVal) ? par_obj.defVal : defVal

     # directly return search dataframes with added empty column if it is empty itself
    if isempty(srcSetIn_df)
        paraMatch_df = copy(srcSetIn_df)
        paraMatch_df[!,newCol]  = Float64[]
        return paraMatch_df
    end

    # directly returns default values if no data was provided for the parameter
    if isempty(par_obj.data) || length(namesSym(par_obj.data)) == 1
        paraMatch_df = copy(srcSetIn_df)
        paraMatch_df[!,newCol] = fill(isempty(par_obj.data) ? defVal_fl : par_obj.data[1,:val],size(paraMatch_df,1))
        return paraMatch_df
    end

    searchCol_arr = namesSym(srcSetIn_df)
    paraData_df = copy(par_obj.data)

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
                append!(paraData_df, useNew ? newData_df : antijoin(newData_df,newMatch_df, on = srcCol_arr))
                append!(paraMatch_df,newMatch_df)

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
            if !allMatch_boo && defVal_fl != nothing && useDef
                defaultMatch_df = noMatch_df
                defaultMatch_df[!,:val] = fill(defVal_fl,cntNoMatch_int)
                if isempty(paraMatch_df)
                    paraMatch_df = defaultMatch_df 
                else
                    append!(paraMatch_df,defaultMatch_df)
                end
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

# ! covers direct inheritance from upper nodes
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

# ! covers all inheritance from nodes below unmatched nodes
function heritParameter_rest(herit_par::Pair{Symbol,Symbol},unmatch_arr::Array{Int,1},paraData_df::DataFrame,sets::Dict{Symbol,Tree})

    # ! reads out specific inheritance options
    heritSet_sym = herit_par[1]
    heritSetShort_sym = Symbol(split(String(heritSet_sym),"_")[1])

    splHerit_arr = split(String(herit_par[2]),"_")
    heritAgg_sym = Symbol(splHerit_arr[1])
    heritFull_boo = Symbol(splHerit_arr[2]) == :full

    # ! initialize values for loop (removes and add val again to control its position)

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

# ! matches limiting parameter with data, this is not straightforward, because not only can limits be aggregated to match with a variable, but also variales are aggregated to match with a limit
function matchLimitParameter(allVar_df::DataFrame,par_obj::ParElement,anyM::anyModel)
    
    agg_tup = tuple(intCol(par_obj.data)...)

    # aggregate search variables according to dimensions in limit parameter
    if isempty(agg_tup)
        grpVar_df = allVar_df
    else
        grpVar_df = combine(groupby(allVar_df,collect(agg_tup)), :var => (x -> sum(x)) => :var)
    end

    # try to aggregate variables to limits directly provided via inputs
    limit_df = copy(par_obj.data)
    if size(limit_df,2) != 1
        limit_df[!,:var] = aggDivVar(grpVar_df, limit_df[!,Not(:val)], agg_tup, anyM.sets, aggFilt = agg_tup)
    else
        limit_df[!,:var] .= sum(grpVar_df[!,:var])
    end

    # gets provided limit parameters, that no variables could assigned to so far and tests if via inheritance any could be assigned
    mtcPar_arr, noMtcPar_arr  = findall(map(x -> x != AffExpr(),limit_df[!,:var])) |>  (x -> [x, setdiff(1:size(par_obj.data,1),x)])
    # removes entries with no parameter assigned from limits
    limit_df = limit_df[mtcPar_arr,:]

    if !isempty(noMtcPar_arr)
        # get unmatched limits 
        aggPar_obj = copy(par_obj,par_obj.data[noMtcPar_arr,:])

        # find ancestors of entries in columns that also occur in remaining limits
        aggEtr_dic = Dict(x => Symbol(split(string(x),"_")[1]) |> (w -> unique(grpVar_df[!,x]) |> (z -> Dict(y => w == :id ? Int[] : intersect(z,getAncestors(y,anyM.sets[w],:int,1)) for y in unique(aggPar_obj.data[!,x])))) for x in agg_tup)

        # extend table of variables with entries for relevant ancestorrs
        rplMad_boo = false
        for u in keys(aggEtr_dic)
            if !all(isempty.(getindex.(collect(aggEtr_dic[u]),2)))
                aggPar_obj.data[!,u] = map(x -> aggEtr_dic[u][x] |> (y -> isempty(y) ? [x] : y), aggPar_obj.data[!,u])
                aggPar_obj.data = flatten(aggPar_obj.data,u)
                rplMad_boo = true
            end
        end

        if rplMad_boo 
            aggPar_obj.data = combine(groupby(aggPar_obj.data,collect(agg_tup)), :val => (x -> sum(x)) => :val)
            
            # again performs aggregation for inherited parameter data and merges if original limits
            aggLimit_df = copy(aggPar_obj.data)
            if !isempty(aggLimit_df)
                aggLimit_df[!,:var]  = aggDivVar(grpVar_df, aggLimit_df, agg_tup, anyM.sets, aggFilt = agg_tup)
                limit_df = vcat(limit_df,filter(x -> x.var != AffExpr(),aggLimit_df))
            end
        end
    end

    return limit_df
end

#endregion
