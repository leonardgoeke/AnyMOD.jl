
using AnyMOD, Cbc, Test

@testset "run_rest" begin

    # create model
    anyM = anyModel("testModel","testModel", objName = "test", shortExp = 10, checkRng = 1e8)

    createOptModel!(anyM)
    setObjective!(:costs,anyM)

    # solve model
    set_optimizer(anyM.optModel,Cbc.Optimizer)
    optimize!(anyM.optModel)

    # write data reports
    reportResults(:summary,anyM, rtnOpt = (:raw,:rawDf,:csv,:csvDf))
    reportResults(:exchange,anyM, rtnOpt = (:raw,:rawDf,:csv,:csvDf))
    reportResults(:costs,anyM, rtnOpt = (:raw,:rawDf,:csv,:csvDf))
    reportTimeSeries(:electricity, anyM, rtnOpt = (:raw,:rawDf,:csv,:csvDf))
    reportTimeSeries(:electricity, anyM, rtnOpt = (:raw,:rawDf,:csv,:csvDf), mergeVar = false)

    # create plots
    plotEnergyFlow(:sankey,anyM, rmvNode = ("export",))
    plotTree(:region,anyM)
    plotEnergyFlow(:graph,anyM)
    moveNode!(anyM,("coal",[0.1,0.1]))


    @test length(anyM.report) == 33
    @test round(objective_value(anyM.optModel),digits = 1) == 147521.1

end
