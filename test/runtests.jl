
using AnyMOD, Cbc, Test

@testset "run_demo" begin

    anyM = anyModel("testModel","testModel", objName = "test", shortExp = 10)

    plotTree(:region,anyM)
    plotEnergyFlow(:graph,anyM)

    createOptModel!(anyM)
    setObjective!(:costs,anyM)

    set_optimizer(anyM.optModel,Cbc.Optimizer)
    optimize!(anyM.optModel)

    reportResults(:summary,anyM)
    reportTimeSeries(:electricity, anyM)
    plotEnergyFlow(:sankey,anyM)

    @test length(anyM.report) == 2
    @test round(objective_value(anyM.optModel),digits = 1) == 141395.1
end
