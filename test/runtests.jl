
using AnyMOD, Cbc, CSV, Test

@testset "run_demo" begin
    # manipulate set_carrier.csv to test on smaller resolution, because otherwise Cbs times out during test
    carrier_df = CSV.read("examples/demo/set_carrier.csv")
    preserveReso_arr = carrier_df[!,:timestep_dispatch]
    carrier_df[!,:timestep_dispatch] = min.(carrier_df[!,:timestep_dispatch],2)
    CSV.write("examples/demo/set_carrier.csv",carrier_df)

    anyM = anyModel("examples/demo","examples", objName = "demo", shortExp = 10)

    plotTree(:region,anyM)
    plotEnergyFlow(:graph,anyM)

    createOptModel!(anyM)
    setObjective!(:costs,anyM)

    set_optimizer(anyM.optModel,Cbc.Optimizer)
    optimize!(anyM.optModel)

    reportResults(:summary,anyM)
    reportTimeSeries(:electricity, anyM)
    plotEnergyFlow(:sankey,anyM)

    # recreates old set_carrier file
    carrier_df = CSV.read("examples/demo/set_carrier.csv")
    carrier_df[!,:timestep_dispatch] = preserveReso_arr
    CSV.write("examples/demo/set_carrier.csv",carrier_df)

    @test length(anyM.report) == 2
    @test round(objective_value(anyM.optModel),digits = 1) == 127160.9
end
