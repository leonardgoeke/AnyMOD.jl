__precompile__()
module anyMOD

    # XXX import packages
    using Reexport, IndexedTables, DataFrames, TableReader, MathOptInterface, Statistics, LinearAlgebra, Dates, CSV, Suppressor
    @reexport using JuliaDB, JuMP

    const MOI = MathOptInterface
    const DB = JuliaDB
    const IT = IndexedTables

    # XXX adds code
    include("define_structs.jl")
    include("data_processing.jl")

    include("read_inputs.jl")
    include("create_mapping.jl")

    include("elements/parameter.jl")
    include("elements/variable.jl")
    include("elements/constraint.jl")
    include("elements/objective.jl")

    export anyModel, addVariables!, addConstraints!, setObjective!, printObject
    export prepareLimitParameter!, prepareDispatchParameter!, createVariable!
    export createConstraint!, createLimitConstraints!, controllCapaConstraints!

    # XXX runs code an example problem at first use to improve precompilation, currently deactiviated
    #=
    @suppress begin
        anyM = anyModel("examples/demo","examples"; startTime = DateTime(0.0), reportLvl = 0, errCheckLvl = 0, errWrtLvl = 0)
        addVariables!(anyM)
        addConstraints!(anyM)
        setObjective!(:costs,anyM)
   end
   =+
end
