__precompile__()
module anyMOD

    # XXX import packages
    using Distributed

    # overwrite the distributed printing message to remove "from worker" part, based on this post by TsurHerman: https://discourse.julialang.org/t/any-way-to-remove-from-worker/23604
    Distributed.redirect_worker_output(ident, stream) = begin
       @async while !eof(stream)
           line = readline(stream)
           if startswith(line, "      From worker ")
               # stdout's of "additional" workers started from an initial worker on a host are not available
               # on the master directly - they are routed via the initial worker's stdout.
               parts = split(line,":")
               printstyled(join(parts[2:end]...), "\n"; color = :light_black)
           else
               printstyled(line, "\n"; color = :light_black)
           end
       end
    end

    # be default adds 6 processes
    addprocs(6)
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

    # XXX runs code an example problem at first use to improve precompilation
   @suppress begin
      anyM = anyModel("examples/precompile","output"; startTime = DateTime(0.0), reportLvl = 0, errCheckLvl = 0, errWrtLvl = 0)
      addVariables!(anyM)
      addConstraints!(anyM)
      setObjective!(:costs,anyM)
   end
end
