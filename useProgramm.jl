
using CSV, DataFrames, Dates, JuMP, Statistics, LinearAlgebra, Base.Threads
using PyCall, SparseArrays, Gurobi

# pyimport_conda("networkx","networkx")
# pyimport_conda("matplotlib.pyplot","matplotlib")
# pyimport_conda("plotly","plotly")

include("src/objects.jl")
include("src/tools.jl")
include("src/modelCreation.jl")

include("src/optModel/exchange.jl")
include("src/optModel/objective.jl")
include("src/optModel/other.jl")
include("src/optModel/tech.jl")

include("src/dataHandling/mapping.jl")
include("src/dataHandling/parameter.jl")
include("src/dataHandling/readIn.jl")
include("src/dataHandling/tree.jl")
include("src/dataHandling/util.jl")

using AnyMOD

anyM_ref = anyModel("examples/demo","results", objName = "demo")


createOptModel!(anyM_ref )
setObjective!(:costs,anyM_ref )

using Gurobi
set_optimizer(anyM_ref.optModel,Gurobi.Optimizer)
set_optimizer_attribute(anyM_ref.optModel, "Method", 2)
set_optimizer_attribute(anyM_ref.optModel, "Crossover", 0)
set_optimizer_attribute(anyM_ref.optModel, "BarOrder", 0)
optimize!(anyM_ref.optModel)


reportResults(:costs,anyM_ref)
reportResults(:summary,anyM_ref)
