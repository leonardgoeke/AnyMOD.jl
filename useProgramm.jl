
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

anyM = anyModel("examples/demo","results", objName = "demo")

anyM.parts.tech[2].var[:gen]

# TODO prüfe model creation für ein jahr nur durch manipulation des objekts nach konstruktor

part = anyM.parts.tech[t]
prepTech_dic = prepVar_dic[t]
parDef_dic = copy(parDef_dic)



createOptModel!(anyM)
setObjective!(:costs,anyM)

using Gurobi
set_optimizer(anyM.optModel,Gurobi.Optimizer)
set_optimizer_attribute(anyM.optModel, "Method", 2)
set_optimizer_attribute(anyM.optModel, "Crossover", 0)
set_optimizer_attribute(anyM.optModel, "BarOrder", 0)
optimize!(anyM.optModel)
