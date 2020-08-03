
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

#using AnyMOD

anyM = anyModel("examples/demo_stoch","results", objName = "stoch")

anyM.subPro = tuple(1,1)
#anyM = anyModel("examples/demo","results", objName = "det")


createOptModel!(anyM)
setObjective!(:costs,anyM)

using Gurobi
set_optimizer(anyM.optModel,Gurobi.Optimizer)
set_optimizer_attribute(anyM.optModel, "Method", 2)
set_optimizer_attribute(anyM.optModel, "Crossover", 1)
set_optimizer_attribute(anyM.optModel, "BarOrder", 0)
optimize!(anyM.optModel)


reportResults(:costs,anyM)
reportResults(:exchange,anyM)
reportTimeSeries(:electricity,anyM)

plotEnergyFlow(:sankey,anyM)



tSym = :wind

tInt = techInt(tSym,anyM.sets[:Te])
part = anyM.parts.tech[tSym]
prepTech_dic = prepVar_dic[tSym]
parDef_dic = copy(parDef_dic)
