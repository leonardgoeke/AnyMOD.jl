using Base.Threads, CSV, Dates, LinearAlgebra, Requires
using MathOptInterface, Reexport, Statistics, PyCall, SparseArrays
using DataFrames, JuMP

pyimport_conda("networkx","networkx")
pyimport_conda("matplotlib.pyplot","matplotlib")
pyimport_conda("plotly","plotly")

include("src/objects.jl")
include("src/tools.jl")
include("src/modelCreation.jl")

include("src/optModel/exchange.jl")
include("src/optModel/system.jl")
include("src/optModel/objective.jl")
include("src/optModel/other.jl")
include("src/optModel/technology.jl")

include("src/dataHandling/mapping.jl")
include("src/dataHandling/parameter.jl")
include("src/dataHandling/readIn.jl")
include("src/dataHandling/tree.jl")
include("src/dataHandling/util.jl")

#using Gurobi


# ansatz über Ts_disSup_last problematisch, da immer Zuorndung des letzten Betrachtungsjahr => falsche Berechnung von Lebenszeitne über den Rahmen hinaus

# mache alle cost tables mit: Jahr, Region, :Exc/:Te => wie umgang mit :Exc/:Te? => auf 0, für manche kosten dann eben auch beides 0 => filtern in der auswertung
# => durchdenke das weiter, was heißt das für limits und objective?

# mache mit dispatch costs weiter co2 preis auch auf exc losses und storage

# mache performance: julia version, add_expressions
# update reporting
 


anyM = anyModel("examples/demo","examples/results", objName = "test")
createOptModel!(anyM)
setObjective!(:costs,anyM)

tSym = :gasStorage
tInt = sysInt(tSym,anyM.sets[:Te])
part = anyM.parts.tech[tSym]
prepTech_dic = prepSys_dic[:Te][tSym]

eSym = :gas2
eInt = sysInt(eSym,anyM.sets[:Exc])
part = anyM.parts.exc[eSym]
prepExc_dic = prepSys_dic[:Exc][eSym]



anyM = anyModel()
objName = "bla"
csvDelim = ","
decomm = :decomm
interCapa = :linear
supTsLvl = 0
shortExp = 10
redStep = 1.0
emissionLoss = true
reportLvl = 2
errCheckLvl = 1
errWrtLvl = 1
coefRng = (mat = (1e-2,1e5), rhs = (1e-2,1e2))
scaFac = (capa = 1e1, insCapa = 1e2, dispConv = 1e3, dispSt = 1e4, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)
bound = (capa = NaN, disp = NaN, obj = NaN)
avaMin = 0.01
checkRng = NaN
inDir = "examples/demo"
outDir = "results"




set_optimizer(anyM.optModel,Gurobi.Optimizer)
optimize!(anyM.optModel)


printObject(anyM.parts.exc.cns[:excCapa], anyM, fileName = "bla2")


using AnyMOD
  