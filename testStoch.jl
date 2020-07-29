
# XXX thread stuff

using CSV, DataFrames, Dates, JuMP, Statistics, LinearAlgebra, Base.Threads
using PyCall, SparseArrays

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

# XXX model initialization

inDir = "examples/demo_stoch"
outDir = "results"

anyM = anyModel()

# XXX constructor

# <editor-fold desc="initialize report and options"


# XXX creates dataframe to which reporting is written
anyM.report = Array{Tuple,1}()

anyM.optModel = Model()

anyM.lock = ReentrantLock()

objName = ""; csvDelim = ","; decomm = :recomm; interCapa = :linear; supTsLvl = 0; shortExp = 10; reportLvl = 2; errCheckLvl = 1; errWrtLvl = 1
coefRng = (mat = (1e-2,1e5), rhs = (1e-2,1e2)); bound = (capa = NaN, disp = NaN, obj = NaN)
scaFac = (capa = 1e1, oprCapa = 1e2, dispConv = 1e3, dispSt = 1e4, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0); avaMin = 0.01; checkRng = NaN
emissionLoss = true; redStep = 1.0


# XXX sets whole options object from specified directories TODO arbeite mit kwargs später
outStamp_str = string(objName,"_",Dates.format(now(),"yyyymmddHHMM"))

defOpt_ntup = (inDir = typeof(inDir) == String ? [inDir] : inDir, outDir = outDir, objName = objName, csvDelim = csvDelim, outStamp = outStamp_str, decomm = decomm, interCapa = interCapa,
																			supTsLvl = supTsLvl, shortExp = shortExp,redStep = redStep, emissionLoss = emissionLoss, coefRng = coefRng, scaFac = scaFac, bound = bound,
																				avaMin = avaMin, checkRng = checkRng, reportLvl = reportLvl, errCheckLvl = errCheckLvl, errWrtLvl = errWrtLvl, startTime = now())

anyM.options = modOptions(defOpt_ntup...)

# </editor-fold>

# <editor-fold desc= read in set and parameter data>
files_dic = readInputFolder(anyM.options.inDir)

# XXX read-in sets and parameters
setData_dic = readSets!(files_dic,anyM)
if !any(map(x -> x[1] == 3, anyM.report))
	paraTemp_dic = readParameters!(files_dic,setData_dic,anyM)
end

produceMessage(anyM.options,anyM.report, 1," - Read-in all set and parameter files")

# </editor-fold>

# <editor-fold desc="create part objects and general mappings"
# assign actual tech to parents
relTech_df = setData_dic[:Te][!,Symbol.(filter(x -> occursin("technology",x) && !isnothing(tryparse(Int16,string(x[end]))), string.(namesSym(setData_dic[:Te]))))]
relTech_df = DataFrame(filter(x -> any(collect(x) .!= ""), eachrow(relTech_df)))
techIdx_arr = filter(z -> isempty(anyM.sets[:Te].nodes[z].down), map(x -> lookupTupleTree(tuple(collect(x)...),anyM.sets[:Te],1)[1], eachrow(relTech_df)))

anyM.parts = (tech = Dict(techSym(x,anyM.sets[:Te]) => TechPart(getUniName(x,anyM.sets[:Te])) for x in techIdx_arr), trd = OthPart(), exc = OthPart(), bal = OthPart(), lim = OthPart(), obj = OthPart())

createCarrierMapping!(setData_dic,anyM)
createTimestepMapping!(anyM)

# XXX write general info about technologies
for t in techIdx_arr createTechInfo!(techSym(t,anyM.sets[:Te]), setData_dic, anyM) end
produceMessage(anyM.options,anyM.report, 2," - Created all mappings among sets")

# XXX assign parameters to model parts
parDef_dic = parameterToParts!(paraTemp_dic, techIdx_arr, anyM)
produceMessage(anyM.options,anyM.report, 2," - Assigned parameter data to model parts")

# XXX create object for data visualization
anyM.graInfo = graInfo(anyM)

produceMessage(anyM.options,anyM.report, 1," - Prepared creation of optimzation model")
# </editor-fold>

# XXX creation

# <editor-fold desc="create technology related variables and constraints"

techSym_arr = collect(keys(anyM.parts.tech))
parDef_dic = defineParameter(anyM.options,anyM.report)

# XXX get dimension of expansion and capacity variables and mapping of capacity constraints
tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
prepVar_dic = Dict{Symbol,Dict{Symbol,NamedTuple}}()
prepareTechs!(techSym_arr,prepVar_dic,tsYear_dic,anyM)
if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

# remove technologies without any potential capacity variables
techSym_arr = collect(keys(prepVar_dic))
foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techSym_arr))

# XXX create all technology related elements

# creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

# creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
allLvlR_arr = union(getindex.(getfield.(getfield.(values(anyM.parts.tech),:balLvl),:exp),2),map(x -> x.rDis,values(anyM.cInfo)))

allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl <= x[2] ? getDescendants(x[1], anyM.sets[:R],false,x[2]) : getAncestors(x[1],anyM.sets[:R],:int,x[2])[end]) |> (z -> typeof(z) <: Array ? z : [z]) for x in Iterators.product(allRExp_arr,allLvlR_arr))

produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")

# constraints for technologies are prepared in threaded loop and stored in an array of dictionaries
techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techSym_arr))
tech_itr = collect(enumerate(techSym_arr))

@threads for (idx,tSym) in tech_itr
	techCnsDic_arr[idx] = createTech!(techInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepVar_dic[tSym],copy(parDef_dic),ts_dic,r_dic,anyM)
end

# loops over array of dictionary with constraint container for each technology to create actual jump constraints
for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
	anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
end
produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

# </editor-fold>

# <editor-fold desc="create exchange related variables and constraints"

prepExc_dic = Dict{Symbol,NamedTuple}()
partExc = anyM.parts.exc
partLim = anyM.parts.lim

# obtain dimensions of expansion variables for exchange
potExc_df = prepareExcExpansion!(partExc,partLim,prepExc_dic,tsYear_dic,anyM)

# obtain capacity dimensions solely based on expansion variables
prepareCapacity!(partExc,prepExc_dic,prepExc_dic[:expExc].var,:capaExc,anyM)
addResidualCapaExc!(partExc,prepExc_dic,potExc_df,anyM)

if !all(map(x -> isempty(x),values(prepExc_dic[:capaExc])))
	# create expansion and capacity variables
	createExpCap!(partExc,prepExc_dic,anyM)
	# create capacity constraint
	createCapaExcCns!(partExc,anyM)
	produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints related to expansion and capacity for exchange")
	# create dispatch related variables
	createExcVar!(partExc,ts_dic,anyM)
	produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")
	# create capacity restrictions
	createRestrExc!(ts_dic,partExc,anyM)
	produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")
	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")
end

# </editor-fold>

createTradeVarCns!(anyM.parts.trd,ts_dic,anyM)
createEnergyBal!(techSym_arr,ts_dic,anyM)
createLimitCns!(anyM.parts.lim,anyM)

setObjective!(:costs,anyM)

using Gurobi
set_optimizer(anyM.optModel,Gurobi.Optimizer)
set_optimizer_attribute(anyM.optModel, "Method", 2)
set_optimizer_attribute(anyM.optModel, "Crossover", 0)
set_optimizer_attribute(anyM.optModel, "BarOrder", 0)
set_optimizer_attribute(anyM.optModel, "BarConvTol", 1e-6)

optimize!(anyM.optModel)

reportResults(:summary,anyM)
reportResults(:costs,anyM)

# XXX dump
printObject(anyM.parts.lim.cns[:emissionUp],anyM)



# write scenario information
if :scr in collectKeys(keys(anyM.sets))
	bla_df = flatten(flatten(DataFrame(Ts_sup  = [collect(supTs_tup)], scr = [filter(x -> x != 0, map(x -> x.idx, values(anyM.sets[:scr].nodes)))]),:Ts_sup),:scr)

	matchSetParameter(bla_df ,anyM.parts.obj.par[:scrProp],anyM.sets)


else

end

printObject(anyM.parts.tech[:openspace].cns[:outRestr],anyM)

anyM.parts.tech[:openspace].par[:avaConv].data[24,:]
