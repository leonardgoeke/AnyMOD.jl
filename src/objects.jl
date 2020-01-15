
# <editor-fold desc="definition and handling of parameters"

# XXX defines struct for handling parameter data
mutable struct ParElement
	name::Symbol
    dim::Tuple
    defVal::Union{Nothing,Float64}
    herit::Tuple
    data::DataFrame
	techPre::NamedTuple{(:preset,:mode),Tuple{Symbol,Tuple{Vararg{Symbol,N} where N}}}

	function ParElement(paraData_df::DataFrame,paraDef_ntup::NamedTuple,name::Symbol,report::Array{Tuple,1})

        setLongShort_dic = Dict(:Ts => :timestep, :R => :region, :C => :carrier, :Te => :technology, :M => :mode)
		if isempty(paraData_df) return new(name,paraDef_ntup.dim,paraDef_ntup.defVal,paraDef_ntup.herit,DataFrame()) end

        # XXX check consistency of rows in input dataframe and definition of set and rename columns according to set defintion
        # assigns array of used suffixes according to parameter defintion to each set
        splitDim_arr = map(x -> map(y -> Symbol(y), split(String(x),"_")),collect(paraDef_ntup.dim))
        setSuf_dic = Dict(x => map(y -> length(y) == 1 ? Symbol() : y[end],filter(z -> z[1] == x,splitDim_arr)) for x in unique(map(x -> x[1],splitDim_arr)))

        # loops over set columns in input dataframe and assigns them to the sets defined for the parameter
        newCol_dic = Dict(:val => :val)
        sufNum_dic = Dict(:b => 2, :c => 3, :d => 4, :e => 5, :f => 6, :g => 7)

        for colNam in setdiff(names(paraData_df),[:val])
            colNam_arr = split(String(colNam),"_")
            setNam = Symbol(colNam_arr[1])

            if !haskey(setSuf_dic,setNam) # parameter provided for a set not appearing in definition (e.g. demand depending on the technology)
                push!(report,(2, :par, name, "parameter data was specified for $(setLongShort_dic[setNam]) set, but it is not defined to depend on this set"))
                continue
            elseif length(setSuf_dic[setNam]) == 1 && length(colNam_arr) > 1 # they are several instances of the set provided, but it only depends on one instance (e.g. two region sets for efficiency)
                push!(report,(2, :par, name, "parameter data was specified for several instances of $(setLongShort_dic[setNam]) set, but it is defined to depend only on one instance, additonal instances were ignored"))
                continue
            elseif setSuf_dic[setNam][1] == Symbol() # set has only one instance and no suffix => no change when converting from read-in dataframe to parameter element
                newCol_dic[colNam] = colNam
            elseif length(setSuf_dic[setNam]) == 1 || length(colNam_arr) == 1 # column name in dataframe has no underscore, but defintion of parameter element has one
                newCol_dic[colNam] = Symbol(setNam, "_", setSuf_dic[setNam][1])
            else
                cntRep_int = sufNum_dic[Symbol(colNam_arr[2])] # set defined for several instances
                newCol_dic[colNam] = Symbol(setNam, "_", setSuf_dic[setNam][cntRep_int])
            end
        end

        # filters only used coulumns, renames them accordingly and converts to table
        writeData_df = paraData_df[:,collect(keys(newCol_dic))]
        DataFrames.rename!(writeData_df,newCol_dic)

        new_obj = new(name,paraDef_ntup.dim,paraDef_ntup.defVal,paraDef_ntup.herit,writeData_df)
        # defines on which level parameter is presetted and which capacity restrictions are affected by different modes for all dispatch parameters, where this is specified
        if haskey(paraDef_ntup,:techPre) new_obj.techPre = paraDef_ntup.techPre end
        return new_obj
    end

	ParElement() = new()
end

# XXX specific struct for read in process of parameter data
mutable struct parEntry
	colSet::Symbol
	entry::Array{Union{Bool,String},1}
	lvl::Array{Int,1}
	startLvl::Int
end

# XXX functions to copy parameter structs of parameter data
import Base.copy
function copy(par_obj::ParElement)
	out = ParElement()
	out.name = par_obj.name
	out.dim = par_obj.dim
	out.defVal = par_obj.defVal
	out.herit = par_obj.herit
	out.data = copy(par_obj.data)
	if isdefined(par_obj,:techPre) out.techPre = par_obj.techPre end
	return out
end

function copy(par_obj::ParElement,data_df::DataFrame)
	out = ParElement()
	out.name = par_obj.name
	out.dim = par_obj.dim
	out.defVal = par_obj.defVal
	out.herit = par_obj.herit
	out.data = data_df
	if isdefined(par_obj,:techPre) out.techPre = par_obj.techPre end
	return out
end

# </editor-fold>

# <editor-fold desc="struct for individual parts of the model"

# XXX defines parts of the model
abstract type AbstractModelPart end

mutable struct TechPart <: AbstractModelPart
	name::Tuple{Vararg{String,N} where N}
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	carrier::NamedTuple
	balLvl::NamedTuple{(:exp,:ref),Tuple{Tuple{Int,Int},Union{Nothing,Tuple{Int,Int}}}}
	capaRestr::DataFrame
	type::Symbol
	disAgg::Bool
	modes::Tuple{Vararg{Int,N} where N}
	TechPart(name::Tuple{Vararg{String,N} where N}) = new(name,Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
end

mutable struct OthPart <: AbstractModelPart
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	OthPart() = new(Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
end

# </editor-fold>

# <editor-fold desc="structs for nodes that then make up the trees to save set data"

# XXX define nodes for set tree and tree itself
mutable struct Node
	idx::Int
	val::String
	lvl::Int
	subIdx::Int
	down::Array{Int,1}
end

mutable struct Tree
	nodes::Dict{Int,Node}
	srcTup::Dict{Tuple,Int}
	srcStr::Dict{String,Array{Int,1}}
	up::Dict{Int,Int}
	height::Int
	Tree() = new(Dict{Int,Node}(),Dict{Tuple,Int}(),Dict{String,Array{Int,1}}(),Dict{Int,Int}(),1)
end

# </editor-fold>

# <editor-fold desc="options for model and model itself"

# XXX defines final model object and its options
struct modOptions

	# data in- and output
	inDir::Array{String,1}
	outDir::String
	name::String
	csvDelim::String
	outStamp::String
	# model generation
	decomm::Symbol
	interCapa::Symbol
	shortExp::Int64
	# managing numerical issues
	scale::NamedTuple{(:cost,:ener,:capa,:ems),Tuple{Float64,Float64,Float64,Float64}}
	bound::NamedTuple{(:exp,:disp, :cost),Tuple{Union{Nothing,Float64},Union{Nothing,Float64},Union{Nothing,Float64}}}
	# reporting related options
	reportLvl::Int
	errCheckLvl::Int
	errWrtLvl::Int
	startTime::DateTime
end

# XXX finally, the model object itself
mutable struct anyModel

	options::modOptions
	report::Array{Tuple,1}
	optModel::Model
	lock::SpinLock

	supTs::NamedTuple{(:lvl,:step,:sca),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64}}}
	cInfo::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:eq),Tuple{Int,Int,Int,Int,Bool}}}

	sets::Dict{Symbol,Tree}
	parts::NamedTuple{(:tech,:trd,:exc,:bal,:lim,:obj),Tuple{Dict{Int,TechPart},OthPart,OthPart,OthPart,OthPart,OthPart}}

	function anyModel(inDir::Union{String,Array{String,1}},outDir::String; name = "", csvDelim = ",", decomm = :none, interCapa = :linear, shortExp = 10, reportLvl = 2, errCheckLvl = 1, errWrtLvl = 1,
																																						bound = (exp = nothing, disp = nothing, cost = nothing))
		anyM = new()

		# <editor-fold desc="initialize report and options"

		# XXX creates dataframe to which reporting is written
		anyM.report = Array{Tuple,1}()

		anyM.optModel = Model()
		anyM.lock = SpinLock()

		# XXX sets whole options object from specified directories TODO arbeite mit kwargs später
		defOpt_ntup = (inDir = typeof(inDir) == String ? [inDir] : inDir, outDir = outDir, name = name, csvDelim = csvDelim, outStamp = Dates.format(now(),"yyyymmddHHMM"), decomm = decomm, interCapa = interCapa, shortExp = shortExp,
																																			scale = (cost = 1.0, ener = 1.0,capa = 1.0, ems = 1.0), bound = bound,
																																				reportLvl = reportLvl, errCheckLvl = errCheckLvl, errWrtLvl = errWrtLvl, startTime = now())

		anyM.options = modOptions(defOpt_ntup...)

		# </editor-fold>

		# <editor-fold desc= read in set and parameter data>
		files_dic = readInputFolder(anyM.options.inDir)

		# XXX read-in sets and parameters
		setData_dic = readSets!(files_dic,anyM)
		paraTemp_dic = readParameters!(files_dic,setData_dic,anyM)

		produceMessage(anyM.options,anyM.report, 1," - Read-in all set and parameter files")

		# </editor-fold>

		# <editor-fold desc="create part objects and general mappings"
		# assign actual tech to parents
		relTech_df = setData_dic[:Te][!,Symbol.(filter(x -> occursin("technology",x) && !isnothing(tryparse(Int16,string(x[end]))), string.(names(setData_dic[:Te]))))]
		techIdx_arr = map(x -> lookupTupleTree(x,anyM.sets[:Te]),map(y -> tuple(filter(x -> x != "",collect(y))...),eachrow(relTech_df)))

		anyM.parts = (tech = Dict(x => TechPart(getUniName(x,anyM.sets[:Te])) for x in techIdx_arr), trd = OthPart(), exc = OthPart(), bal = OthPart(), lim = OthPart(), obj = OthPart())

		createCarrierMapping!(setData_dic,anyM)
		createTimestepMapping!(anyM)


		# XXX write general info about technologies
		relTech_df = setData_dic[:Te][!,Symbol.(filter(x -> occursin("technology",x) && !isnothing(tryparse(Int16,string(x[end]))), string.(names(setData_dic[:Te]))))]
		techIdx_arr = map(x -> lookupTupleTree(x,anyM.sets[:Te]),map(y -> tuple(filter(x -> x != "",collect(y))...),eachrow(relTech_df)))

		for t in techIdx_arr createTechInfo!(t, setData_dic, anyM) end
		produceMessage(anyM.options,anyM.report, 2," - Created all mappings among sets")

		# XXX assign parameters to model parts
		parDef_dic = parameterToParts!(paraTemp_dic, techIdx_arr, anyM)
		produceMessage(anyM.options,anyM.report, 2," - Assigned parameter data to model parts")

		produceMessage(anyM.options,anyM.report, 1," - Prepared creation of optimzation model")
		# </editor-fold>

		return anyM
	end
end

# </editor-fold>

# XXX create optimization model after anyModel has been initialized
function createOptModel!(anyM::anyModel)

	# <editor-fold desc="create technology related variables and constraints"

	techIdx_arr = collect(keys(anyM.parts.tech))
	parDef_dic = defineParameter(anyM.options,anyM.report)

	# XXX get dimension of expansion and capacity variables and mapping of capacity constraints
	tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
	prepVar_dic = Dict{Int,Dict{Symbol,NamedTuple}}()
	prepareTechs!(techIdx_arr,prepVar_dic,tsYear_dic,anyM)

	# remove technologies without any potential capacity variables
	techIdx_arr = collect(keys(prepVar_dic))
	foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techIdx_arr))

	# XXX create all technology related elements
	# creates dictionary that assigns combination of supordinate dispatch timestep and dispatch level to dispatch timesteps
	allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
	ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

	# creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
	allLvlR_arr = unique(union([getfield.(values(anyM.cInfo),x) for x in (:rDis, :rExp)]...))
	allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
	r_dic = Dict((x[1], x[2]) => anyM.sets[:R].nodes[x[1]].lvl == x[2] <= x[2] ? x[1] : getindex.(getAncestors(x[1],anyM.sets[:R],x[2]),1)[1]
																						for x in filter(x -> anyM.sets[:R].nodes[x[1]].lvl >= x[2], collect(Iterators.product(allRExp_arr,allLvlR_arr))))

	produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")
	# loop over each tech to create variables and constraints
	iterateOverTech!(techIdx_arr,prepVar_dic,ts_dic,r_dic,parDef_dic,anyM)
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

	# create expansion and capacity variables
	createExpCap!(partExc,prepExc_dic,anyM)
	# create capacity constraint
	createCapaExcCns!(partExc,anyM)

	produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints related to expansion and capacity for exchange")

	# create dispatch related variables
	createExcVar!(partExc,ts_dic,anyM)
	produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")

	# create capacity restrictions
	createCapaRestrExc!(partExc,anyM)
	produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")

	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")

	# </editor-fold>

	createTradeVarCns!(anyM.parts.trd,anyM)
	createEnergyBal!(techIdx_arr,anyM)
	createLimitCns!(techIdx_arr,anyM.parts.lim,anyM)
end
