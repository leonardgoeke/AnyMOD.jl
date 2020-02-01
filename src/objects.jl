
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
	entry::Array{String,1}
	lvl::Array{Int,1}
	startLvl::Int
end

# </editor-fold>

# <editor-fold desc="extensions of base functions"

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

# XXX extends base function to avoid certain errors, that otherwise would require more costly solutions
import Base.+
+(a::Int,b::Nothing) = a

# </editor-fold>

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

# XXX container to store data defining a constraint (used to seperate definition and actual jump creation of constraints)
struct cnsCont
    data::DataFrame
    sign::Symbol
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
	srcStr::Dict{Tuple{String,Int},Array{Int,1}}
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
	objName::String
	csvDelim::String
	outStamp::String
	# model generation
	decomm::Symbol
	interCapa::Symbol
	supTsLvl::Int
	shortExp::Int
	# managing numerical issues
	coefRng::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Float64,Float64}}}
	scaFac::NamedTuple{(:capa,:disp, :cost, :totCost),Tuple{Float64,Float64,Float64,Float64}}
	bound::NamedTuple{(:capa,:disp, :cost),Tuple{Float64,Float64,Float64}}
	avaMin::Float64
	checkRng::Float64
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

	function anyModel(inDir::Union{String,Array{String,1}},outDir::String; objName = "", csvDelim = ",", decomm = :none, interCapa = :linear, supTsLvl = 0, shortExp = 10, reportLvl = 2, errCheckLvl = 1, errWrtLvl = 1,
																					coefRng = (mat = (1e-2,1e5), rhs = (1e-2,1e2)), scaFac = (capa = 1e2, disp = 1e2, cost = 1e2, totCost = 1e1), bound = (capa = NaN, disp = NaN, cost = NaN), avaMin = 0.01, checkRng = NaN)
		anyM = new()

		# <editor-fold desc="initialize report and options"

		# XXX creates dataframe to which reporting is written
		anyM.report = Array{Tuple,1}()

		anyM.optModel = Model()

		anyM.lock = SpinLock()

		# XXX sets whole options object from specified directories TODO arbeite mit kwargs später
		outStamp_str = string(objName,"_",Dates.format(now(),"yyyymmddHHMM"))
		defOpt_ntup = (inDir = typeof(inDir) == String ? [inDir] : inDir, outDir = outDir, objName = objName, csvDelim = csvDelim, outStamp = outStamp_str, decomm = decomm, interCapa = interCapa, supTsLvl = supTsLvl, shortExp = shortExp,
																					coefRng = coefRng, scaFac = scaFac, bound = bound, avaMin = avaMin, checkRng = checkRng, reportLvl = reportLvl, errCheckLvl = errCheckLvl, errWrtLvl = errWrtLvl, startTime = now())

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
	 	techIdx_arr = filter(z -> isempty(anyM.sets[:Te].nodes[z].down), map(x -> lookupTupleTree(tuple(collect(x)...),anyM.sets[:Te],1), eachrow(relTech_df)))

		anyM.parts = (tech = Dict(x => TechPart(getUniName(x,anyM.sets[:Te])) for x in techIdx_arr), trd = OthPart(), exc = OthPart(), bal = OthPart(), lim = OthPart(), obj = OthPart())

		createCarrierMapping!(setData_dic,anyM)
		createTimestepMapping!(anyM)

		# XXX write general info about technologies
		for t in techIdx_arr createTechInfo!(t, setData_dic, anyM) end
		produceMessage(anyM.options,anyM.report, 2," - Created all mappings among sets")

		# XXX assign parameters to model parts
		parDef_dic = parameterToParts!(paraTemp_dic, techIdx_arr, anyM)
		produceMessage(anyM.options,anyM.report, 2," - Assigned parameter data to model parts")

		produceMessage(anyM.options,anyM.report, 1," - Prepared creation of optimzation model")
		# </editor-fold>

		return anyM
	end

	anyModel() = new()
end

# </editor-fold>
