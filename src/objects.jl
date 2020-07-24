
# <editor-fold desc="definition and handling of parameters"

# XXX defines struct for handling parameter data
"""
Type including data and additional information on parameters. Fields relate to what is provided in [Parameter list](@ref) and include:
* `name::Symbol`: name of the parameter
* `dim::Tuple`: potential dimensions of parameter data
* `defVal::Union{Nothing,Float64}`: default value
* `herit::Tuple`: inheritance rules for parameter, see [Parameter overview](@ref) for details
* `data::DataFrame`: specified parameter data
"""
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

        for colNam in setdiff(namesSym(paraData_df),[:val])
            colNam_arr = split(String(colNam),"_")
            setNam = Symbol(colNam_arr[1])

            if !haskey(setSuf_dic,setNam) # parameter provided for a set not appearing in definition (e.g. demand depending on the technology)
                push!(report,(2, "parameter assignment", string(name), "parameter data was specified for $(setLongShort_dic[setNam]) set, but it is not defined to depend on this set"))
                continue
            elseif length(setSuf_dic[setNam]) == 1 && length(colNam_arr) > 1 # they are several instances of the set provided, but it only depends on one instance (e.g. two region sets for efficiency)
                push!(report,(2, "parameter assignment", string(name), "parameter data was specified for several instances of $(setLongShort_dic[setNam]) set, but it is defined to depend only on one instance, additonal instances were ignored"))
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

# <editor-fold desc="import and extensions of base functions"

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

# XXX usual collect sometimes creates a mysterious error if used on dictionary keys, this command avoids this
import Base._collect
import Base.SizeUnknown

collectKeys(itr) = _collect(Symbol, itr, SizeUnknown())

# </editor-fold>

# <editor-fold desc="struct for individual parts of the model"

# XXX defines parts of the model
abstract type AbstractModelPart end

"""
```julia
	TechPart <: AbstractModelPart
```
Type used for technology parts. Parameters, variables, and constraints are assigned as dictionaries via the fields `par`, `var`, and  `cns`, respectively. Additional fields include:
* `name::Tuple`: name of technology as a series of nodes from the technology tree
* `carrier::NamedTuple`: energy carriers by index assigned to technology by groups (e.g. generation, use, ...)
* `balLvl::NamedTuple`: temporal and spatial resolution for expansion and balance of the technology
* `capaRestr::DataFrame`: specification of capacity restrictions required for technology
* `actSt::Tuple`: actively stored carriers  altough they are not leafs by index
* `type::Tuple`: type of technology (stock, mature, or evolving)
* `disAgg::Bool`: if true, dispatch is modelled at expansion resolution instead of dispatch resolution
* `modes::Tuple`: different operational modes of technology
"""
mutable struct TechPart <: AbstractModelPart
	name::Tuple{Vararg{String,N} where N}
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	carrier::NamedTuple
	balLvl::NamedTuple{(:exp,:ref),Tuple{Tuple{Int,Int},Union{Nothing,Tuple{Int,Int}}}}
	capaRestr::DataFrame
	actSt::Tuple
	type::Symbol
	disAgg::Bool
	modes::Tuple{Vararg{Int,N} where N}
	TechPart(name::Tuple{Vararg{String,N} where N}) = new(name,Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
	TechPart() = new()
end

"""
```julia
	OthPart <: AbstractModelPart
```
Type used for 'exchange', 'trade', 'balance', 'limits', and 'objectives' model parts. Parameters, variables, and constraints are assigned as dictionaries via the fields `par`, `var`, and  `cns`, respectively.
"""
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
	srcTup::Dict{Tuple,Array{Int,1}}
	srcStr::Dict{Tuple{String,Int},Array{Int,1}}
	up::Dict{Int,Int}
	height::Int
	Tree() = new(Dict{Int,Node}(),Dict{Tuple,Int}(),Dict{String,Array{Int,1}}(),Dict{Int,Int}(),1)
end

# </editor-fold>

# <editor-fold desc="options for model and model itself"

# create abstract model object to reference before creation (avoid circular type definiton)
abstract type AbstractModel end

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
	redStep::Float64
	# managing numerical issues
	emissionLoss::Bool
	coefRng::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Vararg{Float64,2}}}}
	scaFac::NamedTuple{(:capa,:oprCapa,:dispConv,:dispSt,:dispExc, :dispTrd, :costDisp,:costCapa,:obj),Tuple{Vararg{Float64,9}}}
	bound::NamedTuple{(:capa,:disp,:obj),Tuple{Vararg{Float64,3}}}
	avaMin::Float64
	checkRng::Float64
	# reporting related options
	reportLvl::Int
	errCheckLvl::Int
	errWrtLvl::Int
	startTime::DateTime
end

# XXX flow graph object that defines relations between technologies and carriers (and among carriers)
mutable struct flowGraph
	nodeC::Dict{Int64,Int64}
	nodeTe::Dict{Int64,Int64}
	edgeC::Array{Pair{Int,Int},1}
	edgeTe::Array{Pair{Int,Int},1}
	nodePos::Dict{Int,Array{Float64,1}}

	function flowGraph(anyM::AbstractModel)

		# creates dictionary mapping carrier id to node id
		nodeC_dic = Dict(x[2] => x[1] for x in enumerate(sort(filter(x -> x != 0,getfield.(collect(values(anyM.sets[:C].nodes)),:idx)))))

		# get all relevant technology, a technology is not relevant, where all children of a parent have the same carriers (in this case only the parent is relevant)
		t_tree = anyM.sets[:Te]

		allTech_arr =  getfield.(collect(values(t_tree.nodes)),:idx)
		tleaf_dic = Dict(x => unique(filter(y -> techSym(y,anyM.sets[:Te]) in keys(anyM.parts.tech), [x,getDescendants(x,t_tree,true)...])) for x in allTech_arr)
		relTech_arr = Array{Int,1}()

		for t in keys(tleaf_dic)
		    subCar_arr = map(y -> anyM.parts.tech[techSym(y,anyM.sets[:Te])].carrier,tleaf_dic[t])
		    if length(unique(subCar_arr)) == 1
		        push!(relTech_arr,t)
		    else
		        append!(relTech_arr,collect(tleaf_dic[t]))
		    end
		end

		# creates dictionary mapping each relevant id to node id
		nodeTe_dic = Dict(x[2] => x[1] + length(nodeC_dic) for x in enumerate(filter(x -> isempty(intersect(getAncestors(x,t_tree,:int),relTech_arr)),unique(relTech_arr))))

		# creates edges between technologies
		edgeTe_arr = Array{Pair{Int,Int},1}()

		for t in keys(nodeTe_dic)
		    gotTech_boo = false; tItr = t
		    while !gotTech_boo
		        if techSym(tItr,anyM.sets[:Te]) in keys(anyM.parts.tech)
		            gotTech_boo = true
		        else
		            tItr = intersect(getDescendants(t,anyM.sets[:Te],true),map(x -> techInt(x,anyM.sets[:Te]),collectKeys(keys(anyM.parts.tech))))[1]
		        end
		    end

		    car_ntup = anyM.parts.tech[techSym(tItr,anyM.sets[:Te])].carrier

		    for cIn in map(x -> getfield(car_ntup,x),intersect(keys(car_ntup),(:use,:stExtIn))) |> (y -> isempty(y) ? y : union(y...))
		        push!(edgeTe_arr, nodeC_dic[cIn] => nodeTe_dic[t])
		    end

		    for cOut in map(x -> getfield(car_ntup,x),intersect(keys(car_ntup),(:gen,:stExtOut))) |> (y -> isempty(y) ? y : union(y...))
		        push!(edgeTe_arr, nodeTe_dic[t] => nodeC_dic[cOut])
		    end
		end

		# creates edges between carriers
		edgeC_arr = Array{Pair{Int,Int},1}()

		for c in keys(nodeC_dic)
		    for cChild in anyM.sets[:C].nodes[c].down
		        push!(edgeC_arr, nodeC_dic[cChild] => nodeC_dic[c])
		    end
		end

		return new(nodeC_dic,nodeTe_dic,edgeC_arr,edgeTe_arr)
	end
end

# XXX specific information for graphical evaluation
mutable struct graInfo
	graph::flowGraph
	names::Dict{String,String}
	colors::Dict{String,Tuple{Float64,Float64,Float64}}

	function graInfo(anyM::AbstractModel)
		# create default options for names and colors
		graph_obj = flowGraph(anyM)

		# specificy some default names and colors used in visualisations
		namesDef_arr = ["coalPlant" => "coal plant", "gasPlant" => "gas plant", "districtHeat" => "district heat", "naturalGas" => "natural gas", "synthGas" => "synthetic gas", "fossilGas" => "fossil gas",
									"demand" => "final demand", "export" => "export", "import" => "import", "crt" => "curtailment", "lss" => "loss of load", "trdSell" => "trade sell", "trdBuy" => "trade buy"]

		# create dictionary assigning internal model names to names used within visualisations
		allVal_arr = unique(vcat(map(x -> getfield.(values(anyM.sets[x].nodes),:val) ,collect(keys(anyM.sets)))...))
		names_dic = setdiff(allVal_arr,getindex.(namesDef_arr,1))  |> (z -> Dict(vcat(namesDef_arr,Pair.(z,z))))

		# define default colors for default energy carriers
		colorsCar_arr = ["electricity" => (1.0, 0.9215, 0.2313),"heat" => (0.769,0.176,0.290),"districtHeat" => (0.6,0.0,0.169), "gas" => (1.0,0.416,0.212),
									"naturalGas" => (1.0,0.506,0.294),"fossilGas" => (0.898,0.259,0.075), "synthGas" => (0.235,0.506,0.325), "hydrogen" => (0.329,0.447,0.827),
													"coal" => (0.459,0.286,0.216),"biomass" => (0.682,0.898,0.443),"bioGas" => (0.682,0.898,0.443)]

		colors_dic = setdiff(getfield.(values(anyM.sets[:C].nodes),:val),getindex.(colorsCar_arr,1)) |> (z -> Dict(vcat(colorsCar_arr,Pair.(z,fill((0.85,0.85,0.85),length(z))))))

		return new(graph_obj,names_dic,colors_dic)
	end
end

# XXX finally, the model object itself
"""
* `options::modOptions`
* `lock::ReentrantLock`
* `supTs::NamedTuple{(:lvl,:step,:sca),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64}}}`
* `cInfo::Dict{Int,NamedTuple}`

The core model object containing all related data and subordinate objects.
#  Constructor and arguments
```julia
anyModel(inDir::Union{String,Array{String,1}},outDir::String; kwargs)
```
* `inDir::Union{String,Array{String,1}}`: directory of input files, also allows for provide multiple directories via an array
* `outDir::String`: directory of output files are written to
# Optional arguments, data handling
* `objName::String`: name of the model object, will be added to the name of output files and printed during reporting, default is an empty string
* `csvDelim::String`: specifies the delimiter used within the read-in csv files, default is a comma `,`
# Optional arguments, model generation
* `decomm::Symbol`: specifies if the model should perform endogenous decommissioning, options are:
    - `:decomm`: capacities are decommissioned endogenously, once decommissioned capacities cannot be put into operation again (default)
    - `:none`: no endogenous decommissioning, operated capacities equal installed capacities
    - `:recomm`: capacities are decommissioned endogenously and can be put back into operation
* `interCapa::Symbol`: capacity expansion can be modelled at a resolution less detailed than yearly, this options determines how capacities are distributed among the subsequent years in this case, options are:
    - `:linear`: expansion is equally distributed among years resulting in a linear increase in capacity (default)
    - `:none`: all expansion occurs in the first  year
* `supTsLvl::Int`: specifies the depth in the tree of time-steps that provides years, default is `0`
* `shortExp::Int`: intervall in years between years of capacity expansion, default is `10`
* `redStep::Float64`: scales down energy quantities within the model, can be relevant when working with reduced time-series, default is `1.0`

# Optional arguments, reporting (see [Data files](@ref) for details)
* `reportLvl::Int`: controls the frequency of writing updates to the console, default is `2`
* `errCheckLvl::Int`: controls the frequency of checking for errors, default is `2`
* `errWrtLvl::Int`: controls the frequency of writing an error report to a csv file, default is`1`
# Optional arguments, numerical issues (see [Performance and stability](@ref) for details)
* `coefRng::NamedTuple`: specifies the maximum range of coefficients in the matrix and right-hand side of the model's underlying optimization problem, default is `(mat = (1e-2,1e5), rhs = (1e-2,1e2))`
* `checkRng::Float64`: if set, reports all equations whose range exceeds the specified value, default is `NaN`
* `scaFac::NamedTuple`: scales different groups of variables within the model, default is `(capa = 1e1, oprCapa = 1e2, dispConv = 1e3, dispSt = 1e4, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)`
* `bound::NamedTuple`: sets external bounds for all capacities and dispatch variables (both in GW) and for the objective value (in Mil. €), default is `(capa = NaN, disp = NaN, obj = NaN)`
* `avaMin::Float64`: availabilities smaller than this value are set to zero, since in the [Conversion capacity restriction](@ref) and [Storage capacity restriction](@ref) availabilities are inversed, this avoids high coefficients, default is `0.01`
* `emissionLoss::Bool`: determines if losses from exchange and self-discharge of storage are subject to emissions, default is `true`
"""
mutable struct anyModel <: AbstractModel

	options::modOptions
	report::Array{Tuple,1}
	optModel::Model
	lock::ReentrantLock

	supTs::NamedTuple{(:lvl,:step,:sca),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64}}}
	cInfo::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:eq),Tuple{Int,Int,Int,Int,Bool}}}

	sets::Dict{Symbol,Tree}
	parts::NamedTuple{(:tech,:trd,:exc,:bal,:lim,:obj),Tuple{Dict{Symbol,TechPart},OthPart,OthPart,OthPart,OthPart,OthPart}}

	graInfo::graInfo
	function anyModel(inDir::Union{String,Array{String,1}},outDir::String; objName = "", csvDelim = ",", decomm = :recomm, interCapa = :linear, supTsLvl = 0, shortExp = 10, redStep = 1.0, emissionLoss = true,
																										reportLvl = 2, errCheckLvl = 1, errWrtLvl = 1, coefRng = (mat = (1e-2,1e5), rhs = (1e-2,1e2)),
																											scaFac = (capa = 1e1, oprCapa = 1e2, dispConv = 1e3, dispSt = 1e4, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0),
																																bound = (capa = NaN, disp = NaN, obj = NaN), avaMin = 0.01, checkRng = NaN)
		anyM = new()

		# <editor-fold desc="initialize report and options"

		# XXX creates dataframe to which reporting is written
		anyM.report = Array{Tuple,1}()

		anyM.optModel = Model()
		anyM.lock = ReentrantLock()

		# XXX sets whole options object from specified directories TODO arbeite mit kwargs später
		outStamp_str = string(objName,"_",Dates.format(now(),"yyyymmddHHMM"))
		defOpt_ntup = (inDir = typeof(inDir) == String ? [inDir] : inDir, outDir = outDir, objName = objName, csvDelim = csvDelim, outStamp = outStamp_str, decomm = decomm, interCapa = interCapa,
																					supTsLvl = supTsLvl, shortExp = shortExp, redStep = redStep, emissionLoss = emissionLoss, coefRng = coefRng, scaFac = scaFac, bound = bound,
																						avaMin = avaMin, checkRng = checkRng, reportLvl = reportLvl, errCheckLvl = errCheckLvl, errWrtLvl = errWrtLvl, startTime = now())

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

		return anyM
	end

	anyModel() = new()
end

# </editor-fold>
