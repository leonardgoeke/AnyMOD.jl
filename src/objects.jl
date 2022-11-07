
#region # * definition and handling of parameters

# ! defines struct for handling parameter data
"""
```julia
mutable struct ParElement
	name::Symbol
    dim::Tuple
    defVal::Union{Nothing,Float64}
    herit::Tuple
    data::DataFrame
	techPre::NamedTuple{(:preset,:mode),Tuple{Symbol,Tuple{Vararg{Symbol,N} where N}}}
end
```
Type to store parameter data. Includes data and additional information specified in [Parameter list](@ref).

**Fields**
- `name::Symbol`: name of the parameter
- `dim::Tuple`: potential dimensions of parameter data
- `defVal::Union{Nothing,Float64}`: default value
- `herit::Tuple`: inheritance rules for parameter, see [Parameter overview](@ref)
- `data::DataFrame`: parameter data
"""
mutable struct ParElement
	name::Symbol
	problem::Symbol
    dim::Tuple
    defVal::Union{Nothing,Float64}
    herit::Tuple
    data::DataFrame
	techPre::NamedTuple{(:preset,:mode),Tuple{Symbol,Tuple{Vararg{Symbol,N} where N}}}

	function ParElement(paraData_df::DataFrame,paraDef_ntup::NamedTuple,name::Symbol,report::Array{Tuple,1})

        setLongShort_dic = Dict(:Ts => :timestep, :R => :region, :C => :carrier, :Te => :technology, :Exc => :exchange, :M => :mode)
		if isempty(paraData_df) return new(name,paraDef_ntup.problem,paraDef_ntup.dim,paraDef_ntup.defVal,paraDef_ntup.herit,DataFrame()) end

        # ! check consistency of rows in input dataframe and definition of set and rename columns according to set defintion
        # assigns array of used suffixes according to parameter defintion to each set
        splitDim_arr = map(x -> map(y -> Symbol(y), split(String(x),"_")),collect(paraDef_ntup.dim))
        setSuf_dic = Dict(x => map(y -> length(y) == 1 ? Symbol() : (length(y) == 2  ? y[end] : Symbol(y[end-1],"_",y[end])),filter(z -> z[1] == x,splitDim_arr)) for x in unique(map(x -> x[1],splitDim_arr)))

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

        new_obj = new(name,paraDef_ntup.problem,paraDef_ntup.dim,paraDef_ntup.defVal,paraDef_ntup.herit,writeData_df)
        # defines on which level parameter is presetted and which capacity restrictions are affected by different modes for all dispatch parameters, where this is specified
        if haskey(paraDef_ntup,:techPre) new_obj.techPre = paraDef_ntup.techPre end
        return new_obj
    end

	ParElement() = new()
end

# ! specific struct for read in process of parameter data
mutable struct parEntry
	colSet::Symbol
	entry::Array{String,1}
	lvl::Array{Int,1}
	startLvl::Int
end

#endregion

#region # * import and extensions of base functions

# ! functions to copy parameter structs of parameter data
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

# ! usual collect sometimes creates a mysterious error if used on dictionary keys, this command avoids this
import Base._collect
import Base.SizeUnknown

collectKeys(itr) = _collect(Symbol, itr, SizeUnknown())

#endregion

#region # * struct for individual parts of the model

# ! defines parts of the model
abstract type AbstractModelPart end

"""
```julia
mutable struct TechPart <: AbstractModelPart
	name::Tuple{Vararg{String,N} where N}
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	carrier::NamedTuple
	balLvl::NamedTuple{(:exp,:ref),Tuple{Tuple{Int,Int},Union{Nothing,Tuple{Int,Int}}}}
	capaRestr::DataFrame
	actSt::Tuple
	stCyc::Int
	type::Symbol
	disAgg::Bool
	modes::Tuple{Vararg{Int,N} where N}
	TechPart(name::Tuple{Vararg{String,N} where N}) = new(name,Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
	TechPart() = new()
end
```
Type used for technology model parts.

**General fields**

- `par`: dictionary of parameters with names as keys (see [Parameter list](@ref))
- `var`: dictionary of variables with names as keys (see [Variables](@ref))
- `cns`: dictionary of constraints with names as key (see [Constraints](@ref))


**Technology specific fields**

See [Technologies](@ref) for details.

- `name`: full name of technology as a series of nodes from the technology tree
- `carrier`: ids of energy carriers assigned to technology by groups (e.g. generation, use, ...)
- `balLvl`: temporal and spatial resolution for expansion and conversion balance of the technology
- `capaRestr`: specification of capacity restrictions required for technology
- `actSt`: ids of carriers actively stored although they are not leafs
- `type`: type of technology (`stock`, `mature`, or `evolving`)
- `disAgg`: if `true`, dispatch is modelled at expansion resolution instead of dispatch resolution
- `modes`: different operational modes of technology

"""
mutable struct TechPart <: AbstractModelPart
	name::Tuple{Vararg{String,N} where N}
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	carrier::NamedTuple
	balLvl::NamedTuple{(:exp,:ref),Tuple{Tuple{Int,Int},Union{Nothing,Tuple{Int,Int}}}}
	balSign::NamedTuple{(:conv,:st),Tuple{Symbol,Symbol}}
	capaRestr::DataFrame
	type::Symbol
	stCyc::Int
	disAgg::Bool
	intCapaRestr::Bool
	modes::Tuple{Vararg{Int,N} where N}
	decomm::Symbol
	TechPart(name::Tuple{Vararg{String,N} where N}) = new(name,Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
	TechPart() = new()
end

mutable struct ExcPart <: AbstractModelPart
	name::Tuple{Vararg{String,N} where N}
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	carrier::Tuple
	capaRestr::DataFrame
	type::Symbol
	dir::Bool
	decomm::Symbol
	expLvl::Tuple{Int,Int}

	ExcPart(name::Tuple{Vararg{String,N} where N}) = new(name,Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
	ExcPart() = new()
end

"""
```julia
mutable struct TechPart <: AbstractModelPart
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	OthPart() = new(Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}(
end
```

Type used for 'exchange', 'trade', 'balance', 'limits', and 'objective' model parts.

**Fields**

- `par`: dictionary of parameters with names as keys (see [Parameter list](@ref))
- `var`: dictionary of variables with names as keys (see [Variables](@ref))
- `cns`: dictionary of constraints with names as keys (see [Constraints](@ref))

"""
mutable struct OthPart <: AbstractModelPart
	par::Dict{Symbol,ParElement}
	var::Dict{Symbol,DataFrame}
	cns::Dict{Symbol,DataFrame}
	OthPart() = new(Dict{Symbol,ParElement}(),Dict{Symbol,DataFrame}(),Dict{Symbol,DataFrame}())
end

# ! container to store data defining a constraint (used to separate definition and actual jump creation of constraints)
struct cnsCont
    data::DataFrame
    sign::Symbol
end

#endregion

#region # * structs for nodes that then make up the trees to save set data

# ! define nodes for set tree and tree itself
"""
```julia
mutable struct Node
	idx::Int
	val::String
	lvl::Int
	subIdx::Int
	down::Array{Int,1}
end
```

Type to store nodes of hierarchical trees.

**Fields**

- `idx`: internal node id
- `val`: name originally assigned
- `lvl`: level of node within hierarchical tree
- `subIdx`: numbered position among all nodes sharing the same direct ancestor
- `down`: array of children
"""
mutable struct Node
	idx::Int
	val::String
	lvl::Int
	subIdx::Int
	down::Array{Int,1}
end

"""
```julia
mutable struct Tree
	nodes::Dict{Int,Node}
	srcTup::Dict{Tuple,Array{Int,1}}
	srcStr::Dict{Tuple{String,Int},Array{Int,1}}
	up::Dict{Int,Int}
	height::Int
	Tree() = new(Dict{Int,Node}(),Dict{Tuple,Int}(),Dict{String,Array{Int,1}}(),Dict{Int,Int}(),1)
end
```

Type to store hierarchical trees.

**Fields**

- `nodes`: dictionary of nodes with node ids as keys
- `srcTup`: assigns a tuple of consecutive node names to the corresponding id
- `srcStr`: assigns a tuple with a node name and a level to the corresponding id
- `up`: assigns the id of each node to the id of its ancestor
- `height`: maximum level of tree
"""
mutable struct Tree
	nodes::Dict{Int,Node}
	srcTup::Dict{Tuple,Array{Int,1}}
	srcStr::Dict{Tuple{String,Int},Array{Int,1}}
	up::Dict{Int,Int}
	height::Int
	Tree() = new(Dict{Int,Node}(),Dict{Tuple,Int}(),Dict{String,Array{Int,1}}(),Dict{Int,Int}(),1)
end

#endregion

#region # * options for model and model itself

# create abstract model object to reference before creation (avoid circular type definiton)
abstract type AbstractModel end

# ! defines final model object and its options
mutable struct modOptions
	# data in- and output
	inDir::Array{String,1}
	outDir::String
	objName::String
	csvDelim::String
	outStamp::String
	# model generation
	interCapa::Symbol
	supTsLvl::Int
	shortExp::Int
	redStep::Float64
	holdFixed::Bool
	# managing numerical issues
	emissionLoss::Bool
	forceScr::Union{Symbol,Nothing}
	coefRng::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Vararg{Float64,2}}}}
	scaFac::NamedTuple{(:capa,:capaStSize,:insCapa,:dispConv,:dispSt,:dispExc, :dispTrd, :costDisp,:costCapa,:obj),Tuple{Vararg{Float64,10}}}
	bound::NamedTuple{(:capa,:disp,:obj),Tuple{Vararg{Float64,3}}}
	avaMin::Float64
	checkRng::NamedTuple{(:print,:all),Tuple{Bool,Bool}}
	# reporting related options
	reportLvl::Int
	errCheckLvl::Int
	errWrtLvl::Int
	startTime::DateTime
end

# ! flow graph object that defines relations between technologies and carriers (and among carriers)
mutable struct flowGraph
	nodeC::Dict{Int,Int}
	nodeTe::Dict{Int,Int}
	edgeC::Array{Pair{Int,Int},1}
	edgeTe::Array{Pair{Int,Int},1}
	nodePos::Dict{Int,Array{Float64,1}}

	function flowGraph(anyM::AbstractModel)

		# creates dictionary mapping carrier id to node id
		nodeC_dic = Dict(x[2] => x[1] for x in enumerate(sort(filter(x -> x != 0,getfield.(collect(values(anyM.sets[:C].nodes)),:idx)))))

		# get all relevant technology, a technology is not relevant, where all children of a parent have the same carriers (in this case only the parent is relevant)
		t_tree = anyM.sets[:Te]

		allTech_arr =  getfield.(collect(values(t_tree.nodes)),:idx)
		tleaf_dic = Dict(x => unique(filter(y -> sysSym(y,anyM.sets[:Te]) in keys(anyM.parts.tech), [x,getDescendants(x,t_tree,true)...])) for x in allTech_arr)
		relTech_arr = Array{Int,1}()

		for t in keys(tleaf_dic)
		    subCar_arr = map(y -> anyM.parts.tech[sysSym(y,anyM.sets[:Te])].carrier,tleaf_dic[t])
		    append!(relTech_arr,collect(tleaf_dic[t]))
		end

		# creates dictionary mapping each relevant id to node id
		nodeTe_dic = Dict(x[2] => x[1] + length(nodeC_dic) for x in enumerate(filter(x -> isempty(intersect(getAncestors(x,t_tree,:int),relTech_arr)),unique(relTech_arr))))

		# creates edges between technologies
		edgeTe_arr = Array{Pair{Int,Int},1}()

		for t in keys(nodeTe_dic)
		    gotTech_boo = false; tItr = t
		    while !gotTech_boo
		        if sysSym(tItr,anyM.sets[:Te]) in keys(anyM.parts.tech)
		            gotTech_boo = true
		        else
		            tItr = intersect(getDescendants(t,anyM.sets[:Te],true),map(x -> sysInt(x,anyM.sets[:Te]),collectKeys(keys(anyM.parts.tech))))[1]
		        end
		    end

			car_ntup = anyM.parts.tech[sysSym(tItr,anyM.sets[:Te])].carrier	

		    for cIn in map(x -> x == :use ? getfield(car_ntup,x) : union(getfield(car_ntup,x)...),intersect(keys(car_ntup),(:use,:stExtIn))) |> (y -> isempty(y) ? y : union(y...))
		        push!(edgeTe_arr, nodeC_dic[cIn] => nodeTe_dic[t])
		    end

		    for cOut in map(x -> x == :gen ? getfield(car_ntup,x) : union(getfield(car_ntup,x)...),intersect(keys(car_ntup),(:gen,:stExtOut))) |> (y -> isempty(y) ? y : union(y...))
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

# ! specific information for graphical evaluation
"""
```julia
mutable struct graInfo
	graph::flowGraph
	names::Dict{String,String}
	colors::Dict{String,Tuple{Float64,Float64,Float64}}
end
```
Type to store information on styling of graphs. See [Styling](@ref).

**Fields**

- `graph`: saved layout for the qualitative energy flow graph
- `names`: assigns names of nodes to labels used in plots
- `colors`: assigns names or label of nodes to RGB color specified as tuple of three numbers between 0 and 1
"""
mutable struct graInfo
	graph::flowGraph
	names::Dict{String,String}
	colors::Dict{String,Tuple{Float64,Float64,Float64}}

	function graInfo(anyM::AbstractModel)
		# create default options for names and colors
		graph_obj = flowGraph(anyM)

		# specify some default names and colors used in visualisations
		namesDef_arr = ["exchangeLoss" => "exchange losses","coalPlant" => "coal plant", "gasPlant" => "gas plant", "districtHeat" => "district heat", "naturalGas" => "natural gas", "synthGas" => "synthetic gas", "fossilGas" => "fossil gas",
									"demand" => "final demand", "export" => "export", "import" => "import", "netImport" => "net import", "netExport" => "net export","crt" => "curtailment", "lss" => "loss of load", "trdSell" => "trade sell", "trdBuy" => "trade buy"]

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

# ! finally, the model object itself
"""
```julia
mutable struct anyModel <: AbstractModel
	options::modOptions
	report::Array{Tuple,1}
	optModel::Model
	lock::ReentrantLock
	supTs::NamedTuple{(:lvl,:step,:sca),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64}}}
	cInfo::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:balSign,stBalCapa),Tuple{Int,Int,Int,Int,Symbol,Symbol}}}
	sets::Dict{Symbol,Tree}
	parts::NamedTuple{(:tech,:exc,:bal,:lim,:costs,:obj),Tuple{Dict{Symbol,TechPart},OthPart,OthPart,OthPart,OthPart,OthPart,OthPart}}
	graInfo::graInfo
end
```
The core model object containing all related data and subordinate objects.

**Fields**

- `options`: model options provided as keyword arguments to constructor
- `report`: entries for writing to the reporting file [Error handling](@ref)
- `optModel::Model`: the actual [JuMP](https://github.com/JuliaOpt/JuMP.jl) object of the model's underlying optimization problem
- `lock`: lock used for multi-threading
- `supTs`: information and mappings for superordinate time-steps
- `cInfo`: information on resolution of energy carriers
- `sets`: sets organized as [Tree](@ref) objects (see [Sets and mappings](@ref))
- `parts::NamedTuple`: all part objects of the model (see [Parts](@ref))
- `graInfo::graInfo`: properties for creation of plots and graphics, can be used to adjust colors and labels (see [Styling](@ref))


**Constructor**

```julia
anyModel(inDir::Union{String,Array{String,1}},outDir::String; kwargs)
```

See [Model Object](@ref) for a detailed list of arguments.
"""
mutable struct anyModel <: AbstractModel

	options::modOptions
	report::Array{Tuple,1}
	optModel::Model
	lock::ReentrantLock

	supTs::NamedTuple{(:lvl,:step,:sca,:scr,:scrProp),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64},Dict{Int,Array{Int,1}},Dict{Tuple{Int,Int},Float64}}}
	subPro::Union{Tuple{},Tuple{Int,Int}}
	cInfo::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:balSign,:stBalCapa),Tuple{Int,Int,Int,Int,Symbol,Symbol}}}

	sets::Dict{Symbol,Tree}
	parts::NamedTuple{(:tech,:exc,:bal,:lim,:cost,:obj),Tuple{Dict{Symbol,TechPart},Dict{Symbol,ExcPart},OthPart,OthPart,OthPart,OthPart}}

	graInfo::graInfo
	function anyModel(inDir::Union{String,Array{String,1}},outDir::String; objName = "", csvDelim = ",", interCapa = :linear, supTsLvl = 0, shortExp = 10, redStep = 1.0, holdFixed = false, emissionLoss = true, forceScr = nothing,
																										reportLvl = 2, errCheckLvl = 1, errWrtLvl = 1, coefRng = (mat = (1e-2,1e4), rhs = (1e-2,1e2)),
																											scaFac = (capa = 1e2,  capaStSize = 1e2, insCapa = 1e1,dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0),
																												bound = (capa = NaN, disp = NaN, obj = NaN), avaMin = 0.01, checkRng = (print = false, all = true))
		anyM = new()

		#region # * initialize report and options

		# ! creates dataframe to which reporting is written
		anyM.report = Array{Tuple,1}()

		anyM.optModel = Model()
		anyM.lock = ReentrantLock()

		# ! sets whole options object from specified directories TODO arbeite mit kwargs später
		outStamp_str = string(objName,"_",Dates.format(now(),"yyyymmddHHMM"))
		defOpt_ntup = (inDir = typeof(inDir) == String ? [inDir] : inDir, outDir = outDir, objName = objName, csvDelim = csvDelim, outStamp = outStamp_str, interCapa = interCapa, supTsLvl = supTsLvl, shortExp = shortExp, 
																										redStep = redStep, holdFixed = holdFixed, emissionLoss = emissionLoss, forceScr = forceScr, coefRng = coefRng, scaFac = scaFac, bound = bound,
																											avaMin = avaMin, checkRng = checkRng, reportLvl = reportLvl, errCheckLvl = errCheckLvl, errWrtLvl = errWrtLvl, startTime = now())

		anyM.options = modOptions(defOpt_ntup...)

		if isempty(inDir) return anyM end
		#endregion
		
		#region # * read in set and parameter data
		files_dic = readInputFolder(anyM.options.inDir)

		# ! read-in sets and parameters
		setData_dic = readSets!(files_dic,anyM)
		if !any(map(x -> x[1] == 3, anyM.report))
			paraTemp_dic = readParameters!(files_dic,setData_dic,anyM)
		end

		produceMessage(anyM.options,anyM.report, 1," - Read-in all set and parameter files")
		
		#endregion

		#region # * create part objects and general mappings
		sysArr_dic = Dict{Symbol,Array{Int64,1}}()
		# assign actual tech to parents
		for sys in [(:Te,"technology"),(:Exc,"exchange")]
			if !(sys[1] in keys(setData_dic))
				sysArr_dic[sys[1]] = Int[]
			else
				relSys_df = setData_dic[sys[1]][!,Symbol.(filter(x -> occursin(sys[2],x) && !isnothing(tryparse(Int16,string(x[end]))), string.(namesSym(setData_dic[sys[1]]))))]
				relSys_df = DataFrame(filter(x -> any(collect(x) .!= ""), eachrow(relSys_df)))
				sysArr_dic[sys[1]] = filter(z -> isempty(anyM.sets[sys[1]].nodes[z].down), map(x -> lookupTupleTree(tuple(collect(x)...),anyM.sets[sys[1]],1)[1], eachrow(relSys_df)))
			end
		end
		
		anyM.parts = (tech = Dict(sysSym(x,anyM.sets[:Te]) => TechPart(getUniName(x,anyM.sets[:Te])) for x in sysArr_dic[:Te]), exc = Dict(sysSym(x,anyM.sets[:Exc]) => ExcPart(getUniName(x,anyM.sets[:Exc])) for x in sysArr_dic[:Exc]), bal = OthPart(), lim = OthPart(), cost = OthPart(), obj = OthPart())

		createCarrierMapping!(setData_dic,anyM)
		createTimestepMapping!(anyM)

		# ! write general info about systems (technologies and exchange)
		if :Exc in keys(setData_dic) && !(:carrier_exchange in namesSym(setData_dic[:Exc])) 
			push!(anyM.report,(3,"exchange mapping","carrier","column 'carrier_exchange' missing from set file for exchange"))
		else
			for sys in keys(sysArr_dic), s in sysArr_dic[sys] 
				createSysInfo!(sys,sysSym(s,anyM.sets[sys]), setData_dic, anyM) 
			end
		end
		produceMessage(anyM.options,anyM.report, 2," - Created all mappings among sets", testErr = 3 in getindex.(anyM.report,1))

		# ! assign parameters to model parts
		parameterToParts!(paraTemp_dic, sysArr_dic, anyM)
		produceMessage(anyM.options,anyM.report, 2," - Assigned parameter data to model parts")

		# ! add scenario mappings
		createScenarioMapping!(anyM)

		# ! create object for data visualization
		anyM.graInfo = graInfo(anyM)

		produceMessage(anyM.options,anyM.report, 1," - Prepared creation of optimzation model")
		#endregion

		return anyM
	end

	anyModel() = new()
end