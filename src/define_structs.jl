
# XXX defines model elements (parameters, variables and constraints)

abstract type AbstractModelElement end

mutable struct ParElement <: AbstractModelElement
    name::Symbol
    dim::Tuple
    default_val::Union{Nothing,Float64}
    inherit::Tuple
    data::Union{Nothing,IndexedTable}
    presetLvl::Symbol
    modeDep::Tuple

    function ParElement(paraData_df::Union{Nothing,DataFrame},paraDef_ntup::NamedTuple)

        SetLongShort_dic = Dict(:Ts => :timestep, :R => :region, :C => :carrier, :Te => :technology, :M => :mode)

        # immediately creates object without data if provoided input paraData is nothing
        if paraData_df == nothing return new(paraDef_ntup.name,paraDef_ntup.dim,paraDef_ntup.default_val,paraDef_ntup.inherit,nothing) end

        # XXX check consistency of rows in input dataframe and definition of set and rename columns according to set defintion
        # assigns array of used suffixes according to parameter defintion to each set
        SplitDim_arr = map(x -> map(y -> Symbol(y), split(String(x),"_")),collect(paraDef_ntup.dim))
        SetSuf_dic = Dict(x => map(y -> length(y) == 1 ? nothing : y[end],filter(z -> z[1] == x,SplitDim_arr)) for x in unique(map(x -> x[1],SplitDim_arr)))

        # loops over set columns in input dataframe and assigns them to the sets defined for the parameter
        NewCol_dic = Dict(:val => :val)
        SufNum_dic = Dict(:b => 2, :c => 3, :d => 4, :e => 5, :f => 6, :g => 7)
        for colNam in setdiff(names(paraData_df),[:val])
            colNam_arr = split(String(colNam),"_")
            setNam = Symbol(colNam_arr[1])

            if !(setNam in keys(SetSuf_dic)) # parameter provided for a set not appearing in definition (e.g. demand depending on the technology)
                push!(report,(2, :par, paraDef_ntup.name, "parameter data was specified for $(SetLongShort_dic[setNam]) set, but it is not defined to depend on this set"))
                continue
            elseif length(SetSuf_dic[setNam]) == 1 && length(colNam_arr) > 1 # they are several instances of the set provided, but it only depends on one instance (e.g. two region sets for efficiency)
                push!(report,(2, :par, paraDef_ntup.name, "parameter data was specified for several instances of $(SetLongShort_dic[setNam]) set, but it is defined to depend only on one instance, additonal instances were ignored"))
                continue
            elseif SetSuf_dic[setNam][1] == nothing # set has only one instance and no suffix => no change when converting from read-in dataframe to parameter element
                NewCol_dic[colNam] = colNam
            elseif length(SetSuf_dic[setNam]) == 1 || length(colNam_arr) == 1 # column name in dataframe has no underscore, but defintion of parameter element has one
                NewCol_dic[colNam] = Symbol(setNam, "_", SetSuf_dic[setNam][1])
            else
                cntRep_int = SufNum_dic[Symbol(colNam_arr[2])] # set defined for several instances
                NewCol_dic[colNam] = Symbol(setNam, "_", SetSuf_dic[setNam][cntRep_int])
            end
        end

        # filters only used coulumns, renames them accordingly and converts to table
        paraData_df = paraData_df[:,collect(keys(NewCol_dic))]
        DataFrames.rename!(paraData_df,NewCol_dic)
        writeData_tab = JuliaDB.table(paraData_df)

        new_obj = new(paraDef_ntup.name,paraDef_ntup.dim,paraDef_ntup.default_val,paraDef_ntup.inherit,writeData_tab)
        # defines on which level parameter is presetted and which capacity restrictions are affected by different modes for all dispatch parameters, where this is specified
        if :presetLvl in keys(paraDef_ntup) new_obj.presetLvl = paraDef_ntup.presetLvl end
        if :modeDep   in keys(paraDef_ntup) new_obj.modeDep   = paraDef_ntup.modeDep   end
        return new_obj
    end
end

mutable struct VarElement <: AbstractModelElement
    name::Symbol
    dim::Tuple
    data::IndexedTable

    function VarElement(name::Symbol, dim::Tuple, data::IndexedTable)
        return new(name,dim,data)
    end
end

struct CnsElement <: AbstractModelElement
    name::Symbol
    dim::Tuple
    data::IndexedTable

    CnsElement(name::Symbol; kwargs...) = CnsElement(Val{name}(), name; kwargs...)
    function CnsElement(name::Symbol, dim::Tuple, data::IndexedTable)
        return new(name,dim,data)
    end
end

# XXX defines the anyMOD and its options
struct modOptions

	# data in- and output related options
	inDir::Array{String,1}
	outDir::String
	name::String
	csvDelim::String
	outStamp::String
	# computation related options
	decomm::Symbol
	interCapa::Symbol
	shortInvest::Int64
	digits::NamedTuple{(:read,:comp),Tuple{Int64,Int64}}
	scale::NamedTuple{(:cost,:ener,:capa,:ems),Tuple{Float64,Float64,Float64,Float64}}
	bound::NamedTuple{(:inv,:disp, :cost),Tuple{Union{Nothing,Float64},Union{Nothing,Float64},Union{Nothing,Float64}}}
	# reporting related options
	reportLvl::Int64
	errCheckLvl::Int64
	errWrtLvl::Int64
	startTime::DateTime
end

mutable struct anyModel

	options::modOptions
	report::DataFrame
	optModel::Model
	supDis::NamedTuple{(:lvl,:step,:dic),Tuple{Int32,Tuple{Vararg{Int32,N} where N},Dict{Tuple{Int32,Int32},Float64}}}

	aggVar::Dict{Symbol,Dict{Int64,BitSet}}

	sets::Dict{Symbol,DataFrame}
	mapping::Dict{Symbol,IndexedTable}
	parameter::Dict{Symbol,ParElement}
	variables::Dict{Symbol,VarElement}
	constraints::Dict{Symbol,CnsElement}

	function anyModel(input_dir::Union{String,Array{String,1}},output_dir::String;args...)
		self = new()
		# XXX creates dataframe to which reporting is written
		self.report = DataFrame(type = Int32[], group = Symbol[], instance = Symbol[],  message = String[])

		# XXX sets whole options object from specified directories, defaults and args
		# creates named tuple of default value for options
		defOpt_ntup = (name = "", csvDelim = ",", outStamp = Dates.format(now(),"yyyymmddHHMM"), decomm = :none, interCapa = :linear, shortInvest = 5, digits = (read = 3, comp = 3),
										scale = (cost = 1, ener = 1, capa = 1, ems = 1000), bound = (inv = nothing, disp = nothing, cost = nothing), reportLvl = 2, errCheckLvl = 1, errWrtLvl = 1, startTime = now())

		# checks if any non-existing option values were provided via args
		undef_tup = filter(x -> !(x in keys(defOpt_ntup)), collect(map(x -> x[1],collect(args))))
		self.options = modOptions(values(merge(merge((inDir = (typeof(input_dir) == String ? [input_dir] : input_dir), outDir = output_dir),defOpt_ntup),filter(x -> !(x[1] in undef_tup),collect(args))))...)
		# reports on undefined inputs to intilization
		if !isempty(undef_tup)
			push!(self.report,(2,:call,:options,"undefined options provided to model initialization: $(join(undef_tup,", "))"))
			print(getElapsed(self.options.startTime)); errorTest(self.report,self.options,true)
		end

		# XXX creates JuMP model object and empty dictionary to save aggregations
		self.optModel = Model()
        self.aggVar = Dict{Symbol,Dict{Int64,BitSet}}()

		# XXX read-in csv data and creates mapping
		self.sets, self.parameter, SetData_dic, SaveLookup_dic = readInData(self)
		createAllMappings!(self, SetData_dic, SaveLookup_dic)

		produceMessage(self.options,self.report, 1," - Intialized model structure")
		return self
	end
end
