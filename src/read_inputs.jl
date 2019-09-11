
# XXX main function to read inputs, also creates sets and parameters

function readInData()

	# <editor-fold desc= XXX Sets>
	Files_dic = readInputFolder(InputFolder_str)

	# creates relevant sets, manually adds :mode and assigns short names
	Set_arr = append!(map(x ->  Symbol(x[findfirst("set_",x)[1]+4:end-4]),Files_dic["set"]), [:mode, :id])
	SetLongShort_dic = Dict(:timestep => :Ts, :region => :R, :carrier => :C, :technology => :Te, :mode => :M, :id => :id)

	SetData_dic = Dict{Symbol,DataFrame}()
	Set_dic = Dict{Symbol,DataFrame}()

	# read in sets
	for setFile in Files_dic["set"]
		SetLong_sym = Symbol(setFile[findfirst("set_",setFile)[1]+4:end-4])
		SetShort_sym = get!(SetLongShort_dic,SetLong_sym,SetLong_sym)
		if SetShort_sym in keys(Set_dic) push!(Report_df,(2,:set,SetLong_sym,"multiple input files for set, only considered $setFile")); continue end
		SetData_dic[SetShort_sym] = convertReadIn(CSV.File(setFile;delim = csvDelim_str, pool = false, categorical=true) |> DataFrame,setFile,Set_arr,SetLongShort_dic)
		Set_dic[SetShort_sym] = createTree(SetData_dic[SetShort_sym],SetLong_sym)
		produceMessage(3," - Read-in set file: ",setFile)
	end
	produceMessage(2," - Read-in all set files ")

	# manually adds mode set, creates a top node for all modes to allow for aggregation later
	modes_df = DataFrame(mode_1 = vcat(map(y -> split(replace(y," " => ""),";"),filter(x -> x != "",SetData_dic[:Te][!,:mode]))...))
	if !isempty(modes_df)
		Set_dic[:M] = createTree(modes_df,:mode)
		push!(Set_dic[:M],[0,"topMode",0,0,1,collect(1:length(Set_dic[:M]))])
	else
		Set_dic[:M] = DataFrame()
	end
	# </editor-fold>

	# <editor-fold desc= XXX Parameter>
	ParameterTemp_dic = Dict{Symbol, DataFrame}()
	Parameter_dic = Dict{Symbol, ParElement}()

	SaveLookup_dic = Dict{Symbol,Dict{Tuple,Array}}(x => Dict{Tuple,Array}() for x in values(SetLongShort_dic))

	# read-in parameter files and convert their content
	for parFile in Files_dic["par"]
		ParData_df = convertReadIn(CSV.File(parFile;delim = csvDelim_str, pool = false, categorical=true) |> DataFrame,parFile,Set_arr,SetLongShort_dic,Set_dic)
		SaveLookup_dic, ParameterTemp_dic = writeParameter(ParData_df,Set_dic,ParameterTemp_dic,SetLongShort_dic,parFile,SaveLookup_dic)
		produceMessage(3," - Read-in parameter file: ",parFile)
	end
	produceMessage(2," - Read-in all parameter files ")

	# remove non-unique rows and checks for contradicting values within read-in parameter values
	for parameter in keys(ParameterTemp_dic)
		# order regions in ascending order so regions are not ambivalent anymore and duplicates can be identified
		if :R_b in names(ParameterTemp_dic[parameter])
			sortR_mat = sort(hcat([ParameterTemp_dic[parameter][!,x] for x in (:R,:R_b)]...);dims = 2)
			for (index,col) in enumerate((:R,:R_b)) ParameterTemp_dic[parameter][!,col] = sortR_mat[:,index] end
		end

		# checks for duplicates and removes them in case
		nonUnique_bool = nonunique(ParameterTemp_dic[parameter])
		if any(nonUnique_bool)
			push!(Report_df,(1,:par,parameter,"non-unique entries discovered"))
			deleterows!(ParameterTemp_dic[parameter],nonUnique_bool)
		end

		# checks for contradicting values
		contradic_bool = nonunique(ParameterTemp_dic[parameter][:,removeVal(ParameterTemp_dic[parameter])])
		if any(contradic_bool)
			 push!(Report_df,(3,:par,parameter,"contradicting entries discovered"))
		end

	end

	# gets defintion of parameters and checks, if all input parameters are defined
	ParDef_dic = defineParameter()
	UndefinedPar_arr = setdiff(keys(ParameterTemp_dic),keys(ParDef_dic))
	if !isempty(UndefinedPar_arr)
		for undefined in UndefinedPar_arr push!(Report_df,(3,:par,undefined,"parameter was not defined in parameter.jl")) end
		print(getElapsed())
		errorTest(true)
	end

	# converts read-in parameter values into parameter objects
	Parameter_dic = Dict(x => ParElement(ParameterTemp_dic[x],ParDef_dic[x]) for x in keys(ParameterTemp_dic))
	# creates parameter object with nothing as data for parameters having a default value, but no values provided
	for x in setdiff(keys(ParDef_dic),keys(ParameterTemp_dic))
		if ParDef_dic[x].default_val != nothing  Parameter_dic[x] = ParElement(nothing,ParDef_dic[x]) end
	end

	# </editor-fold>

	produceMessage(1," - Completed data read-in ")

	return Set_dic, Parameter_dic, SetData_dic, SaveLookup_dic
end

# <editor-fold desc="read and process original csv data"

# XXX read inputs folders for all 'csv' or 'jl' files starting with 'set', 'par', 'var' and 'eqn'
function readInputFolder(InputFolder_str::String,Files_dic = Dict(b => [] for b in ("set","par","var","eqn")))
    FilesDir_arr = readdir(InputFolder_str)

    # loops over files in input folder
    for file in FilesDir_arr

        if occursin(".",file) FileType_str = file[findfirst(".",file)[1]+1:end] else FileType_str = "" end
        FileGrp_str = file[1:3]
        FileDir_str = string(InputFolder_str,"/",file)

        # loops over subfolders
        if (FileType_str == "csv" && FileGrp_str in ("set","par","var","eqn"))
            Files_dic[FileGrp_str] = push!(Files_dic[FileGrp_str],FileDir_str)
        elseif !isfile(FileDir_str)
            Files_dic = readInputFolder(FileDir_str,Files_dic)
        end
    end

    return Files_dic
end

# XXX filters missing and adjusts data according to "all" statements
function convertReadIn(ReadIn_df::DataFrame,FileName_str::String,Set_arr::Array{Symbol},SetLongShort_dic::Dict{Symbol,Symbol},Set_dic::Dict{Symbol,DataFrame} = Dict{Symbol,DataFrame}())

    SetNames_arr = filterSetColumns(ReadIn_df,Set_arr)
    OprNames_arr = filterSetColumns(ReadIn_df,[:parameter,:variable,:value, :id])
	ReadInCol_tup = tuple(names(ReadIn_df)...)

	# XXX drop unrequired rows and convert missing values
    types_arr = eltypes(ReadIn_df)
    for (idx, col) in enumerate(names(ReadIn_df))
		# drop irrelevant column that do not relate to a set or an operator
        if !(col in vcat(SetNames_arr[1],SetNames_arr[2],OprNames_arr[1]))
            deletecols!(ReadIn_df, col)
            continue
        end

		# convert remaining columns to strings and replace 'missing' with empty string
        if types_arr[idx] != Union{Missing, String}
            ReadIn_df[!,col] = map(x -> string(x),ReadIn_df[!,col])
            ReadIn_df[!,col] = replace(ReadIn_df[!,col] , "missing"=>"")
         end
        ReadIn_df[ismissing.(ReadIn_df[!,col]), col] = ""
    end

	# XXX converts type of array container to array to allow for changing the ReadIn_df later
	for col in eachcol(ReadIn_df, true)
	#	if (typeof(col[2]) <: Array)!
			ReadIn_df[!,col[1]] = convert(Array{String,1},ReadIn_df[!,col[1]])
	#	end
	end

    # XXX rewrites rows with all commands into full format
    for col in SetNames_arr[1]

        # check column for keywords
        ColVal_arr = convert(Array{String,1},ReadIn_df[:, col])
        RowsAll_arr = map(x -> length(x) >= 3 && lowercase(x[1:3]) == "all",ColVal_arr)

        if all(!,RowsAll_arr) continue end

        # determine relevant reference for "all", if parameter are read in
        if  !isempty(Set_dic) # take reference from readin sets
            SpecSet_arr = split(String(col),"_")
            RelSet_df = Set_dic[SetLongShort_dic[Symbol(SpecSet_arr[1])]]
            ColValUni_arr = unique(RelSet_df[RelSet_df[:,:lvl] .== parse(Int,SpecSet_arr[2]),:val])
        else # take reference from other column values, relevant when sets are currently read in
            ColValUni_arr = sort(unique(filter(x -> !isempty(x),ColVal_arr[(!).(RowsAll_arr)])))
        end

        # loop over rows with all
        for row in eachrow(ReadIn_df[RowsAll_arr,:])
            # append new rows to dataframe
            AddRow_df = row
            AllInfo_str = reduce(replace, ["all"=>"", "("=>"", ")"=>""], init=lowercase(AddRow_df[col]))

            if occursin(":",AllInfo_str)
                AllVal_arr = split(AllInfo_str,":")
                RplVal_arr = ColValUni_arr[findall(x->x==AllVal_arr[1], ColValUni_arr)[1]:findall(x->x==AllVal_arr[2], ColValUni_arr)[1]]
            elseif occursin(",",AllInfo_str)
                AllVal_arr = split(AllInfo_str,",")
                RplVal_arr = ColValUni_arr[map(x -> in(x,AllVal_arr),ColValUni_arr)]
            else
                RplVal_arr = ColValUni_arr
            end
            for addVal in RplVal_arr
                AddRow_df[col] = addVal
                push!(ReadIn_df, [AddRow_df[col] for col in ReadInCol_tup])
            end
        end
        #remove inital rows with all#

        deleterows!(ReadIn_df,findall(RowsAll_arr))
    end

	# XXX convert column names if sets are defined for multiple insances (e.g. two regions in case of trade related parameters)
	if split(FileName_str,"/")[end][1:3] == "par"
		# creates a dictionary that assigns everything after the set name seperated with a "_" to the respective set
		SplitCol_arr = map(x -> split(String(x),"_"),setdiff(names(ReadIn_df),OprNames_arr[1]))
		SetCol_arr = unique(map(x -> Symbol(x[1]),SplitCol_arr))
		GrpCol_dic = Dict(x => map(z -> z[2:end],filter(y -> String(x) == y[1],SplitCol_arr)) for x in SetCol_arr)

		# loop over dictionary to check for irregular names and two sets having two columns assigned
		letters_arr = ("b","c","d","e","f","g","h")

		for set in keys(GrpCol_dic)
			newCol_dic = Dict{Symbol,Symbol}()

			if any(map(x -> tryparse(Int,x),vcat(GrpCol_dic[set]...)) .== nothing)
				push!(Report_df,(3,:par,Symbol(FileName_str),"column for set $(set) does not contain a number after _"))
				continue
			end

			# get the unique number of "_" that appear in columns assigned to the respective set
			# if the csv file has several columns of the same name JULIA adds a "_1" to the second column when reading in as a dataframe => so this checks if set is defined for multiple instances
			uniLen_arr = unique(map(x -> length(x),GrpCol_dic[set]))
			if  length(uniLen_arr) == 2
				setNumbers_arr = map(x -> parse(Int,x[1]),GrpCol_dic[set])
				switchBool_arr = fill(false,length(setNumbers_arr))
				for i in 2:length(setNumbers_arr)
					if setNumbers_arr[i-1] >= setNumbers_arr[i]
						switchBool_arr[i] = true
					end
				end
				switchInt_arr = findall(switchBool_arr)
				z = 0
				for (idx,k) in enumerate(GrpCol_dic[set])
					if idx in switchInt_arr
						z = z+1
					end
					if z == 0
						newCol_dic[Symbol(join([set,k...],"_"))] = Symbol(set,"_",k[1])
					else
						newCol_dic[Symbol(join([set,k...],"_"))] = Symbol(set,"_",letters_arr[z],"_",k[1])
					end
				end

				DataFrames.rename!(ReadIn_df,newCol_dic)
			elseif length(uniLen_arr) > 2
				push!(Report_df,(3,:par,Symbol(FileName_str),"column for set $(set) contains more than one _"))
				continue
			end
		end
	end

    return ReadIn_df
end

# XXX filters all columns of dataframe that are related to the sets
function filterSetColumns(Input_df::DataFrame,Input_arr::Array{Symbol},OutStr_boo::Bool = false)
    ColNames_arr = [String(names(Input_df)[i]) for i = 1:size(Input_df,2)]

    # filters columns that relate to input array and further splits them based on "_" seperators
    InRelColNames_arr = collect(Iterators.flatten(map(y -> filter(x -> x[1:minimum([length(y),length(x)])] == y, ColNames_arr),map(x-> string(x),Input_arr))))
    # columns that relate to input because of numbering or identity
    CtrColNames_arr = vcat(filter(x -> isa(tryparse(Int,x[end:end]),Int),InRelColNames_arr),intersect(ColNames_arr,map(x -> String(x),Input_arr)))
    # remaining columns, solely used to filter mapping set files
    MapColNames_arr = setdiff(InRelColNames_arr,CtrColNames_arr)
    Return_arr = [CtrColNames_arr, MapColNames_arr]

    # convert to symbol unless optional input is set to true
    if !OutStr_boo Return_arr = map(y -> map(x -> Symbol(x),y),Return_arr) end
    return Return_arr
end

# XXX reads-in parameter data for respective sheet
function writeParameter(ParData_df::DataFrame, Set_dic::Dict{Symbol,DataFrame}, Para_dic::Dict{Symbol,DataFrame}, SetLongShort_dic::Dict{Symbol,Symbol}, FileName_str::String, SaveLookup_dic::Union{Nothing,Dict{Symbol,Dict{Tuple,Array}}}=nothing)

    SetShortLong_dic = Dict(value => key for (key, value) in SetLongShort_dic)
    Set_arr = vcat(collect(SetShortLong_dic[key] for key in keys(Set_dic))...,:id)
    SetNames_arr = filterSetColumns(ParData_df,Set_arr)[1]

	# creates array of all levels provided per set grouped by set
	SetIndex_arr = map(setdiff(SetNames_arr,Set_arr)) do x
		splitSet = split(String(x),"_")
		return length(splitSet) == 1 ? (Symbol(splitSet[1]),1) : (Symbol(join(splitSet[1:end-1],"_")),parse(Int,splitSet[end]))
	end

	SetIndex_arr = map(y -> (y,map(w -> w[2],filter(z -> z[1] == y,SetIndex_arr))),unique(map(x -> x[1],SetIndex_arr)))

	# creates array that is later edited to lookup and save set values, entries: set, blank for set values, levels, start level
	SetIni_arr = [[z[1],initializeLookup(maximum(z[2])-minimum(z[2])+1),z[2],minimum(z[2])] for z in SetIndex_arr]
    # creates special entry for sets with only one level, because they do not need to have a number at the end
    for i in intersect(Set_arr,SetNames_arr) push!(SetIni_arr,[i,Union{Bool, String}[false],false,1]) end

	# throws error, if column level exceeds level of the respective set used
	for ele in SetIndex_arr
		set = Symbol(split(string(ele[1]),"_")[1]) # extracts just the actual set name, if it has a letter in the end, because set is used multiple times
		if set != :id && maximum(Set_dic[SetLongShort_dic[set]][:,:lvl]) < maximum(ele[2])
			push!(Report_df,(2,:par,Symbol(FileName_str),"columns provided for $(ele[1]) exceed level of definiton, parameter input ignored"))
			return SaveLookup_dic, Para_dic
		end
	end

    # assigns relevant columns to sets
    SetCol_dic = Dict{Symbol,Array}()
    for i in SetIni_arr
		if i[3] == false
			SetCol_dic[i[1]] = [i[1]]; i[3] = 1
		else
			SetCol_dic[i[1]] = [Symbol(i[1],"_",j) for j in i[3]]
		end
	end

    # determines relvant parameter/value columns
    OprNames_arr = filterSetColumns(ParData_df,[:parameter,:variable,:value],true)[1]
    OprLvl_arr = filter(x -> x != nothing,map(x -> tryparse(Int16,x[end:end]),OprNames_arr))
    ParVal_arr = isempty(OprLvl_arr) ? [[:parameter,:value]] : [[Symbol("parameter_",j),Symbol("value_",j)] for j in unique(OprLvl_arr)]

    # converts parameter columns to symbols
    for i in ParVal_arr ParData_df[!,i[1]] = map(x -> Symbol(x),ParData_df[!,i[1]]) end

    # loop over rows to read respective parameter values
    for row in eachrow(ParData_df)

		SetId_dic = Dict{Symbol,Any}()

        # XXX obtains node ids for row
        # overwrites default values for specific row in SetIni_arr
        for i in SetIni_arr, (index,j) in enumerate(i[3])
			i[2][j+1-i[4]] = row[SetCol_dic[i[1]][index]]
        end

    	# extracts specific set values and looks them up to obtain the respective node ids, in case of an id column just directly writes value
        relSets_arr = filter(x ->!all(("" .== x[2]) .| (false .== x[2])),SetIni_arr)
        for ele in relSets_arr
			if ele[1] == :id
				SetId_dic[:id] = parse(Int16,ele[2][1])
			else
				split_arr = split(string(ele[1]),"_")
	            SetShort_sym = SetLongShort_dic[Symbol(split_arr[1])]
				SaveDic_sym = length(split_arr) == 1 ? SetShort_sym : Symbol(SetShort_sym,"_",split_arr[2])
	            SetId_dic[SaveDic_sym], SaveLookup_dic[SetShort_sym]  = lookupTupleTree(ele[2],Set_dic[SetShort_sym],SaveLookup_dic[SetShort_sym],ele[4])
			end
        end

        # goes to next iteration and writes report, if any set used is undefined
        if false in values(SetId_dic)
            undefinedDim_arr = map(filter(x -> SetId_dic[x] == false,collect(keys(SetId_dic)))) do x
				split_arr = split(String(x),"_")
				setName = SetShortLong_dic[Symbol(split_arr[1])]
				return length(split_arr) == 1 ? setName : Symbol(setName,"_",split_arr[2])
			end
            undefinedSets_arr = map(y -> join(map(z -> string(y[2][z]," (lvl ",y[3][z],")") ,1:length(y[3]))," > "),filter(x -> x[1] in undefinedDim_arr,relSets_arr))
			for undef in undefinedSets_arr
            	push!(Report_df,(2,:par,Symbol(FileName_str),"values provided for undefined set $(undef...)"))
			end
            continue
        end

        # creates all possible combinations of found values
        AgnNodes_mat = []
        for i in Iterators.product(collect(values(SetId_dic))...) push!(AgnNodes_mat,collect(i)) end
        OrdAgnNodes_ord = hcat(AgnNodes_mat...)

        AddEntry_df = DataFrame()

        for (index,y) in enumerate(keys(SetId_dic)) AddEntry_df[!,y] = OrdAgnNodes_ord[index,:] end

        # XXX loop over parameter/value columns, prepares and writes
        for i in ParVal_arr

            # extract parameter type and value
            par_sym = row[i[1]]
			if par_sym == Symbol()
				push!(Report_df,(2,:par,Symbol(FileName_str),"empty value in parameter column detected"))
				continue
			end
            AddEntry_df[!,:val] .= parse(Float64,row[i[2]])

            # creates empty dataframe for parameter, if non-existent so far
            if !in(par_sym,keys(Para_dic)) Para_dic[par_sym] = DataFrame(val = Float64[]) end

            # adds 0 to dictionary for sets the parameter depends on, but that dont appear in the current file/row
            for missKey in setdiff(names(Para_dic[par_sym]),names(AddEntry_df)) AddEntry_df[!,missKey] .= 0 end

        	# adds new column to dataframe for respective parameter if required
        	Rows_int = nrow(Para_dic[par_sym])
        	for key in setdiff(names(AddEntry_df),names(Para_dic[par_sym]))
        		Para_dic[par_sym][!,key] = zeros(Int16, Rows_int) #Array{Int16}(undef,Rows_int) # needs symbol
        	end

            permutecols!(AddEntry_df, names(Para_dic[par_sym]))
            append!(Para_dic[par_sym],AddEntry_df)
        end
    end

    return SaveLookup_dic, Para_dic
end

# </editor-fold>


# <editor-fold desc="converting set data provided to tree"

# XXX creates tree orientated dataframe for sets
function createTree(ReadIn_df::DataFrame, SetLoad_sym::Symbol)
    SetLoad_str = string(SetLoad_sym)
    # create relevant objects
    Tree_df = DataFrame(idx = Int16[], val = String[], lvl = Int16[], pare = Int16[], sub_id = Int16[], children = Array{Int16,1}[])

    # writes values of first column
    FirstCol_sym = Symbol(SetLoad_str,"_1")
    TopNodes_arr =  filter(x -> !isempty(x),convert(Matrix,unique(ReadIn_df[:,names(ReadIn_df) .== FirstCol_sym])))

    for (index, node) in enumerate(sort(TopNodes_arr))
        push!(Tree_df, (index, node,1,0,index,[]))
    end

    # iteration to read subsequent levels
    MaxLvl_int = maximum((map(x -> parse(Int,x[end]), filter(x-> (tryparse(Int,string(x[end])) != nothing) && x[1:minimum([length(x),length(SetLoad_str)])] .== SetLoad_str,[String(names(ReadIn_df)[i]) for i = 1:size(ReadIn_df,2)]))))

    for i in 2:MaxLvl_int
        Tree_df =  createTreeLevel(ReadIn_df, Tree_df, SetLoad_str, i)
    end

    return Tree_df
end

function createTreeLevel(ReadIn_df::DataFrame, Tree_df::DataFrame, SetLoad_str::String, i::Integer)

    ColNames_arr = names(ReadIn_df)
    LoLvl_Sym = Symbol(SetLoad_str,"_",i)

    # provides the nodes of the lower level grouped by the upper nodes
    LowerNodes_gdf = DataFrames.groupby(unique(ReadIn_df[(ReadIn_df[!,LoLvl_Sym] .!= ""),filter(x -> x in ColNames_arr[1:i], ColNames_arr)]), filter(x -> x in ColNames_arr[1:(i-1)], ColNames_arr))

    # assigns the upper nodes by id to strings of corresponding lower nodes
    UpToLow_dic = Dict(lookupTupleTree([lowerNode[1,ColNames_arr[j]] for j = 1:i-1], Tree_df)[1] =>  lowerNode[!,LoLvl_Sym] for lowerNode in LowerNodes_gdf)
	# deletes false from dic
	for del in filter(x -> x == false, keys(UpToLow_dic)) delete!(UpToLow_dic,del) end

    # iterates over dict to write new nodes into tree
    for upperNodeId in deepSort(convert(Array{Int64,1},collect(keys(UpToLow_dic))),Tree_df)
        NumRow_Int = nrow(Tree_df)
		index::Int = 0
        for (index, lowerNode) in enumerate(UpToLow_dic[upperNodeId])
            push!(Tree_df, ((NumRow_Int + index), lowerNode, i, upperNodeId, index,[]))
        end
		Tree_df[upperNodeId,:children] = Array((NumRow_Int+1):(NumRow_Int+length(UpToLow_dic[upperNodeId])))
    end

    return Tree_df

    # TODO mögliche fehler: namen am knoten nicht unique, leere felder, wo keine sein dürfen, fehlerhafte spalten benennung
end

# </editor-fold>
