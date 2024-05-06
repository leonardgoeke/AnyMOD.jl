
#region # * top-level and general read-in functions for sets and parameter

# ! read-in all set files
function readSets!(files_dic::Dict{String,Array{String,1}}, anyM::anyModel)

	# creates relevant sets, manually adds :mode and assigns short names
	set_arr = append!(map(x ->  Symbol(x[findfirst("set_", x)[1]+4:end-4]), files_dic["set"]), [:mode, :id])
	setLngShrt_dic = Dict(:timestep => :Ts, :region => :R, :carrier => :C, :technology => :Te, :exchange => :Exc, :mode => :M, :id => :id, :scenario => :scr)

	setData_dic = Dict{Symbol,DataFrame}()
	anyM.sets = Dict{Symbol,Tree}()

	# loop over sets read-in data and create tree objects
	for setFile in files_dic["set"]
		setLong_sym = getindex(setFile[findfirst("set_", setFile)[1]+4:end-4] |> (y -> filter(x -> occursin(string(x), y), collectKeys(keys(setLngShrt_dic)))), 1)
		setShort_sym = get!(setLngShrt_dic, setLong_sym, setLong_sym)

		# check for erros
		if setShort_sym in keys(anyM.sets)
			push!(anyM.report, (3, "set read-in", string(setLong_sym), "multiple input files provided for set"))
		end
		setData_dic[setShort_sym] = convertReadIn(CSV.read(setFile, DataFrame ;delim = anyM.options.csvDelim[1]), setFile, set_arr, setLngShrt_dic, anyM.report, anyM.lock)

		if isempty(setData_dic[setShort_sym])
			push!(anyM.report, (2, "set read-in", string(setLong_sym), "detected an empty set file, check your node definition (a common mistake is to do not define any specific nodes on the first level)"))
			continue
		end

		if setShort_sym == :scr
			# exception for scenario file, checks column naming and manipulates tree
			if namesSym(setData_dic[setShort_sym]) != [:scenario]
				push!(anyM.report, (3, "set read-in", string(setLong_sym), "only one column named 'scenario' allowed in set file defining scenarios"))
			else
				setData_dic[setShort_sym] = rename(setData_dic[setShort_sym], :scenario => :scenario_1)
			end
		else
			# reporting on wrong column naming
			colNames_arr = map(x -> x[end], split.(names(setData_dic[setShort_sym]), "_"))
			if filter(x -> !isnothing(x), tryparse.(Int, colNames_arr)) |> (y -> y != collect(1:maximum(y)))
				push!(anyM.report, (3, "set read-in", string(setLong_sym), "wrong column naming detected, columns to define set have to be named: " * join(map(x -> string(setLong_sym, "_", x), 1:2), ", ") * "..."))
				continue
			end
		end

		# write tree
		anyM.sets[setShort_sym] = createTree(setData_dic[setShort_sym], setLong_sym, anyM.report)

	    produceMessage(anyM.options, anyM.report, 3, " - Read-in set file: ", setFile)
	end

	# manually adds mode set, creates a top node for all modes to allow for aggregation later
	if :mode in namesSym(setData_dic[:Te])
		modes_df = DataFrame(mode_1 = vcat(map(y -> split(replace(y, " " => ""), ";"), filter(x -> x != "", setData_dic[:Te][!,:mode]))...))
	else
		modes_df = DataFrame(mode_1 = String[])
	end
	anyM.sets[:M] = createTree(modes_df, :mode, anyM.report)

	# creates nodes for scenarios to allow for aggregation later, if none were defined before
	if !(:scr in collectKeys(keys(setData_dic)))
		anyM.sets[:scr] = createTree(DataFrame(scenario_1 = String[]), :scenario, anyM.report)
	end

	# manually adds id set, creates a top node for all modes to allow for aggregation later
	anyM.sets[:id] = createTree(DataFrame(id_1 = string.(1:20)), :id, anyM.report)

	# reports, if reserved chars were used in set names, if a required set was not defined, or if a non-unique carrier or technology names was defined
	for set in filter(x -> !(x in (:mode, :id, :scenario, :exchange)), collectKeys(keys(setLngShrt_dic)))
		if !(setLngShrt_dic[set] in keys(setData_dic))
			push!(anyM.report, (3, "set read-in", string(set), "no file provided to define set"))
		else
			# reports reserved chars
			strSet_arr = getfield.(values(anyM.sets[setLngShrt_dic[set]].nodes), :val)
			if any(occursin.(")", strSet_arr)) push!(anyM.report, (3, "set read-in", string(set), "reserved character ')' in set name detected")) end
			if any(occursin.("(", strSet_arr)) push!(anyM.report, (3, "set read-in", string(set), "reserved character '(' in set name detected")) end
			if any(occursin.(";", strSet_arr)) push!(anyM.report, (3, "set read-in", string(set), "reserved character ';' in set name detected")) end

			# reports error if carrier names are non-unique
			if set == :carrier || set == :technology || set == :exchange
				if length(strSet_arr) != length(unique(strSet_arr))
					push!(anyM.report, (3, "set read-in", string(set), "non-unique $set names detected"))
				end
			end
		end
	end

	produceMessage(anyM.options, anyM.report, 2, " - Read-in all set files")
	return setData_dic
end

# ! read-in all parameter files, create model parts and assign parameter
function readParameters!(files_dic::Dict{String,Array{String,1}}, setData_dic::Dict{Symbol,DataFrame}, anyM::anyModel)
	# ! read-in parameters (do not add to parts yet)
	paraTemp_dic = Dict{String, Dict{Symbol, DataFrame}}()

	# creates relevant sets, manually adds :mode and assigns short names
	set_arr = append!(map(x ->  Symbol(x[findfirst("set_", x)[1]+4:end-4]), files_dic["set"]), [:mode, :id])
	setLngShrt_dic = Dict(:timestep => :Ts, :region => :R, :carrier => :C, :technology => :Te, :exchange => :Exc, :mode => :M, :id => :id, :scenario => :scr)

	# read-in parameter files and convert their content
	@threads for parFile in files_dic["par"]
		cont_boo = false
		parData_df = convertReadIn(CSV.read(parFile, DataFrame;delim = anyM.options.csvDelim[1]), parFile, set_arr, setLngShrt_dic, anyM.report, anyM.lock, anyM.sets)
		lock(anyM.lock)
		if isempty(parData_df) || any(getindex.(anyM.report, 1) .== 3) cont_boo = true end
		unlock(anyM.lock)
		if cont_boo continue end
		para_obj =  writeParameter(parData_df, anyM.sets, setLngShrt_dic, parFile, anyM.report, anyM.lock)
		lock(anyM.lock)
		paraTemp_dic[parFile] = para_obj
		unlock(anyM.lock)
		produceMessage(anyM.options, anyM.report, 3, " - Read-in parameter file: ", parFile)
	end
	
	produceMessage(anyM.options, anyM.report, 2, " - Read-in all parameter files")
	return paraTemp_dic
end

# ! read inputs folders for all 'csv' or 'jl' files starting with 'set', 'par', 'var' and 'eqn'
function readInputFolder(inputFolders::Array{String,1}, files_dic::Dict{String,Array{String,1}} = Dict(b => String[] for b in ("set", "par", "var", "eqn")))
	# loops over main folders provides in constructor
    for folder in inputFolders
	    for file in readdir(folder)
	        if occursin(".", file) fileType_str = file[findfirst(".", file)[1]+1:end] else fileType_str = "" end
	        fileGrp_str = file[1:min(3, length(file))]
	        fileDir_str = string(folder, "/", file)
	        # loops over subfolders, if any exist
	        if (fileType_str == "csv" && fileGrp_str in ("set", "par", "var", "eqn"))
	            files_dic[fileGrp_str] = push!(files_dic[fileGrp_str], fileDir_str)
	        elseif !isfile(fileDir_str)
	            files_dic = readInputFolder([fileDir_str], files_dic)
	        end
	    end
	end

    return files_dic
end

# ! filters missing and adjusts data according to "all" statements
function convertReadIn(readIn_df::DataFrame, fileName_str::String, set_arr::Array{Symbol}, setLngShrt_dic::Dict{Symbol,Symbol}, report::Array{Tuple,1}, lock_::ReentrantLock, sets::Dict{Symbol,Tree} = Dict{Symbol,Tree}())

	strTypes_arr = [String, String1, String3, String7, String15, String31, String63, String127, String255]

	# reports if scenario column exists but no scenarios were defined
	if :scenario in namesSym(readIn_df) && !(:scenario in set_arr)
		# get all non-scenario set columns 
		relCol_arr = filter(x -> !(occursin("parameter", x) || occursin("value", x) || x == "scenario"), names(readIn_df))
		# only use value for one scenario		
		sort!(readIn_df, [:scenario])
		readIn_df = combine(x -> x[1,:], groupby(readIn_df, relCol_arr))
		push!(report, (2, "parameter read-in", "definition", "scenario column provided in '$(fileName_str)', but scenarios were not defined in a set file, only used values for smallest scenario"))
	end

	setNames_arr = filterSetColumns(readIn_df, set_arr)
    oprNames_arr = filterSetColumns(readIn_df, [:parameter, :variable, :value])
	readInColAll_tup = tuple(namesSym(readIn_df)...)

	# drop irrelevant column that do not relate to a set or an operator or are completely empty
	select!(readIn_df, Not(setdiff(readInColAll_tup, vcat(setNames_arr[1], setNames_arr[2], oprNames_arr[1]))))
	emptyCol_arr = filter(x -> eltype(readIn_df[!,x]) == Missing, namesSym(readIn_df))
	setNames_arr[1] = setdiff(setNames_arr[1], emptyCol_arr)
	select!(readIn_df, Not(emptyCol_arr))

	# filter value columns
	readInCol_arr = namesSym(readIn_df)
	valCol_arr = filter(x -> occursin("value", string(x)), readInCol_arr)

	# ! convert missing values and change array container type for editing later
	for j in 1:size(readIn_df,2)
		col = collect(readIn_df[!,j])

		if eltype(col) >: Int
			col = replace(string.(col), "missing" => "")
			if readInCol_arr[j] in valCol_arr
				replace!(col, "" => "NaN")
				col = parse.(Float64, col)
			end
			readIn_df[!,j] = col
		elseif eltype(col) >: Missing
			str_type = typeintersect(eltype(col), Union{String, String1, String3, String7, String15, String31, String63, String127, String255})
			act_type = any(eltype(col) .>: strTypes_arr) ? str_type : Float64
			# convert remaining columns to strings and replace 'missing' with empty string
			col[findall(ismissing.(col))] .= act_type == str_type ? "" : NaN
			readIn_df[!,j] = convert(Array{act_type,1}, col)
		else
			readIn_df[!,j] = col
		end
	end

	# ! check types of columns
	if !isempty(valCol_arr) && any(map(x -> x in strTypes_arr, map(x -> eltype(readIn_df[!,x]),  findall(map(x -> x in valCol_arr, readInCol_arr)))))
		lock(lock_)
		push!(report, (3, "parameter read-in", fileName_str, "detected strings in value column, file was not read-in"))
		unlock(lock_)
		return DataFrame()
	end

	for supCol in findall(.!map(x -> x in strTypes_arr, eltype.(eachcol(readIn_df))))
		if !occursin("value", string(readInCol_arr[supCol]))
			lock(lock_)
			push!(report, (3, "parameter read-in", fileName_str, "entries in '$(readInCol_arr[supCol])' could not be converted to strings (probably provided as floats), file was not read-in"))
			unlock(lock_)
			return DataFrame()
		end
	end

    # ! rewrites rows with all commands into full format
    for col in setNames_arr[1]

		colVal_arr = readIn_df[!, col]
        # check column for keywords
        rowsAll_arr = map(x -> length(x) >= 3 && lowercase(x[1:3]) == "all", colVal_arr)

        if all(!, rowsAll_arr) continue end

        # determine relevant reference for "all", if parameter are read in
        if  !isempty(sets) # take reference from readin sets
            specSet_arr = split(String(col), "_")
            relSet_obj = sets[setLngShrt_dic[Symbol(specSet_arr[1])]]
            colValUni_arr = (length(specSet_arr) == 1 ? "1" : specSet_arr[2]) |> (y -> unique(map(x -> x.val, getNodesLvl(relSet_obj, parse(Int, y)))))
        else # take reference from other column values, relevant when sets are currently read in
            colValUni_arr = sort(unique(filter(x -> !isempty(x), colVal_arr[(!).(rowsAll_arr)])))
        end

		readIn_df[!,col] = convert.(String, readIn_df[!,col])

        # loop over rows with all
        for row in eachrow(readIn_df[rowsAll_arr, :])
            # append new rows to dataframe
            addRow_df = row
            allInfo_str = reduce(replace, ["all"=>"", "("=>"", ")"=>""], init=addRow_df[col])

            if occursin(":", allInfo_str)
                allVal_arr = split(allInfo_str, ":")
                rplVal_arr = colValUni_arr[findall(x->x==allVal_arr[1], colValUni_arr)[1]:findall(x->x==allVal_arr[2], colValUni_arr)[1]]

				# reports if values within all expression could not be matched to sets
				if length(rplVal_arr) != length(allVal_arr)
					lock(lock_)
					push!(report, (2, "parameter read-in", fileName_str, "at least one value within all expression '$(allInfo_str)' could not be matched to an existing set"))
					unlock(lock_)
				end
            elseif occursin(",", allInfo_str)
                allVal_arr = split(allInfo_str, ",")
                rplVal_arr = colValUni_arr[map(x -> in(x, allVal_arr), colValUni_arr)]

				# reports if values within all expression could not be matched to sets
				if length(rplVal_arr) != length(allVal_arr)
					lock(lock_)
					push!(report, (2, "parameter read-in", fileName_str, "at least one value within all expression '$(allInfo_str)' could not be matched to an existing set"))
					unlock(lock_)
				end
            else
                rplVal_arr = colValUni_arr
            end

            for addVal in rplVal_arr
                addRow_df[col] = addVal
                push!(readIn_df, [addRow_df[col] for col in readInCol_arr])
            end
        end
        # remove inital rows with all
        deleteat!(readIn_df, findall(rowsAll_arr))
    end

	# ! convert column names if sets are defined for multiple instances (e.g. two regions in case of trade related parameters)
	if split(fileName_str, "/")[end][1:3] == "par"
		# creates a dictionary that assigns everything after the set name separated with a "_" to the respective set
		splitCol_arr = map(x -> split(String(x), "_"), setdiff(namesSym(readIn_df), oprNames_arr[1]))
		setCol_arr = unique(map(x -> Symbol(x[1]), splitCol_arr))
		grpCol_dic = Dict(x => map(z -> z[2:end], filter(y -> String(x) == y[1], splitCol_arr)) for x in setCol_arr)

		# loop over dictionary to check for irregular names and two sets having two columns assigned
		letters_arr = ("b", "c", "d", "e", "f", "g", "h", "i", "j")

		for set in keys(grpCol_dic)
			newCol_dic = Dict{Symbol,Symbol}()

			if any(map(x -> tryparse(Int, x), vcat(grpCol_dic[set]...)) .== nothing)
				lock(lock_)
				push!(report, (3, "parameter read-in", fileName_str, "column for set $(set) does not contain a number after _"))
				unlock(lock_)
				continue
			end

			# get the unique number of "_" that appear in columns assigned to the respective set
			# if the csv file has several columns of the same name the CSV read-in adds a "_1" to the second column when reading it in as a dataframe => so this checks if set is defined for multiple instances
			uniLen_arr = unique(map(x -> length(x), grpCol_dic[set]))
			if  length(uniLen_arr) == 2
				setNumbers_arr = map(x -> parse(Int, x[1]), grpCol_dic[set])
				switchBool_arr = fill(false, length(setNumbers_arr))
				for i in 2:length(setNumbers_arr)
					if setNumbers_arr[i-1] >= setNumbers_arr[i]
						switchBool_arr[i] = true
					end
				end
				switchInt_arr = findall(switchBool_arr)
				z = 0
				for (idx, k) in enumerate(grpCol_dic[set])
					if idx in switchInt_arr
						z = z+1
					end
					if z == 0
						newCol_dic[Symbol(join([set, k...], "_"))] = Symbol(set, "_", k[1])
					else
						newCol_dic[Symbol(join([set, k...], "_"))] = Symbol(set, "_", letters_arr[z], "_", k[1])
					end
				end

				rename!(readIn_df, newCol_dic)
			end
		end
	end

	# ! check if all "id" columns are validity
	if any(occursin.("id", names(readIn_df)))	
		for x in filter(x -> occursin("id", x), names(readIn_df))
			if !isempty(setdiff(readIn_df[!, Symbol(x)], vcat(string.(0:20), [""])))
				lock(lock_)
				push!(report, (3, "parameter read-in", fileName_str, "entries in id column have to be between 1 and 20"))
				unlock(lock_)
				return DataFrame()
			end
		end
	end

	return readIn_df
end

#endregion

#region # * creation of tree objects for sets

# ! creates tree object for set
function createTree(readIn_df::DataFrame, setLoad_sym::Symbol, report::Array{Tuple,1})

	setLoad_str = string(setLoad_sym)
	# create tree object and add the top node
	tree_obj = Tree()
	tree_obj.nodes[0] = Node(0, "none", 0, 1, Int[])

	# writes values of first column
	firstCol_sym = Symbol(setLoad_str, "_1")
	topNodes_arr =  filter(x -> !isempty(x), unique(hcat(map(x -> readIn_df[!,namesSym(readIn_df) .== firstCol_sym][!,x], 1:sum(namesSym(readIn_df) .== firstCol_sym))...)))

	if setLoad_sym != :id sort!(topNodes_arr) end

	for (idx, node) in enumerate(topNodes_arr)
	    tree_obj.nodes[idx] = Node(idx, node, 1, idx, Int[])
		tree_obj.srcTup[(node,)] = [idx]
		tree_obj.up[idx] = 0
	end
	tree_obj.nodes[0].down = collect(keys(tree_obj.up))

	# adds dictionary for occurrence of single strings
	for v in getNodesLvl(tree_obj, 1)
		a = v.val
		if haskey(tree_obj.srcStr, (a, 1))
			push!(tree_obj.srcStr[(a, 1)], v.idx)
		else
			tree_obj.srcStr[(a, 1)] = [v.idx]
		end
	end

	# loop over subsequent columns and add respective tree levels
	height_int = maximum((map(x -> parse(Int, x[end]), filter(x-> (tryparse(Int, string(x[end])) != nothing) && x[1:minimum([length(x), length(setLoad_str)])] .== setLoad_str, [String(namesSym(readIn_df)[i]) for i = 1:size(readIn_df, 2)]))))
	for i in 2:height_int
		createTreeLevel!(readIn_df, tree_obj, setLoad_str, i, report)
	end

	# adds max level
	tree_obj.height = height_int

    return tree_obj
end

# ! adds nodex on level i to tree object
function createTreeLevel!(readIn_df::DataFrame, tree_obj::Tree, setLoad_str::String, i::Int, report::Array{Tuple,1})
	colNames_arr = filter(x -> occursin(setLoad_str, string(x)), namesSym(readIn_df))
	loLvl_Sym = Symbol(setLoad_str, "_", i)

	# removes upper columns with empty values only
	grpCol_arr = filter(x -> x in colNames_arr[1:(i-1)], colNames_arr)
	grpIn_df = unique(readIn_df[readIn_df[!,loLvl_Sym] .!= "", filter(x -> x in colNames_arr[1:i], colNames_arr)])
	grpRel_arr = setdiff(grpCol_arr, filter(x -> [""] == unique(grpIn_df[!,x]), grpCol_arr))
	select!(grpIn_df, Not(setdiff(grpCol_arr, grpRel_arr)))

	# provides the nodes of the lower level grouped by the upper nodes
	lowerNodes_gdf = groupby(grpIn_df, grpRel_arr)

	# checks for nodes wihtout any upper node assigned
	noUp_arr = findall(map(x -> all(x .== ""), [collect(parent(lowerNodes_gdf)[i, groupcols(lowerNodes_gdf)]) for i in lowerNodes_gdf.starts]))
	up_arr = setdiff(1:length(lowerNodes_gdf), noUp_arr)
	if !isempty(noUp_arr)
		noUpVal_arr = setdiff(union(map(x -> collect(x[!,i]), collect(lowerNodes_gdf[noUp_arr]))...), union(map(x -> collect(x[!,i]), collect(lowerNodes_gdf[up_arr]))...))
		for i in noUpVal_arr
			push!(report, (3, "set read-in", setLoad_str, "node named '$(i)' could not be assigned to an upper node"))
		end
	end

	# ! assigns the upper nodes by id to strings of corresponding lower nodes
	startLvl_int = parse(Int, string(grpRel_arr[1])[end])
	upToLow_dic = Dict(lookupTupleTree(tuple(collect(lowerNode[1,grpRel_arr])...), tree_obj, startLvl_int)[1] =>  lowerNode[!,loLvl_Sym] for lowerNode in lowerNodes_gdf[up_arr])
	# ! iterates over dict to write new nodes into tree
	createNodes!(upToLow_dic, tree_obj, i)

	# adds dictionary for occurrence of single strings
	for v in getNodesLvl(tree_obj, i)
		a = v.val
		if haskey(tree_obj.srcStr, (a,i))
			push!(tree_obj.srcStr[(a,i)], v.idx)
		else
			tree_obj.srcStr[(a,i)] = [v.idx]
		end
	end
end

# ! create specific node on branch
function createNodes!(upToLow_dic::Dict, tree_obj::Tree, i::Int)
	upToLowSort_dic = Dict(map(x -> x => upToLow_dic[x], sort(collect(keys(upToLow_dic)))))
	up_arr =  sort(collect(keys(upToLowSort_dic)))
	for upperNodeId in (i == 2 ? up_arr : sortSiblings(up_arr, tree_obj))
		numRow_int = length(tree_obj.nodes) -1
		exUp_int = length(tree_obj.nodes[upperNodeId].down)
		for (idx, lowerNode) in enumerate(sort(upToLowSort_dic[upperNodeId]))
			newIdx_int = numRow_int + idx
			tree_obj.nodes[newIdx_int] = Node(newIdx_int, lowerNode, i, idx+exUp_int, Int[])
			tree_obj.up[newIdx_int] = upperNodeId

			keyStr_tup = fill("", i)
			keyStr_tup[i] = lowerNode
			foreach(x -> keyStr_tup[x[2]] = tree_obj.nodes[x[1]].val, getAncestors(newIdx_int, tree_obj, :tup, 1))

			tree_obj.srcTup[tuple(keyStr_tup...)] = [newIdx_int]
		end
		tree_obj.nodes[upperNodeId].down = union(tree_obj.nodes[upperNodeId].down, collect((numRow_int+1):(numRow_int+length(upToLowSort_dic[upperNodeId]))))
	end
end

#endregion

#region # * read-in of parameter data

# ! reads-in parameter data for respective sheet
function writeParameter(parData_df::DataFrame, sets::Dict{Symbol,Tree}, setLngShrt_dic::Dict{Symbol,Symbol}, fileName_str::String, report::Array{Tuple,1}, lock_::ReentrantLock)

	setShrtLng_dic = Dict(value => key for (key, value) in setLngShrt_dic)
    set_arr = collect(setShrtLng_dic[key] for key in keys(sets))
    setNames_arr = filterSetColumns(parData_df, set_arr)[1]
	para_dic = Dict{Symbol, DataFrame}()

	# creates array of all levels provided per set grouped by set
	setIndex_arr = map(setdiff(setNames_arr, set_arr)) do x
		splitSet = split(String(x), "_")
		return length(splitSet) == 1 ? (Symbol(splitSet[1]), 1) : (Symbol(join(splitSet[1:end-1], "_")), parse(Int, splitSet[end]))
	end

	setIndex_arr = map(y -> (y, map(w -> w[2], filter(z -> z[1] == y, setIndex_arr))), unique(map(x -> x[1], setIndex_arr)))

	# creates array that is later edited to lookup and save set values, entries: set, blank for set values, levels, start level
	setIni_arr = [parEntry(z[1], initializeLookup(maximum(z[2])-minimum(z[2])+1), z[2], minimum(z[2])) for z in setIndex_arr]
    # creates special entry for sets with only one level, because they do not need to have a number at the end
    for i in intersect(set_arr, setNames_arr) push!(setIni_arr, parEntry(i, String[""], Int[], 1)) end

	# throws error, if column level exceeds level of the respective set used
	for ele in setIndex_arr
		set = Symbol(split(string(ele[1]), "_")[1]) # extracts just the actual set name, if it has a letter in the end, because set is used multiple times
		if set != :id && sets[setLngShrt_dic[set]].height < maximum(ele[2])
			lock(lock_)
			push!(report, (2, "parameter read-in", fileName_str, "columns provided for $(ele[1]) exceed level of definition, parameter input ignored"))
			unlock(lock_)
			return para_dic
		end
	end

    # assigns relevant columns to sets
    setCol_dic = Dict{Symbol,Array}()
    for i in setIni_arr
		if isempty(i.lvl)
			setCol_dic[i.colSet] = [i.colSet]; i.lvl = [1]
		else
			setCol_dic[i.colSet] = [Symbol(i.colSet, "_", j) for j in i.lvl]
		end
	end

    # determines relevant parameter/value columns
    oprNames_arr = filterSetColumns(parData_df, [:parameter, :variable, :value], true)[1]
    oprLvl_arr = filter(x -> x != nothing, map(x -> tryparse(Int, split(x, "_")[end]), oprNames_arr))
    parVal_arr = isempty(oprLvl_arr) ? [[:parameter, :value]] : [[Symbol("parameter_", j), Symbol("value_", j)] for j in unique(oprLvl_arr)]

    # converts parameter columns to symbols
    for i in parVal_arr 
		parData_df[!,i[1]] = Symbol.(parData_df[!,i[1]])
	end

    # loop over rows to read respective parameter values
	convertParameter!(parData_df, sets, setIni_arr, parVal_arr, para_dic, setCol_dic, setLngShrt_dic, fileName_str, report, lock_)

    return para_dic
end

# ! gets idx from set names and orders all data in dataframe for respective parameter
function convertParameter!(parData_df::DataFrame, sets::Dict{Symbol,Tree}, setIni_arr::Array{parEntry,1}, parVal_arr::Array{Array{Symbol,1},1}, para_dic::Dict{Symbol,DataFrame}, setCol_dic::Dict{Symbol,Array}, setLngShrt_dic::Dict{Symbol,Symbol}, fileName_str::String, report::Array{Tuple,1}, lock_::ReentrantLock)
	setShrtLng_dic = Dict(value => key for (key, value) in setLngShrt_dic)
	for row in eachrow(parData_df)
		setId_dic = Dict{Symbol,Union{Int,Array{Int,1}}}()

		# ! obtains node ids for row
		# overwrites default values for specific row in setIni_arr
		for i in setIni_arr, (index, j) in enumerate(i.lvl)
			i.entry[j+1-i.startLvl] = row[setCol_dic[i.colSet][index]]
		end

		# extracts specific set values and looks them up to obtain the respective node ids, in case of an id column just directly writes value
		relSets_arr = filter(x ->!all(("" .== x.entry) .| (false .== x.entry)), setIni_arr)
		for ele in relSets_arr
			if ele.colSet == :id
				setId_dic[:id] = parse(Int, ele.entry[1])
			else
				split_arr = split(string(ele.colSet), "_")
				setShort_sym = setLngShrt_dic[Symbol(split_arr[1])]
				saveDic_sym = length(split_arr) == 1 ? setShort_sym : Symbol(setShort_sym, "_", split_arr[2])
				setId_dic[saveDic_sym] = lookupTupleTree(tuple(ele.entry...), sets[setShort_sym], ele.startLvl)
			end
		end
		
		# goes to next iteration and writes report, if any set used is undefined
		if Int[] in values(setId_dic)
			undefinedDim_arr = map(filter(x -> setId_dic[x] == Int[], collect(keys(setId_dic)))) do x
				split_arr = split(String(x), "_")
				setName = setShrtLng_dic[Symbol(split_arr[1])]
				return length(split_arr) == 1 ? setName : Symbol(setName, "_", split_arr[2])
			end
			undefinedSets_arr = map(y -> join(map(z -> string(y.entry[z], " (lvl ", y.lvl[z], ")") , 1:length(y.lvl)), " > "), filter(x -> x.colSet in undefinedDim_arr, relSets_arr))

			lock(lock_)
			for undef in undefinedSets_arr
				push!(report, (2, "parameter read-in", fileName_str, "values provided for undefined set '$(undef...)'"))
			end
			unlock(lock_)

			continue
		end

		# creates all possible combinations of found values
		agnNodes_mat = []
		for i in Iterators.product(collect(values(setId_dic))...) push!(agnNodes_mat, collect(i)) end
		ordAgnNodes_ord = hcat(agnNodes_mat...)

		addEntry_df = DataFrame()

		for (index, y) in enumerate(keys(setId_dic)) addEntry_df[!,y] = ordAgnNodes_ord[index,:] end

		# ! loop over parameter/value columns, prepares and writes
		for i in parVal_arr

			# extract parameter type and value
			par_sym = row[i[1]]
			if par_sym == Symbol()
				continue
			end

			# adds values to dataframe
			if isempty(addEntry_df)
				if isnan(row[i[2]]) continue end
				addEntry_df = DataFrame(val = row[i[2]])
			else
				if isnan(row[i[2]]) continue end
				addEntry_df[!,:val] .= row[i[2]]
			end

			# creates empty dataframe for parameter, if non-existent so far
			if !in(par_sym, keys(para_dic)) para_dic[par_sym] = DataFrame(val = Float64[]) end

			# adds 0 to dictionary for sets the parameter depends on, but that dont appear in the current file/row
			for missKey in setdiff(namesSym(para_dic[par_sym]), namesSym(addEntry_df)) addEntry_df[!,missKey] .= 0 end

			# adds new column to dataframe for respective parameter if required
			rows_int = nrow(para_dic[par_sym])
			for key in setdiff(namesSym(addEntry_df), namesSym(para_dic[par_sym]))
				para_dic[par_sym][!,key] = zeros(Int, rows_int)
			end

			# ! checks if any ratios were defined without an carrier
			if par_sym in (:ratioConvInUp, :ratioConvInLow, :ratioConvInFix, :ratioConvOutUp, :ratioConvOutLow, :ratioConvOutFix) && (!(:C in namesSym(addEntry_df)) || addEntry_df[1,:C] == 0)
				push!(report, (3, "parameter read-in", par_sym, "ratios was defined without specifying a carrier"))
			end

			select!(addEntry_df, namesSym(para_dic[par_sym]))
			append!(para_dic[par_sym], addEntry_df)
		end
	end
end

# ! filters all columns of dataframe that are related to the sets
function filterSetColumns(input_df::DataFrame, input_arr::Array{Symbol}, outStr_boo::Bool = false)
    colNames_arr = [String(namesSym(input_df)[i]) for i = 1:size(input_df, 2)]

    # filters columns that relate to input array and further splits them based on "_" seperators
    inRelColNames_arr = collect(Iterators.flatten(map(y -> filter(x -> x[1:minimum([length(y), length(x)])] == y, colNames_arr), map(x-> string(x), input_arr))))
    # columns that relate to input because of numbering or identity
    ctrColNames_arr = vcat(filter(x -> isa(tryparse(Int, x[end:end]), Int), inRelColNames_arr), intersect(colNames_arr, map(x -> String(x), input_arr)))
    # remaining columns, solely used to filter mapping set files
    mapColNames_arr = setdiff(inRelColNames_arr, ctrColNames_arr)
    return_arr = [ctrColNames_arr, mapColNames_arr]

    # convert to symbol unless optional input is set to true
    if !outStr_boo return_arr = map(y -> map(x -> Symbol(x), y), return_arr) end
    return return_arr
end

# ! initializes dictionary that saves lookups in tree
function initializeLookup(size_int::Int)
    Ini_arr = Array{String}(undef,size_int)
    Ini_arr .= ""
    return Ini_arr
end

#endregion
