# <editor-fold desc="navigating through trees of sets"
# XXX finds provided string tuple in tree structure and returns node id (or false)
function lookupTupleTree(Input_arr::Union{Array,String},Tree_df::DataFrame, SaveLookupSub_dic::Union{Nothing,Dict{Tuple,Array}}=nothing,StartLvl_int::Integer= 1,writeDic::Bool = true)

	if isempty(Tree_df) return false, SaveLookupSub_dic end
    if isa(Input_arr,String) Input_arr = [Input_arr] end

    UseLookup_boo = !isnothing(SaveLookupSub_dic)
    Lookup_tup = Tuple([StartLvl_int,Input_arr...])

    # immediately returns value, if lookup dictionary is used and value is provided
    if UseLookup_boo && Lookup_tup in keys(SaveLookupSub_dic)
        Upper_id = SaveLookupSub_dic[Lookup_tup]
        writeDic ? (return Upper_id, SaveLookupSub_dic) : (return Upper_id)
    end

    # avoid loops if input has just one dimension (= corresponds to one level)
    if isa(Input_arr,String) || length(Input_arr) == 1
        Upper_id = Tree_df[(Tree_df[!,:val].==Input_arr[1]) .& (Tree_df[!,:lvl].==StartLvl_int),:idx]
        if isempty(Upper_id) Upper_id = false end
        (UseLookup_boo && writeDic) ? (if Upper_id != false SaveLookupSub_dic[Lookup_tup] = Upper_id end; return Upper_id, SaveLookupSub_dic) : (return Upper_id)
    end

    # finds relevant values and levels for loop over multiple levels
    SearchVal_tup = Tuple(filter(x -> !isempty(x),Input_arr))
    SearchLvl_tup = Tuple(findall(Input_arr .!= "")) .+ (StartLvl_int - 1)

    Upper_id = Int32[]

    # loops over subsequent levels
    for (index,i) in enumerate(SearchLvl_tup)

        # looks for subsequent level in saved lookups too
        if UseLookup_boo && (i,SearchVal_tup[index]) in keys(SaveLookupSub_dic)
            sub_df = Tree_df[SaveLookupSub_dic[(i,SearchVal_tup[index])],[:pare,:idx]]
            Upper_id = index > 1 ? sub_df[BitArray([j in Upper_id for j in sub_df[!,:pare]]),:idx] : sub_df[!,:idx]
        else
			if SearchVal_tup[index] == false # skips to next level in tree if value on an intermediate level was not specified
				Upper_id = Tree_df[(if index > 1  BitArray([j in Upper_id for j in Tree_df[:pare]]) else true end) .& (Tree_df[:lvl].==i),:idx]
			else
				SubTree =  Tree_df[findall(Tree_df[!,:lvl] .== i),:]
				Upper_id = SubTree[(if index > 1  BitArray([j in Upper_id for j in SubTree[!,:pare]]) else true end) .& (SubTree[!,:val] .== SearchVal_tup[index]),:idx]
			end
        end
        if isempty(Upper_id) Upper_id = false; break end
    end
    (UseLookup_boo && writeDic) ? (if Upper_id != false SaveLookupSub_dic[Lookup_tup] = Upper_id end; return Upper_id, SaveLookupSub_dic) : (return Upper_id)
end

# XXX initializes dictionary that saves lookups in tree
function initializeLookup(size_int::Integer)
    Ini_arr = Array{Union{Bool,String}}(undef,size_int)
    Ini_arr .= false
    return Ini_arr
end

# XXX get all children (and children) until end level, if specified, gives all children or just on last level depending on getAll
function getChildren(StartNode_int::Integer,Tree_df::DataFrame,getAll::Bool = false, limitLvl_int::Union{Nothing,Integer}=nothing)

	# determines starting point
	if StartNode_int == 0
		startLvl_int = 0
		startIdx_arr = Tree_df[Tree_df[:,:lvl] .== 1, :idx]
	else
		startLvl_int = Tree_df[StartNode_int,:lvl]
		if startLvl_int == limitLvl_int return StartNode_int end
		startIdx_arr = Tree_df[StartNode_int,:children]
	end
	curLvl_int = startLvl_int

	# initialize array of all children
	if getAll allIdx_arr = startIdx_arr end

	# sets limits to maximum value if nothing provided
	if isnothing(limitLvl_int) limitLvl_int = maximum(Tree_df[:,:lvl]) end

	while curLvl_int < (limitLvl_int-1)
		lookUp_arr = vcat(map(x -> Tree_df[x,:children],startIdx_arr)...)
		if isempty(lookUp_arr)
			#break;
		else
			startIdx_arr = lookUp_arr
			if getAll allIdx_arr = vcat(allIdx_arr,startIdx_arr) end
		end
		curLvl_int = curLvl_int + 1
	end

	return getAll ? allIdx_arr : startIdx_arr
end

# XXX gives all parents (id, level) combination, if node is already on top/bottom level returns itself, if limitLvl_int is set only provide parent on that level
function getHeritanceLine(StartNode_int::Integer,Tree_df::DataFrame,limitLvl_int::Union{Nothing,Integer}=nothing)
	ParLvl_arr = [[Tree_df[Tree_df[:,:idx] .== StartNode_int,:pare][1], 0]]
    if ParLvl_arr[1] == [0,0] isnothing(limitLvl_int) ? (return [[StartNode_int,1]]) : (return StartNode_int) end
    while ParLvl_arr[end][1] != 0
        loop = Tree_df[Tree_df[!,:idx] .== ParLvl_arr[end][1],[:pare,:lvl]]
        ParLvl_arr[end][2] = loop[!,:lvl][1]
		if ParLvl_arr[end][2] == limitLvl_int
			return ParLvl_arr[end][1]
		end
        push!(ParLvl_arr,[loop[!,:pare][1],0])
    end
	return ParLvl_arr[1:end-1]
end

# XXX sorts inputs nodes according to their tree position
function deepSort(NodesIndex_arr::Array{Int64,1},Tree_df::DataFrame)
    HertiLine_mat = map(x -> getHeritanceLine(x, Tree_df),NodesIndex_arr)

    RowNum_int = length(NodesIndex_arr)
    ColNum_int = maximum([HertiLine_mat[i][1][2] for i = 1:RowNum_int])

    Herti_mat = zeros(Int64, RowNum_int, ColNum_int)

    for (row, row_arr) in enumerate(HertiLine_mat)
        for ele in row_arr
            Herti_mat[row,ele[2]] = ele[1]
        end
    end

    Order_mat = sortslices(hcat(NodesIndex_arr,Herti_mat), dims=1, by = x-> x[2:end])

    return Order_mat[:,1]
end
# </editor-fold>

# <editor-fold desc="table related data processing"
# XXX adds a dummy named column of nothing to a table to avoid a bug when joining on all columns of the table
function addDummyCol(inputTab::IndexedTable)
	return IT.transform(inputTab,:dummy => fill(nothing,length(inputTab)))
end

# XXX removes dummy column created to avoid join bug again
function rmvDummyCol(inputTab::IndexedTable)
	return DB.select(inputTab,DB.Not(:dummy))
end

# XXX performs a left or outer join operation and replaces any missing values that accure with missVal_any
function joinMissing(leftData_tab::IndexedTable, rightData_tab::IndexedTable, leftKey_tup::Tuple, rightKey_tup::Tuple, how_sym::Symbol, missVal_tup::Tuple)

	# adds dummy columns to avoid join bug in case this is necessary
	rmvDummy_boo = false
	if isempty(setdiff(colnames(leftData_tab),leftKey_tup))   leftData_tab = addDummyCol(leftData_tab);  rmvDummy_boo = true end
	if isempty(setdiff(colnames(rightData_tab),rightKey_tup)) rightData_tab = addDummyCol(rightData_tab); rmvDummy_boo = true end

	# perform join operation
    joinData_tab = DB.join(leftData_tab,rightData_tab; lkey = leftKey_tup, rkey = rightKey_tup, how = how_sym)

	# removes dummy column again
	if rmvDummy_boo joinData_tab = rmvDummyCol(joinData_tab) end

    # check, if any column contains missing values
    if !(Missing <: eltype(eltype(joinData_tab))) return joinData_tab end#
	missValCnt_int = 1

    # replace missing value, cases differ depending if data type needs to be adjusted
    for col in colnames(joinData_tab)
        colVal_arr = DB.select(joinData_tab,col)
		typeVar_typ::Union{Union,DataType,Array{DataType,1}} = eltype(typeof(colVal_arr))
        if !(Missing <: typeVar_typ) continue end
		colType_sym::DataType = filter(x -> x != Missing,collect(typeVar_typ))[1]
        joinData_tab = IT.transform(joinData_tab,col => convert(Array{colType_sym,1},map(x -> coalesce(x,missVal_tup[missValCnt_int]),colVal_arr)))
		missValCnt_int = missValCnt_int +1
    end

    return joinData_tab
end

# XXX aggregates input table among columns specified in aggCol_tup, uses dictionaries to speed this up according to grpInter_tup, also checks, if entries potAgg_tab can be defined via aggregation and if yes, return them too
# the aggregation process is speeded-up by saving the location of set combinations that were already looked up to a dictionary
# grpInter_tup provides tuples that defines for what combination of sets dictionaries are created (e.g. ((:Ts_dis, :Te), (:R_dis, :C)) saves looked up Ts_dis/Te and R_dis/C combinations to dictionaries)
# these dictionaries can also be nested indicated by a pair ( e.g. (((:Ts_inv, :Te, :Ts_dis) => (:Ts_inv, :Te))) saves looks-up for Ts_inv/Te and uses these to look-up and save Ts_inv/Te/Ts_dis
# WARNING unreasonable grpInter_tup can lead to wrong results (e.g.  (((:Ts_inv, :Te) => (:Ts_inv, :Te, :Ts_dis)))
function aggregateSetTable(inData_tab::IndexedTable, aggCol_tup::Tuple, grpInter_tup::Tuple, sets::Dict{Symbol,DataFrame}, potAgg_tab::Union{IndexedTable,Nothing})

	# <editor-fold desc="initialization of variables"
	idxChildRows_dic = Dict{Symbol,Dict{Int32,BitSet}}()
	# adds potential aggregations at the end of table if provided
	newVar_tab = nothing
	if isnothing(potAgg_tab)
		aggData_tab = DB.select(inData_tab,aggCol_tup)
	else
		aggData_tab = IT.table(vcat(rows(DB.select(inData_tab,aggCol_tup)),rows(DB.select(potAgg_tab,aggCol_tup))))
	end
	# </editor-fold>

	# <editor-fold desc="create dictionaries in each dimension that assign rows suited for aggregation to each value"
	for col in aggCol_tup
		# entries that other entries can be aggregated to
		searchCol_arr = DB.select(aggData_tab,col)
		searchVal_arr = BitSet(unique(searchCol_arr))
		# entries that can be aggregated to others => hence entries from potential aggregation are not included!
		findCol_arr = DB.select(inData_tab,col)

		setName = Symbol(split(string(col),"_")[1])

		# to every unique value in column the value itself and its children are assigned
		idxChild_dic = Dict(x => vcat(x,intersect(searchVal_arr,BitSet(getChildren(x,sets[setName],true)))...) for x in searchVal_arr)

		# to everything occuring in values, the rows where it appears are assigned
		childrenRow_dic = Dict(x => BitSet(sort(findall(y -> y == x,findCol_arr))) for x in unique(vcat(values(idxChild_dic)...)))
		# for each unique value in column the rows with children are assigned
		idxChildRows_dic[col] = Dict(x => union(map(y -> childrenRow_dic[y],idxChild_dic[x])...) for x in keys(idxChild_dic))
	end
	# </editor-fold>

	# <editor-fold desc="finds aggregation by intersecting suited rows in each dimension"
	# for speedup already computed intersection between certain combination of sets are stored within dictionaries accoring to groupings provided
	# creates dictionary based on grouping provided
	if isempty(grpInter_tup) grpInter_tup = tuple(aggCol_tup) end
	saveInter_dic = Dict(x => Dict{Tuple{Vararg{Int32}},BitSet}() for x in vcat(map(x -> mixedTupToTup(x),grpInter_tup)...))

	# creates actual lookups
	aggRow_arr = map(rows(aggData_tab)) do row
		# creates first intersection
		inter = lookupIntersect(row, grpInter_tup[1],saveInter_dic,idxChildRows_dic)
		if length(inter) == 1 return BitSet() end
		# creates remaining intersections
		for grp in grpInter_tup[2:end]
			inter = intersect(inter,lookupIntersect(row, grp,saveInter_dic,idxChildRows_dic))
			if length(inter) == 1 return BitSet() end # returns empty bitset in case rows only assigns itself
		end
  		return inter
	end

	# filters assignemnts of rows to itself
	allAgg_arr = collect(zip(collect(1:length(aggRow_arr)),aggRow_arr))
	relAgg_arr = filter(x -> !(isempty(x[2])), allAgg_arr)
	relAggRmv_dic = Dict(map(x -> (x[1],setdiff(x[2],x[1])) ,relAgg_arr))
	# </editor-fold>

	# <editor-fold desc="remove redundant values from aggregation and return results"
	# e.g. yearly aggregation would so far aggregate weekly and hourly values, but hourly values would be redundant since they are included in weekly numbers, so they are filtered here
	allKey_arr = BitSet(collect(keys(relAggRmv_dic)))
	for usedKey in allKey_arr
		rmvKey_set = intersect(allKey_arr,relAggRmv_dic[usedKey])
		if !isempty(rmvKey_set)
			relAggRmv_dic[usedKey] = setdiff(relAggRmv_dic[usedKey], Set(vcat(map(x -> collect(relAggRmv_dic[x]), collect(rmvKey_set))...)))
		end
	end

	# returns values, if no potential aggregations were provided
	if isnothing(potAgg_tab)
		return relAggRmv_dic, nothing
	else
        # filters actual aggregation to potential and adds BitSet of corresponding rows in variable table
        potAggId_arr = sort(intersect(collect(keys(relAggRmv_dic)),collect(length(inData_tab)+1:(length(inData_tab)+length(potAgg_tab)))))
        actAgg_tab = IT.transform(potAgg_tab[potAggId_arr .- length(inData_tab)],:aggVar => map(x -> relAggRmv_dic[x],potAggId_arr))
		if isempty(actAgg_tab) return relAggRmv_dic, actAgg_tab end
        # removes aggregations from potential table from output dictionary
        for y in potAggId_arr delete!(relAggRmv_dic, y) end
		return relAggRmv_dic, actAgg_tab
	end
	# </editor-fold>
end

# XXX merges all tables within input dictionary
function mergeDicTable(table_dic::Dict{Symbol,IndexedTable},outerJoin_boo::Bool=true)
	if isempty(table_dic) return nothing end
	keys_arr = collect(keys(table_dic))
	mergeTable_tab = table_dic[keys_arr[1]]
	joinCol_tup = tuple(filter(x -> !(x in keys_arr), collect(colnames(mergeTable_tab)))...)

	for restKey in keys_arr[2:end]
		if outerJoin_boo
			mergeTable_tab = DB.join(mergeTable_tab, table_dic[restKey]; lkey = joinCol_tup, rkey = joinCol_tup, how = :outer)
		else
			mergeTable_tab = merge(mergeTable_tab, table_dic[restKey])
		end
	end

	return mergeTable_tab
end

# XXX expands table columns by replacing level entry with all sets on the respective level, therefore expCol_tup assigns columns to sets
function expandSetColumns(expData_tab::IndexedTable,expCol_tup::Tuple,sets::Dict{Symbol,DataFrame},returnM::Bool = false)
	for ex in expCol_tup
		set = Symbol(split(String(ex),"_")[1])
		expData_tab = flatten(IT.transform(expData_tab,ex => DB.select(expData_tab,ex => r -> sets[set][sets[set][:,:lvl] .== r,:idx])),ex)
	end
	# returns one version with mode column flattend and one without this column at all
	if returnM
		return flatten(expData_tab,:M), DB.select(expData_tab,DB.Not(:M))
	else
		return expData_tab
	end
end

# XXX expands any table including columns with temporal and spatial dispatch levels and the corresponding investment regions and supordinate dispatch steps to full dispatch table
function expandInvestToDisp(inData_tab::IndexedTable,temp_dic::Dict{Tuple{Int32,Int32},Array{Int32,1}},reg_dic::Dict{Tuple{Int32,Int32},Int32},preserveTsSupDis::Bool = false)
    # adds regional timesteps and check if this causes non-unique values (spatial investment level can be below dispatch level)
	inReg2_tab = IT.table(unique(IT.transform(DB.select(inData_tab,DB.Not(All(:R_inv,:lvlR))),:R_dis => DB.select(inData_tab,(:R_inv, :lvlR) => x -> reg_dic[(x[1],x[2])]))))

    # adds dispatch timesteps to table and returns
	if preserveTsSupDis
		return flatten(IT.transform(DB.select(inReg2_tab,DB.Not(:lvlTs)),:Ts_dis => DB.select(inReg2_tab,(:Ts_supDis, :lvlTs) => x -> temp_dic[(x[1],x[2])])),:Ts_dis) # teuer
	else
		return DB.select(flatten(IT.transform(DB.select(inReg2_tab,DB.Not(:lvlTs)),:Ts_dis => DB.select(inReg2_tab,(:Ts_supDis, :lvlTs) => x -> temp_dic[(x[1],x[2])])),:Ts_dis),DB.Not(:Ts_supDis))
	end
end

# XXX adds scaling factor to capacity field of input table
function addScaling(add_tab::IndexedTable,capField_sym::Symbol,timeTree_df::DataFrame,supDis::NamedTuple{(:lvl,:step,:dic),Tuple{Int32,Tuple{Vararg{Int32,N} where N},Dict{Tuple{Int32,Int32},Float64}}})
    tsDisLvl_dic = Dict(x => x == 0 ? 1 : timeTree_df[x,:lvl] for x in unique(DB.select(add_tab,:Ts_dis)))
    addLvl_tab = IT.transform(add_tab,:lvl => map(x -> tsDisLvl_dic[x],DB.select(add_tab,:Ts_dis)))
	aboveSupScale_flt = maximum(values(supDis.dic)) * length(supDis.step) # scaling value used for variables above the superordinate dispatch level
	addScaled_tab = IT.transform(DB.select(addLvl_tab,DB.Not(All(:lvl,capField_sym))),capField_sym
							=> DB.select(addLvl_tab,(capField_sym,:Ts_supDis,:lvl) => x -> getproperty(x,capField_sym)* (supDis.lvl > x.lvl ? aboveSupScale_flt : supDis.dic[(x.Ts_supDis,x.lvl)])))
    return addScaled_tab
end

# XXX removes all entries occuring in remove array from input table
function removeEntries(remove_arr::Array{IndexedTable,1},input_tab::IndexedTable)
    if !isempty(remove_arr)
        remove_tab = length(remove_arr) == 1 ? remove_arr[1] : DB.merge(remove_arr...)
        colRemove_arr = colnames(input_tab)
        return rmvDummyCol(join(addDummyCol(input_tab),remove_tab; lkey = colRemove_arr, rkey = colRemove_arr, how = :anti))
    else
        return input_tab
    end
end

# XXX rename table columns according to provided Array
function renameTable(inData_tab::IndexedTable,newNames_tup::Array)
	curNames_tup = colnames(inData_tab)
	renameData_tab = DB.rename(inData_tab,curNames_tup[i] => newNames_tup[i] for i in 1:min(length(curNames_tup),length(newNames_tup)))
	return renameData_tab
end
# </editor-fold>

# <editor-fold desc="reporting of calculation progress and error handling"

# XXX return elapsed time since Start_date
function getElapsed(start::DateTime)
    ElapSec_per = Dates.value(floor(now() - start,Dates.Second(1)))
    if ElapSec_per < 3600*24
        Elap_str = Dates.format(DateTime(2015,01,01,Int(floor(ElapSec_per / 3600)),Int(floor(ElapSec_per % 3600/ 60)),ElapSec_per % 60), "HH:MM:SS")
    else
        Elap_str = Dates.format(DateTime(2015,01,Int(floor(ElapSec_per / (3600*24))),Int(floor(ElapSec_per % (3600*24) / 3600)),Int(floor(ElapSec_per % 3600/ 60)),ElapSec_per % 60), "dd:HH:MM:SS")
    end
    return Elap_str
end

# XXX teste for errors so far and optional writes report file, even if no serious errrors occured yet
function errorTest(report::DataFrame,options::modOptions,writeReporting::Bool = false)
    ErrStatus_dic = Dict(1 => :green, 2 => :yellow,3 => :red)
    if any(report.type .== 3)
        CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(report[!,2:end], 1, :errStatus => map(x -> ErrStatus_dic[x],report[!,:type])))
		printstyled(" - Errors encountered! Wrote reporting_$(options.outStamp).csv for details!"; color = :light_red)
        error()
    else
		numWarn = length(findall(report.type .== 2))
        if writeReporting && nrow(report) > 0
            CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(report[!,2:end], 1, :errStatus => map(x -> ErrStatus_dic[x],report[!,:type])))
			printstyled(" - No errors and $numWarn warnings encountered. Wrote reporting_$(options.outStamp).csv for details! \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        else
			printstyled(" - No errors and $numWarn warnings encountered. \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        end
    end
end

# XXX produces a output message and tests for errors accordingly to globally set reporting values
function produceMessage(options::modOptions,report::DataFrame,currentLvl::Int64,fixedString::String,dynamicString::Any="")
	sty_dic = Dict(1 => :bold, 2 => :normal, 3 => :light_black)

	sty_dic[currentLvl]
    if options.reportLvl >= currentLvl
		options.errCheckLvl >= currentLvl ? printstyled(options.name, getElapsed(options.startTime), fixedString, dynamicString; color = sty_dic[currentLvl]) :
															printstyled(options.name,getElapsed(options.startTime), fixedString, dynamicString, "\n"; color = sty_dic[currentLvl])
	end
    if options.errCheckLvl >= currentLvl errorTest(report,options,options.errWrtLvl >= currentLvl) end
end

# XXX plots tree graph for input set
"""
    drawNodeTree(Tree_df::DataFrame, options::modOptions; args...)
Draw a tree for all nodes provided by the set data frame and copies it to the out directory. Supported options are:
	- `rgb = (0.251,0.388,0.847)`
	    - Color of nodes.
	- `trans = 4.5`
		- Controls fading of color going further down the tree.
	- `wide = fill(1.0,maximum(Tree_df[!,:lvl]))`
		- Ratio of distances between nodes that have and do not have the same parent (separate on each level).
	- `name = "graph"`
		- Name of the output file.
	- `labelsize = 7`
		- Size of labels in graph.
"""
function drawNodeTree(Tree_df::DataFrame, options::modOptions; args...)

  	# sets options files
	defOpt_ntup = Dict(:rgb => (0.251,0.388,0.847), :trans => 4.5, :wide => fill(1.0,maximum(Tree_df[!,:lvl])), :name => "graph", :labelsize => 7)
	opt = merge(args,Dict(x => defOpt_ntup[x] for x in filter(x -> !(x in keys(args)), collect(keys(defOpt_ntup)))))

	# adds a new dummy top node
	Tree_df = Tree_df[1:end-1,:]
	nodes_int = nrow(Tree_df)+1
	idxPos_dic = Dict(zip(Tree_df[:,:idx], 1:(nodes_int-1)))

    # create vertical position and labels from input tree
    LocY_arr = append!(float(Tree_df[!,:lvl]),0)
    NodeLabel_arr = append!(copy(Tree_df[!,:val]),[""])

    # horizontal position is computed in a two step process
    LocX_arr = zeros(Float64, nodes_int)

    # first step, filter all nodes at end of a respective branch and sort them correctly
    LowLvl_df = Tree_df[isempty.(Tree_df[!,:children]),:]
    LowLvl_df = LowLvl_df[map(y -> findall(x -> x == y, LowLvl_df[:,:idx])[1],deepSort(convert(Array{Int64,1},LowLvl_df[:,:idx]),Tree_df)),:]

    # set position of starting node
    LocX_arr[LowLvl_df[1,:idx]] = 0

    # sets distance from next node on the left depending on if they are part of the same subtree
    for (index, lowNode) in Iterators.drop(enumerate(eachrow(LowLvl_df)),1)
		if lowNode[:pare] == LowLvl_df[index-1,:pare] distance_fl = opt[:wide][lowNode[:lvl]] else distance_fl = 1 end
		LocX_arr[idxPos_dic[lowNode[:idx]]] = LocX_arr[idxPos_dic[LowLvl_df[index-1,:idx]]] + distance_fl
    end

    # second step, remaining horizontal nodes are place in the middle of their children
    HighLvl_df = Tree_df[false .== isempty.(Tree_df[!,:children]),:]

    for highNode in reverse(eachrow(HighLvl_df))
		children_arr = map(x -> idxPos_dic[x] ,Tree_df[Tree_df[!,:pare] .== highNode[:idx],:idx])
		LocX_arr[idxPos_dic[highNode[:idx]]] = Statistics.mean(LocX_arr[children_arr])
    end

    LocX_arr[end] = Statistics.mean(LocX_arr[Tree_df[findall(Tree_df[:,:lvl] .== 1),:idx]])

    # draw final tree
    Tree_gra = SimpleGraph(nodes_int+1)
    for rowTree in eachrow(Tree_df)
      # 0 node in Tree_df becomes last node in graph, because there is 0 node within the plots
      if rowTree[:pare] == 0 pare_int = nodes_int else pare_int = idxPos_dic[rowTree[:pare]] end
      add_edge!(Tree_gra, idxPos_dic[rowTree[:idx]], pare_int)
    end

    color_arr = [RGB24(min(1,opt[:rgb][1]*(1+x/opt[:trans])),min(1,opt[:rgb][2]*(1+x/opt[:trans])),min(1,opt[:rgb][3]*(1+x/opt[:trans]))) for x in LocY_arr]

    # add invisible dummy node on right side to avoid text being pushed out of the frame
    push!(LocX_arr,maximum(LocX_arr)*1.1)
    push!(LocY_arr,maximum(LocY_arr)*1.2)
    push!(color_arr,RGB24(1,1,1))
    push!(NodeLabel_arr,"")

    Tree_pl = gplot(Tree_gra, LocX_arr, LocY_arr, nodelabel=NodeLabel_arr,  nodelabeldist=3, nodelabelangleoffset= π/4, NODELABELSIZE = opt[:labelsize], EDGELINEWIDTH=1, nodefillc = color_arr)

    draw(SVG("$(options.outDir)/$(opt[:name]).svg", 28cm, 16cm), Tree_pl)
end
# </editor-fold>

# <editor-fold desc="reporting of results"

# XXX compute a subset of infeasible constraints
function printIIS(anyM::anyModel)

    # computes iis
    Gurobi.compute_conflict(anyM.optModel.moi_backend.optimizer.model)

    if anyM.optModel.moi_backend.optimizer.model.inner.conflict != 0 return end
    # loops over constraint tables to find constraints within iis
    for eqnObj in values(anyM.constraints)
		if eqnObj.name == :objEqn continue end
        allConstr_arr = findall(DB.select(eqnObj.data,:eqn =>  x -> MOI.get(anyM.optModel.moi_backend, Gurobi.ConstraintConflictStatus(), x.index)))
        # prints constraints within iis
        if !isempty(allConstr_arr)
            println("$(length(allConstr_arr)) of IIS in $(eqnObj.name) constraints.")
            colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in eqnObj.dim)
            for iisConstr in allConstr_arr
                row_tup = eqnObj.data[iisConstr]
                dimStr_arr = map(x -> getproperty(row_tup,x) == 0 ?  "" : string(x,": ",createFullString(getproperty(row_tup,x), anyM.sets[colSet_dic[x]],true)),collect(keys(colSet_dic)))
                println("$(join(filter(x -> x != "",dimStr_arr),", ")), constraint: $(row_tup.eqn)")
            end
        end
    end
end

# XXX returns a structured string of set name, if index and data tree is provided
function createFullString(setIdx_int,Tree_df::DataFrame,writeLvl_boo::Bool=true)
	if setIdx_int == 0  return "entry" end
	setLvl_int = Tree_df[setIdx_int,:lvl]
	setVal_int = writeLvl_boo ? string(Tree_df[setIdx_int,:val]," (lvl ",Tree_df[setIdx_int,:lvl] ,")") : Tree_df[setIdx_int,:val]

	if setLvl_int == 1
		carStr_str = join(reverse(map(x -> writeLvl_boo ? string(Tree_df[x[1],:val]," (lvl ",Tree_df[x[1],:lvl] ,")") : Tree_df[x[1],:val],getHeritanceLine(setIdx_int,Tree_df)))," < ")
	else
		carStr_str = join(reverse(vcat(setVal_int,map(x -> writeLvl_boo ? string(Tree_df[x[1],:val]," (lvl ",Tree_df[x[1],:lvl] ,")") : Tree_df[x[1],:val],getHeritanceLine(setIdx_int,Tree_df))))," < ")
	end
	return carStr_str
end

# XXX prints any AbstractModelElement
function printObject(print_obj::AbstractModelElement,sets::Dict{Symbol,DataFrame},options::modOptions, threshold::Union{Nothing,Float64} = 0.001, filterFunc::Union{Nothing,Function} = nothing)
	# initialize
	colNam_arr = colnames(print_obj.data)
    cntCol_int = length(colNam_arr)
    print_tab = table()

	# filters values according to filter function,
	dataTable = isnothing(filterFunc) ? print_obj.data : DB.filter(filterFunc,print_obj.data)

	# converts variable column to value of variable
	if :var in colNam_arr
		dataTable = DB.select(IT.transform(dataTable,:val => map(x -> value(x), DB.select(dataTable,:var))),DB.Not(All(:var)))
	end

	# resets these to reflect changes above
	colNam_arr = colnames(dataTable)
	cntCol_int = length(colNam_arr)

	# removes entries where value is zero
	if !(isnothing(threshold)) && :val in colNam_arr dataTable = DB.filter(r -> r.val >= threshold, dataTable) end

    for i = 1:cntCol_int
        lookUp_sym = Symbol(split(String(colNam_arr[i]),"_")[1])
		if !(lookUp_sym in keys(sets))
			if lookUp_sym == :eqn
				print_tab = IT.transform(print_tab,lookUp_sym => string.(DB.select(dataTable,i)))
			else
				print_tab = IT.transform(print_tab,lookUp_sym => DB.select(dataTable,i))
			end
        else
            print_tab = IT.transform(print_tab,colNam_arr[i] => DB.select(dataTable,colNam_arr[i] => x -> x != 0 ? createFullString(x,sets[lookUp_sym]) : ""))
        end
    end

    CSV.write("$(options.outDir)/$(print_obj.name)_$(options.outStamp).csv",  print_tab)
end
# </editor-fold>

# <editor-fold desc="miscellaneous data processing"
# XXX builds intersection of rows matching the current one in cases where a subdictionary is used according to the named tuple provided, belongs to aggregateSetTable
function lookupIntersect(row::NamedTuple,grp::T,saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int32,BitSet}}) where T <: Pair
	lookup_tup = tuple(collect(getproperty(row,j) for j in grp[1])...)

	if lookup_tup in keys(saveInter_dic[grp[1]]) # take value directly from dictionary
		ele_arr = saveInter_dic[grp[1]][lookup_tup]
	else # compute value and write to dictionary
		ele_arr = intersect(lookupIntersect(row,grp[2],saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int32,BitSet}}),idxChildRows_dic[grp[1][end]][getproperty(row,grp[1][end])]) # lookup subdictionary
		saveInter_dic[grp[1]][lookup_tup] = ele_arr
	end
	return ele_arr
end

# XXX builds intersection of rows matching the current one in cases where no subdictionary is used, belongs to aggregateSetTable
function lookupIntersect(row::NamedTuple,grp::T,saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int32,BitSet}}) where T <: Tuple
	lookup_tup = tuple(collect(getproperty(row,j) for j in grp)...)

	if lookup_tup in keys(saveInter_dic[grp]) # take value directly from dictionary
		ele_arr = saveInter_dic[grp][lookup_tup]
	else  # compute value and write to dictionary
		ele_arr = intersect([idxChildRows_dic[j][getproperty(row,j)] for j in grp]...)
		saveInter_dic[grp][lookup_tup] = ele_arr
	end
	return ele_arr
end

# XXX creates a dictionary that assigns each dispatch timestep inputed to its supordinate dispatch timestep
function assignSupDis(inputSteps_arr::Array{Int32,1},timeTree_df::DataFrame,supordinateLvl_int::Int32)

	assSup_dic = Dict{Int32,Int32}()

	# assigns zero node to itself
	if 0 in inputSteps_arr
		assSup_dic[0] = 0
		inputSteps_arr = filter(r -> r != 0, inputSteps_arr)
	end

	# assigns entries on or above subordinate dispatch level to itselfs
	aboveSupDis_arr = filter(z -> timeTree_df[z,:lvl] <= supordinateLvl_int, inputSteps_arr)
	for x in aboveSupDis_arr assSup_dic[x] = x end
	inputSteps_arr = setdiff(inputSteps_arr,aboveSupDis_arr)

	# assigns remaining entries
	for x in inputSteps_arr
		assSup_dic[x] = getHeritanceLine(x,timeTree_df,supordinateLvl_int)[1][1]
	end

	return assSup_dic
end

# XXX writes all tuples occuring in a tuple of pairs and tuples
mixedTupToTup(x) = typeof(x) <: Pair ? map(y -> mixedTupToTup(y),collect(x)) :  x

# XXX returns dataframe columns without value column
removeVal(input_df::DataFrame) = filter(x -> x != :val,names(input_df))

# XXX filters variables defined for aggregation
filterAgg(varTab::IndexedTable,aggIdx::Array{Int64,1}) = varTab[setdiff(1:length(varTab),aggIdx)]

# XXX returns all types within union type statement (taken from https://github.com/JuliaLang/julia/issues/14695#issuecomment-377662648)
Base.collect(t::Union{Type, DataType, Union{}}) = _collect(t, [])
_collect(t::Type, list) = t<:Union{} ? push!(list, t) : _collect(t.b, push!(list, t.a))
_collect(t::Union{DataType,Core.TypeofBottom}, list) = push!(list, t)
# </editor-fold>
