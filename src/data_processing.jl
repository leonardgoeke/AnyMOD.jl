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

    Upper_id = Int16[]

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

# XXX initalizes dictionary that saves lookups in tree
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

# XXX aggregates input table among columns specified in aggCol_tup, uses dictionaries to speed this up according to grpInter_tup
# the aggregation process is speeded-up by saving the location of set combinations that were already looked up to a dictionary
# grpInter_tup provides tuples that defines for what combination of sets dictionaries are created (e.g. ((:Ts_dis, :Te), (:R_dis, :C)) saves looked up Ts_dis/Te and R_dis/C combinations to dictionaries)
# these dictionaries can also be nested indicated by a pair ( e.g. (((:Ts_inv, :Te, :Ts_dis) => (:Ts_inv, :Te))) saves looks-up for Ts_inv/Te and uses these to look-up and save Ts_inv/Te/Ts_dis
# WARNING unreasonable grpInter_tup can lead to wrong results (e.g.  (((:Ts_inv, :Te) => (:Ts_inv, :Te, :Ts_dis))) )
function aggregateSetTable(aggData_tab::IndexedTable,aggCol_tup::Tuple,grpInter_tup::Tuple,Set_dic::Dict{Symbol,DataFrame},rmvRedu_boo::Bool=true,selfAss_boo::Bool=true)

	# creates dictionary of dictionaries that assigns potential rows for aggregation for the respectiv dimension
	idxChildRows_dic = Dict{Symbol,Dict{Int16,BitSet}}()

	for col in colnames(aggData_tab)
		searchCol_arr = DB.select(aggData_tab,col)
		searchVal_arr = unique(searchCol_arr)

		# if column is not aggregated  children of key value appear (these are
		if col in aggCol_tup
			# if column is aggregated, occurrence of all children is assigned (and of row itself if enabled, disabling makes sense if aggregation is only done for one row)
			if selfAss_boo
				idxChild_dic = Dict(x => vcat(x,intersect(searchVal_arr,getChildren(x,Set_dic[Symbol(split(string(col),"_")[1])],true))...) for x in searchVal_arr)
			else
				idxChild_dic = Dict(x => intersect(searchVal_arr,getChildren(x,Set_dic[Symbol(split(string(col),"_")[1])],true)) for x in searchVal_arr)
			end

			idxChildRows_dic[col] = Dict(x => BitSet(sort(findall([j in idxChild_dic[x] for j in searchCol_arr]))) for x in keys(idxChild_dic))
		else
			# if column is not aggregated, only occurrence of set itself is assigned
			idxChildRows_dic[col] = Dict(x => BitSet(sort(findall(x .== searchCol_arr))) for x in searchVal_arr)
		end
	end

	# XXX searches for potential aggregations by building the intersection of all potential aggregation rows
	# for speedup already computed intersection between certain combination of sets are stored within dictionaries accoring to groupings provided

	# creates dictionary based on grouping provided
	if isempty(grpInter_tup) grpInter_tup = tuple(colnames(aggData_tab)) end
	saveInter_dic = Dict(x => Dict{Tuple{Vararg{Int16}},BitSet}() for x in vcat(map(x -> mixedTupToTup(x),grpInter_tup)...))

	# creates actual lookups
	aggRow_arr = map(rows(aggData_tab)) do row
		return intersect([lookupIntersect(row, grp,saveInter_dic,idxChildRows_dic) for grp in tuple(grpInter_tup...)]...)
	end

	# filters assignemnts of rows to itself
	allAgg_arr = collect(zip(collect(1:length(aggRow_arr)),aggRow_arr))
	relAgg_arr = filter(x -> length(x[2]) > 1, allAgg_arr)
	relAggRmv_dic = Dict(map(x -> (x[1],setdiff(x[2],x[1])) ,relAgg_arr))


	# remove redundant values from aggregation arrays
	# e.g. yearly aggregation would so far aggregate weekly and hourly values, but hourly values would be redundant since they are included in weekly numbers, so they are filtered here
	if rmvRedu_boo
		for usedKey in keys(relAggRmv_dic)
			rmvKey_set = intersect(keys(relAggRmv_dic),relAggRmv_dic[usedKey])
			if !isempty(rmvKey_set)
				relAggRmv_dic[usedKey] = setdiff(relAggRmv_dic[usedKey], Set(vcat(map(x -> collect(relAggRmv_dic[x]), collect(rmvKey_set))...)))
			end
		end
	end

	return relAggRmv_dic
end

# XXX expands table columns by replacing level entry with all sets on the respective level, therefore expCol_tup assigns columns to sets
function expandSetColumns(expData_tab::IndexedTable,expCol_tup::Tuple,Set_dic::Dict{Symbol,DataFrame},returnM::Bool = false)
	for ex in expCol_tup
		set = Symbol(split(String(ex),"_")[1])
		expData_tab = flatten(IT.transform(expData_tab,ex => DB.select(expData_tab,ex => r -> Set_dic[set][Set_dic[set][:,:lvl] .== r,:idx])),ex)
	end
	# returns one version with mode column flattend and one without this column at all
	if returnM
		return flatten(expData_tab,:M), DB.select(expData_tab,DB.Not(:M))
	else
		return expData_tab
	end
end

# XXX expands any table including columns with temporal and spatial dispatch levels and the corresponding investment regions and supordinate dispatch steps to full dispatch table
function expandInvestToDisp(inData_tab::IndexedTable,temp_dic::Dict{Tuple{Int16,Int16},Array{Int16,1}},reg_dic::Dict{Tuple{Int16,Int16},Int16},preserveTsSupDis::Bool = false)
    # adds regional timesteps and check if this causes non-unique values (spatial investment level can be below dispatch level)
	inReg_tab = table(unique(IT.transform(DB.select(inData_tab,DB.Not(All(:R_inv,:lvlR))),:R_dis => DB.select(inData_tab,(:R_inv, :lvlR) => x -> reg_dic[(x[1],x[2])]))))

    # adds dispatch timesteps to table and returns
	if preserveTsSupDis
		return flatten(IT.transform(DB.select(inReg_tab,DB.Not(:lvlTs)),:Ts_dis => DB.select(inReg_tab,(:Ts_supDis, :lvlTs) => x -> temp_dic[(x[1],x[2])])),:Ts_dis) # teuer
	else
		return DB.select(flatten(IT.transform(DB.select(inReg_tab,DB.Not(:lvlTs)),:Ts_dis => DB.select(inReg_tab,(:Ts_supDis, :lvlTs) => x -> temp_dic[(x[1],x[2])])),:Ts_dis),DB.Not(:Ts_supDis)) # teuer
	end
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
using Dates
const global Start_date = now()
const global Start_str = Dates.format(now(),"yyyymmddHHMM")

# XXX return elapsed time since Start_date
function getElapsed()
    ElapSec_per = Dates.value(floor(now() - Start_date,Dates.Second(1)))
    if ElapSec_per < 3600*24
        Elap_str = Dates.format(DateTime(2015,01,01,Int(floor(ElapSec_per / 3600)),Int(floor(ElapSec_per % 60)),ElapSec_per % 60), "HH:MM:SS")
    else
        Elap_str = Dates.format(DateTime(2015,01,Int(floor(ElapSec_per / (3600*24))),Int(floor(ElapSec_per / 3600)),Int(floor(ElapSec_per / 60)),ElapSec_per % 60), "dd:HH:MM:SS")
    end
    return Elap_str
end

# XXX teste for errors so far and optional writes report file, even if no serious errrors occured yet
function errorTest(writeReporting::Bool = false)
    ErrStatus_dic = Dict(1 => :green, 2 => :yellow,3 => :red)
    if any(Report_df.type .== 3)
        CSV.write("$OutputFolder_str/reporting_$Start_str.csv",  insertcols!(Report_df[!,2:end], 1, :errStatus => map(x -> ErrStatus_dic[x],Report_df[!,:type])))
        print(" - Errors encountered! Wrote reporting_$Start_str.csv for details!")
        error()
    else
        if writeReporting && nrow(Report_df) > 0
            CSV.write("$OutputFolder_str/reporting_$Start_str.csv",  insertcols!(Report_df[!,2:end], 1, :errStatus => map(x -> ErrStatus_dic[x],Report_df[!,:type])))
            println(" - No errors and ",length(Report_df.type .== 2)," warning(s) encountered. Wrote reporting_$Start_str.csv for details!")
        else
            println(" - No errors and ",length(Report_df.type .== 2)," warning(s) encountered.")
        end
    end
end

# XXX produces a output message and tests for errors accordingly to globally set reporting values
function produceMessage(CurrentLevel_int::Int64,fixedString::String,dynamicString::Any="")
    if ReportingLevel_int >= CurrentLevel_int ErrorCheckLevel_int >= CurrentLevel_int ? print(getElapsed(), fixedString, dynamicString) : println(getElapsed(), fixedString, dynamicString) end
    if ErrorCheckLevel_int >= CurrentLevel_int errorTest(ErrorWriteLevel_int >= CurrentLevel_int) end
end
# </editor-fold>

# <editor-fold desc="reporting of results"

# XXX compute a subset of infeasible equations
function printIIS(model_obj::JuMP.Model,Set_dic::Dict{Symbol,DataFrame},Equation_dic::Dict{Symbol,EqnElement})

    # computes iis
    Gurobi.compute_conflict(model_obj.moi_backend.optimizer.model)

    if model_obj.moi_backend.optimizer.model.inner.conflict != 0 return end
    # loops over equation tables to find equations within iis
    for eqnObj in values(Equation_dic)
        allConstr_arr = findall(DB.select(eqnObj.data,:eqn =>  x -> MOI.get(model_obj.moi_backend, Gurobi.ConstraintConflictStatus(), x.index)))
        # prints equations within iis
        if !isempty(allConstr_arr)
            println("$(length(allConstr_arr)) of IIS in $(eqnObj.name) equations.")
            colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in eqnObj.dim)
            for iisConstr in allConstr_arr
                row_tup = eqnObj.data[iisConstr]
                dimStr_arr = map(x -> getproperty(row_tup,x) == 0 ?  "" : string(x,": ",createFullString(getproperty(row_tup,x), Set_dic[colSet_dic[x]],true)),collect(keys(colSet_dic)))
                println("$(join(filter(x -> x != "",dimStr_arr),", ")), equation: $(row_tup.eqn)")
            end
        end
    end
end

# TODO konvertierung von spaltennamen besser, funktion für tabellen schöner, gucken ob schneller geht
function print_object(print_obj,Set_dic::Dict{Symbol,DataFrame},write_csv::Bool=true)
    cntCol_int = length(print_obj.dim)
    colNam_arr = colnames(print_obj.data)

    print_tab = table()

    for i = 1:cntCol_int
        lookUp_sym = Symbol(split(String(colNam_arr[i]),"_")[1])
        print_tab = IT.transform(print_tab,colNam_arr[i] => DB.select(print_obj.data,colNam_arr[i] => x -> x != 0 ? Set_dic[lookUp_sym][x,:val] : ""))
    end

    for i = cntCol_int+1:length(colNam_arr)
            print_tab = IT.transform(print_tab, colNam_arr[i] => DB.select(print_obj.data,i))
    end
    if write_csv CSV.write("$(OutputFolder_str)/$(print_obj.name).csv",  print_tab) end
    return print_tab
end

function print_object(print_obj::IndexedTable,Set_dic::Dict{Symbol,DataFrame},name::String="table",write_csv::Bool=true)
    colNam_arr = colnames(print_obj)
    cntCol_int = length(colNam_arr)

    print_tab = table()

    for i = 1:cntCol_int
        lookUp_sym = Symbol(split(String(colNam_arr[i]),"_")[1])
        if lookUp_sym in (:val,:var,:eqn,:varExp,:exp,:resi,:dispVar,:capaVar,:varIn,:varOut)
            print_tab = IT.transform(print_tab,lookUp_sym => map(x -> string('"',x,'"'), DB.select(print_obj,i)))
        else
            print_tab = IT.transform(print_tab,colNam_arr[i] => DB.select(print_obj,colNam_arr[i] => x -> x != 0 ? createFullString(x,Set_dic[lookUp_sym]) : ""))
        end
    end

    if write_csv CSV.write("$(OutputFolder_str)/$(name).csv",  print_tab) end
    return print_tab
end

# XXX returns a structured string of set name, if index and data tree is provided
function createFullString(setIdx_int,Tree_df::DataFrame,writeLvl_boo::Bool=true)
	setLvl_int = Tree_df[setIdx_int,:lvl]
	setVal_int = writeLvl_boo ? string(Tree_df[setIdx_int,:val]," (lvl ",Tree_df[setIdx_int,:lvl] ,")") : Tree_df[setIdx_int,:val]

	if setLvl_int == 1
		carStr_str = join(reverse(map(x -> writeLvl_boo ? string(Tree_df[x[1],:val]," (lvl ",Tree_df[x[1],:lvl] ,")") : Tree_df[x[1],:val],getHeritanceLine(setIdx_int,Tree_df)))," < ")
	else
		carStr_str = join(reverse(vcat(setVal_int,map(x -> writeLvl_boo ? string(Tree_df[x[1],:val]," (lvl ",Tree_df[x[1],:lvl] ,")") : Tree_df[x[1],:val],getHeritanceLine(setIdx_int,Tree_df))))," < ")
	end
	return carStr_str
end
# </editor-fold>

# <editor-fold desc="miscellaneous data processing"
# XXX builds intersection of rows matching the current one in cases where a subdictionary is used according to the named tuple provided, belongs to aggregateSetTable
function lookupIntersect(row::NamedTuple,grp::T,saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int16,BitSet}}) where T <: Pair
	lookup_tup = tuple(collect(getproperty(row,j) for j in grp[1])...)

	if lookup_tup in keys(saveInter_dic[grp[1]]) # take value directly from dictionary
		ele_arr = saveInter_dic[grp[1]][lookup_tup]
	else # compute value and write to dictionary
		ele_arr = intersect(lookupIntersect(row,grp[2],saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int16,BitSet}}),idxChildRows_dic[grp[1][end]][getproperty(row,grp[1][end])]) # lookup subdictionary
		saveInter_dic[grp[1]][lookup_tup] = ele_arr
	end
	return ele_arr
end

# XXX builds intersection of rows matching the current one in cases where no subdictionary is used, belongs to aggregateSetTable
function lookupIntersect(row::NamedTuple,grp::T,saveInter_dic::Dict,idxChildRows_dic::Dict{Symbol,Dict{Int16,BitSet}}) where T <: Tuple
	lookup_tup = tuple(collect(getproperty(row,j) for j in grp)...)

	if lookup_tup in keys(saveInter_dic[grp]) # take value directly from dictionary
		ele_arr = saveInter_dic[grp][lookup_tup]
	else  # compute value and write to dictionary
		ele_arr = intersect([idxChildRows_dic[j][getproperty(row,j)] for j in grp]...)
		saveInter_dic[grp][lookup_tup] = ele_arr
	end
	return ele_arr
end

# XXX writes all tuples occuring in a tuple of pairs and tuples
mixedTupToTup(x) = typeof(x) <: Pair ? map(y -> mixedTupToTup(y),collect(x)) :  x

# XXX returns dataframe columns without value column
removeVal(input_df::DataFrame) = filter(x -> x != :val,names(input_df))

# XXX returns all types within union type statement (taken from https://github.com/JuliaLang/julia/issues/14695#issuecomment-377662648)
Base.collect(t::Union{Type, DataType, Union{}}) = _collect(t, [])
_collect(t::Type, list) = t<:Union{} ? push!(list, t) : _collect(t.b, push!(list, t.a))
_collect(t::Union{DataType,Core.TypeofBottom}, list) = push!(list, t)
# </editor-fold>
