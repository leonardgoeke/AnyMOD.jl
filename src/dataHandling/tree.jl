# ! finds provided string tuple in tree structure and returns node id (or false), tuple does not need to start at the top level of tree, in that case function can return an array instead of a number
function lookupTupleTree(input_uni::Tuple{Vararg{String,N} where N},tree_obj::Tree, startLvl_int::Int= 1)

	if isempty(tree_obj.nodes) return false end

	# find leading and trailing empty entries
	firstVal_int = findfirst(x -> x != "",input_uni)
	lastVal_int = findlast(x -> x != "",input_uni)

	# adjust input uni and start level according to removed values
	startLvl_int = plus(firstVal_int,startLvl_int) - 1
	input_uni = input_uni[firstVal_int:lastVal_int]

	gap_arr = findall(input_uni .== "")

	if startLvl_int == 1 && isempty(gap_arr)
		return getDicEmpty(tree_obj.srcTup,input_uni)
	else
		noGap_arr = reverse(setdiff(1:length(input_uni),gap_arr))

		# initialize by searching for last entry in input tuple
		crtLvl_int = noGap_arr[1] + startLvl_int - 1
		found_arr = getDicEmpty(tree_obj.srcStr,(input_uni[noGap_arr[1]],crtLvl_int))

		# checks which nodes found initially actually comply with rest of input_uni
		for i in noGap_arr[2:end]
			crtLvlItr_int = i + startLvl_int - 1
			found_arr = getDicEmpty(tree_obj.srcStr,(input_uni[i],crtLvlItr_int)) |> (y -> filter(x -> goUp(x,tree_obj.up,crtLvl_int-crtLvlItr_int,tree_obj.nodes) in y,found_arr))
		end
	end
	return found_arr
end

# ! sorts inputs nodes according to their tree position
function sortSiblings(nodesIndex_arr::Array{Int,1},tree_obj::Tree)
	hertiLine_mat = map(x -> getAncestors(x, tree_obj,:tup),nodesIndex_arr)

    rowNum_int = length(nodesIndex_arr)
    colNum_int = maximum([hertiLine_mat[i][1][2] .+ 1 for i = 1:rowNum_int])

    herti_mat = zeros(Int64, rowNum_int, colNum_int)

    for (row, row_arr) in enumerate(hertiLine_mat)
		for ele in row_arr
            herti_mat[row,tree_obj.nodes[ele[1]].lvl+1] = ele[1]
        end
    end

    order_mat = sortslices(hcat(nodesIndex_arr,herti_mat), dims=1, by = x-> x[2:end,1])

    return order_mat[:,1]
end

# ! goes up the tree from x for the number of steps defined by steps_int
function goUp(x::Int,up::Dict{Int,Int},steps_int::Int,nodes_dic::Dict{Int,Node})
	startLvl_int = nodes_dic[x].lvl
	steps_ctr = 0
	while steps_ctr < steps_int
		x = up[x]
		steps_ctr = startLvl_int - nodes_dic[x].lvl
	end
	return x
end

# ! gets all parents (id, level) combination, if node is already on top level returns itself, if limitLvl_int is set only provide parents until that level
getAncestors(startNode_int::Int,tree_obj::Tree,retType::Symbol,limitLvl_int::Int=0) = getAncestors(startNode_int::Int,tree_obj::Tree,Val{retType}(),limitLvl_int::Int)

# ! returns an array of tuples with ancestors (idx,level)
function getAncestors(startNode_int::Int,tree_obj::Tree,retType::Val{:tup},limitLvl_int::Int=0)

	# gets level of start node
	currLvl_int = tree_obj.nodes[startNode_int].lvl

	# return array, if no further going up the tree is required
	if currLvl_int == 0 || limitLvl_int == currLvl_int  return [(startNode_int, currLvl_int)] end
	# initialize move up the tree
	heri_arr = Array{Tuple{Int,Int},1}()
	next = startNode_int

	# loops up the tree and obtains (id, level) combinations
	while limitLvl_int < currLvl_int
		next = tree_obj.up[next]
		currLvl_int = tree_obj.nodes[next].lvl
		push!(heri_arr, (next, currLvl_int))
	end

	return heri_arr
end

# ! returns an array of integers with ancestors
function getAncestors(startNode_int::Int,tree_obj::Tree,retType::Val{:int},limitLvl_int::Int=0)

	# gets level of start node
	currLvl_int = tree_obj.nodes[startNode_int].lvl

	# return array, if no further going up the tree is required
	if currLvl_int == 0 || limitLvl_int == currLvl_int  return [startNode_int] end
	# initialize move up the tree
	heri_arr = Array{Int,1}()
	next = startNode_int

	# loops up the tree and obtains (id, level) combinations
	while limitLvl_int < currLvl_int
		next = tree_obj.up[next]
		currLvl_int = tree_obj.nodes[next].lvl
		push!(heri_arr, next)
	end

	return heri_arr
end

# ! gets all children of node
function getDescendants(startNode_int::Int,tree_obj::Tree,getAll::Bool = false, limitLvl_int::Int=0)

	# determines starting point
	startLvl_int = tree_obj.nodes[startNode_int].lvl

	# sets limits to maximum value if none provided
	if limitLvl_int == 0 limitLvl_int = tree_obj.height end

	if startLvl_int == limitLvl_int || (getAll && isempty(tree_obj.nodes[startNode_int].down)) return [startNode_int] end

	startIdx_arr = tree_obj.nodes[startNode_int].down

	curLvl_int = startLvl_int

	# initialize array of all children
	if getAll allIdx_arr = startIdx_arr end

	while curLvl_int < (limitLvl_int-1)
		lookUp_arr = vcat(map(x -> tree_obj.nodes[x].down,startIdx_arr)...)
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

# ! returns all nodes of tree on the level provided
getNodesLvl(tree_obj::Tree, level_int::Int) = filter(r -> r.lvl == level_int, sort(collect(values(tree_obj.nodes)), by = x -> x.idx))

# ! returns (unique) tuple with strings of node itself and its parents
function getUniName(nodeIdx_int::Int, tree_obj::Tree)
	if nodeIdx_int == 0 return ("none",) end
	relNodes_arr = tree_obj.nodes[nodeIdx_int].lvl == 1 ? [nodeIdx_int] : vcat(reverse(getAncestors(nodeIdx_int,tree_obj,:tup,1))..., nodeIdx_int)
	return tuple(map(x -> tree_obj.nodes[x[1]].val, relNodes_arr)...)
end

createFullString(nodeIdx_int::Int,tree_obj::Tree) = join(getUniName(nodeIdx_int,tree_obj)," < ")
