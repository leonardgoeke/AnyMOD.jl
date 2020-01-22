# XXX finds provided string tuple in tree structure and returns node id (or false), tuple does not need to start at the top level of tree, in that case function can return an array instead of a number
function lookupTupleTree(input_uni::Union{Tuple{Vararg{String,N} where N},String},tree_obj::Tree, startLvl_int::Int= 1)

	if isempty(tree_obj.nodes) return false end
    if isa(input_uni,String) input_uni = tuple(input_uni,) end

	noGap_arr = findall(input_uni .!= "")
	gaps_boo = "" in input_uni

	if startLvl_int == 1 && !gaps_boo
		return getDicEmpty(tree_obj.srcTup,input_uni)
	else
		# initialize by searching for last entry in input tuple
		start_int = maximum(noGap_arr)
		found_arr = getDicEmpty(tree_obj.srcStr,input_uni[start_int])

		# checks which nodes found initially actually comply with rest of input_uni
		for i in setdiff(noGap_arr,start_int)
			found_arr = getDicEmpty(tree_obj.srcStr,input_uni[i]) |> (y -> filter(x -> goUp(x,tree_obj.up,start_int - i) in y,found_arr))
		end
	end
	return found_arr
end

# XXX sorts inputs nodes according to their tree position
function sortSiblings(nodesIndex_arr::Array{Int,1},tree_obj::Tree)
	hertiLine_mat = map(x -> getAncestors(x, tree_obj),nodesIndex_arr)

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

# goes up the tree from x for the number of steps defined by steps_int
function goUp(x::Int,up::Dict{Int,Int},steps_int::Int)
	for l in 1:steps_int
		x = up[x]
	end
	return x
end

# XXX gives all parents (id, level) combination, if node is already on top level returns itself, if limitLvl_int is set only provide parents until that level
function getAncestors(startNode_int::Int,tree_obj::Tree,limitLvl_int::Int=0)

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

function getDescendants(startNode_int::Int,tree_obj::Tree,getAll::Bool = false, limitLvl_int::Int=0)

	# determines starting point
	startLvl_int = tree_obj.nodes[startNode_int].lvl

	# sets limits to maximum value if none provided
	if limitLvl_int == 0 limitLvl_int = tree_obj.height end

	if startLvl_int == limitLvl_int return startNode_int end

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

# XXX returns all nodes of tree on the level provided
getNodesLvl(tree_obj::Tree, level_int::Int) = filter(r -> r.lvl == level_int, collect(values(tree_obj.nodes)))

# XXX returns (unique) tuple with strings of node itself and its parents
function getUniName(nodeIdx_int::Int, tree_obj::Tree)
	relNodes_arr = tree_obj.nodes[nodeIdx_int].lvl == 1 ? [nodeIdx_int] : vcat(reverse(getAncestors(nodeIdx_int,tree_obj,1))..., nodeIdx_int)
	return tuple(map(x -> tree_obj.nodes[x[1]].val, relNodes_arr)...)
end

createFullString(nodeIdx_int::Int,tree_obj::Tree) = join(getUniName(nodeIdx_int,tree_obj)," < ")
