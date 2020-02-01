
# XXX compute a subset of infeasible constraints
function printIIS(anyM::anyModel)

    # computes iis
    Gurobi.compute_conflict(anyM.optModel.moi_backend.optimizer.model)

    if anyM.optModel.moi_backend.optimizer.model.inner.conflict != 0 return end
    # loops over constraint tables to find constraints within iis
	allCns_pair = vcat(collect.(vcat(anyM.parts.bal.cns, anyM.parts.trd.cns, anyM.parts.lim.cns, map(x -> x.cns,values(anyM.parts.tech))...))...)

    for cns in allCns_pair
		if cns[1] == :objEqn continue end
        allConstr_arr = findall(DB.select(cns[2],:eqn =>  x -> MOI.get(anyM.optModel.moi_backend, Gurobi.ConstraintConflictStatus(), x.index)))
        # prints constraints within iis
        if !isempty(allConstr_arr)
            println("$(length(allConstr_arr)) of IIS in $(cns[1]) constraints.")
            colSet_dic = Dict(x => Symbol(split(string(x),"_")[1]) for x in eqnObj.dim)
            for iisConstr in allConstr_arr
                row = cns[2][iisConstr,:]
                dimStr_arr = map(x -> row[x] == 0 ?  "" : string(x,": ",createFullString(row[x], anyM.sets[colSet_dic[x]],true)),collect(keys(colSet_dic)))
                println("$(join(filter(x -> x != "",dimStr_arr),", ")), constraint: $(row[:cns])")
            end
        end
    end

end

# XXX prints dataframe to csv file
function printObject(print_df::DataFrame,sets::Dict{Symbol,Tree},options::modOptions; name::String = "", filterFunc::Union{Nothing,Function} = nothing)
	# initialize
	colNam_arr = names(print_df)
    cntCol_int = size(colNam_arr,1)

	# filters values according to filter function,
	print_df = isnothing(filterFunc) ? copy(print_df) : copy(filter(filterFunc,print_df))

	# converts variable column to value of variable
	if :var in colNam_arr
		print_df[!,:var] = value.(print_df[!,:var])
	end

    for i = 1:cntCol_int
        lookUp_sym = Symbol(split(String(colNam_arr[i]),"_")[1])
		if !(lookUp_sym in keys(sets)) && lookUp_sym == :eqn
			print_df[!,i] = string.(print_df[!,i])
        elseif lookUp_sym in keys(sets)
			print_df[!,i] = map(x -> createFullString(x,sets[lookUp_sym]),print_df[!,i])
        end
    end

	# rename columns
	colName_dic = Dict(:Ts_dis => :timestep_dispatch, :Ts_exp => :timestep_expansion, :Ts_expSup => :timestep_supordinate_expansion, :Ts_disSup => :timestep_supordinate_dispatch,
															:R_dis => :region_dispatch, :R_exp => :region_expansion, :R_to => :region_to, :R_from => :region_from, :C => :carrier, :Te => :technology,
																:cns => :constraint, :var => :variable)

	rename!(print_df,map(x -> x in keys(colName_dic) ? colName_dic[x] : x, names(print_df)) )

    CSV.write("$(options.outDir)/$(name)_$(options.outStamp).csv",  print_df)
end

reportResults(reportType::Symbol,anyM::anyModel) = reportResults(Val{reportType}(),anyM::anyModel)

# XXX summary of results for technologies
function reportResults(objGrp::Val{:tech},anyM::anyModel)

    techIdx_arr = collect(keys(anyM.parts.tech))
	allData_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], variable = Symbol[], value = Float64[])

	for t in techIdx_arr
		part = anyM.parts.tech[t]
		tech_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], variable = Symbol[], value = Float64[])

		# get capacity values
		for va in intersect(keys(part.var),(:capaConv, :capaStIn, :capaStOut,  :capaStSize))
			capa_df = copy(part.var[va])
			# set carrier column to zero for conversion capacities and add a spatial dispatch column
			if va == :capaConv
				capa_df[!,:C] .= 0
				capa_df[!,:R_dis] = map(x -> getindex.(getAncestors(x,anyM.sets[:R],part.balLvl.ref[2]),1)[end],capa_df[!,:R_exp])
			else
				capa_df[!,:R_dis] = map(x -> getindex.(getAncestors(x.R_exp,anyM.sets[:R],anyM.cInfo[x.C].rDis),1)[end],eachrow(capa_df))
			end

			select!(capa_df,Not(:R_exp))
			# aggregate values and add to tech data frame
			capa_df = by(capa_df,[:Ts_disSup,:R_dis,:C,:Te],value = [:var] => x -> value.(sum(x.var)))
			capa_df[!,:variable] .= va
			tech_df = vcat(tech_df,capa_df)
		end

		# get dispatch variables
		for va in intersect(keys(part.var),(:use, :gen, :stIntOut,  :stIntIn, :stExtOut, :stExtIn))
			disp_df = filter(x -> x.variable == (va in (:use,:gen) ? :capaConv : :capaStIn),tech_df)[!,Not(:variable)]
			# adds all relevant columns for carrier in case of conversion capacities where it is not specified by capacity
			if va in (:use, :gen)
				disp_df[!,:C] = fill(collect(getfield(part.carrier,va)),size(disp_df,1))
				disp_df = flatten(disp_df,:C)
			end
			# comput values and add to tech data frame
			disp_df[!,:variable] .= va
			disp_df[!,:value] .=  value.(aggDivVar(part.var[va],disp_df, (:Ts_disSup,:R_dis,:C,:Te),anyM.sets)[1]) ./ 1000
			tech_df = vcat(tech_df,unique(disp_df))
		end

		# add tech dataframe to overall data frame
		allData_df = vcat(allData_df,tech_df)
	end

	# get full load hours
	capaFLH_df = filter(x -> x.variable == :capaConv, allData_df)
	vlh_arr = map(eachrow(capaFLH_df)) do row
		relRow_df = filter(y -> y.Ts_disSup == row.Ts_disSup && y.R_dis == row.R_dis && y.Te == row.Te,allData_df)
		var_arr = unique(relRow_df[!,:variable]) |> (i -> (i,intersect(i,(:use,:stIntOut)))) |> (j -> isempty(j[2]) ? intersect(j[1],(:gen,:stIntIn)) : j[2])
		return sum(filter(y -> y.variable in var_arr,relRow_df)[!,:value])/row.value*1000
	end
	capaFLH_df[!,:value] = vlh_arr
	capaFLH_df[!,:variable] .= :FLH

	# get storage flh and cycling

	allData_df = vcat(allData_df,capaFLH_df)

	printObject(allData_df,anyM.sets,anyM.options, name = "results_tech")
end

# XXX plots tree graph for input set
"""
    drawNodeTree(Tree_df::DataFrame, options::modOptions; args...)
Draw a tree for all nodes provided by the set data frame and copies it to the output directory defined within options.

# Options and default values:
- `trans = 4.5`
    - Controls fading of color going further down the tree.
- `wide = fill(1.0,maximum(Tree_df[!,:lvl]))`
    - Ratio of distances between nodes that have and do not have the same parent (separate on each level).
- `labelsize = 7`
    - Size of labels in graph.
- `ratio = 1.0`
	- Aspect ratio of output graph.
"""
function drawTree(tree_sym::Symbol, anyM::anyModel; args...)

	name_dic = Dict(:region => :R,:timestep => :Ts,:carrier => :C,:tech => :Te)

	# convert tree object into a data frame
	treeObj = anyM.sets[name_dic[tree_sym]]
	data_arr = filter(x -> x.idx != 0,collect(values(treeObj.nodes))) |> (y -> map(x -> getfield.(y,x),(:idx,:val,:lvl,:down,:subIdx)))
	tree_df = DataFrame(idx = data_arr[1], val = data_arr[2], lvl =  data_arr[3], down = data_arr[4], subIdx = data_arr[5], up =map(x -> treeObj.up[x],data_arr[1]))

	# sets options
	col_dic = Dict(:region => (0.251,0.388,0.847),:timestep => (0.133, 0.545, 0.133),:carrier => (0.584, 0.345, 0.698),:tech => (0.796,0.235,0.2))
	defOpt_ntup = Dict(:trans => 4.5, :wide => fill(1.0,maximum(tree_df[!,:lvl])), :labelsize => 7, :ratio => 1.0)
	opt = merge(args,Dict(x => defOpt_ntup[x] for x in filter(x -> !(x in keys(args)), collect(keys(defOpt_ntup)))))

	# adds a new dummy top node
	push!(tree_df,(0,"",0,treeObj.nodes[0].down ,0,1))
	nodes_int = nrow(tree_df)
	idxPos_dic = Dict(zip(tree_df[:,:idx], 1:(nodes_int)))

    # create vertical position and labels from input tree
    LocY_arr = float(tree_df[!,:lvl]) .+ 1.2
    NodeLabel_arr = tree_df[!,:val]
	NodeLabel_arr = maximum(length.(NodeLabel_arr)) |> (k -> map(x -> string(x,fill("&#160;",(k-length(x))*2)...) ,NodeLabel_arr))

    # horizontal position is computed in a two step process
    LocX_arr = zeros(Float64, nodes_int)

    # first step, filter all nodes at end of a respective branch and sort them correctly
    LowLvl_df = tree_df[isempty.(tree_df[!,:down]),:]
    LowLvl_df = LowLvl_df[map(y -> findall(x -> x == y, LowLvl_df[:,:idx])[1],sortSiblings(convert(Array{Int64,1},LowLvl_df[:,:idx]),treeObj)),:]

    # set position of starting node
    LocX_arr[map(x -> idxPos_dic[x],LowLvl_df[1,:idx])] = 0

    # sets distance from next node on the left depending on if they are part of the same subtree
    for (idx2, lowNode) in Iterators.drop(enumerate(eachrow(LowLvl_df)),1)
		if lowNode[:up] == LowLvl_df[idx2-1,:up] distance_fl = opt[:wide][lowNode[:lvl]] else distance_fl = 1 end
		LocX_arr[idxPos_dic[lowNode[:idx]]] = LocX_arr[idxPos_dic[LowLvl_df[idx2-1,:idx]]] + distance_fl
    end

    # second step, remaining horizontal nodes are place in the middle of their children
    HighLvl_df = tree_df[false .== isempty.(tree_df[!,:down]),:]

    for highNode in reverse(eachrow(HighLvl_df))
		children_arr = map(x -> idxPos_dic[x], filter(x -> x.idx != highNode.idx && x.up == highNode.idx, tree_df)[!,:idx])
		LocX_arr[idxPos_dic[highNode[:idx]]] = Statistics.mean(LocX_arr[children_arr])
    end

    LocX_arr[end] = Statistics.mean(LocX_arr[map(x -> idxPos_dic[x],tree_df[findall(tree_df[:,:lvl] .== 1),:idx])])

    # draw final tree
    Tree_gra = SimpleGraph(nodes_int+1)
    for rowTree in eachrow(tree_df)
      # 0 node in tree_df becomes last node in graph, because there is 0 node within the plots
      if rowTree[:up] == 0 pare_int = nodes_int else pare_int = idxPos_dic[rowTree[:up]] end
      add_edge!(Tree_gra, idxPos_dic[rowTree[:idx]], pare_int)
    end

    color_arr = col_dic[tree_sym] |> (y -> [RGB24(min(1,y[1]*(1+x/opt[:trans])),min(1,y[2]*(1+x/opt[:trans])),min(1,y[3]*(1+x/opt[:trans]))) for x in LocY_arr])

    # add invisible dummy node on right side to avoid text being pushed out of the frame
    push!(LocX_arr,maximum(LocX_arr)*1.1)
    push!(LocY_arr,maximum(LocY_arr)*1.1)
    push!(color_arr,RGB24(1,1,1))
    push!(NodeLabel_arr,"")

    Tree_pl = gplot(Tree_gra, LocX_arr, LocY_arr, nodelabel=NodeLabel_arr, nodelabeldist=2, nodelabelangleoffset= π/4, NODELABELSIZE = opt[:labelsize], EDGELINEWIDTH=1, nodefillc = color_arr)

    draw(SVG("$(anyM.options.outDir)/$(tree_sym).svg", 60cm, 60*opt[:ratio]cm), Tree_pl)
end
