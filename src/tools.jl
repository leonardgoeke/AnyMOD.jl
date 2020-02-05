
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


# XXX prints dataframe to csv file
function printObject(print_df::DataFrame,sets::Dict{Symbol,Tree},options::modOptions; fileName::String = "", filterFunc::Function = x -> true)
	# initialize
	colNam_arr = names(print_df)
    cntCol_int = size(colNam_arr,1)

	# filters values according to filter function,
	print_df = copy(filter(filterFunc,print_df))

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
															:R => :region, :R_dis => :region_dispatch, :R_exp => :region_expansion, :R_to => :region_to, :R_from => :region_from, :C => :carrier, :Te => :technology,
																:cns => :constraint, :var => :variable)

	rename!(print_df,map(x -> x in keys(colName_dic) ? colName_dic[x] : x, names(print_df)) )

    CSV.write("$(options.outDir)/$(fileName)_$(options.outStamp).csv",  print_df)
end

# <editor-fold desc="reporting of results"

reportResults(reportType::Symbol,anyM::anyModel) = reportResults(Val{reportType}(),anyM::anyModel)

# XXX summary of all capacity and dispatch results
function reportResults(objGrp::Val{:summary},anyM::anyModel)

    techIdx_arr = collect(keys(anyM.parts.tech))
	allData_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], variable = Symbol[], value = Float64[])

	# XXX get expansion and capacity variables
	for t in techIdx_arr
		part = anyM.parts.tech[t]
		tech_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], variable = Symbol[], value = Float64[])

		# get installed capacity values
		for va in intersect(keys(part.var),(:expConv, :expStIn, :expStOut, :expStSize, :expExc, :capaConv, :capaStIn, :capaStOut,  :capaStSize, :commCapaConv, :commCapaStIn, :commCapaStOut, :commCapaStSize))
			capa_df = copy(part.var[va])
			if va in (:expConv, :expStIn, :expStOut, :expStSize)
				capa_df = flatten(capa_df,:Ts_expSup)
				select!(capa_df,Not(:Ts_disSup))
				rename!(capa_df,:Ts_expSup => :Ts_disSup)
			end
			# set carrier column to zero for conversion capacities and add a spatial dispatch column
			if va in (:expConv,:capaConv,:commCapaConv)
				capa_df[!,:C] .= 0
				capa_df[!,:R_dis] = map(x -> getAncestors(x,anyM.sets[:R],:int,part.balLvl.ref[2])[end],capa_df[!,:R_exp])
			else
				capa_df[!,:R_dis] = map(x -> getAncestors(x.R_exp,anyM.sets[:R],:int,anyM.cInfo[x.C].rDis)[end],eachrow(capa_df))
			end

			select!(capa_df,Not(:R_exp))
			# aggregate values and add to tech data frame
			capa_df = by(capa_df,[:Ts_disSup,:R_dis,:C,:Te],value = [:var] => x -> value.(sum(x.var)))
			capa_df[!,:variable] .= va
			tech_df = vcat(tech_df,capa_df)
		end

		# add tech dataframe to overall data frame
		allData_df = vcat(allData_df,tech_df)
	end

	# XXX get dispatch variables
	for va in (:use, :gen, :stIn, :stOut, :stExtIn, :stExtOut, :stIntIn, :stIntOut, :emission, :ctr, :trdBuy, :trdSell)
		# get all variables, group them and get respective values
		allVar_df = getAllVariables(va,anyM)
		if isempty(allVar_df) continue end

		disp_df = by(allVar_df,intersect(intCol(allVar_df),[:Ts_disSup,:R_dis,:C,:Te]),value = [:var] => x -> value(sum(x.var)))
		# scales values to twh (except for emissions of course)
		if va != :emission disp_df[!,:value] = disp_df[!,:value]  ./ 1000 end
		disp_df[!,:variable] .= va

		# add empty values for non-existing columns
		for dim in (:Te,:C)
			if !(dim in names(disp_df))
				disp_df[:,dim] .= 0
			end
		end

		allData_df = vcat(allData_df,disp_df)
	end

	# XXX get exchange variables aggregated by import and export
	allExc_df = getAllVariables(:exc,anyM)
	if !isempty(allExc_df)
	    # add losses to all exchange variables
	    allExc_df = getExcLosses(convertExcCol(allExc_df),anyM.parts.exc.par,anyM.sets)
	    # compute export and import of each region, losses are considered at import
	    excFrom_df = rename(by(allExc_df,[:Ts_disSup,:R_a,:C],value = [:var] => x -> value(sum(x.var))),:R_a => :R_dis)
	    excFrom_df[!,:variable] .= :export; excFrom_df[!,:Te] .= 0

	    excTo_df = rename(by(allExc_df,[:Ts_disSup,:R_b,:C],value = [:var,:loss] => x -> value(dot(x.var,(1 .- x.loss)))),:R_b => :R_dis)
	    excTo_df[!,:variable] .= :import; excTo_df[!,:Te] .= 0


	    allData_df = vcat(allData_df,vcat(excFrom_df,excTo_df))
	end

	# XXX get full load hours for conversion, storage input and storage output
	if anyM.options.decomm == :none
		flh_dic = Dict(:capaConv => :flhConv, :capaStIn => :flhStIn, :capaStOut => :flhStOut)
	else
		flh_dic = Dict(:commCapaConv => :flhConv, :commCapaStIn => :flhStIn, :commCapaStOut => :flhStOut)
	end

	for flhCapa in collect(keys(flh_dic))
		capaFlh_df = filter(x -> x.variable == flhCapa, allData_df)
		# get relevant dispatch variables for respective group
		vlh_arr = map(eachrow(capaFlh_df)) do row
			relRow_df = filter(y -> y.Ts_disSup == row.Ts_disSup && y.R_dis == row.R_dis && y.Te == row.Te,allData_df)
			if flhCapa in (:capaConv,:commCapaConv)
				var_arr = unique(relRow_df[!,:variable]) |> (i -> (i,intersect(i,(:use,:stIntOut)))) |> (j -> isempty(j[2]) ? intersect(j[1],(:gen,:stIntIn)) : j[2])
			elseif flhCapa in (:commCapaStIn,:capaStIn)
				var_arr = [:stIntIn,:stExtIn]
			elseif flhCapa in (:commCapaStOut,:capaStOut)
				var_arr = [:stIntOut,:stExtOut]
			end
			return sum(filter(y -> y.variable in var_arr,relRow_df)[!,:value])/row.value*1000
		end
		capaFlh_df[!,:value] = vlh_arr
		capaFlh_df[!,:variable] .= flh_dic[flhCapa]

		allData_df = vcat(allData_df,capaFlh_df)
	end

	# XXX comptue storage cycles
	if anyM.options.decomm == :none
		cyc_dic = Dict(:capaStIn => :cycStIn, :capaStOut => :cycStOut)
	else
		cyc_dic = Dict(:commCapaStIn => :cycStIn, :commCapaStOut => :cycStOut)
	end

	for cycCapa in collect(keys(cyc_dic))
		capaCyc_df = filter(x -> x.variable == :capaStSize, allData_df)
		# get relevant dispatch variables for respective group
		cyc_arr = map(eachrow(capaCyc_df)) do row
			relRow_df = filter(y -> y.Ts_disSup == row.Ts_disSup && y.R_dis == row.R_dis && y.Te == row.Te,allData_df)
			if cycCapa in (:commCapaStIn,:capaStIn)
				var_arr = [:stIntIn,:stExtIn]
			elseif cycCapa in (:commCapaStOut,:capaStOut)
				var_arr = [:stIntOut,:stExtOut]
			end
			return sum(filter(y -> y.variable in var_arr,relRow_df)[!,:value])/row.value*1000
		end
		capaCyc_df[!,:value] = cyc_arr
		capaCyc_df[!,:variable] .= cyc_dic[cycCapa]
		allData_df = vcat(allData_df,capaCyc_df)
	end

	printObject(allData_df,anyM.sets,anyM.options, fileName = "results_tech")
end

# XXX results for costs
function reportResults(objGrp::Val{:costs},anyM::anyModel)
	# prepare empty dataframe
	allData_df = DataFrame(Ts_disSup = Int[], R = Int[], Te = Int[], C = Int[], variable = Symbol[], value = Float64[])

	# loops over all objective variables with keyword "cost" in it
	for cst in filter(x -> occursin("cost",string(x)),keys(anyM.parts.obj.var))
		cost_df = copy(anyM.parts.obj.var[cst])
		# rename all dispatch and expansion regions simply to region
		if !isempty(intersect([:R_dis,:R_exp],names(cost_df)))
			rename!(cost_df,:R_dis in names(cost_df) ? :R_dis : :R_exp => :R)
		end
		# add empty column for non-existing dimensions
		for dim in (:Te,:C,:R)
			if !(dim in names(cost_df))
				cost_df[:,dim] .= 0
			end
		end
		# obtain values and write to dataframe
		cost_df[:,:variable] .= string(cst)
		cost_df[:,:value] = value.(cost_df[:,:var]) .* anyM.options.scaFac.cost
		allData_df = vcat(allData_df,cost_df[:,Not(:var)])
	end
	printObject(allData_df,anyM.sets,anyM.options, fileName = "results_costs")
end

# XXX results for exchange
function reportResults(objGrp::Val{:exchange},anyM::anyModel)
	allData_df = DataFrame(Ts_disSup = Int[], R_from = Int[], R_to = Int[], C = Int[], variable = Symbol[], value = Float64[])
	if isempty(anyM.parts.exc.var) error("No exchange data found") end

    # XXX expansion variables
	exp_df = copy(anyM.parts.exc.var[:expExc]) |> (x -> vcat(x,rename(x,:R_from => :R_to, :R_to => :R_from)))
	exp_df = flatten(exp_df,:Ts_expSup)
	select!(exp_df,Not(:Ts_disSup))
	rename!(exp_df,:Ts_expSup => :Ts_disSup)

	exp_df = by(exp_df,[:Ts_disSup,:R_from,:R_to,:C],value = [:var] => x -> value.(sum(x.var)))
	exp_df[!,:variable] .= :expExc

	# XXX capacity variables
	capa_df = copy(anyM.parts.exc.var[:capaExc])
	capa_df = vcat(capa_df,rename(filter(x -> x.dir == 0, capa_df),:R_from => :R_to, :R_to => :R_from))
	capa_df = by(capa_df,[:Ts_disSup,:R_from,:R_to,:C],value = [:var] => x -> value.(sum(x.var)))
	capa_df[!,:variable] .= :capaExc

	# XXX dispatch variables
	disp_df = getAllVariables(:exc,anyM)
	disp_df = by(disp_df,[:Ts_disSup,:R_from,:R_to,:C],value = [:var] => x -> value.(sum(x.var)) ./ 1000)
	disp_df[!,:variable] .= :exc


	# XXX get full load hours
	capaExt_df = replCarLeaves(copy(capa_df),anyM.sets[:C])
	flh_df = join(rename(select(capaExt_df,Not(:variable)),:value => :capa),rename(select(disp_df,Not(:variable)),:value => :disp),on = [:Ts_disSup,:R_from,:R_to,:C], kind = :inner)
	flh_df[!,:value] = flh_df[!,:disp] ./ flh_df[!,:capa] .* 1000
	flh_df[!,:variable] .= :flhExc

	# XXX merge and print all data
	allData_df = vcat(exp_df,capa_df,disp_df,select(flh_df,Not([:capa,:disp])))
	printObject(allData_df,anyM.sets,anyM.options, fileName = "results_exchange")
end

# XXX print time series for in and out into seperate tables
function reportTimeSeries(car_sym::Symbol; filterFunc = x -> true, unstackBoo::Bool = true, signVar::Tuple = (:in,:out), minVal::Float64 = 1e-3)

	# XXX converts carrier named provided to index
	node_arr = filter(x -> x.val == string(car_sym),collect(values(anyM.sets[:C].nodes)))
	if length(node_arr) != 1
		error("no carrier named $car_sym defined")
		return
	end
	c_int = node_arr[1].idx

	# XXX initialize dictionary to save data
	allData_dic = Dict{Symbol,DataFrame}()
	for signItr in signVar
		allData_dic[signItr] = DataFrame(Ts_disSup = Int[], Ts_dis = Int[], R_dis = Int[], variable = String[], value = Float64[])
	end

	# XXX initialize relevant dimensions and carriers
	relDim_df = filter(filterFunc,createPotDisp([c_int],anyM))
	relC_arr = unique([c_int,getDescendants(c_int,anyM.sets[:C])...])
	cRes_tup = anyM.cInfo[c_int] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c_int].lvl))

	# XXX add demand and size it
	if :out in signVar
		dem_df = matchSetParameter(relDim_df,anyM.parts.bal.par[:dem],anyM.sets,newCol = :value)
		dem_df[!,:value] = dem_df[!,:value] .* getResize(dem_df,anyM.sets[:Ts],anyM.supTs) .* -1
		dem_df[!,:variable] .= "demand"
		filter!(x -> abs(x.value) > minVal, dem_df)
		allData_df = vcat(allData_dic[:out],select!(dem_df,Not(:C)))
	end

	# XXX adds all technology related variables
	cBalRes_tup = anyM.cInfo[c_int] |> (x -> (x.tsDis, x.rDis))
	relType_tup = map(x -> x in signVar ? (x == :in ? (:use, :stExtIn) : (:gen,:stExtOut)) : tuple(),(:in,:out)) |> (x -> tuple(vcat(collect.(x)...)...))

	for c in relC_arr
		relTech_arr = vcat([intersect(relType_tup,filter(y -> c in anyM.parts.tech[x].carrier[y], collect(keys(anyM.parts.tech[x].carrier)))) |> (y -> collect(zip(fill(x,length(y)),y))) for x in collect(keys(anyM.parts.tech))]...)

		if isempty(relTech_arr) continue end

		for x in relTech_arr

			# gets resolution and adjusts add_df in case of an agggregated technology
			add_df = select(filter(r -> r.C == c,anyM.parts.tech[x[1]].var[x[2]]),[:Ts_disSup,:Ts_dis,:R_dis,:var])
			tRes_tup = anyM.parts.tech[x[1]].disAgg ? (cRes_tup[1], anyM.cInfo[c].rExp) : (cRes_tup[1], cRes_tup[2])
			checkTechReso!(tRes_tup,cBalRes_tup,add_df,anyM.sets)

			# filter values based on filter function and minimum value reported
			add_df = by(add_df,[:Ts_disSup,:Ts_dis,:R_dis],var = [:var] => x -> sum(x.var))
			filter!(filterFunc,add_df)
			add_df[!,:value] = value.(add_df[!,:var]) .* (x[2] in (:use,:stExtIn) ? -1.0 : 1.0)
			add_df[!,:variable] .= string(x[2],"; ", createFullString(x[1],anyM.sets[:Te]))
			filter!(x -> abs(x.value) > minVal, add_df)

			# add to dictionary of dataframe for in or out
			sign_sym = x[2] in (:use,:stExtIn) ? :out : :in
			allData_dic[sign_sym] = vcat(allData_dic[sign_sym] ,select(add_df,Not(:var)))
		end
	end

	# XXX add import and export variables
	exc_df = filterCarrier(anyM.parts.exc.var[:exc],relC_arr)
	if :out in signVar
		excFrom_df = by(filter(filterFunc,rename(copy(exc_df),:R_from => :R_dis)), [:Ts_disSup,:Ts_dis,:R_dis],value = [:var] => x -> value(sum(x.var)) * -1)
		excFrom_df[!,:variable] .= :export
		filter!(x -> abs(x.value) > minVal, excFrom_df)
		if !isempty(excFrom_df)
			allData_dic[:out] = vcat(allData_dic[:out],excFrom_df)
		end
	end

	if :in in signVar
		addLoss_df = rename(getExcLosses(convertExcCol(exc_df),anyM.parts.exc.par,anyM.sets),:R_b => :R_dis)
		excTo_df = by(filter(filterFunc,addLoss_df), [:Ts_disSup,:Ts_dis,:R_dis],value = [:var,:loss] => x -> value(dot(x.var,(1 .- x.loss))))
		excTo_df[!,:variable] .= :import
		filter!(x -> abs(x.value) > minVal, excTo_df)
		if !isempty(excTo_df)
			allData_df = vcat(allData_df,excTo_df)
			allData_dic[:in] = vcat(allData_dic[:in],excTo_df)
		end
	end

	# XXX add trade
	agg_arr = [:Ts_dis, :R_dis, :C]
	if !isempty(anyM.parts.trd.var)
		for trd in intersect(keys(anyM.parts.trd.var),(:trdBuy,:trdSell))
			trdVar_df = copy(relDim_df)
			trdVar_df[!,:value] = value.(filterCarrier(anyM.parts.trd.var[trd],relC_arr) |> (x -> aggUniVar(x,relDim_df,agg_arr,cRes_tup,anyM.sets))) .* (trd == :trdBuy ? 1.0 : -1.0)
			trdVar_df[!,:variable] .= trd
			filter!(x -> abs(x.value) > minVal, trdVar_df)
			sign_sym = :trdBuy == trd ? :in : :out
			allData_dic[sign_sym] = vcat(allData_dic[sign_sym],select(trdVar_df,Not(:C)))
		end
	end

	# XXX add curtailment
	if :crt in keys(anyM.parts.bal.var)
		crt_df = copy(relDim_df)
		crt_df[!,:value] = value.(filterCarrier(anyM.parts.bal.var[:crt],relC_arr) |> (x -> aggUniVar(x,crt_df,agg_arr, cRes_tup,anyM.sets))) .* -1.0
		crt_df[!,:variable] .= :crt
		filter!(x -> abs(x.value) > minVal, crt_df)
		allData_dic[:out] = vcat(allData_dic[:out],select(crt_df,Not(:C)))
	end

	# XXX unstack data and write to csv
	for signItr in signVar
		data_df = allData_dic[signItr]
		if unstackBoo && !isempty(data_df)
			data_df[!,:variable] = CategoricalArray(data_df[!,:variable])
			data_df = unstack(data_df,:variable,:value)
		end

		printObject(data_df,anyM.sets,anyM.options, fileName = string("timeSeries_",car_sym,"_",signItr))
	end
end

# </editor-fold>
