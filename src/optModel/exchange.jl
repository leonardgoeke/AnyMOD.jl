
#region # * prepare and create exchange related variables

# ! prepare dictionary that specifies dimensions for expansion and capacity variables
function prepareExc!(excSym_arr::Array{Symbol,1},prepVar_dic::Dict{Symbol,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	partLim = anyM.parts.lim

	for excSym in excSym_arr
	
		excInt = sysInt(excSym,anyM.sets[:Exc])
	
		partExc = anyM.parts.exc[excSym]
		prepExc_dic = Dict{Symbol,NamedTuple}()
	
		# obtain dimensions of expansion variables for exchange
		prepareExcExpansion!(excInt,partExc,partLim,prepExc_dic,tsYear_dic,anyM)
	
		# obtain capacity dimensions solely based on expansion variables
		prepareCapacity!(partExc,prepExc_dic,prepExc_dic[:expExc].var,:capaExc,anyM)
		addResidualCapaExc!(partExc,prepExc_dic,anyM)
	
		# if any capacity variables or residuals were prepared, add these to overall dictionary
		if collect(values(prepExc_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepVar_dic[excSym] = prepExc_dic
		end
	end	
end

# ! prepare expansion and capacity variables for exchange
function prepareExcExpansion!(excInt::Int,partExc::ExcPart,partLim::OthPart,prepExc_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	# ! determine dimensions of expansion variables (expansion for exchange capacities is NOT directed!)
	# get all possible dimensions of exchange
	potExc_df = getfield.(getNodesLvl(anyM.sets[:R],partExc.expLvl[2]),:idx)  |> (x  -> DataFrame(R_a = [x], R_b = [x], Exc = excInt))
	potExc_df = flatten(flatten(potExc_df,:R_a), :R_b)

	# get dimensions where exchange should actually be defined
	exExp_df = DataFrame(R_a = Int[], R_b = Int[], Exc = Int[])

	for excPar in intersect((:capaExcResi,:capaExcResiDir),keys(partExc.par))
		append!(exExp_df,matchSetParameter(potExc_df,partExc.par[excPar],anyM.sets)[!,[:R_a,:R_b,:Exc]])
	end

	# ensure expansion entries are not directed
	exExp_df = unique(exExp_df) |> (x -> filter(y -> y.R_a < y.R_b,vcat(x,rename(x,replace(namesSym(x),:R_a => :R_b, :R_b => :R_a))))) |> (z -> unique(z))

	# add supordiante timesteps of expansion
	exExp_df[!,:Ts_exp] .= partExc.expLvl[1]
	exExp_df[!,:Ts_expSup] = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), exExp_df[!,:Ts_exp])

	# filter cases where expansion is fixed to zero
	if :expExcFix in keys(anyM.parts.lim.par)
		exExp_df = removeEntries([filterZero(exExp_df,getLimPar(anyM.parts.lim,:expExcFix,anyM.sets[:Te]),anyM)],exExp_df)
	end

	# save result to dictionary for variable creation
	exp_df = unique(addSupTsToExp(exExp_df,partExc.par,:Exc,tsYear_dic,anyM))
	prepExc_dic[:expExc] = (var = orderDf(convertExcCol(exp_df)), resi = DataFrame())

end

# ! add residual capacties for exchange (both symmetric and directed)
function addResidualCapaExc!(partExc::ExcPart,prepExc_dic::Dict{Symbol,NamedTuple},anyM::anyModel)

	potExc_df = select(convertExcCol(prepExc_dic[:capaExc].var),Not([:dir])) |> (y -> vcat(y,rename(y,:R_b => :R_a,:R_a => :R_b)))

	# obtain symmetric residual capacites
	capaResi_df = filter(x -> x.R_a != x.R_b, checkResiCapa(:capaExc,potExc_df, partExc, anyM))
	sortR_mat = sort(hcat([capaResi_df[!,x] for x in (:R_a,:R_b)]...);dims = 2)
	for (index,col) in enumerate((:R_a,:R_b)) capaResi_df[!,col] = sortR_mat[:,index] end

	# manipulate entries in case directed residual capacities are defined
	if :capaExcResiDir in keys(partExc.par)

		directExc_df = matchSetParameter(potExc_df,partExc.par[:capaExcResiDir],anyM.sets)
		directExc_df[!,:var] = map(x -> AffExpr(x), directExc_df[!,:val]); select!(directExc_df,Not(:val))

		excDim_arr = [:Ts_disSup, :Ts_expSup, :R_a, :R_b, :Exc]
		excDimP_arr = replace(excDim_arr,:R_a => :R_b, :R_b => :R_a)

		#  entries, where a directed capacity is provided and a symmetric one already exists
		bothExc_df = vcat(innerjoin(directExc_df, capaResi_df; on = excDim_arr, makeunique = true), innerjoin(directExc_df, capaResi_df; on = Pair.(excDim_arr,excDimP_arr), makeunique = true))
		bothExc_df = combine(x -> (var = x.var + x.var_1,), groupby(bothExc_df,excDim_arr))
		if !(:var in namesSym(bothExc_df)) bothExc_df[!,:var] = AffExpr[] end
		# entries, where only a directed capacity was provided
		onlyDirExc_df = antijoin(directExc_df, bothExc_df; on = excDim_arr)

		# entries originally symmetric that now become directed, because a directed counterpart was introduced
		flipSym_df = antijoin(innerjoin(capaResi_df, bothExc_df[!,Not(:var)]; on = excDim_arr),bothExc_df[!,Not(:var)]; on = excDim_arr .=> excDimP_arr)
		swtExc_df = vcat(bothExc_df,flipSym_df)

		# solely directed entries
		dirExc_df = vcat(onlyDirExc_df,swtExc_df)
		dirExc_df[!,:dir] .= true

		# entries who become directed because their counterpart became directed
		becomDirExc_df = innerjoin(rename(dirExc_df[!,Not([:var,:dir])],:R_a => :R_b, :R_b => :R_a),vcat(capaResi_df,rename(capaResi_df,:R_a => :R_b, :R_b => :R_a)); on = excDim_arr)
		becomDirExc_df[!,:dir] .= true

		# entries entries originally symmetric that remain symmetric
		unDirExc_df = antijoin(capaResi_df, vcat(dirExc_df, rename(dirExc_df,:R_a => :R_b, :R_b => :R_a)); on = excDim_arr )
		unDirExc_df[!,:dir] .= false

		# adjust dataframe of residual capacities according to directed values
		capaResi_df = vcat(dirExc_df,vcat(unDirExc_df,becomDirExc_df))

		# adjust dataframe of capacities determining where variables will be created to reflect which of these correspond to directed cases now
		allVar_df = prepExc_dic[:capaExc].var
		if !isempty(prepExc_dic[:capaExc].var)
			undirBoth_df = vcat(dirExc_df,rename(dirExc_df,replace(namesSym(dirExc_df),:R_a => :R_b, :R_b => :R_a)))[!,Not(:dir)]
			dirVar_df = convertExcCol(innerjoin(convertExcCol(allVar_df[!,Not(:dir)]), vcat(undirBoth_df,swtExc_df)[!,Not(:var)],on = excDim_arr))
			dirVar_df[!,:dir] .= true
			adjVar_df = vcat(dirVar_df,antijoin(allVar_df,dirVar_df,on = [:Ts_disSup, :Ts_expSup, :R_from, :R_to] ))
		else
			adjVar_df = allVar_df
		end
	else
		capaResi_df[!,:dir] .= false
		adjVar_df = prepExc_dic[:capaExc].var
	end

	# filter cases where in and out region are the same
	filter!(x -> x.R_a != x.R_b, capaResi_df)

	prepExc_dic[:capaExc] = (var = unique(adjVar_df), resi = orderDf(convertExcCol(capaResi_df)))
end

# ! create exchange variables
function createExcVar!(partExc::ExcPart,ts_dic::Dict{Tuple{Int,Int},Array{Int,1}},anyM::anyModel)
	# ! extend capacity variables to dispatch variables
	capa_df = partExc.var[:capaExc][!,Not([:var,:dir])] |> (x -> unique(vcat(x,rename(x,replace(namesSym(x),:R_from => :R_to, :R_to => :R_from)))))
	# replace orginal carrier with leafs
	capa_df = replCarLeafs(capa_df,anyM.sets[:C])

	cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, anyM.cInfo[x].rDis) for x in unique(capa_df[!,:C]))
	capa_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],capa_df[!,:C])
	capa_df[!,:lvlR] = map(x -> cToLvl_dic[x][2],capa_df[!,:C])
	rExc_dic = Dict(x => anyM.sets[:R].nodes[x[1]].lvl != x[2] ? getDescendants(x[1],anyM.sets[:R],false,x[2]) : [x[1]]
																							for x in union([map(x -> (x[y],x.lvlR), eachrow(unique(capa_df[!,[y,:lvlR]]))) for y in (:R_from,:R_to)]...))
	capa_df[!,:R_from] = map(x -> rExc_dic[x.R_from,x.lvlR],eachrow(capa_df[!,[:R_from,:lvlR]]))
	capa_df[!,:R_to] = map(x -> rExc_dic[x.R_to,x.lvlR],eachrow(capa_df[!,[:R_to,:lvlR]]))
	capa_df = flatten(select(capa_df,Not(:lvlR)),:R_from); capa_df = unique(flatten(capa_df,:R_to))

	capa_df[!,:scr] = map(x -> anyM.supTs.scr[x],capa_df[!,:Ts_disSup])
	capa_df = flatten(capa_df,:scr)

	disp_df = combine(x -> (Ts_dis = ts_dic[(x.Ts_disSup[1],x.lvlTs[1])],),groupby(capa_df,namesSym(capa_df)))[!,Not(:lvlTs)]

	# filter entries where availability is zero
	if !isempty(partExc.par[:avaExc].data) && 0.0 in partExc.par[:avaExc].data[!,:val]
		disp_df = filter(x -> x.val != 0.0, matchSetParameter(disp_df,partExc.par[:avaExc],anyM.sets))[!,Not(:val)]
	end

	# computes value to scale up the global limit on dispatch variable that is provied per hour and create variables
	partExc.var[:exc] = orderDf(createVar(disp_df,"exc",getUpBound(disp_df,anyM.options.bound.disp / anyM.options.scaFac.dispExc,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.dispExc))
end

# ! converts table where exchange regins are given as "R_a" and "R_b" to "R_to" and "R_from" and the other way around
convertExcCol(in_df::DataFrame) = rename(in_df, namesSym(in_df) |> (x  -> :R_a in x ? replace(x,:R_a => :R_from, :R_b => :R_to) : replace(x,:R_from => :R_a, :R_to => :R_b)))

#endregion

#region # * create exchange related constraints

# ! connect capacity and expansion constraints for exchange
function createCapaExcCns!(partExc::ExcPart,anyM::anyModel)

	if partExc.decomm == :none
		capaVar_df = rename(partExc.var[:capaExc],:var => :capaVar)
	else
		capaVar_df = rename(partExc.var[:insCapaExc],:var => :capaVar)
	end

	if :expExc in keys(partExc.var)
		expVar_df = flatten(partExc.var[:expExc],:Ts_disSup)[!,Not(:Ts_exp)]

		cns_df = innerjoin(capaVar_df, combine(groupby(expVar_df,[:Ts_disSup, :R_from, :R_to, :C]), :var => (x -> sum(x)) => :expVar); on = [:Ts_disSup, :R_from, :R_to, :C])

		# prepare, scale and create constraints
		cns_df[!,:cnsExpr] = map(x -> x.capaVar - x.capaVar.constant - x.expVar, eachrow(cns_df))
		cns_df = intCol(cns_df,:dir) |> (x -> orderDf(cns_df[!,[x...,:cnsExpr]]))
		scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
		partExc.cns[:excCapa] = createCns(cnsCont(cns_df,:equal),anyM.optModel)
	end

	# create and control operated capacity variables
	if anyM.options.decommExc != :none && :capaExc in keys(partExc.var)
		# constraints for operated capacities are saved into a dictionary of containers and then actually created
		cns_dic = Dict{Symbol,cnsCont}()
		createOprVarCns!(partExc,cns_dic,anyM)
		for cnsSym in keys(cns_dic)
			scaleCnsExpr!(cns_dic[cnsSym].data,anyM.options.coefRng,anyM.options.checkRng)
			partExc.cns[cnsSym] = createCns(cns_dic[cnsSym],anyM.optModel)
		end
	end
end

# ! create capacity restriction for exchange
function createRestrExc!(ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},partExc::ExcPart,anyM::anyModel)

	# create entries in both directions where exchange is directed
	if partExc.decomm != :none
		capaVar_df = filter(r -> r.dir,partExc.var[:capaExc]) |> (x -> vcat(filter(r -> !r.dir,partExc.var[:capaExc]),vcat(x,rename(x,replace(namesSym(x),:R_to => :R_from, :R_from => :R_to)))))
	else
		capaVar_df = partExc.var[:capaExc]
	end
	# group exchange capacities by carrier
	grpCapa_df = groupby(rename(capaVar_df,:var => :capa),:C)

	# pre-allocate array of dataframes for restrictions
	restr_arr = Array{DataFrame}(undef,length(grpCapa_df))
	itrRestr = collect(enumerate(grpCapa_df))

	# create restrictions
	@threads for x in itrRestr
		cns_df = copy(x[2])
		cns_df[!,:scr] = map(x -> anyM.supTs.scr[x], cns_df[!,:Ts_disSup])
		restr_arr[x[1]] = prepareRestrExc(flatten(cns_df,:scr),ts_dic,partExc,anyM)
	end

	anyM.parts.exc.cns[:excRestr] = createCns(cnsCont(vcat(restr_arr...),:smaller),anyM.optModel)
end

# ! prepare capacity restriction for specific carrier
function prepareRestrExc(cns_df::DataFrame,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},partExc::ExcPart,anyM::anyModel)

	c_int = cns_df[1,:C]
	leafes_arr = filter(y -> isempty(anyM.sets[:C].nodes[y].down), [c_int,getDescendants(c_int,anyM.sets[:C],true)...])
	cRes_tup = anyM.cInfo[c_int] |> (x -> (Ts_dis = x.tsDis, R_from = x.rDis, R_to = x.rDis))

	# extend constraint dataframe to dispatch levels
	cns_df[!,:Ts_dis] .= map(x -> ts_dic[x,cRes_tup.Ts_dis],cns_df[!,:Ts_disSup])
	cns_df = flatten(cns_df,:Ts_dis)

	# resize capacity variables
	cns_df[!,:capa]  = cns_df[!,:capa] .* map(x -> anyM.supTs.sca[(x,cRes_tup.Ts_dis)], cns_df[!,:Ts_disSup])

	# filter relevant dispatch variables
	relDisp_df = filter(x -> x.C in leafes_arr, partExc.var[:exc])

	# first aggregate symmetric and directed entries in one direction, then directed entries in the other direction
	cns_df[!,:disp] = aggUniVar(relDisp_df,cns_df,[:Ts_dis,:R_from,:R_to,:scr],cRes_tup,anyM.sets)
	dir_arr = findall(.!cns_df[!,:dir])
	cns_df[dir_arr,:disp] = cns_df[dir_arr,:disp] .+ aggUniVar(rename(relDisp_df,:R_from => :R_to, :R_to => :R_from),cns_df[dir_arr,:],[:Ts_dis,:R_from,:R_to,:scr],cRes_tup,anyM.sets)

	# add availablities to dataframe
	cns_df = matchSetParameter(convertExcCol(cns_df),partExc.par[:avaExc],anyM.sets, newCol = :avaSym)

	if :avaExcDir in keys(partExc.par)
		dirAva_df = matchSetParameter(cns_df[!,intCol(cns_df,:dir)],partExc.par[:avaExcDir],anyM.sets, newCol = :avaDir)
		cns_df = joinMissing(cns_df,dirAva_df,[:Ts_disSup,:Ts_dis,:R_a,:R_b,:C,:dir],:left, Dict(:avaDir => nothing))
	else
		cns_df[!,:avaDir] .= nothing
	end

	# prepare, scale and create constraints
	cns_df[!,:cnsExpr] = map(x -> x.disp  - x.capa * (isnothing(x.avaDir) ? x.avaSym : x.avaDir), eachrow(cns_df))
	scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)

	return convertExcCol(cns_df) |> (x -> select(x,intCol(x,:cnsExpr)))
end

#endregion

#region # * utility functions for exchange

# ! obtain values for exchange losses
function getExcLosses(exc_df::DataFrame,excPar_dic::Dict{Symbol,ParElement},sets_dic::Dict{Symbol,Tree})
	lossPar_obj = copy(excPar_dic[:lossExc])
	if :R_a in namesSym(lossPar_obj.data) && :R_b in namesSym(lossPar_obj.data)
		lossPar_obj.data = lossPar_obj.data |> (x -> vcat(x,rename(x,:R_a => :R_b, :R_b => :R_a)))
	end
	excLoss_df = matchSetParameter(exc_df,lossPar_obj,sets_dic,newCol = :loss)

	# overwrite symmetric losses with any directed losses provided
	if :lossExcDir in keys(excPar_dic)
		oprCol_arr = intCol(excLoss_df)
		dirLoss_df = matchSetParameter(excLoss_df[!,oprCol_arr],excPar_dic[:lossExcDir],sets_dic,newCol = :lossDir)
		excLoss_df = joinMissing(excLoss_df,dirLoss_df,oprCol_arr,:left,Dict(:lossDir => nothing))
		excLoss_df[!,:loss] = map(x -> isnothing(x.lossDir) ? x.loss : x.lossDir,eachrow(excLoss_df[!,[:loss,:lossDir]]))
		select!(excLoss_df,Not(:lossDir))
	end

	return excLoss_df
end

#endregion
