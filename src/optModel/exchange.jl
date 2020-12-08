
#=
eSym = :hvac

excDir_arr = map(x -> sysInt(x,anyM.sets[:Exc]), filter(z -> anyM.parts.exc[z].dir, excSym_arr))

part = anyM.parts.exc[eSym]
prepExc_dic = prepSys_dic[:Exc][eSym]

cns_dic = Dict{Symbol,cnsCont}()

# creates capacity, expansion, and retrofitting variables
createExpCap!(part,prepExc_dic,anyM)

# create expansion constraints
if isempty(anyM.subPro) || anyM.subPro == (0,0)
	# connect capacity and expansion variables
	createCapaCns!(part,prepExc_dic,cns_dic,excDir_arr)

	# control operated capacity variables
	if part.decomm != :none
		createOprVarCns!(part,cns_dic,anyM)
	end
end

if isempty(anyM.subPro) || anyM.subPro != (0,0)
	# create dispatch variables
	createExcVar!(part,ts_dic,anyM) 
	produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")
	# create capacity restrictions
	createRestrExc!(ts_dic,part,anyM) # TODO hier weiter
	produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")
	produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")
end

lower_bound(collect(keys(part.var[:insCapaExc][1,:var].terms))[1])
=#

#region # * prepare and create exchange related variables

# ! prepare dictionary that specifies dimensions for expansion and capacity variables
function prepareExc!(excSym_arr::Array{Symbol,1},prepAllExc_dic::Dict{Symbol,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	partLim = anyM.parts.lim

	for excSym in excSym_arr
	
		excInt = sysInt(excSym,anyM.sets[:Exc])
		part = anyM.parts.exc[excSym]
		prepExc_dic = Dict{Symbol,NamedTuple}()
		
		if part.type != :stock
			# obtain dimensions of expansion variables for exchange
			prepareExcExpansion!(excInt,part,partLim,prepExc_dic,tsYear_dic,anyM)
			# obtain capacity dimensions solely based on expansion variables
			prepareCapacity!(part,prepExc_dic,prepExc_dic[:expExc].var,:capaExc,anyM)
		else
			prepExc_dic[:capaExc] = (var = DataFrame(Ts_expSup = Int[], Ts_disSup = Int[], R_from = Int[], R_to = Int[], Exc = Int[], dir = Bool[]), resi = DataFrame())
		end
		# add residual capacities
		addResidualCapaExc!(part,prepExc_dic,anyM)

		# maps required capacity restrictions
		capaDispRestr_arr = Array{Tuple{String,Array{Int,1},Int,Int},1}()
		restrInfo_arr = mapCapaRestr(map(x -> (x, anyM.cInfo[x].tsDis, anyM.cInfo[x].rDis),collect(part.carrier)),:exc,anyM)
		map(x -> push!(capaDispRestr_arr,("exc", restrInfo_arr[x][1], restrInfo_arr[x][2], restrInfo_arr[x][3])),1:length(restrInfo_arr))
		part.capaRestr = isempty(capaDispRestr_arr) ? DataFrame() : categorical(rename(DataFrame(capaDispRestr_arr), :1 => :cnstrType, :2 => :car, :3 => :lvlTs, :4 => :lvlR))
	
		# if any capacity variables or residuals were prepared, add these to overall dictionary
		if collect(values(prepExc_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepAllExc_dic[excSym] = prepExc_dic
		end
	end	
end

# ! prepare expansion and capacity variables for exchange
function prepareExcExpansion!(excInt::Int,part::ExcPart,partLim::OthPart,prepExc_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	# ! determine dimensions of expansion variables (expansion for exchange capacities is NOT directed!)
	# get all possible dimensions of exchange
	potExc_df = getfield.(getNodesLvl(anyM.sets[:R],part.expLvl[2]),:idx)  |> (x  -> DataFrame(R_from = [x], R_to = [x], Exc = excInt))
	potExc_df = flatten(flatten(potExc_df,:R_from), :R_to)

	# get dimensions where exchange should actually be defined
	exExp_df = DataFrame(R_from = Int[], R_to = Int[], Exc = Int[])

	for excPar in intersect((:capaExcResi,:capaExcResiDir),keys(part.par))
		append!(exExp_df,matchSetParameter(potExc_df,part.par[excPar],anyM.sets)[!,[:R_from,:R_to,:Exc]])
	end

	# creates directed or undirected expansion entries
	exExp_df = unique(filter(y ->  (part.dir ? y.R_from != y.R_to : y.R_from < y.R_to),flipExc(unique(exExp_df))))
	
	if !isempty(exExp_df)
		# add supordiante timesteps of expansion
		exExp_df[!,:Ts_exp] .= fill(getfield.(getNodesLvl(anyM.sets[:Ts], part.expLvl[1]),:idx),size(exExp_df,1))
		exExp_df = flatten(exExp_df,:Ts_exp)
		exExp_df[!,:Ts_expSup] = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), exExp_df[!,:Ts_exp])

		# save result to dictionary for variable creation
		exp_df = unique(addSupTsToExp(exExp_df,part.par,:Exc,tsYear_dic,anyM))
		exp_df[!,:dir] = map(x -> part.dir,eachrow(exp_df))
		prepExc_dic[:expExc] = (var = orderDf(exp_df), resi = DataFrame())
	end
end

# ! add residual capacties for exchange (both symmetric and directed)
function addResidualCapaExc!(part::ExcPart,prepExc_dic::Dict{Symbol,NamedTuple},anyM::anyModel)

	eInt = sysInt(Symbol(part.name[end]),anyM.sets[:Exc])

	# create dataframe of all potential exchange capacities
	rCombi_arr = getfield.(getNodesLvl(anyM.sets[:R], part.expLvl[2]),:idx) |> (z -> [getindex.(vcat(collect(Iterators.product(z,z))...),i) for i in (1,2)])
	potExc_df = flatten(DataFrame(Ts_disSup = fill(collect(anyM.supTs.step),length(rCombi_arr[1])), R_from = rCombi_arr[1], R_to = rCombi_arr[2], Exc = fill(eInt,length(rCombi_arr[1]))),:Ts_disSup)
	potExc_df[!,:Ts_expSup] = map(x -> part.type != :emerging ? [0] : filter(y -> y <= x,collect(anyM.supTs.step)), potExc_df[!,:Ts_disSup])
	potExc_df = flatten(potExc_df,:Ts_expSup)

	if part.dir # for directed exchange undirected residuals are applied in both directions
		capaResi_df = flipExc(checkResiCapa(:capaExc,potExc_df, part, anyM))
		
		if :capaExcResiDir in keys(part.par)
			capaDirResi_df = matchSetParameter(potExc_df,part.par[:capaExcResiDir],anyM.sets)
			capaResi_df = joinMissing(capaResi_df,capaDirResi_df,intCol(capaResi_df),:outer,Dict(:var => AffExpr(),:val => 0.0))
			capaResi_df[!,:var] = capaResi_df[!,:val] .+ capaResi_df[!,:var]
			select!(capaResi_df,Not([:val]))
		end
		capaResi_df[!,:dir] .= true

		if part.type != :stock
			adjVar_df = prepExc_dic[:capaExc].var
		else
			adjVar_df = filter(x -> x.R_from != x.R_to,select(capaResi_df,Not([:var])))
		end
	else # for undirected exchange directed residuals are added to directed values
		# obtain symmetric residual capacites
		capaResi_df = filter(x -> x.R_from != x.R_to, checkResiCapa(:capaExc,potExc_df, part, anyM))
		sortR_mat = sort(hcat([capaResi_df[!,x] for x in (:R_from,:R_to)]...);dims = 2)
		for (index,col) in enumerate((:R_from,:R_to)) capaResi_df[!,col] = sortR_mat[:,index] end

		# manipulate entries in case directed residual capacities are defined
		if :capaExcResiDir in keys(part.par)

			directExc_df = matchSetParameter(potExc_df,part.par[:capaExcResiDir],anyM.sets)
			directExc_df[!,:var] = map(x -> AffExpr(x), directExc_df[!,:val]); select!(directExc_df,Not(:val))

			excDim_arr = [:Ts_disSup, :Ts_expSup, :R_from, :R_to, :Exc]
			excDimP_arr = replace(excDim_arr,:R_from => :R_to, :R_to => :R_from)

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
			becomDirExc_df = innerjoin(switchExcCol(dirExc_df[!,Not([:var,:dir])]),flipExc(capaResi_df); on = excDim_arr)
			becomDirExc_df[!,:dir] .= true

			# entries entries originally symmetric that remain symmetric
			unDirExc_df = antijoin(capaResi_df, flipExc(dirExc_df); on = excDim_arr )
			unDirExc_df[!,:dir] .= false

			# adjust dataframe of residual capacities according to directed values
			capaResi_df = vcat(dirExc_df,vcat(unDirExc_df,becomDirExc_df))

			# adjust dataframe of capacities determining where variables will be created to reflect which of these correspond to directed cases now
			allVar_df = prepExc_dic[:capaExc].var
			if !isempty(prepExc_dic[:capaExc].var)
				undirBoth_df = flipExc(dirExc_df)[!,Not(:dir)]
				dirVar_df = innerjoin(allVar_df[!,Not(:dir)], vcat(undirBoth_df,swtExc_df)[!,Not(:var)],on = excDim_arr)
				dirVar_df[!,:dir] .= true
				adjVar_df = vcat(dirVar_df,antijoin(allVar_df,dirVar_df,on = [:Ts_disSup, :Ts_expSup, :R_from, :R_to] ))
			else
				adjVar_df = allVar_df
			end
		else
			capaResi_df[!,:dir] .= false
			adjVar_df = prepExc_dic[:capaExc].var
		end
	end
	
	# filter cases where in and out region are the same
	capaResi_df = filter(x -> x.R_from != x.R_to, combine(groupby(capaResi_df,intCol(capaResi_df,:dir)),:var => (x -> sum(x)) => :var))

	# operated capacity for undirected exchange will always be symmetric, therefore only the installed capacities in the direction that has smaller residuals are needed 
	if !part.dir && part.decomm != :none
		# adds a new temporary column where regions are is ascending order for grouping
		capaResi_df[!,:R_sort] = map(x -> (min(x.R_from,x.R_to),max(x.R_from,x.R_to)) ,eachrow(capaResi_df))
		# filter all rows where the other direction has a smaller residual value
		capaResi_df = select(vcat(map(z -> minimum(getfield.(z.var,:constant)) |> (y -> filter(x -> y == x.var.constant,z)), collect(groupby(capaResi_df,[:Ts_expSup,:Ts_disSup,:Exc,:R_sort])))...),Not([:R_sort]))
		# since residuals for directins were set to value of smaller direction are variables are not directed anymore
		adjVar_df[!,:dir] .= false; capaResi_df[!,:dir] .= false
	end

	prepExc_dic[:capaExc] = (var = unique(adjVar_df), resi = orderDf(capaResi_df))
end

# ! create exchange variables
function createExcVar!(part::ExcPart,ts_dic::Dict{Tuple{Int,Int},Array{Int,1}},anyM::anyModel)
	# ! extend capacity variables to dispatch variables
	capa_df = unique(flipExc(part.var[:capaExc][!,Not([:var,:dir])]))
	
	# add carriers to capacity variables
	capa_df[!,:C] .= fill(collect(part.carrier),size(capa_df,1))
	capa_df = flatten(capa_df,:C)

	# add levels for dispatch variables and extend
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
	if !isempty(part.par[:avaExc].data) && 0.0 in part.par[:avaExc].data[!,:val]
		disp_df = filter(x -> x.val != 0.0, matchSetParameter(disp_df,part.par[:avaExc],anyM.sets))[!,Not(:val)]
	end

	# computes value to scale up the global limit on dispatch variable that is provied per hour and create variables
	part.var[:exc] = orderDf(createVar(disp_df,"exc",getUpBound(disp_df,anyM.options.bound.disp / anyM.options.scaFac.dispExc,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.dispExc))
end

#endregion

#region # * create exchange related constraints

# ! connect capacity and expansion constraints for exchange
function createCapaExcCns!(part::ExcPart,anyM::anyModel)

	if part.decomm == :none
		capaVar_df = rename(part.var[:capaExc],:var => :capaVar)
	else
		capaVar_df = rename(part.var[:insCapaExc],:var => :capaVar)
	end

	if :expExc in keys(part.var)
		expVar_df = flatten(part.var[:expExc],:Ts_disSup)[!,Not(:Ts_exp)]

		cns_df = innerjoin(capaVar_df, combine(groupby(expVar_df,[:Ts_disSup, :R_from, :R_to, :C]), :var => (x -> sum(x)) => :expVar); on = [:Ts_disSup, :R_from, :R_to, :C])

		# prepare, scale and create constraints
		cns_df[!,:cnsExpr] = map(x -> x.capaVar - x.capaVar.constant - x.expVar, eachrow(cns_df))
		cns_df = intCol(cns_df,:dir) |> (x -> orderDf(cns_df[!,[x...,:cnsExpr]]))
		scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)
		part.cns[:excCapa] = createCns(cnsCont(cns_df,:equal),anyM.optModel)
	end

	# create and control operated capacity variables
	if anyM.options.decommExc != :none && :capaExc in keys(part.var)
		# constraints for operated capacities are saved into a dictionary of containers and then actually created
		cns_dic = Dict{Symbol,cnsCont}()
		createOprVarCns!(part,cns_dic,anyM)
		for cnsSym in keys(cns_dic)
			scaleCnsExpr!(cns_dic[cnsSym].data,anyM.options.coefRng,anyM.options.checkRng)
			part.cns[cnsSym] = createCns(cns_dic[cnsSym],anyM.optModel)
		end
	end
end

# ! create capacity restriction for exchange
function createRestrExc!(ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},part::ExcPart,anyM::anyModel)

	# create entries in both directions where exchange is directed
	if part.decomm != :none
		capaVar_df = vcat(filter(r -> !r.dir,part.var[:capaExc]),flipExc(filter(r -> r.dir,part.var[:capaExc])))
	else
		capaVar_df = part.var[:capaExc]
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
		restr_arr[x[1]] = prepareRestrExc(flatten(cns_df,:scr),ts_dic,part,anyM)
	end

	anyM.parts.exc.cns[:excRestr] = createCns(cnsCont(vcat(restr_arr...),:smaller),anyM.optModel)
end

# ! prepare capacity restriction for specific carrier
function prepareRestrExc(cns_df::DataFrame,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},part::ExcPart,anyM::anyModel)

	c_int = cns_df[1,:C]
	leafes_arr = filter(y -> isempty(anyM.sets[:C].nodes[y].down), [c_int,getDescendants(c_int,anyM.sets[:C],true)...])
	cRes_tup = anyM.cInfo[c_int] |> (x -> (Ts_dis = x.tsDis, R_from = x.rDis, R_to = x.rDis))

	# extend constraint dataframe to dispatch levels
	cns_df[!,:Ts_dis] .= map(x -> ts_dic[x,cRes_tup.Ts_dis],cns_df[!,:Ts_disSup])
	cns_df = flatten(cns_df,:Ts_dis)

	# resize capacity variables
	cns_df[!,:capa]  = cns_df[!,:capa] .* map(x -> anyM.supTs.sca[(x,cRes_tup.Ts_dis)], cns_df[!,:Ts_disSup])

	# filter relevant dispatch variables
	relDisp_df = filter(x -> x.C in leafes_arr, part.var[:exc])

	# first aggregate symmetric and directed entries in one direction, then directed entries in the other direction
	cns_df[!,:disp] = aggUniVar(relDisp_df,cns_df,[:Ts_dis,:R_from,:R_to,:scr],cRes_tup,anyM.sets)
	dir_arr = findall(.!cns_df[!,:dir])
	cns_df[dir_arr,:disp] = cns_df[dir_arr,:disp] .+ aggUniVar(switchExcCol(relDisp_df),cns_df[dir_arr,:],[:Ts_dis,:R_from,:R_to,:scr],cRes_tup,anyM.sets)

	# add availablities to dataframe
	cns_df = matchSetParameter(cns_df,part.par[:avaExc],anyM.sets, newCol = :avaSym)

	if :avaExcDir in keys(part.par)
		dirAva_df = matchSetParameter(cns_df[!,intCol(cns_df,:dir)],part.par[:avaExcDir],anyM.sets, newCol = :avaDir)
		cns_df = joinMissing(cns_df,dirAva_df,[:Ts_disSup,:Ts_dis,:R_from,:R_to,:C,:dir],:left, Dict(:avaDir => nothing))
	else
		cns_df[!,:avaDir] .= nothing
	end

	# prepare, scale and create constraints
	cns_df[!,:cnsExpr] = map(x -> x.disp  - x.capa * (isnothing(x.avaDir) ? x.avaSym : x.avaDir), eachrow(cns_df))
	scaleCnsExpr!(cns_df,anyM.options.coefRng,anyM.options.checkRng)

	return select(cns_df,intCol(cns_df,:cnsExpr))
end

#endregion

#region # * utility functions for exchange

# ! obtain values for exchange losses
function getExcLosses(exc_df::DataFrame,excPar_dic::Dict{Symbol,ParElement},sets_dic::Dict{Symbol,Tree})
	lossPar_obj = copy(excPar_dic[:lossExc])
	if :R_from in namesSym(lossPar_obj.data) && :R_to in namesSym(lossPar_obj.data)
		lossPar_obj.data = flipExc(lossPar_obj.data)
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
