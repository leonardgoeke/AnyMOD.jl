
# <editor-fold desc= prepare and create exchange related variables"

# XXX prepare expansion and capacity variables for exchange
function prepareExcExpansion!(partExc::OthPart,partLim::OthPart,prepExc_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	# XXX determine dimensions of expansion variables (expansion for exchange capacities is NOT directed!)
	# get all possible dimensions of exchange
	potDim_df = DataFrame(map(x -> (lvlTs = x[2].tsExp, lvlR = x[2].rDis, C = x[1]), collect(anyM.cInfo)))
	tsLvl_dic = Dict(x => getfield.(getNodesLvl(anyM.sets[:Ts],x),:idx) for x in unique(potDim_df[!,:lvlTs]))
	rLvl_dic = Dict(x => getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in unique(potDim_df[!,:lvlR]))

	potExc_df = flatten(flatten(flatten(by(potDim_df,:C,[:lvlR,:lvlTs] => x -> (Ts_exp = map(y -> tsLvl_dic[y],x.lvlTs), R_a = map(y -> rLvl_dic[y],x.lvlR), R_b = map(y -> rLvl_dic[y],x.lvlR))),:Ts_exp),:R_a),:R_b)

	# get dimensions where exchange should actually be defined
	exExp_df = DataFrame(R_a = Int[], R_b = Int[], C = Int[])

	for excPar in intersect((:capaExcResi,:capaExcResiDir),keys(partExc.par))
		append!(exExp_df,matchSetParameter(potExc_df,partExc.par[excPar],anyM.sets,anyM.report)[!,[:R_a,:R_b,:C]])
	end

	for excPar in intersect((:capaExcUp, :capaExcLow, :capaExFix, :expExcUp, :expExcLow, :expExcFix),keys(partLim.par))
		append!(exExp_df,matchSetParameter(potExc_df,partLim.par[excPar],anyM.sets,anyM.report)[!,[:R_a,:R_b,:C]])
	end

	# ensure expansion entries are not directed
	exExp_df = unique(exExp_df) |> (x -> filter(y -> y.R_a < y.R_b,vcat(x,rename(x,replace(names(x),:R_a => :R_b, :R_b => :R_a))))) |> (z -> unique(z))

	# add supordiante timesteps of expansion
	allExExp_df = join(potExc_df,exExp_df, on = names(exExp_df), kind = :inner)
	allExExp_df[!,:Ts_expSup] = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), allExExp_df[!,:Ts_exp])

	# filter cases where expansion is fixed to zero
	if :expExcFix in keys(partExc.par)
		allExExp_df = removeEntries([filterZero(allExExp_df,getLimPar(anyM.parts.lim,:expExcFix,anyM.sets[:Te]),anyM)],allExExp_df)
	end

	# save result to dictionary for variable creation
	exp_df = addSupTsToExp(allExExp_df,partExc.par,:Exc,tsYear_dic,anyM)
	prepExc_dic[:expExc] = (var = convertExcCol(exp_df),ratio = DataFrame(), resi = DataFrame())

	return potExc_df
end

# XXX create exchange variables
function createExcVar!(partExc::OthPart,ts_dic::Dict{Tuple{Int,Int},Array{Int,1}},anyM::anyModel)
	# extend capacity variables to dispatch variables
	capa_df = partExc.var[:capaExc][!,Not([:var,:dir])] |> (x -> unique(vcat(x,rename(x,replace(names(x),:R_from => :R_to, :R_to => :R_from)))))
	cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, anyM.cInfo[x].rDis) for x in unique(capa_df[!,:C]))
	capa_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],capa_df[!,:C])
	disp_df = by(capa_df,names(capa_df),Ts_dis = [:Ts_disSup, :lvlTs] => x -> ts_dic[(x[1][1],x[2][1])])[!,Not(:lvlTs)]

	# filter entries where availability is zero
	if !isempty(partExc.par[:avaExc].data) && 0.0 in partExc.par[:avaExc].data[!,:val]
		disp_df = filter(x -> x.val != 0.0, matchSetParameter(disp_df,partExc.par[:avaExc],anyM.sets,anyM.report))[!,Not(:val)]
	end

	# computes value to scale up the global limit on dispatch variable that is provied per hour and create variables
	partExc.var[:exc] = orderDf(createVar(disp_df,"exc",getUpBound(disp_df,anyM),anyM.optModel,anyM.lock,anyM.sets))
end

# XXX add residual capacties for exchange (both symmetric and directed)
function addResidualCapaExc!(partExc::OthPart,prepExc_dic::Dict{Symbol,NamedTuple},potExc_df::DataFrame,anyM::anyModel)

	expSup_dic = Dict(x => getDescendants(x,anyM.sets[:Ts],true,anyM.supTs.lvl) for x in unique(potExc_df[!,:Ts_exp]))
	potExc_df[!,:Ts_disSup] = map(x -> expSup_dic[x],potExc_df[!,:Ts_exp])
	potExc_df = flatten(potExc_df[!,Not(:Ts_exp)],:Ts_disSup)

	# obtain symmetric residual capacites
	capaResi_df = filter(r -> r.R_a < r.R_b,checkResiCapa(:capaExc,potExc_df, partExc, anyM))

	# manipulate entries in case directed residual capacities are defined
	if :capaExcResiDir in keys(partExc.par)

		directExc_df = matchSetParameter(potExc_df,partExc.par[:capaExcResiDir],anyM.sets,anyM.report)
		directExc_df[!,:var] = map(x -> AffExpr(x), directExc_df[!,:val]); select!(directExc_df,Not(:val))

		excDim_arr = [:C, :R_a, :R_b, :Ts_disSup]
		excDimP_arr = replace(excDim_arr,:R_a => :R_b, :R_b => :R_a)

		#  entries, where a directed capacity is provided and a symmetric one already exists
		bothExc_df = vcat(join(directExc_df, capaResi_df; on = excDim_arr, kind = :inner, makeunique = true), join(directExc_df, capaResi_df; on = Pair.(excDim_arr,excDimP_arr), kind = :inner, makeunique = true))
		bothExc_df = by(bothExc_df,excDim_arr,var = [:var,:var_1] => x -> sum(x))
		if !(:var in names(bothExc_df)) bothExc_df[!,:var] = AffExpr[] end
		 # entries, where only a directed capacity was provided
		onlyDirExc_df = join(directExc_df, bothExc_df; on = excDim_arr, kind = :anti)

		# entries originally symmetric that now become directed, because a directed counterpart was introduced
		flipSym_df = join(join(capaResi_df, bothExc_df[!,Not(:var)]; on = excDim_arr, kind = :inner),bothExc_df[!,Not(:var)]; on = excDim_arr .=> excDimP_arr, kind = :anti)

		swtExc_df = vcat(bothExc_df,flipSym_df)

		# solely directed entries
		dirExc_df = vcat(onlyDirExc_df,swtExc_df)
		dirExc_df[!,:dir] .= true

		# entries entries originally symmetric that remain symmetric
		unDirExc_df = join(capaResi_df, dirExc_df; on = excDim_arr, kind = :anti)
		unDirExc_df[!,:dir] .= false

		# adjust dataframe of residual capacities according to directed values
		capaResi_df = vcat(dirExc_df,unDirExc_df)

		# adjust dataframe of capacities determining where variables will be created to reflect which of these correspond to directed cases now
		allVar_df = prepExc_dic[:capaExc].var
		if !isempty(prepExc_dic[:capaExc].var)
			undirBoth_df = vcat(dirExc_df,rename(dirExc_df,replace(names(dirExc_df),:R_a => :R_b, :R_b => :R_a)))[!,Not(:dir)]
			dirVar_df = convertExcCol(join(convertExcCol(allVar_df[!,Not(:dir)]), vcat(undirBoth_df,swtExc_df)[!,Not(:var)],on = excDim_arr, kind = :inner ))
			dirVar_df[!,:dir] .= true
			adjVar_df = vcat(dirVar_df,join(allVar_df,dirVar_df,on = [:C, :R_from, :R_to, :Ts_disSup], kind = :anti))
		else
			adjVar_df = allVar_df
		end
	else
		capaResi_df[!,:dir] .= false
		adjVar_df = prepExc_dic[:capaExc].var
	end

	# adjust dictionary accordingly
	capaResi_df[!,:Ts_expSup] .= 0
	prepExc_dic[:capaExc] = (var = adjVar_df, ratio = DataFrame(), resi = convertExcCol(capaResi_df))
end

# XXX converts table where exchange regins are given as "R_a" and "R_b" to "R_to" and "R_from" and the other way around
convertExcCol(in_df::DataFrame) = rename(in_df, names(in_df) |> (x  -> :R_a in x ? replace(x,:R_a => :R_from, :R_b => :R_to) : replace(x,:R_from => :R_a, :R_to => :R_b)))

# </editor-fold>

# <editor-fold desc= create exchange related constraints"

# XXX connect capacity and expansion constraints for exchange
function createCapaExcCns!(part::OthPart,anyM::anyModel)
	capaVar_df = rename(part.var[:capaExc],:var => :capaVar)
	if :expExc in keys(part.var)
		expVar_df = flatten(part.var[:expExc],:Ts_disSup)[!,Not(:Ts_exp)]

		cns_df = join(capaVar_df, by(expVar_df,[:Ts_disSup, :R_from, :R_to, :C], expVar = :var => x -> sum(x)); on = [:Ts_disSup, :R_from, :R_to, :C], kind = :inner)

		withlock(anyM.lock) do
			cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.capaVar - x.capaVar.constant == x.expVar),eachrow(cns_df))
		end
		part.cns[:excCapa] = intCol(cns_df,:dir) |> (x -> orderDf(cns_df[!,[x...,:cns]]))

		# create and control commissioned capacity variables
		if anyM.options.decomm != :none createCommVarCns!(part,anyM) end
	end
end

# XXX create capacity constraints for exchange
function createCapaRestrExc!(part::OthPart,anyM::anyModel)

	dispVar_df = rename(part.var[:exc],:var => :disp)
	capaVar_df = rename(part.var[:capaExc],:var => :capa)

	# join directed cases
	cns_df = rename(join(capaVar_df,dispVar_df; on = [:Ts_disSup,:R_from,:R_to,:C], kind = :inner),:disp => :dispDir)

	# also join other direction for symmetric cases
	dispVar_df[!,:dir] .= false
	cns_df = rename(joinMissing(cns_df,dispVar_df, [:Ts_disSup,:Ts_dis,:R_from,:R_to,:C,:dir] .=> [:Ts_disSup,:Ts_dis,:R_to,:R_from,:C,:dir] , :left, Dict(:disp => AffExpr())),:disp => :dispSym)

	# add availablities to dataframe
	cns_df = matchSetParameter(convertExcCol(cns_df),part.par[:avaExc],anyM.sets,anyM.report, newCol = :avaSym)

	if :avaExcDir in keys(part.par)
		dirAva_df = matchSetParameter(cns_df[!,intCol(cns_df,:dir)],part.par[:avaExcDir],anyM.sets,anyM.report, newCol = :avaDir)
		cns_df = joinMissing(cns_df,dirAva_df,[:Ts_disSup,:Ts_dis,:R_a,:R_b,:C,:dir],:left, Dict(:avaDir => nothing))
	else
		cns_df[!,:avaDir] .= nothing
	end

	# create final constraints
	withlock(anyM.lock) do
		cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, (x.dispDir + x.dispSym) *0.01 <=  0.01 * x.capa * (isnothing(x.avaDir) ? x.avaSym : x.avaDir)),eachrow(cns_df))
	end
	part.cns[:capaExcRestr] = convertExcCol(cns_df) |>  (x -> orderDf(x[!,[intCol(x,:dir)...,:cns]]) )
end

# </editor-fold>
