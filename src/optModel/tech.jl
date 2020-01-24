
# XXX iteration over all technologies to create variables and constraints
function iterateOverTech!(techIdx_arr::Array{Int,1},prepVar_dic::Dict{Int,Dict{Symbol,NamedTuple}},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Int64},parDef_dic::Dict{Symbol,NamedTuple},anyM::anyModel)
	@threads for t in techIdx_arr
		newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
		ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

		part = anyM.parts.tech[t]
		prepTech_dic = prepVar_dic[t]
		# presets all dispatch parameter and obtains mode-dependant variables
		modeDep_dic = presetDispatchParameter!(t,part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

		# creates expansion and capacity variables
		createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

		# connect capacity and expansion variables
		if part.type != :stock createCapaCns!(part,prepTech_dic,anyM) end

		# create and control commissioned capacity variables
		if anyM.options.decomm != :none createCommVarCns!(part,anyM) end
		produceMessage(anyM.options,anyM.report, 3," - Created all variables and constraints related to expansion and capacity for technology $(createFullString(t,anyM.sets[:Te]))")

		# create dispatch variables
		createDispVar!(part,modeDep_dic,ts_dic,r_dic,anyM)
		produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(createFullString(t,anyM.sets[:Te]))")

		# create conversion balance for conversion technologies
		if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn))))
			createConvBal!(part,anyM)
			produceMessage(anyM.options,anyM.report, 3," - Created conversion balance for technology $(createFullString(t,anyM.sets[:Te]))")
		end

		# create storage balance for storage technologies
		if :stLvl in keys(part.var)
			createStBal!(part,anyM)
			produceMessage(anyM.options,anyM.report, 3," - Created storage balance for technology $(createFullString(t,anyM.sets[:Te]))")
		end

		# create capacity restrictions
		createCapaRestr!(part,ts_dic,r_dic,anyM)
		produceMessage(anyM.options,anyM.report, 3," - Created capacity restrictions for technology $(createFullString(t,anyM.sets[:Te]))")

		# create ratio constraints
		if any(map(x -> occursin("ratioEner",string(x)), collect(keys(part.par))))
			createRatioCns!(part,anyM)
			produceMessage(anyM.options,anyM.report, 3," - Created constraints controlling energy ratios for technology $(createFullString(t,anyM.sets[:Te]))")
		end
		produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints for technology $(createFullString(t,anyM.sets[:Te]))")
	end
end

# <editor-fold desc= prepare to create expansion and capacity variables"

# XXX sets dimensions for expansion and capacity variables
function prepareTechs!(techIdx_arr::Array{Int,1},prepVar_dic::Dict{Int,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)
	for t in techIdx_arr
		prepTech_dic = Dict{Symbol,NamedTuple}()
		part = anyM.parts.tech[t]

	    # dimension of expansion and corresponding capacity variables
	    if part.type != :stock
	        prepareExpansion!(prepTech_dic, tsYear_dic, part, t, anyM)

			for expan in collect(keys(prepTech_dic))
				prepareCapacity!(part,prepTech_dic,vcat(map(x -> x[!,removeVal(x)],prepTech_dic[expan])...),Symbol(replace(string(expan),"exp" => "capa")),anyM, tech = t)
			end
		end

		# check for capacities variables that have to be created, because of residual capacities provided
		addResidualCapa!(prepTech_dic, part, t, anyM)

		# map required capacity constraints
		createCapaRestrMap!(t, anyM)

		# if any capacity variables were prepared, add these to overall dictionary
	    if !isempty(prepTech_dic) prepVar_dic[t] = prepTech_dic end
	end
end

# XXX dimensions for expansion variables
function prepareExpansion!(prepTech_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},part::AbstractModelPart,t::Int,anyM::anyModel)

	# extract tech info
	carGrp_ntup = part.carrier
	balLvl_ntup = part.balLvl
	defPar_tup = tuple(keys(part.par)...)

	tsExp_arr, rExp_arr   = [getfield.(getNodesLvl(anyM.sets[x[2]], balLvl_ntup.exp[x[1]]),:idx) for x in enumerate([:Ts,:R])]
	tsExpSup_arr = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), tsExp_arr)
	if anyM.options.interCapa != :linear tsExp_arr = map(x -> [minimum(x)],tsExp_arr) end

	expDim_arr = vcat(collect(Iterators.product(Iterators.zip(tsExp_arr,tsExpSup_arr),rExp_arr))...)
	allMap_df =  getindex.(expDim_arr,1) |> (x -> DataFrame(Ts_exp = getindex.(x,1), Ts_expSup = getindex.(x,2), R_exp = getindex.(expDim_arr,2), Te = fill(t,length(expDim_arr))))

	# prepares expansion dimensions for conversion capacity
	if !isempty(intersect((:gen,:use),keys(carGrp_ntup)))
		# filters cases where expansion is fixed to zero
		convMap_df = removeEntries([filterZero(allMap_df,getLimPar(anyM.parts.lim,:expConvFix, anyM.sets[:Te], tech = t),anyM)],allMap_df)

		if !isempty(convMap_df) prepTech_dic[:expConv] =  (var = addSupTsToExp(convMap_df,part.par,:Conv,tsYear_dic,anyM), ratio = DataFrame(), resi = DataFrame()) end
	end

	stCar_arr::Array{Int,1} = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))

	# prepares expansion dimensions for storage capacity
	if !isempty(stCar_arr)
		stMap_df = by(allMap_df,names(allMap_df),C = :Te => x -> stCar_arr)

		for st in (:StIn, :StOut, :StSize)
			remove_arr = Array{DataFrame,1}()

			# filters cases where expansion is fixed to zero
			if Symbol(:exp,st,:Fix) in defPar_tup
				push!(remove_arr,filterZero(stMap_df,getLimPar(anyM.parts.lim,Symbol(:exp,st,:Fix), anyM.sets[:Te], tech = t),anyM))
			end

			remove_arr, ratioTab_df = findStorageRatio(t,stMap_df,st, remove_arr, part, :exp, anyM)
			specStMap_df = removeEntries(remove_arr,stMap_df)

			if !(isempty(specStMap_df) && isempty(ratioTab_df))
				prepTech_dic[Symbol(:exp,st)] =  (var = addSupTsToExp(specStMap_df,part.par,st,tsYear_dic,anyM), ratio = addSupTsToExp(ratioTab_df,part.par,st,tsYear_dic,anyM), resi = DataFrame())
			end
		end
	end
end

# XXX dimensions for capacity variables
function prepareCapacity!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},exp_df::DataFrame,capaVar::Symbol,anyM::anyModel; tech::Int = 0)

	# XXX initialize assignments and data
	defPar_tup = tuple(keys(part.par)...)
	techType_sym = :type in fieldnames(typeof(part)) ? part.type : :mature

	capaVar_df = expandExpToCapa(exp_df)

	# groups by expansion time steps in case of mature technologies
	if techType_sym == :mature
		select!(capaVar_df,Not(:Ts_expSup))
		capaVar_df = unique(capaVar_df)
		capaVar_df[!,:Ts_expSup] .= 0
	end

	# filters cases where capacity is fixed to zero
    varFix_sym = Symbol(capaVar,:Fix)

	if varFix_sym in defPar_tup
		capaVar_df = removeEntries([filterZero(capaVar_df,getLimPar(anyM.parts.lim,Symbol(capaVar,:Fix),anyM.sets[:Te], tech = tech),anyM)],capaVar_df)
	end

	# for exchange capacities add column to indicate these values are symmetric
	if capaVar == :capaExc capaVar_df[!,:dir] .= false; select!(capaVar_df,Not(:Ts_expSup)) end

	prep_dic[capaVar] =  (var = orderDf(capaVar_df), ratio = DataFrame(), resi = DataFrame())
end

# XXX checks if a storage capacity is defined via a ratio somewhere
function findStorageRatio(t_int::Int,find_df::DataFrame,st_sym::Symbol,remove_arr::Array{DataFrame,1},part::AbstractModelPart,kind_sym::Symbol,anyM::anyModel)

	expTypeRatio_dic = Dict(:StIn => :stInToConv, :StOut => :stOutToStIn, :StSize => :sizeToStIn)
	strRatio_dic = Dict(:StIn => "storage input", :StOut => "storage output", :StSize => "storage size")
	strLim_dic = Dict(:Up => "upper limits", :Low => "lower limits", :Fix => "fixed limits",:Resi => "residual capacities")

	ratio_sym = expTypeRatio_dic[st_sym]

	if ratio_sym in tuple(keys(part.par)...)
		ratioTab_df = filter(r -> r.ratio != 0, matchSetParameter(find_df,part.par[ratio_sym],anyM.sets, newCol = :ratio))
		push!(remove_arr, ratioTab_df[!,Not(:ratio)])
	else
		ratioTab_df = filter(x -> false,find_df)
	end

	# writes a report, if limits (upper/lower/fixed) on the storage expansion were ignored due to these ratios provided
	if !isempty(ratioTab_df) && (kind_sym == :exp || (kind_sym == :capa && part.type == :stock) )
		for limPar in intersect(keys(part.par),map(x -> Symbol(kind_sym,st_sym,x),kind_sym == :exp ? [:Up,:Low,:Fix] : [:Up,:Low,:Fix,:Resi]))
			both_df = getLimPar(anyM.parts.lim,limPar,anyM.sets[:Te], tech = t_int) |> (x -> join(ratioTab_df,x, on = intersect(intCol(ratioTab_df),intCol(x)), kind = :inner))
			if !isempty(both_df)
				push!(anyM.report,(1,:variable,:expansion,"for $(join(part.name, " < ")) $(strLim_dic[Symbol(split(string(limPar),string(st_sym))[end])]) for $(strRatio_dic[st_sym]) were ignored since an conversion/storage input ratio was provided"))
			end
		end
	end

	return remove_arr, ratioTab_df
end

# </editor-fold>

# <editor-fold desc= create technology related variables"

# create expansion and capacity variables
function createExpCap!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},anyM::anyModel,ratioVar_dic::Dict{Symbol,Pair{String,String}} = Dict{Symbol,Pair{String,String}}())
	for expVar in sort(collect(keys(prep_dic)))
		varMap_tup = prep_dic[expVar]
		# create dataframe of capacity or expansion variables by creating the required capacity variables and join them with pure residual values
		var_df = createVar(varMap_tup.var,string(expVar),anyM.options.bound.exp,anyM.optModel,anyM.lock,anyM.sets)
		if !isempty(varMap_tup.resi)
			if expVar == :capaExc # flips and repeats entries for directed exchange variabes before moving on
				var_df = filter(r -> r.dir,var_df) |> (x -> vcat(filter(r -> !r.dir,var_df),vcat(x,rename(x,replace(names(x),:R_to => :R_from, :R_from => :R_to)))))
			end
			join_arr = intCol(var_df,:dir)
			var_df = by(joinMissing(var_df,varMap_tup.resi[!,vcat(:var,join_arr...)], join_arr, :outer, Dict(:var => AffExpr(),:var_1 => AffExpr()),true),intCol(var_df,:dir),var = [:var,:var_1] => x -> sum(x))
		end

		# expands table of expansion variables to supordinate timesteps and modifies expansion variable accordingly
		if occursin("exp",string(expVar)) && !isempty(var_df)
			noExpCol_arr = intCol(var_df)
			allDf_arr = map(eachrow(var_df)) do x
				l_int = length(x.Ts_disSup)
				rem_df = repeat(DataFrame(x[noExpCol_arr]), inner = l_int, outer = 1)
				ext_df = DataFrame(Ts_expSup = x.Ts_expSup, Ts_disSup = x.Ts_disSup, var = x.var ./ fill(l_int,l_int) )
				return hcat(rem_df,ext_df)
			end
			var_df = vcat(allDf_arr...)
		end

		# check for ratios for expansion or (in case of stock technologies) capacities
		if !isempty(varMap_tup.ratio)
			ratioVar_sym = Symbol(replace(string(expVar),ratioVar_dic[Symbol(replace(replace(string(expVar),"exp" => ""),"capa" => ""))]))
			if occursin("exp",string(expVar)) # ratios controlling expansion
				noExpCol_arr = vcat(:ratio,intCol(var_df)...)

				ratio_arr = map(eachrow(varMap_tup.ratio)) do x
					l_int = length(x.Ts_expSup)
					rem_df = repeat(DataFrame(x[noExpCol_arr]), inner = l_int, outer = 1)
					ext_df = DataFrame(Ts_expSup = x.Ts_expSup)
					return hcat(rem_df,ext_df)
				end
				preRatio_df = vcat(ratio_arr...)
				join_arr = intCol(ratio_arr[1])
			else # ratios controlling stock capacities
				preRatio_df = varMap_tup.ratio
				join_arr = intCol(preRatio_df)
			end

			# join ratios and corresponding
			ratio_df = join(preRatio_df,part.var[ratioVar_sym]; on = join_arr, kind = :inner)
			ratio_df[!,:var] = ratio_df[!,:var] .* ratio_df[!,:ratio]
			var_df = ratio_df[!,Not(:ratio)] |> (x -> vcat(x,join(var_df,x, on = join_arr, kind = :anti)))
		end

		if !isempty(var_df)	part.var[expVar] = orderDf(var_df) end
	end
end

# XXX create all dispatch variables
function createDispVar!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Int64},anyM::anyModel)
	# assign relevant availability parameters to each type of variable
	relAva_dic = Dict(:gen => (:avaConv,), :use => (:avaConv,), :stIntIn => (:avaConv, :avaStIn), :stIntOut => (:avaConv, :avaStOut), :stExtIn => (:avaStIn,), :stExtOut => (:avaStOut,), :stLvl => (:avaStSize,))

	for va in collect(keys(part.carrier)) |> (x -> :capaStIn in keys(part.var) ? [:stLvl,x...]  : x) # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use)
		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(part.var[:capaConv])[!,Not(:var)]
			basis_df[!,:C] .= [collect(getfield(part.carrier,va))]
			basis_df = orderDf(flatten(basis_df,:C))
		else
			basis_df= orderDf(copy(part.var[:capaStIn])[!,Not(:var)])
		end

		# adds temporal and spatial level to dataframe
		cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, part.disAgg ? anyM.cInfo[x].rExp : anyM.cInfo[x].rDis) for x in unique(basis_df[!,:C]))
		basis_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],basis_df[!,:C])
		basis_df[!,:lvlR] = map(x -> cToLvl_dic[x][2],basis_df[!,:C])
		allVar_df = orderDf(expandExpToDisp(basis_df,ts_dic,r_dic,true))

		# add mode dependencies
		modeDep_df = copy(modeDep_dic[va])
		modeDep_df[!,:M] .= isempty(part.modes) ? [0] : [collect(part.modes)]
		modeDep_df = flatten(modeDep_df,:M)

		allVar_df = joinMissing(allVar_df,modeDep_df,names(modeDep_dic[va]),:left,Dict(:M => 0))

		# filter entries where availability is zero
		for avaPar in relAva_dic[va]
			if !isempty(part.par[avaPar].data) && 0.0 in part.par[avaPar].data[!,:val]
				allVar_df = filter(x -> x.val != 0.0,  matchSetParameter(allVar_df,part.par[avaPar],anyM.sets))[!,Not(:val)]
			end
		end

		# computes value to scale up the global limit on dispatch variable that is provied per hour and create variable
		part.var[va] = orderDf(createVar(allVar_df,string(va), getUpBound(allVar_df,anyM),anyM.optModel,anyM.lock,anyM.sets))
	end
end

# XXX create variables and constraints regarding commissioned variables
function createCommVarCns!(part::AbstractModelPart,anyM::anyModel)
	for capaVar in filter(x -> occursin("capa",string(x)),keys(part.var))
		commVar_sym = string(capaVar) |> (x -> Symbol(:comm,uppercase(x[1]),x[2:end]))
		# XXX create commissioned variable
		var_df = copy(part.var[capaVar])[!,Not(:var)]
		var_df = createVar(var_df,string(commVar_sym),nothing,anyM.optModel,anyM.lock,anyM.sets)

		part.var[commVar_sym] = orderDf(var)

		# XXX create constraint to connect commissioned and installed capacity
		lock(anyM.lock)
		var_df[!,:cns] = map(x -> @constraint(anyM.optModel,x[1] <= x[2]),zip(var_df[!,:var],part.var[capaVar][!,:var]))
		unlock(anyM.lock)
		part.cns[commVar_sym] = orderDf(var_df[!,Not(:var)])

		# XXX create constraint to prevent re-commissioning of capacity once decommissioned
		if anyM.options.decomm == :decomm
			# add previous period and its capacity variable to table
			prevTs_dic = Dict(x => findall(x .== anyM.supTs.step)[1]-1 for x in anyM.supTs.step[2:end])
			select!(var_df, Not(:cns))
			cns_df = rename(filter(r -> r.Ts_disSup != anyM.supTs.step[1],var_df),:var => :commNow)
			cns_df[!,:Ts_disSupPrev] = map(x -> prevTs_dic[x] ,cns_df[!,:Ts_disSup])
			cns_df = rename(join(cns_df,var_df; on = intCol(var_df,:dir) |> (x -> Pair.(replace(x,:Ts_disSup => :Ts_disSupPrev),x)), kind = :inner),:var => :commPrev)

			# add expansion variable to dataframe
			if !(:type in fieldnames(typeof(part))) || part.type != :stock
				exp_df = part.var[Symbol(replace(string(capaVar),"capa" => "exp"))][!,Not(:Ts_disSup)]
				join_arr = filter(x -> x != :Ts_expSup,intCol(var_df))

				cns_df = joinMissing(cns_df,exp_df, Pair.(join_arr,replace(join_arr,:Ts_disSup => :Ts_expSup)),:left,Dict(:var => AffExpr(),:Ts_exp => 0))
				cns_df = rename(cns_df[!,Not(:Ts_exp)],:var => :expNow)
			else
				cns_df[!,:expNow] .= AffExpr()
			end

			# add residual capacities of current and previous period

			joinResi_arr = filter(x -> x != :Ts_disSupPrev, intCol(cns_df,:dir))
			cns_df = rename(join(cns_df,part.var[capaVar],on = joinResi_arr, kind = :inner),:var => :resiNow)
			cns_df[!,:resiNow] = getfield.(cns_df[!,:resiNow],:constant)
			cns_df = rename(joinMissing(cns_df, part.var[capaVar], Pair.(replace(joinResi_arr,:Ts_disSup => :Ts_disSupPrev),joinResi_arr),:left, Dict(:resiNow => AffExpr(),:var => AffExpr())),:var => :resiPrev)
			cns_df[!,:resiPrev] = getfield.(cns_df[!,:resiPrev],:constant)

			# create actual constraint
			cns_df = filter(x -> x.expNow != AffExpr() || x.resiNow - x.resiPrev > 0.0,cns_df)
			lock(anyM.lock)
			cns_df[!,:cns] = map(x -> @constraint(anyM.optModel,x.commNow <= x.commPrev + x.expNow + (x.resiNow - x.resiPrev |> (l -> l > 0.0 ? l : 0.0))),eachrow(cns_df))
			unlock(anyM.lock)
			cns_ele = orderDf(cns_df[!,Not([:Ts_disSupPrev,:commNow,:commPrev,:expNow,:resiNow,:resiPrev])])
			part.cns[string(commVar_sym) |> (x -> Symbol(:re,uppercase(x[1]),x[2:end]))] = cns_ele
		end
	end
end

# XXX capacity values for stock technologies
function addResidualCapa!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,t::Int,anyM::anyModel)

	carGrp_ntup = part.carrier
	stCar_arr = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))
	defPar_tup = tuple(keys(part.par)...)

	# checks conversion capacities for stock data
	if !isempty(intersect((:gen,:use),keys(carGrp_ntup)))
		permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step))...),i) for i in (1,2)]
		potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(t,length(permutDim_arr[1])))
		# filters stock technologies with residual values provided and saves these in dictionary
		capaResi_df = checkResiCapa(:capaConv,potCapa_df, part, anyM)

		if !isempty(capaResi_df)
			mergePrepDic!(:capaConv,prepTech_dic,capaResi_df)
		end
	end

	# checks storage capacities for stock data
	if !isempty(stCar_arr)
		permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step,stCar_arr))...),i) for i in (1,2,3)]
		potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], C = permutDim_arr[3], Te = fill(t,length(permutDim_arr[1])))

		# loops over different type of storage
		for st in (:StIn, :StOut, :StSize)
			capaResi_df = checkResiCapa(Symbol(:capa,st),potCapa_df, part, anyM)
			capa_sym = Symbol(:capa,st)
			remove_arr, ratioTab_df = findStorageRatio(t,potCapa_df,st, Array{DataFrame,1}(), part, :capa, anyM)
			if !isempty(capaResi_df) capaResi_df = removeEntries(remove_arr,capaResi_df) end

			if !(isempty(capaResi_df) && isempty(ratioTab_df))
				mergePrepDic!(capa_sym,prepTech_dic,capaResi_df,ratioTab_df)
			end
		end
	end
end

# </editor-fold>

# <editor-fold desc= create technology related constraints"

# XXX create conversion balance
function createConvBal!(part::TechPart,anyM::anyModel)

	cns_df = rename(part.par[:effConv].data,:val => :eff)
	agg_arr = filter(x -> !(x in (:M, :Te)) && (part.type == :emerging || x != :Ts_expSup), intCol(cns_df))
	srcRes_ntup = part.balLvl |> (x -> part.type == :emerging ? (Ts_expSup = x.exp[1], Ts_dis = x.ref[1], R_dis = x.ref[2]) : (Ts_dis = x.ref[1], R_dis = x.ref[2]))

	# add variables via aggregation
	in_arr = intersect(keys(part.carrier),(:use,:stIntOut))
	out_arr = intersect(keys(part.carrier),(:gen,:stIntIn))

	for va in union(in_arr,out_arr)
		cns_df[!,va] = aggUniVar(part.var[va], select(cns_df,intCol(cns_df)), agg_arr, srcRes_ntup, anyM.sets)
	end

	# aggregate in and out variables respectively
	cns_df[!,:in] = map(x -> sum(x),eachrow(cns_df[!,in_arr]))
	cns_df[!,:out] = map(x -> sum(x),eachrow(cns_df[!,out_arr]))
	select(cns_df,Not(vcat(in_arr,out_arr)))

	# create actual constraint
	lock(anyM.lock)
	cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.in*x.eff ==  x.out),eachrow(cns_df))
	unlock(anyM.lock)
	part.cns[:convBal] = orderDf(cns_df[!,[intCol(cns_df)...,:cns]])
end

# XXX create storage balance
function createStBal!(part::TechPart,anyM::anyModel)

	# XXX get variables for storage level
	# get variables for current storage level
	cns_df = rename(part.var[:stLvl],:var => :stLvl)
	cnsDim_arr = filter(x -> x != :Ts_disSup, intCol(cns_df))

	# join variables for next storage level
	tsChildren_dic = Dict((x,y) => getDescendants(x,anyM.sets[:Ts],false,y) for x in anyM.supTs.step, y in unique(map(x -> getfield(anyM.sets[:Ts].nodes[x],:lvl), cns_df[!,:Ts_dis])))
	lastFirstTs_dic = Dict(maximum(tsChildren_dic[z]) => minimum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
	lastTs_arr = collect(keys(lastFirstTs_dic))

	cns_df[!,:Ts_disNext] = map(x -> x in lastTs_arr ? lastFirstTs_dic[x] : x + 1, cns_df[!,:Ts_dis])
	cns_df = rename(joinMissing(cns_df,part.var[:stLvl], intCol(part.var[:stLvl]) |> (x -> Pair.(replace(x,:Ts_dis => :Ts_disNext),x)), :left, Dict(:var => AffExpr())),:var => :stLvlNext)

	# XXX join in and out variables and combines them with respective efficiencies
	for typ in (:in,:out)
		typVar_df = copy(cns_df[!,cnsDim_arr])
		# join both types of in or out variables
		allType_arr = intersect(keys(part.carrier),typ == :in ? (:stExtIn,:stIntIn) : (:stExtOut,:stIntOut))
		for va in allType_arr
			typVar_df = rename(joinMissing(typVar_df,part.var[va], cnsDim_arr, :inner,Dict(:var => AffExpr()))[!,Not(:Ts_disSup)],:var => va)
		end
		effPar_sym = typ == :in ? :effStIn : :effStOut
		# join all types of in and out variables and adds efficiency
		grpDim_arr = intCol(typVar_df)
		typVar_df = by(typVar_df,grpDim_arr,allVar = allType_arr => x -> sum(map(y -> getfield(x,y),allType_arr)))
		typVar_df = part.par[effPar_sym].data |> (x -> join(typVar_df,x; on = intCol(x), kind = :inner))

		if typ == :in
			typVar_df = rename(by(typVar_df,grpDim_arr, allVar = [:allVar,:val] => x -> dot(x.allVar,x.val)),:allVar => typ)
		else
			typVar_df = rename(by(typVar_df,grpDim_arr, allVar = [:allVar,:val] => x -> dot(x.allVar,1 ./ x.val)),:allVar => typ)
		end
		cns_df = joinMissing(cns_df,typVar_df, cnsDim_arr,:left, Dict(typ => AffExpr()))
	end

	# XXX adds further parameters
	# add discharge parameter, if defined
	if :stDis in keys(part.par)
		sca_arr = getScale(cns_df,anyM.sets[:Ts],anyM.supTs)
		cns_df[!,:stDis] =  1 ./ (1 .- part.par[:stDis].data[!,:val] .^ sca_arr)
	else
		cns_df[!,:stDis] .= 1.0
	end

	# add inflow parameter, if defined
	if :stInflow in keys(part.par)
		cns_df[!,:stInflow] = part.par[:stInflow].data[!,:val]
	else
		cns_df[!,:stInflow] .= 0.0
	end

	# XXX create final equation
	lock(anyM.lock)
	cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.stLvlNext * x.stDis == x.stLvl + x.stInflow + x.in - x.out),eachrow(cns_df))
	unlock(anyM.lock)
	part.cns[:stBal] = orderDf(cns_df[!,[cnsDim_arr...,:cns]])
end

# XXX create all capacity restrictions for technology
function createCapaRestr!(part::TechPart,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Int64},anyM::anyModel)

	cnstrType_dic = Dict(:out => (dis = (:gen, :stIntIn), capa = :Conv), :in => (dis = (:use,:stIntOut), capa = :Conv),
							:stIn => (dis = (:stExtIn, :stIntIn), capa = :StIn), :stOut => (dis = (:stExtOut, :stIntOut), capa = :StOut), :stSize => (dis = (:stLvl,), capa = :StSize))

	capa_sym = anyM.options.decomm != :none ? :capaComm : :capa
	capaRestr_gdf = groupby(part.capaRestr,:cnstrType)

	# loop over groups of capacity restrictions (like out, stIn, ...)
	for restrGrp in capaRestr_gdf
		# relevant capacity variables
		type_sym = Symbol(restrGrp.cnstrType[1])
		info_ntup = cnstrType_dic[type_sym]

		allCns_arr = Array{DataFrame}(undef,size(restrGrp,1))

		# loop over indiviudal constraints
		for (idx,restr) in enumerate(eachrow(restrGrp))
			allCns_arr[idx] = createRestr(part,copy(part.var[Symbol(capa_sym,info_ntup.capa)]),restr,type_sym,info_ntup,ts_dic,r_dic,anyM.sets,anyM.supTs)
		end

		allCns_df = vcat(allCns_arr...)

		lock(anyM.lock)
		allCns_df[!,:cns] = map(x -> @constraint(anyM.optModel,  0.01 * x.disp <= 0.01 * x.capa),eachrow(allCns_df))
		unlock(anyM.lock)

		# add all constraints to part
		part.cns[Symbol(type_sym,:Restr)] = orderDf(allCns_df[!,[intCol(allCns_df)...,:cns]])
	end
end

# XXX sub-function to create restriction
function createRestr(part::TechPart, capaVar_df::DataFrame, restr::DataFrameRow, type_sym::Symbol, info_ntup::NamedTuple,
															ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Int64}, sets_dic::Dict{Symbol,Tree}, supTs_ntup::NamedTuple)

	conv_boo = type_sym in (:out,:in)
	dim_arr = conv_boo ? [:Ts_expSup,:Ts_dis,:R_dis,:Te] : [:Ts_expSup,:Ts_dis,:R_dis,:C,:Te]
	agg_arr = conv_boo ? [:Ts_expSup,:Ts_dis,:R_dis] : [:Ts_expSup,:Ts_dis,:R_dis,:C] |> (x -> filter(x -> part.type == :emerging || x != :Ts_expSup,x))

	# determines dimensions for aggregating dispatch variables
	capaVar_df[!,:lvlTs] .= restr.lvlTs
	capaVar_df[!,:lvlR] .= restr.lvlR

	# scale capacity variables temporal (expect for stSize since these are already provided in energy units)
	if type_sym != :stSize
		capaVar_df[!,:var]  = capaVar_df[!,:var] .* map(x -> supTs_ntup.sca[(x.Ts_disSup,x.lvlTs)],	eachrow(capaVar_df[!,[:Ts_disSup,:lvlTs]]))
	end

	# replaces expansion with dispatch regions
	grpCapaVar_df = copy(capaVar_df) |> (y -> unique(by(y,names(y),R_dis = [:R_exp,:lvlR] => x -> r_dic[(x[1][1],x[2][1])])[!,Not([:R_exp,:lvlR])]))

	if restr.lvlR < part.balLvl.exp[2] # aggregate capacity variables spatially, if necessary
		resExp_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = part.balLvl.exp[1], Ts_disSup = anyM.supTs.lvl, R_dis = restr.lvlR) : (Ts_disSup = anyM.supTs.lvl, R_dis = restr.lvlR)
		grpCapaVar_df[!,:var] = aggUniVar(rename(capaVar_df,:R_exp => :R_dis),grpCapaVar_df,replace(agg_arr,:Ts_dis => :Ts_disSup),resExp_ntup,sets_dic)
	end

	# expand capacity to dimensions of dispatch variables
	capaDim_df = by(grpCapaVar_df[!,Not(:var)],names(grpCapaVar_df[!,Not(:var)]),Ts_dis = [:Ts_disSup, :lvlTs] => x -> ts_dic[(x[1][1],x[2][1])])[!,Not(:lvlTs)]

	select!(grpCapaVar_df,Not(:lvlTs))

	# obtain all relevant dispatch variables
	dispVar_arr = type_sym != :stSize ? intersect(keys(part.carrier),info_ntup.dis) : collect(info_ntup.dis)
	resDis_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = part.balLvl.exp[1], Ts_dis = restr.lvlTs, R_dis = restr.lvlR) : (Ts_dis = restr.lvlTs, R_dis = restr.lvlR)
	for va in dispVar_arr
		# filter dispatch variables not belonging to relevant carrier
		allVar_df = filter(r -> r.C in restr.car, part.var[va])[!,Not(:Ts_disSup)]

		# get availablity (and in case of paramter of type out also efficiency since capacities refer to input capacity) parameter and add to dispatch variable
		ava_arr = matchSetParameter(allVar_df,part.par[Symbol(:ava,info_ntup.capa)],sets_dic, newCol = :ava)[!,:ava]
		if type_sym == :out
			ava_arr = matchSetParameter(allVar_df,part.par[Symbol(:effConv)],sets_dic,newCol = :eff)[!,:eff] .* ava_arr
		end
		allVar_df[!,:var] = allVar_df[!,:var] .* 1 ./ ava_arr
		# aggregate dispatch variables
		capaDim_df[!,va] = aggUniVar(allVar_df, select(capaDim_df,intCol(capaDim_df)), agg_arr, resDis_ntup, sets_dic)
	end
	# sum dispatch variables and filter cases without any
	capaDim_df[!,:disp] = map(x -> sum(x),eachrow(capaDim_df[!,dispVar_arr]))
	select!(capaDim_df,Not(dispVar_arr))
	capaDim_df = filter(x -> !(x.disp == AffExpr()),capaDim_df)

	# join capacity and dispatch variables to create final constraint
	grpCapaVar_df = by(grpCapaVar_df,replace(dim_arr,:Ts_dis => :Ts_disSup),capa = [:var] => x -> sum(x))
	cns_df = join(capaDim_df,grpCapaVar_df,on = intCol(grpCapaVar_df),kind = :inner)
	return cns_df
end

# XXX create ratio constraints (Fix, Low and Up for use and gen)
function createRatioCns!(part::TechPart,anyM::anyModel)

	# collects all tables for equations
	for type in intersect(keys(part.carrier),(:use, :gen)), limit in (:Up, :Low, :Fix)

		typeUp_sym = string(type) |> (x -> Symbol(uppercase(x[1]),x[2:end]))
		ratioName_sym = Symbol(:ratioEner,typeUp_sym,limit)
		if !(ratioName_sym in keys(part.par)) continue end

		# obtain variable name and parameter data
		cns_df = rename(copy(part.par[ratioName_sym].data),:val => :ratio)

		# joins parameter data with ratio controlled variable and all variables
		agg1_arr = filter(r -> r != :Te && (part.type == :emerging || r != :Ts_expSup), intCol(cns_df))
		srcRes_ntup = (anyM.sets[:Ts].nodes[cns_df[1,:Ts_dis]].lvl, anyM.sets[:R].nodes[cns_df[1,:R_dis]].lvl) |> (x -> part.type == :emerging ? (Ts_expSup = part.balLvl.exp[1], Ts_dis = x[1], R_dis = x[2]) : (Ts_dis = x[1], R_dis = x[2]))

		cns_df[!,:ratioVar] = aggUniVar(part.var[type], select(cns_df,intCol(cns_df)), agg1_arr, srcRes_ntup, anyM.sets)
		cns_df[!,:allVar] =	aggUniVar(part.var[type], select(cns_df,intCol(cns_df)), filter(r -> r != :C, agg1_arr), srcRes_ntup, anyM.sets)

		# create corresponding constraint
		lock(anyM.lock)
		if occursin("Fix",string(limit))
			cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.ratioVar ==  x.allVar * x.ratio),eachrow(cns_df))
		elseif occursin("Low",string(limit))
			cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.ratioVar >=  x.allVar * x.ratio),eachrow(cns_df))
		else
			cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, x.ratioVar <=  x.allVar * x.ratio),eachrow(cns_df))
		end
		unlock(anyM.lock)

		part.cns[ratioName_sym] = orderDf(cns_df[!,[intCol(cns_df)...,:cns]])
	end
end

# </editor-fold>
