
# XXX iteration over all technologies to create variables and constraints
function createTech!(tInt::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

    cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(tInt,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
    modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

    # creates expansion and capacity variables
    createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

    # connect capacity and expansion variables
    if part.type != :stock createCapaCns!(part,prepTech_dic,cns_dic) end

    # create and control operated capacity variables
    if anyM.options.decomm != :none createOprVarCns!(part,cns_dic,anyM) end
    produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

    # create dispatch variables
    createDispVar!(part,modeDep_dic,ts_dic,r_dic,anyM)
    produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")

    # create conversion balance for conversion technologies
    if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn))))
        cns_dic[:convBal] = createConvBal(part,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared conversion balance for technology $(tech_str)")
    end

    # create storage balance for storage technologies
    if :stLvl in keys(part.var)
        cns_dic[:stBal] = createStBal(part,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared storage balance for technology $(tech_str)")
    end

    # create capacity restrictions
    createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM)
    produceMessage(anyM.options,anyM.report, 3," - Prepared capacity restrictions for technology $(tech_str)")

    # create ratio constraints
    if any(map(x -> occursin("ratioEner",string(x)), collectKeys(keys(part.par))))
        createRatioCns!(part,cns_dic,anyM)
        produceMessage(anyM.options,anyM.report, 3," - Prepared constraints controlling energy ratios for technology $(tech_str)")
    end

    # all constraints are scaled and then written into their respective array position
    foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

    produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

    return cns_dic
end

# <editor-fold desc= prepare to create expansion and capacity variables"

# XXX sets dimensions for expansion and capacity variables
function prepareTechs!(techSym_arr::Array{Symbol,1},prepVar_dic::Dict{Symbol,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)
	for tSym in techSym_arr
		prepTech_dic = Dict{Symbol,NamedTuple}()
		part = anyM.parts.tech[tSym]
        tInt = techInt(tSym,anyM.sets[:Te])

	    # dimension of expansion and corresponding capacity variables
	    if part.type != :stock
	        prepareExpansion!(prepTech_dic, tsYear_dic, part, tInt, anyM)

			for expan in collectKeys(keys(prepTech_dic))
				prepareCapacity!(part,prepTech_dic,vcat(map(x -> x[!,removeVal(x)],prepTech_dic[expan])...),Symbol(replace(string(expan),"exp" => "capa")),anyM, tech = tInt)
			end
		end

		# check for capacities variables that have to be created, because of residual capacities provided
		addResidualCapa!(prepTech_dic, part, tInt, anyM)

		# map required capacity constraints
		createCapaRestrMap!(tSym, anyM)

		# if any capacity variables or residuals were prepared, add these to overall dictionary
	    if collect(values(prepTech_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepVar_dic[tSym] = prepTech_dic
			# write reporting, if not all 3 kind of storage capacities will be created
			if map(x -> x in keys(prepTech_dic),(:capaStIn,:capaStOut,:capaStSize)) |> (y -> any(y) && !all(y))
				push!(anyM.report,(3,"technology dimensions","storage","in case of $(string(tSym)) information for one storage capacity is missing (capaStIn, capaStOut or capaStSize)"))
			end
		end
	end
end

# XXX dimensions for expansion variables
function prepareExpansion!(prepTech_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},part::AbstractModelPart,tInt::Int,anyM::anyModel)

	# extract tech info
	carGrp_ntup = part.carrier
	balLvl_ntup = part.balLvl
	defPar_tup = tuple(keys(part.par)...)

	tsExp_arr, rExp_arr   = [getfield.(getNodesLvl(anyM.sets[x[2]], balLvl_ntup.exp[x[1]]),:idx) for x in enumerate([:Ts,:R])]
	tsExpSup_arr = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), tsExp_arr)
	if anyM.options.interCapa != :linear tsExp_arr = map(x -> [minimum(x)],tsExp_arr) end

	expDim_arr = vcat(collect(Iterators.product(Iterators.zip(tsExp_arr,tsExpSup_arr),rExp_arr))...)
	allMap_df =  getindex.(expDim_arr,1) |> (x -> DataFrame(Ts_exp = getindex.(x,1), Ts_expSup = getindex.(x,2), R_exp = getindex.(expDim_arr,2), Te = fill(tInt,length(expDim_arr))))

	# prepares expansion dimensions for conversion capacity
	if !isempty(intersect((:gen,:use),keys(carGrp_ntup)))
		# filters cases where expansion is fixed to zero
		convMap_df = removeEntries([filterZero(allMap_df,getLimPar(anyM.parts.lim,:expConvFix, anyM.sets[:Te], tech = tInt),anyM)],allMap_df)

		if !isempty(convMap_df) prepTech_dic[:expConv] =  (var = addSupTsToExp(convMap_df,part.par,:Conv,tsYear_dic,anyM), ratio = DataFrame(), resi = DataFrame()) end
	end

	stCar_arr::Array{Int,1} = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))

	# prepares expansion dimensions for storage capacity
	if !isempty(stCar_arr)
		stMap_df = combine(groupby(allMap_df,namesSym(allMap_df)), :Te => (x -> stCar_arr) => :C)

		for st in (:StIn, :StOut, :StSize)
			remove_arr = Array{DataFrame,1}()

			# filters cases where expansion is fixed to zero
			if Symbol(:exp,st,:Fix) in defPar_tup
				push!(remove_arr,filterZero(stMap_df,getLimPar(anyM.parts.lim,Symbol(:exp,st,:Fix), anyM.sets[:Te], tech = tInt),anyM))
			end

			remove_arr, ratioTab_df = findStorageRatio(tInt,stMap_df,st, remove_arr, part, :exp, anyM)
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
			lim_obj = getLimPar(anyM.parts.lim,limPar,anyM.sets[:Te], tech = t_int)
			if !(isdefined(lim_obj,:name)) continue end
			both_df =  innerjoin(ratioTab_df,lim_obj.data, on = intersect(intCol(ratioTab_df),intCol(lim_obj.data)))
			if !isempty(both_df)
				push!(anyM.report,(1,:variable,:expansion,"for $(join(part.name, " < ")) $(strLim_dic[Symbol(split(string(limPar),string(st_sym))[end])]) for $(strRatio_dic[st_sym]) were ignored since an conversion/storage input ratio was provided"))
			end
		end
	end

	return remove_arr, ratioTab_df
end

# </editor-fold>

# <editor-fold desc= create technology related variables"

# XXX create expansion and capacity variables
function createExpCap!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},anyM::anyModel,ratioVar_dic::Dict{Symbol,Pair{String,String}} = Dict{Symbol,Pair{String,String}}())
	for expVar in sort(collectKeys(keys(prep_dic)))

		varMap_tup = prep_dic[expVar]
		# create dataframe of capacity or expansion variables by creating the required capacity variables and join them with pure residual values
		var_df = createVar(varMap_tup.var,string(expVar),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.capa)
		if !isempty(varMap_tup.resi)
			if expVar == :capaExc # flips and repeats entries for directed exchange variabes before moving on
				var_df = filter(r -> r.dir,var_df) |> (x -> vcat(filter(r -> !r.dir,var_df),vcat(x,rename(x,replace(namesSym(x),:R_to => :R_from, :R_from => :R_to)))))
			end
			join_arr = intCol(var_df,:dir)
			var_df = combine(x -> (var = x.var + x.var_1,), groupby(joinMissing(var_df,varMap_tup.resi[!,vcat(:var,join_arr...)], join_arr, :outer, Dict(:var => AffExpr(),:var_1 => AffExpr()),true),intCol(var_df,:dir)))
		end

		# expands table of expansion variables to superordinate timesteps and modifies expansion variable accordingly
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
			else # ratios controlling stock capacities
				preRatio_df = varMap_tup.ratio
			end

			join_arr = intCol(part.var[ratioVar_sym])
			# join ratios and corresponding
			ratio_df = select(innerjoin(preRatio_df,part.var[ratioVar_sym]; on = join_arr),unique(vcat(join_arr,[:var,:ratio,:Ts_disSup])))
			ratio_df[!,:var] = ratio_df[!,:var] .* ratio_df[!,:ratio]
	
			var_df = ratio_df[!,Not(:ratio)] |> (x -> isempty(var_df) ? x : vcat(x,antijoin(var_df,x, on = join_arr)))
		end

		if !isempty(var_df)	part.var[expVar] = orderDf(var_df) end
	end
end

# XXX create all dispatch variables
function createDispVar!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)
	# assign relevant availability parameters to each type of variable
	relAva_dic = Dict(:gen => (:avaConv,), :use => (:avaConv,), :stIntIn => (:avaConv, :avaStIn), :stIntOut => (:avaConv, :avaStOut), :stExtIn => (:avaStIn,), :stExtOut => (:avaStOut,), :stLvl => (:avaStSize,))

	for va in collectKeys(keys(part.carrier)) |> (x -> :capaStIn in keys(part.var) ? [:stLvl,x...]  : x) # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use)
		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(part.var[:capaConv])[!,Not(:var)]
			basis_df[!,:C] .= [collect(getfield(part.carrier,va))]
			basis_df = orderDf(flatten(basis_df,:C))
		else
            lock(anyM.lock)
			basis_df = orderDf(copy(part.var[:capaStIn])[!,Not(:var)])
			unlock(anyM.lock)
			# filter carriers that are can be actively stored, although they got descendants
			intC_arr = union(collect(part.actSt),map(y -> part.carrier[y],filter(x -> x in keys(part.carrier),[:stIntIn,:stIntOut])) |> (y -> isempty(y) ? Int[] : union(y...)))
			basis_df = replCarLeafs(basis_df,anyM.sets[:C],noLeaf = intC_arr)
			# filter entries that are already descendants of carrier being actively stored
			unique(vcat(map(x -> filter(y -> x != y,getDescendants(x,anyM.sets[:C],true)),unique(basis_df[!,:C]))...)) |> (z -> filter!(x -> !(x.C in z) || x.C in intC_arr,basis_df))
		end

		# adds temporal and spatial level to dataframe
		cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[x].rDis) for x in unique(basis_df[!,:C]))
		basis_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],basis_df[!,:C])
		basis_df[!,:lvlR] = map(x -> cToLvl_dic[x][2],basis_df[!,:C])
		allVar_df = orderDf(expandExpToDisp(basis_df,ts_dic,r_dic,true))

		# add mode dependencies
		modeDep_df = copy(modeDep_dic[va])
		modeDep_df[!,:M] .= isempty(modeDep_df) ? [0] : [collect(part.modes)]
		modeDep_df = flatten(modeDep_df,:M)

		allVar_df = joinMissing(allVar_df,modeDep_df,namesSym(modeDep_dic[va]),:left,Dict(:M => 0))

		# filter entries where availability is zero
		for avaPar in relAva_dic[va]
			if !isempty(part.par[avaPar].data) && 0.0 in part.par[avaPar].data[!,:val]
				allVar_df = filter(x -> x.val != 0.0,  matchSetParameter(allVar_df,part.par[avaPar],anyM.sets))[!,Not(:val)]
			end
		end

		# computes value to scale up the global limit on dispatch variable that is provied per hour and create variable
        if conv_boo
			scaFac_fl = anyM.options.scaFac.dispConv
		else
			scaFac_fl = anyM.options.scaFac.dispSt
		end
		part.var[va] = orderDf(createVar(allVar_df,string(va), getUpBound(allVar_df,anyM.options.bound.disp / scaFac_fl,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = scaFac_fl))
	end
end

# XXX create variables and constraints regarding operated variables
function createOprVarCns!(part::AbstractModelPart,cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)
	for capaVar in filter(x -> occursin("capa",string(x)),keys(part.var))
		oprVar_sym = string(capaVar) |> (x -> Symbol(:opr,uppercase(x[1]),x[2:end]))
		# XXX create operated variable
		var_df = copy(part.var[capaVar])[!,Not(:var)]
		var_df = createVar(var_df,string(oprVar_sym),NaN,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.oprCapa)

		part.var[oprVar_sym] = orderDf(var_df)

		# XXX create constraint to connect operated and installed capacity
		var_df[!,:cnsExpr] = map(x -> x[1] - x[2],zip(var_df[!,:var],part.var[capaVar][!,:var]))
		cns_dic[oprVar_sym] = cnsCont(select(var_df,Not(:var)),:smaller)

		# XXX create constraint to prevent re-commissioning of capacity once decommissioned
		if anyM.options.decomm == :decomm
			# add previous period and its capacity variable to table
			prevTs_dic = Dict(x => anyM.supTs.step[findall(x .== anyM.supTs.step)[1]]-1 for x in anyM.supTs.step[2:end])
			select!(var_df, Not(:cnsExpr))
			cns_df = rename(filter(r -> r.Ts_disSup != anyM.supTs.step[1],var_df),:var => :oprNow)
			cns_df[!,:Ts_disSupPrev] = map(x -> prevTs_dic[x] ,cns_df[!,:Ts_disSup])
			cns_df = rename(innerjoin(cns_df,var_df; on = intCol(var_df,:dir) |> (x -> Pair.(replace(x,:Ts_disSup => :Ts_disSupPrev),x))),:var => :oprPrev)

			# add expansion variable to dataframe
			if (!(:type in fieldnames(typeof(part))) || part.type != :stock) && Symbol(replace(string(capaVar),"capa" => "exp")) in keys(part.var)
				exp_df = part.var[Symbol(replace(string(capaVar),"capa" => "exp"))][!,Not(:Ts_disSup)]
				join_arr = filter(x -> x != :Ts_expSup,intCol(var_df))

				cns_df = joinMissing(cns_df,exp_df, Pair.(join_arr,replace(join_arr,:Ts_disSup => :Ts_expSup)),:left,Dict(:var => AffExpr(),:Ts_exp => 0))
				cns_df = rename(cns_df[!,Not(:Ts_exp)],:var => :expNow)
			else
				cns_df[!,:expNow] .= AffExpr()
			end

			# add residual capacities of current and previous period
			joinResi_arr = filter(x -> x != :Ts_disSupPrev, intCol(cns_df,:dir))
			cns_df = rename(innerjoin(cns_df,part.var[capaVar],on = joinResi_arr),:var => :resiNow)
			cns_df[!,:resiNow] = getfield.(cns_df[!,:resiNow],:constant)
			cns_df = rename(joinMissing(cns_df, part.var[capaVar], Pair.(replace(joinResi_arr,:Ts_disSup => :Ts_disSupPrev),joinResi_arr),:left, Dict(:resiNow => AffExpr(),:var => AffExpr())),:var => :resiPrev)
			cns_df[!,:resiPrev] = getfield.(cns_df[!,:resiPrev],:constant)

			# create actual constraint information
			cns_df[!,:cnsExpr]  = map(x -> - x.oprNow + x.oprPrev + x.expNow + (x.resiNow - x.resiPrev |> (l -> l > 0.0 ? l : 0.0)),eachrow(cns_df))
			select!(cns_df,Not([:Ts_disSupPrev,:oprNow,:oprPrev,:expNow,:resiNow,:resiPrev]))
			cns_dic[string(oprVar_sym) |> (x -> Symbol(:re,uppercase(x[1]),x[2:end]))] = cnsCont(orderDf(cns_df),:greater)
		end
	end
end

# XXX capacity values for stock technologies
function addResidualCapa!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,tInt::Int,anyM::anyModel)

	carGrp_ntup = part.carrier
	stCar_arr = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))
	defPar_tup = tuple(keys(part.par)...)

	# checks conversion capacities for stock data
	if !isempty(intersect((:gen,:use),keys(carGrp_ntup)))
		permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step))...),i) for i in (1,2)]
		potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(tInt,length(permutDim_arr[1])))
		# filters stock technologies with residual values provided and saves these in dictionary
		capaResi_df = checkResiCapa(:capaConv,potCapa_df, part, anyM)

		if !isempty(capaResi_df)
			mergePrepDic!(:capaConv,prepTech_dic,capaResi_df)
		end
	end

	# checks storage capacities for stock data
	if !isempty(stCar_arr)
		permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step,stCar_arr))...),i) for i in (1,2,3)]
		potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], C = permutDim_arr[3], Te = fill(tInt,length(permutDim_arr[1])))

		# loops over different type of storage
		for st in (:StIn, :StOut, :StSize)
			capaResi_df = checkResiCapa(Symbol(:capa,st),potCapa_df, part, anyM)
			capa_sym = Symbol(:capa,st)
			remove_arr, ratioTab_df = findStorageRatio(tInt,potCapa_df,st, Array{DataFrame,1}(), part, :capa, anyM)
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
function createConvBal(part::TechPart,anyM::anyModel)

	cns_df = rename(part.par[:effConv].data,:val => :eff)
	sort!(cns_df,sort(intCol(cns_df)))
	agg_arr = filter(x -> !(x in (:M, :Te)) && (part.type == :emerging || x != :Ts_expSup), intCol(cns_df))

	# defines tuple specificing dimension of aggregation later
	if part.type == :emerging
		srcRes_ntup = part.balLvl |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x.ref[1], R_dis = x.ref[2]))
	else
		srcRes_ntup = part.balLvl |> (x -> (Ts_dis = x.ref[1], R_dis = x.ref[2]))
	end

	# if modes are specified, gets rows of conversion dataframe where they are relevant and creates different tuples to define grouping dimensions
	if :M in namesSym(cns_df)
		srcResM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(1,values(srcRes_ntup)...))...)
		srcResNoM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(0,values(srcRes_ntup)...))...)
		m_arr = findall(0 .!= cns_df[!,:M])
		noM_arr = setdiff(1:size(cns_df,1),m_arr)
	end

	# add variables via aggregation
	in_arr = intersect(keys(part.carrier),(:use,:stIntOut))
	out_arr = intersect(keys(part.carrier),(:gen,:stIntIn))

	for va in union(in_arr,out_arr)
		if :M in namesSym(cns_df) # aggregated dispatch variables, if a mode is specified somewhere, mode dependant and non-mode dependant balances have to be aggregated separately
			cns_df[!,va] .= AffExpr()
			cns_df[m_arr,va] = aggUniVar(part.var[va], select(cns_df[m_arr,:],intCol(cns_df)), [:M,agg_arr...], srcResM_ntup, anyM.sets)
			cns_df[noM_arr,va] = aggUniVar(part.var[va], select(cns_df[noM_arr,:],intCol(cns_df)), [:M,agg_arr...], srcResNoM_ntup, anyM.sets)
		else
			cns_df[!,va] = aggUniVar(part.var[va], select(cns_df,intCol(cns_df)), agg_arr, srcRes_ntup, anyM.sets)
		end
	end

	# aggregate in and out variables respectively
	cns_df[!,:in] = map(x -> sum(x),eachrow(cns_df[!,in_arr]))
	cns_df[!,:out] = map(x -> sum(x),eachrow(cns_df[!,out_arr]))
	select(cns_df,Not(vcat(in_arr,out_arr)))

	# create actual constraint
	cns_df[!,:cnsExpr] = map(x -> x.in*x.eff - x.out,eachrow(cns_df))
	return cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]),:equal)
end

# XXX create storage balance
function createStBal(part::TechPart,anyM::anyModel)

	# XXX get variables for storage level
	# get variables for current storage level
	cns_df = rename(part.var[:stLvl],:var => :stLvl)
	cnsDim_arr = filter(x -> x != :Ts_disSup, intCol(cns_df))

	# join variables for previous storage level
	tsChildren_dic = Dict((x,y) => getDescendants(x,anyM.sets[:Ts],false,y) for x in anyM.supTs.step, y in unique(map(x -> getfield(anyM.sets[:Ts].nodes[x],:lvl), cns_df[!,:Ts_dis])))
	firstLastTs_dic = Dict(minimum(tsChildren_dic[z]) => maximum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
	firstTs_arr = collect(keys(firstLastTs_dic))

	cns_df[!,:Ts_disPrev] = map(x -> x in firstTs_arr ? firstLastTs_dic[x] : x - 1, cns_df[!,:Ts_dis])
	cns_df = rename(joinMissing(cns_df,part.var[:stLvl], intCol(part.var[:stLvl]) |> (x -> Pair.(replace(x,:Ts_dis => :Ts_disPrev),x)), :left, Dict(:var => AffExpr())),:var => :stLvlPrev)

	# determines dimensions for aggregating dispatch variables
	agg_arr = filter(x -> !(x in (:M, :Te)) && (part.type == :emerging || x != :Ts_expSup), cnsDim_arr)

	# obtain all different carriers of level variable and create array to store the respective level constraint data
	uniC_arr = unique(cns_df[!,:C])
	cCns_arr = Array{DataFrame}(undef,length(uniC_arr))

	for (idx,c) in enumerate(uniC_arr)

		# get constraints relevant for carrier and find rows where mode is specified
		cnsC_df = filter(x -> x.C == c,cns_df)
		sort!(cnsC_df,sort(intCol(cnsC_df)))

		m_arr = findall(0 .!= cnsC_df[!,:M])
		noM_arr = setdiff(1:size(cnsC_df,1),m_arr)

		if part.type == :emerging
			srcRes_ntup = anyM.cInfo[c] |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl, M = 1))
		else
			srcRes_ntup = anyM.cInfo[c] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl, M = 1))
		end

		# XXX join in and out dispatch variables and adds efficiency to them (hence efficiency can be specific for different carriers that are stored in and out)
		for typ in (:in,:out)
			typVar_df = copy(cns_df[!,cnsDim_arr])
			# create array of all dispatch variables
			allType_arr = intersect(keys(part.carrier),typ == :in ? (:stExtIn,:stIntIn) : (:stExtOut,:stIntOut))
			effPar_sym = typ == :in ? :effStIn : :effStOut
			# adds dispatch variables
			typExpr_arr = map(allType_arr) do va
				typVar_df = filter(x -> x.C == c,part.par[effPar_sym].data) |> (x -> innerjoin(part.var[va],x; on = intCol(x)))
				if typ == :in
					typVar_df[!,:var] = typVar_df[!,:var] .* typVar_df[!,:val]
				else
					typVar_df[!,:var] = typVar_df[!,:var] ./ typVar_df[!,:val]
				end
				return typVar_df[!,Not(:val)]
			end

			# adds dispatch variable to constraint dataframe, mode dependant and non-mode dependant balances have to be aggregated separately
			dispVar_df = vcat(typExpr_arr...)
			cnsC_df[!,typ] .= AffExpr()
			if isempty(dispVar_df) continue end
			cnsC_df[m_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[m_arr,:],intCol(cnsC_df)), [:M,agg_arr...], (M = 1,), anyM.sets)
			cnsC_df[noM_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[noM_arr,:],intCol(cnsC_df)), [:M,agg_arr...], (M = 0,), anyM.sets)
		end

		# XXX adds further parameters that depend on the carrier specified in storage level (superordinate or the same as dispatch carriers)
		sca_arr = getResize(cnsC_df,anyM.sets[:Ts],anyM.supTs)

		# add discharge parameter, if defined
		if :stDis in keys(part.par)
			part.par[:stDis].defVal = 0.0
			cnsC_df = matchSetParameter(cnsC_df,part.par[:stDis],anyM.sets)
			cnsC_df[!,:stDis] =   (1 .- cnsC_df[!,:val]) .^ sca_arr
			select!(cnsC_df,Not(:val))
		else
			cnsC_df[!,:stDis] .= 1.0
		end

		# add inflow parameter, if defined
		if :stInflow in keys(part.par)
			part.par[:stInflow].defVal = 0.0
			cnsC_df = matchSetParameter(cnsC_df,part.par[:stInflow],anyM.sets, newCol = :stInflow)
			if !isempty(part.modes)
            	cnsC_df[!,:stInflow] = cnsC_df[!,:stInflow] ./ length(part.modes) .* sca_arr
			end
		else
			cnsC_df[!,:stInflow] .= 0.0
		end

		# XXX create final equation
		cnsC_df[!,:cnsExpr] = map(x -> x.stLvlPrev * x.stDis + x.stInflow + x.in - x.out - x.stLvl,eachrow(cnsC_df))
		cCns_arr[idx] = cnsC_df
	end

	cns_df =  vcat(cCns_arr...)
	return cnsCont(orderDf(cns_df[!,[cnsDim_arr...,:cnsExpr]]),:equal)
end

# XXX create all capacity restrictions for technology
function createCapaRestr!(part::TechPart,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	cnstrType_dic = Dict(:out => (dis = (:gen, :stIntIn), capa = :Conv), :in => (dis = (:use,:stIntOut), capa = :Conv),
							:stIn => (dis = (:stExtIn, :stIntIn), capa = :StIn), :stOut => (dis = (:stExtOut, :stIntOut), capa = :StOut), :stSize => (dis = (:stLvl,), capa = :StSize))

	capa_sym = anyM.options.decomm != :none ? :oprCapa : :capa
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

		# add all constraints to part
		allCns_df[!,:cnsExpr] = map(x -> x.disp - x.capa,eachrow(allCns_df))
		cns_dic[Symbol(type_sym,:Restr)] = cnsCont(orderDf(allCns_df[!,[intCol(allCns_df)...,:cnsExpr]]),:smaller)
	end
end

# XXX sub-function to create restriction
function createRestr(part::TechPart, capaVar_df::DataFrame, restr::DataFrameRow, type_sym::Symbol, info_ntup::NamedTuple,
															ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, sets_dic::Dict{Symbol,Tree}, supTs_ntup::NamedTuple)

	conv_boo = type_sym in (:out,:in)
	dim_arr = conv_boo ? [:Ts_expSup,:Ts_dis,:R_dis,:Te] : [:Ts_expSup,:Ts_dis,:R_dis,:C,:Te]
	agg_arr = [:Ts_expSup,:Ts_dis,:R_dis] |> (x -> filter(x -> part.type == :emerging || x != :Ts_expSup,x))

	# get relevant carriers for conversion and storage variables
	relConv_arr = restr.car
	intC_arr = union(collect(part.actSt),map(y -> part.carrier[y],filter(x -> x in keys(part.carrier),[:stIntIn,:stIntOut])) |> (y -> isempty(y) ? Int[] : union(y...)))
	relSt_arr = filter(y -> isempty(sets_dic[:C].nodes[y].down) || y in intC_arr, [restr.car[1],getDescendants(restr.car[1],sets_dic[:C],true)...])

	# determines dimensions for aggregating dispatch variables
	capaVar_df[!,:lvlTs] .= restr.lvlTs
	capaVar_df[!,:lvlR] .= restr.lvlR

	# resize capacity variables (expect for stSize since these are already provided in energy units)
	if type_sym != :stSize
		capaVar_df[!,:var]  = capaVar_df[!,:var] .* map(x -> supTs_ntup.sca[(x.Ts_disSup,x.lvlTs)],	eachrow(capaVar_df[!,[:Ts_disSup,:lvlTs]]))
	end

	# replaces expansion with dispatch regions and aggregates capacity variables accordingy if required
	grpCapaVar_df = copy(select(capaVar_df,Not(:var))) |> (y -> unique(combine(x -> (R_dis = r_dic[(x.R_exp[1],x.lvlR[1])],),groupby(y,namesSym(y)))[!,Not([:R_exp,:lvlR])]))
	resExp_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = part.balLvl.exp[1], Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1) : (Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1)
	sort!(grpCapaVar_df,sort(intCol(grpCapaVar_df)))
	grpCapaVar_df[!,:var] = aggUniVar(rename(capaVar_df,:R_exp => :R_dis),grpCapaVar_df,replace(agg_arr,:Ts_dis => :Ts_disSup),resExp_ntup,sets_dic)

	# expand capacity to dimension of dispatch
	capaDim_df = combine(x -> (Ts_dis = ts_dic[(x.Ts_disSup[1],x.lvlTs[1])],), groupby(grpCapaVar_df[!,Not(:var)],namesSym(grpCapaVar_df[!,Not(:var)])))[!,Not(:lvlTs)]
	sort!(capaDim_df,sort(intCol(capaDim_df)))
	select!(grpCapaVar_df,Not(:lvlTs))

	# obtain all relevant dispatch variables
	dispVar_arr = type_sym != :stSize ? intersect(keys(part.carrier),info_ntup.dis) : collect(info_ntup.dis)
	resDis_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = part.balLvl.exp[1], Ts_dis = restr.lvlTs, R_dis = restr.lvlR) : (Ts_dis = restr.lvlTs, R_dis = restr.lvlR)
	for va in dispVar_arr
		# filter dispatch variables not belonging to relevant carrier
		if va in (:gen,:use)
			relC_arr = relConv_arr
		else
			relC_arr = relSt_arr
		end
		allVar_df = filter(r -> r.C in relC_arr, part.var[va])[!,Not(:Ts_disSup)]

		# get availablity (and in case of paramter of type out also efficiency since capacities refer to input capacity) parameter and add to dispatch variable
		ava_arr = matchSetParameter(allVar_df,part.par[Symbol(:ava,info_ntup.capa)],sets_dic, newCol = :ava)[!,:ava]
		if type_sym in (:out,:stOut)
			ava_arr = matchSetParameter(allVar_df,part.par[type_sym == :out ? :effConv : :effStOut],sets_dic,newCol = :eff)[!,:eff] .* ava_arr
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
	grpCapaVar_df = combine(groupby(grpCapaVar_df,replace(dim_arr,:Ts_dis => :Ts_disSup)), :var => (x -> sum(x)) => :capa)
	cns_df = innerjoin(capaDim_df,grpCapaVar_df,on = intCol(grpCapaVar_df))
	return cns_df
end

# XXX create ratio constraints (Fix, Low and Up for use and gen)
function createRatioCns!(part::TechPart,cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	# collects all tables for equations
	for type in intersect(keys(part.carrier),(:use, :gen)), limit in (:Up, :Low, :Fix)

		typeUp_sym = string(type) |> (x -> Symbol(uppercase(x[1]),x[2:end]))
		ratioName_sym = Symbol(:ratioEner,typeUp_sym,limit)
		if !(ratioName_sym in keys(part.par)) continue end

		# obtain variable name and parameter data
		cns_df = rename(copy(part.par[ratioName_sym].data),:val => :ratio)
		sort!(cns_df,sort(intCol(cns_df)))

		# joins parameter data with ratio controlled variable and all variables
		agg_arr = filter(r -> r != :Te && (part.type == :emerging || r != :Ts_expSup), intCol(cns_df))

		if part.type == :emerging
			srcRes_ntup = (anyM.sets[:Ts].nodes[cns_df[1,:Ts_dis]].lvl, anyM.sets[:R].nodes[cns_df[1,:R_dis]].lvl) |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x[1], R_dis = x[2]))
		else
			srcRes_ntup = (anyM.sets[:Ts].nodes[cns_df[1,:Ts_dis]].lvl, anyM.sets[:R].nodes[cns_df[1,:R_dis]].lvl) |> (x -> (Ts_dis = x[1], R_dis = x[2]))
		end

		if :M in namesSym(cns_df) # aggregates dispatch variables, if a mode is specified somewhere, mode dependant and non-mode dependant balances have to be aggregated separately
			# find cases where ratio constraint is mode dependant
			srcResM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(1,values(srcRes_ntup)...))...)
			srcResNoM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(0,values(srcRes_ntup)...))...)
			m_arr = findall(0 .!= cns_df[!,:M])
			noM_arr = setdiff(1:size(cns_df,1),m_arr)
			# aggregate variables with defined ratio
			cns_df[!,:ratioVar] .= AffExpr()
			cns_df[m_arr,:ratioVar] = aggUniVar(part.var[type], select(cns_df[m_arr,:],intCol(cns_df)), agg_arr, srcResM_ntup, anyM.sets)
			cns_df[noM_arr,:ratioVar] = aggUniVar(part.var[type], select(cns_df[noM_arr,:],intCol(cns_df)), agg_arr, srcResNoM_ntup, anyM.sets)
			# aggregate all variables
			cns_df[!,:allVar] .= AffExpr()
			cns_df[m_arr,:allVar] =	aggUniVar(part.var[type], select(cns_df[m_arr,:],intCol(cns_df)), filter(x -> x != :C,agg_arr), srcResM_ntup, anyM.sets)
			cns_df[noM_arr,:allVar] =	aggUniVar(part.var[type], select(cns_df[noM_arr,:],intCol(cns_df)), filter(x -> x != :C,agg_arr), srcResNoM_ntup, anyM.sets)
		else
			cns_df[!,:ratioVar] = aggUniVar(part.var[type], select(cns_df,intCol(cns_df)), agg_arr, srcRes_ntup, anyM.sets)
			cns_df[!,:allVar] =	aggUniVar(part.var[type], select(cns_df,intCol(cns_df)), filter(x -> x != :C,agg_arr), srcRes_ntup, anyM.sets)
		end

		# create corresponding constraint
		if occursin("Fix",string(limit))
			sign_sym = :equal
		elseif occursin("Low",string(limit))
			sign_sym = :greater
		else
			sign_sym = :smaller
		end

		cns_df[!,:cnsExpr] = map(x -> x.ratioVar - x.allVar * x.ratio, eachrow(cns_df))


		cns_dic[ratioName_sym] = cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]),sign_sym)
	end
end

# </editor-fold>

