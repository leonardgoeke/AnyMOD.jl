
# ! iteration over all technologies to create variables and constraints
function createTech!(tInt::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(tInt,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
    modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

    # creates expansion and capacity variables
    createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

    # create expansion constraints
	if isempty(anyM.subPro) || anyM.subPro == (0,0)
		# connect capacity and expansion variables
		if part.type != :stock
			createCapaCns!(part,prepTech_dic,cns_dic)
		end
		# control operated capacity variables
		if part.decomm != :none
			createOprVarCns!(part,cns_dic,anyM)
		end
	end

    produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

    # create dispatch variables
	if isempty(anyM.subPro) || anyM.subPro != (0,0)
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

		produceMessage(anyM.options,anyM.report, 3," - Prepared constraints controlling energy ratios for technology $(tech_str)")
	end

	# create ratio constraints
	createRatioCns!(part,cns_dic,r_dic,anyM)

    # all constraints are scaled and then written into their respective array position
    foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

    produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

    return cns_dic
end

#region # * prepare to create expansion and capacity variables

# ! prepare dictionaries that specifies dimensions for expansion and capacity variables
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

		# ensure consistency among different storage capacities (to every storage in- or output capacity a corresponding storage size has to exist)
		stKey_arr = collectKeys(keys(prepTech_dic))

		if !isempty(intersect([:capaStIn,:capaStOut],stKey_arr))
			# determines all defined storage in- and output capacities
			allSt_arr = filter(z -> !isempty(z), vcat(map(y -> collect(map(x -> getfield(prepTech_dic[y],x),(:var,:resi))),intersect([:capaStIn,:capaStOut],stKey_arr))...))
			relSt_df = unique(vcat(map(w -> select(w,intCol(w)), allSt_arr)...))

			if isempty(relSt_df) continue end

			# finds cases where no storage size capacity can be matched to in- or output and adds corresponding entries
			if :capaStSize in stKey_arr
				newSize_df = (part.type == :stock ? :resi : :var) |> (z -> vcat(prepTech_dic[:capaStSize].var,antijoin(relSt_df,getfield(prepTech_dic[:capaStSize],z), on = names(relSt_df))))
				prepTech_dic[:capaStSize] = (var = newSize_df, resi = prepTech_dic[:capaStSize].resi)
			else
				prepTech_dic[:capaStSize]= (var = relSt_df, resi = DataFrame)
			end
		end

		# map required capacity constraints
		createCapaRestrMap!(tSym, anyM)

		# add entry for operated capacity
		if part.decomm != :none
			for capTy in intersect(keys(prepTech_dic),(:capaConv,:capaStIn,:capaStOut,:capaStSize,:capaExc))
				if part.type != :stock
					prepTech_dic[Symbol(:ins,makeUp(capTy))] =  (var = prepTech_dic[capTy].var, resi = DataFrame())
				else
					prepTech_dic[Symbol(:ins,makeUp(capTy))] =  (var = select(prepTech_dic[capTy].resi,Not([:var])), resi = DataFrame())
				end
			end
		end

		# if any capacity variables or residuals were prepared, add these to overall dictionary
		if collect(values(prepTech_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepVar_dic[tSym] = prepTech_dic
		end
	end
end

# ! dimensions for expansion variables
function prepareExpansion!(prepTech_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},part::AbstractModelPart,tInt::Int,anyM::anyModel)

	# extract tech info
	carGrp_ntup = part.carrier
	balLvl_ntup = part.balLvl

	tsExp_arr, rExp_arr   = [getfield.(getNodesLvl(anyM.sets[x[2]], balLvl_ntup.exp[x[1]]),:idx) for x in enumerate([:Ts,:R])]
	tsExpSup_arr = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), tsExp_arr)
	if anyM.options.interCapa != :linear tsExp_arr = map(x -> [minimum(x)],tsExp_arr) end

	expDim_arr = vcat(collect(Iterators.product(Iterators.zip(tsExp_arr,tsExpSup_arr),rExp_arr))...)
	allMap_df =  getindex.(expDim_arr,1) |> (x -> DataFrame(Ts_exp = getindex.(x,1), Ts_expSup = getindex.(x,2), R_exp = getindex.(expDim_arr,2), Te = fill(tInt,length(expDim_arr))))

	stCar_arr::Array{Int,1} = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))
	convCar_arr::Array{Int,1} = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:use,:gen))))...))


	# loops over type of capacities to specify dimensions of capacity variables
	for exp in (:Conv, :StIn, :StOut, :StSize)

		# removes entries where capacities are fixed to zero
		if exp == :Conv && !isempty(convCar_arr)
			exp_df = removeEntries([filterZero(allMap_df,getLimPar(anyM.parts.lim,:expConvFix, anyM.sets[:Te], tech = tInt),anyM)],allMap_df)
		elseif exp != :Conv && !isempty(stCar_arr)
			stMap_df = combine(groupby(allMap_df,namesSym(allMap_df)), :Te => (x -> stCar_arr) => :C)
			exp_df = removeEntries([filterZero(stMap_df,getLimPar(anyM.parts.lim,Symbol(:exp,exp,:Fix), anyM.sets[:Te], tech = tInt),anyM)],stMap_df)
		else
			continue
		end
		# saves required dimensions to dictionary
		prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(exp_df,part.par,exp,tsYear_dic,anyM), resi = DataFrame())
	end
end

# ! dimensions for capacity variables
function prepareCapacity!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},exp_df::DataFrame,capaVar::Symbol,anyM::anyModel; tech::Int = 0)

	# ! initialize assignments and data
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
	if capaVar == :capaExc
		capaVar_df[!,:dir] .= false; select!(capaVar_df,Not(:Ts_expSup))
	end

	# create entry for capacity
	prep_dic[capaVar] =  (var = orderDf(capaVar_df), resi = DataFrame())
end

# ! capacity values for stock technologies
function addResidualCapa!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,tInt::Int,anyM::anyModel)

	carGrp_ntup = part.carrier
	stCar_arr = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut))))...))

	for resi in (:Conv, :StIn, :StOut, :StSize)
		# cretes dataframe of potential entries for residual capacities
		if resi == :Conv
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step))...),i) for i in (1,2)]
			potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(tInt,length(permutDim_arr[1])))
		elseif !isempty(stCar_arr)
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step,stCar_arr))...),i) for i in (1,2,3)]
			potCapa_df = DataFrame(Ts_expSup = fill(0,length(permutDim_arr[1])), Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], C = permutDim_arr[3], Te = fill(tInt,length(permutDim_arr[1])))
		else
			continue
		end

		# tries to obtain residual capacities and adds them to preparation dictionary
		capaResi_df = checkResiCapa(Symbol(:capa,resi),potCapa_df, part, anyM)

		if !isempty(capaResi_df)
			mergePrepDic!(Symbol(:capa,resi),prepTech_dic,capaResi_df)
		end
	end
end

#endregion

#region # * create technology related variables

# ! create expansion and capacity variables
function createExpCap!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},anyM::anyModel,ratioVar_dic::Dict{Symbol,Pair{String,String}} = Dict{Symbol,Pair{String,String}}())
	for expVar in sort(collectKeys(keys(prep_dic)))
		varMap_tup = prep_dic[expVar]
		# determines scaling factor
		if occursin("exp",string(expVar)) || occursin("insCapa",string(expVar))
			scaFac_fl = anyM.options.scaFac.insCapa
		else
			scaFac_fl = anyM.options.scaFac.capa
		end

		# create dataframe of capacity or expansion variables by creating the required capacity variables and join them with pure residual values
		var_df = createVar(varMap_tup.var,string(expVar),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = scaFac_fl)
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

		if !isempty(var_df)	part.var[expVar] = orderDf(var_df) end
	end
end

# ! create all dispatch variables
function createDispVar!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)
	# assign relevant availability parameters to each type of variable
	relAva_dic = Dict(:gen => (:avaConv,), :use => (:avaConv,), :stIntIn => (:avaConv, :avaStIn), :stIntOut => (:avaConv, :avaStOut), :stExtIn => (:avaStIn,), :stExtOut => (:avaStOut,), :stLvl => (:avaStSize,))

	for va in collectKeys(keys(part.carrier)) |> (x -> :capaStIn in keys(part.var) ? [:stLvl,x...]  : x) # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use)
		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(part.var[:capaConv ])[!,Not(:var)]
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
		allVar_df = orderDf(expandExpToDisp(basis_df,ts_dic,r_dic,anyM.supTs.scr,true))

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

# ! create constraints regarding operated variables
function createOprVarCns!(part::AbstractModelPart,cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	for capaVar in filter(x -> occursin("capa",string(x)),keys(part.var))
		insVar_sym = string(capaVar) |> (x -> Symbol(:ins,uppercase(x[1]),x[2:end]))
		var_df = part.var[insVar_sym]

		# ! create constraint to connect operated and installed capacity
		var_df[!,:cnsExpr] = map(x -> x[2] - x[1],zip(var_df[!,:var],part.var[capaVar][!,:var]))
		cns_dic[insVar_sym] = cnsCont(select(var_df,Not(:var)),:smaller)

		# ! create constraint to prevent re-commissioning of capacity once decommissioned
		if part.decomm == :decomm
			# add previous period and its capacity variable to table
			prevTs_dic = Dict(x => anyM.supTs.step[findall(x .== anyM.supTs.step)[1]]-1 for x in anyM.supTs.step[2:end])
			select!(var_df, Not(:cnsExpr))
			cns_df = rename(filter(r -> r.Ts_disSup != anyM.supTs.step[1],var_df),:var => :oprNow)
			cns_df[!,:Ts_disSupPrev] = map(x -> prevTs_dic[x] ,cns_df[!,:Ts_disSup])
			cns_df = rename(innerjoin(cns_df,var_df; on = intCol(var_df,:dir) |> (x -> Pair.(replace(x,:Ts_disSup => :Ts_disSupPrev),x))),:var => :oprPrev)

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
			cns_df = rename(innerjoin(cns_df,part.var[capaVar],on = joinResi_arr),:var => :resiNow)
			cns_df[!,:resiNow] = getfield.(cns_df[!,:resiNow],:constant)
			cns_df = rename(joinMissing(cns_df, part.var[capaVar], Pair.(replace(joinResi_arr,:Ts_disSup => :Ts_disSupPrev),joinResi_arr),:left, Dict(:resiNow => AffExpr(),:var => AffExpr())),:var => :resiPrev)
			cns_df[!,:resiPrev] = getfield.(cns_df[!,:resiPrev],:constant)

			# create actual constraint information
			cns_df[!,:cnsExpr]  = map(x -> - x.oprNow + x.oprPrev + x.expNow + (x.resiNow - x.resiPrev |> (l -> l > 0.0 ? l : 0.0)),eachrow(cns_df))
			select!(cns_df,Not([:Ts_disSupPrev,:oprNow,:oprPrev,:expNow,:resiNow,:resiPrev]))
			cns_dic[string(insVar_sym) |> (x -> Symbol(:re,uppercase(x[1]),x[2:end]))] = cnsCont(orderDf(cns_df),:greater)
		end
	end
end

#endregion

#region # * create technology related constraints

# ! create conversion balance
function createConvBal(part::TechPart,anyM::anyModel)

	cns_df = rename(part.par[:effConv].data,:val => :eff)
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

# ! create storage balance
function createStBal(part::TechPart,anyM::anyModel)

	# ! get variables for storage level
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

		m_arr = findall(0 .!= cnsC_df[!,:M])
		noM_arr = setdiff(1:size(cnsC_df,1),m_arr)

		if part.type == :emerging
			srcRes_ntup = anyM.cInfo[c] |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl, M = 1))
		else
			srcRes_ntup = anyM.cInfo[c] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c].lvl, M = 1))
		end

		# ! join in and out dispatch variables and adds efficiency to them (hence efficiency can be specific for different carriers that are stored in and out)
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
			cnsC_df[m_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[m_arr,:],intCol(cnsC_df)), [:M,agg_arr...], (M = 1,), anyM.sets)
			cnsC_df[noM_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[noM_arr,:],intCol(cnsC_df)), [:M,agg_arr...], (M = 0,), anyM.sets)
		end

		# ! adds further parameters that depend on the carrier specified in storage level (superordinate or the same as dispatch carriers)
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

		# ! create final equation
		cnsC_df[!,:cnsExpr] = map(x -> x.stLvlPrev * x.stDis + x.stInflow + x.in - x.out - x.stLvl,eachrow(cnsC_df))
		cCns_arr[idx] = cnsC_df
	end

	cns_df =  vcat(cCns_arr...)
	return cnsCont(orderDf(cns_df[!,[cnsDim_arr...,:cnsExpr]]),:equal)
end

# ! create all capacity restrictions for technology
function createCapaRestr!(part::TechPart,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	cnstrType_dic = Dict(:out => (dis = (:gen, :stIntIn), capa = :Conv), :in => (dis = (:use,:stIntOut), capa = :Conv),
							:stIn => (dis = (:stExtIn, :stIntIn), capa = :StIn), :stOut => (dis = (:stExtOut, :stIntOut), capa = :StOut), :stSize => (dis = (:stLvl,), capa = :StSize))

	capaRestr_gdf = groupby(part.capaRestr,:cnstrType)

	# loop over groups of capacity restrictions (like out, stIn, ...)
	for restrGrp in capaRestr_gdf
		# relevant capacity variables
		type_sym = Symbol(restrGrp.cnstrType[1])
		info_ntup = cnstrType_dic[type_sym]

		allCns_arr = Array{DataFrame}(undef,size(restrGrp,1))

		# loop over indiviudal constraints
		for (idx,restr) in enumerate(eachrow(restrGrp))
			allCns_arr[idx] = createRestr(part,copy(part.var[Symbol(:capa,info_ntup.capa)]),restr,type_sym,info_ntup,ts_dic,r_dic,anyM.sets,anyM.supTs)
		end

		allCns_df = vcat(allCns_arr...)

		# add all constraints to part
		allCns_df[!,:cnsExpr] = map(x -> x.disp - x.capa,eachrow(allCns_df))
		cns_dic[Symbol(type_sym,:Restr)] = cnsCont(orderDf(allCns_df[!,[intCol(allCns_df)...,:cnsExpr]]),:smaller)
	end
end

# ! sub-function to create restriction
function createRestr(part::TechPart, capaVar_df::DataFrame, restr::DataFrameRow, type_sym::Symbol, info_ntup::NamedTuple,
															ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, sets_dic::Dict{Symbol,Tree}, supTs_ntup::NamedTuple)

	conv_boo = type_sym in (:out,:in)
	dim_arr = conv_boo ? [:Ts_expSup,:Ts_dis,:R_dis,:Te,:scr] : [:Ts_expSup,:Ts_dis,:R_dis,:C,:Te,:scr]
	agg_arr = [:Ts_expSup,:Ts_dis,:R_dis,:scr] |> (x -> filter(x -> part.type == :emerging || x != :Ts_expSup,x))

	# get relevant carriers for conversion and storage variables
	relConv_arr = restr.car
	intC_arr = union(collect(part.actSt),map(y -> part.carrier[y],filter(x -> x in keys(part.carrier),[:stIntIn,:stIntOut])) |> (y -> isempty(y) ? Int[] : union(y...)))
	relSt_arr = filter(y -> isempty(sets_dic[:C].nodes[y].down) || y in intC_arr, [restr.car[1],getDescendants(restr.car[1],sets_dic[:C],true)...])

	# determines dimensions for aggregating dispatch variables
	capaVar_df[!,:lvlTs] .= restr.lvlTs
	capaVar_df[!,:lvlR] .= restr.lvlR

	# extend dataframe with scenarios
	capaVar_df[!,:scr] = map(x -> supTs_ntup.scr[x], capaVar_df[!,:Ts_disSup])
	capaVar_df = flatten(capaVar_df,:scr)

	# resize capacity variables (expect for stSize since these are already provided in energy units)
	if type_sym != :stSize
		capaVar_df[!,:var]  = capaVar_df[!,:var] .* map(x -> supTs_ntup.sca[(x.Ts_disSup,x.lvlTs)],	eachrow(capaVar_df[!,[:Ts_disSup,:lvlTs]]))
	end

	# replaces expansion with dispatch regions and aggregates capacity variables accordingy if required
	grpCapaVar_df = copy(select(capaVar_df,Not(:var))) |> (y -> unique(combine(x -> (R_dis = r_dic[(x.R_exp[1],x.lvlR[1])],),groupby(y,namesSym(y)))[!,Not([:R_exp,:lvlR])]))
	resExp_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = part.balLvl.exp[1], Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1) : (Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1)
	grpCapaVar_df[!,:var] = aggUniVar(rename(capaVar_df,:R_exp => :R_dis),grpCapaVar_df,replace(agg_arr,:Ts_dis => :Ts_disSup),resExp_ntup,sets_dic)

	# expand capacity to dimension of dispatch
	capaDim_df = combine(x -> (Ts_dis = ts_dic[(x.Ts_disSup[1],x.lvlTs[1])],), groupby(grpCapaVar_df[!,Not(:var)],namesSym(grpCapaVar_df[!,Not(:var)])))[!,Not(:lvlTs)]
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

# ! create ratio constraints (conversion ratios, ratios on storage capacity, and flh etc.)
function createRatioCns!(part::TechPart,cns_dic::Dict{Symbol,cnsCont},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	# creates dictionary assigning first part of parameter name to the corresponding limits enforced
	par_arr = filter(x -> any(map(y -> occursin(y,x),["Up","Low","Fix"])),String.(collectKeys(keys(part.par))))
	ratioLim_arr = map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2, map(y -> y => split(x,y),["Up","Low","Fix"])))[1], par_arr)
	parToLim_dic = Dict(y => getindex.(filter(z -> z[2] == y,ratioLim_arr),1) for y in unique(getindex.(ratioLim_arr,2)))

	ratioVar_dic = Dict(:stInToConv => ((:capaConv, :capaStIn),(:expConv, :expStIn)), :stOutToStIn => ((:capaStIn, :capaStOut),(:expStIn, :expStOut)),
												:sizeToStIn => ((:capaStSize, :capaStIn),(:expStSize, :expStIn)), :flhConv => ((:capaConv,:in),), :flhStIn => ((:capaStIn,:stIn),),
																				:flhStOut => ((:capaStOut,:stOut),), :cycStIn => ((:capaStSize,:stIn),), :cycStOut => ((:capaStSize,:stOut),))

	va_dic = Dict(:stIn => (:stExtIn, :stIntIn), :stOut => (:stExtOut, :stIntOut), :in => (:use,:stIntOut), :out => (:gen,:stIntIn))

	# loop over all variables that are subject to any type of limit (except emissions)
	signLim_dic = Dict(:Up => :greater, :Low => :smaller, :Fix => :equal)

	# loop over parameters for conversion ratios
	for par in filter(x -> occursin("ratioEner",string(x)),collectKeys(keys(parToLim_dic)))

		for lim in parToLim_dic[par]

			ratioType_sym = par == :ratioEnerOut ? :out : :in

			# obtain variable name and parameter data
			cns_df = rename(copy(part.par[Symbol(par,lim)].data),:val => :ratio)

			# joins parameter data with ratio controlled variable and all variables
			agg_arr = filter(r -> r != :Te && (part.type == :emerging || r != :Ts_expSup), intCol(cns_df))

			if part.type == :emerging
				srcRes_ntup = (anyM.sets[:Ts].nodes[cns_df[1,:Ts_dis]].lvl, anyM.sets[:R].nodes[cns_df[1,:R_dis]].lvl) |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x[1], R_dis = x[2]))
			else
				srcRes_ntup = (anyM.sets[:Ts].nodes[cns_df[1,:Ts_dis]].lvl, anyM.sets[:R].nodes[cns_df[1,:R_dis]].lvl) |> (x -> (Ts_dis = x[1], R_dis = x[2]))
			end

			# collect relevant dispatch variables
			relVar_df = vcat(map(x -> part.var[x],intersect(keys(part.carrier),va_dic[ratioType_sym]))...)

			if :M in namesSym(cns_df) # aggregated dispatch variables, if a mode is specified somewhere, mode dependant and non-mode dependant balances have to be aggregated seperately
				# find cases where ratio constraint is mode dependant
				srcResM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(1,values(srcRes_ntup)...))...)
				srcResNoM_ntup = (; zip(tuple(:M,keys(srcRes_ntup)...),tuple(0,values(srcRes_ntup)...))...)
				m_arr = findall(0 .!= cns_df[!,:M])
				noM_arr = setdiff(1:size(cns_df,1),m_arr)
				# aggregate variables with defined ratio
				cns_df[!,:ratioVar] .= AffExpr()
				cns_df[m_arr,:ratioVar] = aggUniVar(relVar_df, select(cns_df[m_arr,:],intCol(cns_df)), agg_arr, srcResM_ntup, anyM.sets)
				cns_df[noM_arr,:ratioVar] = aggUniVar(relVar_df, select(cns_df[noM_arr,:],intCol(cns_df)), agg_arr, srcResNoM_ntup, anyM.sets)
				# aggregate all variables
				cns_df[!,:allVar] .= AffExpr()
				cns_df[m_arr,:allVar] =	aggUniVar(relVar_df, select(cns_df[m_arr,:],intCol(cns_df)), filter(x -> x != :C,agg_arr), srcResM_ntup, anyM.sets)
				cns_df[noM_arr,:allVar] =	aggUniVar(relVar_df, select(cns_df[noM_arr,:],intCol(cns_df)), filter(x -> x != :C,agg_arr), srcResNoM_ntup, anyM.sets)
			else
				cns_df[!,:ratioVar] = aggUniVar(relVar_df, select(cns_df,intCol(cns_df)), agg_arr, srcRes_ntup, anyM.sets)
				cns_df[!,:allVar] =	aggUniVar(relVar_df, select(cns_df,intCol(cns_df)), filter(x -> x != :C,agg_arr), srcRes_ntup, anyM.sets)
			end

			# create corresponding constraint
			cns_df[!,:cnsExpr] = map(x -> x.allVar * x.ratio - x.ratioVar, eachrow(cns_df))
			cns_dic[Symbol(par,lim)] = cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]),signLim_dic[lim])
		end
	end

	# loops over all other parameters (ratios on storage capacity, flh, cycling)
	for par in filter(x -> !occursin("ratioEner",string(x)),collectKeys(keys(parToLim_dic)))

		capaRatio_boo = par in (:stInToConv, :stOutToStIn, :sizeToStIn)

		# controls variables ratio is applied
		if capaRatio_boo && (part.type == :stock || (!isempty(anyM.subPro) && anyM.subPro != (0,0))) # removes expansion for stock technologies or for subproblem
			limVa_arr = (ratioVar_dic[par][1],)
		elseif capaRatio_boo && part.decomm == :none  # removes capacity in case without decomm
			limVa_arr = (ratioVar_dic[par][2],)
		else
			limVa_arr = ratioVar_dic[par]
		end

		# loops over variables limits are enforced on
		for limVa in limVa_arr, lim in parToLim_dic[par]

			# get variables for denominator
			cns_df = copy(part.var[limVa[1]])

			# adjustments for flh and cycling restrictions
			if !capaRatio_boo
				# aggregate to dispatch regions
				cns_df[!,:R_dis] = map(x -> r_dic[x,part.balLvl.ref[2]][1],cns_df[!,:R_exp])
				select!(cns_df,Not([:R_exp]))
				cns_df = combine(groupby(cns_df,intCol(cns_df)), :var => (x -> sum(x)) => :var)
				# extend to scenarios
				cns_df[!,:scr] = map(x -> anyM.supTs.scr[x], cns_df[!,:Ts_disSup])
				cns_df = flatten(cns_df,:scr)
			end

			# matches variables with parameters denominator
			cns_df = rename(matchSetParameter(cns_df,part.par[Symbol(par,lim)],anyM.sets),:var => :denom)

			# get variables for nominator
			rlvTop_arr =  limVa[2] in keys(va_dic) ? intersect(keys(part.carrier),va_dic[limVa[2]]) : (limVa[2],)

			# use out instead of in for flh of conversion technologies, if technology has no conversion input
			if isempty(rlvTop_arr) && rat == :flhConv
				rlvTop_arr = intersect(keys(part.carrier),(:stIntIn))
			end

			top_df = vcat(map(x -> part.var[x],rlvTop_arr)...)

			# rename column for aggregation
			if !capaRatio_boo cns_df = rename(cns_df,:Ts_disSup => :Ts_dis) end

			# connect denominator and nominator
			cns_df[!,:nom] =  aggDivVar(top_df, cns_df, tuple(intCol(cns_df)...), anyM.sets)

			# name column back again
			if !capaRatio_boo cns_df = rename(cns_df,:Ts_dis => :Ts_disSup) end

			# create constraint
			cns_df[!,:cnsExpr] = map(x -> x.val * x.denom - x.nom, eachrow(cns_df))

			va_str = capaRatio_boo ? (string(limVa)[1:4] == "capa" ? "capa" : "exp") : ""
			cns_dic[Symbol(par,lim,makeUp(va_str))] = cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]),signLim_dic[lim])
		end
	end
end

#endregion
