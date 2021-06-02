
# ! iteration over all technologies to create variables and constraints
function createTech!(tInt::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},yTs_dic::Dict{Int64,Int64},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up), :minGen => (:Ts_dis => :up, :R_dis => :up), :minUse => (:Ts_dis => :up, :R_dis => :up))  # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(tInt,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
	modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

	# create investment variables and constraints
	if part.type != :unrestricted
		# creates capacity, expansion, and retrofitting variables
		createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

		# create expansion constraints
		if isempty(anyM.subPro) || anyM.subPro == (0,0)
			# connect capacity and expansion variables
			createCapaCns!(part,anyM.sets,prepTech_dic,cns_dic,anyM.optModel)

			# control operated capacity variables
			if part.decomm != :none
				createOprVarCns!(part,cns_dic,anyM)
			end
		end
	end

	# create dispatch variables and constraints
	if !isempty(part.var) || part.type == :unrestricted 

		# prepare must-run parameters
		if :mustOut in keys(part.par) && (:capaConv in keys(prepTech_dic) || :capaStIn in keys(prepTech_dic))
			if part.type == :unrestricted
				push!(anyM.report,(3,"must output","","must-run parameter for technology '$(tech_str)' ignored, because technology is unrestricted,"))
			else
				computeDesFac!(part,yTs_dic,anyM)
				prepareMustOut!(part,modeDep_dic,prepTech_dic,yTs_dic,r_dic,cns_dic,anyM)
			end
		end

		# map required capacity constraints
		if part.type != :unrestricted 
			rmvOutC_arr = createCapaRestrMap!(part, anyM) 
		end
			
		produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

		# create dispatch variables and constraints
		if isempty(anyM.subPro) || anyM.subPro != (0,0)
			createDispVar!(part,modeDep_dic,ts_dic,r_dic,prepTech_dic,anyM)
			produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")

			# create conversion balance for conversion technologies
			if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn)))) && (:capaConv in keys(part.var) || part.type == :unrestricted)
				cns_dic[:convBal] = createConvBal(part,anyM)
				produceMessage(anyM.options,anyM.report, 3," - Prepared conversion balance for technology $(tech_str)")
			end

			# create storage balance for storage technologies
			if :stLvl in keys(part.var)
				cns_dic[:stBal] = createStBal(part,anyM)
				produceMessage(anyM.options,anyM.report, 3," - Prepared storage balance for technology $(tech_str)")
			end

			# create capacity restrictions
			if part.type != :unrestricted
				createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM,yTs_dic,rmvOutC_arr)
			end
			produceMessage(anyM.options,anyM.report, 3," - Prepared capacity restrictions for technology $(tech_str)")

		end

		# create ratio constraints
		createRatioCns!(part,cns_dic,r_dic,anyM)

		# all constraints are scaled and then written into their respective array position
		foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

		produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

	end

    return cns_dic
end

# ! prepare dictionaries that specifies dimensions for expansion and capacity variables
function prepareTechs!(techSym_arr::Array{Symbol,1},prepAllTech_dic::Dict{Symbol,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	for tSym in techSym_arr

		prepTech_dic = Dict{Symbol,NamedTuple}()
		part = anyM.parts.tech[tSym]
        tInt = sysInt(tSym,anyM.sets[:Te])

	    # ! dimension of expansion and corresponding capacity variables
	    if part.type in (:mature,:emerging)
	        prepareTeExpansion!(prepTech_dic, tsYear_dic, part, tInt, anyM)

			for expan in collectKeys(keys(prepTech_dic))
				prepareCapacity!(part,prepTech_dic,vcat(map(x -> x[!,removeVal(x)],prepTech_dic[expan])...),Symbol(replace(string(expan),"exp" => "capa")),anyM, sys = tInt)
			end
		end

		# ! check for capacities variables that have to be created, because of residual capacities provided
		addResidualCapaTe!(prepTech_dic, part, tInt, anyM)

		# if any capacity variables or residuals were prepared, add these to overall dictionary
		if collect(values(prepTech_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepAllTech_dic[tSym] = prepTech_dic
		end

	end
end

# ! prepare expansion variables for technology
function prepareTeExpansion!(prepTech_dic::Dict{Symbol,NamedTuple},tsYear_dic::Dict{Int,Int},part::AbstractModelPart,tInt::Int,anyM::anyModel)

	# extract tech info
	carGrp_ntup = part.carrier
	balLvl_ntup = part.balLvl

	tsExp_arr, rExp_arr   = [getfield.(getNodesLvl(anyM.sets[x[2]], balLvl_ntup.exp[x[1]]),:idx) for x in enumerate([:Ts,:R])]
	tsExpSup_arr = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), tsExp_arr)
	if anyM.options.interCapa != :linear tsExp_arr = map(x -> [minimum(x)],tsExp_arr) end

	expDim_arr = vcat(collect(Iterators.product(Iterators.zip(tsExp_arr,tsExpSup_arr),rExp_arr))...)
	allMap_df =  getindex.(expDim_arr,1) |> (x -> DataFrame(Ts_exp = getindex.(x,1), Ts_expSup = getindex.(x,2), R_exp = getindex.(expDim_arr,2), Te = fill(tInt,length(expDim_arr))))

	stCar_arr::Array{Int,1} = intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut)) |> (z -> isempty(z) ? Int[] : unique(union(union(map(x -> getproperty(carGrp_ntup,x),z)...)...)))
	convCar_arr::Array{Int,1} = unique(vcat(collect.(map(x -> getproperty(carGrp_ntup,x),intersect(keys(carGrp_ntup),(:use,:gen))))...))


	# loops over type of capacities to specify dimensions of capacity variables
	for exp in (:Conv, :StIn, :StOut, :StSize)
		
		# saves required dimensions to dictionary
		if exp == :Conv && !isempty(convCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(allMap_df,part,exp,tsYear_dic,anyM), resi = DataFrame())
		elseif exp != :Conv && !isempty(stCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(combine(groupby(allMap_df,namesSym(allMap_df)), :Te => (x -> collect(1:countStGrp(carGrp_ntup))) => :id),part,exp,tsYear_dic,anyM), resi = DataFrame())
		else
			continue
		end
		
	end
end

# ! add entries with residual capacities for technologies
function addResidualCapaTe!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,tInt::Int,anyM::anyModel)

	carGrp_ntup = part.carrier
	stCar_arr = intersect(keys(carGrp_ntup),(:stExtIn,:stExtOut,:stIntIn,:stIntOut)) |> (z -> isempty(z) ? Int[] : unique(union(union(map(x -> getproperty(carGrp_ntup,x),z)...)...)))

	for resi in (:Conv, :StIn, :StOut, :StSize)

		# cretes dataframe of potential entries for residual capacities
		if resi == :Conv
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step))...),i) for i in (1,2)]
			potCapa_df = DataFrame(Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(tInt,length(permutDim_arr[1])))
		elseif !isempty(stCar_arr)
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step,collect(1:countStGrp(carGrp_ntup))))...),i) for i in (1,2,3)]
			potCapa_df = DataFrame(Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(tInt,length(permutDim_arr[1])), id = permutDim_arr[3])
		else
			continue
		end

		if isempty(potCapa_df) continue end
		
		potCapa_df[!,:Ts_expSup] = map(x -> part.type != :emerging ? [0] : filter(y -> y <= x,collect(anyM.supTs.step)), potCapa_df[!,:Ts_disSup])
		potCapa_df = flatten(potCapa_df,:Ts_expSup)

		# tries to obtain residual capacities and adds them to preparation dictionary
		capaResi_df = orderDf(checkResiCapa(Symbol(:capa,resi),potCapa_df, part, anyM))

		if !isempty(capaResi_df)
			mergePrepDic!(Symbol(:capa,resi),prepTech_dic,capaResi_df)
		end
	end
end

#region # * create technology related variables and constraints

# ! create all dispatch variables
function createDispVar!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},prepTech_dic::Dict{Symbol,NamedTuple},anyM::anyModel)
	# assign relevant availability parameters to each type of variable
	relAva_dic = Dict(:gen => (:avaConv,), :use => (:avaConv,), :stIntIn => (:avaConv, :avaStIn), :stIntOut => (:avaConv, :avaStOut), :stExtIn => (:avaStIn,), :stExtOut => (:avaStOut,), :stLvl => (:avaStSize,))
	hasSt_boo = :capaStSize in keys(prepTech_dic)

	for va in collectKeys(keys(part.carrier)) |> (x -> hasSt_boo  ? [:stLvl,x...]  : x) # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use) && :capaConv in keys(prepTech_dic)
		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(unique(vcat(map(x -> select(x,intCol(x)),collect(prepTech_dic[:capaConv]))...)))
			basis_df[!,:C] .= [collect(getfield(part.carrier,va))]
			basis_df = orderDf(flatten(basis_df,:C))
		elseif hasSt_boo && !(va in (:gen,:use))
			basis_df = orderDf(copy(unique(vcat(map(x -> select(x,intCol(x)),collect(prepTech_dic[:capaStSize]))...))))
			if isempty(basis_df) continue end
			# gets array of carriers defined for each group of storage
			subField_arr = intersect((:stExtIn,:stExtOut,:stIntIn,:stIntOut),keys(part.carrier))
			idC_dic = Dict(y => union(map(x -> getfield(part.carrier,x)[y],subField_arr)...) for y in 1:length(part.carrier[subField_arr[1]]))
			# expands capacities according to carriers
			basis_df[!,:C] .= map(x -> idC_dic[x],basis_df[!,:id])
			basis_df = orderDf(flatten(basis_df,:C))
		else
			continue
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
	in_arr = intersect(collect(keys(part.var)),(:use,:stIntOut))
	out_arr = intersect(collect(keys(part.var)),(:gen,:stIntIn))

	for va in union(in_arr,out_arr)
		# add energy content to expression if defined
		var_df = addEnergyCont(part.var[va],part,anyM.sets)
		
		# aggregated dispatch variables, if a mode is specified somewhere, mode dependant and non-mode dependant balances have to be aggregated separately
		if :M in namesSym(cns_df) 
			cns_df[!,va] = map(x -> AffExpr(),1:size(cns_df,1))
			cns_df[m_arr,va] = aggUniVar(var_df, select(cns_df[m_arr,:],intCol(cns_df)), [:M,agg_arr...], srcResM_ntup, anyM.sets)
			cns_df[noM_arr,va] = aggUniVar(var_df, select(cns_df[noM_arr,:],intCol(cns_df)), [:M,agg_arr...], srcResNoM_ntup, anyM.sets)
		else
			cns_df[!,va] = aggUniVar(var_df, select(cns_df,intCol(cns_df)), agg_arr, srcRes_ntup, anyM.sets)
		end
	end

	# aggregate in and out variables respectively and create actual constraint
	aggCol!(cns_df,in_arr)
	aggCol!(cns_df,out_arr)
	
	cns_df[!,:cnsExpr] = @expression(anyM.optModel,cns_df[in_arr[1]] .* cns_df[:eff] .- cns_df[out_arr[1]])
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
	uniId_arr = map(x -> (x.C,x.id),eachrow(unique(cns_df[!,[:C,:id]])))
	cCns_arr = Array{DataFrame}(undef,length(uniId_arr))

	for (idx,bal) in enumerate(uniId_arr)

		# get constraints relevant for carrier and find rows where mode is specified
		cnsC_df = filter(x -> x.C == bal[1] && x.id == bal[2],cns_df)

		m_arr = findall(0 .!= cnsC_df[!,:M])
		noM_arr = setdiff(1:size(cnsC_df,1),m_arr)

		if part.type == :emerging
			srcRes_ntup = anyM.cInfo[bal[1]] |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[bal[1]].lvl, M = 1))
		else
			srcRes_ntup = anyM.cInfo[bal[1]] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[bal[1]].lvl, M = 1))
		end

		# ! join in and out dispatch variables and adds efficiency to them (hence efficiency can be specific for different carriers that are stored in and out)
		for typ in (:in,:out)
			typVar_df = copy(cns_df[!,cnsDim_arr])
			# create array of all dispatch variables
			allType_arr = intersect(keys(part.carrier),typ == :in ? (:stExtIn,:stIntIn) : (:stExtOut,:stIntOut))
			# aborts if no variables on respective side exist
			if isempty(allType_arr)
				cnsC_df[!,typ] .= AffExpr() 
				continue
			end

			effPar_sym = typ == :in ? :effStIn : :effStOut
			# adds dispatch variables
			typExpr_arr = map(allType_arr) do va
				typVar_df = filter(x -> x.C == bal[1],part.par[effPar_sym].data) |> (x -> innerjoin(part.var[va],x; on = intCol(x)))
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
			cnsC_df = matchSetParameter(cnsC_df,part.par[:stDis],anyM.sets, defVal = 0.0)
			cnsC_df[!,:stDis] =   (1 .- cnsC_df[!,:val]) .^ sca_arr
			select!(cnsC_df,Not(:val))
		else
			cnsC_df[!,:stDis] .= 1.0
		end

		# add inflow parameter, if defined
		if :stInflow in keys(part.par)
			cnsC_df = matchSetParameter(cnsC_df,part.par[:stInflow],anyM.sets, newCol = :stInflow, defVal = 0.0)
			cnsC_df[!,:stInflow] = cnsC_df[!,:stInflow] .* sca_arr
			if !isempty(part.modes)
            	cnsC_df[!,:stInflow] = cnsC_df[!,:stInflow] ./ length(part.modes)
			end
		else
			cnsC_df[!,:stInflow] .= 0.0
		end

		# ! create final equation	
		cnsC_df[!,:cnsExpr] = @expression(anyM.optModel, cnsC_df[:stLvlPrev] .* cnsC_df[:stDis] .+ cnsC_df[:stInflow] .+ cnsC_df[:in] .- cnsC_df[:out] .- cnsC_df[:stLvl])
		cCns_arr[idx] = cnsC_df
	end

	cns_df =  vcat(cCns_arr...)
	return cnsCont(orderDf(cns_df[!,[cnsDim_arr...,:cnsExpr]]),:equal)
end

# ! computes design factors and create specific variables plus corresponding contraint for must run capacities where sensible
function prepareMustOut!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},prepTech_dic::Dict{Symbol,NamedTuple},yTs_dic::Dict{Int64,Int64},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	# adds specific variables for share of capacity used to satisfy must-run where sensible (for example chp plant in industry that could also have more capacity than required to be able to operate more flexible), 
	cMust_arr = unique(part.par[:desFac].data[!,:C])

	for capa in collect(filter(x ->x[1] in (:capaConv,:capaStOut),part.var))

		# for conversion specific variables are sensible, 
		if capa[1] == :capaStOut
			if !isempty(modeDep_dic[:stExtOut]) # if storage output is mode dependant
				var_df = capa[2]
			else # if also non must-run carriers are output of storage
				var_df = filter(x -> !isempty(setdiff(cMust_arr,part.carrier.stExtOut[x.id])),capa[2])
			end
		elseif capa[1] == :capaConv && (!isempty(setdiff(cMust_arr,collect(part.carrier.gen))) || !isempty(modeDep_dic[:gen])) # if non must-run carriers are generated or generation is mode dependant
			var_df = capa[2]
		else
			var_df = DataFrame()
		end

		if isempty(var_df) continue end
			
		# create capacity variables for must output and match them with general capacity variables
		part.var[Symbol("must",makeUp(capa[1]))] = createVar(var_df[!,Not(:var)],string("must",makeUp(capa[1])),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.capa)
		bothVar_df = innerjoin(rename(var_df,:var => :capa),part.var[Symbol("must",makeUp(capa[1]))],on = intCol(var_df))
		bothVar_df[!,:cnsExpr] = @expression(anyM.optModel, bothVar_df[:var] .- bothVar_df[:capa])
		cns_dic[Symbol("must",makeUp(capa[1]))] = cnsCont(select(bothVar_df,Not([:var,:capa])),:smaller)

	end

	# add missing capacity variables for storage in case of subproblems (otherwise infeasibility is possible, if storage cannot be charged sufficiently)
	if !isempty(anyM.subPro) && anyM.subPro != (0,0) && :costMissCapa in keys(anyM.parts.bal.par)
		stCapa_df = filter(x -> x.id != 0,part.par[:desFac].data)
		if !isempty(stCapa_df)
			stCapa_df = orderDf(select(matchSetParameter(select(stCapa_df,Not([:val])),anyM.parts.bal.par[:costMissCapa],anyM.sets),Not([:val])))
			part.var[:missCapa] = createVar(stCapa_df,"missCapa",NaN,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.insCapa)
		end	
	end
end

# ! compute design factors, either based on defined must run parameters or for all capacities in input dataframe
function computeDesFac!(part::TechPart,yTs_dic::Dict{Int64,Int64},anyM::anyModel,ts::Dict{Tuple{Int64,Int64},Array{Int64,1}}=Dict{Tuple{Int64,Int64},Array{Int64,1}}(), capa::DataFrame=DataFrame())

	# obtain all dispatch entries (either uses or defined must run or input dataframe of capacities)
	if isempty(capa)
		allFac_df = rename(part.par[:mustOut].data,:val => :mustOut)
	else
		allFac_df = capa
		# add dispatch timesteps and scenarios
		allFac_df[!,:Ts_dis] = map(x -> ts[(x.Ts_disSup,anyM.cInfo[x.C].tsDis)], eachrow(allFac_df))
		allFac_df[!,:scr] = map(x -> anyM.supTs.scr[x], allFac_df[!,:Ts_disSup])
		allFac_df = flatten(allFac_df,:scr)
		allFac_df = flatten(select(allFac_df,Not([:Ts_disSup])),:Ts_dis)
		# add must run factor of 1 to ensure consistency with other case
		allFac_df[!,:mustOut] .= 1.0
	end

	# add all operational modes
	if !isempty(part.modes)
		allFac_df[!,:M] = collect(part.modes) |> (z -> map(x -> z,1:size(allFac_df,1)))
		allFac_df = flatten(allFac_df,:M)
	else
		allFac_df[!,:M] .= 0
	end

	# split into conversion and storage capacities
	if isempty(capa)
		if :capaStOut in keys(part.var)
			facSt_df = allFac_df
			facSt_df[!,:id] = fill(unique(part.var[:capaStOut][!,:id]),size(facSt_df,1))
			facSt_df = flatten(facSt_df,:id)
		else
			facSt_df = DataFrame()
		end

		if :capaConv in keys(part.var)
			facConv_df = allFac_df
			facConv_df[!,:id] .= 0
		else
			facConv_df = DataFrame()
		end
	else
		facConv_df = filter(x -> x.id == 0,allFac_df)
		facSt_df = filter(x -> x.id != 0,allFac_df)
	end

	# get availability and efficiency for storage capacities
	if !isempty(facSt_df)
		facSt_df = matchSetParameter(facSt_df,part.par[:avaStOut],anyM.sets; newCol = :ava)
		facSt_df = orderDf(matchSetParameter(facSt_df,part.par[:effStOut],anyM.sets; newCol = :eff))
	else
		facSt_df = DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], M = Int[], scr = Int[], id = Int[], eff = Float64[], mustOut = Float64[], ava = Float64[])
	end

	# get availability and efficiency, and eventually ratios for conversion capacities
	if !isempty(facConv_df)
		# add availability, efficiency and ratios if any are defined
		facConv_df = matchSetParameter(facConv_df,part.par[:avaConv],anyM.sets; newCol = :ava)
		facConv_df = orderDf(matchSetParameter(facConv_df,part.par[:effConv],anyM.sets; newCol = :eff))

		# TODO enable other kinds of conversion ratios  
		# throws an error, if ratios are not implemented as fixed ratios on the must run carrier
		if :ratioConvOutUp in keys(part.par) || :ratioConvOutLow in keys(part.par) || (:ratioConvOutFix in keys(part.par) && isempty(capa) && !isempty(setdiff(unique(part.par[:ratioConvOutFix].data[!,:C]),unique(part.par[:mustOut].data[!,:C]))))
			push!(anyM.report,(3,"must output","","output of technology '$(createFullString(allFac_df[1,:Te],anyM.sets[:Te]))' is fixed, in this case so far the only permitted ratio parameter is fixing the ratio of the must-run carrier, but other ratios were detected"))
		end

		# adds conversion ratio if any defined
		if :ratioConvOutFix in keys(part.par)
			facConv_df = matchSetParameter(facConv_df,part.par[:ratioConvOutFix],anyM.sets; newCol = :ratio, defVal = 1.0)
			facConv_df[!,:eff] = facConv_df[!,:eff] .* facConv_df[!,:ratio]
			select!(facConv_df,Not([:ratio]))
		end
	else
		facConv_df = DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], M = Int[], scr = Int[], id = Int[], eff = Float64[], mustOut = Float64[], ava = Float64[])
	end

	#  computes overall mustrun factor in relation to input capacity for each timestep and mode
	allFac_df = vcat(facConv_df,facSt_df)
	allFac_df = combine(x -> (run = maximum(x.eff .* x.ava ./ x.mustOut), mustOut = x.mustOut[1]),groupby(allFac_df,filter(x -> x != :M,intCol(allFac_df))))
	allFac_df[!,:Ts_disSup] = map(x -> yTs_dic[x],allFac_df[!,:Ts_dis])
	allFac_df = combine(x -> (desFac = minimum(x.run) * maximum(x.mustOut),),groupby(allFac_df,filter(x -> !(x in [:Ts_dis,:scr]),intCol(allFac_df))))

	# add computed factors to parameter data
	if !isempty(allFac_df)
		parDef_ntup = (dim = (:Ts_expSup, :Ts_disSup, :R_dis, :C, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :C => :up, :Te => :up, :R_dis => :avg_any), part = :techConv)
		part.par[:desFac] = ParElement(DataFrame(),parDef_ntup,:desFac,anyM.report)
		part.par[:desFac].data = rename(allFac_df,:desFac => :val)
	end

end

# ! returns type of variable and corrects with energy content
function addEnergyCont(var_df::DataFrame,part::AbstractModelPart,sets_dic::Dict{Symbol,Tree})

	if :enCont in keys(part.par)
		part.par[:enCont].defVal = 1.0
		var_df = filter(x -> x.val != 0.0, matchSetParameter(var_df,part.par[:enCont],sets_dic))
		var_df[!,:var] = var_df[!,:var] .* var_df[!,:val]
		select!(var_df,Not([:val]))
	end

	return var_df
end

#endregion









