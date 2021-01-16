
# ! iteration over all technologies to create variables and constraints
function createTech!(tInt::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

	cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(tInt,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
	modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

	if part.type != :unrestricted
		# map required capacity constraints
		createCapaRestrMap!(part, anyM)

		# creates capacity, expansion, and retrofitting variables
		createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)

		# create expansion constraints
		if isempty(anyM.subPro) || anyM.subPro == (0,0)
			# connect capacity and expansion variables
			createCapaCns!(part,prepTech_dic,cns_dic)

			# control operated capacity variables
			if part.decomm != :none
				createOprVarCns!(part,cns_dic,anyM)
			end
		end
	end

    produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

    # create dispatch variables
	if isempty(anyM.subPro) || anyM.subPro != (0,0)
	    createDispVar!(part,modeDep_dic,ts_dic,r_dic,prepTech_dic,anyM)
	    produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")

	    # create conversion balance for conversion technologies
	    if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn)))) && :capaConv in keys(part.var)
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
			createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM)
		end
	    produceMessage(anyM.options,anyM.report, 3," - Prepared capacity restrictions for technology $(tech_str)")

	end

	# create ratio constraints
	createRatioCns!(part,cns_dic,r_dic,anyM)

    # all constraints are scaled and then written into their respective array position
    foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

    produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

    return cns_dic
end

# ! prepare dictionaries that specifies dimensions for expansion and capacity variables
function prepareTechs!(techSym_arr::Array{Symbol,1},prepAllTech_dic::Dict{Symbol,Dict{Symbol,NamedTuple}},tsYear_dic::Dict{Int,Int},anyM::anyModel)

	for tSym in techSym_arr
		prepTech_dic = Dict{Symbol,NamedTuple}()
		part = anyM.parts.tech[tSym]
        tInt = sysInt(tSym,anyM.sets[:Te])

	    # dimension of expansion and corresponding capacity variables
	    if part.type != :stock
	        prepareExpansion!(prepTech_dic, tsYear_dic, part, tInt, anyM)

			for expan in collectKeys(keys(prepTech_dic))
				prepareCapacity!(part,prepTech_dic,vcat(map(x -> x[!,removeVal(x)],prepTech_dic[expan])...),Symbol(replace(string(expan),"exp" => "capa")),anyM, sys = tInt)
			end
		end

		# check for capacities variables that have to be created, because of residual capacities provided
		addResidualCapaTech!(prepTech_dic, part, tInt, anyM)

		# ensure consistency among different storage capacities (to every storage in- or output capacity a corresponding storage size has to exist)
		stKey_arr = collectKeys(keys(prepTech_dic))

		if !isempty(intersect([:capaStIn,:capaStOut],stKey_arr))
			# determines all defined storage in- and output capacities
			allSt_arr = filter(z -> !isempty(z), vcat(map(y -> collect(map(x -> getfield(prepTech_dic[y],x),(:var,:resi))),intersect([:capaStIn,:capaStOut],stKey_arr))...))
			relSt_df = unique(vcat(map(w -> select(w,intCol(w)), allSt_arr)...))

			if isempty(relSt_df) continue end

			# finds cases where no storage size capacity can be matched to in- or output and adds corresponding entries
			if :capaStSize in stKey_arr
				newSize_df = (part.type == :stock ? [:resi,] : [:resi,:var]) |> 
								(z -> vcat(prepTech_dic[:capaStSize].var,antijoin(relSt_df,unique(vcat(map(u -> getfield(prepTech_dic[:capaStSize],u) |> (k -> select(k,intCol(k))) ,z)...)), on = names(relSt_df))))
				prepTech_dic[:capaStSize] = (var = newSize_df, resi = prepTech_dic[:capaStSize].resi)
			else
				prepTech_dic[:capaStSize]= (var = relSt_df, resi = DataFrame())
			end
		end

		# if any capacity variables or residuals were prepared, add these to overall dictionary
		if collect(values(prepTech_dic)) |> (z -> any(map(x -> any(.!isempty.(getfield.(z,x))), (:var,:resi))))
			prepAllTech_dic[tSym] = prepTech_dic
		end
	end
end

# ! add entries with residual capacities for technologies
function addResidualCapaTech!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,tInt::Int,anyM::anyModel)

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

	for va in collectKeys(keys(part.carrier)) |> (x -> :capaStIn in keys(prepTech_dic) ? [:stLvl,x...]  : x) # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use) && :capaConv in keys(prepTech_dic)
		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(prepTech_dic[:capaConv].var)
			basis_df[!,:C] .= [collect(getfield(part.carrier,va))]
			basis_df = orderDf(flatten(basis_df,:C))
		elseif :capaStIn in keys(prepTech_dic) && !(va in (:gen,:use))
			basis_df = orderDf(copy(prepTech_dic[:capaStIn].var))
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

	
	# aggregate in and out variables respectively and create actual constraint
	cns_df[!,:cnsExpr] = map(x -> sum(getindex(x,in_arr))*x.eff - sum(getindex(x,out_arr)),eachrow(cns_df))
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

#endregion


# TODO einlesen von vordefinierten faktoren
#=
# compute design factor
if :fixOut in keys(part.par) && :capaConv in keys(prepTech_dic)
	# obtained fixed out and add availability and efficiency
	allFac_df = rename(part.par[:fixOut].data,:val => :fixOut)
	
	allFac_df[!,:M] = collect(part.modes) |> (z -> map(x -> z,1:size(allFac_df,1)))
	allFac_df = flatten(allFac_df,:M)
	
	allFac_df = matchSetParameter(allFac_df,part.par[:avaConv],anyM.sets; newCol = :ava)
	allFac_df = orderDf(matchSetParameter(allFac_df,part.par[:effConv],anyM.sets; newCol = :eff))

	# TODO hier muss eben noch ein zu berechnender faktor für ratio rein!
	bla = combine(x -> (run = maximum(x.eff .* x.ava ./ x.fixOut), fixOut = x.fixOut[1]),groupby(allFac_df,filter(x -> x != :M,intCol(allFac_df))))

	# add Ts_disSup, group and compute design factor
	newTs_dic = Dict{Int,Int}()
	for x in collect(ts_dic), y in x[2]
		newTs_dic[y] = x[1][1]
	end

	bla[!,:Ts_disSup] = map(x -> newTs_dic[x],bla[!,:Ts_dis])

	blub = combine(x -> (desFac = minimum(x.run) * maximum(x.fixOut),),groupby(bla,filter(x -> !(x in [:Ts_dis,:scr]),intCol(bla))))
	# TODO muss design faktor wirkich im vorraus für ganze lebenszeit berechnet werden? deckt die normierung auf 1 das nicht ab? was wäre sonst mit stock technologies bzw. kapazitäten


	# ! adde information on restricted output due to output ratios
	#=
	# get cases where ratio of carrier with fixed output is also fixed
	if :ratioOutFix in keys(part.par)
		fixRatio_df = matchSetParameter(allFac_df,part.par[:ratioOutFix],anyM.sets; newCol = :ratio)
		noFixRatio_df = antijoin(allFac_df,fixRatio_df,on = intCol(allFac_df))
	else
		noFixRatio_df = allFac_df
	end

	# get cases where ratio is restriced by an upper limit on carrier itself
	if :ratioOutUp in keys(part.par)
		upRatio_df = matchSetParameter(noFixRatio_df,part.par[:ratioOutUp],anyM.sets; newCol = :upRatio)
	else
		upRatio_df = copy(noFixRatio_df)
		upRatio_df[!,:upRatio] .= 1.0
	end

	# get cases where output is either restriced by fixed or lower limits on output of other carriers
	fixC_arr = unique(allFac_df[!,:C])
	noFixC_arr = filter(x -> !(x in fixC_arr),collect(part.carrier.gen))

	checkOther_df = copy(noFixRatio_df)
	checkOther_df[!,:C] = map(x -> noFixC_arr,1:size(checkOther_df,1))
	checkOther_df = flatten(checkOther_df,:C)

	allOther_arr = Array{DataFrame,1}()
	for ratioRest in intersect((:ratioOutFix,:ratioOutLow),collect(keys(part.par)))
		othMatched_df = combine(groupby(matchSetParameter(checkOther_df,part.par[ratioRest],anyM.sets),filter(x -> x != :C,intCol(bla))), :val => (x -> 1 - sum(x)) => :val)
		if !isempty(othMatched_df)
			push!(allOther_arr,othMatched_df)
		end
	end

	if isempty(allOther_arr)
		otherRatio_df = copy(noFixRatio_df); otherRatio_df[!,:otherRatio] = 1.0
	elseif length(allOther_arr) == 1
		otherRatio_df = rename(allOther_arr[1],:val => :otherRatio)
		otherRatio_df[!,:C]  = map(x -> fixC_arr,1:size(otherRatio_df,1))
		otherRatio_df = flatten(otherRatio_df,:C)
	else

	end

	# determine if upper ratio on carrier itself or limits on other carriers are more restrictive
	upAndOther_df = outerjoin(upRatio_df,otherRatio_df,on = intCol(upRatio_df))
	upAndOther_df[!,:ratio] = map(x -> min(x.upRatio,x.otherRatio),eachrow(upAndOther_df))

	# join with cases for fixed ratio
	allFac_df = vcat(select(upAndOther_df,intCol(upAndOther_df,:ratio)),fixRatio_df)
	=#



	
end
=#