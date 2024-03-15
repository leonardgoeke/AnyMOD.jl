
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
			createCapaCns!(part,anyM.sets,prepTech_dic,cns_dic,anyM.optModel,anyM.options.holdFixed)

			# control operated capacity variables
			if part.decomm != :none createOprVarCns!(part,cns_dic,anyM) end
		end
	end

	# create dispatch variables and constraints
	if !isempty(part.var) || part.type == :unrestricted 

		# prepare must-run related parameters
		if :mustOut in keys(part.par)
			if part.type == :unrestricted
				push!(anyM.report,(3,"must output","","must-run parameter for technology '$(tech_str)' ignored, because technology is unrestricted,"))
			else
				computeDesFac!(part,yTs_dic,anyM)
				prepareMustOut!(part,modeDep_dic,cns_dic,anyM)
			end
		end

		# already return if purpose was only computation of design factors
		if anyM.options.onlyDesFac return cns_dic end

		# map required capacity constraints
		if part.type != :unrestricted 
			rmvOutC_arr = createCapaRestrMap!(part, anyM)
			# adjust tracking level of storage
			if !isnothing(part.stTrack)
				stSizeRow_arr = findall(map(x -> occursin("stSize",x), part.capaRestr[!,:cnstrType]))
				part.capaRestr[stSizeRow_arr,:lvlTs] .= part.stTrack
			end
		end
			
		produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

		# create dispatch variables and constraints
		if isempty(anyM.subPro) || anyM.subPro != (0,0) || anyM.options.createVI.bal || anyM.options.lvlFrs != 0
			createDispVar!(part,modeDep_dic,ts_dic,r_dic,prepTech_dic,anyM)
			produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")
 
			if anyM.subPro != (0,0) || anyM.options.createVI.bal
				# create conversion balance for conversion technologies
				if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn)))) && (:capaConv in keys(part.var) || part.type == :unrestricted) && part.balSign.conv != :none
					cns_dic[:convBal] = createConvBal(part,anyM)
					produceMessage(anyM.options,anyM.report, 3," - Prepared conversion balance for technology $(tech_str)")
				end

				# create storage balance for storage technologies
				if :stLvl in keys(part.var) && part.balSign.st != :none && !anyM.options.createVI.bal
					cns_dic[:stBal] = createStBal(part,anyM)
					produceMessage(anyM.options,anyM.report, 3," - Prepared storage balance for technology $(tech_str)")
				# only create storage inflows as generation for valid inequalities
				elseif :stInflow in keys(part.par) && anyM.options.createVI.bal 
					inPar_df = rename(copy(part.par[:stInflow].data),:Ts_expSup => :Ts_disSup)
					inPar_df[!,:Ts_expSup] = inPar_df[!,:Ts_disSup]
					inPar_df[!,:Ts_disSup] .= inPar_df[!,:Ts_dis]
					inPar_df[!,:val] = inPar_df[!,:val] .* getResize(inPar_df,anyM.sets[:Ts],anyM.supTs)
					inPar_df[!,:R_dis] .= 0
					inPar_df[!,:M] .= 0
					part.var[:gen] = combine(x -> (var = AffExpr(sum(x.val)),),groupby(inPar_df,filter(x -> x != :id, intCol(inPar_df))))
				end
			end

			# create capacity restrictions
			sizeRestr_boo =  anyM.options.lvlFrs != 0 && :stLvl in keys(part.var) && anyM.subPro == (0,0)
			if part.type != :unrestricted && (anyM.subPro != (0,0) || anyM.options.createVI.bal || sizeRestr_boo)
				if sizeRestr_boo filter!(x -> occursin("stSize",x.cnstrType),part.capaRestr) end
				createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM,yTs_dic,rmvOutC_arr)
			end

			# enforce achievable storage levels
			if anyM.options.createVI.st && anyM.options.lvlFrs != 0 && anyM.subPro == (0,0) && :stLvl in keys(part.var) cns_dic = createStVI(part,ts_dic,r_dic,cns_dic,anyM) end

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
	balLvl_ntup = part.balLvl

	tsExp_arr, rExp_arr   = [getfield.(getNodesLvl(anyM.sets[x[2]], balLvl_ntup.exp[x[1]]),:idx) for x in enumerate([:Ts,:R])]
	tsExpSup_arr = map(x -> getDescendants(x,anyM.sets[:Ts],false,anyM.supTs.lvl) |> (y -> typeof(y) == Array{Int,1} ? y : [y] ), tsExp_arr)
	if anyM.options.interCapa != :linear tsExp_arr = map(x -> [minimum(x)],tsExp_arr) end

	expDim_arr = vcat(collect(Iterators.product(Iterators.zip(tsExp_arr,tsExpSup_arr),rExp_arr))...)
	allMap_df =  getindex.(expDim_arr,1) |> (x -> DataFrame(Ts_exp = getindex.(x,1), Ts_expSup = getindex.(x,2), R_exp = getindex.(expDim_arr,2), Te = fill(tInt,length(expDim_arr))))

	stCar_arr = getCarrierFields(part.carrier,(:stExtIn,:stExtOut,:stIntIn,:stIntOut))
	convCar_arr=  getCarrierFields(part.carrier,(:use,:gen))

	
	# loops over type of capacities to specify dimensions of capacity variables
	for exp in (:Conv, :StIn, :StOut, :StSize)
		
		# saves required dimensions to dictionary
		if exp == :Conv && !isempty(convCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(allMap_df,part,exp,tsYear_dic,anyM), resi = DataFrame())
		elseif exp != :Conv && !isempty(stCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(combine(groupby(allMap_df,namesSym(allMap_df)), :Te => (x -> collect(1:countStGrp(part.carrier))) => :id),part,exp,tsYear_dic,anyM), resi = DataFrame())
		else
			continue
		end
		
	end
end

# ! add entries with residual capacities for technologies
function addResidualCapaTe!(prepTech_dic::Dict{Symbol,NamedTuple},part::TechPart,tInt::Int,anyM::anyModel)

	stCar_arr = getCarrierFields(part.carrier,(:stExtIn,:stExtOut,:stIntIn,:stIntOut))

	for resi in (:Conv, :StIn, :StOut, :StSize)

		# cretes dataframe of potential entries for residual capacities
		if resi == :Conv
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step))...),i) for i in (1,2)]
			potCapa_df = DataFrame(Ts_disSup = permutDim_arr[2], R_exp = permutDim_arr[1], Te = fill(tInt,length(permutDim_arr[1])))
		elseif !isempty(stCar_arr)
			permutDim_arr = [getindex.(vcat(collect(Iterators.product(getfield.(getNodesLvl(anyM.sets[:R], part.balLvl.exp[2]),:idx), anyM.supTs.step,collect(1:countStGrp(part.carrier))))...),i) for i in (1,2,3)]
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
	hasSt_boo = :capaStSize in keys(prepTech_dic) && (!anyM.options.createVI.bal || anyM.options.lvlFrs != 0)
	dispVar_arr = collectKeys(keys(part.carrier)) |> (x -> hasSt_boo  ? [:stLvl,x...]  : x)
	if anyM.subPro == (0,0) && !anyM.options.createVI.bal filter!(x -> x == :stLvl,dispVar_arr) end # case of top problem and reduced foresight
	onlyGen_boo = :gen in dispVar_arr && isempty(intersect([:use,:stIntIn],dispVar_arr))

	# filter map of capacity restrictions based on actually created dispatch variables
	if anyM.options.createVI.bal && isdefined(part,:capaRestr)
		if !hasSt_boo filter!(x -> !any(occursin.(["stIn","stOut","stSize"],x.cnstrType)),part.capaRestr) end
		if onlyGen_boo filter!(x -> false, part.capaRestr) end
	end

	for va in dispVar_arr # loop over all relevant kind of variables
		conv_boo = va in (:gen,:use) && :capaConv in keys(prepTech_dic)
		# dont create storage variables when only creating valid inequalities
		if (va in (:stExtOut,:stExtIn,:stLvl) || (va == :stIntIn && :gen in dispVar_arr) || (va == :stIntOut && :use in dispVar_arr)) && anyM.options.createVI.bal && anyM.options.lvlFrs == 0
			continue 
		end

		# dont create storage level for top problem if cycling is within foresight level
		if va == :stLvl && anyM.subPro == (0,0) && anyM.options.lvlFrs != 0 && anyM.scr.lvl <= part.stCyc continue end

		# obtains relevant capacity variable
		if conv_boo
			basis_df = copy(unique(vcat(map(x -> select(x,intCol(x)),collect(prepTech_dic[:capaConv]))...)))
			if isempty(basis_df) continue end
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

		# adds temporal level to dataframe
		cToLvl_dic = Dict(x => (anyM.cInfo[x].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[x].rDis) for x in unique(basis_df[!,:C]))
		basis_df[!,:lvlTs] = map(x -> cToLvl_dic[x][1],basis_df[!,:C])
		# replaces level for storage level with specific resolution provided
		if va == :stLvl && !isnothing(part.stTrack)
			if any(part.stTrack .> basis_df[!,:lvlTs])
				push!(anyM.report,(3,"technology mapping","storage tracked","specific resolution for tracking storage level provided for technology '$(string(sSym))' is more detailed than resolution for the stored carrier"))
				return
			end
			basis_df[!,:lvlTs] .= part.stTrack
		end
		
		# adds spatial level to dataframe
        basis_df[!,:lvlR] = map(x -> cToLvl_dic[x][2],basis_df[!,:C])
		defScr_arr = va == :stLvl && anyM.options.lvlFrs != 0 && !isempty(anyM.subPro) && anyM.scr.lvl > part.stCyc ? [anyM.subPro[2]] : Int[] 
        allVar_df = orderDf(expandExpToDisp(basis_df,ts_dic,r_dic,anyM.sets[:Ts],anyM.scr,true,defScr_arr))
	
		# add mode dependencies
		modeDep_df = copy(modeDep_dic[va])
		modeDep_df[!,:M] .= isempty(modeDep_df) ? [0] : [collect(part.modes)]
		modeDep_df = flatten(modeDep_df,:M)

		allVar_df = joinMissing(allVar_df,modeDep_df,namesSym(modeDep_dic[va]),:left,Dict(:M => 0))

		# adjust table for case of reduced foresight
		if anyM.options.lvlFrs != 0 && va == :stLvl
			# get time-steps that are at the start of a foresight period
			frsStep_arr = [getDescendants(x,anyM.sets[:Ts],false,y) for x in getfield.(getNodesLvl(anyM.sets[:Ts],anyM.options.lvlFrs),:idx), y in unique(map(x -> getfield(anyM.sets[:Ts].nodes[x],:lvl), allVar_df[!,:Ts_dis]))]
			frsStart_arr = vec(maximum.(frsStep_arr))
			# save copy of variable table with all periods
			if anyM.subPro != (0,0)	allVarFull_df = copy(allVar_df) end
			# set scenario to zero for all time-steps at the end of a foresight period
			allVar_df[!,:scr] = map(x ->  x.Ts_dis in frsStart_arr ? 0 : x.scr,eachrow(allVar_df))
			allVar_df = unique(allVar_df)
			
			# extend table with storage levels needed but not existing yet since scenario does not exist for previous period
			if isempty(anyM.subPro)
				allVarFull_df = combine(x -> (scr = x.Ts_dis[end] in frsStart_arr ? [getStScr(x.Ts_dis[end],part.stCyc,anyM.sets[:Ts],anyM.scr)] :  [x.scr],), groupby(allVarFull_df, filter(x -> x != :scr, intCol(allVarFull_df))))
				allVarFull_df = flatten(allVarFull_df,:scr)
			elseif anyM.subPro == (0,0)
				filter!(x -> x.scr == 0, allVar_df)
			end
		end

		# filter entries where availability is zero
		for avaPar in relAva_dic[va]
			if avaPar in keys(part.par) && !isempty(part.par[avaPar].data) && 0.0 in part.par[avaPar].data[!,:val]
				allVar_df = filter(x -> x.val != 0.0,  matchSetParameter(allVar_df,part.par[avaPar],anyM.sets))[!,Not(:val)]
			end
		end

		# filter cases where dispatch variable is fixed to zero
		if Symbol(va,:Fix) in keys(anyM.parts.lim.par) && !(anyM.options.createVI.bal && va == :use && :stIntOut in dispVar_arr)
			checkFix_boo = false
			# checks, if trying to filter zero fixes is sensible because cases could exist based on technology column
			if !(:Te in namesSym(anyM.parts.lim.par[Symbol(va,:Fix)].data)) 
				checkFix_boo = true
			else
				fixTe_arr = unique(anyM.parts.lim.par[Symbol(va,:Fix)].data[!,:Te])
				if 0 in fixTe_arr || sysInt(Symbol(part.name[end]),anyM.sets[:Te]) in fixTe_arr
					checkFix_boo = true
				end
			end	
			# actually filter zero fixes
			if checkFix_boo
				fixZero_df = select(filter(r -> r.val == 0, getFix(allVar_df,anyM.parts.lim.par[Symbol(va,:Fix)],anyM)),Not(:val))
				if !isempty(fixZero_df) allVar_df = antijoin(allVar_df,fixZero_df, on = intCol(fixZero_df)) end
			end
		end
		
		if isempty(allVar_df) continue end

		# computes value to scale up the global limit on dispatch variable that is provied per hour and create variable
		if conv_boo
			scaFac_fl = anyM.options.scaFac.dispConv
		else
			scaFac_fl = anyM.options.scaFac.dispSt
		end
		allVar_df = createVar(allVar_df,string(va), getUpBound(allVar_df,anyM.options.bound.disp / scaFac_fl,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets, scaFac = scaFac_fl)
	
		# extend table again for case of reduced foresight 
		if anyM.options.lvlFrs != 0 && va == :stLvl && anyM.subPro != (0,0)
			# create entries for all conceivable scenarios for start of each period again
			allScr_arr = filter(x -> x != 0, collect(keys(anyM.sets[:scr].nodes)))
			allVar_df[!,:scr] = map(x ->  x.scr == 0 ? allScr_arr : [x.scr],eachrow(allVar_df))
			allVar_df = flatten(allVar_df,:scr)
			# joins with stored dataframe of all entries that sould exist 
			allVar_df = innerjoin(allVarFull_df,allVar_df, on = intCol(allVarFull_df))
		end
		
		part.var[va] = orderDf(allVar_df)
	end
end

# ! create conversion balance
function createConvBal(part::TechPart,anyM::anyModel)

	cns_df = rename(part.par[:effConv].data,:val => :eff)

	sort!(cns_df,orderDim(intCol(cns_df)))
	agg_arr = filter(x -> !(x in (:M, :Te)) && (part.type == :emerging || x != :Ts_expSup), intCol(cns_df))

	# defines tuple specificing dimension of aggregation later
	if part.type == :emerging
		srcRes_ntup = part.balLvl |> (x -> (Ts_expSup = anyM.supTs.lvl, Ts_dis = x.ref[1], R_dis = anyM.options.createVI.bal ? 0 : x.ref[2]))
	else
		srcRes_ntup = part.balLvl |> (x -> (Ts_dis = x.ref[1], R_dis = anyM.options.createVI.bal ? 0 : x.ref[2]))
	end

	# aggregate table spatially in case of valid inequalities
	if anyM.options.createVI.bal
		cns_df[!,:R_dis] .= 0
		cns_df = combine(x -> (eff = maximum(x.eff),), groupby(cns_df,intCol(cns_df)))
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
	
	cns_df[!,:cnsExpr] = @expression(anyM.optModel,cns_df[!,in_arr[1]] .* cns_df[!,:eff] .- cns_df[!,out_arr[1]])

	return cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]), part.balSign.conv == :eq ? :equal : :greater)
end

# ! create storage balance
function createStBal(part::TechPart,anyM::anyModel)

	# ! get variables for storage level
	# get variables for current storage level
	cns_df = rename(part.var[:stLvl],:var => :stLvl)
	
	# filter cases where storage level variable just exists to formulate balance
	if anyM.options.lvlFrs != 0 
		filter!(x -> getAncestors(x.Ts_dis,anyM.sets[:Ts],:int,anyM.scr.lvl)[end] in keys(anyM.scr.scr), cns_df)
	end
	cnsDim_arr = filter(x -> x != :Ts_disSup, intCol(cns_df))

	# join variables for previous storage level
	tsChildren_dic = Dict((x,y) => getDescendants(x,anyM.sets[:Ts],false,y) for x in getfield.(getNodesLvl(anyM.sets[:Ts],part.stCyc),:idx), y in unique(map(x -> getfield(anyM.sets[:Ts].nodes[x],:lvl), cns_df[!,:Ts_dis])))
	filter!(x -> !isempty(x[2]),tsChildren_dic)
	firstLastTs_dic = Dict(minimum(tsChildren_dic[z]) => maximum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
	firstTs_arr = collect(keys(firstLastTs_dic))

	cns_df[!,:Ts_disPrev] = map(x -> x in firstTs_arr ? firstLastTs_dic[x] : x - 1, cns_df[!,:Ts_dis])
	cns_df = rename(joinMissing(cns_df,part.var[:stLvl], intCol(part.var[:stLvl]) |> (x -> Pair.(replace(x,:Ts_dis => :Ts_disPrev),x)), :left, Dict(:var => AffExpr())),:var => :stLvlPrev)

	# filter storage variables not enforcing a balance in case of interdependent subperiods (variables only exist to enforce right value at the start of the next period, occurs if number of scenarios varies)
	if anyM.options.lvlFrs != 0 filter!(x -> x.scr in anyM.scr.scr[getAncestors(x.Ts_dis,anyM.sets[:Ts],:int,anyM.scr.lvl)[end]], cns_df) end

	# determines dimensions for aggregating dispatch variables
	agg_arr = filter(x -> !(x in (:M, :Te)) && (part.type == :emerging || x != :Ts_expSup), cnsDim_arr)

	# obtain all different carriers of level variable and create array to store the respective level constraint data
	uniId_arr = map(x -> (x.C,x.id),eachrow(unique(cns_df[!,[:C,:id]])))
	cCns_arr = Array{DataFrame}(undef,length(uniId_arr))

	for (idx,bal) in enumerate(uniId_arr)

		# get constraints relevant for carrier and find rows where mode is specified
		cnsC_df = filter(x -> x.C == bal[1] && x.id == bal[2],cns_df)
		sort!(cnsC_df,orderDim(intCol(cnsC_df)))

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

			# adds dispatch variable to constraint dataframe, mode dependant and non-mode dependant balances have to be aggregated separately and timesteps only need aggregration if resolution for storage level differs
			dispVar_df = vcat(typExpr_arr...)
			cnsC_df[!,typ] .= AffExpr()
			if isempty(dispVar_df) continue end

			mAgg_tup = isnothing(part.stTrack) ?  (M = 1,) : (M = 1, Ts_dis = anyM.sets[:Ts].nodes[cnsC_df[1,:Ts_dis]].lvl)
			noMAgg_tup = isnothing(part.stTrack) ?  (M = 1,) : (M = 1, Ts_dis = anyM.sets[:Ts].nodes[cnsC_df[1,:Ts_dis]].lvl)
			cnsC_df[m_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[m_arr,:],intCol(cnsC_df)), [:M,agg_arr...], mAgg_tup, anyM.sets)
			cnsC_df[noM_arr,typ] = aggUniVar(dispVar_df, select(cnsC_df[noM_arr,:],intCol(cnsC_df)), [:M,agg_arr...], noMAgg_tup, anyM.sets)			
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

		# add infeasibility variables for reduced foresight with cutting plane algorithm
		if anyM.options.lvlFrs != 0 && !isempty(anyM.subPro) && anyM.scr.lvl > part.stCyc && :costStLvlLss in keys(anyM.parts.cost.par)
			# get time-steps at end of foresight period
			endTs_df = combine(x -> (Ts_dis = maximum(x.Ts_dis),),groupby(cnsC_df,filter(x -> !(x in (:Ts_dis,:Ts_disPrev)),intCol(cnsC_df))))
			# add times-steps at start of foresight period, if option is set
			if anyM.options.dbInf
				startTs_df = combine(x -> (Ts_dis = minimum(x.Ts_dis),),groupby(cnsC_df,filter(x -> !(x in (:Ts_dis,:Ts_disPrev)),intCol(cnsC_df))))
				endTs_df = vcat(startTs_df,endTs_df)
			end
			
			matchTs_df = select(matchSetParameter(endTs_df,anyM.parts.cost.par[:costStLvlLss],anyM.sets),Not([:val]))
			if !isempty(matchTs_df)
				# on net-increase
				part.var[:stLvlInfeasIn] = createVar(matchTs_df,"stLvlInfeasIn",getUpBound(matchTs_df,anyM.options.bound.disp,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets)
				bothInf_df = copy(part.var[:stLvlInfeasIn])
				# on net-decrease, only needed for output if storage balance is an equality constraint
				if part.balSign.st != :ineq
					part.var[:stLvlInfeasOut] = createVar(matchTs_df,"stLvlInfeasOut",getUpBound(matchTs_df,anyM.options.bound.disp,anyM.supTs,anyM.sets[:Ts]),anyM.optModel,anyM.lock,anyM.sets)
					if :stLvlInfeasIn in keys(part.var)
						bothInf_df[!,:var] .= bothInf_df[!,:var] .- part.var[:stLvlInfeasOut][!,:var]
					else
						bothInf_df = copy(part.var[:stLvlInfeasOut])
						bothInf_df[!,:var] .= -1 .* bothInf_df[!,:var]
					end
				end 
				# add expression to constraint, if any infeasibility variables exist
				if :stLvlInfeasIn in keys(part.var) || :stLvlInfeasOut in keys(part.var)
					cnsC_df = joinMissing(cnsC_df,rename(bothInf_df,:var => :infeas), intCol(bothInf_df), :left, Dict(:infeas => AffExpr()))
					cnsC_df[!,:stInflow] .= cnsC_df[!,:stInflow] .+ cnsC_df[!,:infeas]
					select!(cnsC_df,Not([:infeas]))
				end
			end
		end

		# ! create final equation	
		cnsC_df[!,:cnsExpr] = @expression(anyM.optModel, cnsC_df[!,:stLvlPrev] .* cnsC_df[!,:stDis] .+ cnsC_df[!,:stInflow] .+ cnsC_df[!,:in] .- cnsC_df[!,:out] .- cnsC_df[!,:stLvl])
		cCns_arr[idx] = cnsC_df
	end

	cns_df =  vcat(cCns_arr...)
	return cnsCont(orderDf(cns_df[!,[cnsDim_arr...,:cnsExpr]]),part.balSign.st == :eq ? :equal : :greater)
end

# ! enforces storage levels that can actually be achieved with the available capacity
function createStVI(part::TechPart,ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	stLvl_df = copy(part.var[:stLvl])
	cnsDim_arr = filter(x -> x != :Ts_disSup, intCol(stLvl_df))

	# gets order of time-steps
	relTs_arr = unique(stLvl_df[!,:Ts_dis])
	tsChildren_dic = Dict((x,y) => intersect(relTs_arr,getDescendants(x,anyM.sets[:Ts],false,y)) for x in getfield.(getNodesLvl(anyM.sets[:Ts],part.stCyc),:idx), y in unique(map(x -> getfield(anyM.sets[:Ts].nodes[x],:lvl), relTs_arr)))
	filter!(x -> !isempty(x[2]),tsChildren_dic)
	firstLastTs_dic = Dict(minimum(tsChildren_dic[z]) => maximum(tsChildren_dic[z]) for z in keys(tsChildren_dic))
	firstTs_arr = collect(keys(firstLastTs_dic))
	orderTs_dic = Dict(x => x in firstTs_arr ? firstLastTs_dic[x] : map(x -> x > 0 ? x : Inf,x .- relTs_arr) |> (z -> relTs_arr[findall(z .== minimum(z))][end]) for x in relTs_arr)

	# joins previous storage level
	stLvl_df[!,:Ts_disPrev] = map(x -> orderTs_dic[x], stLvl_df[!,:Ts_dis])
	rename!(stLvl_df,:var => :stLvlCur)
	stLvl_df = rename(joinMissing(stLvl_df,part.var[:stLvl], intCol(part.var[:stLvl]) |> (x -> Pair.(replace(x,:Ts_dis => :Ts_disPrev),x)), :left, Dict(:var => AffExpr())),:var => :stLvlPrev)

	# add scenarios 
	stLvl_df[!,:scr] .= filter(x -> x != 0, collect(keys(anyM.sets[:scr].nodes))) |> (z -> map(x -> z,eachrow(stLvl_df)))
	stLvl_df = flatten(stLvl_df,:scr)

	# assign end of period to all steps in period
	inTs_dic = Dict{Int,Int}()
	for x in collect(orderTs_dic)
		for y in getAncestors(x[1],anyM.sets[:Ts],:int,anyM.scr.lvl)[end] |> (z -> getDescendants(z,anyM.sets[:Ts],false,anyM.sets[:Ts].nodes[x[1]].lvl))
			inTs_dic[y] = x[1]
		end
	end

	# aggregate storage inflows
	if :stInflow in keys(part.par)
		# replace actual timestep with ending timestep of foresight period
		stInf_df = copy(part.par[:stInflow].data)
		stInf_df[!,:Ts_dis] .= map(x -> inTs_dic[x],stInf_df[!,:Ts_dis])
		# aggregate by end of foresight period and add to levels
		aggInflow_df = combine(x -> (inf = sum(x.val)/length(x.val),), groupby(stInf_df,intCol(stInf_df)))
		
		# join inflows with storage levels
		stLvl_df = innerjoin(stLvl_df,aggInflow_df,on = intCol(aggInflow_df))
	else
		stLvl_df[!,:inf] .= 0.0
	end

	# create all relevant restrictions
	for restr in filter(x -> occursin("stSize",x.cnstrType), eachrow(part.capaRestr))
		for t in (:In,:Out)
			if t == :In || part.balSign.st != :ineq # valid inequality on output only relevant, if storage balance is not an inequality constraint
				
				# join relevant capacity if existing
				if Symbol(:capaSt,t) in keys(part.var)
					~, cns_df, ~ = getCapaToRestr(part,copy(part.var[Symbol(:capaSt,t)]),restr,:capaStSize,ts_dic,r_dic,anyM.sets,anyM.supTs)
					cns_df = rename(innerjoin(stLvl_df, cns_df, on = intCol(cns_df)),:var => :capa)
				else
					cns_df = copy(stLvl_df)
					cns_df[!,:capa] .= AffExpr()
				end
	
				# create constraint
				if t == :In
					# correct input capacity with charging efficiency and availability
					allAvaEff_df = matchSetParameter(rename(part.par[:effStIn].data,:val => :eff),part.par[:avaStIn],anyM.sets; newCol = :ava)
					allAvaEff_df[!,:Ts_dis] .= map(x -> inTs_dic[x],allAvaEff_df[!,:Ts_dis]) # replace actual timestep with ending timestep of foresight period
					allAvaEff_df = combine(x -> (avaEff = sum(x.eff.*x.ava)/length(x.eff),), groupby(allAvaEff_df,intCol(allAvaEff_df)))
					# join with constraint dataframe
					cns_df = innerjoin(cns_df,allAvaEff_df,on = intCol(allAvaEff_df))
					
					# filter redundant constraints 
					cns_df = combine(x -> fltRedIn(x), groupby(cns_df,filter(x -> x != :scr,intCol(cns_df))))
						
					# create constraint expression
					expr_arr = map(x -> getScaFac(x.Ts_dis,anyM) |> (y -> x.stLvlCur - x.stLvlPrev - x.capa * x.avaEff * y - x.inf * y), eachrow(cns_df))
				else
					# correct output capacity with availability
					if !isempty(part.par[:avaStOut].data)
						# get average availability for each foresight period
						avaOut_df = copy(part.par[:avaStOut].data)
						avaOut_df[!,:Ts_dis] .= map(x -> inTs_dic[x],avaOut_df[!,:Ts_dis]) # replace actual timestep with ending timestep of foresight period
						avaOut_df = combine(x -> (ava = sum(x.val)/length(x.val),), groupby(avaOut_df,intCol(avaOut_df)))
						# join with constraint dataframe
						cns_df = innerjoin(cns_df,avaOut_df,on = intCol(avaOut_df))
					else
						cns_df[!,:ava] .= 1.0
					end
					
					# compute upper bound on storage self-discharge losses
					if :stDis in keys(part.par)
						# get variables for storage size
						~, stSize_df, ~ = getCapaToRestr(part,copy(part.var[:capaStSize]),restr,:capaStSize,ts_dic,r_dic,anyM.sets,anyM.supTs)
						# get average product of discharge and availability for each foresight period
						disAva_df = matchSetParameter(rename(part.par[:stDis].data,:val => :dis),part.par[:avaStSize],anyM.sets; newCol = :ava)
						disAva_df[!,:Ts_dis] .= map(x -> inTs_dic[x],disAva_df[!,:Ts_dis]) # replace actual timestep with ending timestep of foresight period
						disAva_df = combine(x -> (maxStDis = sum(x.dis.*x.ava)/length(x.dis),), groupby(disAva_df,intCol(disAva_df)))
						disAva_df[!,:Ts_disSup] .= map(x -> getAncestors(x,anyM.sets[:Ts],:int,anyM.supTs.lvl)[end],disAva_df[!,:Ts_dis])
						# join with storage size to compute self-discharge losses
						disAva_df = innerjoin(disAva_df,stSize_df,on = intCol(stSize_df))
						disAva_df[!,:maxStDis] .= disAva_df[!,:maxStDis] .* disAva_df[!,:var]
						# join with constraint dataframe
						cns_df = innerjoin(cns_df,disAva_df,on = intCol(disAva_df))
					else
						cns_df[!,:maxStDis] .= 0.0
					end
					
					# filter redundant constraints
					cns_df = combine(x -> fltRedOut(x), groupby(cns_df,filter(x -> x != :scr,intCol(cns_df))))

					# create constraint expression
					expr_arr = map(x -> getScaFac(x.Ts_dis,anyM) |> (y -> x.stLvlPrev - x.stLvlCur + x.inf * y - (x.capa * x.ava + x.maxStDis) * y), eachrow(cns_df))
				end

				cns_df[!,:cnsExpr] = @expression(anyM.optModel,expr_arr)
				cns_dic[Symbol(:stLvlMax,t)] = cnsCont(orderDf(cns_df[!,[cnsDim_arr...,:cnsExpr]]),:smaller)
			end
		end
	end
	
	return cns_dic

end

# filter redundant constraints for valid inequalities on storage (see function above)
function fltRedIn(in_dfr::SubDataFrame)
	# in case input capacity does not exist or maximum input is the same in both scenarios, only keep case with smaller inflows
	if all(in_dfr.capa .== AffExpr()) || length(unique(in_dfr.avaEff)) == 1
		relIdx_int = minimum(in_dfr.inf) |> (z -> findall(in_dfr.inf .== z)[1])
	# if inflows are the same, only keep smaller avaEff
	elseif length(unique(in_dfr.inf)) == 1
		relIdx_int = minimum(in_dfr.avaEff) |> (z -> findall(in_dfr.avaEff .== z)[1])						
	end
	return in_dfr[relIdx_int,:]
end

function fltRedOut(in_dfr::SubDataFrame)
	
	# checks which parameters have multiple unique values (if only one parameter has multiple unique values, some constraints can be filtered)
	avaUni_boo = !(all(in_dfr.capa .== AffExpr()) || length(unique(in_dfr.ava)) == 1)
	maxDisUni_boo = length(unique(in_dfr.maxStDis)) != 1
	infUni_boo = length(unique(in_dfr.inf)) != 1
	
	# ouptut availability and maximum discharge are the same => only inflows are relevant
	if !avaUni_boo && !maxDisUni_boo
		relIdx_int = maximum(in_dfr.inf) |> (z -> findall(in_dfr.inf .== z)[1])
	# output availability and inflows are the same => only maximum discharge is relevant
	elseif !avaUni_boo && !infUni_boo
		relIdx_int = minimum(in_dfr.maxStDis) |> (z -> findall(in_dfr.maxStDis .== z)[1])
	# maximum discharge and inflows are the same => only output availability is relevant
	elseif !maxDisUni_boo && !infUni_boo
		relIdx_int = minimum(in_dfr.ava) |> (z -> findall(in_dfr.ava .== z)[1])
	end
	return in_dfr[relIdx_int,:]
end

# ! compute design factors, either based on defined must run parameters or for all capacities in input dataframe
function computeDesFac!(part::TechPart,yTs_dic::Dict{Int64,Int64},anyM::anyModel)
	
	# obtain all dispatch entries (either uses or defined must run or input dataframe of capacities)
	allFac_df = rename(part.par[:mustOut].data,:val => :mustOut)

	# add all operational modes
	if !isempty(part.modes)
		allFac_df[!,:M] = collect(part.modes) |> (z -> map(x -> z,1:size(allFac_df,1)))
		allFac_df = flatten(allFac_df,:M)
	else
		allFac_df[!,:M] .= 0
	end

	# split into conversion and storage capacities
	if :capaStOut in keys(part.var) # only if capacities exist and storage can be charged internally or externally
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

	# get availability and efficiency for storage capacities to compute ratio between input capacity and maximum output
	if !isempty(facSt_df)
		facSt_df = matchSetParameter(facSt_df,part.par[:avaStOut],anyM.sets; newCol = :ava)
		facSt_df = orderDf(matchSetParameter(facSt_df,part.par[:effStOut],anyM.sets; newCol = :eff))
	else
		facSt_df = DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], M = Int[], scr = Int[], id = Int[], eff = Float64[], mustOut = Float64[], ava = Float64[])
	end

	# get availability, efficiency and conversion ratios for conversion capacities to compute ratio between input capacity and maximum output
	if !isempty(facConv_df)
		
		# add availability and efficiency
		facConv_df = matchSetParameter(facConv_df,part.par[:avaConv],anyM.sets; newCol = :ava)
		facConv_df = orderDf(matchSetParameter(facConv_df,part.par[:effConv],anyM.sets; newCol = :eff))

		if any(map(x -> x in keys(part.par), [:ratioConvOutUp,:ratioConvOutLow,:ratioConvOutFix]))
			# set restriction from ratio to one per default
			facConv_df[!,:ratio] .= 1.0

			# corrects ratio, if output must-out carrier is fixed or subject to a upper limit
			for lim in (:Fix,:Up)
				if Symbol(:ratioConvOut,lim) in keys(part.par)
					facConv_df = matchSetParameter(facConv_df,part.par[Symbol(:ratioConvOut,lim)],anyM.sets; newCol = :ratio2, defVal = 1.0)
					facConv_df[!,:ratio] = map(x -> min(x.ratio2,x.ratio), eachrow(facConv_df))
					select!(facConv_df,Not([:ratio2]))
				end
			end

			# corrects ratio, if output of other carriers is fixed or has a lower limit, effectively limiting the mustRun carrier as well
			if any(map(x -> x in keys(part.par), [:ratioConvOutLow,:ratioConvOutFix]))

				# collects lower and fixed ratios and joins them in one dataframe
				otherRatio_dic = Dict(x => Symbol(:ratioConvOut,x) |> (u -> u in keys(part.par) ? rename(part.par[u].data,:val => x) : DataFrame()) for x in [:Fix,:Low])

				if !isempty(otherRatio_dic[:Fix]) && !isempty(otherRatio_dic[:Low])
					# ensures either both or none of the dataframes are mode specific so they can be joines
					for l in ((:Fix,:Low),(:Low,:Fix))
						if !(:M in namesSym(otherRatio_dic[l[1]])) && :M in namesSym(otherRatio_dic[l[2]])
							otherRatio_dic[l[1]][!,:M] .= unique(otherRatio_dic[l[2]][!,:M]) |> (w -> map(x -> w, 1:size(otherRatio_dic[l[1]],1)))
							otherRatio_dic[l[1]] = flatten(otherRatio_dic[l[1]],:M)
						end
					end
					joinRatio_df = joinMissing(otherRatio_dic[:Fix], otherRatio_dic[:Low], intCol(otherRatio_dic[:Low]), :outer, Dict(:Fix => 0.0, :Low => 0.0))
				elseif isempty(otherRatio_dic[:Fix])
					joinRatio_df = otherRatio_dic[:Low]
					joinRatio_df[!,:Fix] .= 0.0
				else
					joinRatio_df = otherRatio_dic[:Fix]
					joinRatio_df[!,:Low] .= 0.0
				end

				# determines binding ratio on lower deployment level 
				joinRatio_df[!,:lowAll] =  map(x -> max(x.Fix,x.Low),eachrow(joinRatio_df))
				select!(joinRatio_df,Not([:Fix,:Low]))

				# loop over must-out carrier to compute ratios on other carriers affecting its maximum output
				cMust_arr = unique(facConv_df[!,:C])
				othRatio_df = DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], M = Int[], scr = Int[], val = Float64[])
		
				for c in cMust_arr
					cOthRatio_df = combine(groupby(filter(x -> x.C != c,joinRatio_df),filter(x -> x != :C, intCol(joinRatio_df))), :lowAll => (x -> 1 - sum(x)) => :val)
					if isempty(cOthRatio_df) continue end
					cOthRatio_df[!,:C] .= c
					if !(:M in intCol(cOthRatio_df)) cOthRatio_df[!,:M] .= 0 end
					append!(othRatio_df,cOthRatio_df)
				end

				if !isempty(othRatio_df)
					# create new parameter object to match obtained data
					paraDef_ntup = (dim = (:Ts_dis, :Ts_expSup, :R_dis, :C, :Te, :M, :scr), problem = :sub,  defVal = nothing, herit = (:Ts_expSup => :up, :Ts_dis => :avg_any, :R_dis => :up, :Te => :up, :Ts_dis => :up, :scr => :up), part = :techConv)
					othRatio_obj = ParElement(DataFrame(),paraDef_ntup,:ratioConvOutFix,anyM.report)
					othRatio_obj.data = othRatio_df

					facConv_df = matchSetParameter(facConv_df,othRatio_obj,anyM.sets; newCol = :othRatio, defVal = 1.0)
					facConv_df[!,:ratio] = map(x -> min(x.othRatio,x.ratio), eachrow(facConv_df))
					select!(facConv_df,Not([:othRatio]))
				end
			end

			# correct efficiency for ratio
			facConv_df[!,:eff] = facConv_df[!,:eff] .* facConv_df[!,:ratio]
			select!(facConv_df,Not([:ratio]))
		end
	else
		facConv_df = DataFrame(Ts_expSup = Int[], Ts_dis = Int[], R_dis = Int[], C = Int[], Te = Int[], M = Int[], scr = Int[], id = Int[], eff = Float64[], mustOut = Float64[], ava = Float64[])
	end

	#  computes overall must-run factor in relation to input capacity for each timestep and mode
	allFac_df = vcat(facConv_df,facSt_df)
	allFac_df = combine(x -> (run = maximum(x.eff .* x.ava ./ x.mustOut), mustOut = x.mustOut[1]),groupby(allFac_df,filter(x -> x != :M,intCol(allFac_df))))
	allFac_df[!,:Ts_disSup] = map(x -> yTs_dic[x],allFac_df[!,:Ts_dis])
	allFac_df = combine(x -> (desFac = minimum(x.run) * maximum(x.mustOut),),groupby(allFac_df,filter(x -> !(x in [:Ts_dis,:scr]),intCol(allFac_df))))


	# check for pre-defined capacity factors and use them instead of computed values
	if :desFac in keys(part.par)
		preDefFac_df = matchSetParameter(select(allFac_df,Not([:desFac])),part.par[:desFac],anyM.sets, newCol = :desFac_pre)
		# check cases with pre-existing factors
		extDesFac_df = innerjoin(allFac_df, preDefFac_df, on = intCol(allFac_df))
		# use maximum of pre-defined and computed value to ensure feasibility
		extDesFac_df[!,:desFac] = map(x -> max(x.desFac, x.desFac_pre), eachrow(extDesFac_df))
		select!(extDesFac_df, Not([:desFac_pre]))
		allFac_df = vcat(antijoin(allFac_df, extDesFac_df, on = intCol(allFac_df)), extDesFac_df)
	end

	# add computed factors to parameter data
	if !isempty(allFac_df)
		parDef_ntup = (dim = (:Ts_expSup, :Ts_disSup, :R_dis, :C, :Te, :id), problem = :top, defVal = nothing, herit = (:Ts_expSup => :up, :Ts_disSup => :up, :R_dis => :up, :C => :up, :Te => :up, :R_dis => :avg_any), part = :techConv)
		part.par[:desFac] = ParElement(DataFrame(),parDef_ntup,:desFac,anyM.report)
		part.par[:desFac].data = rename(allFac_df,:desFac => :val)
	end
end

# ! computes design factors and create specific variables plus corresponding contraint for must run capacities where sensible
function prepareMustOut!(part::TechPart,modeDep_dic::Dict{Symbol,DataFrame},cns_dic::Dict{Symbol,cnsCont},anyM::anyModel)

	# adds specific variables for must-out, if it is sensible for technology (for example chp plant in industry that could also have more capacity than required to be able to operate more flexible), 
	cMust_arr = unique(part.par[:desFac].data[!,:C])

	for capa in collect(filter(x ->x[1] in (:capaConv,:capaStOut),part.var))
		
		#region # * create variable for must capacity
		demC_arr = union(map(x -> getDescendants(x,anyM.sets[:C],true,0) ,unique(anyM.parts.bal.par[:dem].data[!,:C]))...) # gets all carrier with demand defined

		if capa == :capaStOut# specific variables for must-out of storage
			if !isempty(modeDep_dic[:stExtOut]) # storage output is mode dependant
				capa_df = capa[2]
			else # non must-out carriers are output of storage too
				capa_df = filter(x -> !isempty(setdiff(cMust_arr,part.carrier.stExtOut[x.id])),capa[2])
			end
			# filter carriers where storage capacity is even allowed to appear in capacity balance
			idCar_dic = Dict(x => filter(y -> anyM.cInfo[y].stBalCapa == :yes,collect(part.carrier.stExtOut[x])) for x in 1:length(part.carrier.stExtOut))
			filter!(x -> !isempty(idCar_dic[x.id]), capa_df)
		elseif capa[1] == :capaConv && (!isempty(setdiff(collect(part.carrier.gen),cMust_arr)) || !isempty(modeDep_dic[:gen]) || !isempty(intersect(demC_arr, part.carrier.gen))) # if non must-run carriers are generated, generation is mode dependant or there is demand defined for the carrier
			capa_df = capa[2]
		else
			capa_df = DataFrame()
		end

		if isempty(capa_df) continue end

		# create capacity variables for must-out
		mustCapa_df = capa_df[!,Not(:var)]
		if anyM.options.holdFixed # replace fixed variables with a parameter, if holdFixed is active
			# get relevant parameter data
			fixMust_df = getFix(mustCapa_df,getLimPar(anyM.parts.lim,Symbol("must",makeUp(capa[1]),:Fix),anyM.sets[:Te], sys = sysInt(Symbol(part.name[end]),anyM.sets[:Te])),anyM)
			if !(:val in namesSym(fixMust_df)) fixMust_df[!,:val] .= 0.0 end; fixMust_df = rename(fixMust_df,:val => :var)
			# get all cases where variables are fixed
			varMust_df = createVar(antijoin(mustCapa_df,fixMust_df, on = intCol(fixMust_df)),string("must",makeUp(capa[1])),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.capa)
			part.var[Symbol("must",makeUp(capa[1]))] = orderDf(vcat(fixMust_df,varMust_df))
		else
			part.var[Symbol("must",makeUp(capa[1]))] = createVar(capa_df[!,Not(:var)],string("must",makeUp(capa[1])),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.capa)
		end
		#endregion

		#region # * control must capacity 

		if isempty(anyM.subPro) || anyM.subPro == (0,0)
			decom_boo = part.decomm != :none
			exp_sym = Symbol(replace(String(capa[1]),"capa" => "exp"))
			
			# ensure must-out capacity complies with operated capacity in case of decommissioning
			if decom_boo || !(exp_sym in keys(part.var)) # limit must out capacity with ordinary capacity if necessary in case of decomm, if this is not enforced indirectly via expansion
				bothVar_df = innerjoin(rename(capa_df,:var => :capa),part.var[Symbol("must",makeUp(capa[1]))],on = intCol(capa_df))
				bothVar_df[!,:cnsExpr] = @expression(anyM.optModel, bothVar_df[!,:var] .- bothVar_df[!,:capa])
				cns_dic[Symbol("must",makeUp(capa[1]),decom_boo ? "Opr" : "")] = cnsCont(select(bothVar_df,Not([:var,:capa])),:smaller)
			else exp_sym in keys(part.var) # creates and enforces specific expansion variables for must out
				# create expansion variables for must-out
				expVar_df = capa == :capaStOut ? filter(x -> x.id in unique(capa_df[!,:id]), part.var[exp_sym]) : part.var[exp_sym]
				mustExp_df = expVar_df[!,Not(:var)] 
				if anyM.options.holdFixed # replace fixed variables with a parameter, if holdFixed is active
					# get all cases where variables are fixed
					fixMust_df = getFix(mustExp_df,getLimPar(anyM.parts.lim,Symbol("must",makeUp(exp_sym),:Fix),anyM.sets[:Te], sys = sysInt(Symbol(part.name[end]),anyM.sets[:Te])),anyM)
					if !(:val in namesSym(fixMust_df)) fixMust_df[!,:val] .= 0.0 end; fixMust_df = rename(fixMust_df,:val => :var)
					varMust_df = createVar(antijoin(mustExp_df,fixMust_df, on = intCol(fixMust_df)),string("must",makeUp(exp_sym)),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.insCapa)
					part.var[Symbol("must",makeUp(exp_sym))] = orderDf(vcat(fixMust_df,varMust_df))
				else
					part.var[Symbol("must",makeUp(exp_sym))] = createVar(expVar_df[!,Not(:var)],string("must",makeUp(exp_sym)),anyM.options.bound.capa,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.insCapa)
				end
				
				# match expansion variable for must-out with general expansion variables
				bothVar_df = innerjoin(select(rename(expVar_df,:var => :exp),Not([:Ts_disSup])),select(part.var[Symbol("must",makeUp(exp_sym))],Not([:Ts_disSup])),on = intCol(expVar_df))
				bothVar_df[!,:cnsExpr] = @expression(anyM.optModel, bothVar_df[!,:var] .- bothVar_df[!,:exp])
				filter!(x -> !(isempty(x.cnsExpr.terms)), bothVar_df)
				cns_dic[Symbol("must",makeUp(exp_sym))] = cnsCont(select(bothVar_df,Not([:var,:exp])),:smaller)
		
				# connect must-out capacity and expansion variables
				cns_df = rename(part.var[Symbol("must",makeUp(capa[1]))],:var => :capa)
				expVar_df = flatten(part.var[Symbol("must",makeUp(exp_sym))],:Ts_disSup)
				join_arr = intCol(cns_df) |> (w -> part.type != :mature ? w : filter(x -> x != :Ts_expSup,collect(w)))
				cns_df = joinMissing(cns_df,combine(groupby(expVar_df,join_arr), :var => (x -> sum(x)) => :exp),join_arr,:left,Dict(:exp => AffExpr()))

				# add residual capacities to dataframe
				insCapa_df = select(copy(part.var[decom_boo ? Symbol(:ins,makeUp(capa[1])) : capa[1]]),Not([:var]))
				insCapa_df = joinMissing(insCapa_df,rename(checkResiCapa(capa[1],insCapa_df, part, anyM),:var => :resi),intCol(insCapa_df),:left,Dict(:resi => AffExpr()))
				cns_df = innerjoin(cns_df, select(insCapa_df,vcat(join_arr,[:resi])), on = join_arr)
				
				# create upper and lower limits on must-out capacity where there is residual capacity, otherwise just create on equality constraint
				cnsEq_df, cnsNoEq_df = [filter(z,cns_df) for z in (x -> x.resi == 0.0, x -> x.resi != 0.0)]
				if !isempty(cnsEq_df)
					cnsEq_df[!,:cnsExpr] = @expression(anyM.optModel,cnsEq_df[!,:capa] .- cnsEq_df[!,:exp])	
					filter!(x -> !(isempty(x.cnsExpr.terms)), cnsEq_df)
					cns_dic[Symbol("must",makeUp(capa[1]),"Eq")] = cnsCont(select(cnsEq_df,intCol(cnsEq_df,:cnsExpr)),:equal)
				end
				if !isempty(cnsNoEq_df)
					# set lower limit based on expansion
					cnsNoEq_df[!,:cnsExpr] = @expression(anyM.optModel,cnsNoEq_df[!,:capa] .- cnsNoEq_df[!,:exp])
					filter!(x -> !(isempty(x.cnsExpr.terms)), cnsNoEq_df)
					cns_dic[Symbol("must",makeUp(capa[1]),"Gr")] =  filter(x -> x.exp != AffExpr(),cnsNoEq_df) |> (w -> cnsCont(select(w,intCol(w,:cnsExpr)),:greater))
					# set upper limit based on expansion plus residuals
					cnsNoEq_df[!,:cnsExpr] = @expression(anyM.optModel,cnsNoEq_df[!,:capa] .- cnsNoEq_df[!,:exp] - cnsNoEq_df[!,:resi])
					filter!(x -> !(isempty(x.cnsExpr.terms)) && (!decom_boo || x.exp != AffExpr()), cnsNoEq_df)
					cns_dic[Symbol("must",makeUp(capa[1]),"Sm")] = cnsCont(select(cnsNoEq_df,intCol(cnsNoEq_df,:cnsExpr)),:smaller)
				end
			end
		end

		#endregion
	end

	# add missing capacity variables in case of subproblems (otherwise infeasibility is possible, if storage cannot be charged sufficiently)
	if !isempty(anyM.subPro) && anyM.subPro != (0,0) && :costMissCapa in keys(anyM.parts.bal.par)
		stCapa_df = filter(x -> x.id != 0,part.par[:desFac].data)
		if !isempty(stCapa_df)
			stCapa_df = orderDf(select(matchSetParameter(select(stCapa_df,Not([:val])),anyM.parts.bal.par[:costMissCapa],anyM.sets),Not([:val])))
			part.var[:missCapa] = createVar(stCapa_df,"missCapa",NaN,anyM.optModel,anyM.lock,anyM.sets, scaFac = anyM.options.scaFac.insCapa)
		end	
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

