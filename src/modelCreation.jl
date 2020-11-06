
# ! create optimization model after anyModel has been initialized
"""
```julia
createOptModel!(model_object::anyModel)
```

Create all elements of the model's underlying optimization problem except for the objective function.
"""
function createOptModel!(anyM::anyModel)

	#region # * create technology related variables and constraints
	parDef_dic = defineParameter(anyM.options,anyM.report)

    # ! gets dictionary with dimensions of expansion and capacity variables
    tsYear_dic = Dict(zip(anyM.supTs.step,collect(0:anyM.options.shortExp:(length(anyM.supTs.step)-1)*anyM.options.shortExp)))
    prepSys_dic = Dict(sys => Dict{Symbol,Dict{Symbol,NamedTuple}}() for sys in (:Te,:Exc))
	prepareTechs!(collect(keys(anyM.parts.tech)),prepSys_dic[:Te],tsYear_dic,anyM)
	prepareExc!(collect(keys(anyM.parts.exc)),prepSys_dic[:Exc],tsYear_dic,anyM)

	# TODO genau hier weiter machen

	# ! remove unrequired elements in case of distributed model creation
	if !isempty(anyM.subPro)
	    distributedMapping!(anyM,prepTech_dic,prepExc_dic)
	end
	# abort if there is already an error
    if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

    # remove technologies without any potential capacity variables
    techSym_arr = collect(keys(prepTech_dic))
    foreach(x -> delete!(anyM.parts.tech, x),setdiff(collect(keys(anyM.parts.tech)),techSym_arr))

    # ! create all technology related elements

    # creates dictionary that assigns combination of superordinate dispatch timestep and dispatch level to dispatch timesteps
    allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo),:tsDis))
    ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1],anyM.sets[:Ts],false,x[2]) for x in Iterators.product(anyM.supTs.step,allLvlTsDis_arr))

    # creates dictionary that assigns combination of expansion region and dispatch level to dispatch region
    allLvlR_arr = union(getindex.(getfield.(getfield.(values(anyM.parts.tech),:balLvl),:exp),2),map(x -> x.rDis,values(anyM.cInfo)))

    allRExp_arr = union([getfield.(getNodesLvl(anyM.sets[:R],x),:idx) for x in allLvlR_arr]...)
    r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl <= x[2] ? getDescendants(x[1], anyM.sets[:R],false,x[2]) : getAncestors(x[1],anyM.sets[:R],:int,x[2])[end]) |> (z -> typeof(z) <: Array ? z : [z]) for x in Iterators.product(allRExp_arr,allLvlR_arr))

    produceMessage(anyM.options,anyM.report, 3," - Determined dimension of expansion and capacity variables for technologies")

    # constraints for technologies are prepared in threaded loop and stored in an array of dictionaries
	techCnsDic_arr = Array{Dict{Symbol,cnsCont}}(undef,length(techSym_arr))
	tech_itr = collect(enumerate(techSym_arr))

	@threads for (idx,tSym) in tech_itr
		techCnsDic_arr[idx] = createTech!(sysInt(tSym,anyM.sets[:Te]),anyM.parts.tech[tSym],prepTech_dic[tSym],copy(parDef_dic),ts_dic,r_dic,anyM)
	end

    # loops over array of dictionary with constraint container for each technology to create actual jump constraints
    for (idx,cnsDic) in enumerate(techCnsDic_arr), cnsSym in keys(cnsDic)
        anyM.parts.tech[techSym_arr[idx]].cns[cnsSym] = createCns(cnsDic[cnsSym],anyM.optModel)
    end
    produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for all technologies")

	#endregion

	#region # * create exchange related variables and constraints


	if !all(map(x -> isempty(x),values(prepExc_dic[:capaExc])))
		partExc = anyM.parts.exc
		# create expansion and capacity variables
		createExpCap!(partExc,prepExc_dic,anyM)
		# create capacity constraint
		if isempty(anyM.subPro) || anyM.subPro == (0,0)
			createCapaExcCns!(partExc,anyM)
		end
		produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints related to expansion and capacity for exchange")
		# create dispatch related exchange elements
		if isempty(anyM.subPro) || anyM.subPro != (0,0)
			# create dispatch variables
			createExcVar!(partExc,ts_dic,anyM)
			produceMessage(anyM.options,anyM.report, 2," - Created all dispatch variables for exchange")
			# create capacity restrictions
			createRestrExc!(ts_dic,partExc,anyM)
			produceMessage(anyM.options,anyM.report, 2," - Created all capacity restrictions for exchange")
			produceMessage(anyM.options,anyM.report, 1," - Created variables and constraints for exchange")
		end
	end

	#endregion

	if isempty(anyM.subPro) || anyM.subPro != (0,0)
		createTradeVarCns!(anyM.parts.trd,ts_dic,anyM)
		createEnergyBal!(techSym_arr,ts_dic,anyM)
	end
	createLimitCns!(anyM.parts.lim,anyM)

	produceMessage(anyM.options,anyM.report, 1," - Completed model creation")
end



# ensures regions are on the level of the start or target system
function adjustRetroRegion(sys::Symbol,retro_df::DataFrame,start::Bool=true)
	
	# check if start or target region should be overwritten
	if start 
		keep_sym = :i; drop_sym = :j
	else
		keep_sym = :j; drop_sym = :i
	end
	# adjusts respective regions
	if sys != :Exc
		retro_df[!,Symbol(:R_exp_,drop_sym)] = retro_df[!,Symbol(:R_exp_,keep_sym)] 
	else
		retro_df[!,Symbol(:R_a_,drop_sym)] = retro_df[!,Symbol(:R_a_,keep_sym)]; 
		retro_df[!,Symbol(:R_b_,drop_sym)] = retro_df[!,Symbol(:R_b_,keep_sym)]
	end
	return unique(retro_df)
end

#=
# ! gather all existing capacity entries that could be relevant for retrofitting
allCapaDf_dic = Dict{Symbol,DataFrame}()
retroPotSym_arr = Symbol.(replace.(string.(filter(x -> occursin("costRetro",string(x)), collect(keys(anyM.parts.obj.par)))),"costRetro" => ""))

for retroSym in intersect(retroPotSym_arr,(:Conv,:StIn,:StOut,:StSize))
	capaSym = Symbol(:capa,retroSym)
	allCapaDf_dic[capaSym] = unique(vcat(filter(w -> !isempty(w), vcat(map(x -> capaSym in keys(x) ? map(y -> getfield(x[capaSym],y) |> (z -> select(z,intCol(z))),[:var,:resi]) : DataFrame[],values(prepSys_dic[:Te]))...))...))
end

if :Exc in retroPotSym_arr
	allCapaDf_dic[:capaExc] = filter(x -> x.R_from < x.R_to,unique(vcat(filter(w -> !isempty(w), vcat(map(x -> :capaExc in keys(x) ? map(y -> getfield(x[:capaExc],y) |> (z -> select(z,intCol(z))),[:var,:resi]) : DataFrame[],values(prepSys_dic[:Exc]))...))...)))
end


# ! create actual entries for retrofitting by matching existing capacities with case where costs data was defined
for sys in (:Te, :Exc)
	sysSym_arr = collect(keys(prepSys_dic[sys]))

	for sSym in sysSym_arr, capaSym in filter(x -> occursin("capa",string(x)), intersect(collect(keys(allCapaDf_dic)),collect(keys(prepSys_dic[sys][sSym]))))

		part = sys == :Te ? anyM.parts.tech[sSym] : anyM.parts.exc[sSym]
		retroName_sym = Symbol(replace(string(capaSym),"capa" => "retro"))

		# ! creata dataframe for potential retrofits by filtering for target system
		# filter capacities for relevant system from all system capacities
		relSys_df = filter(x -> x[sys] == sysInt(sSym,anyM.sets[sys]),allCapaDf_dic[capaSym])
		
		# rename columns so they refer to target system, joins with all other capacity entries as starting points and renames them as well
		if capaSym == :capaConv
			relSys_df  = innerjoin(rename(relSys_df, :Ts_expSup => :Ts_expSup_j, sys => Symbol(sys,"_j"), :R_exp => :R_exp_j),allCapaDf_dic[capaSym],on = [:Ts_disSup])
			rename!(relSys_df,:Ts_expSup => :Ts_expSup_i, sys => Symbol(sys,"_i"), :R_exp => :R_exp_i, :Ts_disSup => :Ts_expSup)
		elseif capaSym in (:capaStIn, :capaStOut, :capaStSize)
			relSys_df  = innerjoin(rename(relSys_df, :Ts_expSup => :Ts_expSup_j, sys => Symbol(sys,"_j"), :R_exp => :R_exp_j, :C => :C_j),allCapaDf_dic[capaSym],on = [:Ts_disSup])
			rename!(relSys_df,:Ts_expSup => :Ts_expSup_i, sys => Symbol(sys,"_i"), :R_exp => :R_exp_i, :C => :C_i, :Ts_disSup => :Ts_expSup)
		else
			relSys_df = innerjoin(rename(relSys_df, :Ts_expSup => :Ts_expSup_j, sys => Symbol(sys,"_j"), :R_from => :R_a_j, :R_to => :R_b_j),allCapaDf_dic[capaSym],on = [:Ts_disSup])
			rename!(relSys_df,:Ts_expSup => :Ts_expSup_i, sys => Symbol(sys,"_i"), :R_from => :R_a_i, :R_to => :R_b_i, :Ts_disSup => :Ts_expSup)
		end

		# filter all rows where regions are not related
		relR_dic = Dict(x =>  vcat([x],getAncestors(x,anyM.sets[:R],:int)...,getDescendants(x,anyM.sets[:R])...) for x in (sys != :Exc ? unique(relSys_df[!,:R_exp_j]) : unique(vcat(map(z -> relSys_df[!,z],[:R_a_i,:R_a_j,:R_b_i,:R_b_j])...))))
		filter!(x -> capaSym != :capaExc ? x.R_exp_i in relR_dic[x.R_exp_j] : (x.R_a_i in relR_dic[x.R_a_j] && x.R_b_i in relR_dic[x.R_b_j]),relSys_df)

		# ! match with cost data to see where actual retrofitting is possible
		allRetro_df = select(orderDf(matchSetParameter(relSys_df,anyM.parts.obj.par[Symbol(:costRetro,replace(String(capaSym),"capa" => ""))],anyM.sets)),Not([:val]))

		# expand table with actual expansion time-steps
		supExp_dic = Dict(x => getAncestors(x,anyM.sets[:Ts],:int,(sys != :Exc ? part.balLvl.exp[1] : part.expLvl[1]))[end] for x in allRetro_df[!,:Ts_expSup])
		allRetro_df[!,:Ts_exp] = map(x -> supExp_dic[x], allRetro_df[!,:Ts_expSup])

		# group data analogously to entries for expansion
		expMap_df = combine(groupby(allRetro_df,filter(x -> x != :Ts_expSup, namesSym(allRetro_df))), :Ts_expSup => (x -> [x]) => :Ts_expSup)
		retro_df = orderDf(addSupTsToExp(expMap_df,part.par,makeUp(retroName_sym),tsYear_dic,anyM))

		# add entries for target technology as entry for retrofitting
		prepSys_dic[sys][sSym][retroName_sym] = (var = adjustRetroRegion(sys,retro_df), resi = DataFrame())

		# add entries for start technology
		for t in unique(retro_df[!,Symbol(sys,"_i")])
			prepSys_dic[sys][sysSym(t,anyM.sets[sys])][retroName_sym] = (var = adjustRetroRegion(sys,filter(x -> x[Symbol(sys,"_i")] == t, retro_df),false), resi = DataFrame())
		end
	end
end




# TODO Umrüstung hinzu tech verhindenr dass stock ist (capa eintrag würde ja existieren)
# TODO move alles removeEntries aus loops und schiebe hinter den teil zum retrofitting (d.h. expansion wo 0 entfernen, resultierende capacities entfernen, capacity wo 0 entfernen)
# filters cases where capacity is fixed to zero
varFix_sym = Symbol(capaVar,:Fix)

if varFix_sym in defPar_tup
	capaVar_df = removeEntries([filterZero(capaVar_df,getLimPar(anyM.parts.lim,Symbol(capaVar,:Fix),anyM.sets[sym], sys = sys),anyM)],capaVar_df)
end

=#