
#region # * prepare to create expansion, retrofitting and capacity variables


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
		
		# saves required dimensions to dictionary
		if exp == :Conv && !isempty(convCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(allMap_df,part.par,exp,tsYear_dic,anyM), resi = DataFrame())
		elseif exp != :Conv && !isempty(stCar_arr)
			prepTech_dic[Symbol(:exp,exp)] =  (var = addSupTsToExp(combine(groupby(allMap_df,namesSym(allMap_df)), :Te => (x -> stCar_arr) => :C),part.par,exp,tsYear_dic,anyM), resi = DataFrame())
		else
			continue
		end
		
	end
end

# ! dimensions for capacity variables
function prepareCapacity!(part::AbstractModelPart,prep_dic::Dict{Symbol,NamedTuple},exp_df::DataFrame,capaVar::Symbol,anyM::anyModel; sys::Int = 0)

	sym = capaVar == :capaExc ? :Exc : :Te

	# ! initialize assignments and data
	defPar_tup = tuple(keys(part.par)...)

	capaVar_df = expandExpToCapa(exp_df)

	# groups by expansion time steps in case of mature technologies
	if part.type == :mature
		select!(capaVar_df,Not(:Ts_expSup))
		capaVar_df = unique(capaVar_df)
		capaVar_df[!,:Ts_expSup] .= 0
	end

	# filters cases where capacity is fixed to zero
    varFix_sym = Symbol(capaVar,:Fix)

	if varFix_sym in defPar_tup
		capaVar_df = removeEntries([filterZero(capaVar_df,getLimPar(anyM.parts.lim,Symbol(capaVar,:Fix),anyM.sets[sym], sys = sys),anyM)],capaVar_df)
	end

	# for exchange capacities add column to indicate these values are symmetric
	if sym == :Exc
		capaVar_df[!,:dir] .= false
	end

	# create entry for capacity
	prep_dic[capaVar] =  (var = unique(orderDf(capaVar_df)), resi = DataFrame())
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

# ! add entries for retrofitting variables
function addRetrofitting!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}},anyM::anyModel)

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
		sysSym_arr = filter(x -> getfield(anyM.parts, sys == :Te ? :tech : :exc)[x].type != :stock, collect(keys(prepSys_dic[sys])))

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
			if isempty(allRetro_df) continue end

			# expand table with actual expansion time-steps
			supExp_dic = Dict(x => getAncestors(x,anyM.sets[:Ts],:int,(sys != :Exc ? part.balLvl.exp[1] : part.expLvl[1]))[end] for x in allRetro_df[!,:Ts_expSup])
			allRetro_df[!,:Ts_exp] = map(x -> supExp_dic[x], allRetro_df[!,:Ts_expSup])

			# group data analogously to entries for expansion
			expMap_df = combine(groupby(allRetro_df,filter(x -> x != :Ts_expSup, namesSym(allRetro_df))), :Ts_expSup => (x -> [x]) => :Ts_expSup)
			retro_df = orderDf(addSupTsToExp(expMap_df,part.par,makeUp(retroName_sym),tsYear_dic,anyM))

			# add entries for target technology as entry for retrofitting
			prepSys_dic[sys][sSym][retroName_sym] = (var = adjustRetroRegion(sys,retro_df), resi = DataFrame())

			# add entries for start technology
			for s in unique(retro_df[!,Symbol(sys,"_i")])
				start_sym = sysSym(s,anyM.sets[sys])
				start_df = adjustRetroRegion(sys,filter(x -> x[Symbol(sys,"_i")] == s, retro_df),false)
				prepSys_dic[sys][start_sym][retroName_sym] = (var = start_df, resi = DataFrame())

				# if the start technology is a stock technology being a start to retrofitting still makes it necessary to create a variable for installed capacity
				partStart = sys == :Te ? anyM.parts.tech[start_sym] : anyM.parts.exc[start_sym]
				
				if partStart.type == :stock
					# filter cases where retrofitting necessitates an explict variable for installed capacities 
					startCapa_df = select(flatten(start_df,:Ts_expSup),Not(vcat([:Ts_disSup,:Ts_exp],sys == :Te ? [:Te_j,:Ts_expSup_j,:R_exp_j] : [:Ts_expSup_j,:R_a_j,:R_b_j,:Exc_j])))
					# find matches with provided resiudal capacities
					join1_arr = vcat([:Ts_disSup, :Ts_expSup], sys == :Te ? [:R_exp,:Te] : [:R_from,:R_to,:Exc])
					join2_arr = vcat([:Ts_expSup], sys == :Te ? [:Ts_expSup_i, :R_exp_i, :Te_i] : [:Ts_expSup_i,:R_a_i,:R_b_i,:Exc_i])
					capa_df = innerjoin(select(prepSys_dic[sys][start_sym][capaSym].resi,Not([:var])), startCapa_df, on = join1_arr .=> join2_arr)
					# adds capacity entries to dictionary
					prepSys_dic[sys][start_sym][capaSym] = prepSys_dic[sys][start_sym][capaSym] |> (z -> (var = unique(vcat(z.var,capa_df)), resi = z.resi))
				end
			end
		end
	end
	
    return allCapaDf_dic
end

# ! ensures regions are on the level of the start or target system
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

# ! remove entries where expansion or capacity is fixed zero and no capacity can be created via retrofitting
function removeFixed!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}},allCapaDf_dic::Dict{Symbol,DataFrame},anyM::anyModel)

	for sys in (:Te,:Exc)
		sysSym_arr = filter(x -> getfield(anyM.parts, sys == :Te ? :tech : :exc)[x].type != :stock, collect(keys(prepSys_dic[sys])))

		for sSym in sysSym_arr

			# find entries of existing preparation dictionary where variables are already fixed to zero and remove them
			for prepSym in collect(keys(prepSys_dic[sys][sSym]))
				# get relevant parameter data
				limPar_obj = getLimPar(anyM.parts.lim,Symbol(prepSym,:Fix),anyM.sets[sys], sys = sysInt(sSym,anyM.sets[sys]))
				remainCapa_df = sys == :Te ? filterZero(prepSys_dic[sys][sSym][prepSym].var,limPar_obj,anyM) : convertExcCol(filterZero(convertExcCol(prepSys_dic[sys][sSym][prepSym].var),limPar_obj,anyM))

				prepSys_dic[sys][sSym][prepSym] = prepSys_dic[sys][sSym][prepSym] |> (x -> (var = removeEntries([remainCapa_df],x.var),resi = x.resi))
			end

			# filter enries of preparation dictionary where capacity variable cannot exist, because there is no corresponding expansion or retrofitting variable 
			for capaSym in filter(x -> occursin("capa",string(x)), intersect(collect(keys(allCapaDf_dic)),collect(keys(prepSys_dic[sys][sSym]))))

				# get and expand related entries for expansion
				potCapa_df = expandExpToCapa(prepSys_dic[sys][sSym][Symbol(replace(string(capaSym),"capa" => "exp"))].var)
				
				# get and expand related entries for retrofitting
				retro_sym = Symbol(replace(string(capaSym),"capa" => "retro"))
				if retro_sym in keys(prepSys_dic[sys][sSym])
					if sys == :Te
						retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var,Not([:Ts_exp,:Ts_expSup_i, :R_exp_i, :Te_i])),:Te_j => :Te, :R_exp_j => :R_exp, :Ts_expSup_j => :Ts_exp)
					else
						retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var,Not([:Ts_exp,:Ts_expSup_i, :R_a_i, :R_b_i, :Exc_i])),:Exc_j => :Exc, :R_a_j => :R_from, :R_b_j => :R_to, :Ts_expSup_j => :Ts_exp)
					end
					potCapa_df = unique(vcat(potCapa_df,expandExpToCapa(retro_df)))
				end

				# groups by expansion time steps in case of mature technologies
				if getfield(anyM.parts, sys == :Te ? :tech : :exc)[sSym].type == :mature
					potCapa_df[!,:Ts_expSup] .= 0
					potCapa_df = unique(potCapa_df)
				end

				# only preserve capacity entries that could be created based on expansion and retrofitting
				prepSys_dic[sys][sSym][capaSym] = prepSys_dic[sys][sSym][capaSym] |> (x -> (var = innerjoin(x.var, potCapa_df, on = intCol(x.var)), resi = x.resi))
			end
		end
	end
end

# ! add entries for installed capacity
function addInsCapa!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}},anyM::anyModel)

	# add installed capacity for exchange
	for excSym in collect(keys(prepSys_dic[:Exc]))
		prepExc_dic = prepSys_dic[:Exc][excSym]
	
		if anyM.parts.exc[excSym].decomm != :none && :capaExc in keys(prepExc_dic)
			prepExc_dic[:insCapaExc] =  (var = prepExc_dic[:capaExc].var, resi = prepExc_dic[:capaExc].resi)
			excResi_df = select(prepExc_dic[:capaExc].resi,Not([:var,:Ts_expSup]))
			prepExc_dic[:capaExc] =  (var = unique(vcat(prepExc_dic[:capaExc].var, filter(x -> x.R_from < x.R_to, vcat(excResi_df,rename(excResi_df,:R_from => :R_to,:R_to => :R_from))))), resi = DataFrame())
		end
	end

	# add installed capacity for technologies
	for tSym in collect(keys(prepSys_dic[:Te]))
		prepTech_dic = prepSys_dic[:Te][tSym]
		if anyM.parts.tech[tSym].decomm != :none
			for capTy in intersect(keys(prepTech_dic),(:capaConv,:capaStIn,:capaStOut,:capaStSize,:capaExc))
				prepTech_dic[Symbol(:ins,makeUp(capTy))] =  (var = prepTech_dic[capTy].var, resi = prepTech_dic[capTy].resi)
				prepTech_dic[capTy] =  (var = anyM.parts.tech[tSym].type != :stock ? prepTech_dic[capTy].var :  select(prepTech_dic[capTy].resi,Not([:var])), resi = DataFrame())
			end
		end
	end
end

#endregion


