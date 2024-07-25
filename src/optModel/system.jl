
#region # * prepare to create expansion, retrofitting and capacity variables

# ! dimensions for capacity variables
function prepareCapacity!(part::AbstractModelPart, prep_dic::Dict{Symbol,NamedTuple}, exp_df::DataFrame, capaVar::Symbol, anyM::anyModel; sys::Int = 0)

	sym = capaVar == :capaExc ? :Exc : :Te

	# ! initialize assignments and data

	# for exchange capacities remove id column from expansion variables
	if sym == :Exc
		exp_df = unique(select(exp_df, Not([:id])))
	end

	capaVar_df = expandExpToCapa(exp_df)

	# groups by expansion time steps in case of mature technologies
	if part.type == :mature
		select!(capaVar_df, Not(:Ts_expSup))
		capaVar_df = unique(capaVar_df)
		capaVar_df[!,:Ts_expSup] .= 0
	end

	# for exchange capacities add column to indicate these values are symmetric
	if sym == :Exc
		capaVar_df[!,:dir] .= part.dir
	end

	# create entry for capacity
	prep_dic[capaVar] =  (var = unique(orderDf(capaVar_df)), resi = DataFrame(var = AffExpr[]))
end

# ! add entries for retrofitting variables
function addRetrofitting!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}}, anyM::anyModel)

	# ! gather all existing capacity entries that could be relevant for retrofitting
	allCapaDf_dic = Dict{Symbol,DataFrame}()
	retroPotSym_arr = Symbol.(replace.(string.(filter(x -> occursin("costRetro", string(x)), collect(keys(anyM.parts.cost.par)))), "costRetro" => ""))

	# get systems which are irrelevant because they are have no capacity variables
	unResTe_arr, unResExc_arr = [filter(x -> getfield(anyM.parts, z)[x].type == :unrestricted, collect(keys(getfield(anyM.parts, z)))) for z in (:tech, :exc)]

	# prefilter systems that can be relevant for retrofitting technologies
	filtSys_dic = Dict{Symbol,Array{Symbol,1}}()
	
	for capa in retroPotSym_arr
		sys_sym = capa != :Exc ? :Te : :Exc
		parData_df = anyM.parts.cost.par[Symbol(:costRetro, capa)].data
		sysInt_arr = map(y -> filter(w -> isempty(anyM.sets[sys_sym].nodes[w].down), getDescendants(y, anyM.sets[sys_sym], true)), intersect([Symbol(sys_sym, :_i), Symbol(sys_sym, :_j)], namesSym(parData_df)) |> (z -> isempty(z) ? [0] : union([parData_df[!,x] for x in z]...)))
		filtSys_dic[capa] = map(x -> sysSym(x, anyM.sets[sys_sym]), union(sysInt_arr...))
	end

	for retroSym in intersect(retroPotSym_arr, (:Conv, :StIn, :StOut, :StSize))
		capaSym = Symbol(:capa, retroSym)
		relCapa_dic = filter(x -> x[1] in filtSys_dic[retroSym] && !(x[1] in unResTe_arr), prepSys_dic[:Te])
		allCapaDf_dic[capaSym] = unique(vcat(filter(w -> !isempty(w), vcat(map(x -> capaSym in keys(x) ? map(y -> getfield(x[capaSym], y) |> (z -> select(z, intCol(z))), [:var, :resi]) : DataFrame[], values(relCapa_dic))...))...))
	end
	
	if :Exc in retroPotSym_arr
		relCapa_dic = filter(x -> x[1] in filtSys_dic[:Exc]&& !(x[1] in unResExc_arr), prepSys_dic[:Exc])
		capaExc_df = unique(vcat(filter(w -> !isempty(w), vcat(map(x -> :capaExc in keys(x) ? map(y -> getfield(x[:capaExc], y) |> (z -> select(z, intCol(z))), [:var, :resi]) : DataFrame[], values(relCapa_dic))...))...))
		allCapaDf_dic[:capaExc] = filter(x -> anyM.parts.exc[sysSym(x.Exc, anyM.sets[:Exc])].dir || (x.R_from < x.R_to), capaExc_df)
	end

	# ! create actual entries for retrofitting by matching existing capacities with case where costs data was defined
	for sys in (:Te, :Exc)
		sysSym_arr = collect(keys(prepSys_dic[sys]))

		for sSym in sysSym_arr, capaSym in filter(x -> occursin("capa", string(x)), intersect(collect(keys(allCapaDf_dic)), collect(keys(prepSys_dic[sys][sSym]))))
			
			part = sys == :Te ? anyM.parts.tech[sSym] : anyM.parts.exc[sSym]
			retroName_sym = Symbol(replace(string(capaSym), "capa" => "retro"))
			type_sym = Symbol(replace(string(capaSym), "capa" => ""))

			# ! creata dataframe for potential retrofits by filtering for start system
			# filter capacities for relevant system from all system capacities
			relSys_df = filter(x -> x[sys] == sysInt(sSym, anyM.sets[sys]), allCapaDf_dic[capaSym])
			
			# rename columns so they refer to target system, joins with all other capacity entries as starting points and renames them as well
			startCol_arr = capaSym == :capaConv ? [:Te, :Ts_expSup, :R_exp] : (capaSym in (:capaStIn, :capaStOut, :capaStSize) ? [:Te, :Ts_expSup, :R_exp, :id] : [:Exc, :Ts_expSup, :R_from, :R_to])
			relSys_df  = innerjoin(rename(relSys_df, startCol_arr .=> Symbol.(startCol_arr, "_i")), allCapaDf_dic[capaSym], on = [:Ts_disSup])
			rename!(relSys_df, vcat(vcat(startCol_arr, [:Ts_disSup]) .=> vcat(Symbol.(startCol_arr, "_j"), [:Ts_retro])))

			# filter all rows where regions are not related
			relR_dic = Dict(x =>  vcat([x], getAncestors(x, anyM.sets[:R], :int)..., getDescendants(x, anyM.sets[:R])...) for x in (sys != :Exc ? unique(relSys_df[!,:R_exp_i]) : unique(vcat(map(z -> relSys_df[!,z], [:R_from_j, :R_from_i, :R_to_j, :R_to_i])...))))
			filter!(x -> capaSym != :capaExc ? x.R_exp_j in relR_dic[x.R_exp_i] : (x.R_from_j in relR_dic[x.R_from_i] && x.R_to_j in relR_dic[x.R_to_i]), relSys_df)

			# ! match with cost data to see where actual retrofitting is possible
			allRetro_df = filter(x -> x[Symbol(sys, :_i)] != x[Symbol(sys, :_j)], select(orderDf(matchSetParameter(relSys_df, anyM.parts.cost.par[Symbol(:costRetro, replace(String(capaSym), "capa" => ""))], anyM.sets)), Not([:val])))
			if isempty(allRetro_df) continue end

			# ! add column for last superordinate timestep of operation
			# join lifetime of starting technology
			if capaSym != :capaExc
				allRetro_df[!,:lifeStart] = matchSetParameter(rename(allRetro_df, Symbol.(startCol_arr, "_i") .=> startCol_arr), part.par[Symbol(:life, type_sym)], anyM.sets)[!,:val]
			else
				allRetro_df[!,:lifeStart] = matchExcParameter(Symbol(:life, type_sym), rename(allRetro_df, Symbol.(startCol_arr, "_i") .=> startCol_arr), part, anyM.sets, part.dir)[!,:val]
			end

			# add column for last superordinate dispatch timesteps capacity would be operating, filters cases where last superordinate dispatch timesteps of operating would be so far in the future, that combination is impossible due to the lifetime
			allRetro_df[!,:Ts_disSup_last] = map(x -> filter(y -> y >= x.Ts_retro && (y-x.Ts_retro) * anyM.options.shortExp <= x.lifeStart, collect(anyM.supTs.step)), eachrow(allRetro_df))
			allRetro_df = flatten(allRetro_df, :Ts_disSup_last)
			if isempty(allRetro_df) continue end
			
			# ! compute all timesteps in lifetime of target capacity
			# get credit factor for lifetime of retrofitted capacity and new lifetime from retrofitting to get the lifetime of new unit
			allRetro_df = matchSetParameter(allRetro_df, part.par[Symbol(:creditRetro, type_sym)], anyM.sets, newCol = :credit)
			allRetro_df = matchSetParameter(allRetro_df, part.par[Symbol(:lifeRetro, type_sym)], anyM.sets, newCol = :lifeRetroSys)
			allRetro_df[!,:lifeRetro] = map(x -> ((x.Ts_disSup_last-x.Ts_retro) * anyM.options.shortExp + x.lifeStart % anyM.options.shortExp) * x.credit + x.lifeRetroSys, eachrow(allRetro_df))

			# compute array of superordinate timesteps in lifetime
			allRetro_df[!,:Ts_disSup] = map(x -> filter(z -> z >= x.Ts_retro && z < x.Ts_retro + x.lifeRetro/anyM.options.shortExp, collect(anyM.supTs.step)), eachrow(allRetro_df))
			allRetro_df = orderDf(select!(allRetro_df, Not([:lifeStart, :credit, :lifeRetroSys, :lifeRetro])))

			# add entries for start technology for retrofitting
			allRetroStart_df = copy(allRetro_df)
			addToRetro!(prepSys_dic[sys][sSym], allRetroStart_df, retroName_sym)

			# ! adjusts capacities for start

			# if the start technology is a stock technology being a start to retrofitting still makes it necessary to create a variable for installed capacity
			if part.type == :stock
				# filter cases where retrofitting necessitates an explict variable for installed capacities 
				select_arr = sys == :Te ? capaSym != :capaConv ? [:Te_j, :Ts_expSup_j, :R_exp_j, :id_j] : [:Te_j, :Ts_expSup_j, :R_exp_j] : [:Ts_expSup_j, :R_from_j, :R_to_j, :Exc_j]
				startCapa_df = unique(select(flatten(allRetroStart_df, :Ts_disSup), Not(vcat([:Ts_retro, :Ts_disSup_last], select_arr))))
				# find matches with provided resiudal capacities
				join1_arr = vcat([:Ts_disSup, :Ts_expSup], sys == :Te ? (capaSym != :capaConv ? [:R_exp, :Te, :id] : [:R_exp, :Te]) : [:R_from, :R_to, :Exc])
				join2_arr = vcat([:Ts_disSup], sys == :Te ? (capaSym != :capaConv ? [:Ts_expSup_i, :R_exp_i, :Te_i, :id_i] : [:Ts_expSup_i, :R_exp_i, :Te_i]) : [:Ts_expSup_i, :R_from_i, :R_to_i, :Exc_i])
				capa_df = innerjoin(select(prepSys_dic[sys][sSym][capaSym].resi, Not([:var])), startCapa_df, on = join1_arr .=> join2_arr)
				# adds capacity entries to dictionary
				prepSys_dic[sys][sSym][capaSym] = prepSys_dic[sys][sSym][capaSym] |> (z -> (var = orderDf(unique(vcat(z.var, capa_df))), resi = orderDf(z.resi)))
			end

			# filters cases where capacity is affected by retrofitting
			joinOn_arr = sys == :Te ? capaSym != :capaConv ? [:Ts_expSup_i, :Ts_disSup, :R_exp_i, :Te_i, :id_i] : [:Ts_expSup_i, :Ts_disSup, :R_exp_i, :Te_i] : [:Ts_expSup_i, :Ts_disSup, :R_from_i, :R_to_i, :Exc_i]
			
			capaGrp_df = (part.type == :stock ? select(prepSys_dic[sys][sSym][capaSym].resi, Not([:var])) : prepSys_dic[sys][sSym][capaSym].var) |> (z -> semijoin(z, flatten(allRetroStart_df, :Ts_disSup), on = intCol(z) .=> joinOn_arr))
			
			# ! create new entries for start grouped by operating year
			# add column for last superordinate dispatch timesteps capacity would be operating, analogously to above 	
			capaGrp_df[!,:lifeStart] = matchSetParameter(capaGrp_df, part.par[Symbol(:life, type_sym)], anyM.sets)[!,:val]
			capaGrp_df[!,:Ts_disSup_last] = map(x -> filter(y -> y >= x.Ts_disSup && (y-x.Ts_disSup) * anyM.options.shortExp <= x.lifeStart, collect(anyM.supTs.step)), eachrow(capaGrp_df))
			capaGrp_df = flatten(capaGrp_df, :Ts_disSup_last)
			select!(capaGrp_df, Not([:lifeStart]))

			# ! compute new residual capacities for start grouped by laster operating year
			# add column for last superordinate dispatch timesteps capacity would be operating, analogously to above 	
			allGrp_arr = DataFrame[]
			if !isempty(prepSys_dic[sys][sSym][capaSym].resi)
				resi_df = semijoin(prepSys_dic[sys][sSym][capaSym].resi, sys == :Exc && part.dir ? flipExc(capaGrp_df) : capaGrp_df, on = intCol(prepSys_dic[sys][sSym][capaSym].resi))
				resi_df[!,:Ts_disSup_last] = map(x -> filter(y -> y >= x, collect(anyM.supTs.step)), resi_df[!,:Ts_disSup])
				# groups by region and year of expansion
				resi_df = flatten(resi_df, :Ts_disSup_last)
				resiGrp_df = groupby(resi_df, capaSym == :capaConv ? [:Ts_expSup, :R_exp] : (capaSym == :capaExc ? [:Ts_expSup, :R_from, :R_to] : [:Ts_expSup, :R_exp, :id]))
				# loops over element of group to adjust residual values according to last year of operation
				for x in resiGrp_df
					grp_df = DataFrame(x)
					grp_df[!,:newResi] .= 0.0 # adds columns for new residual capacity based on last year of operation
					sortDisSup_arr = sort(unique(grp_df[!,:Ts_disSup]), rev=true)

					for a in sortDisSup_arr
						# computes value for lattest year of operation in superordinate timestep a
						newResi_fl = filter(x -> x.Ts_disSup == a && x.Ts_disSup_last == a, grp_df)[1,:var].constant - sum(filter(x -> x.Ts_disSup == a, grp_df)[:,:newResi])
						# adds value for same lattest year, but other superordinate timesteps, if possible
						grp_df[!,:newResi] = map(x -> x.Ts_disSup_last == a ?  min(newResi_fl, x.var.constant) : x.newResi, eachrow(grp_df))
					end
					grp_df[!,:var] = map(x -> AffExpr(x), grp_df[!,:newResi])
					push!(allGrp_arr, select(grp_df, Not([:newResi])))
				end

				# remove residual from non-grouped capacity variable
				prepSys_dic[sys][sSym][capaSym] = (var = prepSys_dic[sys][sSym][capaSym].var, resi = antijoin(prepSys_dic[sys][sSym][capaSym].resi, resi_df, on = intCol(prepSys_dic[sys][sSym][capaSym].resi)))
			end
			
			# write entries for grouped capacity variables
			resi_df = isempty(allGrp_arr) ? prepSys_dic[sys][sSym][capaSym].resi : orderDf(vcat(allGrp_arr...))
			prepSys_dic[sys][sSym][Symbol(:grp, makeUp(capaSym))] = (var = orderDf(capaGrp_df), resi = orderDf(resi_df))

			# ! replace entries for target technology
			for s in unique(allRetro_df[!, Symbol(sys, "_j")])
				# obtains new retrofitting getAllVariables
				newRetro_df = orderDf(filter(x -> x[Symbol(sys, "_j")] == s, allRetro_df))
				# merges with existing retrofitting variables if any exist
				addToRetro!(prepSys_dic[sys][sysSym(s, anyM.sets[sys])], newRetro_df, Symbol(:retro, type_sym))
			end
		end
	end
	
    return allCapaDf_dic
end

# ! remove entries where expansion or capacity is fixed zero and no capacity can be created via retrofitting
function removeFixed!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}}, allCapaDf_dic::Dict{Symbol,DataFrame}, anyM::anyModel)

	fixPar_dic = Dict(:expStSize => (:sizeToStOutFix => :expStOut, :sizeToStOutFix => :expStIn), :expStIn => (:stInToConvFix => :expConv, :stOutToStInFix => :expStOut), :expStOut => (:stOutToStInFix => :expStIn, :sizeToStOutFix => :expStSize), 
										:capaStSize => (:sizeToStOutFix => :capaStOut, :sizeToStOutFix => :capaStIn), :capaStIn => (:stInToConvFix => :capaConv, :stOutToStInFix => :capaStOut), :capaStOut => (:stOutToStInFix => :capaStIn, :sizeToStOutFix => :capaStSize))

	for sys in (:Te, :Exc)
		sysSym_arr = filter(x -> getfield(anyM.parts, sys == :Te ? :tech : :exc)[x].type in (:stock, :mature, :emerging), collect(keys(prepSys_dic[sys])))

		for sSym in sysSym_arr

			sys_int = sysInt(sSym, anyM.sets[sys])
			part_obj = getfield(anyM.parts, sys == :Te ? :tech : :exc)[sSym]
			
			# ! find entries where variables are already fixed to zero and remove them
			for prepSym in collect(keys(prepSys_dic[sys][sSym]))
				# get relevant parameter data
				limPar_obj = getLimPar(anyM.parts.lim, Symbol(prepSym, :Fix), anyM.sets[sys], sys = sysInt(sSym, anyM.sets[sys]))
				# get all cases where variables are fixed
				fixLim_df = getFix(prepSys_dic[sys][sSym][prepSym].var, limPar_obj, anyM)
				# removes cases where variables are fixed to zero 
				if !isempty(fixLim_df) && sys == :Te
					remainCapa_df = select(filter(r -> r.val == 0, fixLim_df), Not(:val))
					prepSys_dic[sys][sSym][prepSym] = prepSys_dic[sys][sSym][prepSym] |> (x -> (var = removeEntries([remainCapa_df], x.var), resi = x.resi))				
					
					# if capacity has a fixed ratio to another type of capacites, remove entries for that variable as well where it is zero
					if prepSym in keys(fixPar_dic) 	
						for par1 in filter(x -> x[1] in keys(anyM.parts.tech[sSym].par), fixPar_dic[prepSym])
							# get cases where ratio is fixed
							matchPar1_df = matchSetParameter(select(remainCapa_df, intCol(remainCapa_df)), anyM.parts.tech[sSym].par[par1[1]], anyM.sets)
							# remove entries for corresponding variable
							if !isempty(matchPar1_df)
								prepSys_dic[sys][sSym][par1[2]] = prepSys_dic[sys][sSym][par1[2]] |> (x -> (var = removeEntries([select(matchPar1_df, intersect(intCol(matchPar1_df), intCol(x.var)))], x.var), resi = x.resi))
								# recursively checking other variables
								for par2 in filter(x -> x[1] in keys(anyM.parts.tech[sSym].par) && x[2] != prepSym, fixPar_dic[par1[2]])
									# get cases where ratio is fixed
									matchPar2_df = matchSetParameter(select(matchPar1_df, intCol(matchPar1_df)), anyM.parts.tech[sSym].par[par2[1]], anyM.sets)
									if !isempty(matchPar2_df)
										prepSys_dic[sys][sSym][par2[2]] = prepSys_dic[sys][sSym][par2[2]] |> (x -> (var = removeEntries([select(matchPar2_df, intersect(intCol(matchPar2_df), intCol(x.var)))], x.var), resi = x.resi))
									end
								end
							end

						end
					end
					
					# removes respective counterparts for capacity and installed capacity 
					if occursin("ins", string(prepSym))
						capa_sym = Symbol(makeLow(replace(string(prepSym), "ins" => "")))
						if capa_sym in keys(prepSys_dic[sys][sSym])
							prepSys_dic[sys][sSym][capa_sym] = prepSys_dic[sys][sSym][capa_sym] |> (x -> (var = removeEntries([remainCapa_df], x.var), resi = x.resi))
						end
					elseif occursin("capa", string(prepSym))
						if  Symbol(:ins, makeUp(prepSym)) in keys(prepSys_dic[sys][sSym])
							select!(fixLim_df, Not(:val))
							prepSys_dic[sys][sSym][Symbol(:ins, makeUp(prepSym))] = prepSys_dic[sys][sSym][Symbol(:ins, makeUp(prepSym))] |> (x -> (var = removeEntries([fixLim_df], x.var), resi = removeEntries([fixLim_df], x.resi)))
						end
					end
				end
			end	

			if part_obj.type != :stock
				# ! filter entries where capacity variable cannot exist, because there is no corresponding expansion or retrofitting variable
				for capaSym in filter(x -> occursin("capa", string(x)), collect(keys(prepSys_dic[sys][sSym])))

					# get and expand related entries for expansion
					potCapa_df = expandExpToCapa(prepSys_dic[sys][sSym][Symbol(replace(string(capaSym), "capa" => "exp"))].var)
					
					# removes id columns for expansion (different "projects") and ensures potential capacities in both directions are created for undirected case
					if capaSym == :capaExc	
						potCapa_df = select(potCapa_df, Not([:id]))
						if !anyM.parts.exc[sSym].dir
							dirOther_df = copy(potCapa_df)
							dirOther_df[!,:dir]  = map(x -> x ? 0 : 1, potCapa_df[!,:dir])
							potCapa_df = unique(vcat(dirOther_df, potCapa_df))
						end
					end

					# get and expand related entries for retrofitting
					retro_sym = Symbol(replace(string(capaSym), "capa" => "retro"))
					if retro_sym in keys(prepSys_dic[sys][sSym])
						if sys == :Te && capaSym == :capaConv
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_disSup_last, :Ts_expSup_i, :R_exp_i, :Te_i])), :Te_j => :Te, :R_exp_j => :R_exp, :Ts_expSup_j => :Ts_expSup)
						elseif sys == :Te
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_disSup_last, :Ts_expSup_i, :R_exp_i, :Te_i, :id_i])), :Te_j => :Te, :R_exp_j => :R_exp, :Ts_expSup_j => :Ts_expSup, :id_j => :id)
						else
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_disSup_last, :Ts_expSup_i, :R_from_i, :R_to_i, :Exc_i])), :Exc_j => :Exc, :R_from_j => :R_from, :R_to_j => :R_to, :Ts_expSup_j => :Ts_expSup)
						end
						if !isempty(retro_df) potCapa_df = unique(vcat(potCapa_df, flatten(retro_df, :Ts_disSup))) end
					end

					# groups by expansion time steps in case of mature technologies
					if part_obj.type == :mature
						potCapa_df[!,:Ts_expSup] .= 0
						potCapa_df = unique(potCapa_df)
					end

					# only preserve capacity entries that could be created based on expansion and retrofitting
					prepSys_dic[sys][sSym][capaSym] = prepSys_dic[sys][sSym][capaSym] |> (x -> (var = innerjoin(x.var, potCapa_df, on = intCol(x.var, :dir)), resi = x.resi))	
				end

				# ! remove variables for retrofitting and grouped capacity for cases where no start capacity can exist
				for grpSym in filter(x -> occursin("grp", lowercase(string(x))), collect(keys(prepSys_dic[sys][sSym])))

					type_sym = Symbol(replace(string(grpSym), "grpCapa" => ""))
					# obtain original grouped capacity variables
					grpCapa_tup = prepSys_dic[sys][sSym][grpSym]
					potResi_df = filter(x -> x.var != AffExpr(), grpCapa_tup.resi)
					potCapa_df = potResi_df |> (x -> select(x, filter(x -> !(x in (:var, :dir)), namesSym(potResi_df))))

					# ! determine which combinations of disSup and disSup_last are possible

					# checks expansion variables
					if Symbol(:exp, type_sym) in keys(prepSys_dic[sys][sSym]) && !isempty(prepSys_dic[sys][sSym][Symbol(:exp, type_sym)].var)
						
						exp_df = prepSys_dic[sys][sSym][Symbol(:exp, type_sym)].var

						# expand according to expansin timesteps
						allDf_arr = map(eachrow(exp_df)) do x
							l_int = length(x.Ts_disSup)
							rem_df = repeat(DataFrame(x[intCol(exp_df)]), inner = l_int, outer = 1)
							ext_df = DataFrame(Ts_expSup = x.Ts_expSup, Ts_disSup = x.Ts_disSup)
							return hcat(rem_df, ext_df)
						end
						exp_df = vcat(allDf_arr...)

						# obtain last year of operation for corresponding expansion
						exp_df[!,:Ts_disSup_last] = map(x -> maximum(x), exp_df[!,:Ts_disSup])
						exp_df = flatten(exp_df, :Ts_disSup)
						# sets superordinate expansion timestep to zero where its not specified in residual capacities either
						if (sys == :Te ? anyM.parts.tech[sSym].type : anyM.parts.exc[sSym].type) != :emerging 
							exp_df[!,:Ts_expSup] .= 0 
						end
						potCapa_df = unique(select(exp_df, Not([:Ts_exp]))) |> (x -> isempty(potCapa_df) ? x : joinMissing(potCapa_df, x, intCol(potCapa_df), :outer, Dict()))
					end

					# check retrofitting variables with tech as a target
					retro_sym = Symbol(replace(string(grpSym), "grpCapa" => "retro"))
					if retro_sym in keys(prepSys_dic[sys][sSym])
						if sys == :Te && grpSym == :grpCapaConv
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_expSup_i, :R_exp_i, :Te_i])), :Te_j => :Te, :R_exp_j => :R_exp, :Ts_expSup_j => :Ts_expSup)
						elseif sys == :Te
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_expSup_i, :R_exp_i, :Te_i, :id_i])), :Te_j => :Te, :R_exp_j => :R_exp, :Ts_expSup_j => :Ts_expSup, :id_j => :id)
						else
							retro_df = rename(select(prepSys_dic[sys][sSym][retro_sym].var, Not([:Ts_retro, :Ts_expSup_i, :R_from_i, :R_to_i, :Exc_i])), :Exc_j => :Exc, :R_from_j => :R_from, :R_to_j => :R_to, :Ts_expSup_j => :Ts_expSup)
						end
						if !isempty(retro_df)
							retro_df = filter(x -> x[sys] == sys_int, flatten(retro_df, :Ts_disSup))
							potCapa_df = unique(retro_df) |> (x -> isempty(potCapa_df) ? x : joinMissing(potCapa_df, x, intCol(potCapa_df), :outer, Dict()))
						end
					end

					if isempty(potCapa_df) # delete corresponding entries if retrofitting cannot occur
						delete!(prepSys_dic[sys][sSym], grpSym)
						delete!(prepSys_dic[sys][sSym], Symbol(:retro, type_sym))
					else
						join_arr = type_sym == :Conv ? [:Ts_expSup, :R_exp, :Te] : ( type_sym == :Exc ? [:Ts_expSup, :R_from, :R_to, :Exc] : [:Ts_expSup, :R_exp, :Te, :id])
						prepSys_dic[sys][sSym][grpSym] = (var = orderDf(potCapa_df), resi = potResi_df)

						# expand retrofitting entries, filter unrequired entries and group again
						allRetro_df = flatten(rename(prepSys_dic[sys][sSym][Symbol(:retro, type_sym)].var, Symbol.(string.(join_arr, "_i")) .=> join_arr), :Ts_disSup)
						startRetro_df, targetRetro_df = [filter(z -> y ? z[sys] == sys_int : z[sys] != sys_int, allRetro_df) for y in [true, false]]
						allRetro_df = isempty(potCapa_df) ? targetRetro_df : vcat(targetRetro_df, innerjoin(startRetro_df, potCapa_df, on =  intCol(potCapa_df)))

						allRetro_df = rename(combine(groupby(allRetro_df, filter(x -> x != :Ts_disSup, intCol(allRetro_df))), :Ts_disSup => (x -> [x]) => :Ts_disSup), join_arr .=> Symbol.(string.(join_arr, "_i")))
						allRetro_df[!,:Ts_disSup] = map(x -> collect(x), allRetro_df[!,:Ts_disSup])
						# remove retrofitting entries
						prepSys_dic[sys][sSym][Symbol(:retro, type_sym)] = (var = allRetro_df, resi = DataFrame())

						# ! replace entries for target technology
						for s in unique(allRetro_df[!,Symbol(sys,"_j")])
							nonRel_df  = filter(x -> x[Symbol(sys,"_j")] != s, prepSys_dic[sys][sysSym(s, anyM.sets[sys])][Symbol(:retro, type_sym)].var)
							prepSys_dic[sys][sysSym(s, anyM.sets[sys])][Symbol(:retro, type_sym)] = (var = orderDf(vcat(nonRel_df, filter(x -> x[Symbol(sys,"_j")] == s, allRetro_df))), resi = DataFrame())
						end
					end
				end

				# ! removes storage variables where capacity does not exist for interal storage
				if (:stIntIn in keys(part_obj.carrier) && !(:stExtIn in keys(part_obj.carrier))) || (:stExtIn in keys(part_obj.carrier) && !(:stExtOut in keys(part_obj.carrier))) && :capaConv in keys(prepSys_dic[sys][sSym])
				
					for x in intersect(keys(prepSys_dic[sys][sSym]), [:capaStIn, :capaStOut, :capaStSize])
						flt_df = prepSys_dic[sys][sSym][:capaConv].var |> (w -> innerjoin(prepSys_dic[sys][sSym][x].var, w, on = intCol(w)))
						prepSys_dic[sys][sSym][x] = (var = flt_df, resi = prepSys_dic[sys][sSym][x].resi)
					end
				end
			end
			
			# ! replace fixed variables with a parameter, if holdFixed is active
			if anyM.options.holdFixed
				for prepSym in collect(keys(prepSys_dic[sys][sSym]))
					# get relevant parameter data
					limPar_obj = getLimPar(anyM.parts.lim, Symbol(prepSym, :Fix), anyM.sets[sys], sys = sysInt(sSym, anyM.sets[sys]))
					# get all cases where variables are fixed
					fixLim_df = getFix(prepSys_dic[sys][sSym][prepSym].var, limPar_obj, anyM)
					# fixed variables are not created and value is enforced via parameters
					if !isempty(fixLim_df)
						fixLim_df[!,:var] .= map(x -> AffExpr(x), fixLim_df[!,:val])
						resi_df = prepSys_dic[sys][sSym][prepSym].resi
						resi_df = select(filter(x -> x.val != 0.0, fixLim_df), Not([:val])) |> (w -> isempty(resi_df) ? w : vcat(w, antijoin(resi_df, w, on = intCol(w))))
						prepSys_dic[sys][sSym][prepSym] = prepSys_dic[sys][sSym][prepSym] |> (x -> (var =  removeEntries([select(fixLim_df, Not([:val, :var]))], x.var), resi = resi_df))
					end
				end
			end

			# ! remove expansion variables that become obsolete because no corresponding capacities exist anymore
			for expVar in filter(x -> occursin("exp", string(x)), keys(prepSys_dic[sys][sSym]))
				# gets capacity and expansion variables
				capa_df = prepSys_dic[sys][sSym][Symbol(replace(string(expVar), "exp" => "capa"))] |> (w -> mergePrep(w))
				if isempty(capa_df) delete!(prepSys_dic[sys][sSym], expVar), continue end
				if isempty(prepSys_dic[sys][sSym][expVar].var) continue end
				exp_df = flatten(prepSys_dic[sys][sSym][expVar].var, [:Ts_expSup, :Ts_disSup])
				# groups capacity by superordinate dispatch and matches with expansion
				capaGrp_df = combine(groupby(capa_df, filter(x -> x != :Ts_disSup, intCol(capa_df))), :Ts_disSup => (x -> [x]) => :Ts_disSup)
				exp_df = (part_obj.type != :emerging ? filter(x -> x != :Ts_expSup, intCol(capaGrp_df)) : intCol(capaGrp_df)) |> (w -> innerjoin(exp_df, select(rename(capaGrp_df, :Ts_disSup => :Ts_disSup2), vcat(w, [:Ts_disSup2])), on = w))
				if isempty(exp_df) && isempty(prepSys_dic[sys][sSym][expVar].resi) delete!(prepSys_dic[sys][sSym], expVar), continue end
				# filters cases where there is no capacity in all superordinate dispatch timesteps relevant for the respective expansion variable
				filter!(x -> !isempty(intersect(x.Ts_disSup, x.Ts_disSup2)), exp_df)
				if isempty(exp_df) && isempty(prepSys_dic[sys][sSym][expVar].resi) delete!(prepSys_dic[sys][sSym], expVar), continue end
				# groups expansion again and writes remaining entries to dictionary
				prepSys_dic[sys][sSym][expVar] = (var = combine(x -> (Ts_expSup = [x.Ts_expSup], Ts_disSup = [x.Ts_disSup]), groupby(select(exp_df, Not([:Ts_disSup2])), filter(x -> x != :Ts_expSup, intCol(exp_df)))), resi = prepSys_dic[sys][sSym][expVar].resi)
			end

		end
	end

	# ! ensure consistency among different storage capacities (to every storage in- or output capacity a corresponding storage size has to exist)
	stVar_arr = ([:capaStIn, :capaStOut, :capaStSize], [:expStIn, :expStOut, :expStSize])
	nameSt_dic = Dict(:capaStIn => "input power", :capaStOut => "output power", :capaStSize => "energy", :expStIn => "input power", :expStOut => "output power", :expStSize => "energy")

	for tSym in collect(keys(prepSys_dic[:Te]))
		
		# collect variables 
		prepTech_dic = prepSys_dic[:Te][tSym]
		part = anyM.parts.tech[tSym]
		stKey_arr = collectKeys(keys(prepTech_dic))

		for a in 1:2
			# check storage consistency if storage is relevant
			if !isempty(intersect(stVar_arr[a], stKey_arr))

				# loops over groups of storage capacities
				stTypes_arr = intersect(keys(part.carrier), (:stExtIn, :stExtOut, :stIntIn, :stIntOut))
				
				for i in 1:length(part.carrier[stTypes_arr[1]])

					# get all storage capacities
					allSt_arr = filter(z -> !isempty(z), vcat(map(y -> collect(map(x -> filter(w -> w.id == i, getfield(prepTech_dic[y], x)), (:var, :resi))), intersect(stVar_arr[a], stKey_arr))...))
					if isempty(allSt_arr) continue end
					
					relSt_df = unique(vcat(map(w -> select(w, intCol(w, [:Ts_expSup, :Ts_disSup])), allSt_arr)...))
					
					# loops over relevant capacities
					for var in stVar_arr[a]

						# avoids check if technology does not require storage input or output capacity
						if var in (:capaStSize, :expStSize) || (var in (:capaStIn, :expStIn) && (:stExtIn in stTypes_arr || :stIntIn in stTypes_arr)) || (var in (:capaStOut, :expStOut) && (:stExtOut in stTypes_arr || :stIntOut in stTypes_arr))

							# find cases where variable of checked type is missing
							if var in stKey_arr 
								disInter_boo = part.stCyc == -1 && !isempty(anyM.subPro) && anyM.subPro != (0,0) && var == :capaStSize
								noVar_df = (part.type == :stock && part.decomm == :none && !disInter_boo ? [:resi,] : [:resi, :var]) |> (z -> antijoin(relSt_df, unique(vcat(map(u -> getfield(prepTech_dic[var], u) |> (k -> select(k, intCol(k, [:Ts_expSup, :Ts_disSup]))), z)...)), on = names(relSt_df)))
							else
								noVar_df = relSt_df
							end

							# add missing entries to preparation dictionaries and report
							if !isempty(noVar_df)
								prepTech_dic[var] = var in stKey_arr ? (var = vcat(prepTech_dic[var].var, noVar_df), resi = prepTech_dic[var].resi) : (var = noVar_df, resi = DataFrame())
								if part.type == :stock
									push!(anyM.report, (2, "technology mapping", "storage capacity", "in some cases $(nameSt_dic[var]) capacities for stock technology '$(string(tSym))' are not defined, but other storage capacity exists, if not subject to other limits, these capacities are unrestricted"))
								else
									varType_str = a == 1 ? "capacity" : "expansion"
									push!(anyM.report, (2, "technology mapping", "storage $varType_str", "in some cases limits prevent $(nameSt_dic[var]) $varType_str for technology '$(string(tSym))', but other storage $varType_str exists, therefore respective variables exist anyway"))
								end
							end
						end
					end 
				end
			end
		end
	end

end

# ! function to either create new entry of retrofitting or merge with existing ones
function addToRetro!(prep_dic::Dict{Symbol,NamedTuple}, new_df::DataFrame, add_sym::Symbol)

	# merges with existing retrofitting variables if any exist
	if add_sym in keys(prep_dic)
		fullRetro_df = vcat(prep_dic[add_sym].var, new_df)
	else
		fullRetro_df = new_df
	end

	prep_dic[add_sym] = (var = fullRetro_df, resi = DataFrame(var = AffExpr[]))
end

# ! converts all regions to values of start or target system for retrofitting
function adjustRetroRegion(sys::Symbol, retro_df::DataFrame, start::Bool=true)
	
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
		retro_df[!,Symbol(:R_from_,drop_sym)] = retro_df[!,Symbol(:R_from_,keep_sym)]; 
		retro_df[!,Symbol(:R_to_,drop_sym)] = retro_df[!,Symbol(:R_to_,keep_sym)]
	end
	return unique(retro_df)
end

# ! add entries for installed capacity
function addInsCapa!(prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}}, anyM::anyModel)

	# add installed capacity for exchange
	for excSym in collect(keys(prepSys_dic[:Exc]))
		prepExc_dic = prepSys_dic[:Exc][excSym]
	
		if anyM.parts.exc[excSym].decomm != :none && :capaExc in keys(prepExc_dic)
			# get all cases where variables are not-fixed and as a result different variables for operated and installed capacity variables are still needed
			if anyM.options.holdFixed
				limPar_obj = getLimPar(anyM.parts.lim, :capaExcFix, anyM.sets[:Exc], sys = sysInt(excSym, anyM.sets[:Exc]))
				unfix_df = prepExc_dic[:capaExc].resi |> (w -> antijoin(w, getFix(w, limPar_obj, anyM), on = intCol(w)))
			else
				unfix_df = prepExc_dic[:capaExc].resi
			end
			# re-define capacity variables as installed variables
			prepExc_dic[:insCapaExc] =  (var = prepExc_dic[:capaExc].var, resi = unfix_df)
			# create an entry for capacity variables, also where installed capacities only exists as fixed residual
			excResi_df = select(unfix_df, Not([:var]))
			capaExc_df = unique(vcat(prepExc_dic[:capaExc].var, filter(x -> x.R_from < x.R_to, vcat(excResi_df, rename(excResi_df, :R_from => :R_to, :R_to => :R_from)))))
			# unless exchange itself is directed, operated capacity is always undirected (meaning symmetric)
			capaExc_df[!,:dir] = map(x -> anyM.parts.exc[excSym].dir, eachrow(capaExc_df))
			prepExc_dic[:capaExc] =  (var = capaExc_df, resi = antijoin(prepExc_dic[:capaExc].resi, capaExc_df, on = intCol(capaExc_df)))
			# rename entry for grouped capacities just to be consistent
			if :grpCapaExc in keys(prepExc_dic)
				prepExc_dic[:grpInsCapaExc] = prepExc_dic[:grpCapaExc]
				delete!(prepExc_dic, :grpCapaExc)
			end
			# delete corresponding entry if no installed capacites were defined
			for cap in intersect(keys(prepExc_dic), [:insCapaExc, :grpInsCapaExc])
				if prepExc_dic[cap] |> (w -> isempty(w.var) && isempty(w.resi)) delete!(prepExc_dic, cap) end
			end
		end
	end

	# add installed capacity for technologies (and distributed cases with inter-annual storage where capaStSize must be a variable)
	for tSym in collect(keys(prepSys_dic[:Te]))
		prepTech_dic = prepSys_dic[:Te][tSym]
		if anyM.parts.tech[tSym].decomm != :none
			for capTy in intersect(keys(prepTech_dic), (:capaConv, :capaStIn, :capaStOut, :capaStSize, :capaExc))	
				if isempty(prepTech_dic[capTy].resi) && length(anyM.supTs.step) == 1 continue end 
				unfix_df = prepTech_dic[capTy].resi
				# re-define capacity variables as installed variables
				prepTech_dic[Symbol(:ins, makeUp(capTy))] =  (var = prepTech_dic[capTy].var, resi = unfix_df)
				if !isempty(unfix_df)
					# determines where variables for installed capacity are necessary
					newResi_df = antijoin(prepTech_dic[capTy].resi, unfix_df, on = intCol(unfix_df))
					prepTech_dic[capTy] =  (var = unfix_df |> (x -> isempty(x) ? prepTech_dic[capTy].var : unique(vcat(prepTech_dic[capTy].var, select(x, Not([:var]))))), resi = newResi_df)
				end
				# rename entry for grouped capacities just to be consistent
				if Symbol(:grp, makeUp(capTy)) in keys(prepTech_dic)
					prepTech_dic[Symbol(:grpIns, makeUp(capTy))] = prepTech_dic[Symbol(:grp, makeUp(capTy))]
					delete!(prepTech_dic, Symbol(:grp, makeUp(capTy)))
				end
				# delete corresponding entry if no installed capacites were defined
				for cap in intersect(keys(prepTech_dic), [Symbol(x, makeUp(capTy)) for x in [:ins, :grpIns]])
					if prepTech_dic[cap] |> (w -> isempty(w.var) && isempty(w.resi)) delete!(prepTech_dic, cap) end
				end
			end
		elseif anyM.parts.tech[tSym].stCyc == -1 && !isempty(anyM.subPro) && anyM.subPro != (0,0) && :capaStSize in keys(prepTech_dic)	
			prepTech_dic[:capaStSize] = (var = unique(vcat(prepTech_dic[:capaStSize].var, select(prepTech_dic[:capaStSize].resi, Not([:var])))), resi = DataFrame())
		end
	end
end

#endregion

#region # * create investment related variables and constraints

# ! create expansion and capacity variables
function createExpCap!(part::AbstractModelPart, prep_dic::Dict{Symbol,NamedTuple}, anyM::anyModel, ratioVar_dic::Dict{Symbol,Pair{String,String}} = Dict{Symbol,Pair{String,String}}())
	
	# create variables for expansion and capacity
	for expVar in sort(collectKeys(keys(prep_dic)))

		seasStSize_boo = expVar == :capaStSize && part.stCyc == -1 && !isempty(anyM.subPro) && anyM.subPro != (0,0) # only needs capacity for seasonal storage in case of inter-annual storage system and distributed creation
		
		exc_boo = typeof(part) <: ExcPart
		s_sym = exc_boo ? :Exc : :Te
		sys_int = sysInt(Symbol(part.name[end]), anyM.sets[s_sym])

		varMap_tup = prep_dic[expVar]
		# determines scaling factor
		if occursin("exp", string(expVar)) || occursin("insCapa", string(expVar))
			scaFac_fl = anyM.options.scaFac.insCapa
		else
			scaFac_fl = getfield(anyM.options.scaFac, occursin("StSize", string(expVar)) ? :capaStSize : :capa)
		end

		# adjusts regions for retrofitting variables (otherwise region in start and target could differ)
		if occursin("retro", string(expVar))
			varMap_tup = (var = vcat([adjustRetroRegion(s_sym, filter(x -> x[Symbol(s_sym, :_i)] |> (z -> y ? z == sys_int : z != sys_int), varMap_tup.var), y) for y in [true, false]]...), resi = varMap_tup.resi)
		end

		# create dataframe of capacity or expansion variables by creating the required capacity variables and join them with pure residual values
		var_df = createVar(varMap_tup.var, seasStSize_boo ? "capaStSizeSeason" : string(expVar), anyM.options.bound.capa, anyM.optModel, anyM.lock, anyM.sets, scaFac = scaFac_fl)

		# add columns to retrofitting variables to indicate start or target
		if occursin("retro", string(expVar)) 
			sInt = sysInt(Symbol(part.name[end]), anyM.sets[exc_boo ? :Exc : :Te]) 
			var_df[!,:start] = map(x -> x == sInt, var_df[!, Symbol(exc_boo ? :Exc : :Te, :_i)])
		end

		# if retrofitting of exchange relates directed and undirected some entries need to flipped 
		if expVar == :retroExc
			gatherRetro_arr = DataFrame[] 
			for x in (:i, :j) 
				# get all start capacities in first and all target capacities in second iteration
				retroVar_df = filter(a -> a[Symbol(:Exc_, x)] == sys_int, var_df)
				y = x == :i ? :j : :i
				# loops over other retrofitting capacity and flips in case one is directed and the other undirected 
				for excSub in groupby(retroVar_df, [Symbol(:Exc_, y)])
					if part.dir && !anyM.parts.exc[sysSym(excSub[1, Symbol(:Exc_, y)], anyM.sets[:Exc])].dir
						push!(gatherRetro_arr, vcat(orderDf(rename(DataFrame(excSub), Symbol(:R_from_, x) => Symbol(:R_to_, x), Symbol(:R_to_, x) => Symbol(:R_from_, x))), DataFrame(excSub)))
					else
						push!(gatherRetro_arr, DataFrame(excSub))
					end
				end
			end
			var_df = vcat(gatherRetro_arr...)
		end

		# add residual capacities in case of installed capacities
		if !isempty(varMap_tup.resi)
				
			# flips and repeats entries for directed exchange variabes before moving on
			if exc_boo && !part.dir && ((expVar == :capaExc && part.decomm == :none) || (expVar == :insCapaExc && part.decomm != :none))
				var_df = filter(r -> r.dir, var_df) |> (x -> vcat(filter(r -> !r.dir, var_df), vcat(x, rename(x, replace(namesSym(x), :R_to => :R_from, :R_from => :R_to)))))
			end

			# in case of decommissioning or retrofitting, set lower limit of the installed capacity variable to the negative of the residual capacity, so the lower limit for the whole expression is effectively zero
			if occursin("ins", string(expVar)) || occursin("grp", string(expVar))
				if expVar == :insCapaExc # in case of directed exchange the bigger residual capacities in both directions is used as lower bound => symmetric decommissioning of asymmetric capacities
					resi_df = part.dir ? varMap_tup.resi : flipExc(varMap_tup.resi)
					resi_df = combine(groupby(resi_df, filter(x -> x != :dir, intCol(var_df))), :var => (x -> AffExpr(minimum(getfield.(x, :constant)))) => :val)
				else
					resi_df = rename(varMap_tup.resi, :var => :val)
				end
				setLow_df = joinMissing(var_df, resi_df, intersect(intCol(var_df, :dir), intCol(resi_df, :dir)), :left, Dict(:val => AffExpr()))
				foreach(x -> set_lower_bound(collect(keys(x.var.terms))[1], -x.val.constant/collect(values(x.var.terms))[1]), eachrow(setLow_df))
			end

			# add residual values to expression with variable
			join_arr = intCol(var_df, [:Ts_expSup, :Ts_disSup, :dir])
			var_df = combine(x -> (var = x.var + x.var_1,), groupby(joinMissing(var_df, varMap_tup.resi[!,vcat(:var,join_arr...)], join_arr, :outer, Dict(:var => AffExpr(), :var_1 => AffExpr()), true), join_arr))
		end

		# expands table of expansion variables to superordinate timesteps and modifies expansion variable accordingly
		if (occursin("exp", string(expVar)))
			allDf_arr = map(eachrow(var_df)) do x
				l_int = length(x.Ts_disSup)
				rem_df = repeat(DataFrame(x[intCol(var_df)]), inner = l_int, outer = 1)
				ext_df = DataFrame(Ts_expSup = x.Ts_expSup, Ts_disSup = x.Ts_disSup, var = x.var ./ fill(l_int, l_int) )
				return hcat(rem_df, ext_df)
			end
			var_df = vcat(allDf_arr...)
		end

		if !isempty(var_df)	part.var[seasStSize_boo ? :capaStSizeSeason : expVar] = var_df end
	end

	# create variables for seasonal and inter-annual storage size
	if isdefined(part,:stCyc) && part.stCyc == -1 && (isempty(anyM.subPro) || anyM.subPro == (0,0)) && :capaStSize in keys(part.var)
		part.var[:capaStSizeSeason] = createVar(select(part.var[:capaStSize], Not([:var])), "capaStSizeSeason", anyM.options.bound.capa, anyM.optModel, anyM.lock, anyM.sets, scaFac = anyM.options.scaFac.capaStSize)
		part.var[:capaStSizeInter] = createVar(select(part.var[:capaStSize], Not([:var])), "capaStSizeInter", anyM.options.bound.capa, anyM.optModel, anyM.lock, anyM.sets, scaFac = anyM.options.scaFac.capaStSize)
	end

end

# ! connect capacity and expansion variables
function createCapaCns!(part::AbstractModelPart, sets_dic::Dict{Symbol,Tree}, cns_dic::Dict{Symbol,cnsCont}, optModel::Model, holdFixed::Bool, excDir_arr::Array{Int64,1} = Int[])
	# create capacity constraint for installed capacities	
	for capaVar in filter(x -> any(occursin.(["capa", "insCapa"], string(x))), keys(part.var))

		# skip capacity variable, if also installed capacity exists
		isDecomm_boo = false
		if Symbol(replace(string(capaVar), "capa" => "insCapa")) in keys(part.var) 
			if occursin("capa", string(capaVar))
				isDecomm_boo = true
			else
				continue
			end
		end

        index_arr = intCol(part.var[capaVar])
		join_arr = part.type != :mature ? index_arr : filter(x -> x != :Ts_expSup, collect(index_arr))
		exc_boo = occursin("Exc", string(capaVar))
		st_boo = occursin("St", string(capaVar)) 
		sys_int = sysInt(Symbol(part.name[end]), sets_dic[exc_boo ? :Exc : :Te])

        # joins corresponding capacity, retrofitting and expansion variables together
		expVar_sym, retroVar_sym = [Symbol(replace(string(capaVar), (isDecomm_boo ? "insCapa" : "capa") => x)) for x in ["exp", "retro"]]    
		exp_boo, retro_boo = [expVar_sym in keys(part.var), retroVar_sym in keys(part.var) && sys_int in part.var[retroVar_sym][!, exc_boo ? :Exc_j : :Te_j]]
	
		# gets capacity variables
		cns_df = rename(part.var[capaVar], :var => :capa)

		# create constraints for cases where due to retrofitting seperate variables for grouped capacity exist 
		if Symbol(:grp, makeUp(capaVar)) in keys(part.var)
			join_arr = exc_boo || st_boo ? (st_boo ? [:Ts_expSup, :Ts_disSup, :R_exp, :Te, :id] : [:Ts_expSup, :Ts_disSup, :R_from, :R_to] ) : [:Ts_expSup, :Ts_disSup, :R_exp, :Te]
			grpCapa_df = combine(groupby(part.var[Symbol(:grp, makeUp(capaVar))], join_arr), :var => (x -> sum(x)) => :var)
			grpCns_df = innerjoin(cns_df, grpCapa_df, on = join_arr)
			grpCns_df[!,:cnsExpr] = @expression(optModel, grpCns_df[:capa] .- grpCns_df[:var])
			cns_dic[Symbol(:grp, makeUp(capaVar), :_a)] = cnsCont(select(grpCns_df, intCol(grpCns_df, :cnsExpr)), :equal)
			cns_df = antijoin(cns_df, grpCns_df, on = join_arr)
		end

		# moves on if no more variables are left to be controlled by a capacity constraint
		if isempty(cns_df) || all(map(x -> isempty(x.terms), cns_df[!,:capa])) || !(exp_boo || retro_boo) continue end
	
		if exc_boo && !part.dir filter!(x -> x.R_from < x.R_to, cns_df) end

		# adds retrofitting variables where technology is target
		if retro_boo
			retroVar_df = filter(x -> sys_int == x[exc_boo ? :Exc_j : :Te_j], flatten(part.var[retroVar_sym], :Ts_disSup))
			retro_arr = vcat([:Ts_expSup_j, :Ts_disSup], exc_boo ? [:R_from_j, :R_to_j, :Exc_j] : (st_boo ? [:R_exp_j, :Te_j, :id_j] : [:R_exp_j, :Te_j]))
			if exc_boo && part.dir
				noFlip_df = filter(x -> x[:Exc_i] in excDir_arr, retroVar_df)
				flip_df = filter(x -> !(x[:Exc_i] in excDir_arr), retroVar_df)  |> (y -> vcat(y, rename(y, :R_from_j => :R_to_j, :R_to_j => :R_from_j)))
				retroVar_df = vcat(noFlip_df, flip_df)
			end
			retro_df = rename(combine(groupby(retroVar_df, retro_arr), :var => (x -> sum(x)) => :var), retro_arr .=> intCol(cns_df))
			
			cns_df[!,:retro_j] = aggDivVar(retro_df, cns_df, tuple(intCol(cns_df)...), sets_dic)
		end
		
		# adds expansion variables
		if exp_boo
			expVar_df = flatten(part.var[expVar_sym], :Ts_disSup)
			cns_df = joinMissing(cns_df, combine(groupby(expVar_df, join_arr), :var => (x -> sum(x)) => :exp), join_arr, :left, Dict(:exp => AffExpr()))
		end 

		if !exp_boo && !retro_boo continue end

		# creates final constraint object
		cns_df[!,:cnsExpr] = @expression(optModel, cns_df[!,:capa] .-  getfield.(cns_df[!,:capa], :constant) .+ (exp_boo ? .- cns_df[!,:exp] : 0.0) .+ (retro_boo ? .- cns_df[!,:retro_j] : 0.0))	
		cns_dic[Symbol(capaVar)] = cnsCont(filter(x -> x.cnsExpr != AffExpr(), select(cns_df, intCol(cns_df, :cnsExpr))), :equal)
	end

	# in case system is start to retrofitting, installed capacities are further differentiated by remaining lifetime, constraints on these variables are created next
	for capaVar in filter(x -> occursin("grp", lowercase(string(x))), keys(part.var))

		grpCapa_df = rename(part.var[capaVar], :var => :capa)
		type_sym = Symbol(replace(replace(String(capaVar), "grpCapa" => ""), "grpInsCapa" => ""))

		# join retrofitting variables
		grp_arr = type_sym == :Conv ? [:Ts_expSup, :R_exp, :Te] : (type_sym != :Exc ? [:Ts_expSup, :R_exp, :Te, :id] : [:Ts_expSup, :R_from, :R_to, :Exc])
		
		retro_df = rename(combine(groupby(rename(flatten(part.var[Symbol(:retro, type_sym)], :Ts_disSup), Symbol.(string.(grp_arr, "_i")) .=> grp_arr), intCol(grpCapa_df)), :var => (x -> sum(x)) => :var), :var => :retro)
		cns_df = joinMissing(grpCapa_df, retro_df, intCol(grpCapa_df), :left, Dict(:retro => AffExpr()))

		# join expansion variables
		if Symbol(:exp, type_sym) in keys(part.var)
			exp_df = rename(part.var[Symbol(:exp, type_sym)], :var => :exp)
			exp_df[!,:Ts_disSup_last] = maximum.(exp_df[!,:Ts_disSup])

			if part.type != :emerging
				exp_df[!,:Ts_expSup] .= 0
			end

			exp_df = flatten(select(exp_df, Not([:Ts_exp])), :Ts_disSup)
			cns_df = joinMissing(cns_df, combine(groupby(exp_df, intCol(exp_df)), :exp => (x -> sum(x)) => :exp) , intCol(grpCapa_df), :left, Dict(:exp => AffExpr()))
		end

		# create constraint
		cns_df[!,:cnsExpr] = @expression(optModel, cns_df[!,:capa] .-  getfield.(cns_df[!,:capa],:constant) .+ cns_df[!,:retro] .+ (Symbol(:exp,type_sym) in keys(part.var) ? .- cns_df[!,:exp]  : 0.0))
		if holdFixed filter!(x -> !isempty(x.cnsExpr.terms), cns_df) end # filter cases where no actual variables are compared since they were replaced with parameters
		cns_dic[Symbol(capaVar, :_b)] = cnsCont(filter(x -> x.cnsExpr != AffExpr(), select(cns_df, intCol(cns_df, :cnsExpr))), :equal)
	end
end

# ! create constraints regarding operated variables
function createOprVarCns!(part::AbstractModelPart, cns_dic::Dict{Symbol,cnsCont}, anyM::anyModel)

	for capaVar in filter(x -> occursin("capa", string(x)), keys(part.var))
		insVar_sym = string(capaVar) |> (x -> Symbol(:ins, uppercase(x[1]), x[2:end]))
		if !(insVar_sym in keys(part.var)) continue end
		var_df = part.var[insVar_sym]
		exc_boo = :R_from in intCol(var_df)

		# ! create constraint to connect operated and installed capacity
		if exc_boo
			oprVar_df = part.dir ? part.var[capaVar] : flipExc(part.var[capaVar])
			var_df = leftjoin(var_df, rename(select(oprVar_df, Not([:dir])), :var => :var_2), on = intCol(var_df))
			var_df[!,:cnsExpr] = @expression(anyM.optModel, var_df[:var_2] .- var_df[:var])
			select!(var_df, Not([:var_2]))
		else
			oprVar_df = part.var[capaVar]
			var_df[!,:cnsExpr] = map(x -> x[2] - x[1], zip(var_df[!,:var], oprVar_df[!,:var]))
		end
		
		cns_dic[string(insVar_sym) |> (x -> Symbol(:de, uppercase(x[1]), x[2:end]))] = cnsCont(select(var_df, Not(:var)), :smaller)

		# ! create constraint to prevent re-commissioning of capacity once decommissioned
		if part.decomm == :decomm
			# add previous period and its capacity variable to table
			prevTs_dic = Dict(x => anyM.supTs.step[findall(x .== anyM.supTs.step)[1]] - 1 for x in anyM.supTs.step[2:end])
			cns_df = rename(filter(r -> r.Ts_disSup != anyM.supTs.step[1], oprVar_df), :var => :oprNow)
			cns_df[!,:Ts_disSupPrev] = map(x -> prevTs_dic[x], cns_df[!,:Ts_disSup])
			cns_df = rename(innerjoin(cns_df, oprVar_df; on = intCol(oprVar_df, :dir) |> (x -> Pair.(replace(x, :Ts_disSup => :Ts_disSupPrev), x))), :var => :oprPrev)

			# add expansion variable to dataframe
			if Symbol(replace(string(capaVar), "capa" => "exp")) in collect(keys(part.var))
				exp_df = part.var[Symbol(replace(string(capaVar), "capa" => "exp"))][!,Not(:Ts_disSup)]
				join_arr = filter(x -> x != :Ts_expSup, intCol(var_df))
				# aggregates expansion variables by "project"
				if exc_boo
					exp_df = combine(groupby(exp_df, filter(x -> x != :id, intCol(exp_df))), :var => (x -> sum(x)) => :var)
				end

				cns_df = joinMissing(cns_df, exp_df, Pair.(join_arr, replace(join_arr, :Ts_disSup => :Ts_expSup)), :left, Dict(:var => AffExpr(), :Ts_exp => 0))
				cns_df = rename(cns_df[!,Not(:Ts_exp)], :var => :expNow)
			else
				cns_df[!,:expNow] = map(x -> AffExpr(), 1:size(cns_df, 1))
			end

			# add retrofitting variable to dataframe
			if Symbol(replace(string(capaVar), "capa" => "retro")) in collect(keys(part.var)) && !isempty(cns_df)
				sInt = unique(cns_df[!,exc_boo ? :Exc : :Te])[1]
				retro_df = filter(x -> x[exc_boo ? :Exc_j : :Te_j] == sInt, part.var[Symbol(replace(string(capaVar), "capa" => "retro"))][!,Not(:Ts_disSup)])
				if !isempty(retro_df)
					rename_arr = vcat([:Ts_retro => :Ts_disSup, :Ts_expSup_j => :Ts_expSup], exc_boo ? [:R_from_j => :R_from, :R_to_j => :R_to, :Exc_j => :Exc] : [:R_exp_j => :R_exp, :Te_j => :Te])
					cns_df[!,:retroNow] = aggDivVar(rename(retro_df, rename_arr), cns_df, tuple(filter(x -> x != :Ts_disSupPrev, intCol(cns_df))...), anyM.sets)
				else
					cns_df[!,:retroNow] .= AffExpr()
				end
			else
				cns_df[!,:retroNow] .= AffExpr()
			end

			# add residual capacities of current and previous period
			joinResi_arr = filter(x -> x != :Ts_disSupPrev, intCol(cns_df, :dir))
			cns_df = rename(innerjoin(cns_df, part.var[capaVar], on = joinResi_arr), :var => :resiNow)
			cns_df[!,:resiNow] = getfield.(cns_df[!,:resiNow], :constant)
			cns_df = rename(joinMissing(cns_df, part.var[capaVar], Pair.(replace(joinResi_arr, :Ts_disSup => :Ts_disSupPrev), joinResi_arr), :left, Dict(:resiNow => AffExpr(), :var => AffExpr())), :var => :resiPrev)
			cns_df[!,:resiPrev] = getfield.(cns_df[!,:resiPrev], :constant)

			# create actual constraint information
			cns_df[!,:resiDelta] =  map(x -> (x.resiNow - x.resiPrev |> (l -> l > 0.0 ? l : 0.0)), eachrow(cns_df))
			cns_df[!,:cnsExpr]  = @expression(anyM.optModel, -1 .* cns_df[!,:oprNow] .+ cns_df[!,:oprPrev] .+ cns_df[!,:expNow] .+ cns_df[!,:retroNow] .+ cns_df[!,:resiDelta])

			if isempty(cns_df) continue end

			select!(cns_df, Not([:Ts_disSupPrev, :oprNow, :oprPrev, :expNow, :retroNow, :resiNow, :resiPrev, :resiDelta]))
			cns_dic[string(insVar_sym) |> (x -> Symbol(:re, uppercase(x[1]), x[2:end]))] = cnsCont(orderDf(cns_df), :greater)
		end
	end
end

# ! connect retrofitting variables from different systems
function createRetroConst!(capaSym::Symbol, cnsDic_arr::Array{Dict{Symbol,cnsCont},1}, sys_itr::Array{Tuple{Int64,Symbol},1}, anyM::anyModel, excDir_arr::Array{Int64,1}=Int[])

	exc_boo = capaSym == :Exc
	sys_sym = exc_boo ? :Exc : :Te

	retro_df = getAllVariables(Symbol(:retro, capaSym), anyM)

	# filters rows for duplicate rows where retrofitting connects directed and undirected exchanged
	if exc_boo
		filter!(x -> !((x.Exc_i in excDir_arr) != (x.Exc_j in excDir_arr)  && ((x.R_from_i > x.R_to_i) || (x.R_from_j > x.R_to_j))), retro_df)
	end

	if isempty(retro_df) return end

	# correct variables with retrofitting factor
	retro_df = matchSetParameter(select(retro_df, Not([:Ts_disSup])), anyM.parts.cost.par[Symbol(:facRetro, capaSym)], anyM.sets)
	retro_df[!,:var] = map(x -> x.start ? x.var * x.val : x.var, eachrow(retro_df))

	# aggregate retrofitting variables (first try to aggregate starting entries to target entries => works if start is not less detailed than target, afterwards aggregate remaining cases)
	start_df, target_df = [select(filter(x -> x.start == y, retro_df), Not([:start, :val])) for y in [1, 0]]
	start_df[!,:var] = -1 .* start_df[!,:var]
	target_df[!,:var2] = aggDivVar(start_df, target_df, tuple(intCol(retro_df)...), anyM.sets)

	more_df = select(filter(x -> x.var2 == AffExpr(), target_df), Not([:var2]))
	if !isempty(more_df)	
		start_df[!,:var2] = aggDivVar(more_df, start_df, tuple(intCol(retro_df)...), anyM.sets)
		retro_df = vcat(filter(x -> x.var2 != AffExpr(), start_df), filter(x -> x.var2 != AffExpr(), target_df))	
	else
		retro_df = target_df
	end

	# create final constraint expression by summing up variables
	retro_df = combine(groupby(retro_df, intCol(retro_df)), [:var, :var2] => ((x, y) -> x + y) => :cnsExpr)

	# add to different cnsDic for target technology or exchange
	for t in unique(retro_df[!, Symbol(sys_sym, :_i)])
		cnsDic_arr[filter(x -> x[2] == sysSym(t, anyM.sets[sys_sym]), sys_itr)[1][1]][Symbol(:retro, capaSym)] = cnsCont(select(filter(x -> x[Symbol(sys_sym, :_i)] == t, retro_df), intCol(retro_df, :cnsExpr)), :equal)
	end

end

#endregion

#region # * create capacity restrictions

# ! create all capacity restrictions for technology and exchange
function createCapaRestr!(part::AbstractModelPart, ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, cns_dic::Dict{Symbol,cnsCont}, anyM::anyModel, yTs_dic::Dict{Int64,Int64}=Dict{Int64,Int64}(), rmvOutC_arr::Array{Int,1}=Int[])

	# ! create restrictions for must run
	mustRestr_df = filter(x -> x.cnstrType == "must", part.capaRestr)

	if !isempty(mustRestr_df)
		# gather must run constraints for all carriers in loop
		allMustCns_arr = Array{DataFrame}(undef, size(mustRestr_df, 1))
		idx = 1

		for m in eachrow(mustRestr_df)
			addConvOut_boo = false

			# get relevant capacity variables and add carrier
			relCapa_arr = intersect((:capaConv, :capaStOut), keys(part.var))
			capaVar_df = vcat(map(x -> (Symbol(:must, makeUp(x)) in keys(part.var) ? part.var[Symbol(:must, makeUp(x))] : part.var[x]) |> (z -> x == :capaStOut ? z : insertcols!(copy(z), 1, :id => fill(0, size(z, 1)))), relCapa_arr)...)
			filter!(x -> x.id == 0 || (:stExtOut in keys(part.carrier) && !isempty(intersect(part.carrier.stExtOut, m.car))), capaVar_df)

			# add carriers and expand to dispatch regions
			capaVar_df[!,:C] .= m.car[1]
			capaVar_df[!,:R_dis] = map(x -> r_dic[(x.R_exp, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[x.C].rDis)], eachrow(capaVar_df))
			capaVar_df = select(flatten(capaVar_df, :R_dis), Not(:R_exp))

			# add variables for missing capacities (only relevant in subproblems)
			if :missCapa in keys(part.var)
				missCapa_df = filter(x -> x.C in m.car, part.var[:missCapa])
				missCapa_df[!,:var] = missCapa_df[!,:var] .* (-1)
				capaVar_df = vcat(missCapa_df, capaVar_df)
			end

			# match with design factor and aggregate capacities
			capaVar_df = matchSetParameter(capaVar_df, part.par[:desFac], anyM.sets; newCol = :desFac)
			capaVar_df[!,:var] = capaVar_df[!,:var] .* capaVar_df[!,:desFac]
			capaVar_df = combine(groupby(capaVar_df, filter(x -> x != :id, intCol(capaVar_df))), :var => (x -> sum(x)) => :capa)

			# get must-run parameter 
			mustOut_df = filter(x -> x.C == m.car[1], rename(part.par[:mustOut].data, :val => :mustOut))
		
			# match must-run with capacity variables
			mustOut_df[!,:Ts_disSup] = map(x -> yTs_dic[x], mustOut_df[!,:Ts_dis])
			mustOut_df = innerjoin(mustOut_df, capaVar_df, on = intCol(capaVar_df))
			select!(mustOut_df, Not([:Ts_disSup]))

			# gather relevant dispatch variables
			dis_arr = collect(intersect(keys(part.var), [:gen, :stExtOut]))
			join_arr = filter(x -> x != :C, intCol(mustOut_df))
			for dis in dis_arr
				# get relevant dispatch variables
				disVar_df = filter(x -> x.C == m.car[1], part.var[dis])
				# group variables and aggregate to must-out
				grpDis_df = combine(groupby(disVar_df, join_arr), :var => (x -> sum(x)) => dis)
				mustOut_df= joinMissing(mustOut_df, grpDis_df, join_arr, :left, Dict(dis => AffExpr()))

				# in case the current must run replaces a restriction on conversion output, check if all relevant variables are constrained
				if m.car[1] in rmvOutC_arr && dis == :gen && !addConvOut_boo
					if !isempty(antijoin(grpDis_df, mustOut_df, on = join_arr)) addConvOut_boo = true end
				end
			end

			# create actual constraint
			outVar_arr = intersect(namesSym(mustOut_df), [:gen, :stExtOut])
			aggCol!(mustOut_df, outVar_arr)
			mustOut_df[!,:cnsExpr] = @expression(anyM.optModel, mustOut_df[!,:mustOut] .* mustOut_df[!,:capa] .- mustOut_df[!,outVar_arr[1]])
			
			allMustCns_arr[idx] = select(mustOut_df, [:Ts_expSup, :Ts_dis, :R_dis, :C, :Te, :scr, :cnsExpr])
			idx = idx + 1

			# add a new restriction on conversion output, in case it was detected that must run cannot replace conversion output
			if addConvOut_boo
				push!(part.capaRestr, (cnstrType = "convOut", car = m.car, lvlTs = m.lvlTs, lvlR = m.lvlR))
			end
		end

		cns_dic[:mustOut] = cnsCont(vcat(allMustCns_arr...), :mustCapaConv in keys(part.var) ? :smaller : :equal)
	end

	# ! create all other capacity restrictions
	cnstrType_dic = Dict(:exc => (dis = (:exc,), capa = :Exc), :convOut => (dis = (:gen, :stIntIn), capa = :Conv), :convIn => (dis = (:use, :stIntOut), capa = :Conv),
							:stIn => (dis = (:stExtIn, :stIntIn), capa = :StIn), :stOut => (dis = (:stExtOut, :stIntOut), capa = :StOut), :stSize => (dis = (:stLvl,), capa = :StSize), :stSizeSeason => (dis = (:stLvl,), capa = :StSizeSeason))

	capaRestr_gdf = groupby(filter(x -> x.cnstrType != "must", part.capaRestr), :cnstrType)

	# loop over groups of capacity restrictions except for must-run (like out, stIn, ...)
	for restrGrp in capaRestr_gdf

		# relevant capacity variables
		type_sym = Symbol(split(String(restrGrp.cnstrType[1]), "_")[1])
		info_ntup = cnstrType_dic[type_sym]

		# removes internal variables for restriction if corresponding option is set 
		if type_sym in (:stOut, :stIn) && !part.intCapaRestr info_ntup = type_sym == :stIn ? (dis = (:stExtIn,), capa = :StIn) : (dis = (:stExtOut, ), capa = :StOut) end

		allCns_arr = Array{DataFrame}(undef,size(restrGrp,1))

		# obtains corresponding capacity if any exists
		if Symbol(:capa, info_ntup.capa) in keys(part.var)
			capaVar_df = part.var[Symbol(:capa, info_ntup.capa)] |> (z -> type_sym in (:stIn, :stOut, :stSize, :stSizeSeason) ? filter(x -> x.id == parse(Int, split(String(restrGrp.cnstrType[1]), "_")[2]), z) : z)
		else
			continue
		end
		
		# check special cases relevant for reduced foresight and storage level
		if typeof(part) == TechPart
			topFrs_boo = anyM.subPro == (0,0) && anyM.scr.frsLvl != 0
			subFrs_boo = anyM.subPro != (0,0) && !isempty(anyM.subPro ) && anyM.scr.frsLvl != 0 && anyM.scr.frsLvl > part.stCyc
		else
			topFrs_boo = false
			subFrs_boo = false
		end
		
		# loop over indiviudal constraints
		for (idx, restr) in enumerate(eachrow(restrGrp))
			allCns_arr[idx] = createRestr(part, copy(capaVar_df), restr, type_sym, info_ntup, ts_dic, r_dic, anyM.sets, anyM.supTs, anyM.scr, anyM.optModel, topFrs_boo, subFrs_boo)
		end

		allCns_df = vcat(filter(x -> !isempty(x), allCns_arr)...)
		if isempty(allCns_df) continue end

		# add all constraints to part
		allCns_df[!,:cnsExpr] = @expression(anyM.optModel, allCns_df[!,:disp] .- allCns_df[!,:capa])
		cns_dic[Symbol(type_sym, :Restr)] = cnsCont(orderDf(allCns_df[!, [intCol(allCns_df)..., :cnsExpr]]), :smaller)
	end

end

# adjust capacity variables spatially for match with dispatch variables
function getCapaToRestr(part::AbstractModelPart, capaVar_df::DataFrame, restr::DataFrameRow, type_sym::Symbol, ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, sets_dic::Dict{Symbol,Tree}, supTs_ntup::NamedTuple)

	agg_arr = type_sym == :exc ? [:Ts_expSup, :Ts_dis, :R_from, :R_to, :scr] : [:Ts_expSup, :Ts_dis, :R_dis, :scr] |> (x -> filter(x -> part.type == :emerging || x != :Ts_expSup, x))
	aggCapa_arr = filter(x -> x != :scr, agg_arr)

	# determines dimensions for aggregating dispatch variables
	capaVar_df[!,:lvlTs] .= restr.lvlTs
	capaVar_df[!,:lvlR] .= restr.lvlR

	# replaces expansion with dispatch regions and aggregates capacity variables accordingy if required
	if type_sym != :exc
		grpCapaVar_df = copy(select(capaVar_df, Not([:var]))) |> (y -> unique(combine(x -> (R_dis = r_dic[(x.R_exp[1], x.lvlR[1])],), groupby(y, namesSym(y)))[!,Not([:R_exp,:lvlR])]))
		resExp_ntup = :Ts_expSup in aggCapa_arr ? (Ts_expSup = supTs_ntup.lvl, Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1) : (Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1)
		sort!(grpCapaVar_df, orderDim(intCol(grpCapaVar_df)))
		grpCapaVar_df[!,:var] = aggUniVar(rename(capaVar_df, :R_exp => :R_dis), grpCapaVar_df, replace(aggCapa_arr, :Ts_dis => :Ts_disSup), resExp_ntup, sets_dic)
	else
		grpCapaVar_df = rename(copy(select(capaVar_df, Not([:var]))), :R_from => :R_a, :R_to => :R_b) |> (y -> unique(combine(x -> (R_from = r_dic[(x.R_a[1], x.lvlR[1])], R_to = r_dic[(x.R_b[1], x.lvlR[1])]), groupby(y, namesSym(y)))[!,Not([:lvlR,:R_a,:R_b])]))
		resExp_ntup = :Ts_expSup in aggCapa_arr ? (Ts_expSup = supTs_ntup.lvl, Ts_disSup = supTs_ntup.lvl, R_from = restr.lvlR, R_to = restr.lvlR, scr = 1) : (Ts_disSup = supTs_ntup.lvl, R_dis = restr.lvlR, scr = 1)
		sort!(grpCapaVar_df, orderDim(intCol(grpCapaVar_df)))
		grpCapaVar_df[!,:var] = aggUniVar(capaVar_df, grpCapaVar_df, replace(aggCapa_arr, :Ts_dis => :Ts_disSup), resExp_ntup, sets_dic)
	end

	# expand capacity to dimension of dispatch
	capaDim_df = combine(x -> (Ts_dis = ts_dic[(x.Ts_disSup[1], x.lvlTs[1])],), groupby(grpCapaVar_df[!,Not(:var)], namesSym(grpCapaVar_df[!,Not(:var)])))[!,Not(:lvlTs)]
	sort!(capaDim_df, orderDim(intCol(capaDim_df)))
	select!(grpCapaVar_df, Not(:lvlTs))

	return capaDim_df, grpCapaVar_df,  agg_arr 

end

# ! sub-function to create restriction
function createRestr(part::AbstractModelPart, capaVar_df::DataFrame, restr::DataFrameRow, type_sym::Symbol, info_ntup::NamedTuple,
															ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, sets_dic::Dict{Symbol,Tree}, supTs_ntup::NamedTuple, scr_ntup::NamedTuple, optModel::Model, topFrs_boo::Bool, subFrs_boo::Bool)

	conv_boo = type_sym in (:convOut, :convIn) && type_sym != :exc
	dim_arr = type_sym == :exc ? [:Ts_expSup, :Ts_dis, :R_from, :R_to, :Exc, :scr] : (conv_boo ? [:Ts_expSup, :Ts_dis, :R_dis, :Te, :scr] : [:Ts_expSup, :Ts_dis, :R_dis, :Te, :id, :scr])
	capaDim_df, grpCapaVar_df, agg_arr = getCapaToRestr(part, capaVar_df, restr, type_sym, ts_dic, r_dic, sets_dic, supTs_ntup)
	
	# add scenarios if required
	if !topFrs_boo
		capaDim_df = addScenarios(capaDim_df, sets_dic[:Ts], scr_ntup)
	else occursin("stSize", restr.cnstrType)
		capaDim_df[!,:scr] .= 0
	end

	# delete benders cases where storage variable will be fixed anyway
	if subFrs_boo && occursin("stSize", restr.cnstrType)
		rmvTs_df = combine(x -> (Ts_dis = maximum(x.Ts_dis),), groupby(capaDim_df, filter(x -> x != :Ts_dis, intCol(capaDim_df))))
		capaDim_df = antijoin(capaDim_df, rmvTs_df, on = intCol(rmvTs_df))
		if isempty(capaDim_df) return DataFrame() end
	end

	# obtain all relevant dispatch variables
	dispVar_arr = type_sym != :exc ? (!(type_sym in (:stSize,:stSizeSeason)) ? intersect(collect(keys(part.var)), info_ntup.dis) : collect(info_ntup.dis)) : [:exc]
	if type_sym != :exc
		resDis_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = supTs_ntup.lvl, Ts_dis = restr.lvlTs, R_dis = restr.lvlR) : (Ts_dis = restr.lvlTs, R_dis = restr.lvlR)
	else
		resDis_ntup = :Ts_expSup in agg_arr ? (Ts_expSup = supTs_ntup.lvl, Ts_dis = restr.lvlTs, R_from = restr.lvlR, R_to = restr.lvlR) : (Ts_dis = restr.lvlTs,  R_from = restr.lvlR, R_to = restr.lvlR)
	end

	if isempty(dispVar_arr) return DataFrame() end

	for va in dispVar_arr
		# filter dispatch variables not belonging to relevant carrier
		allVar_df = filter(r -> r.C in restr.car, part.var[va])[!,Not(:Ts_disSup)] |> (x -> sort!(x, orderDim(intCol(x))))

		# correct dispatch variables with energy carrier
		allVar_df = addEnergyCont(allVar_df, part, sets_dic)

		# get availablity (and in case of paramter of type out also efficiency since capacities refer to input capacity) parameter and add to dispatch variable
		ava_arr = matchSetParameter(allVar_df, part.par[Symbol(:ava, info_ntup.capa == :StSizeSeason ? :StSize : info_ntup.capa)], sets_dic, newCol = :ava)[!,:ava]
		
		if va != :exc
			if type_sym in (:convOut, :stOut)
				ava_arr = matchSetParameter(allVar_df, part.par[type_sym == :convOut ? :effConv : :effStOut], sets_dic, newCol = :eff)[!,:eff] .* ava_arr
			end
			allVar_df[!,:var] = @expression(optModel, allVar_df[!,:var] .* 1 ./ ava_arr)
		else
			allVar_df[!,:var] = allVar_df[!,:var] .* 1 ./ ava_arr
			if !part.dir allVar_df = flipExc(allVar_df) end
		end

		# convert dispatch variables to energy units (expect for stSize since these are already provided in energy units) 
		if !(type_sym in (:stSize,:stSizeSeason))
			allVar_df[!,:var]  = allVar_df[!,:var] .* getEnergyFac(allVar_df[!,:Ts_dis], supTs_ntup)
		end

		# aggregate dispatch variables
		capaDim_df[!,va] = aggUniVar(allVar_df, select(capaDim_df, intCol(capaDim_df)), agg_arr, resDis_ntup, sets_dic)
	end

	# sum dispatch variables and filter cases without any
	if size(dispVar_arr, 1) > 1
		aggCol!(capaDim_df, dispVar_arr)
		select!(capaDim_df, Not(dispVar_arr[2:end]))
	end
	rename!(capaDim_df, dispVar_arr[1] => :disp)
	capaDim_df = filter(x -> !(x.disp == AffExpr()), capaDim_df)

	# join capacity and dispatch variables to create final constraint
	grpCapaVar_df = combine(groupby(grpCapaVar_df, replace(filter(x -> x != :scr, dim_arr), :Ts_dis => :Ts_disSup)), :var => (x -> sum(x)) => :capa)
	cns_df = innerjoin(capaDim_df, grpCapaVar_df, on = intCol(grpCapaVar_df))

	# scale capacity to energy units as well
	if !(type_sym in (:stSize,:stSizeSeason)) cns_df[!,:capa] = cns_df[!,:capa] .* getEnergyFac(cns_df[!,:Ts_dis], supTs_ntup) end

	return cns_df
end

#endregion

#region # * create miscellaneous contraints

function createRatioCns!(part::AbstractModelPart, cns_dic::Dict{Symbol,cnsCont}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, anyM::anyModel)

	# creates dictionary assigning first part of parameter name to the corresponding limits enforced
	par_arr = filter(x -> any(map(y -> occursin(y, x), ["Up", "Low", "Fix"])), String.(collectKeys(keys(part.par))))
	ratioLim_arr = map(x -> map(k -> Symbol(k[1]) => Symbol(k[2][1]), filter(z -> length(z[2]) == 2, map(y -> y => split(x, y), ["Up", "Low", "Fix"])))[1], par_arr)
	parToLim_dic = Dict(y => unique(getindex.(filter(z -> z[2] == y, ratioLim_arr), 1)) for y in unique(getindex.(ratioLim_arr, 2)))

	ratioVar_dic = Dict(:stInToConvCapa => ((:capaConv, :capaStIn), ), :stOutToStInCapa => ((:capaStIn, :capaStOut), ), :sizeToStOutCapa => ((:capaStOut, :capaStSize), ), 
							:stInToConvExp => ((:expConv, :expStIn), ), :stOutToStInExp => ((:expStIn, :expStOut), ), :sizeToStOutExp => ((:expStIn, :expStSize), ), 	
								:flhConv => ((:capaConv, :convIn), ), :flhStIn => ((:capaStIn, :stIn), ), :flhExc => ((:capaExc, :exc), ), :flhStOut => ((:capaStOut, :stOut), ), 
									:cycStIn => ((:capaStSize, :stIn), ), :cycStOut => ((:capaStSize, :stOut),))

	va_dic = Dict(:stIn => (:stExtIn, :stIntIn), :stOut => (:stExtOut, :stIntOut), :convIn => (:use, :stIntOut), :convOut => (:gen, :stIntIn))

	# loop over all variables that are subject to any type of limit (except emissions)
	signLim_dic = Dict(:Up => :greater, :Low => :smaller, :Fix => :equal)

	# loop over parameters for conversion and exchange ratios
	if isempty(anyM.subPro) || anyM.subPro != (0,0)
		for par in filter(x -> occursin("ratio", string(x)), collectKeys(keys(parToLim_dic)))
			for lim in parToLim_dic[par]

				ratioType_sym = par == :ratioConvOut ? :convOut : :convIn

				# obtain variable name and parameter data
				if par != :ratioExc
					cns_df = rename(copy(part.par[Symbol(par, lim)].data), :val => :ratio)
				else	
					cns_df = select(rename(matchExcParameter(Symbol(par, lim), part.var[:exc], part, anyM.sets), :val => :ratio), Not([:var]))
				end

				if isempty(cns_df) continue end

				# loops over different carriers with a ratio defined and creates corresponding constraint
				grpCns_gdf = groupby(cns_df, [:C])	
				allCns_arr = Array{DataFrame}(undef, length(grpCns_gdf))
				
				for (idx, subCns) in enumerate(grpCns_gdf)

					subCns_df = DataFrame(subCns)
					sort!(subCns_df, orderDim(intCol(subCns_df)))

					# get columns being aggregated
					agg_arr = filter(r -> r != (par != :ratioExc ? :Te : :Exc) && (part.type == :emerging || r != :Ts_expSup), intCol(subCns_df))

					# writes tuple with names of search columns and respective level they are being aggregated on
					srcSym_tup = tuple(orderDim(intersect(intCol(subCns_df), vcat(part.type == :emerging ? [:Ts_expSup] : Symbol[], [:Ts_dis, :R_dis, :R_from, :R_to])))...)
					srcLvl_tup = vcat(part.type == :emerging ? [anyM.supTs.lvl] : Int[], [anyM.sets[:Ts].nodes[subCns_df[1,:Ts_dis]].lvl], par != :ratioExc ? [anyM.sets[:R].nodes[subCns_df[1,:R_dis]].lvl] : anyM.sets[:R].nodes[subCns_df[1,:R_from]].lvl |> (y -> [y,y]) )
					srcRes_ntup = NamedTuple{srcSym_tup}(tuple(srcLvl_tup...))

					# get variables
					relVar_df = vcat(map(x -> select(part.var[x], vcat(intCol(subCns_df), [:var])), par != :ratioExc ? intersect(keys(part.carrier), va_dic[ratioType_sym]) : [:exc])...)
					relVar_df[!,:var] .= relVar_df[!,:var] .* getEnergyFac(relVar_df[!,:Ts_dis], anyM.supTs) # convert to energy units

					if :M in namesSym(subCns_df) # aggregated dispatch variables, if a mode is specified somewhere, mode dependant and non-mode dependant balances have to be aggregated separately
						# find cases where ratio constraint is mode dependant
						srcResM_ntup = (; zip(tuple(:M, keys(srcRes_ntup)...), tuple(1, values(srcRes_ntup)...))...)
						srcResNoM_ntup = (; zip(tuple(:M, keys(srcRes_ntup)...), tuple(0, values(srcRes_ntup)...))...)
						m_arr = findall(0 .!= subCns_df[!,:M])
						noM_arr = setdiff(1:size(subCns_df, 1), m_arr)
						# aggregate variables with defined ratio
						subCns_df[!,:ratioVar] = map(x -> AffExpr(), 1:size(subCns_df, 1))
						subCns_df[m_arr,:ratioVar] = aggUniVar(relVar_df, select(subCns_df[m_arr,:], intCol(subCns_df)), agg_arr, srcResM_ntup, anyM.sets)
						subCns_df[noM_arr,:ratioVar] = aggUniVar(relVar_df, select(subCns_df[noM_arr,:], intCol(subCns_df)), agg_arr, srcResNoM_ntup, anyM.sets)
						# aggregate all variables
						subCns_df[!,:allVar] = map(x -> AffExpr(), 1:size(subCns_df, 1))
						subCns_df[m_arr,:allVar] =	aggUniVar(relVar_df, select(subCns_df[m_arr,:], intCol(subCns_df)), filter(x -> x != :C, agg_arr), srcResM_ntup, anyM.sets)
						subCns_df[noM_arr,:allVar] =	aggUniVar(relVar_df, select(subCns_df[noM_arr,:], intCol(subCns_df)), filter(x -> x != :C, agg_arr), srcResNoM_ntup, anyM.sets)
					else
						subCns_df[!,:ratioVar] = aggUniVar(relVar_df, select(subCns_df, intCol(subCns_df)), agg_arr, srcRes_ntup, anyM.sets)
						subCns_df[!,:allVar] =	aggUniVar(relVar_df, select(subCns_df, intCol(subCns_df)), filter(x -> x != :C, agg_arr), srcRes_ntup, anyM.sets)
					end

					# create corresponding constraint
					subCns_df[!,:cnsExpr] = @expression(anyM.optModel, subCns_df[!,:allVar] .* subCns_df[!,:ratio] .- subCns_df[!,:ratioVar])
					
					allCns_arr[idx] = subCns_df
				end
				cns_df = vcat(allCns_arr...)
				cns_dic[Symbol(par,lim)] = cnsCont(orderDf(cns_df[!, [intCol(cns_df)..., :cnsExpr]]), signLim_dic[lim])
			end
		end
	end

	# loops over all other parameters (ratios on storage capacity/expansion, flh, cycling)
	for par in filter(x -> !occursin("ratio", string(x)), collectKeys(keys(parToLim_dic)))

		capaRatio_boo = par in (:stInToConvCapa, :stOutToStInCapa, :sizeToStOutCapa, :stInToConvExp, :stOutToStInExp, :sizeToStOutExp)

		# controls variables ratio is applied
		limVa_arr = ratioVar_dic[par]

		# loops over variables limits are enforced on
		for limVa in limVa_arr, lim in parToLim_dic[par]
			# skip lower and upper bound on storage capacites for subproblems (already in top problem)
			if lim in (:Low, :Up) && par in capaRatio_boo && (isempty(anyM.subPro) || anyM.subPro != (0,0)) continue end

			# get variables for denominator
			if limVa[1] in keys(part.var)
				cns_df = copy(part.var[limVa[1]])
			else
				continue
			end

			# adjustments for flh and cycling restrictions
			if !capaRatio_boo
				# aggregate to dispatch regions
				if limVa[1] != :capaExc
					cns_df[!,:R_dis] = map(x -> r_dic[x, part.balLvl.ref[2]][1], cns_df[!,:R_exp])
					select!(cns_df, Not([:R_exp]))
				end

				cns_df = combine(groupby(cns_df, intCol(cns_df)), :var => (x -> sum(x)) => :var)
				# extend to scenarios
				cns_df = addScenarios(cns_df, anyM.sets[:Ts], anyM.scr, anyM.supTs)
				# scale to energy units
				cns_df[!,:var] .= cns_df[!,:var] .* getEnergyFac(cns_df[!,:Ts_dis], anyM.supTs) 
			end

			# matches variables with parameters denominator
			if limVa[1] != :capaExc
				cns_df = rename(matchSetParameter(cns_df, part.par[Symbol(par,lim)], anyM.sets), :var => :denom)
			else
				cns_df = rename(matchExcParameter(Symbol(par, lim), cns_df, part, anyM.sets, part.dir), :var => :denom)
			end
	
			# get variables for numerator
			rlvTop_arr =  intersect(keys(part.var), limVa[2] in keys(va_dic) ? intersect(keys(part.carrier), va_dic[limVa[2]]) : (limVa[2],))

			# use out instead of in for flh of conversion technologies, if technology has no conversion input
			if isempty(rlvTop_arr) && par == :flhConv
				rlvTop_arr = intersect(keys(part.carrier), (:stIntIn, :gen))
			elseif isempty(rlvTop_arr)
				continue
			end

			top_df = vcat(map(x -> part.var[x], collect(rlvTop_arr))...)

			# rename column for aggregation
			if !capaRatio_boo cns_df = rename(cns_df, :Ts_disSup => :Ts_dis) end
			if capaRatio_boo && string(limVa[1])[1:3] != "exp" && :Ts_exp in intCol(cns_df) select!(cns_df, Not([:Ts_exp])) end

			# connect denominator and numerator
			cns_df[!,:num] =  aggDivVar(top_df, cns_df, tuple(intCol(cns_df)...), anyM.sets)

			# name column back again
			if !capaRatio_boo cns_df = rename(cns_df, :Ts_dis => :Ts_disSup) end

			# create constraint
			cns_df[!,:cnsExpr] = @expression(anyM.optModel, cns_df[!,:val] .* cns_df[!,:denom] .- cns_df[!,:num])

			if isempty(cns_df) continue end

			cns_dic[Symbol(par,lim)] = cnsCont(orderDf(cns_df[!,[intCol(cns_df)...,:cnsExpr]]), signLim_dic[lim])
		end
	end

end

#endregion