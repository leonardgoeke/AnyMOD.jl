# ! create variables and equations for costs
function createCost!(partCost::OthPart, anyM::anyModel)

	parObj_arr = collectKeys(keys(partCost.par))
	sysSym_dic = Dict(:tech => collect(keys(anyM.parts.tech)), :exc => collect(keys(anyM.parts.exc)))
	reachEnd_boo = false

	# computes discount factors from discount rate provided and saves them as new parameter elements
	computeDisFac!(partCost, anyM)

	#region # * expansion related costs

	if isempty(anyM.subPro) || anyM.subPro == (0, 0)

		# ! add elements for expansion costs of technologies
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

			part_sym = va == :Exc ? :exc : :tech
			var_sym = Symbol(:exp, va)
			costPar_sym = Symbol(:costExp, va)

			# ! get all expansion variables
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
			allExp_df = getAllVariables(var_sym, anyM)
			if !isempty(allExp_df)
				allExp_df = rename(allExp_df, :var => :exp)
			else
				continue
			end
			
			# ! determine relevant lifetime (use economic lifetime where defined, else use technical lifetime)
			
			# add economic lifetime to table where it is defined
			if Symbol(:lifeEco, va) in parObj_arr || Symbol(:lifeEco, va, :Dir) in parObj_arr
				if va != :Exc
					ecoLife_df = matchSetParameter(allExp_df, partCost.par[Symbol(:lifeEco, va)], anyM.sets, newCol = :life)
				else
					ecoLife_df = rename(matchCostExcParameter(Symbol(:lifeEco, va), allExp_df, anyM), :val => :life)
				end
				noEcoLife_df = antijoin(allExp_df, ecoLife_df, on = intCol(allExp_df))
				noEcoLife_df[!,:life] .= nothing
				allExp_df = vcat(ecoLife_df, noEcoLife_df)
			else
				allExp_df[!,:life] .= nothing
			end

			sysFilt_arr = (va == :Exc ? :Exc : :Te) |> (z -> filter(y ->  sysInt(y, anyM.sets[z]) in allExp_df[:,z], sysSym_dic[part_sym]))

			# add technical lifetime to table
			parObj_dic = Dict{Symbol,ParElement}()

			for par in (va != :Exc ? (Symbol(:life, va),) : (:lifeExcDir, :lifeExc))
				allPar_arr = map(w -> isempty(w) ? DataFrame(val = [va != :Exc ? 20.0 : 50.0]) : w, map(x -> getfield(anyM.parts, part_sym)[x].par[par].data, sysFilt_arr))
				union(intCol.(allPar_arr)...) |> (z -> map(x -> map(y -> insertcols!(x, 1, (y => fill(0, size(x, 1)))) , setdiff(z, intCol(x))), allPar_arr))
				parObj_dic[par] = copy(getfield(anyM.parts, part_sym)[sysFilt_arr[1]].par[par], unique(vcat(allPar_arr...)))
			end

			# use economic lifetime where it is defined, else use technical lifetime
			techLife_df = select(filter(x -> isnothing(x.life), allExp_df), Not([:life]))
			if va != :Exc
				techLife_df = matchSetParameter(techLife_df, parObj_dic[Symbol(:life, va)], anyM.sets, newCol = :life)
			else
				techLife_df = rename(matchCostExcParameter(costPar_sym, techLife_df, anyM, nothing, parObj_dic[:lifeExc], parObj_dic[:lifeExcDir]), :val => :life)
			end
			allExp_df = vcat(techLife_df, filter(x -> !isnothing(x.life), allExp_df))

			# ! gets expansion costs and compute annuity
			if va != :Exc
				allExp_df = matchSetParameter(allExp_df, partCost.par[costPar_sym], anyM.sets, newCol = :cost)
			else
				allExp_df = rename(matchCostExcParameter(costPar_sym, allExp_df, anyM), :val => :cost)
			end

			if isempty(allExp_df) continue end

			# computes annuity based on discount factors
			allExp_df = computeAnn(va, :Exp, allExp_df, anyM)

			# adds discount factor and computes cost expression
			allExp_df = matchSetParameter(allExp_df, partCost.par[va != :Exc ? :disFac : :disFacExc], anyM.sets, newCol = :disFac)

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allExp_df = rename(combine(x -> (expr = sum(x.disFac .* x.exp .* x.costAnn),), groupby(allExp_df, va != :Exc ? [:Ts_disSup, :R_exp, :Te] : [:Ts_disSup, :R_from, :R_to, :Exc])), :Ts_disSup => :Ts_exp)
			transferCostEle!(allExp_df, partCost, costPar_sym, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costCapa, anyM.options.checkRng, anyM)
			reachEnd_boo = true
		end

		# ! costs for filling stochastic inter-annual storage
		if :costStStartLvl in keys(partCost.par)
			# get level variables
			startStLvl_df = getAllVariables(:startStLvl, anyM)
			if !isempty(startStLvl_df)
				# match variables with cost and discount factor
				startStLvl_df = matchSetParameter(startStLvl_df, partCost.par[:costStStartLvl], anyM.sets, newCol = :cost)
				startStLvl_df = matchSetParameter(rename(startStLvl_df,:R_dis => :R_exp), partCost.par[:disFac], anyM.sets, newCol = :disFac)
				# create cost expression
				startStLvl_df = combine(x -> (expr = sum(x.disFac .* x.var .* x.cost) ./ 1000,), groupby(startStLvl_df, [:Ts_disSup, :R_exp, :Te]))
				transferCostEle!(startStLvl_df, partCost, :costStartStLvl, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costCapa, anyM.options.checkRng, anyM)
			end
			reachEnd_boo = true
		end
		
		if reachEnd_boo 
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for expansion costs")
			reachEnd_boo = false
		end

		# ! add elements for operational costs of technologies
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

			var_sym = Symbol(:capa, va)
			costPar_sym = Symbol(:costOpr, va)

			# ! compute operating costs
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
	        allCapa_df = getAllVariables(var_sym, anyM)
	        if isempty(allCapa_df)
	            continue
	        else
	            allCapa_df = rename(allCapa_df, :var => :capa)
	        end

			# joins costs and discount factors to create cost expression
			if va != :Exc
				allCapa_df = matchSetParameter(allCapa_df, partCost.par[costPar_sym], anyM.sets, newCol = :costOpr)
			else
				allCapa_df = rename(matchCostExcParameter(costPar_sym, allCapa_df, anyM), :val => :costOpr)
			end
			allCapa_df = matchSetParameter(allCapa_df, partCost.par[va != :Exc ? :disFac : :disFacExc], anyM.sets, newCol = :disFac)

			if isempty(allCapa_df) continue end

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allCapa_df = combine(x -> (expr = sum(x.disFac .* x.capa .* x.costOpr),), groupby(allCapa_df, va != :Exc ? [:Ts_disSup, :R_exp, :Te] : [:Ts_disSup, :R_from, :R_to, :Exc]))
			transferCostEle!(allCapa_df, partCost, costPar_sym, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costCapa, anyM.options.checkRng, anyM)
			reachEnd_boo = true
		end

		if reachEnd_boo 
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for capacity costs")
			reachEnd_boo = false
		end
		
		# ! add elements for retrofitting costs
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)
			
			var_sym = Symbol(:retro, va)
			costPar_sym = Symbol(:costRetro, va)
			part_sym = va == :Exc ? :exc : :tech 

			# ! get all retrofitting variables
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
			allRetro_df = filter(x -> x.start, getAllVariables(var_sym, anyM))
			if !isempty(allRetro_df)
				allRetro_df = rename(allRetro_df, :var => :retro)
			else
				continue
			end

			# ! determine relevant lifetime (use economic lifetime where defined, else use technical lifetime, remember technical lifetime also depends on remaining lifetime of target)

			# add economic lifetime to table where it is defined
			if Symbol(:lifeRetroEco, va) in parObj_arr  || Symbol(:lifeRetroEco, va, :Dir) in parObj_arr
				if va != :Exc
					ecoLife_df = matchSetParameter(allRetro_df, partCost.par[Symbol(:lifeRetroEco, va)], anyM.sets, newCol = :life)
				else
					ecoLife_df = rename(matchCostExcParameter(Symbol(:lifeRetroEco, va), allRetro_df, anyM), :val => :life)
				end
				noEcoLife_df = antijoin(allRetro_df, ecoLife_df, on = intCol(allRetro_df))
				noEcoLife_df[!,:life] .= nothing
				allRetro_df = vcat(ecoLife_df, noEcoLife_df)
			else
				allRetro_df[!,:life] .= nothing
			end

			# collects all technical lifetimes of start system and credit factors to compute technical lifetime of retrofitting
			sysFilt_arr = (va == :Exc ? :Exc : :Te) |> (z -> filter(y ->  sysInt(y, anyM.sets[z]) in allRetro_df[:,Symbol(z,:_j)], sysSym_dic[part_sym]))
			
			parObj_dic = Dict{Symbol,ParElement}()

			for par in (va != :Exc ? (Symbol(:creditRetro, va), Symbol(:lifeRetro, va), Symbol(:life, va)) : (Symbol(:creditRetro, va), Symbol(:lifeRetro, va), Symbol(:life, va), Symbol(:life, va, :Dir)))
				allPar_arr = map(w -> isempty(w) ? DataFrame(val = [par == Symbol(:creditRetro, va) ? 1.0 : (va != :Exc ? 20.0 : 50.0)]) : w, map(x -> getfield(anyM.parts, part_sym)[x].par[par].data, sysFilt_arr))
				union(intCol.(allPar_arr)...) |> (z -> map(x -> map(y -> insertcols!(x, 1, (y => fill(0, size(x, 1)))) , setdiff(z, intCol(x)) ), allPar_arr))
				parObj_dic[par] = copy(getfield(anyM.parts, part_sym)[sysFilt_arr[1]].par[par], unique(vcat(allPar_arr...)))
			end

			# match technical lifetime of start system, requires to rename columns
			startCol_arr = va == :Conv ? [:Te, :Ts_expSup, :R_exp] : (va in (:StIn, :StOut, :StSize) ? [:Te, :Ts_expSup, :R_exp, :id] : [:Exc, :Ts_expSup, :R_from, :R_to])

			if va != :Exc
				techLife_df = matchSetParameter(rename(allRetro_df, Symbol.(startCol_arr, "_i") .=> startCol_arr), parObj_dic[Symbol(:life, va)], anyM.sets, newCol = :lifeStart)
			else
				techLife_df = rename(matchCostExcParameter(:lifeExc, rename(allRetro_df, Symbol.(startCol_arr, "_i") .=> startCol_arr), anyM, nothing, parObj_dic[:lifeExc], parObj_dic[:lifeExcDir]), :val => :lifeStart)
			end
			rename!(techLife_df, startCol_arr .=> Symbol.(startCol_arr, "_i"))

			# compute technical lifetime of retrofitting
			techLife_df = matchSetParameter(techLife_df, parObj_dic[Symbol(:creditRetro, va)], anyM.sets, newCol = :credit)
			techLife_df = matchSetParameter(techLife_df, parObj_dic[Symbol(:lifeRetro, va)], anyM.sets, newCol = :lifeRetroSys)
			techLife_df[!,:life] = map(x -> ((x.Ts_disSup_last-x.Ts_retro) * anyM.options.shortExp + x.lifeStart % anyM.options.shortExp) * x.credit + x.lifeRetroSys, eachrow(techLife_df))
			
			# uses economic life where one is defined, otherwise use technical lifetime
			allRetro_df = vcat(select(techLife_df, Not([:lifeStart, :credit, :lifeRetroSys])), filter(x -> !isnothing(x.life), allRetro_df))

			# ! gets expansion costs and compute annuity

			# gets retrofitting costs and factor (since we used start system here to obtain paramter for technical lifetime, but cost relate to target system)
			allRetro_df = matchSetParameter(allRetro_df, partCost.par[Symbol(:facRetro, va)], anyM.sets, newCol = :fac)
			allRetro_df = matchSetParameter(allRetro_df, partCost.par[costPar_sym], anyM.sets, newCol = :cost)

			if isempty(allRetro_df) continue end

			# get discount rate and computes annuity
			allRetro_df = computeAnn(va, :Retro, allRetro_df, anyM)

			# adds discount factor and computes cost expression
			rename!(allRetro_df, Symbol.(startCol_arr, "_i") .=> startCol_arr)
			allRetro_df = matchSetParameter(allRetro_df, partCost.par[va != :Exc ? :disFac : :disFacExc], anyM.sets, newCol = :disFac)

			# switches dimension so cost relate to technology
			rename!(allRetro_df, startCol_arr .=> Symbol.(startCol_arr, "_i"))
			rename!(allRetro_df, Symbol.(startCol_arr, "_j") .=> startCol_arr)

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allExp_df = combine(x -> (expr = sum(x.disFac .* x.retro .* x.fac .* x.costAnn),), groupby(allRetro_df, va != :Exc ? [:Ts_disSup, :R_exp, :Te] : [:Ts_disSup, :R_from, :R_to, :Exc]))
			transferCostEle!(allExp_df, partCost, costPar_sym, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costCapa, anyM.options.checkRng, anyM)
			reachEnd_boo = true
		end

		if reachEnd_boo 
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for retrofitting costs")
			reachEnd_boo = false
		end

	end
	
	# costs of missing capacity

	# obtain missing capacity variables
	if !isempty(anyM.subPro) && anyM.subPro != (0, 0) # for sub-problems missing capacity variables only server to avoid infeasibilities with storage technologies and are stored in technology parts
		missCapa_df = filter(x -> :missCapa in keys(anyM.parts.tech[x].var), collect(keys(anyM.parts.tech))) |> (z -> isempty(z) ? DataFrame() : vcat(map(y -> anyM.parts.tech[y].var[:missCapa], z)...))
	else
		missCapa_df = :missCapa in keys(anyM.parts.bal.var) ? anyM.parts.bal.var[:missCapa] : DataFrame()
	end

	if !isempty(missCapa_df)
		# compute discounted costs
		allVar_df = rename(matchSetParameter(missCapa_df, anyM.parts.bal.par[:costMissCapa], anyM.sets, newCol = :cost), :var => :missCapa)
		allVar_df = matchSetParameter(rename(allVar_df, :R_dis => :R_exp), partCost.par[:disFac], anyM.sets, newCol = :disFac)
		if !isempty(anyM.subPro) && anyM.subPro != (0, 0)  
			# add scenario probability
			allVar_df[!,:scr] .= anyM.subPro[2]
			allVar_df[!,:missCapa] = allVar_df[!,:missCapa] .* map(x -> anyM.scr.scrProb[(x.Ts_disSup, x.scr)], eachrow(allVar_df))
		end
		# groups cost expressions, scales groups expression and creates a variables for each grouped entry
		allVar_df = rename(combine(x -> (expr = sum(x.disFac .* x.missCapa .* x.cost),), groupby(allVar_df, [:Ts_disSup, :R_exp, :C])), :R_exp => :R_dis)
		transferCostEle!(allVar_df, partCost, :costMissCapa, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costCapa, anyM.options.checkRng, anyM, 0.0)
	end

	produceMessage(anyM.options, anyM.report, 2, " - Created all variables and constraints for expansion related costs")

	#endregion

	#region # * dispatch related costs

	if isempty(anyM.subPro) || anyM.subPro != (0, 0)
		
		# ! infeasibility costs of emissions
		if :emissionInf in keys(anyM.parts.lim.var)
			# matches infeasibility variables with costs
			allVar_df = matchSetParameter(anyM.parts.lim.var[:emissionInf], anyM.parts.lim.par[:emissionInf], anyM.sets, newCol = :cost)
			# adds discount factor
			allVar_df = (:R_dis in intCol(allVar_df) ? rename(allVar_df, :R_dis => :R_exp) : allVar_df) |> (z -> :Ts_dis in intCol(z) ? rename(z, :Ts_dis => :Ts_disSup) : z)
			allVar_df = matchSetParameter(allVar_df, partCost.par[:disFac], anyM.sets, newCol = :disFac)
			# cretae cost expression
			allVar_df = combine(x -> (expr = sum(x.disFac .* x.var .* x.cost),), groupby(allVar_df, intersect([:Ts_disSup, :R_exp], intCol(allVar_df))))
			transferCostEle!(allVar_df, partCost, :costEmInf, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costDisp, anyM.options.checkRng, anyM, 0.0)
		end

		# ! add elements for variable costs of systems
		for va in (:use, :gen, :stIn, :stOut, :in, :out, :exc, :stLvlInfeasIn, :stLvlInfeasOut)

			costPar_sym = string(va) |> (x -> va in (:stLvlInfeasIn, :stLvlInfeasOut) ? :costStLvlLss : Symbol(:costVar, uppercase(x[1]), x[2:end]))

			if !(costPar_sym in parObj_arr) continue end

			# obtain all variables
			allDisp_df = getAllVariables(va, anyM)
			if !isempty(allDisp_df)
				allDisp_df = rename(allDisp_df, :var => :disp)
			else
				continue
			end

			# special case for variable costs of exchange (direct and symmetric values need to be considered both) and of use (emission price needs to be considered)
			if va != :exc
				allDisp_df = matchSetParameter(allDisp_df, anyM.parts.cost.par[costPar_sym], anyM.sets, newCol = :costVar)
			else
				allDisp_df = rename(matchCostExcParameter(costPar_sym, allDisp_df, anyM, true), :val => :costVar)
			end

			if isempty(allDisp_df) continue end

			# add scenario probability
			allDisp_df[!,:disp] = allDisp_df[!,:disp] .* map(x -> getScrProb(x.Ts_dis, x.scr, anyM.sets[:Ts], anyM.scr), eachrow(allDisp_df))

			# renames dispatch regions to enable join with discount factors
			allDisp_df[!,:disFac] = matchSetParameter(va != :exc ? rename(allDisp_df, :R_dis => :R_exp) : allDisp_df, partCost.par[va != :exc ? :disFac : :disFacExc], anyM.sets)[!,:val]

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allDisp_df = combine(x -> (expr = sum(x.disFac .* x.disp .* x.costVar) ./ 1000.0,), groupby(allDisp_df, va != :exc ? [:Ts_disSup, :R_dis, :Te] : [:Ts_disSup, :R_from, :R_to, :C]))
			costVar_sym = va in (:stLvlInfeasIn, :stLvlInfeasOut) ? (va == :stLvlInfeasIn ? Symbol(costPar_sym, :In) : Symbol(costPar_sym, :Out)) : costPar_sym
			transferCostEle!(allDisp_df, partCost, costVar_sym, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costDisp, anyM.options.checkRng, anyM)
			reachEnd_boo = true
		end
		
		if reachEnd_boo 
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for variables costs")
			reachEnd_boo = false
		end

		# ! add elements for emission costs
		if :emissionPrc in parObj_arr && :emissionFac in keys(anyM.parts.lim.par)
			# get emission variables, prices and discount factor
			emVar_df = matchSetParameter(getAllVariables(:emission, anyM), partCost.par[:emissionPrc], anyM.sets, newCol = :emPrc)
			emVar_df = matchSetParameter(rename(emVar_df, :R_dis => :R_exp), partCost.par[:disFac], anyM.sets, newCol = :disFac)
			# add scenario probability
			emVar_df[!,:var] = emVar_df[!,:var] .* map(x -> getScrProb(x.Ts_dis, x.scr, anyM.sets[:Ts], anyM.scr), eachrow(emVar_df))
			# groups cost expressions scales groups expression and creates a variables for each grouped entry
			emVar_df = combine(x -> (expr = sum(x.disFac .* x[!,:var] .* x.emPrc),), groupby(emVar_df, [:Ts_disSup, :R_exp, :C]))
			if !isempty(emVar_df)
				transferCostEle!(rename(emVar_df, :R_exp => :R_dis), partCost, :costEm, anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costDisp, anyM.options.checkRng, anyM, 0.0)
			end
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for emission costs")
		end

		# ! add elements for curtailment and loss of load costs of energy carriers
		varToPar_dic = Dict(:crt => :costCrt, :lss => :costLss, :trdBuy => :trdBuyPrc, :trdSell => :trdSellPrc)
		for va in [:crt, :lss, :trdBuy, :trdSell]
			if va in keys(anyM.parts.bal.var)
				cost_sym = varToPar_dic[va]
				# compute discounted curtailment costs
				allVar_df = rename(matchSetParameter(anyM.parts.bal.var[va], anyM.parts.bal.par[cost_sym], anyM.sets, newCol = :cost), :var => va)
				allVar_df = matchSetParameter(rename(allVar_df, :R_dis => :R_exp), partCost.par[:disFac], anyM.sets, newCol = :disFac)
				# add scenario probability and scaling factor
				allVar_df[!,va] = allVar_df[!,va] .* map(x -> getScrProb(x.Ts_dis, x.scr, anyM.sets[:Ts], anyM.scr), eachrow(allVar_df)) .* getEnergyFac(allVar_df[!,:Ts_dis], anyM.supTs)
				# groups cost expressions scales groups expression and creates a variables for each grouped entry
				allVar_df = rename(combine(x -> (expr = sum(x.disFac .* x[!,va] .* x.cost) ./ 1000.0,), groupby(allVar_df, [:Ts_disSup, :R_exp, :C])), :R_exp => :R_dis)
				transferCostEle!(allVar_df, partCost, va in (:crt, :lls) ? cost_sym : Symbol(cost_sym), anyM.optModel, anyM.lock, anyM.sets, anyM.options.coefRng, anyM.options.scaFac.costDisp, anyM.options.checkRng, anyM, (va == :trdSell ? NaN : 0.0))
				reachEnd_boo = true
			end
		end

		if reachEnd_boo 
			produceMessage(anyM.options, anyM.report, 3, " - Created variables and constraints for curtailment and trade costs")
			reachEnd_boo = false
		end
	end

	produceMessage(anyM.options, anyM.report, 2, " - Created all variables and constraints for dispatch related costs")
	#endregion

	produceMessage(anyM.options, anyM.report, 1, " - Created all variables and constraints for costs")
end

# ! computes annuity of expansion or retrofitting costs
function computeAnn(va_sym::Symbol, type_sym::Symbol, allData_df::DataFrame, anyM::anyModel)

	# ! uses tech specific discount rate and fall back on general discount rate as default
	if Symbol(:rate, type_sym, va_sym) in keys(anyM.parts.cost.par)
		if va_sym != :Exc || type_sym == :Retro
			techRate_df = matchSetParameter(allData_df, anyM.parts.cost.par[Symbol(:rate, type_sym, va_sym)], anyM.sets, newCol = :rate)
		else
			techRate_df = rename(matchCostExcParameter(Symbol(:rate, type_sym, va_sym), allData_df, anyM), :val => :rate)
		end
	else
		techRate_df = filter(x -> false, allData_df); techRate_df[!,:rate] .= Float64[]
	end
	# ! obtains general discount rate
	
	# renames columns to match general discount rate for retrofitting
	generRate_df = rename(antijoin(allData_df, techRate_df, on = intCol(techRate_df)), (type_sym == :Exp ? :Ts_expSup : :Ts_retro) => :Ts_disSup, :Ts_disSup => (type_sym == :Exp ? :Ts_expSup : :Ts_expSup_temp))
	
	if type_sym == :Retro
		startCol_arr = va_sym == :Conv ? [:Te, :Ts_expSup, :R_exp] : (va_sym in (:StIn, :StOut, :StSize) ? [:Te, :Ts_expSup, :R_exp] : [:Exc, :Ts_expSup, :R_from, :R_to])
		rename!(generRate_df, Symbol.(startCol_arr, "_i") .=> startCol_arr)
	end

	# match discount rates
	if va_sym != :Exc
		generRate_df = matchSetParameter(generRate_df, anyM.parts.cost.par[:rateDisc], anyM.sets, newCol = :rate)
	else
		rateB_arr = matchSetParameter(rename(generRate_df, :R_from => :R_exp), anyM.parts.cost.par[:rateDisc], anyM.sets, newCol = :rateA)[!,:rateA]
		rateA_arr = matchSetParameter(rename(generRate_df, :R_to => :R_exp), anyM.parts.cost.par[:rateDisc], anyM.sets, newCol = :rateB)[!,:rateB]
		generRate_df[!,:rate] = 0.5 .* (rateA_arr .+ rateB_arr)
	end

	# switch column names back again and merge dataframe for different rates

	if type_sym == :Retro rename!(generRate_df, startCol_arr .=> Symbol.(startCol_arr, "_i")) end
	allData_df = vcat(techRate_df, rename(generRate_df, :Ts_disSup => (type_sym == :Exp ? :Ts_expSup : :Ts_retro), (type_sym == :Exp ? :Ts_expSup : :Ts_expSup_temp) => :Ts_disSup))

	# ! compute annuity costs
	allData_df[!,:costAnn] = map(x -> x.cost * (x.rate == 0.0 ? 1/x.life : (x.rate * (1 + x.rate)^x.life) / ((1 + x.rate)^x.life-1)), eachrow(allData_df))
	select!(allData_df, Not([:cost, :life, :rate]))
	allData_df = flatten(allData_df, :Ts_disSup)

	return allData_df
end

# ! creates new parameter objects for discount factors from discount rates provided
function computeDisFac!(partCost::OthPart, anyM::anyModel)

	# ! discount factor for technologies
	rExp_arr = union(map(x -> getfield.(getNodesLvl(anyM.sets[:R], x), :idx), unique(getfield.(values(anyM.cInfo), :rExp)))...)
	discR_df = matchSetParameter(flatten(flatten(DataFrame(Ts_disSup = anyM.supTs.step, R_exp = rExp_arr), :Ts_disSup), :R_exp), partCost.par[:rateDisc], anyM.sets)

	# factor to correct discount factor in case of distributed model generation
	if !isempty(anyM.subPro) && anyM.subPro != (0, 0) 
		factY_int= 1 + getAncestors(anyM.subPro[1], anyM.sets[:Ts], :int, anyM.supTs.lvl)[end] - minimum(getfield.(getNodesLvl(anyM.sets[:Ts], anyM.supTs.lvl), :idx))
	else 
		factY_int = 1
	end

	discR_df[!,:disFac] = 1 ./ (1 .+ discR_df[!,:val]).^(anyM.options.shortExp * factY_int)
	discR_df[!,:disFac] = map(x -> filter(y -> y < x.Ts_disSup, collect(anyM.supTs.step)) |> (z -> prod(filter(y -> y.R_exp == x.R_exp && y.Ts_disSup in z, discR_df)[!,:disFac])*x.disFac), eachrow(discR_df))
	select!(discR_df, Not(:val))

	discPar_obj = copy(partCost.par[:rateDisc], rename(discR_df, :disFac => :val))
	discPar_obj.name = :discFac
	discPar_obj.defVal = nothing
	partCost.par[:disFac] = discPar_obj

	# ! discount factor for exchange (average of from and to region)
	discRExc_df = rename(copy(discR_df), :R_exp => :R_from, :disFac => :disFacFrom)
	discRExc_df[!,:R_to] .= [unique(discRExc_df[!,:R_from])]
	discRExc_df = flatten(discRExc_df, :R_to)

	discRExc_df = innerjoin(discRExc_df, discR_df, on = [:Ts_disSup, :R_to] .=> [:Ts_disSup, :R_exp])
	discRExc_df[!,:disFac] = (discRExc_df[!,:disFac] + discRExc_df[!,:disFacFrom]) * 0.5
	select!(discRExc_df, Not(:disFacFrom))

	discPar_obj = copy(partCost.par[:rateDisc], rename(discRExc_df, :disFac => :val))
	discPar_obj.name = :disFacExc
	discPar_obj.defVal = nothing
	discPar_obj.dim = (:Ts_dis, :R_from, :R_to)
	discPar_obj.herit = (:Ts_disSup => :up, :R_from => :up, :R_to => :up, :Ts_disSup => :avg_any, :R_from => :avg_any, :R_to => :avg_any)
	partCost.par[:disFacExc] = discPar_obj
end

# ! matches exchange variables with cost parameters, matchExcParameter cannot be applied directly, because cost parameters are stored in other model part
function matchCostExcParameter(par_sym::Symbol, data_df::DataFrame, anyM::anyModel, useDir_boo::Union{Bool, Nothing}=nothing, unDir_obj::Union{ParElement,Nothing}=nothing, dir_obj::Union{ParElement,Nothing}=nothing)

	# groups by exchange technology
	grpExc_gdf = groupby(data_df,[:Exc])
	matchData_arr = Array{DataFrame}(undef,length(grpExc_gdf))

	# loops over exchange technologies and obtains data
	for (idx, excDf) in enumerate(grpExc_gdf)	
		matchData_arr[idx] = matchExcParameter(par_sym, DataFrame(excDf), anyM.parts.cost, anyM.sets, isnothing(useDir_boo) ? anyM.parts.exc[sysSym(excDf.Exc[1], anyM.sets[:Exc])].dir : useDir_boo, unDir_obj, dir_obj)
	end

	return vcat(matchData_arr...)
end

# ! transfers provided cost dataframe into dataframe of overall objective variables and equations (and scales them)
function transferCostEle!(cost_df::DataFrame, partCost::OthPart, costPar_sym::Symbol, optModel::Model, lock_::ReentrantLock, sets_dic::Dict{Symbol,Tree},
	coefRng_tup::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Float64,Float64}}}, scaCost_fl::Float64, checkRng_ntup::NamedTuple{(:print,:all),Tuple{Bool,Bool}}, anyM::anyModel, lowBd::Float64 = 0.0)

	# create variables for cost entry and builds corresponding expression for equations controlling them
	cost_df = createVar(cost_df, string(costPar_sym), NaN, optModel, lock_, sets_dic, scaFac = scaCost_fl, lowBd = costPar_sym == :costCrt ? NaN : lowBd)
	cost_df[!,:cnsExpr] = @expression(anyM.optModel, cost_df[!,:expr] .- cost_df[!,:var])
	select!(cost_df, Not(:expr))

	# scales cost expression
	scaleCnsExpr!(cost_df, coefRng_tup, checkRng_ntup)

	# writes equations and variables
	partCost.cns[costPar_sym] = createCns(cnsCont(select(cost_df, Not(:var)), :equal), optModel, anyM.options.holdFixed)
	partCost.var[costPar_sym] = select(cost_df, Not(:cnsExpr))
end