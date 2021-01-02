# ! create variables and equations for cost objective

function createCost!(partCost::OthPart,anyM::anyModel)

	partCost = anyM.parts.cost

	parObj_arr = collectKeys(keys(partCost.par))
	sysSym_dic = Dict(:tech => collect(keys(anyM.parts.tech)), :exc => collect(keys(anyM.parts.exc)))

	# computes discount factors from discount rate provided and saves them as new parameter elements
	computeDisFac!(partCost,anyM)

	#region # * expansion related costs

	if isempty(anyM.subPro) || anyM.subPro == (0,0)

		# ! add elements for expansion costs of technologies
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

			part_sym = va == :Exc ? :exc : :tech
			var_sym = Symbol(:exp,va)
			costPar_sym = Symbol(:costExp,va)

			# ! get all expansion variables
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
			allExp_df = getAllVariables(var_sym,anyM)
			if !isempty(allExp_df)
				allExp_df = rename(allExp_df,:var => :exp)
			else
				continue
			end
			
			# ! determine relevant lifetime (use economic lifetime where defined, else use technical lifetime)
			
			# add economic lifetime to table where it is defined
			if Symbol(:lifeEco,va) in parObj_arr || Symbol(:lifeEco,va,:Dir) in parObj_arr
				if va != :Exc
					ecoLife_df = matchSetParameter(allExp_df,partCost.par[Symbol(:lifeEco,va)],anyM.sets,newCol = :life)
				else
					ecoLife_df = rename(matchCostExcParameter(Symbol(:lifeEco,va),allExp_df,anyM),:val => :life)
				end
				noEcoLife_df = antijoin(allExp_df,ecoLife_df, on = intCol(allExp_df))
				noEcoLife_df[!,:life] .= nothing
				allExp_df = vcat(ecoLife_df,noEcoLife_df)
			else
				allExp_df[!,:life] .= nothing
			end

			sysFilt_arr = (va == :Exc ? :Exc : :Te) |> (z -> filter(y ->  sysInt(y,anyM.sets[z]) in allExp_df[:,z], sysSym_dic[part_sym]))

			# add technical lifetime to table
			parObj_dic = Dict{Symbol,ParElement}()

			for par in (va != :Exc ? (Symbol(:life,va),) : (:lifeExcDir,:lifeExc))
				allPar_arr = map(w -> isempty(w) ? DataFrame(val = [va != :Exc ? 20.0 : 50.0]) : w,map(x -> getfield(anyM.parts,part_sym)[x].par[par].data, sysFilt_arr))
				union(intCol.(allPar_arr)...) |> (z -> map(x -> map(y -> insertcols!(x,1,(y => fill(0,size(x,1)))) , setdiff(z,intCol(x)) ),allPar_arr))
				parObj_dic[par] = copy(getfield(anyM.parts,part_sym)[sysFilt_arr[1]].par[par],unique(vcat(allPar_arr...)))
			end

			# use economic lifetime where it is defined, else use technical lifetime
			techLife_df = select(filter(x -> isnothing(x.life),allExp_df),Not([:life]))
			if va != :Exc
				techLife_df = matchSetParameter(techLife_df,parObj_dic[Symbol(:life,va)],anyM.sets,newCol = :life)
			else
				techLife_df = rename(matchCostExcParameter(costPar_sym,techLife_df,anyM,parObj_dic[:lifeExc],parObj_dic[:lifeExcDir]),:val => :life)
			end
			allExp_df = vcat(techLife_df,filter(x -> !isnothing(x.life),allExp_df))

			# ! gets expansion costs and compute annuity
			if va != :Exc
				allExp_df = matchSetParameter(allExp_df,partCost.par[costPar_sym],anyM.sets,newCol = :cost)
			else
				allExp_df = rename(matchCostExcParameter(costPar_sym,allExp_df,anyM),:val => :cost)
			end

			if isempty(allExp_df) continue end

			# computes annuity based on discount factors
			allExp_df = computeAnn(va,:Exp,allExp_df,anyM)

			# adds discount factor and computes cost expression
			allExp_df = matchSetParameter(allExp_df,partCost.par[va != :Exc ? :disFac : :disFacExc],anyM.sets,newCol = :disFac)

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allExp_df = rename(combine(x -> (expr = sum(x.disFac .* x.exp .* x.costAnn),) ,groupby(allExp_df,va != :Exc ? [:Ts_disSup,:R_exp,:Te] : [:Ts_disSup,:Exc])),:Ts_disSup => :Ts_exp)
			transferCostEle!(allExp_df, partCost,costPar_sym,anyM.optModel,anyM.lock,anyM.sets,anyM.options.coefRng,anyM.options.scaFac.costCapa,anyM.options.checkRng)
		end
		produceMessage(anyM.options,anyM.report, 3," - Created expression for expansion costs")

		# ! add elements for operational costs of technologies
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

			var_sym = Symbol(:capa,va)
			costPar_sym = Symbol(:costOpr,va)

			# ! compute operating costs
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
	        allCapa_df = getAllVariables(var_sym,anyM)
	        if isempty(allCapa_df)
	            continue
	        else
	            allCapa_df = rename(allCapa_df,:var => :capa)
	        end

			# joins costs and discount factors to create cost expression
			if va != :Exc
				allCapa_df = matchSetParameter(allCapa_df,partCost.par[costPar_sym],anyM.sets,newCol = :costOpr)
			else
				allCapa_df = rename(matchCostExcParameter(costPar_sym,allCapa_df,anyM),:val => :costOpr)
			end
			allCapa_df = matchSetParameter(allCapa_df,partCost.par[va != :Exc ? :disFac : :disFacExc],anyM.sets,newCol = :disFac)

			if isempty(allCapa_df) continue end

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allCapa_df = combine(x -> (expr = sum(x.disFac .* x.capa .* x.costOpr),), groupby(allCapa_df,va != :Exc ? [:Ts_disSup,:R_exp,:Te] : [:Ts_disSup,:Exc]))
			transferCostEle!(allCapa_df, partCost,costPar_sym,anyM.optModel,anyM.lock,anyM.sets,anyM.options.coefRng,anyM.options.scaFac.costCapa,anyM.options.checkRng)
		end
		produceMessage(anyM.options,anyM.report, 3," - Created expression for capacity costs")

		# ! add elements for retrofitting costs
		for va in (:Conv, :StIn, :StOut, :StSize, :Exc)
			
			var_sym = Symbol(:retro,va)
			costPar_sym = Symbol(:costRetro,va)
			part_sym = va == :Exc ? :exc : :tech 

			# ! get all retrofitting variables
			if !(costPar_sym in parObj_arr) continue end

			# get all variables
			allRetro_df = filter(x -> x.start,getAllVariables(var_sym,anyM))
			if !isempty(allRetro_df)
				allRetro_df = rename(allRetro_df,:var => :retro)
			else
				continue
			end

			# ! determine relevant lifetime (use economic lifetime where defined, else use technical lifetime, remember technical lifetime also depends on remaining lifetime of target)

			# add economic lifetime to table where it is defined
			if Symbol(:lifeRetroEco,va) in parObj_arr  || Symbol(:lifeRetroEco,va,:Dir) in parObj_arr
				if va != :Exc
					ecoLife_df = matchSetParameter(allRetro_df,partCost.par[Symbol(:lifeRetroEco,va)],anyM.sets,newCol = :life)
				else
					ecoLife_df = rename(matchCostExcParameter(Symbol(:lifeRetroEco,va),allRetro_df,anyM),:val => :life)
				end
				noEcoLife_df = antijoin(allRetro_df,ecoLife_df, on = intCol(allRetro_df))
				noEcoLife_df[!,:life] .= nothing
				allRetro_df = vcat(ecoLife_df,noEcoLife_df)
			else
				allRetro_df[!,:life] .= nothing
			end

			# collects all technical lifetimes of start system and credit factors to compute technical lifetime of retrofitting
			sysFilt_arr = (va == :Exc ? :Exc : :Te) |> (z -> filter(y ->  sysInt(y,anyM.sets[z]) in allRetro_df[:,Symbol(z,:_j)], sysSym_dic[part_sym]))
			
			parObj_dic = Dict{Symbol,ParElement}()

			for par in (va != :Exc ? (Symbol(:creditRetro,va),Symbol(:lifeRetro,va),Symbol(:life,va)) : (Symbol(:creditRetro,va),Symbol(:lifeRetro,va),Symbol(:life,va),Symbol(:life,va,:Dir)))
				allPar_arr = map(w -> isempty(w) ? DataFrame(val = [par == Symbol(:creditRetro,va) ? 1.0 : (va != :Exc ? 20.0 : 50.0)]) : w,map(x -> getfield(anyM.parts,part_sym)[x].par[par].data, sysFilt_arr))
				union(intCol.(allPar_arr)...) |> (z -> map(x -> map(y -> insertcols!(x,1,(y => fill(0,size(x,1)))) , setdiff(z,intCol(x)) ),allPar_arr))
				parObj_dic[par] = copy(getfield(anyM.parts,part_sym)[sysFilt_arr[1]].par[par],unique(vcat(allPar_arr...)))
			end

			# match technical lifetime of start system, requires to rename columns
			startCol_arr = va == :Conv ? [:Te, :Ts_expSup, :R_exp] : (va in (:StIn, :StOut, :StSize) ? [:Te, :Ts_expSup, :R_exp, :id] : [:Exc,:Ts_expSup,:R_from,:R_to])

			if va != :Exc
				techLife_df = matchSetParameter(rename(allRetro_df,Symbol.(startCol_arr,"_i") .=> startCol_arr),parObj_dic[Symbol(:life,va)],anyM.sets,newCol = :lifeStart)
			else
				techLife_df = rename(matchCostExcParameter(:lifeExc,rename(allRetro_df,Symbol.(startCol_arr,"_i") .=> startCol_arr),anyM,parObj_dic[:lifeExc],parObj_dic[:lifeExcDir]),:val => :lifeStart)
			end
			rename!(techLife_df,startCol_arr .=> Symbol.(startCol_arr,"_i"))

			# compute technical lifetime of retrofitting
			techLife_df = matchSetParameter(techLife_df,parObj_dic[Symbol(:creditRetro,va)],anyM.sets,newCol = :credit)
			techLife_df = matchSetParameter(techLife_df,parObj_dic[Symbol(:lifeRetro,va)],anyM.sets,newCol = :lifeRetroSys)
			techLife_df[!,:life] = map(x -> ((x.Ts_disSup_last-x.Ts_retro) * anyM.options.shortExp + x.lifeStart % anyM.options.shortExp) * x.credit + x.lifeRetroSys, eachrow(techLife_df))
			
			# uses economic life where one is defined, otherwise use technical lifetime
			allRetro_df = vcat(select(techLife_df,Not([:lifeStart,:credit,:lifeRetroSys])),filter(x -> !isnothing(x.life),allRetro_df))

			# ! gets expansion costs and compute annuity

			# gets retrofitting costs and factor (since we used start system here to obtain paramter for technical lifetime, but cost relate to target system)
			allRetro_df = matchSetParameter(allRetro_df,partCost.par[Symbol(:facRetro,va)],anyM.sets,newCol = :fac)
			allRetro_df = matchSetParameter(allRetro_df,partCost.par[costPar_sym],anyM.sets,newCol = :cost)

			if isempty(allRetro_df) continue end

			# get discount rate and computes annuity
			allRetro_df = computeAnn(va,:Retro,allRetro_df,anyM)

			# adds discount factor and computes cost expression
			rename!(allRetro_df,Symbol.(startCol_arr,"_i") .=> startCol_arr)
			allRetro_df = matchSetParameter(allRetro_df,partCost.par[va != :Exc ? :disFac : :disFacExc],anyM.sets,newCol = :disFac)

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allExp_df = rename(combine(x -> (expr = sum(x.disFac .* x.retro .* x.fac .* x.costAnn),) ,groupby(allRetro_df,va != :Exc ? [:Ts_disSup,:R_exp,:Te] : [:Ts_disSup,:Exc])),:Ts_disSup => :Ts_exp)
			transferCostEle!(allExp_df, partCost,costPar_sym,anyM.optModel,anyM.lock,anyM.sets,anyM.options.coefRng,anyM.options.scaFac.costCapa,anyM.options.checkRng)
		end

	end

	#endregion

	#region # * dispatch related costs

	if isempty(anyM.subPro) || anyM.subPro != (0,0)
		# ! add elements for variable costs of technologies
		for va in (:use,:gen,:stIn,:stOut,:exc,:in,:out)

			costPar_sym = string(va) |> (x -> Symbol(:costVar,uppercase(x[1]),x[2:end]))

			if !(costPar_sym in parObj_arr || (va == :use && :emissionPrc in parObj_arr && :emissionFac in keys(anyM.parts.lim.par))) continue end

			# obtain all variables
			allDisp_df = getAllVariables(va,anyM)
			if isempty(allDisp_df)
				continue
			else
				allDisp_df = rename(allDisp_df,:var => :disp)
			end

			# special case for variable costs of exchange (direct and symmetric values need to be considered both) and of use (emission price needs to be considered)
			if va != :exc
				allDisp_df = matchSetParameter(allDisp_df,anyM.parts.cost.par[costPar_sym],anyM.sets,newCol = :costVar)
			else
				allDisp_df = rename(matchCostExcParameter(costPar_sym,allDisp_df,anyM),:val => :costVar)
			end

			if isempty(allDisp_df) continue end

			# add scenario probability
			allDisp_df[!,:disp] = allDisp_df[!,:disp] .* map(x -> anyM.supTs.scrProp[(x.Ts_disSup,x.scr)],eachrow(allDisp_df))

			# renames dispatch regions to enable join with discount factors
			if va != :exc rename!(allDisp_df,:R_dis => :R_exp) end
			allDisp_df = matchSetParameter(allDisp_df,partCost.par[va != :exc ? :disFac : :disFacExc],anyM.sets,newCol = :disFac)

			# ! group cost expressions by system, scales groups expression and creates a variables for each grouped entry
			allDisp_df = combine(x -> (expr = sum(x.disFac .* x.disp .* x.costVar) ./ 1000.0 .* anyM.options.redStep,) ,groupby(allDisp_df,va != :exc ? [:Ts_disSup,:R_exp,:Te,:scr] : [:Ts_disSup,:C,:scr]))
			transferCostEle!(allDisp_df, partCost,costPar_sym,anyM.optModel,anyM.lock,anyM.sets,anyM.options.coefRng,anyM.options.scaFac.costDisp,anyM.options.checkRng)
		end
		produceMessage(anyM.options,anyM.report, 3," - Created expression for variables costs")

		# ! add elements for curtailment and loss of load costs of energy carriers
		varToPar_dic = Dict(:crt => :costCrt, :lss => :costLss, :trdBuy => :trdBuyPrc, :trdSell => :trdSellPrc)
		for varType in [:crt,:lss,:trdBuy, :trdSell]
			if varType in keys(anyM.parts.bal.var)
				cost_sym = varToPar_dic[varType]
				# compute discounted curtailment costs
				allVar_df = rename(matchSetParameter(anyM.parts.bal.var[varType],anyM.parts.bal.par[cost_sym],anyM.sets,newCol = :cost),:var => varType)
				allVar_df = matchSetParameter(rename(allVar_df,:R_dis => :R_exp),partCost.par[:disFac],anyM.sets,newCol = :disFac)
				# add scenario probability
				allVar_df[!,varType] = allVar_df[!,varType] .* map(x -> anyM.supTs.scrProp[(x.Ts_disSup,x.scr)],eachrow(allVar_df))
				# groups cost expressions by carrier, scales groups expression and creates a variables for each grouped entry
				allVar_df = combine(x -> (expr = sum(x.disFac .* x[!,varType] .* x.cost) ./ 1000.0 .* anyM.options.redStep,) ,groupby(allVar_df, [:Ts_disSup,:R_exp,:C,:scr]))
				transferCostEle!(allVar_df, partCost,cost_sym,anyM.optModel,anyM.lock,anyM.sets,anyM.options.coefRng,anyM.options.scaFac.costDisp,anyM.options.checkRng, (va == :trdBuy ? 0.0 : NaN))
			end
		end

		produceMessage(anyM.options,anyM.report, 3," - Created expression for curtailment and trade costs")
	end

	#endregion

	# ! creates overall costs variable considering scaling parameters
	relVar_arr = filter(x -> x != :objVar, collectKeys(keys(partCost.var)))
	grpVar_arr = fill(:costs,length(relVar_arr))
	objVar_arr = map(relVar_arr) do varName
		# sets lower limit of zero, except for curtailment and revenues from selling, because these can incure "negative" costs
		lowBd_tup = !(varName in (:costCrt,:costTrdSell)) |> (x -> (x,x ? 0.0 : NaN))
		info = VariableInfo(lowBd_tup[1], lowBd_tup[2], false, NaN, false, NaN, false, NaN, false, false)
		return JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, info),string(varName))
	end

	# create dataframe with for overall cost equations and scales it
	objExpr_arr = [objVar_arr[idx] - sum(partCost.var[name][!,:var]) for (idx, name) in enumerate(relVar_arr)]
	cns_df = DataFrame(group = fill(:costs,length(objExpr_arr)), name = relVar_arr, cnsExpr = objExpr_arr)

	# add variables and equations to overall objective dataframes
	partCost.cns[:objEqn] = vcat(partCost.cns[:objEqn],createCns(cnsCont(cns_df,:equal),anyM.optModel))
	partCost.var[:objVar] = vcat(partCost.var[:objVar],DataFrame(group = grpVar_arr, name = relVar_arr, var = objVar_arr))
end


# ! computes annuity of expansion or retrofitting costs
function computeAnn(va_sym::Symbol,type_sym::Symbol,allData_df::DataFrame,anyM::anyModel)

	# ! uses tech specific discount rate and fall back on general discount rate as default
	if Symbol(:rate,type_sym,va_sym) in keys(anyM.parts.cost.par)
		if va_sym != :Exc || type_sym == :Retro
			techRate_df = matchSetParameter(allData_df,anyM.parts.cost.par[Symbol(:rate,type_sym,va_sym)],anyM.sets,newCol = :rate)
		else
			techRate_df = rename(matchCostExcParameter(Symbol(:rate,type_sym,va_sym),allData_df,anyM),:val => :rate)
		end
	else
		techRate_df = filter(x -> false,allData_df); techRate_df[!,:rate] .= Float64[]
	end
	# ! obtains general discount rate
	
	# renames columns to match general discount rate for retrofitting
	generRate_df = rename(antijoin(allData_df,techRate_df,on = intCol(techRate_df)),(type_sym == :Exp ? :Ts_expSup : :Ts_retro) => :Ts_disSup, :Ts_disSup => (type_sym == :Exp ? :Ts_expSup : :Ts_expSup_temp))
	
	if type_sym == :Retro
		startCol_arr = va_sym == :Conv ? [:Te, :Ts_expSup, :R_exp] : (va_sym in (:StIn, :StOut, :StSize) ? [:Te, :Ts_expSup, :R_exp] : [:Exc,:Ts_expSup,:R_from,:R_to])
		rename!(generRate_df,Symbol.(startCol_arr,"_i") .=> startCol_arr)
	end

	# match discount rates
	if va_sym != :Exc
		generRate_df = matchSetParameter(generRate_df, anyM.parts.cost.par[:rateDisc],anyM.sets,newCol = :rate)
	else
		rateB_arr = matchSetParameter(rename(generRate_df,:R_from => :R_exp), anyM.parts.cost.par[:rateDisc],anyM.sets,newCol = :rateA)[!,:rateA]
		rateA_arr = matchSetParameter(rename(generRate_df,:R_to => :R_exp), anyM.parts.cost.par[:rateDisc],anyM.sets,newCol = :rateB)[!,:rateB]
		generRate_df[!,:rate] = 0.5 .* (rateA_arr .+ rateB_arr)
	end

	# switch column names back again and merge dataframe for different rates

	if type_sym == :Retro rename!(generRate_df,startCol_arr .=> Symbol.(startCol_arr,"_i")) end
	allData_df = vcat(techRate_df,rename(generRate_df, :Ts_disSup => (type_sym == :Exp ? :Ts_expSup : :Ts_retro), (type_sym == :Exp ? :Ts_expSup : :Ts_expSup_temp) => :Ts_disSup))

	# ! compute annuity costs
	allData_df[!,:costAnn] = map(x -> x.cost * (x.rate == 0.0 ? 1/x.life : (x.rate * (1 + x.rate)^x.life) / ((1 + x.rate)^x.life-1)), eachrow(allData_df))
	select!(allData_df,Not([:cost,:life,:rate]))
	allData_df = flatten(allData_df,:Ts_disSup)

	return allData_df
end

# ! creates new parameter objects for discount factors from discount rates provided
function computeDisFac!(partObj::OthPart,anyM::anyModel)

	# ! discount factor for technologies
	rExp_arr = union(map(x -> getfield.(getNodesLvl(anyM.sets[:R],x),:idx), unique(getfield.(values(anyM.cInfo),:rExp)))...)
	discR_df = matchSetParameter(flatten(flatten(DataFrame(Ts_disSup = anyM.supTs.step, R_exp = rExp_arr),:Ts_disSup),:R_exp),partObj.par[:rateDisc],anyM.sets)

	discR_df[!,:disFac] = 1 ./ (1 .+ discR_df[!,:val]).^anyM.options.shortExp
	discR_df[!,:disFac] = map(x -> filter(y -> y < x.Ts_disSup ,collect(anyM.supTs.step)) |> (z -> prod(filter(y -> y.R_exp == x.R_exp && y.Ts_disSup in z, discR_df)[!,:disFac])*x.disFac),eachrow(discR_df))
	select!(discR_df,Not(:val))

	discPar_obj = copy(partObj.par[:rateDisc],rename(discR_df,:disFac => :val))
	discPar_obj.name = :discFac
	discPar_obj.defVal = nothing
	partObj.par[:disFac] = discPar_obj

	# ! discount factor for exchange (average of from and to region)
	discRExc_df = rename(copy(discR_df),:R_exp => :R_from,:disFac => :disFacFrom)
	discRExc_df[!,:R_to] .= [unique(discRExc_df[!,:R_from])]
	discRExc_df = flatten(discRExc_df,:R_to)

	discRExc_df = innerjoin(discRExc_df,discR_df, on = [:Ts_disSup,:R_to] .=> [:Ts_disSup,:R_exp])
	discRExc_df[!,:disFac] = (discRExc_df[!,:disFac] + discRExc_df[!,:disFacFrom]) * 0.5
	select!(discRExc_df,Not(:disFacFrom))

	discPar_obj = copy(partObj.par[:rateDisc],rename(discRExc_df,:disFac => :val))
	discPar_obj.name = :disFacExc
	discPar_obj.defVal = nothing
	discPar_obj.dim = (:Ts_dis, :R_from, :R_to)
	discPar_obj.herit = (:Ts_disSup => :up, :R_from => :up, :R_to => :up, :Ts_disSup => :avg_any, :R_from => :avg_any, :R_to => :avg_any)
	partObj.par[:disFacExc] = discPar_obj
end

# matches exchange variables with cost parameters, matchExcParameter cannot be applied directly, because cost parameters are stored in other model part
function matchCostExcParameter(par_sym::Symbol,data_df::DataFrame,anyM::anyModel,unDir_obj::Union{ParElement,Nothing}=nothing,dir_obj::Union{ParElement,Nothing}=nothing)

	# groups by exchange technology
	grpExc_gdf = groupby(data_df,[:Exc])
	matchData_arr = Array{DataFrame}(undef,length(grpExc_gdf))

	# loops over exchange technologies and obtains data
	for (idx,excDf) in enumerate(grpExc_gdf)	
		matchData_arr[idx] = matchExcParameter(par_sym,DataFrame(excDf),anyM.parts.cost,anyM.sets,anyM.parts.exc[sysSym(excDf.Exc[1],anyM.sets[:Exc])].dir,unDir_obj,dir_obj)
	end

	return vcat(matchData_arr...)
end