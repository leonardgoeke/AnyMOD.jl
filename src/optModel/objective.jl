
# XXX sets the objective function according to the arguments provided in obj_dic
function setObjective!(obj_dic::Union{Dict{Symbol,Float64},Symbol},anyM::anyModel,minimize::Bool=true)

    # XXX converts input into dictionary, if only a symbol was provided, if :none keyword was provided returns a dummy objective function
    if typeof(obj_dic) == Symbol
        if obj_dic == :none @objective(anyM.optModel, Min, 1); return, produceMessage(anyM.options,anyM.report, 1," - Set an empty objective function") end
        obj_dic = Dict(obj_dic => 1.0)

    end

    # XXX create empty variables table for objective variables, if already other object defined, these variables and equations are removed from the model
    partObj = anyM.parts.obj

	if :objVar in keys(partObj.var)
		map(x -> delete(anyM.optModel,collect(keys(x.terms))[1]),partObj.var[:objVar][!,:var])
		map(x -> delete(anyM.optModel,x),partObj.cns[:objEqn][!,:cns])
	end

	partObj.var[:objVar] = DataFrame(name = Symbol[], group = Symbol[], var = VariableRef[])
	partObj.cns[:objEqn] = DataFrame(name = Symbol[], group = Symbol[], cns = ConstraintRef[])

    # XXX create variables and constraints required for specified objectives
    for objGrp in keys(obj_dic)
        createObjective!(objGrp,partObj,anyM)
    end

    # XXX sets overall objective variable according to weights provided in dictionary
    if minimize
        @objective(anyM.optModel, Min, sum(map(x -> sum(filter(r -> r.group == x,partObj.var[:objVar])[!,:var])*obj_dic[x], collect(keys(obj_dic)))))
    else
        @objective(anyM.optModel, Max, sum(map(x -> sum(filter(r -> r.group == x,partObj.var[:objVar])[!,:var])*obj_dic[x], collect(keys(obj_dic)))))
    end

	produceMessage(anyM.options,anyM.report, 1," - Set objective function according to inputs")
end

createObjective!(objGrp::Symbol, partObj::OthPart,anyM::anyModel) = createObjective!(Val{objGrp}(), partObj::OthPart,anyM::anyModel)

# XXX create variables and constraints for cost objective
function createObjective!(objGrp::Val{:costs},partObj::OthPart,anyM::anyModel)

	exprCost_dic = Dict{Symbol,Array{GenericAffExpr{Float64,VariableRef},1}}()

	parObj_arr = collect(keys(partObj.par))
	techIdx_arr = collect(keys(anyM.parts.tech))
	varToPart_dic = Dict(:exc => :exc, :ctr => :bal,:trdSell => :trd, :trdBuy => :trd)

	# computes discount factors from discount rate provided and saves them as new parameter elements
	computeFacDisc!(partObj,anyM)

	# XXX get all expansion costs
	expCost_arr = Array{GenericAffExpr{Float64,VariableRef},1}()

	for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

		var_sym = Symbol(:exp,va)
		costPar_sym = Symbol(:costExp,va)

		if !(costPar_sym in parObj_arr) continue end

		# get all variables
		allExp_df = getAllVariables(var_sym,anyM)

		# add economic lifetime to table where it is defined
		if Symbol(:lifeEco,va) in parObj_arr
			ecoLife_df = matchSetParameter(allExp_df,partObj.par[Symbol(:lifeEco,va)],anyM.sets,anyM.report,newCol = :life)
			noEcoLife_df = join(allExp_df,ecoLife_df, on = intCol(allExp_df), kind = :anti)
			noEcoLife_df[!,:life] .= nothing
			allExp_df = vcat(ecoLife_df,noEcoLife_df)
		else
			allExp_df[!,:life] .= nothing
		end

		techFilt_arr = filter(x -> !(any(map(x -> occursin(x,string(va)),("capa","exp")))) || anyM.parts.tech[x].type != :stock, collect(keys(anyM.parts.tech)))

		# use technical lifetime where no economic lifetime could be obtained
		if va != :Exc
			allPar_arr = map(x -> anyM.parts.tech[x].par[Symbol(:life,va)].data,filter(y -> var_sym in keys(anyM.parts.tech[y].var), techIdx_arr))
			union(intCol.(allPar_arr)...) |> (z -> map(x -> map(y -> insertcols!(x,1,(y => fill(0,size(x,1)))) , setdiff(z,intCol(x)) ) ,allPar_arr))
			lifePar_obj = copy(anyM.parts.tech[techFilt_arr[1]].par[Symbol(:life,va)],vcat(allPar_arr...))
		else
			lifePar_obj = anyM.parts.exc.par[:lifeExc]
		end

		techLife_df = matchSetParameter(filter(x -> isnothing(x.life),allExp_df)[!,Not(:life)],lifePar_obj,anyM.sets,anyM.report,newCol = :life)
		allExp_df = vcat(techLife_df,filter(x -> !isnothing(x.life),allExp_df))

		# gets expansion costs and interest reat to compute annuity
		allExp_df = matchSetParameter(convertExcCol(allExp_df),partObj.par[costPar_sym],anyM.sets,anyM.report,newCol = :costExp)
		allExp_df = matchSetParameter(allExp_df,partObj.par[Symbol(:rateExp,va)],anyM.sets,anyM.report,newCol = :rate)
		allExp_df[!,:costAnn] = map(x -> x.costExp * (x.rate * (1 + x.rate)^x.life) / ((1 + x.rate)^x.life-1), eachrow(allExp_df))
		select!(allExp_df,Not([:costExp,:life,:rate]))
		allExp_df = flatten(allExp_df,:Ts_disSup)

		# adds discount factor and computes cost expression
		allExp_df = matchSetParameter(convertExcCol(allExp_df),partObj.par[va != :Exc ? :facDisc : :facDiscExc],anyM.sets,anyM.report,newCol = :disFac)
		push!(expCost_arr, sum(allExp_df[!,:var] .* allExp_df[!,:disFac] .* allExp_df[!,:costAnn]))
	end
	if !isempty(expCost_arr) exprCost_dic[:totExpCost] = expCost_arr end
	produceMessage(anyM.options,anyM.report, 3," - Created expression for expansion costs")

	# XXX get all operation costs
	oprCost_arr = Array{GenericAffExpr{Float64,VariableRef},1}()
	# if decommissioning is enabled, capacity costs depend on commissioned and not on installed capacities
	capaTyp_sym =  anyM.options.decomm != :none ? :capaComm : :capa

	for va in (:Conv, :StIn, :StOut, :StSize, :Exc)

		var_sym = Symbol(capaTyp_sym,va)
		costPar_sym = Symbol(:costOpr,va)

		if !(costPar_sym in parObj_arr) continue end

		# get all variables
		allCapa_df = getAllVariables(var_sym,anyM)

		# joins costs and discount factors to create cost expression
		allCapa_df = matchSetParameter(convertExcCol(allCapa_df),partObj.par[costPar_sym],anyM.sets,anyM.report,newCol = :costOpr)
		allCapa_df = matchSetParameter(convertExcCol(allCapa_df),partObj.par[va != :Exc ? :facDisc : :facDiscExc],anyM.sets,anyM.report,newCol = :disFac)

		push!(oprCost_arr, sum(allCapa_df[!,:var] .* allCapa_df[!,:disFac] .* allCapa_df[!,:costOpr]))
	end
	if !isempty(oprCost_arr) exprCost_dic[:totOprCost] = oprCost_arr end
	produceMessage(anyM.options,anyM.report, 3," - Created expression for capacity costs")

	# XXX variable costs
	varCost_arr = Array{GenericAffExpr{Float64,VariableRef},1}()
	for va in (:use,:gen,:stIn,:stOut,:stSize,:exc)

		costPar_sym = string(va) |> (x -> Symbol(:costVar,uppercase(x[1]),x[2:end]))

		if !(costPar_sym in parObj_arr || (va == :use && :emissionPrc in parObj_arr && :emissionFac in keys(anyM.parts.lim.par))) continue end

		# obtain all variables
		allVar_df = getAllVariables(va,anyM)

		# special case for variable costs of exchange (direct and symmetric values need to be considered both) and of use (emission price needs to be considered)
		if va == :exc
			if :costVarExcDir in parObj_arr
				dirCost_df = matchSetParameter(convertExcCol(allVar_df),anyM.parts.obj.par[:costVarExcDir],anyM.sets,anyM.report,newCol = :costVar)
			else
				dirCost_df = convertExcCol(allVar_df[[],:])
			end
			noDirCost_df = matchSetParameter(join(convertExcCol(allVar_df),dirCost_df, on = intCol(dirCost_df), kind = :anti),anyM.parts.obj.par[costPar_sym],anyM.sets,anyM.report,newCol = :costVar)

			allVar_df = convertExcCol(vcat(dirCost_df,noDirCost_df))
		elseif va == :use && :emissionPrc in parObj_arr && :emissionFac in keys(anyM.parts.lim.par)
			# get emission prices as a costs entry
			emPrc_df = matchSetParameter(select(allVar_df,Not(:var)),partObj.par[:emissionPrc],anyM.sets,anyM.report, newCol = :prc)
			emPrc_df = matchSetParameter(emPrc_df,anyM.parts.lim.par[:emissionFac],anyM.sets,anyM.report, newCol = :fac)
			emPrc_df[!,:costEms] = emPrc_df[!,:prc] .*  emPrc_df[!,:fac] ./ 1000
			select!(emPrc_df,Not([:prc,:fac]))
			# merge emission costs with other variable costs or just use emission costs if there are not any other
			if costPar_sym in parObj_arr
				otherVar_df = matchSetParameter(select(allVar_df,Not(:var)),anyM.parts.obj.par[costPar_sym],anyM.sets,anyM.report,newCol = :costVar)
				allCost_df = joinMissing(otherVar_df,emPrc_df,intCol(emPrc_df),:outer,merge(Dict{Symbol,Any}(:costVar => 0.0, :costEms => 0.0),Dict{Symbol,Any}(x => 0 for x in intCol(emPrc_df))) )
				allCost_df[!,:costVar] = allCost_df[!,:costVar] .+ allCost_df[!,:costEms]
				select!(allCost_df,Not(:costEms))
			else
				allCost_df = emPrc_df
				rename!(allCost_df,:costEms => :costVar)
			end

			allVar_df = join(allCost_df,allVar_df, on = intCol(allVar_df), kind = :inner)
		else
			allVar_df = matchSetParameter(allVar_df,anyM.parts.obj.par[costPar_sym],anyM.sets,anyM.report,newCol = :costVar)
		end

		# renames dispatch regions to enable join with discount factors
		if va != :exc rename!(allVar_df,:R_dis => :R_exp) end
		allVar_df = matchSetParameter(allVar_df,partObj.par[va != :exc ? :facDisc : :facDiscExc],anyM.sets,anyM.report,newCol = :facDisc)
		# computes cost expression
		push!(varCost_arr, sum(allVar_df[!,:var] .* allVar_df[!,:facDisc] .* allVar_df[!,:costVar] ./ 1000))
	end
	if !isempty(varCost_arr) exprCost_dic[:totVarCost] = varCost_arr end
	produceMessage(anyM.options,anyM.report, 3," - Created expression for variables costs")

	# XXX curtailment and trade costs
	othCost_arr = Array{GenericAffExpr{Float64,VariableRef},1}()
	if :crt in keys(anyM.parts.bal.var)
		allVar_df = matchSetParameter(anyM.parts.bal.var[:crt],anyM.parts.bal.par[:costCrt],anyM.sets,anyM.report,newCol = :costCrt)
		allVar_df = matchSetParameter(rename(allVar_df,:R_dis => :R_exp),partObj.par[:facDisc],anyM.sets,anyM.report,newCol = :facDisc)
		push!(othCost_arr, sum(allVar_df[!,:var] .* allVar_df[!,:facDisc] .* allVar_df[!,:costCrt]  ./ 1000))
	end

	for va in (:trdBuy, :trdSell)
		if va in keys(anyM.parts.trd.var)
			allVar_df = matchSetParameter(anyM.parts.trd.var[va],anyM.parts.trd.par[Symbol(va,:Prc)],anyM.sets,anyM.report,newCol = :costTrd)
			allVar_df = matchSetParameter(rename(allVar_df,:R_dis => :R_exp),partObj.par[:facDisc],anyM.sets,anyM.report,newCol = :facDisc)
			push!(othCost_arr, sum(allVar_df[!,:var] .* allVar_df[!,:facDisc] .* allVar_df[!,:costTrd] ./ 1000))
		end
	end
	if !isempty(othCost_arr) exprCost_dic[:totOthCost] = othCost_arr end
	produceMessage(anyM.options,anyM.report, 3," - Created expression for curtailment and trade costs")

	# XXX create costs variables and adds them table of object variables
	if isnothing(anyM.options.bound.cost)
		info = VariableInfo(true, 0.0, true, 1e5, false, NaN, false, NaN, false, false)
		infoOth = VariableInfo(false, NaN, false, NaN, false, NaN, false, NaN, false, false)
	else
		costBound_flt = anyM.options.bound.cost * length(anyM.supTs.step)
		info = VariableInfo(true, 0.0, true, costBound_flt, false, NaN, false, NaN, false, false)
		infoTrd = VariableInfo(true, - costBound_flt, true, costBound_flt, false, NaN, false, NaN, false, false) # trade costs do not have a lower limit of zero, because sell revenues can exceed costs
	end

	# XXX creates overall costs variable considering scaling parameters
	scale_dic = Dict(:totExpCost => 0.01, :totOprCost => 0.01, :totVarCost => 1.0, :totOthCost => 0.2)

	objVar_arr = map(collect(keys(exprCost_dic))) do name
		# sets lower limit of zero or none for other costs, because curtailment and trade can create reveneus
		lowBd_tup = (name != :totOthCost) |> (x -> (x,x ? 0.0 : NaN))
		# sets an upper limit, if a corresponding value was provided
		upBd_tup = !isnothing(anyM.options.bound.cost) |> (x -> (x,x ? anyM.options.bound.cost * scale_dic[name] : NaN))
		# corrects the lower limit of trade, if an upper cost limit exists
		if lowBd_tup[1] == false && !isnothing(anyM.options.bound.cost)
			lowBd_tup = (true, -upBd_tup[2])
		end
		info = VariableInfo(lowBd_tup[1], lowBd_tup[2], upBd_tup[1], upBd_tup[2], false, NaN, false, NaN, false, false)
		JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, info),string(name)) * 1/scale_dic[name]
	end

	partObj.var[:objVar] = vcat(partObj.var[:objVar],DataFrame(group = fill(:costs,length(objVar_arr)), name = collect(keys(exprCost_dic)), var = objVar_arr))

	# XXX creates constraints defining cost variables and adds them to table of object constraints
	objEqn_arr = [@constraint(anyM.optModel, objVar_arr[idx] * scale_dic[name] == sum(exprCost_dic[name]) * scale_dic[name]) for (idx, name) in enumerate(keys(exprCost_dic))]
	partObj.cns[:objEqn] = vcat(partObj.cns[:objEqn],DataFrame(group = fill(:costs,length(objEqn_arr)), name = collect(keys(exprCost_dic)), cns = objEqn_arr))

	produceMessage(anyM.options,anyM.report, 2," - Created all variables and constraints for cost objective")
end
