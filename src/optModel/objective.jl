
# ! sets the objective function according to the arguments provided in obj_dic
"""
Set the objective of the model's underlying optimization problem.
```julia
setObjective!(obj_dic::Union{Dict{Symbol,Float64},Symbol}, model_object::anyModel)
```
`obj_dic` is a keyword argument that specifies the respective objective. To perform a multi-criteria optimization, it can also be a dictionary with the keywords as keys and weights as values. So far, the only supported key-word is `:costs`.
"""
function setObjective!(obj_dic::Union{Dict{Symbol,Float64},Symbol},anyM::anyModel,minimize::Bool=true)

    # ! converts input into dictionary, if only a symbol was provided, if :none keyword was provided returns a dummy objective function
    if typeof(obj_dic) == Symbol
        if obj_dic == :none @objective(anyM.optModel, Min, 1); return, produceMessage(anyM.options,anyM.report, 1," - Set an empty objective function") end
        obj_dic = Dict(obj_dic => 1.0)
    end

    # ! create empty variables table for objective variables, if already other object defined, these variables and equations are removed from the model
    partObj = anyM.parts.obj

	if !(:objVar in keys(partObj.var))
		partObj.var[:objVar] = DataFrame(name = Symbol[], group = Symbol[], var = AffExpr[])
		partObj.cns[:objEqn] = DataFrame(name = Symbol[], group = Symbol[], cns = ConstraintRef[])
	end

    # ! create variables and equations required for specified objectives
    for objGrp in setdiff(keys(obj_dic),unique(partObj.var[:objVar][!,:group]))
        createObjective!(objGrp,partObj,anyM)
    end

	# ! adds alpha variable for benders
	if anyM.subPro == (0,0)
		info = VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)
		push!(partObj.var[:objVar],(name = :allCut, group = :benders, var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, info),"allCut")))
		obj_dic[:benders] = 1.0
		partObj.cns[:bendersCuts] = DataFrame(id=Int[], Ts_disSup = Int[], scr = Int[], cut = String[], cns = ConstraintRef[])
	end

    # ! sets overall objective variable with upper limits and according to weights provided in dictionary
	objBd_flt = anyM.options.bound.obj |> (x -> isnan(x) ? NaN : x / anyM.options.scaFac.obj)
	obj_var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(false, NaN, !isnan(objBd_flt), objBd_flt, false, NaN, false, NaN, false, false)),"obj") * anyM.options.scaFac.obj
	obj_eqn = @constraint(anyM.optModel, obj_var == sum(map(x -> sum(filter(r -> r.group == x,partObj.var[:objVar])[!,:var])*obj_dic[x], collectKeys(keys(obj_dic)))))
	partObj.var[:obj] = DataFrame(var = obj_var)

    if minimize
        @objective(anyM.optModel, Min, obj_var / anyM.options.scaFac.obj)
    else
        @objective(anyM.optModel, Max, obj_var / anyM.options.scaFac.obj)
    end

	produceMessage(anyM.options,anyM.report, 1," - Set objective function according to inputs")
end

createObjective!(objGrp::Symbol, partObj::OthPart,anyM::anyModel) = createObjective!(Val{objGrp}(), partObj::OthPart,anyM::anyModel)

#=
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
=#