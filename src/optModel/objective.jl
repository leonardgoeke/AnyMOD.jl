
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



# ! transfers provided cost dataframe into dataframe of overall objective variables and equations (and scales them)
function transferCostEle!(cost_df::DataFrame, partObj::OthPart,costPar_sym::Symbol,optModel::Model,lock_::ReentrantLock,sets_dic::Dict{Symbol,Tree},
												coefRng_tup::NamedTuple{(:mat,:rhs),Tuple{Tuple{Float64,Float64},Tuple{Float64,Float64}}}, scaCost_fl::Float64, checkRng_fl::Float64, lowBd::Float64 = 0.0)

	# create variables for cost entry and builds corresponding expression for equations controlling them
	cost_df = createVar(cost_df,string(costPar_sym),NaN,optModel,lock_,sets_dic, scaFac = scaCost_fl, lowBd = lowBd)
	cost_df[!,:cnsExpr] = map(x -> x.expr - x.var, eachrow(cost_df))
	select!(cost_df,Not(:expr))

	# scales cost expression
	scaleCnsExpr!(cost_df,coefRng_tup,checkRng_fl)
	cost_df[!,:var] = cost_df[!,:var]

	# writes equations and variables
	partObj.cns[costPar_sym] = createCns(cnsCont(select(cost_df,Not(:var)),:equal),optModel)
	partObj.var[costPar_sym] = select(cost_df,Not(:cnsExpr))
end
