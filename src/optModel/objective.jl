
# ! sets the objective function according to the arguments provided in obj_dic
"""
Set the objective of the model's underlying optimization problem.
```julia
setObjective!(obj::Union{Symbol,Tuple}, model_object::anyModel)
```
neu
"""

# ! functions to convert object input to dictionary
function setObjective!(obj::Symbol,anyM::anyModel,minimize::Bool=true)
	
	# convert argument into uniform dictionary
	obj_dic = Dict{Symbol,NamedTuple}()
	obj_dic[obj] = (flt = x -> true,fac = 1.0)
	
	# set actual objective
	createObjective!(obj_dic,anyM,minimize)

end

function setObjective!(obj::Tuple,anyM::anyModel,minimize::Bool=true)
	
	# convert argument into uniform dictionary
	obj_dic = Dict{Symbol,NamedTuple}()
	
	for ob in obj
		if typeof(ob) <: Pair
			obj_dic[ob[1]] = (flt = haskey(ob[2],:flt) ? ob[2].flt : (x -> true),fac = haskey(ob[2],:fac) ? ob[2].fac : 1.0)
		else
			obj_dic[ob] = (flt = (x -> true),fac = 1.0)
		end
	end
	
	# set actual objective
	createObjective!(obj_dic,anyM,minimize)

end

# ! create actual objective
function createObjective!(obj_dic::Dict{Symbol,NamedTuple},anyM::anyModel,minimize::Bool=true)

	partObj = anyM.parts.obj

	# ! create elements of objective variables and equations
	partObj.var[:objVar] = DataFrame(name = Symbol[], var = AffExpr[])
	partObj.cns[:objEqn] = DataFrame(name = Symbol[], cns = ConstraintRef[])

	for obj in collect(obj_dic)
		allObjVar_df = getAllVariables(obj[1],anyM, filterFunc = obj[2].flt)
		if isempty(allObjVar_df)
			error("At least one cost argument returned no variables. Check for typo in name!")
		end
		obj_expr = obj[2].fac * sum(allObjVar_df[!,:var])
		obj_var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(false, NaN, false, NaN, false, NaN, false, NaN, false, false)),string(obj[1]))
		push!(partObj.var[:objVar], (name = obj[1], var = obj_var))
		push!(partObj.cns[:objEqn], (name = obj[1], cns = @constraint(anyM.optModel, obj_var == obj_expr)))
	end

	# ! add cut variable for benders
	if anyM.subPro == (0,0)
		push!(partObj.var[:objVar],(name = :benders, var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)),"allCut")))
		partObj.cns[:bendersCuts] = DataFrame(i=Int[], Ts_disSup = Int[], scr = Int[], limCoef = Bool[], cns = ConstraintRef[])
	end

	# ! sets overall objective variable with upper limits and according to weights provided in dictionary
	objBd_flt = anyM.options.bound.obj |> (x -> isnan(x) ? NaN : x / anyM.options.scaFac.obj)
	obj_var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(false, NaN, !isnan(objBd_flt), objBd_flt, false, NaN, false, NaN, false, false)),"obj") * anyM.options.scaFac.obj
	obj_eqn = @constraint(anyM.optModel, obj_var == sum(partObj.var[:objVar][!,:var]))
	partObj.var[:obj] = DataFrame(var = obj_var)

	if minimize
		@objective(anyM.optModel, Min, obj_var / anyM.options.scaFac.obj)
	else
		@objective(anyM.optModel, Max, obj_var / anyM.options.scaFac.obj)
	end

	produceMessage(anyM.options,anyM.report, 1," - Set objective function according to inputs")
end