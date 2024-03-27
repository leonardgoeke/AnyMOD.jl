
# ! sets the objective function according to the arguments provided in obj_tup
"""
Set the objective of the model's underlying optimization problem.
```julia
setObjective!(obj::Union{Symbol,Tuple}, model_object::anyModel)
```
neu
"""

# ! functions to convert object input to dictionary
function setObjective!(obj::Symbol,anyM::anyModel,minimize::Bool=true,filterFunc::Function = x -> true)
	
	# convert argument into uniform dictionary
	obj_tup = (obj => (flt = filterFunc,fac = 1.0),)
	
	# set actual objective
	createObjective!(obj_tup,anyM,minimize)

end

function setObjective!(obj_tup::Tuple, anyM::anyModel, minimize::Bool=true)
	
	# set actual objective
	createObjective!(obj_tup, anyM, minimize)

end

# ! create actual objective
function createObjective!(obj_tup::Tuple, anyM::anyModel, minimize::Bool=true)

	partObj = anyM.parts.obj

	# delete any existing objective elements
	if :objVar in keys(partObj.var) delete.(anyM.optModel,map(x -> collect(x.terms)[1][1], partObj.var[:objVar][!,:var])) end
	if :objEqn in keys(partObj.cns) delete.(anyM.optModel, partObj.cns[:objEqn][!,:cns]) end

	# ! create elements of objective variables and equations
	partObj.var[:objVar] = DataFrame(name = Symbol[], fac = Float64[], var = AffExpr[])
	partObj.cns[:objEqn] = DataFrame(name = Symbol[], cns = ConstraintRef[])

	
	for obj in obj_tup
		allObjVar_df = getAllVariables(obj[1],anyM, reflectRed = false, filterFunc = obj[2].flt)

		if length(findall(map(x -> x[1] == obj[1], obj_tup))) == 1
			objName_sym = Symbol(obj[1])
		else
			objName_sym = Symbol(obj[1],"_",sum(findall(map(x -> x[1] == obj[1],obj_tup)) .<= findall(map(x -> x == obj,obj_tup))))
		end

		if isempty(allObjVar_df)
			error("At least one cost argument returned no variables. Check for typo in name!")
		end
		obj_expr = sum(allObjVar_df[!,:var])
		obj_var = anyM.options.scaFac.obj * JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(false, NaN, false, NaN, false, NaN, false, NaN, false, false)),string(objName_sym))

		push!(partObj.var[:objVar], (name = objName_sym, fac = obj[2].fac, var = obj_var))
		push!(partObj.cns[:objEqn], (name = objName_sym, cns = @constraint(anyM.optModel, obj_var == obj_expr)))
	end

	# ! add cut variable for benders
	if anyM.subPro == (0,0)
		facCost_fl = filter(x -> occursin("cost",string(x.name)),partObj.var[:objVar])[1,:fac]
		push!(partObj.var[:objVar],(name = :benders, fac = facCost_fl, var = anyM.options.scaFac.obj * JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(true, 0.0, false, NaN, false, NaN, false, NaN, false, false)),"allCut")))
		
		if !(:bendersCuts in keys(partObj.cns)) # create table for cuts if none exist yet
			partObj.cns[:bendersCuts] = DataFrame(i=Int[], Ts_dis = Int[], scr = Int[], limCoef = Bool[], actItr = Int[], cns = ConstraintRef[])
		else # connect cut variables with new overall benders variable
			push!(anyM.parts.obj.cns[:objEqn], (name = :aggCut, cns = @constraint(anyM.optModel, sum(anyM.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders,anyM.parts.obj.var[:objVar])[1,:var])))
		end
	end

	# ! sets overall objective variable with upper limits and according to weights provided in dictionary
	objBd_flt = anyM.options.bound.obj |> (x -> isnan(x) ? NaN : x)
	if !(:obj in keys(partObj.var))
		obj_var = JuMP.add_variable(anyM.optModel, JuMP.build_variable(error, VariableInfo(false, NaN, !isnan(objBd_flt), objBd_flt, false, NaN, false, NaN, false, false)),"obj")
		partObj.var[:obj] = DataFrame(var = obj_var)
	else	
		obj_var = partObj.var[:obj][1,:var]
	end

	# set objective function equal to objective variable
	if :obj in keys(partObj.cns) delete(anyM.optModel,partObj.cns[:obj][1,:cns]) end
	obj_eqn = @constraint(anyM.optModel, obj_var == sum(partObj.var[:objVar][!,:fac] .* getindex.(collect.(keys.(getfield.(partObj.var[:objVar][!,:var],:terms))),1)))
	partObj.cns[:obj] = DataFrame(cns = obj_eqn)

	if minimize
		@objective(anyM.optModel, Min, obj_var)
	else
		@objective(anyM.optModel, Max, obj_var)
	end

	produceMessage(anyM.options,anyM.report, 1," - Set objective function according to inputs")
end