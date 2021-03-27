
#region # * benders objects

# ! struct for results of a sub-problem
mutable struct bendersData
	objValue::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}())
end

# ! stores all information on current trust region 
mutable struct trustRegion
	objValue::Float64
	rad::Float64
	cns::ConstraintRef
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	cnsInfo::NamedTuple{(:sca,:cons,:val),Tuple{Float64,Float64,Array{Float64,1}}}	
	trustRegion() = new()
end

#endregion

#region # * function to run top and sub problem of benders

# ! run sub-Level problem
function runSubLevel(sub_m::anyModel,capaData_obj::bendersData)

	# set rhs of constraints fixing the capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys]), capaSym in keys(capaData_obj.capa[sys][sSym])
			
			# filter capacity data for respective year
			filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], capaData_obj.capa[sys][sSym][capaSym])
			# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
			if isempty(capaData_obj.capa[sys][sSym][capaSym])
				delete!(capaData_obj.capa[sys][sSym],capaSym)
			else
				fixCapa!(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym])
			end
		end
	end

	# set optimizer attributes and solves
	@suppress begin
		optimize!(sub_m.optModel)
	end

	# add duals and objective value to capacity data
	capaData_obj.objValue = objective_value(sub_m.optModel)

	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys]), capaSym in keys(capaData_obj.capa[sys][sSym])
			capaData_obj.capa[sys][sSym][capaSym] = addDual(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym])
		end
	end

	return capaData_obj
end

# ! run top-Level problem
function runTopLevel!(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	bounds_tup = (0.0,0.0)
	capaData_obj = bendersData()
	subObj_fl = Inf


	# obtain sum of objective values for sub-problems
	subObj_fl = min(sum(map(x -> x.objValue, values(cutData_dic))),subObj_fl)

	# create array of expressions with duals for sub-problems
	for sub in keys(cutData_dic)
		subCut = cutData_dic[sub]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(subCut.capa[sys]), capaSym in keys(subCut.capa[sys][sSym])
				push!(cutExpr_arr,getBendersCut(subCut.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym]))
			end
		end
		
		# add benders cut to top problem
		newCut_cns = @constraint(top_m.optModel, subCut.objValue + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)) <= filter(x -> x.Ts_disSup == top_m.supTs.step[sub[1]] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var])
		top_m.parts.obj.cns[:bendersCuts] |> (x -> append!(x,DataFrame(id = i, Ts_disSup = top_m.supTs.step[sub[1]],scr = sub[2], cns = newCut_cns)))
	end


	# solve model
	@suppress begin
		optimize!(top_m.optModel)
	end

	# write technology capacites to BendersData object
	capaData_obj.capa = writeCapa(top_m)



	topObj_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	# derive lower and upper bounds on objective value
	if size(top_m.parts.obj.cns[:bendersCuts],1) > 0
		bounds_tup = (topObj_fl+value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var]),bounds_tup[2])
	else
		bounds_tup = (0.0,bounds_tup[2])
	end

	return capaData_obj, bounds_tup, topObj_fl
end

#endregion

#region # * sub-functions for benders decomposition

# ! add trust region to objective part of model
function centerTrustRegion(capa_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},radScal_fl::Float64,top_m::anyModel)

	# * match capacity values with variables
	capaExpr_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		capaExpr_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(capa_dic[sys])
			subCapa_dic = capa_dic[sys][sSym]
			capaExpr_dic[sys][sSym] = Dict(subCapa_dic[capaSym] |> (z -> capaSym => innerjoin(part_dic[sSym].var[capaSym],z, on = intCol(z,:dir))) for capaSym in filter(x -> occursin("capa",string(x)), keys(subCapa_dic)))
		end
	end

	# * write trust region constraint and save its key parameters
	
	# get quadratic left hand side
	allCapa_df = vcat(vcat(vcat(map(x -> capaExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	capaSum_expr = sum(map(x -> collect(keys(x.var.terms))[1] |> (z -> z^2 - 2*x.value*z + x.value^2),eachrow(allCapa_df)))
	
	# save scaling factor, constant of left hand side and array of capacities values
	cnsInfo_ntup =  (sca = capaSum_expr.aff.constant, cons = top_m.options.coefRng.mat[1]/minimum(abs.(collect(values(capaSum_expr.aff.terms)))), val = allCapa_df[!,:value])
	trustRegion_cns = @constraint(top_m.optModel,  capaSum_expr * cnsInfo_ntup.sca <= sum(map(x -> (x * radScal_fl)^2, cnsInfo_ntup.val))  * cnsInfo_ntup.sca)

	return trustRegion_cns, cnsInfo_ntup
end

# ! shrings the trust region around the current center according to factor
function shrinkTrustRegion!(trustReg_obj::trustRegion; shrFac_fl::Float64 = 0.5)
	# reduce factor for region
	trustReg_obj.rad = trustReg_obj.rad * shrFac_fl
	# adjust righ hand side of constraint
	cnsInfo_tup = trustReg_obj.cnsInfo
	set_normalized_rhs(trustReg_obj.cns, cnsInfo_tup.cons - sum(map(x -> (x * trustReg_obj.rad)^2, cnsInfo_tup.cons.val))  * cnsInfo_tup.cons.sca )
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = innerjoin(variable_df,sub_df, on = intCol(sub_df,:dir))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *( collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
end

# ! write capacities in input model to returned capacity dictionary
function writeCapa(in_m::anyModel)
	capa_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		capa_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts,sys)
		for sSym in filter(x -> part_dic[x].type in (:mature,:emerging),keys(part_dic))
			capa_dic[sys][sSym] = Dict(capaSym => getCapa(copy(part_dic[sSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(part_dic[sSym].var)))
		end
	end
	return capa_dic
end

# ! copy functions for benders related results
function copy(ben_obj::bendersData)
	out = bendersData()
	out.objValue = ben_obj.objValue
	out.capa = deepcopy(ben_obj.capa)
	return out
end

# ! replaces the variable column with a column storing the value of the variable
function getCapa(capa_df::DataFrame)
	# filter entries without variable
	filter!(x -> !isempty(x.var.terms),capa_df)
	# write value of variable dataframe
	capa_df[!,:value] = map(x -> round.(value(collect(keys(x.terms))[1]),digits = 5),capa_df[!,:var])
	return select(capa_df,Not([:var]))
end

# ! fixes capacity variable from second dataframe according to value in the first
function fixCapa!(value_df::DataFrame,variable_df::DataFrame)
	if !isempty(value_df)
		capaSub_df = leftjoin(variable_df,value_df,on = intCol(value_df,:dir))|> (y -> y[completecases(y),:])
		foreach(x -> x.var  |> (y -> fix(collect(keys(y.terms))[1], x.value; force = true)),eachrow(capaSub_df))
	end
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,variable_df::DataFrame)
	new_df = leftjoin(dual_df,variable_df, on = intCol(dual_df,:dir))
	new_df[!,:dual] = map(x -> dual(FixRef(collect(keys(x.terms))[1])), new_df[!,:var])
	return select(filter(x -> x.dual != 0.0,new_df),Not([:var]))
end

#endregion