#region # * functions and objects for benders specific data handling

# ! struct to save benders related results
mutable struct bendersData
	objValue::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}())
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

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersExpr(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = innerjoin(variable_df,sub_df, on = intCol(sub_df,:dir))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *( collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
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
				push!(cutExpr_arr,getBendersExpr(subCut.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym]))
			end
		end
		
		# add benders cut to top problem
		newCut_cns = @constraint(top_m.optModel, subCut.objValue + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)) <= filter(x -> x.Ts_disSup == top_m.supTs.step[sub[1]] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var])
		top_m.parts.obj.cns[:bendersCuts] |> (x -> append!(x,DataFrame(id = i, Ts_disSup = top_m.supTs.step[sub[1]],scr = sub[2], cns = newCut_cns, binds = [Int[]])))
	end


	# solve model
	@suppress begin
		optimize!(top_m.optModel)
	end

	# write technology capacites to BendersData object
	for sys in (:tech,:exc)
		capaData_obj.capa[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(part_dic)
			capaData_obj.capa[sys][sSym] = Dict(capaSym => getCapa(copy(part_dic[sSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(part_dic[sSym].var)))
		end
	end

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

#=
# ! steinbruch


import Base.isempty
isempty(ben_obj::bendersData) = isempty(ben_obj.capaTech) && isempty(ben_obj.capaExc)


# ! creates variables corresponding to limits on dual in the dual problem for bundle cutting
function createBoundElements(anyM::anyModel,variable_df::DataFrame)
	var_obj = JuMP.build_variable(error, VariableInfo(true, 0, false, NaN, false, NaN, false, NaN, false, false))
	cns_df = filter(x -> !isempty(x.var.terms),variable_df)
	cns_df[!,:lowDual] = map(x -> JuMP.add_variable(anyM.optModel, var_obj, "lowDual"), 1:size(cns_df,1))
	cns_df[!,:upDual] = map(x -> JuMP.add_variable(anyM.optModel, var_obj, "upDual"), 1:size(cns_df,1))
	cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, collect(keys(x.var.terms))[1] - x.lowDual + x.upDual == 0.0), eachrow(cns_df))

	return cns_df
end

# ! gets the product of dual variables for bounds on duals and the upper and lower values on the duals
function getBoundObjective(var_dic::Dict{Symbol,DataFrame},bound_df::DataFrame,capa::Symbol)
	# join variables for upper and lower bound
	boundObj_df = rename(var_dic[Symbol(:ccbLow,makeUp(capa))],:var => :lowVar)
	boundObj_df = rename(innerjoin(boundObj_df,var_dic[Symbol(:ccbUp,makeUp(capa))], on = intCol(bound_df,:dir)),:var => :upVar)
	# add defined bounds and create expression for objective
	boundObj_df = innerjoin(boundObj_df,bound_df, on = intCol(bound_df,:dir))
	return sum(boundObj_df[!,:lowVar] .* boundObj_df[!,:lowBound]), sum(boundObj_df[!,:upVar] .* boundObj_df[!,:upBound])
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,fixCns_df::DataFrame)
	new_df = innerjoin(dual_df,fixCns_df, on = intCol(dual_df,:dir))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns])
	return select(new_df,Not([:cns]))
end

#endregion

#region # * benders related data analysis

# eta parts eigentlich nur in iteration nach der standard iteratio notwendig
function analyseCuts(bendersCut_arr::Array{Dict{Tuple{Int64,Int64},bendersData}},ccbCover::Float64)
		
	teCut_df =  DataFrame(var = Symbol[], Ts_expSup = Int[], Ts_disSup = Int[], R_exp = Int[], C = Int[], Te = Int[], value = Float64[], dual = Float64[])
	excCut_df = DataFrame(Ts_disSup = Int[], R_from = Int[], R_to = Int[], C = Int[], dir = Bool[], value = Float64[], dual = Float64[])

	# loop ovar all current cuts
	for bendersCut in bendersCut_arr
		for x in sub_tup
			for teSym in keys(bendersCut[x].capaTech), capaSym in keys(bendersCut[x].capaTech[teSym])
				te_df = copy(bendersCut[x].capaTech[teSym][capaSym])
				te_df[!,:var] .= capaSym
				if !(:C in namesSym(te_df))
					te_df[!,:C] .= 0
				end

				append!(teCut_df,te_df)
			end

			append!(excCut_df,bendersCut[x].capaExc)
		end
	end
	# aggregate data for different scenarios (value is always the same, duals are summed)
	teCut_df = combine([:value,:dual] => (a, b) -> (value=a[1], dual=sum(b)),groupby(teCut_df,intCol(teCut_df,:var)))
	excCut_df  = combine([:value,:dual] => (a, b) -> (value=a[1], dual=sum(b)),groupby(excCut_df,intCol(excCut_df)))

	# density of cut
	density_fl = (length(findall(teCut_df[!,:dual] .!= 0.0)) + length(findall(excCut_df[!,:dual] .!= 0.0))) / (size(teCut_df,1) + size(excCut_df,1))
	produceMessage(top_mod.options,top_mod.report, 1," - Density of cut: $(round(density_fl*100,digits = 3))%")
	# share of variables that are alpha covered
	allDual_arr = abs.(vcat(teCut_df[!,:dual],excCut_df[!,:dual]))
	maxDual_fl = maximum(allDual_arr)
	alphaCover_fl = length(findall(allDual_arr .>= maxDual_fl * ccbCover)) / (size(teCut_df,1) + size(excCut_df,1))
	produceMessage(top_mod.options,top_mod.report, 1," - Alpha covered of cut: $(round(alphaCover_fl*100,digits = 2))%")
	
	cbbEta_fl = mean(allDual_arr[findall(allDual_arr .>= maxDual_fl * ccbCover)])
	produceMessage(top_mod.options,top_mod.report, 1," - Eta of cut: $(round(cbbEta_fl,;sigdigits=3))")

	# determine a random capacity not alpha covered currently,# TODO bisher nur technologies
	#noCover = filter(x -> abs.(x.dual) < maxDual_fl * ccbCover,eachrow(teCut_df))[1,:]

	return cbbEta_fl, alphaCover_fl

end

#endregion


=#