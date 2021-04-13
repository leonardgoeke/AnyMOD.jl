
#region # * benders objects

# ! struct for results of a sub-problem
mutable struct bendersData
	objVal::Float64
	capa::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}}}}
	balLvl::DataFrame
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}(),DataFrame())
end

# ! stores all information on current trust region 
mutable struct trustRegion
	objVal::Float64
	rad::Float64
	cns::ConstraintRef
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	coef::NamedTuple{(:sca,:pol),Tuple{Float64,Float64}}	
	trustRegion() = new()
end

#endregion

#region # * functions to run algorithm 


#end


#region # * sub-function to run top and sub problems of decomposition

# ! run top-Level problem
function runTopLevel!(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	capaData_obj = bendersData()

	# obtain sum of objective values for sub-problems
	subObj_fl = sum(map(x -> x.objVal, values(cutData_dic)))

	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(id = Int[], Ts_disSup = Int[],scr = Int[], cnsExpr = AffExpr[])
	for sub in keys(cutData_dic)
		subCut = cutData_dic[sub]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		# compute cut element for each capacity
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(subCut.capa[sys]), capaSym in keys(subCut.capa[sys][sSym])
				push!(cutExpr_arr,getBendersCut(subCut.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],top_m.options.coefRng.rhs[1]/top_m.options.coefRng.mat[2]))
			end
		end
		
		# compute cut expression 
		cut_expr = @expression(top_m.optModel, subCut.objVal + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)))
		
		# remove coefficients that are much smaller than the other coefficients in the problem
		if !isempty(cut_expr.terms)
			minCoef_fl = max(1.0,maximum(map(x -> abs(x[2]),collect(cut_expr.terms))))/(top_m.options.coefRng.mat[2]/top_m.options.coefRng.mat[1])
			cut_expr.terms = filter(x -> abs(x[2]) > minCoef_fl,cut_expr.terms)
		end

		# add benders variable to cut and push to dataframe of all cuts
		push!(cut_df,(id = i, Ts_disSup = top_m.supTs.step[sub[1]],scr = sub[2], cnsExpr = cut_expr - filter(x -> x.Ts_disSup == top_m.supTs.step[sub[1]] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var]))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df,top_m.options.coefRng,top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts] ,createCns(cnsCont(cut_df,:smaller),top_m.optModel))

	# solve model
	@suppress begin
		optimize!(top_m.optModel)
	end

	# write technology capacites and level of capacity balance to benders object
	capaData_obj.capa = writeCapa(top_m)
	capaData_obj.balLvl = getCapaBalLvl(top_m)

	# get objective values of top problem with and without cut
	lowerLimTrust_fl = value(sum(top_m.parts.obj.var[:objVar][!,:var]))

	return capaData_obj, lowerLimTrust_fl
end

# ! run sub-Level problem
function runSubLevel(sub_m::anyModel,capaData_obj::bendersData)

	# fixing capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in keys(capaData_obj.capa[sys][sSym])
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], capaData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if isempty(capaData_obj.capa[sys][sSym][capaSym])
					delete!(capaData_obj.capa[sys][sSym],capaSym)
				else
					fixCapa!(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],sub_m)
				end
			end
		end
	end

	# set level of capacity balance
	if :capaBal in keys(sub_m.parts.bal.cns)
		bal_df = sub_m.parts.bal.cns[:capaBal]
		bal_df = leftjoin(select(bal_df,Not([:actCapa])),capaData_obj.balLvl, on = intCol(bal_df))
		foreach(x -> set_normalized_rhs(x.cns,x.dem - x.lvl),eachrow(bal_df))
	end

	# set optimizer attributes and solves
	@suppress begin
		optimize!(sub_m.optModel)
	end

	# add duals and objective value to capacity data
	capaData_obj.objVal = objective_value(sub_m.optModel)

	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys]), capaSym in keys(capaData_obj.capa[sys][sSym])
			capaData_obj.capa[sys][sSym][capaSym] = addDual(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].cns[Symbol(capaSym,:BendersFix)])
		end
	end

	return capaData_obj
end

#endregion

#region # * utility functions for benders algorithm

# ! add trust region to objective part of model
function centerTrustRegion(capa_dic::Union{Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}}}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}},top_m::anyModel)

	# * match capacity values with variables
	capaExpr_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		capaExpr_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(capa_dic[sys])
			subCapa_dic = capa_dic[sys][sSym]
			capaExpr_dic[sys][sSym] = Dict(deSelectSys(subCapa_dic[capaSym]) |> (z -> capaSym => innerjoin(deSelectSys(part_dic[sSym].var[capaSym]),z, on = intCol(z,:dir))) for capaSym in filter(x -> occursin("capa",string(x)), keys(subCapa_dic)))
		end
	end

	# * write trust region constraint and save its key parameters
	
	# get quadratic left hand side
	allCapa_df = vcat(vcat(vcat(map(x -> capaExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	allCapa_df[!,:value] = map(x -> x.value < top_m.options.coefRng.mat[1]/top_m.options.coefRng.mat[2] ? 0.0 : x.value,eachrow(allCapa_df))
	capaSum_expr = sum(map(x -> collect(keys(x.var.terms))[1] |> (z -> z^2 - 2*x.value*z + x.value^2),eachrow(allCapa_df)))
	
	# save scaling factor, constant of left hand side and array of capacities values
	cnsInfo_ntup =  (sca = top_m.options.coefRng.mat[1]/minimum(abs.(collect(values(capaSum_expr.aff.terms)))), pol = capaSum_expr.aff.constant)
	trustRegion_cns = @constraint(top_m.optModel,  capaSum_expr * cnsInfo_ntup.sca <= trustReg_obj.rad^2  * cnsInfo_ntup.sca)

	return trustRegion_cns, cnsInfo_ntup
end

# ! shrings the trust region around the current center according to factor
function shrinkTrustRegion!(trustReg_obj::trustRegion; shrFac_fl::Float64 = 0.5)
	# reduce factor for region
	trustReg_obj.rad = trustReg_obj.rad * shrFac_fl
	# adjust righ hand side of constraint
	cnsInfo_tup = trustReg_obj.coef
	set_normalized_rhs(trustReg_obj.cns,  (trustReg_obj.rad^2   - cnsInfo_tup.pol) * cnsInfo_tup.sca)
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, variable_df::DataFrame,low_fl::Float64)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(variable_df),z, on = intCol(z,:dir)))
	ben_df[!,:value] = map(x -> low_fl > x ? 0.0 : x,ben_df[!,:value]) # corrects capacity in cut to zero, if it was below threshold
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *(collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
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
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	out.balLvl = deepcopy(ben_obj.balLvl)
	return out
end

# ! replaces the variable column with a column storing the value of the variable
function getCapa(capa_df::DataFrame)
	# filter entries without variable
	filter!(x -> !isempty(x.var.terms),capa_df)
	# write value of variable dataframe
	capa_df[!,:value] = map(x -> value(collect(keys(x.terms))[1]),capa_df[!,:var])

	return select(capa_df,Not([:var]))
end

# ! create constraint fixing capacity
function fixCapa!(value_df::DataFrame,variable_df::DataFrame,capa_sym::Symbol,part_obj::AbstractModelPart,fix_m::anyModel,th_fl::Union{Float64,Nothing}=nothing)

	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(variable_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])

	# sets capacities below threshold to zero, if a threshold is defined
	if !isnothing(th_fl)
		fix_df[!,:value] = map(x -> x >= th_fl ? x : 0.0,fix_df[!,:value])
	end

	# extract actual variable
	fix_df[!,:var] = map(x -> collect(keys(x.var.terms))[1], eachrow(fix_df))
	# compute scaling factor
	fix_df[!,:scale] = map(x -> x.value <= fix_m.options.coefRng.rhs[1] ? min(fix_m.options.coefRng.mat[2],fix_m.options.coefRng.rhs[1]/x.value) : 1.0, eachrow(fix_df))
	fix_df[!,:scale] = map(x -> x.value >= fix_m.options.coefRng.rhs[2] ? min(fix_m.options.coefRng.mat[1],fix_m.options.coefRng.rhs[2]/x.value) : x.scale, eachrow(fix_df))

	# compute righ-hand side and factor of variables for constraint
	fix_df[!,:rhs] = map(x -> x.value * x.scale |> (u -> round(u,digits = 4) >= fix_m.options.coefRng.rhs[1] ? u : 0.0), eachrow(fix_df))
	fix_df[!,:fac] = map(x -> x.rhs == 0.0 ? 1.0 : x.scale, eachrow(fix_df))

	if !(Symbol(capa_sym,:BendersFix) in keys(part_obj.cns))
		# create actual constraint and attach to model part
		fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac == x.rhs),eachrow(fix_df))
		part_obj.cns[Symbol(capa_sym,:BendersFix)] = select(fix_df,Not([:var,:value,:scale,:rhs]))
	else
		# adjust rhs and factor of existing constraint
		fix_df = innerjoin(select(part_obj.cns[Symbol(capa_sym,:BendersFix)],Not([:fac])),fix_df,on = intCol(fix_df,:dir))
		set_normalized_rhs.(fix_df[!,:cns], fix_df[!,:rhs])
		set_normalized_coefficient.(fix_df[!,:cns], fix_df[!,:var], fix_df[!,:fac])	
	end

	part_obj.cns[Symbol(capa_sym,:BendersFix)] = select(fix_df,Not([:var,:value,:scale,:rhs]))

end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,cns_df::DataFrame)
	new_df = deSelectSys(cns_df) |> (z -> leftjoin(dual_df,z, on = intCol(z,:dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac]
	return select(filter(x -> x.dual != 0.0,new_df),Not([:cns,:fac]))
end

# ! writesl level of capacity balance that needs to be enforced in subproblems 
function getCapaBalLvl(mod_m::anyModel)
	if :capaBal in keys(mod_m.parts.bal.cns)
		lvl_df =  copy(mod_m.parts.bal.cns[:capaBal])
		# add missing capacity variable if defined
		if :missCapa in keys(mod_m.parts.bal.var)
			lvl_df = joinMissing(lvl_df,mod_m.parts.bal.var[:missCapa],intCol(lvl_df),:left,Dict(:var => AffExpr()))
		else
			lvl_df[!,:var] .= AffExpr()
		end

		# compute by how much rhs of capacity balance has to be adjusted in subproblem, first term gives over capacits in top-problem, second gives missing capacities already accounted for by top objective
		lvl_df[!,:lvl] = lvl_df[!,:actCapa] .* (normalized_rhs.(lvl_df[!,:cns]) - value.(lvl_df[!,:cns])) + value.(lvl_df[!,:var])
		select!(lvl_df,Not([:var,:cns]))
	else
		lvl_df = DataFrame()
	end
	return lvl_df
end	

#endregion