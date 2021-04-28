
#region # * objects for decomposition

# ! struct for results of a sub-problem
mutable struct bendersData
	objVal::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}())
end

# ! copy functions for benders related results
function copy(ben_obj::bendersData)
	out = bendersData()
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	return out
end

# ! stores all information on current trust region 
mutable struct trustRegion
	objVal::Float64
	rad::Float64
	abs::Float64
	cns::ConstraintRef
	exp::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	coef::NamedTuple{(:sca,:pol),Tuple{Float64,Float64}}	
	opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:extThrs),Tuple{Float64,Float64,Float64,Float64}}	

	function trustRegion(exp_df::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trust_opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:extThrs),Tuple{Float64,Float64,Float64,Float64}})
		trustReg_obj = new()
		trustReg_obj.opt = trust_opt
		trustReg_obj.exp = exp_df
		trustReg_obj.abs = sum(vcat(vcat(vcat(map(x -> trustReg_obj.exp[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...))
		trustReg_obj.rad = trustReg_obj.abs * trustReg_obj.opt.startRad
		return trustReg_obj
	end
end

#endregion

#region # * top level functions of iteration algorithm

# ! set optimizer attributes and options, in case gurobi is used (recommended)
function prepareMod!(mod_m::anyModel,opt_obj::DataType)
	# create optimization problem
	createOptModel!(mod_m)
	setObjective!(:cost,mod_m)
	# set optimizer and attributes, if gurobi
	set_optimizer(mod_m.optModel,opt_obj)
	if opt_obj <: Gurobi.Optimizer
		set_optimizer_attribute(mod_m.optModel, "Method", 2)
		set_optimizer_attribute(mod_m.optModel, "Crossover", 0)
	end
end

# ! run top problem and sub problems with results of heuristic solution
function heuristicCut(heu_m::anyModel,top_m::anyModel,sub_dic::Dict{Tuple{Int64,Int64},anyModel},trustReg_obj::trustRegion,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},capaReport_df::DataFrame,inputMod_arr::Array{String,1},resultDir_str::String,opt_obj::DataType)
	
	sub_tup = collect(keys(sub_dic))
	
	# write solution of heuristic model to benders object and fix top problem to these values
	capaData_obj = bendersData()
	
	for sys in (:tech,:exc)
		capaData_obj.capa[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		partHeu_dic = getfield(heu_m.parts,sys)
		partTop_dic = getfield(top_m.parts,sys)
		for sSym in keys(partHeu_dic)
			part = partTop_dic[sSym]
			# write to capacity data object
			capaData_obj.capa[sys][sSym] = Dict(capaSym => getResult(copy(partHeu_dic[sSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(partHeu_dic[sSym].var)))
			 # sets capacity of heuristic solution as upper limits for top, fixing could cause infeasibility due to imprecisions
			for capaSym in filter(x -> occursin("capa",string(x)), keys(partHeu_dic[sSym].var))
				fixCapa!(capaData_obj.capa[sys][sSym][capaSym],part.var[capaSym],capaSym,part,top_m,false)
			end
		end
	end

	# change objective of top problem and solve
	@objective(top_m.optModel, Max, getPhaseVar(top_m,capaData_obj.capa))
	set_optimizer_attribute(top_m.optModel, "Crossover", 1)
	@suppress optimize!(top_m.optModel)

	# fix variables for phase to current values
	topCapa_dic = writeResult(top_m,:capa)
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(topCapa_dic[sys])
			for capaSym in keys(topCapa_dic[sys][sSym])
				fixCapa!(topCapa_dic[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],top_m)
			end
		end
	end


	# re-set objective
	@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,:var] / top_m.options.scaFac.obj)

	# solve top problem with fixed capacites and save objective
	@suppress optimize!(top_m.optModel)

	if primal_status(top_m.optModel) != MOI.ResultStatusCode(1)
		printIIS(top_m)
	end

	capaData_obj.objVal = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	println("kosten invest: $(capaData_obj.objVal)")

	# run subproblems
	@threads for x in collect(sub_tup)
		dual_etr = runSubLevel(sub_dic[x],copy(capaData_obj))
		cutData_dic[x] = dual_etr
	end

	# re-create top problem again (is faster than just deleting constraints fixing variables)
	@suppress begin
		top_m = anyModel(inputMod_arr,resultDir_str, objName = "topModel", supTsLvl = 2, reportLvl = 1, shortExp = 5)
		top_m.subPro = tuple(0,0)
		prepareMod!(top_m,opt_obj)
	end

	# create seperate variables for costs of subproblems and aggregate them (cannot be part of model creation, because requires information about subproblems) 
	top_m.parts.obj.var[:cut] = map(y -> map(x -> y == 1 ? top_m.supTs.step[x] : sub_tup[x][2], 1:length(sub_tup)),1:2) |> (z -> createVar(DataFrame(Ts_disSup = z[1], scr = z[2]),"subCut",NaN,top_m.optModel,top_m.lock,top_m.sets, scaFac = 1e2))
	push!(top_m.parts.obj.cns[:objEqn], (name = :aggCut, group = :benders, cns = @constraint(top_m.optModel, sum(top_m.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])))

	# add trust region constraint to top problem
	trustReg_obj.cns, trustReg_obj.coef = centerTrustRegion(trustReg_obj.exp,top_m,trustReg_obj.rad);
	
	# save total costs of heuristic solution
	trustReg_obj.objVal = capaData_obj.objVal + sum(map(x -> x.objVal, values(cutData_dic)))
	
	# write capacity results 
	reportCapa!(0,cutData_dic,capaReport_df)

	return top_m, trustReg_obj, cutData_dic, capaReport_df

end

# ! run top-Level problem
function runTopLevel(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	capaData_obj = bendersData()

	# obtain sum of objective values for sub-problems
	subObj_fl = sum(map(x -> x.objVal, values(cutData_dic)))

	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_disSup = Int[],scr = Int[], limCoef = Bool[], cnsExpr = AffExpr[])
	for sub in keys(cutData_dic)

		subCut = cutData_dic[sub]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		# compute cut element for each capacity
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(subCut.capa[sys]), capaSym in keys(subCut.capa[sys][sSym])
				push!(cutExpr_arr,getBendersCut(subCut.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym]))
			end
		end
		
		# get cut variable and compute cut expression 
		cut_var = filter(x -> x.Ts_disSup == top_m.supTs.step[sub[1]] && x.scr == sub[2], top_m.parts.obj.var[:cut])[1,:var]
		cut_expr = @expression(top_m.optModel, subCut.objVal + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)))
		
		# remove extremely small terms and limit the coefficient of extremely large terms
		
		limCoef_boo = false

		if !isempty(cut_expr.terms)
			maxRng_fl = top_m.options.coefRng.mat[2]/top_m.options.coefRng.mat[1] # maximum range of coefficients
			cutCoef_fl = collect(values(cut_var.terms))[1]	# scaling factor of cut variable
			
			# remove small coefficients otherwise volating range
			minCoef_fl = max(maximum(map(x -> abs(x[2]),collect(cut_expr.terms))),cut_expr.constant* top_m.options.coefRng.mat[2]/top_m.options.coefRng.rhs[2])/maxRng_fl
			cut_expr.terms = filter(x -> abs(x[2]) > minCoef_fl,cut_expr.terms)

			# check if the cut also would violate range, limit coefficients in this case
			if cutCoef_fl < minCoef_fl
				limCoef_boo = true
				maxCoef_fl = cutCoef_fl * maxRng_fl # biggest possible coefficient, so cut variable is still in range
				foreach(x -> abs(cut_expr.terms[x]) > maxCoef_fl ? cut_expr.terms[x] = maxCoef_fl : nothing, collect(keys(cut_expr.terms))) # limits coefficients to maximum value
			end
		end


		# add benders variable to cut and push to dataframe of all cuts
		push!(cut_df,(i = i, Ts_disSup = top_m.supTs.step[sub[1]],scr = sub[2], limCoef = limCoef_boo, cnsExpr = cut_expr - cut_var))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df,top_m.options.coefRng,top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts] ,createCns(cnsCont(cut_df,:smaller),top_m.optModel))

	# solve model
	@suppress begin
		optimize!(top_m.optModel)
	end

	if primal_status(top_m.optModel) != MOI.ResultStatusCode(1)
		printIIS(top_m)
	end

	# write technology capacites and level of capacity balance to benders object
	capaData_obj.capa, expTrust_dic = [writeResult(top_m,x) for x in [:capa,:exc]]

	# get objective value of top problem
	objTopTrust_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	lowLimTrust_fl = objTopTrust_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])

	return capaData_obj, expTrust_dic, objTopTrust_fl, lowLimTrust_fl
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

# ! solves top problem without trust region and obtains lower limits
function runTopWithoutTrust(mod_m::anyModel,trustReg_obj::trustRegion)
	# solve top again with trust region and re-compute bound for soultion
	delete(mod_m.optModel,trustReg_obj.cns)
	set_optimizer_attribute(mod_m.optModel, "Crossover", 1) # add crossover to get an exact solution
	@suppress optimize!(mod_m.optModel)
	set_optimizer_attribute(mod_m.optModel, "Crossover", 0)

	# obtain different objective values
	objTop_fl = value(sum(filter(x -> x.name == :cost, mod_m.parts.obj.var[:objVar])[!,:var])) # costs of unconstrained top-problem
	lowLim_fl = objTop_fl + value(filter(x -> x.name == :benders,mod_m.parts.obj.var[:objVar])[1,:var]) # objective (incl. benders) of unconstrained top-problem

	return objTop_fl, lowLim_fl

end

#endregion

#region # * trust region functions

# ! dynamically adjusts the trust region
function adjustTrustRegion(top_m::anyModel,expTrust_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trustReg_obj::trustRegion,objSub_fl::Float64,objTopTrust_fl::Float64,lowLim_fl::Float64,lowLimTrust_fl::Float64)
	
	# re-create trust region
	if (objTopTrust_fl + objSub_fl) < trustReg_obj.objVal # recenter trust region, if new best solution was obtained
		changeTrustExp!(expTrust_dic,trustReg_obj) # writes relevant expansion of top problem to trust region 
		trustReg_obj.cns, trustReg_obj.coef  = centerTrustRegion(trustReg_obj.exp,top_m,trustReg_obj.rad)
		trustReg_obj.objVal = objTopTrust_fl + objSub_fl
		produceMessage(top_m.options,top_m.report, 1," - Re-centered trust-region!", testErr = false, printErr = false)
	else
		trustReg_obj.cns, trustReg_obj.coef = centerTrustRegion(trustReg_obj.exp,top_m,trustReg_obj.rad)
		if abs(1 - lowLimTrust_fl / (objTopTrust_fl + objSub_fl)) < trustReg_obj.opt.extThrs # extend trust region, if constrained top problem converged
			resizeTrustRegion!(trustReg_obj,1.5)
			produceMessage(top_m.options,top_m.report, 1," - Extended trust-region!", testErr = false, printErr = false)
		elseif abs(1 - lowLim_fl / lowLimTrust_fl) < trustReg_obj.opt.shrThrs && trustReg_obj.rad > (trustReg_obj.abs * trustReg_obj.opt.lowRad) # shrink trust region, if it does not constrain the top problem and the lower limit for its size is not yet reached
			resizeTrustRegion!(trustReg_obj,0.5)
			produceMessage(top_m.options,top_m.report, 1," - Shrunk trust-region!", testErr = false, printErr = false)	
		end
	end

	return trustReg_obj
end

# ! write values in exp_dic with corresponding entries in trust region to these entries
function changeTrustExp!(exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trustReg_obj::trustRegion)
	# loop over all fields of exp_dic
	for sys in (:tech,:exc)
		for sSym in keys(exp_dic[sys])
			for expSym in keys(exp_dic[sys][sSym]) 
				trustReg_obj.exp[sys][sSym][expSym] = select(trustReg_obj.exp[sys][sSym][expSym],Not([:value])) |> (z -> innerjoin(z,exp_dic[sys][sSym][expSym],on = intCol(z,:dir)))	# write capacities where they match		
			end
		end
	end
end

# ! add trust region to objective part of model
function centerTrustRegion(exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel,rad_fl::Float64)

	# * match capacity values with variables
	expExpr_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		expExpr_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(exp_dic[sys])
			subExp_dic = exp_dic[sys][sSym]
			expExpr_dic[sys][sSym] = Dict(deSelectSys(subExp_dic[expSym]) |> (z -> expSym => innerjoin(deSelectSys(unique(select(part_dic[sSym].var[expSym],Not([:Ts_expSup,:Ts_disSup])))),z, on = intCol(z,:dir))) for expSym in filter(x -> occursin("exp",string(x)), keys(subExp_dic)))
		end
	end

	# * write trust region constraint and save its key parameters
	
	# get quadratic left hand side
	allExp_df = vcat(vcat(vcat(map(x -> expExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	allExp_df[!,:value] = map(x -> x.value < 0.01 / top_m.options.scaFac.capa ? 0.0 : x.value,eachrow(allExp_df)) # set current capacity to zero, if value is small than one megawatt
	capaSum_expr = sum(map(x -> collect(keys(x.var.terms))[1] |> (z -> z^2 - 2*x.value*z + x.value^2),eachrow(allExp_df)))
	
	# save scaling factor, constant of left hand side and array of capacities values
	cnsInfo_ntup =  (sca = 10*top_m.options.coefRng.mat[1]/minimum(abs.(collect(values(capaSum_expr.aff.terms)) |> (z -> isempty(z) ? [1.0] : z))), pol = capaSum_expr.aff.constant)
	trustRegion_cns = @constraint(top_m.optModel,  capaSum_expr * cnsInfo_ntup.sca <= rad_fl^2  * cnsInfo_ntup.sca)

	return trustRegion_cns, cnsInfo_ntup
end

# ! shrings the trust region around the current center according to factor
function resizeTrustRegion!(trustReg_obj::trustRegion, shrFac_fl::Float64)
	# reduce factor for region
	trustReg_obj.rad = trustReg_obj.rad * shrFac_fl
	# adjust righ hand side of constraint
	cnsInfo_tup = trustReg_obj.coef
	set_normalized_rhs(trustReg_obj.cns,  (trustReg_obj.rad^2   - cnsInfo_tup.pol) * cnsInfo_tup.sca)
end

#endregion

#region # * phase switchting functions 

# ! remove benders cuts from previous iteration that included limited coefficients to avoid numerical trouble			
function rmvLimitCuts!(mod_m::anyModel)
	foreach(x -> delete(mod_m.optModel,x.cns), filter(x -> x.limCoef,eachrow(mod_m.parts.obj.cns[:bendersCuts])))
	mod_m.parts.obj.cns[:bendersCuts] = DataFrame(filter(x -> !(x.limCoef),eachrow(mod_m.parts.obj.cns[:bendersCuts])))
end

# ! fixes variables in exp_dic to 0.01 GW or the next biggest values possible
function fixPhase!(mod_m::anyModel,exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}})

	# set upper limits for phase variables
	switchExpPhase!(mod_m,exp_dic,:unfix)
	switchExpPhase!(mod_m,exp_dic,:up, 0.01/mod_m.options.scaFac.capa)

	# change objective of top problem and solve
	@objective(mod_m.optModel, Max, getPhaseVar(mod_m,exp_dic))
	optimize!(mod_m.optModel)

	# fix variables for phase to current values
	switchExpPhase!(mod_m,exp_dic,:fix, NaN)

	# re-set objective
	@objective(mod_m.optModel, Min, mod_m.parts.obj.var[:obj][1,:var] / mod_m.options.scaFac.obj)

end

# ! function to fix or unfix capacites for different phases
function switchExpPhase!(mod_m::anyModel,exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},opr_sym::Symbol,fix_fl::Float64 = 0.0)
	# loop over all capacity variables in input dictionary
	for sys in (:tech,:exc)
		modSys_obj = getfield(mod_m.parts,sys)
		for sSym in keys(exp_dic[sys])
			modPart_obj = modSys_obj[sSym]
			for expSym in keys(exp_dic[sys][sSym])
				fixUnfix!(exp_dic[sys][sSym][expSym],unique(select(modPart_obj.var[expSym],Not([:Ts_expSup,:Ts_disSup]))),opr_sym,fix_fl)
			end
		end
	end
end

# ! fixes or unfixed variables in variable_df that can be matched with entries in value_df
function fixUnfix!(value_df::DataFrame,variable_df::DataFrame,opr_sym::Symbol,fix_fl::Float64)
	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(variable_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])
	# extract actual variable
	fix_df[!,:var] = map(x -> collect(keys(x.var.terms))[1], eachrow(fix_df))

	if opr_sym == :fix # fix corresponding capacities to zero
		if isnan(fix_fl)
			foreach(x -> fix(x.var,value(x.var); force = true),eachrow(fix_df))
		else
			foreach(x -> fix(x.var,fix_fl; force = true),eachrow(fix_df))
		end
	elseif opr_sym == :unfix # unfix capacities (and re-enforce lower limit of zero)
		foreach(x -> unfix(x.var),eachrow(fix_df))
		foreach(x -> set_lower_bound(x.var,0.0),eachrow(fix_df))
	elseif opr_sym == :up
		foreach(x -> set_upper_bound(x.var,fix_fl),eachrow(fix_df))
	end	
end

# ! obtaines sum of all variables relating to entries to exp_dic
function getPhaseVar(mod_m::anyModel,exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}})
	allVar_arr = Array{AffExpr,1}()
	for sys in (:tech,:exc)
		modSys_obj = getfield(mod_m.parts,sys)
		for sSym in keys(exp_dic[sys])
			modPart_obj = modSys_obj[sSym]
			for expSym in keys(exp_dic[sys][sSym])
				push!(allVar_arr,sum(deSelectSys(exp_dic[sys][sSym][expSym]) |>  (z -> leftjoin(modPart_obj.var[expSym],z,on = intCol(z,:dir))) |> (y -> unique(y[completecases(y),:var]))))
			end
		end
	end
	return sum(allVar_arr)
end

#endregion

#region # * utility functions for decomposition

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(variable_df),z, on = intCol(z,:dir)))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *(collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
end

# ! write capacities or expansion in input model to returned capacity dictionary
function writeResult(in_m::anyModel, resType::Symbol, filterFunc::Function = x -> true)
	res_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()

	fixPar_dic = Dict(:expStSize => :sizeToStOutFixExp,:expStIn => :stOutToStInFixExp)

	for sys in (:tech,:exc)
		res_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts,sys)
		for sSym in filter(x -> part_dic[x].type in (:mature,:emerging),keys(part_dic))
			res_dic[sys][sSym] = Dict(capaSym => filter(filterFunc, getResult(copy(part_dic[sSym].var[capaSym]))) for capaSym in filter(x -> occursin(string(resType),string(x)), keys(part_dic[sSym].var)))
			if resType == :exp
				# check if storage expansion is fixed to storage output and removes variables in these cases
				for stExp in intersect([:expStSize,:expStIn],collect(keys(res_dic[sys][sSym])))
					if fixPar_dic[stExp] in collect(keys(part_dic[sSym].cns))	
						fixCns_df = select(part_dic[sSym].cns[fixPar_dic[stExp]],Not([:cns]))
						res_dic[sys][sSym][stExp] = res_dic[sys][sSym][stExp] |> (x -> antijoin(x,fixCns_df, on = intCol(x)))				
					end
				end
			end
		end
	end
	return res_dic
end

# ! replaces the variable column with a column storing the value of the variable
function getResult(res_df::DataFrame)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		res_df = unique(select(res_df,Not([:Ts_expSup,:Ts_disSup])))
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms),res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> value(collect(keys(x.terms))[1]) |> (z -> z < 1e-8 ? 0.0 : z),res_df[!,:var])

	return select(res_df,Not([:var]))
end

# ! create constraint fixing capacity (or setting a lower limits)
function fixCapa!(value_df::DataFrame,variable_df::DataFrame,capa_sym::Symbol,part_obj::AbstractModelPart,fix_m::anyModel,fix_boo::Bool=true)

	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(variable_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])

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
		fix_df[!,:cns] = map(x -> fix_boo ? @constraint(fix_m.optModel,x.var * x.fac == x.rhs) : @constraint(fix_m.optModel,x.var * x.fac <= x.rhs),eachrow(fix_df))
		part_obj.cns[Symbol(capa_sym,fix_boo ? :BendersFix : :BendersUp)] = select(fix_df,Not([:var,:value,:scale,:rhs]))
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

# ! write technology capacities to reporting dataframe
function reportCapa!(i::Int,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},capaReport_df::DataFrame)
	# report capacities for technologies
	for x in collect(keys(cutData_dic))
		for sSym in keys(cutData_dic[x].capa[:tech])
			subCapa_dic = cutData_dic[x].capa[:tech][sSym]
			for capaSym in keys(subCapa_dic)
				add_df = copy(subCapa_dic[capaSym])
				add_df[!,:variable] .= string(capaSym)
				add_df[!,:i] .= i
				foreach(x -> add_df[!,x] .= 0 ,setdiff(namesSym(capaReport_df),namesSym(add_df)))
				append!(capaReport_df,add_df)
			end
		end
	end
end


#endregion




