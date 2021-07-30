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
mutable struct quadTrust
	objVal::Float64
	rad::Float64
	abs::Float64
	cns::ConstraintRef
	exp::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	coef::NamedTuple{(:sca,:pol),Tuple{Float64,Float64}}	
	opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:extThrs),Tuple{Float64,Float64,Float64,Float64}}	

	function quadTrust(exp_df::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trust_opt::NamedTuple{(:startRad,:lowRad,:shrThrs,:extThrs),Tuple{Float64,Float64,Float64,Float64}})
		trustReg_obj = new()
		trustReg_obj.opt = trust_opt
		trustReg_obj.exp = exp_df
		trustReg_obj.abs = sum(vcat(vcat(vcat(map(x -> trustReg_obj.exp[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...))
		trustReg_obj.rad = trustReg_obj.abs * trustReg_obj.opt.startRad
		return trustReg_obj
	end
end

#endregion

#region # * functions for heurstic

# ! run heuristic and return exact capacities plus heuristic cut
function heuristicSolve(sub_dic::Dict{Tuple{Int64,Int64},anyModel},modOpt_tup::NamedTuple,sub_tup::Tuple,zeroThrs_fl::Float64,redFac::Float64, t_int::Int)

	#region # * solve heuristic model and get exact results
	
	# create and solve model
	heu_m = anyModel(modOpt_tup.heuIn, modOpt_tup.resultDir, objName = "heuristicModel_" *string(round(redFac,digits = 3)), supTsLvl = modOpt_tup.supTsLvl, reportLvl = 2, shortExp = modOpt_tup.shortExp, redStep = redFac)
	prepareMod!(heu_m,modOpt_tup.opt)
	set_optimizer_attribute(heu_m.optModel, "Threads", t_int)
	optimize!(heu_m.optModel)


	# create top level problem
	top_m = anyModel(modOpt_tup.modIn,modOpt_tup.resultDir, objName = "topModel", supTsLvl = modOpt_tup.supTsLvl, reportLvl = 1, shortExp = modOpt_tup.shortExp)
	top_m.subPro = tuple(0,0)
	prepareMod!(top_m,modOpt_tup.opt)

	# get feasible capacities (due to imprecisions of barrier heuristic solution can be infeasible)
	heuData_obj = getFeasResult(heu_m,top_m,zeroThrs_fl)
	
	#endregion
	
	#region # * create heuristic cut
	
	# fix variables of top problem to exact values
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(heuData_obj.capa[sys]), capaSym in filter(x -> occursin("capa",string(x)), collect(keys(heuData_obj.capa[sys][sSym])))
			limitCapa!(heuData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],top_m,:Fix,false)
		end
	end

	# re-set objective to costs and solve
	@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,:var] / top_m.options.scaFac.obj)
	@suppress optimize!(top_m.optModel)

	heuData_obj.objVal = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))

	# run subproblems and get cut info
    cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()
	@threads for x in collect(sub_tup)
		dual_etr = runSubLevel(sub_dic[x],copy(heuData_obj))
		cutData_dic[x] = dual_etr
	end

	#endregion

	return heuData_obj, cutData_dic
end

# ! get feasible results from solve heuristic problem
function getFeasResult(heu_m::anyModel,top_m::anyModel,zeroThrs_fl::Float64)

    # create absolute value constraints for capacities or expansion variables
    for sys in (:tech,:exc)
        partHeu_dic = getfield(heu_m.parts,sys)
        partTop_dic = getfield(top_m.parts,sys)
        for sSym in keys(partHeu_dic)
            part = partTop_dic[sSym]
			# uses capacity variables for system with decommissioning, otherwise expansion 
			trust_str = part.decomm == :none ? "exp" : "capa" 
			# obtain scaling factor used for expansion or capacity variables
			scaFac_fl = part.decomm == :none ? top_m.options.scaFac.insCapa : top_m.options.scaFac.capa
            # write to capacity data object
            var_dic = Dict(varSym => getResult(copy(partHeu_dic[sSym].var[varSym])) for varSym in filter(x -> occursin(trust_str,string(x)), keys(partHeu_dic[sSym].var)))
            # create variabbles and writes constraints to minimize absolute value of capacity delta
            for varSym in filter(x -> occursin(trust_str,string(x)), keys(partHeu_dic[sSym].var))
				var_df = part.var[varSym] |> (w -> part.decomm == :none ? collapseExp(w) : w)
				abs_df = deSelectSys(var_dic[varSym]) |>  (z -> leftjoin(var_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])
				# set variables below threshold to zero and scales value according to factor used in original variable
				scaFac_fl = part.decomm == :none ? top_m.options.scaFac.insCapa : top_m.options.scaFac.capa
				abs_df[!,:value] = map(x -> (x.value > (zeroThrs_fl/scaFac_fl) ? x.value : 0.0) * collect(values(x.var.terms))[1],eachrow(abs_df))
                # create variable for absolute value and connect with rest of dataframe again
                part.var[Symbol(:abs,makeUp(varSym))] = createVar(select(abs_df,Not([:var,:value])), string(:abs,makeUp(varSym)),top_m.options.bound.capa,top_m.optModel, top_m.lock,top_m.sets; scaFac = scaFac_fl)
                abs_df[!,:varAbs] .= part.var[Symbol(:abs,makeUp(varSym))][!,:var]
                # create constraints for absolute value
                abs_df[!,:absLow] = map(x -> x.varAbs - x.var + x.value,eachrow(abs_df))
                abs_df[!,:absUp] = map(x -> x.varAbs  + x.var - x.value,eachrow(abs_df)) 
                part.cns[Symbol(varSym,:AbsLow)] = createCns(cnsCont(rename(orderDf(abs_df[!,[intCol(abs_df)...,:absLow]]),:absLow => :cnsExpr),:greater),top_m.optModel)
                part.cns[Symbol(varSym,:AbsUp)] = createCns(cnsCont(rename(orderDf(abs_df[!,[intCol(abs_df)...,:absUp]]),:absUp => :cnsExpr),:greater),top_m.optModel)
            end
        end
    end
	
    # change objective of top problem and solve
	absVar_arr = [:CapaConv,:CapaExc,:CapaStOut,:CapaStIn,:CapaStSize,:ExpConv,:ExpExc,:ExpStOut,:ExpStIn,:ExpStSize]
    @objective(top_m.optModel, Min, sum(map(x -> sum(getAllVariables(Symbol(:abs,x),top_m) |> (w -> isempty(w) ? [AffExpr()] : w[!,:var])),absVar_arr)))
    set_optimizer_attribute(top_m.optModel, "Crossover", 1)
    @suppress optimize!(top_m.optModel)

    # write precise results for capacity and expansion
    capaData_obj = bendersData()
	capaData_obj.capa = writeResult(top_m,[:exp,:capa])

    return capaData_obj
end

# ! get limits imposed on by linear trust region
function getLinTrust(val1_fl::Float64,val2_fl::Float64,thres_fl::Float64)

	if val1_fl == 0.0 && val2_fl == 0.0 # fix to zero, if both values are zero
		val_arr, cns_arr = [0.0], [:Fix]
	elseif val1_fl != 0.0 && val2_fl == 0.0 # set second value as upper limits, if only this one is zero
		val_arr, cns_arr = [val1_fl], [:Up]
	elseif val1_fl == 0.0 && val2_fl != 0.0 # set second value as upper limits, if only this one is zero
		val_arr, cns_arr = [val2_fl], [:Up]
	elseif abs(val1_fl/val2_fl-1) > thres_fl # set to mean, if difference does not exceed threshold
		val_arr, cns_arr = sort([val1_fl,val2_fl]), [:Low,:Up]
	else # enfore lower and upper limits, if difference does exceed threshold	
		val_arr, cns_arr = [(val1_fl+val2_fl)/2], [:Fix]
	end
		
	return val_arr, cns_arr
end

# ! get variables controlling unfixed part of linear trust region
function getNonFixLin(top_m::anyModel,capaData_obj::bendersData)

	var_dic = Dict(x => Dict{Symbol,Dict{Symbol,DataFrame}}() for x in [:tech,:exc])

	for sys in (:exc,:tech)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])	
			var_dic[sys][sSym] = Dict{Symbol,Dict{Symbol,DataFrame}}() # create empty dataframe for values
			sys_obj = part_dic[sSym] # get part object of system
			trust_str = part_dic[sSym].decomm == :none ? "exp" : "capa" # uses capacity variables for system with decommissioning, otherwise use expansion
			for trstSym in filter(x -> occursin(trust_str,string(x)), collect(keys(capaData_obj.capa[sys][sSym])))
				# get capacities that are not fixed
				relCns_arr = intersect([Symbol(trstSym,:Benders,x) for x in [:Up,:Low]],keys(sys_obj.cns))
				if !isempty(relCns_arr)
					# get all cases where variable is not fixed
					limCapa_df = vcat(map(x -> select(sys_obj.cns[x],Not([:cns,:fac])),relCns_arr)...) 
					# get un-fixed variables
					selCol_arr = filter(x -> x != :Ts_expSup,intCol(limCapa_df,:var))
					var_dic[sys][sSym][trstSym] = unique(innerjoin(select(sys_obj.var[trstSym],selCol_arr),select(limCapa_df,selCol_arr),on = selCol_arr)) |> (w -> innerjoin(w,capaData_obj.capa[sys][sSym][trstSym],on = intCol(w)))
					# remove if no capacities remain
					if isempty(var_dic[sys][sSym][trstSym]) delete!(var_dic[sys][sSym],trstSym) end
				end
			end
			# remove entire system if not capacities
			if isempty(var_dic[sys][sSym]) delete!(var_dic[sys],sSym) end
		end
	end
	return var_dic
end

# ! solves top problem without trust region and obtains lower limits
function runTopWithoutQuadTrust(mod_m::anyModel,trustReg_obj::quadTrust)
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

# ! dynamically adjusts the trust region
function adjustQuadTrust(top_m::anyModel,expTrust_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},trustReg_obj::quadTrust,objSub_fl::Float64,objTopTrust_fl::Float64,lowLim_fl::Float64,lowLimTrust_fl::Float64)

	# re-create trust region
	if (objTopTrust_fl + objSub_fl) < trustReg_obj.objVal # recenter trust region, if new best solution was obtained
		trustReg_obj.cns, trustReg_obj.coef  = centerQuadTrust(trustReg_obj.exp,top_m,trustReg_obj.rad)
		trustReg_obj.objVal = objTopTrust_fl + objSub_fl
		produceMessage(top_m.options,top_m.report, 1," - Re-centered trust-region!", testErr = false, printErr = false)
	else
		trustReg_obj.cns, trustReg_obj.coef = centerQuadTrust(trustReg_obj.exp,top_m,trustReg_obj.rad)
		if abs(1 - lowLimTrust_fl / (objTopTrust_fl + objSub_fl)) < trustReg_obj.opt.extThrs # extend trust region, if constrained top problem converged
			resizeQuadTrust!(trustReg_obj,1.5)
			produceMessage(top_m.options,top_m.report, 1," - Extended trust-region!", testErr = false, printErr = false)
		elseif abs(1 - lowLim_fl / lowLimTrust_fl) < trustReg_obj.opt.shrThrs && trustReg_obj.rad > (trustReg_obj.abs * trustReg_obj.opt.lowRad) # shrink trust region, if it does not constrain the top problem and the lower limit for its size is not yet reached
			resizeQuadTrust!(trustReg_obj,0.5)
			produceMessage(top_m.options,top_m.report, 1," - Shrunk trust-region!", testErr = false, printErr = false)	
		end
	end

	return trustReg_obj
end

# ! add trust region to objective part of model
function centerQuadTrust(exp_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel,rad_fl::Float64)

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
function resizeQuadTrust!(trustReg_obj::quadTrust, shrFac_fl::Float64)
	# reduce factor for region
	trustReg_obj.rad = trustReg_obj.rad * shrFac_fl
	# adjust righ hand side of constraint
	cnsInfo_tup = trustReg_obj.coef
	set_normalized_rhs(trustReg_obj.cns,  (trustReg_obj.rad^2   - cnsInfo_tup.pol) * cnsInfo_tup.sca)
end

#endregion

#region # * benders solution algorithm

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

# ! run sub-Level problem
function runSubLevel(sub_m::anyModel,capaData_obj::bendersData)

	# fixing capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa",string(x)), collect(keys(capaData_obj.capa[sys][sSym])))
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], capaData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(capaData_obj.capa[sys][sSym][capaSym])
					delete!(capaData_obj.capa[sys][sSym],capaSym)
				else
					limitCapa!(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],sub_m)
				end
			end
			# remove system if no capacities exist
			if isempty(capaData_obj.capa[sys][sSym]) delete!(capaData_obj.capa[sys],sSym) end
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
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa",string(x)), collect(keys(capaData_obj.capa[sys][sSym])))
				capaData_obj.capa[sys][sSym][capaSym] = addDual(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].cns[Symbol(capaSym,:BendersFix)])
				# remove capacity if none exists (again necessary because dual can be zero)
				if isempty(capaData_obj.capa[sys][sSym][capaSym]) delete!(capaData_obj.capa[sys][sSym],capaSym) end
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			if isempty(capaData_obj.capa[sys][sSym]) delete!(capaData_obj.capa[sys],sSym) end
		end
	end

	return capaData_obj
end

# ! run top-Level problem
function runTopLevel(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	capaData_obj = bendersData()

	# add cuts
	if !isempty(cutData_dic) addCuts!(top_m,cutData_dic,i) end

	# solve model
	@suppress begin
		optimize!(top_m.optModel)
	end

	# write technology capacites and level of capacity balance to benders object
	capaData_obj.capa, expTrust_dic = [writeResult(top_m,[x]) for x in [:capa,:exc]]

	# get objective value of top problem
	objTopTrust_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	lowLimTrust_fl = objTopTrust_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])

	return capaData_obj, expTrust_dic, objTopTrust_fl, lowLimTrust_fl
end

# ! add all cuts from input dictionary to top problem
function addCuts!(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},bendersData},i::Int)

	# create array of expressions with duals for sub-problems
	cut_df = DataFrame(i = Int[], Ts_disSup = Int[],scr = Int[], limCoef = Bool[], cnsExpr = AffExpr[])
	for sub in keys(cutData_dic)

		subCut = cutData_dic[sub]
		cutExpr_arr = Array{GenericAffExpr,1}()
		
		# compute cut element for each capacity
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(subCut.capa[sys]), capaSym in filter(x -> occursin("capa",string(x)), collect(keys(subCut.capa[sys][sSym])))
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
			filter!(x -> abs(x[2]) > minCoef_fl,cut_expr.terms)

			# check if the cut also would violate range, limit coefficients in this case
			if cutCoef_fl < minCoef_fl
				limCoef_boo = true
				maxCoef_fl = cutCoef_fl * maxRng_fl # biggest possible coefficient, so cut variable is still in range
				foreach(x -> abs(cut_expr.terms[x]) > maxCoef_fl ? cut_expr.terms[x] = maxCoef_fl : nothing, collect(keys(cut_expr.terms))) # limits coefficients to maximum value
			end
		end

		# add benders variable to cut and push to dataframe of all cuts
		push!(cut_df,(i = i, Ts_disSup = top_m.supTs.step[sub[1]], scr = sub[2], limCoef = limCoef_boo, cnsExpr = cut_expr - cut_var))
	end

	# scale cuts and add to dataframe of benders cuts in model
	scaleCnsExpr!(cut_df,top_m.options.coefRng,top_m.options.checkRng)
	append!(top_m.parts.obj.cns[:bendersCuts] ,createCns(cnsCont(cut_df,:smaller),top_m.optModel))

end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersCut(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = deSelectSys(sub_df) |> (z -> innerjoin(deSelectSys(variable_df),z, on = intCol(z,:dir)))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *(collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
end

#endregion

#region # * transfer results between models

# ! write capacities or expansion in input model to returned capacity dictionary
function writeResult(in_m::anyModel, var_arr::Array{Symbol,1})
	
	var_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	fixPar_dic = Dict(:expStSize => :sizeToStOutFixExp, :expStIn => :stOutToStInFixExp, :capaStSize => :sizeToStOutFixCapa, :capaStIn => :stOutToStInFixCapa)

	for sys in (:tech,:exc)
		var_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(in_m.parts,sys)
		for sSym in filter(x -> part_dic[x].type in (:stock,:mature,:emerging),keys(part_dic))
			
			if part_dic[sSym].type == :stock &&  part_dic[sSym].decomm == :none
				continue
			end	

			var_dic[sys][sSym] = Dict(varSym => getResult(copy(part_dic[sSym].var[varSym])) for varSym in filter(x -> any(occursin.(string.(var_arr),string(x))), keys(part_dic[sSym].var)))

			# check if storage expansion is fixed to storage output and removes variables in these cases
			for stExp in intersect([:expStSize,:expStIn,:capaStSize,:capaStIn],collect(keys(var_dic[sys][sSym])))
				if fixPar_dic[stExp] in collect(keys(part_dic[sSym].cns))	
					fixCns_df = select(part_dic[sSym].cns[fixPar_dic[stExp]],Not([:cns]))
					var_dic[sys][sSym][stExp] = var_dic[sys][sSym][stExp] |> (x -> antijoin(x,fixCns_df, on = intCol(x)))	
				end
			end
			# remove empty fields
			filter!(x -> !isempty(x[2]),var_dic[sys][sSym])
			if isempty(var_dic[sys][sSym]) delete!(var_dic[sys],sSym) end
		end
	end
	return var_dic
end

# ! replaces the variable column with a column storing the value of the variable
function getResult(res_df::DataFrame)
	
	if :Ts_exp in namesSym(res_df) # for expansion filter unique variables
		res_df = collapseExp(res_df)
	else # for expansion filter cases where only residual values exist
		filter!(x -> !isempty(x.var.terms),res_df)
	end

	# write value of variable dataframe
	res_df[!,:value] = map(x -> value(collect(keys(x.terms))[1]) |> (z -> z < 1e-8 ? 0.0 : z),res_df[!,:var])

	return select(res_df,Not([:var]))
end

# ! create constraint fixing capacity (or setting a lower limits)
function limitCapa!(value_df::DataFrame,variable_df::DataFrame,var_sym::Symbol,part_obj::AbstractModelPart,fix_m::anyModel,lim_sym::Symbol=:Fix,round_boo::Bool=true)

	cns_sym = Symbol(:Benders,lim_sym)
	# join variables with capacity values
	fix_df = deSelectSys(value_df) |>  (z -> leftjoin(variable_df,z,on = intCol(z,:dir))) |> (y -> y[completecases(y),:])

	# extract actual variable
	fix_df[!,:var] = map(x -> collect(keys(x.var.terms))[1], eachrow(fix_df))
	# compute scaling factor
	fix_df[!,:scale] = map(x -> x.value <= fix_m.options.coefRng.rhs[1] ? min(fix_m.options.coefRng.mat[2],fix_m.options.coefRng.rhs[1]/x.value) : 1.0, eachrow(fix_df))
	fix_df[!,:scale] = map(x -> x.value >= fix_m.options.coefRng.rhs[2] ? min(fix_m.options.coefRng.mat[1],fix_m.options.coefRng.rhs[2]/x.value) : x.scale, eachrow(fix_df))

	# compute righ-hand side and factor of variables for constraint
	fix_df[!,:rhs] = map(x -> x.value * x.scale |> (u -> round(u,digits = 4) >= fix_m.options.coefRng.rhs[1] ? u : round_boo ? 0.0 : u), eachrow(fix_df))
	fix_df[!,:fac] = map(x -> x.rhs == 0.0 ? 1.0 : x.scale, eachrow(fix_df))

	if !(Symbol(var_sym,cns_sym) in keys(part_obj.cns))
		# create actual constraint and attach to model part
		if lim_sym == :Fix
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac == x.rhs),eachrow(fix_df))
		elseif lim_sym == :Up
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac <= x.rhs),eachrow(fix_df))
		else lim_sym == :Low
			fix_df[!,:cns] = map(x -> @constraint(fix_m.optModel,x.var * x.fac >= x.rhs),eachrow(fix_df))
		end
		
	else
		# adjust rhs and factor of existing constraint
		fix_df = innerjoin(select(part_obj.cns[Symbol(var_sym,cns_sym)],Not([:fac])),fix_df,on = intCol(fix_df,:dir))
		set_normalized_rhs.(fix_df[!,:cns], fix_df[!,:rhs])
		set_normalized_coefficient.(fix_df[!,:cns], fix_df[!,:var], fix_df[!,:fac])	
	end

	part_obj.cns[Symbol(var_sym,cns_sym)] = select(fix_df,Not([:var,:value,:scale,:rhs]))
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,cns_df::DataFrame)
	new_df = deSelectSys(cns_df) |> (z -> leftjoin(dual_df,z, on = intCol(z,:dir)))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns]) .* new_df[!,:fac]
	return select(filter(x -> x.dual != 0.0,new_df),Not([:cns,:fac]))
end

#endregion

