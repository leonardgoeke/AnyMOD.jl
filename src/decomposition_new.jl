
# merge all entries of dictionary used for capacity data into one dataframe for the specified columns
mergeVar(var_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},outCol::Array{Symbol,1}) = vcat(vcat(vcat(map(x -> var_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,outCol],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)

# structure managing stabilization method
mutable struct stabObj
	method::Array{Symbol,1} # array of method names used for stabilization
	methodOpt::Array{NamedTuple,1} # array of options for adjustment of stabilization parameters
	ruleSw::Union{NamedTuple{(), Tuple{}}, NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}}} # rule for switching between stabilization methods
	actMet::Int # index of currently active stabilization method
	objVal::Float64 # array of objective value for current center
	dynPar::Array{Float64,1} # array of dynamic parameters for each method
	var::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}} # variables subject to stabilization
	cns::ConstraintRef
	function stabObj(meth_tup::Tuple, ruleSw_ntup::NamedTuple, objVal_fl::Float64,lowBd_fl::Float64,relVar_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel)
		stab_obj = new()

		# set fields for name and options of method
		meth_arr = Symbol[]
		methOpt_arr = NamedTuple[]
		for (key,val) in meth_tup
			push!(meth_arr,key)
			push!(methOpt_arr,val)
			if key == :qtr && !isempty(setdiff(keys(val),(:start,:low,:thr,:fac)))
				error("options provided for trust-region do not match the defined options 'start', 'low', 'thr', and 'fac'")
			elseif key == :prx && !isempty(setdiff(keys(val),(:start,:low,:fac)))
				error("options provided for proximal bundle do not match the defined options 'start', 'low', 'thr', and 'fac'")
			elseif key == :lvl && !isempty(setdiff(keys(val),(:la,)))
				error("options provided for level bundle do not match the defined options 'la'")
			end
		end
		
		if !(isempty(ruleSw_ntup) || typeof(ruleSw_ntup) == NamedTuple{(:itr,:avgImp,:itrAvg),Tuple{Int64,Float64,Int64}})
			error("rule for switching stabilization method must be empty or have the fields 'itr', 'avgImp', and 'itrAvg'")
		end

		if !isempty(ruleSw_ntup) && ruleSw_ntup.itr < 2
			error("parameter 'itr' for  minimum iterations before switching stabilization method must be at least 2")
		end

		if length(meth_arr) != length(unique(meth_arr)) error("stabilization methods must be unique") end
		stab_obj.method = meth_arr
		stab_obj.methodOpt = methOpt_arr

		# method specific adjustments (e.g. starting value for dynamic parameter, new variables for objective function)
		dynPar_arr = Float64[]
		for m in 1:size(meth_arr,1)
			if meth_arr[m] == :prx
				dynPar = methOpt_arr[m].start # starting value for penalty
			elseif meth_arr[m] == :lvl
				dynPar = (methOpt_arr[m].la * lowBd_fl  + (1 - methOpt_arr[m].la) * objVal_fl) / top_m.options.scaFac.obj # starting value for level
				if methOpt_arr[m].la >= 1 || methOpt_arr[m].la <= 0 
					error("lambda for level bundle must be strictly between 0 and 1")
				end
			elseif meth_arr[m] == :qtr
				dynPar = methOpt_arr[m].start # starting value for radius
			else
				error("unknown stabilization method provided, method must either be 'prx', 'lvl', or 'qtr'")
			end
			push!(dynPar_arr,dynPar)
		end
		stab_obj.dynPar = dynPar_arr
		
		# set other fields
		stab_obj.ruleSw = ruleSw_ntup
		stab_obj.actMet = 1
		stab_obj.objVal = objVal_fl
		stab_obj.var = filterStabVar(relVar_dic,top_m)
		
		# compute number of variables subject to stabilization
		stabExpr_arr = vcat(vcat(vcat(map(x -> stab_obj.var[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)

		return stab_obj, size(stabExpr_arr,1)
	end
end

# filter variables used for stabilization
function filterStabVar(allVal_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},top_m::anyModel)

	var_dic = Dict(x => Dict{Symbol,Dict{Symbol,DataFrame}}() for x in [:tech,:exc])

	for sys in (:exc,:tech)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(allVal_dic[sys])
			var_dic[sys][sSym] = Dict{Symbol,Dict{Symbol,DataFrame}}() # create empty dataframe for values
			
			# determine where using expansion rather than capacity is possible and more efficient
			varNum_dic = Dict(x => size(unique(getfield.(part_dic[sSym].var[x][!,:var],:terms)),1) for x in collect(keys(part_dic[sSym].var))) # number of unique variables
			trstVar_arr = map(filter(x -> occursin("capa",string(x)),collect(keys(part_dic[sSym].var)))) do x
				expVar_sym = Symbol(replace(string(x),"capa" => "exp"))
				return part_dic[sSym].decomm == :none && expVar_sym in keys(varNum_dic) && varNum_dic[expVar_sym] <= varNum_dic[x] ? expVar_sym : x
			end

			for trstSym in intersect(keys(allVal_dic[sys][sSym]),trstVar_arr)
				var_df = allVal_dic[sys][sSym][trstSym]
				if trstSym == :capaExc && !part_dic[sSym].dir filter!(x -> x.R_from < x.R_to,var_df) end # only get relevant capacity variables of exchange
				if sys == :tech var_df = removeFixStorage(trstSym,var_df,part_dic[sSym]) end # remove storage variables controlled by ratio
				# filter cases where acutal variables are defined
				var_dic[sys][sSym][trstSym] = intCol(var_df) |> (w ->innerjoin(var_df,unique(select(filter(x -> !isempty(x.var.terms), part_dic[sSym].var[trstSym]),w)), on = w))
				# remove if no capacities remain
				removeEmptyDic!(var_dic[sys][sSym],trstSym)
			end
			
			# remove entire system if not capacities
			removeEmptyDic!(var_dic[sys],sSym)
		end
	end
	return var_dic
end

# function to update the center of stabilization method
centerStab!(method::Symbol,stab_obj::stabObj,top_m::anyModel) = centerStab!(Val{method}(),stab_obj::stabObj,top_m::anyModel)

# function for quadratic trust region
function centerStab!(method::Val{:qtr},stab_obj::stabObj,top_m::anyModel)

	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var,top_m)
	allVar_df = vcat(vcat(vcat(map(x -> expExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	
	# sets values of variables that will violate range to zero
	minFac_fl = (2*maximum(allVar_df[!,:value]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	abs_fl = sum(allVar_df[!,:value]) |> (x -> x < 0.01 * size(allVar_df,1) ? 10 * size(allVar_df,1) : x)
	scaRng_tup = top_m.options.coefRng.rhs ./ abs((abs_fl * stab_obj.dynPar[stab_obj.actMet])^2 - sum(allVar_df[!,:value].^2))

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# create final constraint
	stab_obj.cns = @constraint(top_m.optModel,  capaSum_expr * scaFac_fl <= (abs_fl * stab_obj.dynPar[stab_obj.actMet])^2  * scaFac_fl)
	
end

# function for proximal bundle method
function centerStab!(method::Val{:prx},stab_obj::stabObj,top_m::anyModel)
	
	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var,top_m)
	allVar_df = vcat(vcat(vcat(map(x -> expExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)

	pen_fl = stab_obj.dynPar[stab_obj.actMet]
	
	# sets values of variables that will violate range to zero
	minFac_fl = (2*pen_fl*maximum(allVar_df[!,:value]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> 2*pen_fl*x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# adjust objective function
	@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1] + pen_fl * capaSum_expr  * scaFac_fl)

end

# function for level bundle method
function centerStab!(method::Val{:lvl},stab_obj::stabObj,top_m::anyModel)
	
	# match values with variables in model
	expExpr_dic = matchValWithVar(stab_obj.var,top_m)
	allVar_df = vcat(vcat(vcat(map(x -> expExpr_dic[x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,[:var,:value]],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
	
	# sets values of variables that will violate range to zero
	minFac_fl = (maximum(allVar_df[!,:value]))/(top_m.options.coefRng.mat[2] / top_m.options.coefRng.mat[1])
	allVar_df[!,:value] = map(x -> x.value < minFac_fl ? 0.0 : x.value,eachrow(allVar_df))

	# compute possible range of scaling factors with rhs still in range
	scaRng_tup = top_m.options.coefRng.rhs ./ sum(allVar_df[!,:value].^2)

	# get scaled l2-norm expression for capacities
	capaSum_expr, scaFac_fl = computeL2Norm(allVar_df,stab_obj,scaRng_tup,top_m)

	# adjust objective function and level set
	@objective(top_m.optModel, Min, 0.5 * capaSum_expr  * scaFac_fl)
	set_upper_bound(top_m.parts.obj.var[:obj][1,1],stab_obj.dynPar[stab_obj.actMet])
end

# ! solves top problem without trust region and obtains lower limits
function runTopWithoutStab(top_m::anyModel,stab_obj::stabObj)
	
	if stab_obj.method[stab_obj.actMet] == :qtr
		delete(top_m.optModel,stab_obj.cns) # remove trust-region
	elseif stab_obj.method[stab_obj.actMet] == :prx
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1]) # remove penalty form objective
	elseif stab_obj.method[stab_obj.actMet] == :lvl
		@objective(top_m.optModel, Min, top_m.parts.obj.var[:obj][1,1])
		delete_upper_bound(top_m.parts.obj.var[:obj][1,1])
	end
	
	set_optimizer_attribute(top_m.optModel, "Method", 0)
	set_optimizer_attribute(top_m.optModel, "NumericFocus", 0)
	optimize!(top_m.optModel)

	# obtain different objective values
	objTop_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var])) # costs of unconstrained top-problem
	lowLim_fl = objTop_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var]) # objective (incl. benders) of unconstrained top-problem

	return objTop_fl, lowLim_fl
end

# ! matches values in dictionary with variables of provided problem
function matchValWithVar(capa_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}},mod_m::anyModel)
	# ! match values with variables
	expExpr_dic = Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}()
	for sys in (:tech,:exc)
		expExpr_dic[sys] = Dict{Symbol,Dict{Symbol,DataFrame}}()
		part_dic = getfield(mod_m.parts,sys)
		for sSym in keys(capa_dic[sys])
			expExpr_dic[sys][sSym] = Dict{Symbol,DataFrame}()
			for varSym in keys(capa_dic[sys][sSym])
				val_df = deSelectSys(capa_dic[sys][sSym][varSym])
				# correct scaling and storage values together with corresponding variables
				val_df[!,:value] = val_df[!,:value] ./ getfield(mod_m.options.scaFac, occursin("exp",string(varSym)) ? :insCapa : (occursin("StSize",string(varSym)) ? :capaStSize : :capa))
				expExpr_dic[sys][sSym][varSym] = unique(select(innerjoin(deSelectSys(part_dic[sSym].var[varSym]),val_df, on = intCol(val_df,:dir)),[:var,:value]))
			end
		end
	end

	return expExpr_dic
end

# ! compute scaled l2 norm
function computeL2Norm(allVar_df::DataFrame,stab_obj::stabObj,scaRng_tup::Tuple,top_m::anyModel)

	# set values of variable to zero or biggest value possible without scaling violating rhs range
	for x in eachrow(allVar_df)	
		if top_m.options.coefRng.mat[1]/(x.value*2) > scaRng_tup[2] # factor requires more up-scaling than possible
			x[:value] = 0 # set value to zero
		elseif top_m.options.coefRng.mat[2]/(x.value*2) < scaRng_tup[1] # factor requires more down-scaling than possible
			x[:value] = top_m.options.coefRng.mat[2]/(scaRng_tup[1]*2) # set to biggest value possible within range
		end
	end

	# computes left hand side expression and scaling factor
	capaSum_expr = sum(map(x -> collect(keys(x.var.terms))[1] |> (z -> z^2 - 2*x.value*z + x.value^2),eachrow(allVar_df)))
	scaFac_fl =  top_m.options.coefRng.mat[1]/minimum(abs.(collect(values(capaSum_expr.aff.terms)) |> (z -> isempty(z) ? [1.0] : z)))

	return capaSum_expr, scaFac_fl

end

# ! run top-problem
function runTop(top_m::anyModel,cutData_dic::Dict{Tuple{Int64,Int64},resData},stab_obj::Union{Nothing,stabObj},i::Int)

	capaData_obj = resData()

	# add cuts
	if !isempty(cutData_dic) addCuts!(top_m,cutData_dic,i) end
	# solve model
	set_optimizer_attribute(top_m.optModel, "Method", 2)
	set_optimizer_attribute(top_m.optModel, "Crossover", 0)
	set_optimizer_attribute(top_m.optModel, "NumericFocus", 3)
	optimize!(top_m.optModel)
	
	# if infeasible and level bundle stabilization, increase level until feasible
	if !isnothing(stab_obj) && stab_obj.method[stab_obj.actMet] == :lvl
		opt_tup = stab_obj.methodOpt[stab_obj.actMet]
		while termination_status(top_m.optModel) in (MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED)
			stab_obj.dynPar[stab_obj.actMet] = (opt_tup.la * stab_obj.dynPar[stab_obj.actMet]*1000  + (1 - opt_tup.la) * stab_obj.objVal) / top_m.options.scaFac.obj
			set_upper_bound(top_m.parts.obj.var[:obj][1,1],stab_obj.dynPar[stab_obj.actMet])
			optimize!(top_m.optModel)
		end
	end
	checkIIS(top_m)

	# write technology capacites and level of capacity balance to benders object
	capaData_obj.capa, allVal_dic = [writeResult(top_m,x; rmvFix = true) for x in [[:capa,:mustCapa],[:capa,:exp]]] 
	
	# get objective value of top problem
	objTop_fl = value(sum(filter(x -> x.name == :cost, top_m.parts.obj.var[:objVar])[!,:var]))
	lowLim_fl = objTop_fl + value(filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])

	return capaData_obj, allVal_dic, objTop_fl, lowLim_fl
end

# ! run sub-problem
function runSub(sub_m::anyModel,capaData_obj::resData,sol_sym::Symbol,optTol_fl::Float64=1e-8,wrtRes_boo::Bool=false)

	# fixing capacity
	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in sort(filter(x -> occursin("capa",lowercase(string(x))), collect(keys(capaData_obj.capa[sys][sSym]))),rev = true)
				# filter capacity data for respective year
				filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], capaData_obj.capa[sys][sSym][capaSym])
				# removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
				if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(capaData_obj.capa[sys][sSym][capaSym])
					delete!(capaData_obj.capa[sys][sSym],capaSym)
				else
					capaData_obj.capa[sys][sSym][capaSym] = limitVar!(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].var[capaSym],capaSym,part_dic[sSym],sub_m)
				end
			end
			# remove system if no capacities exist
			removeEmptyDic!(capaData_obj.capa[sys],sSym)
		end
	end

	# set optimizer attributes and solves
	if sol_sym == :barrier
		set_optimizer_attribute(sub_m.optModel, "Method", 2)
		set_optimizer_attribute(sub_m.optModel, "Crossover", 0)
		set_optimizer_attribute(sub_m.optModel, "BarOrder", 1)
		set_optimizer_attribute(sub_m.optModel, "BarConvTol", optTol_fl)
	elseif sol_sym == :simplex
		set_optimizer_attribute(sub_m.optModel, "Method", 1)
		set_optimizer_attribute(sub_m.optModel, "Threads", 1)
		set_optimizer_attribute(sub_m.optModel, "OptimalityTol", optTol_fl)
		set_optimizer_attribute(sub_m.optModel, "Presolve", 2)
	end

	optimize!(sub_m.optModel)
	checkIIS(sub_m)

	# write results into files (only used once optimum is obtained)
	if wrtRes_boo
		reportResults(:summary,sub_m)
		reportResults(:cost,sub_m)
	end

	# add duals and objective value to capacity data
	scaObj_fl = sub_m.options.scaFac.obj
	capaData_obj.objVal = value(sum(sub_m.parts.obj.var[:objVar][!,:var]))

	for sys in (:tech,:exc)
		part_dic = getfield(sub_m.parts,sys)
		for sSym in keys(capaData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa",lowercase(string(x))), collect(keys(capaData_obj.capa[sys][sSym])))
				if Symbol(capaSym,:BendersFix) in keys(part_dic[sSym].cns)
					scaCapa_fl = getfield(sub_m.options.scaFac,occursin("StSize",string(capaSym)) ? :capaStSize : :capa)
					capaData_obj.capa[sys][sSym][capaSym] = addDual(capaData_obj.capa[sys][sSym][capaSym],part_dic[sSym].cns[Symbol(capaSym,:BendersFix)],scaObj_fl/scaCapa_fl)
					# remove capacity if none exists (again necessary because dual can be zero)
					removeEmptyDic!(capaData_obj.capa[sys][sSym],capaSym)
				end
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			removeEmptyDic!(capaData_obj.capa[sys],sSym)
		end
	end

	return capaData_obj
end

# ! computes convergence tolerance for subproblems
function getConvTol(gapCur_fl::Float64,gapEnd_fl::Float64,conSub_tup::NamedTuple{(:rng, :int), Tuple{Vector{Float64}, Symbol}})

	if conSub_tup.int == :lin
		m = (conSub_tup.rng[1] - conSub_tup.rng[2])/(1-gapEnd_fl)
		b = conSub_tup.rng[1] - m
		return b + m * gapCur_fl
	elseif conSub_tup.int == :exp
		m = log(conSub_tup.rng[1]/conSub_tup.rng[2])/(1-gapEnd_fl)
		b = log(conSub_tup.rng[1]) - m
		return exp(b + m * gapCur_fl)
	elseif conSub_tup.int == :log
		b = conSub_tup.rng[1]
		m = (conSub_tup.rng[2] - b ) / log(gapEnd_fl)
		return b + m * log(gapCur_fl)
	end
end

# ! update dynamic parameter of stabilization method
function adjustDynPar!(stab_obj::stabObj,top_m::anyModel,iUpd_int::Int,adjCtr_boo::Bool,lowLimNoStab_fl::Float64,lowLim_fl::Float64,currentBest_fl::Float64,report_m::anyModel)

	opt_tup = stab_obj.methodOpt[iUpd_int]
	if stab_obj.method[iUpd_int] == :qtr # adjust radius of quadratic trust-region
		if !adjCtr_boo && abs(1 - lowLimNoStab_fl / lowLim_fl) < opt_tup.thr && stab_obj.dynPar[iUpd_int] > opt_tup.low
			stab_obj.dynPar[iUpd_int] = max(opt_tup.low,stab_obj.dynPar[iUpd_int] / opt_tup.fac)
			produceMessage(report_m.options,report_m.report, 1," - Reduced quadratic trust-region!", testErr = false, printErr = false)	
		end
	elseif stab_obj.method[iUpd_int] == :prx # adjust penalty term
		if adjCtr_boo
			stab_obj.dynPar[iUpd_int] = max(opt_tup.low,stab_obj.dynPar[iUpd_int] / opt_tup.fac)
			produceMessage(report_m.options,report_m.report, 1," - Reduced penalty term of proximal bundle!", testErr = false, printErr = false)
		else
			stab_obj.dynPar[iUpd_int] = max(opt_tup.low,stab_obj.dynPar[iUpd_int] * opt_tup.fac)
			produceMessage(report_m.options,report_m.report, 1," - Increased penalty term of proximal bundle!", testErr = false, printErr = false)
		end
	elseif stab_obj.method[iUpd_int] == :lvl # adjust level
		stab_obj.dynPar[iUpd_int] = (opt_tup.la * lowLimNoStab_fl  + (1 - opt_tup.la) * currentBest_fl) / top_m.options.scaFac.obj
	end

end