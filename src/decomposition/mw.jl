function runSubMW(sub_m::anyModel, benders_obj::bendersObj, resData_obj::resData, rngVio_fl::Float64, sol_sym::Symbol, timeLim_fl::Float64, optTol_fl::Float64=1e-8, crsOver_boo::Bool=false, check_boo::Bool = false, resultOpt::NamedTuple = NamedTuple())

    str_time = now()

    #region # * fix complicating variables

    resDataFix_obj = filterResData(resData_obj, sub_m, [:capa, :mustCapa, :stLvl, :lim]; rmvFix = true, fltSt = false)

    # fixing capacity
    for sys in (:tech, :exc)
        part_dic = getfield(sub_m.parts, sys)
        for sSym in keys(resDataFix_obj.capa[sys])
            for capaSym in sort(filter(x -> occursin("capa", lowercase(string(x))), collect(keys(resDataFix_obj.capa[sys][sSym]))), rev = true)
                # filter capacity data for respective year
                filter!(x -> x.Ts_disSup == sub_m.supTs.step[1], resDataFix_obj.capa[sys][sSym][capaSym])
                # removes entry from capacity data, if capacity does not exist in respective year, otherwise fix to value
                if !(sSym in keys(part_dic)) || !(capaSym in keys(part_dic[sSym].var)) || isempty(resDataFix_obj.capa[sys][sSym][capaSym])
                    delete!(resDataFix_obj.capa[sys][sSym], capaSym)
                else
                    cnsName_str = string(sys, "_", sSym, "_", capaSym)
                    resDataFix_obj.capa[sys][sSym][capaSym] = limitVar!(resDataFix_obj.capa[sys][sSym][capaSym], part_dic[sSym].var[capaSym], capaSym, part_dic[sSym], rngVio_fl, sub_m, :Fix, cnsName_str)
                end
            end
            # remove system if no capacities exist
            removeEmptyDic!(resDataFix_obj.capa[sys], sSym)
        end
    end

    # fixing storage levels
    if !isempty(resDataFix_obj.stLvl)
        for sSym in keys(resDataFix_obj.stLvl)
            if sSym in keys(sub_m.parts.tech)
                part_obj = sub_m.parts.tech[sSym]
                for stType in keys(resDataFix_obj.stLvl[sSym])
                    cnsName_str = string(sSym, "_", stType)
                    fix_df = select(filter(x -> stType == :stLvl ? true : x.scr == sub_m.subPro[2], resDataFix_obj.stLvl[sSym][stType]), Not([:scr]))
                    resDataFix_obj.stLvl[sSym][stType] = limitVar!(fix_df, select(part_obj.var[stType], Not([:scr])), stType, part_obj, rngVio_fl, sub_m, :Fix, cnsName_str)
                    removeEmptyDic!(resDataFix_obj.stLvl[sSym], stType)
                end
                # remove system if no storage level exists
                removeEmptyDic!(resDataFix_obj.stLvl, sSym)
            end
        end
    end

    # fixing limiting variables
    if !isempty(resDataFix_obj.lim)
        for limSym in keys(resDataFix_obj.lim)
            lim_df = select(filter(x -> x.sub == sub_m.subPro, resDataFix_obj.lim[limSym]), Not([:sub]))
            if !isempty(lim_df)
                cnsName_str = string(limSym)
                resDataFix_obj.lim[limSym] = limitVar!(lim_df, sub_m.parts.lim.var[limSym], limSym, sub_m.parts.lim, rngVio_fl, sub_m, :Fix, cnsName_str)
                # remove system if no storage level exists
                removeEmptyDic!(resDataFix_obj.lim, limSym)
            end
        end
    end

    #endregion

    #region # * solve primal problem

    # set optimizer attributes and solves
    @suppress begin
        if sol_sym == :barrier
            set_optimizer_attribute(sub_m.optModel, "Method", 2)
            set_optimizer_attribute(sub_m.optModel, "Crossover", crsOver_boo ? 1 : 0)
            set_optimizer_attribute(sub_m.optModel, "BarOrder", 1)
            set_optimizer_attribute(sub_m.optModel, "BarConvTol", optTol_fl)
        elseif sol_sym == :simplex
            set_optimizer_attribute(sub_m.optModel, "Method", 1)
            set_optimizer_attribute(sub_m.optModel, "OptimalityTol", optTol_fl)
            set_optimizer_attribute(sub_m.optModel, "Presolve", 2)
            set_optimizer_attribute(sub_m.optModel, "NumericFocus", 3)
        end
        if timeLim_fl != 0.0 set_optimizer_attribute(sub_m.optModel, "TimeLimit", timeLim_fl * 60) end # in seconds
    end

    # increase numeric focus if model did not solve
	println("Solve primal model! - ", Dates.toms(now() - str_time) / Dates.toms(Second(1)))
    numFoc_int = solveModel!(sub_m, sub_m.optModel, [0,3], true, check_boo)

    # write primal duals
    resPrimal_obj = writeDualPrimal(sub_m, copy(resDataFix_obj)) 
    # writeAllResults!(sub_m, resultOpt, false)

    #endregion

    #region # * create and solve dual problem

    if !isdefined(sub_m,:dual) # # create dual model, if not created yet
        # dualize the sub-problem
        dual_mod = dualize(sub_m.optModel; dual_names = DualNames("dualVar_", "dualCns_"))

        # set objective value as constraint
        # oldObj_expr = objective_function(dual_mod) # TODO only needed, when trying single step method
        objVal_fl = objective_value(sub_m.optModel) # TODO make ratio flexible
        slack_var = @variable(dual_mod, lower_bound = 0.0, base_name = "slackMW")
        cns_obj = @constraint(dual_mod, objective_function(dual_mod) + slack_var == objVal_fl)
		set_optimizer_attribute(dual_mod, "Threads", benders_obj.algOpt.sub.threads)	
        sub_m.dual = (mod = dual_mod, obj = cns_obj, slack = slack_var)
    else # update capacity fix in dual model

        # update capacity fix, TODO extend for storage and limits
        for sys in (:tech, :exc)
            part_dic = getfield(sub_m.parts, sys)
            for sSym in keys(part_dic)
                for benFix in filter(x -> occursin("BendersFix", string(x)), keys(part_dic[sSym].cns))
                    updateFixDualProblem!(sub_m, part_dic[sSym].cns[benFix])
                end
            end
        end

        # update objective value
        set_normalized_rhs(sub_m.dual.obj, objective_value(sub_m.optModel)) # TODO make ratio flexible

    end

    # create expression of dual variables
    coreVar_expr = createDualCoreExp(sub_m, resData_obj, benders_obj.itr.best.var, 0.5)
    scaObj_fl = sub_m.options.scaFac.obj

    # enforce new objective
    set_objective_function(sub_m.dual.mod, coreVar_expr / scaObj_fl - sub_m.dual.slack * sub_m.options.coefRng.mat[2] / 100)

    # TODO old for for single step method
    #noDual_arr = filter(x -> !occursin("dual", string(x[1])), collect(oldObj_expr.terms))
    #set_objective_function(sub_m.dual.mod, coreVar_expr / scaObj_fl + sum(map(x -> x[1] * x[2], noDual_arr)))

    # solve dual model
	println("Solve dual model! - ", Dates.toms(now() - str_time) / Dates.toms(Second(1)))

	# set optimizer attributes and solves
    @suppress begin
		set_optimizer(sub_m.dual.mod, Gurobi.Optimizer)
        if sol_sym == :barrier
            set_optimizer_attribute(sub_m.dual.mod, "Method", 2)
            set_optimizer_attribute(sub_m.dual.mod, "Crossover", crsOver_boo ? 1 : 0)
            set_optimizer_attribute(sub_m.dual.mod, "BarOrder", 1)
            set_optimizer_attribute(sub_m.dual.mod, "BarConvTol", optTol_fl)
        elseif sol_sym == :simplex
            set_optimizer_attribute(sub_m.dual.mod, "Method", 1)
            set_optimizer_attribute(sub_m.dual.mod, "OptimalityTol", optTol_fl)
            set_optimizer_attribute(sub_m.dual.mod, "Presolve", 2)
            set_optimizer_attribute(sub_m.dual.mod, "NumericFocus", 3)
        end
        if timeLim_fl != 0.0 set_optimizer_attribute(sub_m.dual.mod, "TimeLimit", timeLim_fl * 60) end # in seconds
    end
	


    optimize!(sub_m.dual.mod)
    #compute_conflict!(sub_m.dual.mod)

    # write dual duals
    resDual_obj = writeDualDual(sub_m, copy(resDataFix_obj), value(sub_m.dual.obj)) 

    #endregion

	println("Report results! - ", Dates.toms(now() - str_time) / Dates.toms(Second(1)))
    println("Primal cut - current iterate:", computeCutValue(resPrimal_obj, resDataFix_obj, resDataFix_obj))
    println("Dual cut - current iterate:", computeCutValue(resDual_obj, resDataFix_obj, resDataFix_obj))

    println("Primal cut - current best:", computeCutValue(resPrimal_obj, benders_obj.itr.best.var, resDataFix_obj))
    println("Dual cut - current best:", computeCutValue(resDual_obj, benders_obj.itr.best.var, resDataFix_obj))

    elpSub_time = now() - str_time

    lss_fl = 0.0 # TODO what?

    return resPrimal_obj, resDual_obj, elpSub_time, lss_fl, numFoc_int
	#return resPrimal_obj, resDual_obj, elpSub_time, lss_fl, numFoc_int
end

# create sumproduct of dual variables and core point values
function createDualCoreExp(sub_m::anyModel, curSol_obj::resData, mw_obj::resData, interMW_fl::Float64)
	
	expExpr_arr = AffExpr[]
	lowVal_fl =  sub_m.options.coefRng.mat[1] * sub_m.options.scaFac.obj
	
	# match capacity and storage levels
	for sys in (:tech, :exc)
		part_dic = getfield(sub_m.parts, sys)
		for sSym in keys(part_dic)
            for cnsSym in filter(x -> occursin("BendersFix",string(x)), keys(part_dic[sSym].cns))
                primalCns_df = part_dic[sSym].cns[cnsSym]
                var_sym = Symbol(replace(string(cnsSym),"BendersFix" => ""))
                if var_sym in (:stLvl, :stLvlInter)
                    primalCns_df = rename(innerjoin(primalCns_df, curSol_obj.stLvl[sSym][var_sym], on = intCol(primalCns_df)), :value => :valueCurSol)
                    primalCns_df = rename(innerjoin(primalCns_df, mw_obj.stLvl[sSym][var_sym], on = intCol(primalCns_df)), :value => :valueMW)    
                elseif var_sym in keys(mw_obj.capa[sys][sSym])
                    primalCns_df = rename(innerjoin(primalCns_df, curSol_obj.capa[sys][sSym][var_sym], on = intCol(primalCns_df,:dir)), :value => :valueCurSol)
                    primalCns_df = rename(innerjoin(primalCns_df, mw_obj.capa[sys][sSym][var_sym], on = intCol(primalCns_df,:dir)), :value => :valueMW)   
                else
                    continue
                end  
                # correct values with scaling factor # TODO (wrap?)
                mapScaFac_arr = ["stlvl" => :dispSt, "exp" => :insCapa, "stsize" => :capaStSize, "benderscom" => :dispConv]
                scaFac_sym = occursin.(getindex.(mapScaFac_arr,1), lowercase(string(var_sym)))|> (z -> any(z) ? getindex.(mapScaFac_arr,2)[findall(z)[1]] : :capa)
                primalCns_df[!,:valueMW] = round.(primalCns_df[!,:valueMW] ./ getfield(sub_m.options.scaFac, scaFac_sym), sigdigits = 10)
                primalCns_df[!,:valueCurSol] = round.(primalCns_df[!,:valueCurSol] ./ getfield(sub_m.options.scaFac, scaFac_sym), sigdigits = 10)
                # create dual expression
                primalCns_df[!,:dualVar] = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualVar_,name(x))], primalCns_df[!,:cns])
				# compute factor from weights
				primalCns_df[!,:fac] = ((1 - interMW_fl) .* primalCns_df[!,:valueMW] .- (1 + interMW_fl) .* primalCns_df[!,:valueCurSol])
				# set small factors to zero or smallest possible value within range, whatever is more accurate	
				primalCns_df[!,:fac] = map(x -> x != 0.0 && abs(x) < lowVal_fl ? (x < lowVal_fl / 2 ? 0.0 : lowVal_fl) : x, primalCns_df[!,:fac])
                push!(expExpr_arr, sum(primalCns_df[!,:dualVar] .* primalCns_df[!,:fac]))
            end
        end
	end


	# match limits
    for limSym in filter(x -> occursin("BendersFix",string(x)), keys(sub_m.parts.lim.cns))
        primalCns_df = sub_m.parts.lim.cns[limSym]
        # match constraint with core point
        var_sym = Symbol(replace(string(limSym),"BendersFix" => ""))
        primalCns_df = rename(innerjoin(primalCns_df, filter(x -> x.sub == sub_m.subPro, mw_obj.lim[var_sym]), on = intCol(primalCns_df)), :value => :valueMW)
         primalCns_df = rename(innerjoin(primalCns_df, filter(x -> x.sub == sub_m.subPro, curSol_obj.lim[var_sym]), on = intCol(primalCns_df)), :value => :valueCurSol)
        primalCns_df[!,:dualVar] = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualVar_,name(x))], primalCns_df[!,:cns])
        # correct values with scaling factor # TODO (wrap?)
        mapScaFac_arr = ["stlvl" => :dispSt, "exp" => :insCapa, "stsize" => :capaStSize, "benderscom" => :dispConv]
        scaFac_sym = occursin.(getindex.(mapScaFac_arr,1), lowercase(string(var_sym)))|> (z -> any(z) ? getindex.(mapScaFac_arr,2)[findall(z)[1]] : :capa)
        primalCns_df[!,:valueMW] = round.(primalCns_df[!,:valueMW] ./ getfield(sub_m.options.scaFac, scaFac_sym), sigdigits = 10)
        primalCns_df[!,:valueCurSol] = round.(primalCns_df[!,:valueCurSol] ./ getfield(sub_m.options.scaFac, scaFac_sym), sigdigits = 10)
		# compute factor from weights
		primalCns_df[!,:fac] = ((1 - interMW_fl) .* primalCns_df[!,:valueMW] .- (1 + interMW_fl) .* primalCns_df[!,:valueCurSol])
		# set small factors to zero or smallest possible value within range, whatever is more accurate	
		primalCns_df[!,:fac] = map(x -> x != 0.0 && abs(x) < lowVal_fl ? (x < lowVal_fl / 2 ? 0.0 : lowVal_fl) : x, primalCns_df[!,:fac])
		push!(expExpr_arr, sum(primalCns_df[!,:dualVar] .* primalCns_df[!,:fac]))
    end

	return sum(expExpr_arr)
end

# write results of primal model
function writeDualPrimal(sub_m::anyModel, resData_obj::resData)

	scaObj_fl = sub_m.options.scaFac.obj
	resData_obj.objVal = value(sum(sub_m.parts.obj.var[:objVar][!,:var]))

	# get duals on capacity
	for sys in (:tech, :exc)
		part_dic = getfield(sub_m.parts, sys)
		for sSym in keys(resData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa", lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym])))
				if Symbol(capaSym, :BendersFix) in keys(part_dic[sSym].cns)
					scaCapa_fl = getfield(sub_m.options.scaFac, occursin("StSize", string(capaSym)) ? :capaStSize : :capa)
					resData_obj.capa[sys][sSym][capaSym] = addDual(resData_obj.capa[sys][sSym][capaSym], part_dic[sSym].cns[Symbol(capaSym, :BendersFix)], scaObj_fl / scaCapa_fl)
					# remove capacity if none exists (again necessary because dual can be zero)
					removeEmptyDic!(resData_obj.capa[sys][sSym], capaSym)
				end
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			removeEmptyDic!(resData_obj.capa[sys], sSym)
		end
	end

	# get duals on storage levels
	if !isempty(resData_obj.stLvl)
		for sSym in keys(resData_obj.stLvl)
			if sSym in keys(sub_m.parts.tech)
				part_obj = sub_m.parts.tech[sSym]
				for stType in keys(resData_obj.stLvl[sSym])
					resData_obj.stLvl[sSym][stType] = addDual(resData_obj.stLvl[sSym][stType], part_obj.cns[Symbol(stType,:BendersFix)], scaObj_fl / sub_m.options.scaFac.dispSt)
					removeEmptyDic!(resData_obj.stLvl[sSym], stType)
				end
				removeEmptyDic!(resData_obj.stLvl, sSym)
			end
		end
	end

	# get duals on limits
	if !isempty(resData_obj.lim)
		for limSym in keys(resData_obj.lim)
			resData_obj.lim[limSym] = addDual(resData_obj.lim[limSym], sub_m.parts.lim.cns[Symbol(limSym,:BendersFix)], scaObj_fl / sub_m.options.scaFac.dispConv)
			removeEmptyDic!(resData_obj.lim, limSym)
		end
	end

	return resData_obj
end

# write results of dual model
function writeDualDual(sub_m::anyModel, resData_obj::resData, objVal_fl::Float64)
	
    scaObj_fl = sub_m.options.scaFac.obj
	# get objective value
	resData_obj.objVal = objVal_fl * scaObj_fl

	# get duals on capacity
	for sys in (:tech, :exc)
		part_dic = getfield(sub_m.parts, sys)
		for sSym in keys(resData_obj.capa[sys])
			for capaSym in filter(x -> occursin("capa", lowercase(string(x))), collect(keys(resData_obj.capa[sys][sSym])))			
				# get and join corresponding capacity constraint
				cns_df = part_dic[sSym].cns[Symbol(capaSym,:BendersFix)]
				res_df = innerjoin(resData_obj.capa[sys][sSym][capaSym], cns_df, on = intCol(cns_df,:dir))
				# get scaling factor  
				scaCapa_fl = getfield(sub_m.options.scaFac, occursin("StSize", string(capaSym)) ? :capaStSize : :capa)
				# extract value of dual variable
				res_df[!,:dualVar] = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualVar_,name(x))], res_df[!,:cns])
				res_df[!,:dual] = value.(res_df[!,:dualVar]) .* res_df[!,:fac] .* scaObj_fl ./ scaCapa_fl
				resData_obj.capa[sys][sSym][capaSym] = select(filter(x -> x.dual != 0.0, res_df), Not([:fac,:cns,:dualVar]))
				removeEmptyDic!(resData_obj.capa[sys][sSym], capaSym)
			end
			# remove system if no capacities exist (again necessary because dual can be zero)
			removeEmptyDic!(resData_obj.capa[sys], sSym)
		end
	end

	# get duals on storage levels
	if !isempty(resData_obj.stLvl)
		for sSym in keys(resData_obj.stLvl)
			if sSym in keys(sub_m.parts.tech)
				part_obj = sub_m.parts.tech[sSym]
				for stType in keys(resData_obj.stLvl[sSym])
					# get and join corresponding storage level
					cns_df = part_obj.cns[Symbol(stType,:BendersFix)]
					res_df = innerjoin(resData_obj.stLvl[sSym][stType], cns_df, on = intCol(cns_df,:dir))
					# extract value of dual variable
					res_df[!,:dualVar] = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualVar_,name(x))], res_df[!,:cns])
					res_df[!,:dual] = value.(res_df[!,:dualVar]) .* res_df[!,:fac] .* scaObj_fl ./ sub_m.options.scaFac.dispSt
					resData_obj.stLvl[sSym][stType] = select(res_df, Not([:fac,:cns,:dualVar]))
					removeEmptyDic!(resData_obj.stLvl[sSym], stType)
				end
				removeEmptyDic!(resData_obj.stLvl, sSym)
			end
		end
	end

	# get duals on limits
	if !isempty(resData_obj.lim)
		for limSym in keys(resData_obj.lim)
			# get and join corresponding limit
			cns_df = sub_m.parts.lim.cns[Symbol(limSym,:BendersFix)]
			res_df = innerjoin(resData_obj.lim[limSym], cns_df, on = intCol(cns_df,[:Up,:Low,:Fix]))
			# extract value of dual variable
			res_df[!,:dualVar] = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualVar_,name(x),:_1)], res_df[!,:cns])
			res_df[!,:dual] = value.(res_df[!,:dualVar])  .* res_df[!,:fac] .* scaObj_fl ./ sub_m.options.scaFac.dispConv
			resData_obj.lim[limSym] = select(res_df, Not([:fac,:cns,:dualVar]))
			removeEmptyDic!(resData_obj.lim, limSym)
		end
	end

	return resData_obj

end

# TODO extend for limits and storage
# compute value of cut at a specific point 
function computeCutValue(cutRes_obj::resData, pointRes_obj::resData, pointCut_obj::resData)

	val_fl = cutRes_obj.objVal

	for sys in (:tech, :exc)
		for sSym in keys(cutRes_obj.capa[sys])
			for capaSym in keys(cutRes_obj.capa[sys][sSym])
				join_df = rename(innerjoin(select(cutRes_obj.capa[sys][sSym][capaSym],Not([:value])), pointRes_obj.capa[sys][sSym][capaSym], on = intCol(cutRes_obj.capa[sys][sSym][capaSym],:dir)), :value => :valueRes)
				join_df = rename(innerjoin(join_df, pointCut_obj.capa[sys][sSym][capaSym], on = intCol(pointCut_obj.capa[sys][sSym][capaSym],:dir)), :value => :valueCur)
                val_fl = val_fl + sum(join_df[!,:dual] .* (join_df[!,:valueRes] .- join_df[!,:valueCur]))
			end
		end
	end

	return val_fl
end 

# update capacity fix in dual problem
function updateFixDualProblem!(sub_m::anyModel, bendersFix_df::DataFrame)

	# get reference for variable and constraints in dual problem
	dualVar_arr = map(x -> sub_m.dual.mod.obj_dict[x], Symbol.(:dualVar_, name.(bendersFix_df[!,:cns])))
	dualCns_arr = map(x -> sub_m.dual.mod.obj_dict[Symbol(:dualCns_, string(collect(keys(x.func.terms))[1]))], constraint_object.(bendersFix_df[!,:cns]))

	# get factors and rhs in primal problem
	fac_arr = bendersFix_df[!,:fac]
	rhs_arr = normalized_rhs.(bendersFix_df[!,:cns])

	# update multiplication factor of capacity constraint -> corresponds to factor in dual constraint
	set_normalized_coefficient.(dualCns_arr, dualVar_arr, - fac_arr)
	# update rhs of capacity constraint -> corresponds to factor in dual function
	set_normalized_coefficient.(sub_m.dual.obj, dualVar_arr, rhs_arr)

end

# run iteration with MW
function runIterationMW!(benders_obj::bendersObj, runSubDist::Function)

	allRes_df = DataFrame(i = Int[], Ts_expSup = Int[], Ts_disSup = Int[], Ts_dis = Int[], R_dis = Int[], R_exp = Int[], R_from = Int[], R_to = Int[], C = Int[], Te = Int[], Exc = Int[], M = Int[], scr = Int[], id = Int[], sub = Tuple[], variable = Symbol[], value = Float64[])

	while true

		produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Started iteration $(benders_obj.itr.cnt.i)", testErr = false, printErr = false)
	
		#region # * solve top-problem and (start) sub-problems
		str_time = now()
		resData_obj, stLvl_dic = runTop(benders_obj);
		trackCuts!(benders_obj)
		elpTop_time = now() - str_time

		# start solving sub-problems
        cutDataPrimal_dic = Dict{Tuple{Int64,Int64},resData}()
	    cutDataDual_dic = Dict{Tuple{Int64,Int64},resData}()
		timeSub_dic = Dict{Tuple{Int64,Int64},Millisecond}()
		lss_dic = Dict{Tuple{Int64,Int64},Float64}()
		numFoc_dic = Dict{Tuple{Int64,Int64},Int64}()
	
		acc_fl = interItrPar(benders_obj.itr.gap, benders_obj.algOpt.gap, benders_obj.algOpt.sub.rng, benders_obj.algOpt.sub.int)
	
        if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
        for (id,s) in enumerate(sort(collect(keys(benders_obj.sub))))
            println(s)
            cutDataPrimal_dic[s], cutDataDual_dic[s], timeSub_dic[s], lss_dic[s], numFoc_dic[s] = runSubMW(benders_obj.sub[s], benders_obj, copy(resData_obj), benders_obj.algOpt.rngVio.fix, benders_obj.algOpt.sub.meth, benders_obj.algOpt.sub.timeLim, acc_fl, benders_obj.algOpt.sub.crs, benders_obj.algOpt.sub.check)
        end

		# save current results
		curRes_dic = Dict(x => reportResults(x, benders_obj.top, rtnOpt = (:csvDf,), rmvZero = false) for x in benders_obj.report.res.general)
	
		# top-problem without stabilization
		strNoStab_time = now()
		if !isnothing(benders_obj.stab) 
			# remove stabilization from top problem (to be added again at the end of iteration)
			removeStab!(benders_obj)
			# check if top problem without stabilization should be solved again 
			if benders_obj.itr.cnt.i >= benders_obj.itr.cnt.nextNoStab || benders_obj.stab.crossNoStab
				runTopWithoutStab!(benders_obj)
				# compute next iteration to solve top problem
				par_ntup = benders_obj.stab.solveNoStab
				gap_fl = 1 - benders_obj.itr.res[:lowLimCost] / benders_obj.itr.res[:curBest]
				waitTopNoStab_int = max(1, Int(floor(interItrPar(gap_fl, benders_obj.algOpt.gap, [par_ntup.upper,1], par_ntup.inter, par_ntup.sub))))
				benders_obj.itr.cnt.nextNoStab = benders_obj.itr.cnt.i + waitTopNoStab_int
				# only report, if problem without stabilization is not solved again in the next iteration
				if waitTopNoStab_int != 1
					produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Solved top problem without stabilization. Next solve in iteration $(benders_obj.itr.cnt.nextNoStab)", testErr = false, printErr = false)
				end
			else
				# use results of last correct solve as lower bound
				benders_obj.itr.res[:lowLimCost] = benders_obj.itr.res[:estTotCostNoStab]
			end

		end
		elpNoStab_time = now() - strNoStab_time
	
		# get results of sub-problems
		if benders_obj.algOpt.dist
			wait.(collect(values(futData_dic)))
			for s in sort(collect(keys(benders_obj.sub)))
				cutData_dic[s], timeSub_dic[s], lss_dic[s], numFoc_dic[s] = fetch(futData_dic[s])
			end
		end
		
		#endregion
	
		#region # * analyse results and update refinements

		# update results and stabilization
		srsStep_boo = updateIteration!(benders_obj, cutDataPrimal_dic, resData_obj, curRes_dic, stLvl_dic, cutDataDual_dic)
		# report on iteration
		reportBenders!(benders_obj, resData_obj, elpTop_time, elpNoStab_time, timeSub_dic, lss_dic, numFoc_dic)
	
		# check convergence and finish
		rtn_boo = checkConvergence(benders_obj, lss_dic)
		
		# track capacity over iterations if activated
		if benders_obj.trackCapa reportComplVar!(allRes_df, resData_obj, benders_obj.itr.cnt.i) end

		#endregion

		benders_obj.itr.cnt.i = benders_obj.itr.cnt.i + 1
		if rtn_boo break end
		
	end

	# apply weights to tracked capacities
	if benders_obj.trackCapa
		w_dic = Dict(:capaConv => benders_obj.stab.weight.capa, :mustCapaConv => benders_obj.stab.weight.capa, :capaStSize => benders_obj.stab.weight.capaStSize, :capaStOut => benders_obj.stab.weight.capa, :capaStIn => benders_obj.stab.weight.capa, :capaExc => benders_obj.stab.weight.capa,
																																						:stLvl => benders_obj.stab.weight.stLvl, :stLvlInter => benders_obj.stab.weight.stLvl, :emissionBendersCom => benders_obj.stab.weight.lim)
		allRes_df[!,:value] = map(x -> x.value * w_dic[x.variable], eachrow(allRes_df))
	end

	return allRes_df

end