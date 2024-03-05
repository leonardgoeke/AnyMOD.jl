using AnyMOD, Gurobi, CSV, YAML

b = "C:/Users/pacop/Desktop/git/EuSysMod/"

#region # * options for algorithm

# ! options for general algorithm

conSub_tup = (rng = [1e-8,1e-8], int = :log, crs = false) # range and interpolation method for convergence criteria of subproblems
numOpt_tup = (dbInf = true, numFoc = 3, addVio = 1e4) # options for handling numeric problems
distr_boo = true;

# target gap, threshold for serious step, number of iteration after unused cut is deleted, 2x see above, number of iterations report is written, time-limit for algorithm, distributed computing?, # number of threads, optimizer
algSetup_obj = algSetup(0.001, 0.0, 20, conSub_tup, numOpt_tup, (bal = false, st = false), 100, 120.0, distr_boo, 4, Gurobi.Optimizer)

# ! options for stabilization

methKey_str = "qtr_5"

# write tuple for stabilization
stabMap_dic = YAML.load_file(b * "stabMap.yaml")
if methKey_str in keys(stabMap_dic)
	meth_tup = tuple(map(x -> Symbol(x[1]) => (; (Symbol(k) => v for (k, v) in x[2])...), collect(stabMap_dic[methKey_str]))...)
else
	meth_tup = tuple()
end

swt_ntup = (itr = 6, avgImp = 0.2, itrAvg = 4) # rule to switch between different methods
weight_ntup = (capa = 1.0, capaStSize = 1e-1, stLvl = 1e-2) # weight of variables in stabilization (-> small value for variables with large numbers to equalize)
iniStab_ntup = (setup = :none, det = true) # options to initialize stabilization, :none for first input will skip stabilization, other values control input folders, second input determines, if heuristic model is solved stochastically or not

stabSetup_obj = stabSetup(meth_tup, swt_ntup, weight_ntup, iniStab_ntup)


# ! options for near optimal

# defines objectives for near-optimal (can only take top-problem variables, must specify a variable)
nearOptOpj_tup = ("tradeOffWind_1" => (:min,((0.0,(variable = :capaConv, system = :onshore)),(1.0,(variable = :capaConv, system = :offshore)))),
                	"tradeOffWind_2" => (:min,((0.25,(variable = :capaConv, system = :onshore)),(0.75,(variable = :capaConv, system = :offshore)))),
						"tradeOffWind_3" => (:min,((0.5,(variable = :capaConv, system = :onshore)),(0.5,(variable = :capaConv, system = :offshore)))),
							"tradeOffWind_4" => (:min,((0.75,(variable = :capaConv, system = :onshore)),(0.25,(variable = :capaConv, system = :offshore)))),
								"tradeOffWind_5" => (:min,((1.0,(variable = :capaConv, system = :onshore)),(0.0,(variable = :capaConv, system = :offshore)))))

nearOptSetup_obj = nearOptSetup(0.1, 0.05, 0.05, 0.0001, 20, nearOptOpj_tup) # cost threshold to keep solution, lls threshold to keep solution, epsilon for near-optimal, cut deletion

#endregion

#region # * options for problem

# ! general problem settings
name_str ="_test"
# name, temporal resolution, level of foresight, superordinate dispatch level, length of steps between investment years
info_ntup = (name = name_str, frs = 0, supTsLvl = 1, shortExp = 10) 

# ! input folders
dir_str = b
scr_int = 2 # number of scenarios
res_int = 96

inDir_arr = [dir_str * "_basis",dir_str * "_full",dir_str * "timeSeries/" * string(res_int) * "hours_s" * string(scr_int)] # input directory

if stabSetup_obj.ini.setup in (:none,:full) 
	heuInDir_arr = inDir_arr
elseif stabSetup_obj.ini.setup == :reduced
	heuInDir_arr =  [dir_str * "_basis",dir_str * "_heu",dir_str * "timeSeries/" * string(res_int) * "hours_s" * string(scr_int)]
end 

inputFolder_ntup = (in = inDir_arr, heu = heuInDir_arr, results = dir_str * "results")

# ! scaling settings

scale_dic = Dict{Symbol,NamedTuple}()

scale_dic[:rng] = (mat = (1e-3,1e5), rhs = (1e-1,1e5))
scale_dic[:facHeu] = (capa = 1e2, capaStSize = 1e2, insCapa = 1e1, dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)
scale_dic[:facTop] = (capa = 1e2, capaStSize = 1e1, insCapa = 1e2, dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e0, obj = 1e3)
scale_dic[:facSub] = (capa = 1e0, capaStSize = 1e2, insCapa = 1e0, dispConv = 1e1, dispSt = 1e3, dispExc = 1e1, dispTrd = 1e1, costDisp = 1e0, costCapa = 1e2, obj = 1e1)

#endregion

#region # * prepare iteration

# initialize distributed computing
if distr_boo 
	addprocs(scr_int*2) 
	@suppress @everywhere begin 
		using AnyMOD, Gurobi
		runSubDist(w_int::Int64, resData_obj::resData, sol_sym::Symbol, optTol_fl::Float64=1e-8, crsOver_boo::Bool=false, wrtRes_boo::Bool=false) = Distributed.@spawnat w_int runSub(sub_m, resData_obj, sol_sym, optTol_fl, crsOver_boo, wrtRes_boo)
	end
	passobj(1, workers(), [:info_ntup, :inputFolder_ntup, :scale_dic, :algSetup_obj])

else
	runSubDist = x -> nothing
end

# create benders object
benders_obj = bendersObj(info_ntup, inputFolder_ntup, scale_dic, algSetup_obj, stabSetup_obj, runSubDist, nearOptSetup_obj)

#endregion

#region # * iteration algorithm

while true

	produceMessage(benders_obj.report.mod.options, benders_obj.report.mod.report, 1, " - Started iteration $(benders_obj.itr.cnt.i)", testErr = false, printErr = false)

	#region # * solve top-problem and (start) sub-problems

	startTop = now()
	resData_obj, stabVar_obj, topCost_fl, estCost_fl, nearOptObj_fl, levelDual_fl = @suppress runTop(benders_obj); 
	timeTop = now() - startTop

	# start solving sub-problems
	cutData_dic = Dict{Tuple{Int64,Int64},resData}()
	time_dic = Dict{Tuple{Int64,Int64},Millisecond}()

	if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end
	for (id,s) in enumerate(collect(keys(benders_obj.sub)))
		if benders_obj.algOpt.dist # distributed case
			futData_dic[s] = runSubDist(id + 1, copy(resData_obj), :barrier, 1e-8)
		else # non-distributed case
			time_dic[s], cutData_dic[s] = runSub(benders_obj.sub[s], copy(resData_obj), :barrier, 1e-8)
		end
	end

	# top-problem without stabilization
	if !isnothing(stab_obj) 
		topCostNoStab_fl, estCostNoStab_fl =  @suppress runTopWithoutStab(top_m,stab_obj,solOpt.numFoc.noStab) 
	else
		topCostNoStab_fl, estCostNoStab_fl = [Inf, Inf]
	end

	# get results of sub-problems
	if benders_obj.algOpt.dist
		wait.(collect(values(futData_dic)))
		for s in collect(keys(benders_obj.sub))
			time_dic[s], cutData_dic[s] = fetch(futData_dic[s])
		end
	end
	
	#endregion
	
	#region # * check results

	updateResult(benders_obj, topCost_fl, estCost_fl, nearOptObj_fl, sum(map(x -> x.objVal, values(cutData_dic))))

	function updateResults(benders_obj, topCost_fl::Float64, estCost_fl::Float64, nearOptObj_fl::Float64, subCost_fl::Float64)

		itr_obj = benders_obj.itr

		# ! update current best
		
		
		# check if solution improved (comparision of total costs for costs optimization, costs of sub-problem for near optimal -> target is same costs as in optimal case)
		if (itr_obj.cnt.nearOpt == 0 ? (topCost_fl + subCost_fl) : (subCost_fl - (estCost_fl - topCost_fl))) < best_obj.objVal - expStep_fl * benders_obj.algOpt.srsThr
			itr_obj.best.objVal = itr_obj.cnt.nearOpt == 0 ? (topCost_fl + subCost_fl) : (subCost_fl - (estCost_fl - topCost_fl)) # store current best value
			itr_obj.best.objVal.capa, itr_obj.best.objVal.stLvl = writeResult(benders_obj.top, [:capa,:exp,:mustCapa,:stLvl]; rmvFix = true)		
		end

		# ! update iteration object
		# TODO update
		#if !isempty(nearOpt_ntup) nearOpt_df, lss_fl = writeCapaRes(top_m,sub_dic,sub_tup,nearOpt_df,i,nOpt_int,nearOpt_ntup,topCost_fl,subCost_fl,costOpt_fl,lssOpt_fl) end

	end

	#endregion

	#region # * adjust refinements
	
	# ! delete cuts that not were binding for the defined number of iterations
	deleteCuts!(benders_obj)
	
	# ! adapt center and parameter for stabilization
	if !isempty(meth_tup)

		itr_obj = benders_obj.itr

		expStep_fl = itr_obj.cnt.nOpt == 0 ? (itr_obj.objCurBest - estCost_fl) : 0.0 # expected step size
		
		# determine serious step 
		adjCtr_boo = false
		if best_obj.objVal < stab_obj.objVal - srsThr * expStep_fl
			adjCtr_boo = true
		end

		# initialize counters
		cntNull_int = adjCtr_boo ? 0 : cntNull_int + 1
		cntSrs_int = adjCtr_boo ? cntSrs_int + 1 : 0

		# solve problem without stabilization method
		topCostNoStab_fl, estCostNoStab_fl = @suppress runTopWithoutStab(top_m,stab_obj,solOpt.numFoc)

		# adjust dynamic parameters of stabilization
		prx2Aux_fl = stab_obj.method[stab_obj.actMet] == :prx2 ? computePrx2Aux(cutData_dic,prevCutData_dic) : nothing
		foreach(i -> adjustDynPar!(stab_obj,top_m,i,adjCtr_boo,cntSrs_int,cntNull_int,levelDual_fl,prx2Aux_fl,estCostNoStab_fl,estCost_fl,best_obj.objVal,currentCost,nOpt_int != 0,report_m), 1:length(stab_obj.method))

		# update center of stabilisation
		if adjCtr_boo
			stab_obj.var = filterStabVar(stabVar_obj.capa,stabVar_obj.stLvl,stab_obj.weight,top_m)
			stab_obj.objVal = best_obj.objVal
			produceMessage(report_m.options,report_m.report, 1," - Updated reference point for stabilization!", testErr = false, printErr = false)
		end

		estCost_fl = estCostNoStab_fl # set lower limit for convergence check to lower limit without trust region
	end

	#endregion

	# report on iteration
	reportBenders!(benders_obj, nOpt_int)
	
	#region # * check convergence and adapt stabilization	
	
	# check for termination
	if gap_fl < gap
		if !isempty(nearOpt_ntup) && nOpt_int < length(nearOpt_ntup.obj)
			# switch from cost minimization to near-optimal
			if nOpt_int == 0
				# get characteristics of optimal solution
				costOpt_fl = best_obj.objVal
				lssOpt_fl = lss_fl
				# filter near-optimal solution already obtained
				nearOpt_df[!,:thrs] .= 1 .- best_obj.objVal ./ nearOpt_df[!,:cost]
				filter!(x -> x.thrs <= nearOpt_ntup.cutThres && x.lss <= lssOpt_fl * (1 + nearOpt_ntup.lssThres), nearOpt_df)
				# adjust gap
				gap = nearOpt_ntup.feasGap
			end
			# reset iteration variables
			gap_fl = gap 
			nearOptObj_fl = Inf
			best_obj.objVal = Inf
			if !isempty(meth_tup) # reset current best tracking for stabilization
				stab_obj.objVal = Inf
				stab_obj.dynPar = writeStabOpt(meth_tup,estCost_fl,best_obj.objVal,top_m)[3]
			end 
			nOpt_int = nOpt_int + 1 # update near-opt counter
			# adapt the objective and constraint to near-optimal
			adaptNearOpt!(top_m,nearOpt_ntup,costOpt_fl,nOpt_int)
			produceMessage(report_m.options,report_m.report, 1," - Switched to near-optimal for $(nearOpt_ntup.obj[nOpt_int][1])", testErr = false, printErr = false)
		else
			produceMessage(report_m.options,report_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
			break
		end
	end

	# switch and update quadratic stabilization method
	if !isempty(meth_tup)
		# switch stabilization method
		if !isempty(stab_obj.ruleSw) && i > stab_obj.ruleSw.itr && length(stab_obj.method) > 1
			min_boo = itrReport_df[i - stab_obj.ruleSw.itr,:actMethod] == stab_obj.method[stab_obj.actMet] # check if method as been used for the minimum number of iterations 
			pro_boo = itrReport_df[(i - min(i,stab_obj.ruleSw.itrAvg) + 1):end,:gap] |> (x -> (x[1]/x[end])^(1/(length(x) -1)) - 1 < stab_obj.ruleSw.avgImp) # check if progress in last iterations is below threshold
			if min_boo && pro_boo
				stab_obj.actMet = stab_obj.actMet + 1 |> (x -> length(stab_obj.method) < x ? 1 : x)
				produceMessage(report_m.options,report_m.report, 1," - Switched stabilization to $(nameStab_dic[stab_obj.method[stab_obj.actMet]]) method!", testErr = false, printErr = false)
			end
		end

		# update stabilization method
		centerStab!(stab_obj.method[stab_obj.actMet],stab_obj,solOpt.addVio,top_m,report_m)
	end

	if Dates.value(floor(now() - report_m.options.startTime,Dates.Minute(1))) > timeLim
		produceMessage(report_m.options,report_m.report, 1," - Aborted due to time-limit!", testErr = false, printErr = false)
		break
	end

	#endregion

	i = i + 1
end

#endregion

#region # * write results

function writeBendersResults!(benders_obj::bendersObj, runSubDist::Function)

	# reporting on iteration
	benders_obj.report.itr[!,:case] .= benders_obj.info.name
	CSV.write(benders_obj.report.mod.options.outDir * "/iterationCuttingPlane_$(benders_obj.info.name).csv", benders_obj.report.itr)

	# reporting on near-optimal
	if !isnothing(benders_obj.nearOpt.setup)
		CSV.write(benders_obj.report.mod.options.outDir * "/nearOptSol_$(benders_obj.info.name).csv", benders_obj.report.nearOpt)
	end

	# run top-problem and sub-problems with optimal values fixed and write results
	@suppress computeFeas(benders_obj.top, benders_obj.best.capa, 1e-5, wrtRes = true)

	if benders_obj.algOpt.dist futData_dic = Dict{Tuple{Int64,Int64},Future}() end

	for (id,s) in enumerate(collect(keys(benders_obj.sub)))
		if benders_obj.algOpt.dist # distributed case
			futData_dic[s] = runSubDist(id + 1, copy(startSol_obj), :barrier, 1e-8, false, true)
		else # non-distributed case
			runSub(benders_obj.sub[s], copy(startSol_obj), :barrier, 1e-8, false, true)
		end
	end

	# TODO add writing of storage levels and time-series (but optional)
	
end

#endregion


# write storage levels
for tSym in (:h2Cavern,:reservoir,:pumpedStorage,:redoxBattery,:lithiumBattery)
	stLvl_df = DataFrame(Ts_dis = Int[], scr = Int[], lvl = Float64[])
	for x in collect(sub_tup)
		append!(stLvl_df,combine(x -> (lvl = sum(value.(x.var)),), groupby(sub_dic[x].parts.tech[tSym].var[:stLvl],[:Ts_dis,:scr])))
	end
	stLvl_df = unstack(sort(unique(stLvl_df),:Ts_dis),:scr,:lvl)
	CSV.write(b * "results/stLvl_" * string(tSym) * "_" * suffix_str * ".csv",stLvl_df)
end


