using AnyMOD, Gurobi, CSV, YAML, Base.Threads, Dates
b = "C:/Users/pacop/Desktop/git/EuSysMod/"


#region # * options for algorithm

# ! options for general algorithm

# target gap, threshold for serious step, number of iteration after unused cut is deleted, 2x see above, number of iterations report is written, time-limit for algorithm 
bendersSetup_obj = bendersSetup(0.001, 0.0, 20, conSub, numOpt_tup, (bal = false, st = false), 100, 120.0)

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
# name, temporal resolution, level of foresight, superordinate dispatch level, length of steps between investment years, optimizer, number of threads
genSetup_ntup = (name = name_str, frs = 0, supTsLvl = 1, shortExp = 10, threads = 4, opt = Gurobi.Optimizer) 

# ! input folders
dir_str = b
scr_int = 4 # number of scenarios
res_int = 169

inDir_arr = [dir_str * "_basis",dir_str * "_full",dir_str * "timeSeries/" * string(res_int) * "hours_s" * string(scr_int)] # input directory

if stabSetup_obj in (:none,:full) 
	heuInDir_arr = inDir_arr
elseif iniStab == :reduced
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

# ! create benders object and add subproblems
benders_obj, sub_tup = bendersObj(genSetup_ntup, inputFolder_ntup, scale_dic, bendersSetup_obj, stabSetup_obj, nearOpt = nearOptSetup_obj)
addSubproblems!(genSetup_ntup, inputFolder_ntup, scale_dic, benders_obj, sub_tup)

#endregion

# TODO decide to deal with running subproblems (depends on multi single, part of creation of benders object at the moment) -> multi dispatch?

#region # * benders iteration

nameStab_dic = Dict(:lvl1 => "level bundle",:lvl2 => "level bundle",:qtr => "quadratic trust-region", :prx => "proximal bundle", :box => "box-step method")


if !isempty(meth_tup)
	itrReport_df[!,:actMethod] = fill(Symbol(),size(itrReport_df,1))
	foreach(x -> itrReport_df[!,Symbol("dynPar_",x)] = Union{Float64,Vector{Float64}}[fill(Float64[],size(itrReport_df,1))...], stab_obj.method)
end

if !isempty(nearOpt_ntup)
	nearOpt_df = DataFrame(i = Int[], timestep = String[], region = String[], system = String[], id = String[], capacity_variable = Symbol[], capacity_value = Float64[], cost = Float64[], lss = Float64[])
	itrReport_df[!,:objective] = fill("",size(itrReport_df,1))
	nOpt_int = 0
end

# initialize best solution
best_obj = !isempty(meth_tup) ? startSol_obj : resData()

# initialize loop variables
i = iIni_fl
gap_fl = 1.0
minStep_fl = 0.0
nOpt_int = 0
costOpt_fl = Inf
nearOptObj_fl = Inf
cntNull_int = 0
cntSrs_int = 0
lssOpt_fl = Inf


# iteration algorithm
while true

	produceMessage(report_m.options,report_m.report, 1," - Started iteration $i", testErr = false, printErr = false)

	#region # * solve top-problem 

	startTop = now()
	resData_obj, stabVar_obj, topCost_fl, estCost_fl, levelDual_fl = @suppress runTop(top_m,cutData_dic,stab_obj,solOpt.numFoc,i); 
	timeTop = now() - startTop

	# get objective value for near-optimal
	if nOpt_int != 0 nearOptObj_fl = objective_value(top_m.optModel) end

	#endregion
	
	#region # * solve of sub-problems  
	startSub = now()
	prevCutData_dic = !isempty(meth_tup) && stab_obj.method[stab_obj.actMet] == :prx2 ? copy(cutData_dic) : nothing # save values of previous cut for proximal method variation 2
	for x in collect(sub_tup)
		dual_etr = runSub(sub_dic[x],copy(resData_obj),:barrier,nOpt_int == 0 ? getConvTol(gap_fl,gap,conSub) : conSub.rng[2],conSub.crs)
		cutData_dic[x] = dual_etr
	end
	timeSub = now() - startSub

	#endregion

	#region # * check results

	# ! updates current best
	expStep_fl = nOpt_int == 0 ? (best_obj.objVal - estCost_fl) : 0.0 # expected step size
	subCost_fl = sum(map(x -> x.objVal, values(cutData_dic))) # objective of sub-problems
	currentCost = topCost_fl + subCost_fl

	if (nOpt_int == 0 ? (topCost_fl + subCost_fl) : (subCost_fl - (estCost_fl - topCost_fl))) < best_obj.objVal
		best_obj.objVal = nOpt_int == 0 ? (topCost_fl + subCost_fl) : (subCost_fl - (estCost_fl - topCost_fl)) # store current best value
		best_obj.capa, best_obj.stLvl = writeResult(top_m,[:capa,:exp,:mustCapa,:stLvl]; rmvFix = true)		
	end

	# reporting on current results
	if top_m.options.lvlFrs != 0 && i%reportFreq == 0 
		stReport_df = writeStLvlRes(top_m,sub_dic,sub_tup,i,stReport_df) 
	end
	
	if !isempty(nearOpt_ntup) nearOpt_df, lss_fl = writeCapaRes(top_m,sub_dic,sub_tup,nearOpt_df,i,nOpt_int,nearOpt_ntup,topCost_fl,subCost_fl,costOpt_fl,lssOpt_fl) end

	#endregion

	#region # * adjust refinements
	
	# ! delete cuts that not were binding for the defined number of iterations
	deleteCuts!(top_m, nOpt_int == 0 ? delCut : nearOpt_ntup.cutDel,i)
	
	# ! adapt center and parameter for stabilization
	if !isempty(meth_tup)
		
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
	reportBenders(benders_obj, nOpt_int)
	
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

# write dataframe for reporting on iteration
itrReport_df[!,:case] .= suffix_str
CSV.write(modOpt_tup.resultDir * "/iterationCuttingPlane_$(replace(top_m.options.objName,"topModel" => "")).csv",  itrReport_df)

if !isempty(nearOpt_ntup)
	CSV.write(modOpt_tup.resultDir * "/nearOptSol_$(replace(top_m.options.objName,"topModel" => "")).csv",  nearOpt_df)
end

# run top-problem with optimal values fixed
@suppress computeFeas(top_m,best_obj.capa,1e-5,wrtRes = true)

# run top-problem and sub-problems with optimal values fixed
for x in collect(sub_tup)
	runSub(sub_dic[x],copy(best_obj),:barrier,1e-8,false,true)
end

# write storage levels
for tSym in (:h2Cavern,:reservoir,:pumpedStorage,:redoxBattery,:lithiumBattery)
	stLvl_df = DataFrame(Ts_dis = Int[], scr = Int[], lvl = Float64[])
	for x in collect(sub_tup)
		append!(stLvl_df,combine(x -> (lvl = sum(value.(x.var)),), groupby(sub_dic[x].parts.tech[tSym].var[:stLvl],[:Ts_dis,:scr])))
	end
	stLvl_df = unstack(sort(unique(stLvl_df),:Ts_dis),:scr,:lvl)
	CSV.write(b * "results/stLvl_" * string(tSym) * "_" * suffix_str * ".csv",stLvl_df)
end

#endregion




