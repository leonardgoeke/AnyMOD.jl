
# ! set options

method = :all # options are :all, :fixAndLim, :fixAndQtr :onlyFix and :none
resHeu = 2
resMod = 4
t_int = 2
dir_str = ""

#region # * set and write options

# ! intermediate definitions of parameters

reso_tup = (heu = resHeu, mod = resMod) 
suffix_str = "_" * string(method) * "_" * string(resHeu) * "_" * string(resMod)
inDir_arr = [[dir_str * "_basis",dir_str * "_full",dir_str * "timeSeries/" * string(x) * "days_2010"] for x in [reso_tup.heu, reso_tup.mod]] # input directories

coefRngHeu_tup = (mat = (1e-2,1e4), rhs = (1e0,1e4))
coefRngTop_tup = (mat = (1e-2,1e4), rhs = (1e0,1e4))
coefRngSub_tup = (mat = (1e-2,1e4), rhs = (1e0,1e4))

scaFacHeu_tup = (capa = 1e2,  capaStSize = 1e2, insCapa = 1e1,dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)
scaFacTop_tup = (capa = 1e0, capaStSize = 1e1, insCapa = 1e0, dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e0, obj = 1e3)
scaFacSub_tup = (capa = 1e2,  capaStSize = 1e2, insCapa = 1e1,dispConv = 1e1, dispSt = 1e2, dispExc = 1e1, dispTrd = 1e1, costDisp = 1e0, costCapa = 1e2, obj = 1e1)

# ! general input parameters

# structure of subproblems, indicating the year (first integer) and the scenario (second integer)
sub_tup = ((1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0))

# options of solution algorithm
solOpt_tup = (gap = 0.001, gapLim = 0.02, delCut = 30, linPar = (thrsAbs = 0.05, thrsRel = 0.05), quadPar = (startRad = 1e-1, lowRad = 1e-6, shrThrs = 0.001, extThrs = 0.001))

# options for different models
temp_dir = dir_str * "tempFix" * suffix_str # directory for temporary folder

optMod_dic = Dict{Symbol,NamedTuple}()

# options for model generation 
optMod_dic[:heu] =  (inputDir = inDir_arr[1], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngHeu_tup, scaFac = scaFacHeu_tup)
optMod_dic[:top] =  (inputDir = inDir_arr[2], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngTop_tup, scaFac = scaFacTop_tup)
optMod_dic[:sub] =  (inputDir = inDir_arr[2], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngSub_tup, scaFac = scaFacSub_tup)

#endregion

#region # * initialize workers

using Distributed, MatheClusterManagers # MatheClusterManagers is an altered version of https://github.com/JuliaParallel/ClusterManagers.jl by https://github.com/mariok90 to run on the cluster of TU Berlinn

# add workers to job
nb_workers = 8
@static if Sys.islinux()
    using MatheClusterManagers
    qrsh(nb_workers, timelimit=172800, ram=32, mp = t_int)
else
    addprocs(nb_workers; exeflags="--project=.")
end

@everywhere using AnyMOD, CSV, ParallelDataTransfer, Distributed, Gurobi
opt_obj = Gurobi.Optimizer # solver option

heuWrk_int = last(workers())
subWrk_arr = filter(x-> x != heuWrk_int, workers())

#endregion

#region # * define functions for distributed

# ! run all sub-problems when running code distributed
function runAllSub(sub_tup::Tuple, capaData_obj::bendersData)
	solvedFut_dic = Dict{Int, Future}()
	for j in 1:length(sub_tup)
		solvedFut_dic[j] = @spawnat j+1 runSubDis(copy(capaData_obj))
	end
	return solvedFut_dic
end

# ! get results of all sub-problems when running code distributed
function getSubResults(cutData_dic::Dict{Tuple{Int64,Int64},bendersData}, sub_tup::Tuple, solvedFut_dic::Dict{Int, Future})
	runTime_arr = []
	for (k,v) in solvedFut_dic
		t_fl, cutData_dic[sub_tup[k]] = fetch(v)
		push!(runTime_arr, t_fl)
	end
	return maximum(runTime_arr)
end

#endregion

report_m = @suppress anyModel(String[], optMod_dic[:heu].resultDir; objName="decomposition" * optMod_dic[:heu].suffix) # creates empty model just for reporting

#region # * solve heuristic models and write results
passobj(1, heuWrk_int, [:optMod_dic, :reso_tup, :opt_obj])

if method in (:all, :fixAndLim, :onlyFix, :fixAndQtr)
    # heuristic solve for re-scaled and compressed time-series
	produceMessageShort(" - Started heuristic pre-solve",report_m)
	heu_future = @spawnat heuWrk_int begin
		heuristicSolve(optMod_dic[:heu],365 / reso_tup.heu,t_int,opt_obj,false)
	end
    heu_m, heuSca_obj = heuristicSolve(optMod_dic[:heu], 1.0, t_int, opt_obj)
	produceMessageShort(" - Finished scaled heuristic",report_m)
	heuCom_obj = fetch(heu_future)
	produceMessageShort(" - Finished compressed heuristic",report_m)
	rmprocs(heuWrk_int)
	produceMessageShort(" - Removed worker $heuWrk_int used for compressed heuristic",report_m)
    # write fixes to files and limits to dictionary
    fix_dic, lim_dic, cntHeu_arr = @suppress evaluateHeu(heu_m, heuSca_obj, heuCom_obj, solOpt_tup.linPar) # get fixed and limited variables
	produceMessageShort(" - Get exact feasible solution",report_m)
    feasFix_dic = @suppress getFeasResult(optMod_dic[:top], fix_dic, lim_dic, t_int, solOpt_tup.linPar.thrsAbs, opt_obj) # ensure feasiblity with fixed variables
	produceMessageShort(" - Heuristic found $(cntHeu_arr[1]) fixed variables and $(cntHeu_arr[2]) limited variables",report_m)
    # write fixed variable values to files
    writeFixToFiles(fix_dic, feasFix_dic, temp_dir, heu_m)
	heu_m = nothing
end

#endregion

#region # * create top and sub-problems 

modOptSub_tup = optMod_dic[:sub]
inputDir_arr = method in (:all, :fixAndLim, :fixAndQtr, :onlyFix) ? vcat(modOptSub_tup.inputDir, [temp_dir]) : modOptSub_tup.inputDir

# ! create sub-problems
passobj(1, workers(), [:inputDir_arr, :modOptSub_tup, :sub_tup,:t_int])
produceMessageShort(" - Start creating sub-problems",report_m)


subTasks_arr = map(subWrk_arr) do w
	t = @async @everywhere w begin
		# create sub-problem
		function buildSub(id)
			sub_m = @suppress anyModel(inputDir_arr, modOptSub_tup.resultDir, objName = "subModel_" * string(myid()-1) * modOptSub_tup.suffix, supTsLvl = modOptSub_tup.supTsLvl, shortExp = modOptSub_tup.shortExp, coefRng = modOptSub_tup.coefRng, scaFac = modOptSub_tup.scaFac, reportLvl = 1, holdFixed = true)
			sub_m.subPro = sub_tup[id]
			@suppress prepareMod!(sub_m, Gurobi.Optimizer, t_int)
			return sub_m
		end
		const SUB_M = @suppress buildSub(myid() - 1)

		# define function to run sub-problem
		function runSubDis(capaData_obj)
			start_time = now()
			result_obj = @suppress runSub(SUB_M, capaData_obj)
			elapsed_time = now() - start_time
			return elapsed_time, result_obj
		end

		return nothing
	end

	return w => t
end


# reporting on creation of sub-problems
for (w,t) in subTasks_arr
	@async begin 
		produceMessageShort(" - Started building sub-problem on worker $w",report_m)
		wait(t)
		produceMessageShort(" - Sub-problem on worker $w ready",report_m)
	end
end

# ! create top-problem
modOptTop_tup = optMod_dic[:top]
inputDir_arr = method in (:all,:fixAndLim,:fixAndQtr,:onlyFix) ? vcat(modOptTop_tup.inputDir,[temp_dir]) : modOptTop_tup.inputDir

top_m = anyModel(inputDir_arr, modOptTop_tup.resultDir, objName = "topModel" * modOptTop_tup.suffix, supTsLvl = modOptTop_tup.supTsLvl, shortExp = modOptTop_tup.shortExp, coefRng = modOptTop_tup.coefRng, scaFac = modOptTop_tup.scaFac, reportLvl = 1, holdFixed = true)
top_m.subPro = tuple(0,0)
prepareMod!(top_m,opt_obj,t_int)

# create seperate variables for costs of subproblems and aggregate them (cannot be part of model creation, because requires information about subproblems) 
top_m.parts.obj.var[:cut] = map(y -> map(x -> y == 1 ? top_m.supTs.step[x] : sub_tup[x][2], 1:length(sub_tup)),1:2) |> (z -> createVar(DataFrame(Ts_disSup = z[1], scr = z[2]),"subCut",NaN,top_m.optModel,top_m.lock,top_m.sets, scaFac = 1e2))
push!(top_m.parts.obj.cns[:objEqn], (name = :aggCut, group = :benders, cns = @constraint(top_m.optModel, sum(top_m.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])))

# wait for all sub-problems to be created
subTasks_arr = getindex.(values(subTasks_arr), 2)
if all(istaskdone.(subTasks_arr))
	produceMessageShort(" - All sub-problems are ready",report_m)
else
	produceMessageShort(" - Waiting for sub-problems to be ready",report_m)
	wait.(subTasks_arr)
	produceMessageShort(" - Sub-problems ready",report_m)
end


#endregion

#region # * add linear and quadratic trust region

if method in (:all, :fixAndLim, :fixAndQtr, :onlyFix)
	produceMessageShort(" - Create cuts from heuristic solution",report_m)
    # ! create cuts from heuristic solutions  
    for z in [heuCom_obj, heuSca_obj]
        # run subproblems and get cut info
        cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()
		solvedFut_dic = runAllSub(sub_tup, z)

		for (k,v) in solvedFut_dic
			x = sub_tup[k]
			~, dual_etr = fetch(v)
			# removes entries without dual values
			for sys in [:exc,:tech]
				for sSym in keys(dual_etr.capa[sys])
					for capaSym in keys(dual_etr.capa[sys][sSym])
						if !("dual" in names(dual_etr.capa[sys][sSym][capaSym])) delete!(dual_etr.capa[sys][sSym],capaSym) end
					end
					removeEmptyDic!(dual_etr.capa[sys],sSym)
				end
			end
			cutData_dic[x] = dual_etr
		end
        # add cuts to top-problem
        addCuts!(top_m, cutData_dic, 0)
    end

    # ! add linear trust region
    if method in (:all, :fixAndLim)
        addLinearTrust!(top_m, lim_dic)
		produceMessageShort(" - Enforced linear trust region",report_m)
    end

    # ! add quadratic trust region
    if method in (:all, :fixAndQtr)
        qctVar_dic = filterQtrVar(feasFix_dic, top_m) # get variables for quadratic trust region
        trustReg_obj, eleNum_int = quadTrust(qctVar_dic, solOpt_tup.quadPar)
        trustReg_obj.cns = centerQuadTrust(trustReg_obj.var, top_m, trustReg_obj.rad)
        trustReg_obj.objVal = Inf
		produceMessageShort(" - Initialized quadratic trust region with $eleNum_int variables",report_m)
    end
end

#endregion

#region # * run benders iteration

# initialize loop variables
itrReport_df = DataFrame(i = Int[], low = Float64[], best = Float64[], gap = Float64[], solCur = Float64[], time = Float64[])
cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()

let i = 1, gap_fl = 1.0, currentBest_fl = Inf
	while true
		produceMessageShort(" - Started iteration $i",report_m)

		#region # * solve top-problem and sub-problems

		startTop = now()
		capaData_obj, allVal_dic, objTopTrust_fl, lowLimTrust_fl = @suppress runTop(top_m, cutData_dic, i)
		timeTop = now() - startTop

		solvedFut_dic = runAllSub(sub_tup, capaData_obj)
		
		#endregion

		#region # * compute bounds and analyze cuts

		if method in (:all, :fixAndQtr) # run top-problem without trust region to obtain lower limits
			objTop_fl, lowLim_fl = @suppress runTopWithoutQuadTrust(top_m,trustReg_obj)
		else # without quad trust region, lower limit corresponds unaltered of standard top-problem
			lowLim_fl = lowLimTrust_fl
		end

		# ! delete cuts that not were binding for the defined number of iterations
		deleteCuts!(top_m,solOpt_tup.delCut,i) 
		
		# ! get objective of sub-problems and current best solution
		timeSub = getSubResults(cutData_dic, sub_tup, solvedFut_dic)
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # objective of sub-problems
		currentBest_fl = min(objTopTrust_fl + objSub_fl, currentBest_fl) # current best solution
		
		#endregion

		#region # * result reporting 

		gap_fl = 1 - lowLim_fl/currentBest_fl
		produceMessage(report_m.options,report_m.report, 1," - Lower: $(round(lowLim_fl, sigdigits = 8)), Upper: $(round(currentBest_fl, sigdigits = 8)), gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
		produceMessage(report_m.options,report_m.report, 1," - Time for top: $(Dates.toms(timeTop) / Dates.toms(Second(1))) Time for sub: $(Dates.toms(timeSub) / Dates.toms(Second(1)))", testErr = false, printErr = false)
		
		# write to reporting files
		push!(itrReport_df, (i = i, low = lowLim_fl, best = currentBest_fl, gap = gap_fl, solCur = objTopTrust_fl + objSub_fl, time = Dates.value(floor(now() - report_m.options.startTime,Dates.Second(1)))/60))
		CSV.write(modOptTop_tup.resultDir * "/iterationBenders$(replace(top_m.options.objName,"topModel" => "")).csv",  itrReport_df)
		
		#endregion
		
		#region # * check convergence and adjust limits
		
		# ! adjust limits where they are binding
		if method in (:all,:fixAndLim) && gap_fl < solOpt_tup.gapLim 
			binLim_boo = checkLinearTrust(top_m)
			if binLim_boo
				adjustLinearTrust!(top_m)
				produceMessage(report_m.options,report_m.report, 1," - Moved binding lower and upper limits!", testErr = false, printErr = false)
			end
		end

		# ! terminate or adjust quadratic trust region
		if gap_fl < solOpt_tup.gap && (!(method in (:all,:fixAndLim)) || !binLim_boo)
			produceMessage(report_m.options,report_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
			break
		elseif method in (:all,:fixAndQtr) # adjust trust region in case algorithm has not converged yet
			global trustReg_obj = adjustQuadTrust(top_m,allVal_dic,trustReg_obj,objSub_fl,objTopTrust_fl,lowLim_fl,lowLimTrust_fl,report_m)
		end
		#endregion
		
		i += 1
	end
end

#endregion

#region # * write final results and clean up

# run top-problem with optimal values fixed
top_m = computeFeas(top_m, trustReg_obj.var, solOpt_tup.linPar.thrsAbs)
foreach(x -> reportResults(x, top_m), [:summary, :cost])

# obtain capacities
capaData_obj = bendersData()
capaData_obj.capa = writeResult(top_m, [:capa], true)

# run sub-problems with optimal values fixed
solvedFut_dic = runAllSub(sub_tup, capaData_obj)
wait.(values(solvedFut_dic))

rm(temp_dir; force=true, recursive=true) # remove temporal files again

#endregion
