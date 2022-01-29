using Base.Threads, CSV, Dates, LinearAlgebra, Requires, DelimitedFiles
using MathOptInterface, Reexport, Statistics, PyCall, SparseArrays
using DataFrames, JuMP, Suppressor

pyimport_conda("networkx","networkx")
pyimport_conda("matplotlib.pyplot","matplotlib")
pyimport_conda("plotly","plotly")

include("src/objects.jl")
include("src/tools.jl")
include("src/modelCreation.jl")
include("src/decomposition.jl")

include("src/optModel/technology.jl")
include("src/optModel/exchange.jl")
include("src/optModel/system.jl")
include("src/optModel/cost.jl")
include("src/optModel/other.jl")
include("src/optModel/objective.jl")

include("src/dataHandling/mapping.jl")
include("src/dataHandling/parameter.jl")
include("src/dataHandling/readIn.jl")
include("src/dataHandling/tree.jl")
include("src/dataHandling/util.jl")

include("src/dataHandling/gurobiTools.jl")

using Gurobi# , AnyMOD, CSV
t_int = 4

# ! set options

method = :all # options are :all, :fixAndLim, :fixAndQtr :onlyFix and :none
resHeu = 96
resMod = 192
t_int = 2
dir_str = "C:/Users/pacop/Desktop/work/git/TheModel/"

#region # * set and write options

# ! intermediate definitions of parameters

reso_tup = (heu = resHeu, mod = resMod) 
suffix_str = "_" * string(method) * "_" * string(resHeu) * "_" * string(resMod)
inDir_arr = [[dir_str * "_basis",dir_str * "_full",dir_str * "timeSeries/" * string(x) * "hours_2008"] for x in [reso_tup.heu, reso_tup.mod]] # input directories

coefRngHeuSca_tup = (mat = (1e-2,1e4), rhs = (1e0,1e5))
coefRngHeuCom_tup = (mat = (1e-2,1e4), rhs = (1e0,1e4))
coefRngTop_tup 	  = (mat = (1e-2,1e4), rhs = (1e0,1e4))
coefRngSub_tup    = (mat = (1e-1,1e5), rhs = (1e1,1e5))

scaFacHeuSca_tup = (capa = 1e0, capaStSize = 1e2, insCapa = 1e1, dispConv = 1e1, dispSt = 1e3, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)
scaFacHeuCom_tup = (capa = 1e1, capaStSize = 1e2, insCapa = 1e1, dispConv = 1e0, dispSt = 1e1, dispExc = 1e1, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e1, obj = 1e0)
scaFacTop_tup  	 = (capa = 1e0, capaStSize = 1e1, insCapa = 1e0, dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e0, obj = 1e3)
scaFacSub_tup 	 = (capa = 1e2, capaStSize = 1e2, insCapa = 1e1, dispConv = 1e1, dispSt = 1e2, dispExc = 1e1, dispTrd = 1e1, costDisp = 1e0, costCapa = 1e2, obj = 1e1)

# ! general input parameters

opt_obj = Gurobi.Optimizer # solver option

# structure of subproblems, indicating the year (first integer) and the scenario (second integer)
sub_tup = ((1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0))
sub_tup = ((1,0),(2,0))

# options of solution algorithm
solOpt_tup = (gap = 0.001, gapLim = 0.005, gapSwitch = 0.05, delCut = 30, linPar = (thrsAbs = 0.05, thrsRel = 0.05), quadPar = (startRad = 1e-1, lowRad = 1e-6, shrThrs = 5e-4, extThrs = 5e-4))

# options for different models
temp_dir = dir_str * "tempFix" * suffix_str # directory for temporary folder

optMod_dic = Dict{Symbol,NamedTuple}()

# options for model generation 
optMod_dic[:heuSca] =  (inputDir = inDir_arr[1], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngHeuSca_tup, scaFac = scaFacHeuSca_tup)
optMod_dic[:heuCom] =  (inputDir = inDir_arr[1], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngHeuCom_tup, scaFac = scaFacHeuCom_tup)
optMod_dic[:top] 	=  (inputDir = inDir_arr[2], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngTop_tup,    scaFac = scaFacTop_tup)
optMod_dic[:sub] 	=  (inputDir = inDir_arr[2], resultDir = dir_str * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRngSub_tup,    scaFac = scaFacSub_tup)


#endregion

report_m = @suppress anyModel(String[],optMod_dic[:heuSca].resultDir, objName = "decomposition" * optMod_dic[:heuSca].suffix) # creates empty model just for reporting

produceMessage(report_m.options,report_m.report, 1," - With current range and scaling settings capacities below $(coefRngSub_tup.rhs[1]/coefRngSub_tup.mat[2]*scaFacSub_tup.capa) GW in the top-problem will be set to zero in the sub-problem.", testErr = false, printErr = false)

#region # * solve heuristic models and write results

if method in (:all,:fixAndLim,:onlyFix,:fixAndQtr)
	# ! heuristic solve for re-scaled and compressed time-series
	produceMessage(report_m.options,report_m.report, 1," - Started heuristic pre-solve", testErr = false, printErr = false)
	heu_m, heuSca_obj = @suppress heuristicSolve(optMod_dic[:heuCom],1.0,t_int,opt_obj);
	~, heuCom_obj = @suppress heuristicSolve(optMod_dic[:heuSca],365/reso_tup.heu,t_int,opt_obj)
	# ! write fixes to files and limits to dictionary
	fix_dic, lim_dic, cntHeu_arr = evaluateHeu(heu_m,heuSca_obj,heuCom_obj,solOpt_tup.linPar) # get fixed and limited variables
	produceMessage(report_m.options,report_m.report, 1," - Get an exact feasible solution", testErr = false, printErr = false)
	feasFix_dic = getFeasResult(optMod_dic[:top],fix_dic,lim_dic,t_int,solOpt_tup.linPar.thrsAbs,opt_obj) # ensure feasiblity with fixed variables
	produceMessage(report_m.options,report_m.report, 1," - Heuristic found $(cntHeu_arr[1]) fixed variables and $(cntHeu_arr[2]) limited variables", testErr = false, printErr = false)
	# ! write fixed variable values to files
	writeFixToFiles(fix_dic,feasFix_dic,temp_dir,heu_m)
	heu_m = nothing
end

#endregion

#region # * create top and sub-problems 
produceMessage(report_m.options,report_m.report, 1," - Create top model and sub models", testErr = false, printErr = false)

# ! create top-problem

modOpt_tup = optMod_dic[:top]
inputDir_arr = method in (:all,:fixAndLim,:fixAndQtr,:onlyFix) ? vcat(modOpt_tup.inputDir,[temp_dir]) : modOpt_tup.inputDir
top_m = anyModel(inputDir_arr, modOpt_tup.resultDir, objName = "topModel" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, reportLvl = 1, holdFixed = true)
top_m.subPro = tuple(0,0)
prepareMod!(top_m,opt_obj,t_int)

# ! create sub-problems

modOpt_tup = optMod_dic[:sub]
inputDir_arr = method in (:all,:fixAndLim,:fixAndQtr,:onlyFix) ? vcat(modOpt_tup.inputDir,[temp_dir]) : modOpt_tup.inputDir

sub_dic = Dict{Tuple{Int,Int},anyModel}()

for (id,x) in enumerate(sub_tup)
	# create sub-problem
	s = anyModel(inputDir_arr, modOpt_tup.resultDir, objName = "subModel_" * string(id) * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, reportLvl = 1, holdFixed = true)
	s.subPro = x
	prepareMod!(s,opt_obj,t_int)
	set_optimizer_attribute(s.optModel, "Threads", t_int)
	sub_dic[x] = s
end

# create seperate variables for costs of subproblems and aggregate them (cannot be part of model creation, because requires information about subproblems) 
top_m.parts.obj.var[:cut] = map(y -> map(x -> y == 1 ? top_m.supTs.step[x] : sub_tup[x][2], 1:length(sub_tup)),1:2) |> (z -> createVar(DataFrame(Ts_disSup = z[1], scr = z[2]),"subCut",NaN,top_m.optModel,top_m.lock,top_m.sets, scaFac = 1e2))
push!(top_m.parts.obj.cns[:objEqn], (name = :aggCut, group = :benders, cns = @constraint(top_m.optModel, sum(top_m.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])))

#endregion

#region # * add linear and quadratic trust region

if method in (:all,:fixAndLim,:fixAndQtr,:onlyFix)
	produceMessage(report_m.options,report_m.report, 1," - Create cuts from heuristic solution", testErr = false, printErr = false)
	# ! create cuts from heuristic solutions  
	for z in [heuCom_obj,heuSca_obj]
		# run subproblems and get cut info
		cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()
		for x in collect(sub_tup)
			dual_etr = runSub(sub_dic[x],copy(z),:barrier)
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
		addCuts!(top_m,cutData_dic,0)
	end
	
	# ! add linear trust region
	if method in (:all,:fixAndLim) 
		addLinearTrust!(top_m,lim_dic) 
		produceMessage(report_m.options,report_m.report, 1," - Enforced linear trust region", testErr = false, printErr = false)
	end

	# ! add quadratic trust region
	if method in (:all,:fixAndQtr)
		qctVar_dic = filterQtrVar(feasFix_dic,top_m) # get variables for quadratic trust region
		trustReg_obj, eleNum_int = quadTrust(qctVar_dic,solOpt_tup.quadPar)
		trustReg_obj.cns = centerQuadTrust(trustReg_obj.var,top_m,trustReg_obj.rad);
		trustReg_obj.objVal = Inf
		produceMessage(report_m.options,report_m.report, 1," - Initialized quadratic trust region with $eleNum_int variables", testErr = false, printErr = false)
	end
end

#endregion

#region # * run benders iteration

# initialize loop variables
itrReport_df = DataFrame(i = Int[], low = Float64[], best = Float64[], gap = Float64[], solCur = Float64[], time = Float64[])
cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()

let i = 1, gap_fl = 1.0, currentBest_fl = Inf, rmvLim_boo = false
	while true

		produceMessage(report_m.options,report_m.report, 1," - Started iteration $i", testErr = false, printErr = false)

		#region # * solve top-problem

		startTop = now()
		capaData_obj, allVal_dic, objTopTrust_fl, lowLimTrust_fl = @suppress runTop(top_m,cutData_dic,i);
		timeTop = now() - startTop

		#endregion
		
		#region # * solve of sub-problems

		startSub = now()
		for x in collect(sub_tup)
			dual_etr = @suppress runSub(sub_dic[x],copy(capaData_obj),solOpt_tup.gapSwitch < gap_fl ? :barrier : :simplex)
			cutData_dic[x] = dual_etr
		end
		timeSub = now() - startSub

		#endregion

		#region # * compute bounds and analyze cuts

		if method in (:all, :fixAndQtr) # run top-problem without trust region to obtain lower limits
			objTop_fl, lowLim_fl = @suppress runTopWithoutQuadTrust(top_m,trustReg_obj)
		else # without quad trust region, lower limit corresponds result of unaltered top-problem
			lowLim_fl = lowLimTrust_fl
		end

		# ! delete cuts that not were binding for the defined number of iterations
		deleteCuts!(top_m,solOpt_tup.delCut,i) 

		# ! get objective of sub-problems and current best solution
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # objective of sub-problems
		currentBest_fl = min(objTopTrust_fl + objSub_fl, currentBest_fl) # current best solution

		#endregion

		#region # * result reporting 

		gap_fl = 1 - lowLim_fl/currentBest_fl
		produceMessage(report_m.options,report_m.report, 1," - Lower: $(round(lowLim_fl, sigdigits = 8)), Upper: $(round(currentBest_fl, sigdigits = 8)), gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
		produceMessage(report_m.options,report_m.report, 1," - Time for top: $(Dates.toms(timeTop) / Dates.toms(Second(1))) Time for sub: $(Dates.toms(timeSub) / Dates.toms(Second(1)))", testErr = false, printErr = false)
		
		# write to reporting files
		push!(itrReport_df, (i = i, low = lowLim_fl, best = currentBest_fl, gap = gap_fl, solCur = objTopTrust_fl + objSub_fl, time = Dates.value(floor(now() - report_m.options.startTime,Dates.Second(1)))/60))
		CSV.write(modOpt_tup.resultDir * "/iterationBenders$(replace(top_m.options.objName,"topModel" => "")).csv",  itrReport_df)
		
		#endregion
		
		#region # * check convergence and adjust limits	
		
		if method in (:all,:fixAndLim) && gap_fl < solOpt_tup.gapLim && !rmvLim_boo
			# ! remove limits if they are binding anywhere
			binLim_boo = checkLinearTrust(top_m)
			if binLim_boo
				deleteLinearTrust!(top_m)	
				produceMessage(report_m.options,report_m.report, 1," - Removed lower and upper limits!", testErr = false, printErr = false)
			end
			rmvLim_boo = true
		elseif gap_fl < solOpt_tup.gap
			# ! terminate or adjust quadratic trust region
			produceMessage(report_m.options,report_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
			break
		end

		if method in (:all,:fixAndQtr) # adjust trust region in case algorithm has not converged yet
			global trustReg_obj = adjustQuadTrust(top_m,allVal_dic,trustReg_obj,objSub_fl,objTopTrust_fl,lowLim_fl,lowLimTrust_fl,report_m)
		end
		#endregion

		i = i + 1
	end
end

#endregion

#region # * write final results and clean up

# run top-problem with optimal values fixed
top_m = computeFeas(top_m,trustReg_obj.var,solOpt_tup.linPar.thrsAbs)
foreach(x -> reportResults(x,top_m), [:summary,:cost])
	
# obtain capacities
capaData_obj = bendersData()
capaData_obj.capa = writeResult(top_m,[:capa],true)

# run sub-problems with optimal values fixed
for x in collect(sub_tup)
	runSub(sub_dic[x],copy(capaData_obj),true)
end

rm(temp_dir; force = true, recursive = true) # remove temporal files again

#endregion


pefcSpace

r = sysInt(Symbol("30NL"),top_m.sets[:R])


tSym = :ccgtH2ChpDh
tInt = sysInt(tSym,anyM.sets[:Te])
part = anyM.parts.tech[tSym]
prepTech_dic = prepSys_dic[:Te][tSym]

top_m.capa[:tech][:ccgtGasChpProMed][:mustCapaConv]

heuSca_obj.capa[:tech][:ccgtGasChpProMed][:mustCapaConv]


feasFix_dic[:tech][tSym]