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

using Gurobi #, AnyMOD, CSV
t_int = 4

# ! set options

method = :all # optins are :all, :fixAndLim, :fixAndQtr :onlyFix and :none
resHeu = 2
resMod = 4
b = "C:/work_in_progress/git/TheModel/"

#region # * set and write options

# ! intermediate definitions of parameters

reso_tup = (heu = resHeu, mod = resMod) 
suffix_str = "_" * string(method) * "_" * string(resHeu) * "_" * string(resMod)
inDir_arr = [[b * "_basis",b * "_full",b * "timeSeries/" * string(x) * "days_2010"] for x in [reso_tup.heu, reso_tup.mod]] # input directories

coefRng_tup = (mat = (1e-2,1e4), rhs = (1e0,1e4))

scaFacHeu_tup = (capa = 1e2,  capaStSize = 1e2, insCapa = 1e1,dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e2, obj = 1e0)
scaFacTop_tup = (capa = 1e0, capaStSize = 1e1, insCapa = 1e0, dispConv = 1e3, dispSt = 1e5, dispExc = 1e3, dispTrd = 1e3, costDisp = 1e1, costCapa = 1e0, obj = 1e1)
scaFacSub_tup = (capa = 1e2,  capaStSize = 1e2, insCapa = 1e1,dispConv = 1e1, dispSt = 1e3, dispExc = 1e2, dispTrd = 1e2, costDisp = 1e0, costCapa = 1e2, obj = 1e0)

# ! general input parameters

opt_obj = Gurobi.Optimizer # solver option

# structure of subproblems, indicating the year (first integer) and the scenario (second integer)
sub_tup = ((1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0))

# options of solution algorithm
solOpt_tup = (gap = 0.001, delCut = 30, linPar = (thrsAbs = 0.001, thrsRel = 0.02), quadPar = (startRad = 1e-2, lowRad = 1e-6, shrThrs = 0.001, extThrs = 0.001))

# options for different models
temp_dir = b * "tempFix" * suffix_str # directory for temporary folder

optMod_dic = Dict{Symbol,NamedTuple}()

# options for model generation 
optMod_dic[:heu] =  (inputDir = inDir_arr[1], resultDir = b * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRng_tup, scaFac = scaFacHeu_tup)
optMod_dic[:top] =  (inputDir = inDir_arr[2], resultDir = b * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRng_tup, scaFac = scaFacTop_tup)
optMod_dic[:sub] =  (inputDir = inDir_arr[2], resultDir = b * "results", suffix = suffix_str, supTsLvl = 2, shortExp = 5, coefRng = coefRng_tup, scaFac = scaFacSub_tup)

#endregion

report_m = @suppress anyModel(String[],optMod_dic[:heu].resultDir, objName = "decomposition" * optMod_dic[:heu].suffix) # creates empty model just for reporting

#region # * solve heuristic models and write results

if method in (:all,:fixAndLim,:onlyFix,:fixAndQtr)
	# ! heuristic solve for re-scaled and compressed time-series
	produceMessage(report_m.options,report_m.report, 1," - Started heuristic pre-solve", testErr = false, printErr = false)
	heu_m, heuSca_obj = heuristicSolve(optMod_dic[:heu],1.0,t_int,opt_obj);
	~, heuCom_obj = heuristicSolve(optMod_dic[:heu],365/reso_tup.heu,t_int,opt_obj)
	# ! write fixes to files and limits to dictionary
	fix_dic, lim_dic, cntHeu_arr = evaluateHeu(heu_m,heuSca_obj,heuCom_obj,solOpt_tup.linPar) # get fixed and limited variables
	produceMessage(report_m.options,report_m.report, 1," - Get an exact feasible solution", testErr = false, printErr = false)
	feasFix_dic = getFeasResult(optMod_dic[:top],fix_dic,lim_dic,t_int,solOpt_tup.linPar.thrsAbs,opt_obj) # ensure feasiblity with fixed variables
	produceMessage(report_m.options,report_m.report, 1," - Heuristic found $(cntHeu_arr[1]) fixed variables and $(cntHeu_arr[2]) limited variables", testErr = false, printErr = false)
	# ! write fixed variable values to files
	writeFixToFiles(fix_dic,feasFix_dic,temp_dir,heu_m)
end

#endregion

#region # * create top and sublevel problems 
produceMessage(report_m.options,report_m.report, 1," - Create top model and sub models", testErr = false, printErr = false)

# ! create top level problem

modOpt_tup = optMod_dic[:top]
inputDir_arr = method in (:all,:fixAndLim,:fixAndQtr,:onlyFix) ? vcat(modOpt_tup.inputDir,[temp_dir]) : modOpt_tup.inputDir
top_m = anyModel(inputDir_arr, modOpt_tup.resultDir, objName = "topModel" * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, reportLvl = 1, holdFixed = true, checkRng = true)
top_m.subPro = tuple(0,0)
prepareMod!(top_m,opt_obj,t_int)

# ! create sub level problems

modOpt_tup = optMod_dic[:sub]
inputDir_arr = method in (:all,:fixAndLim,:fixAndQtr,:onlyFix) ? vcat(modOpt_tup.inputDir,[temp_dir]) : modOpt_tup.inputDir

sub_dic = Dict{Tuple{Int,Int},anyModel}()

for (id,x) in enumerate(sub_tup)
	# create sub problem
	s = anyModel(inputDir_arr,modOpt_tup.resultDir, objName = "subModel_" * string(id) * modOpt_tup.suffix, supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, coefRng = modOpt_tup.coefRng, scaFac = modOpt_tup.scaFac, reportLvl = 1, holdFixed = true, checkRng = true)
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
			dual_etr = runSubLevel(sub_dic[x],copy(z))
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
		# add cuts to top problem
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
		trustReg_obj.cns, trustReg_obj.coef, trustReg_obj.abs = centerQuadTrust(trustReg_obj.var,top_m,trustReg_obj.rad);
		trustReg_obj.objVal = Inf
		produceMessage(report_m.options,report_m.report, 1," - Initialized quadratic trust region with $eleNum_int variables", testErr = false, printErr = false)
	end
end

#endregion

#region # * run benders iteration

# initialize loop variables
itrReport_df = DataFrame(i = Int[], low = Float64[], best = Float64[], gap = Float64[], solCur = Float64[], time = Float64[])
capaReport_df = DataFrame(Ts_expSup = Int[], Ts_disSup= Int[], R_exp= Int[], Te= Int[], id = Int[], scr = Int[], i = Int[], variable = String[], value = Float64[], dual = Float64[]) 

gap_fl = 1.0
i = 1
cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()
currentBest_fl = Inf

if method in (:all,:fixAndLim) 
	# create dataframe of all limits to track binding ones
	allLimit_df = DataFrame(Ts_expSup = Int[], Ts_disSup = Int[], R_exp = Int[], R_from = Int[], R_to = Int[], Te = Int[], Exc = Int[], dir = Int[], id = Int[], limCns = Symbol[])

	for sys in (:tech,:exc)
		for sSym in keys(lim_dic[sys])
			for varSym in keys(lim_dic[sys][sSym])
				lim_df = select(lim_dic[sys][sSym][varSym],Not([:limVal]))
				foreach(x -> lim_df[!,x] .= 0 ,setdiff(intCol(allLimit_df),intCol(lim_df)))
				append!(allLimit_df,lim_df)
			end
		end
	end

	allLimit_df[!,:actItr] = fill(Array{Int,1}(),size(allLimit_df,1))
	allLimit_df[!,:actBestItr] = fill(Array{Int,1}(),size(allLimit_df,1))
end

while true

	global i = i

	produceMessage(report_m.options,report_m.report, 1," - Started iteration $i", testErr = false, printErr = false)

	#region # * solve top level problem

	startTop = now()
	capaData_obj, allVal_dic, objTopTrust_fl, lowLimTrust_fl = @suppress runTopLevel(top_m,cutData_dic,i);
	timeTop = now() - startTop

	#endregion
	
	#region # * solve of sublevel problems

	startSub = now()
	for x in collect(sub_tup)
		dual_etr = @suppress runSubLevel(sub_dic[x],copy(capaData_obj))
		cutData_dic[x] = dual_etr
	end
	timeSub = now() - startSub

	#endregion

	#region # * compute bounds and adjust quadratic trust region

	if method in (:all,:fixAndQtr)
		# run top-problem without trust region to obtain lower limits
		objTop_fl, lowLim_fl = @suppress runTopWithoutQuadTrust(top_m,trustReg_obj)
		# adjust trust region
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # summed objective of sub-problems # ! hier warten auf subprobleme
		# write current best solution
		global currentBest_fl = min(objTopTrust_fl + objSub_fl,trustReg_obj.objVal)

		#region # * track limits
		# track binding limits
		for sys in (:tech,:exc)
			part_dic = getfield(top_m.parts,sys)
			for sSym in keys(part_dic)
				for limCns in filter(x -> any(occursin.(["BendersUp","BendersLow"],string(x))), keys(part_dic[sSym].cns))
					# detect binding constraints
					lim_df = copy(select(part_dic[sSym].cns[limCns],Not([:fac,:cns])))
					lim_df[!,:limCns] .= occursin("BendersUp",string(limCns)) ? :Low : :Up
					lim_df[!,:act] = map(x -> dual(x) != 0.0, part_dic[sSym].cns[limCns][!,:cns])
					# merge info into dataframe for all limits
					joinLim_df = innerjoin(lim_df,allLimit_df,on = intCol(lim_df,[:limCns,:dir]))
					joinLim_df[!,:actItr] = map(x -> x.act ? vcat(x.actItr,[i]) : x.actItr,eachrow(joinLim_df))
					joinLim_df[!,:actBestItr] = map(x -> x.act && (objTopTrust_fl + objSub_fl == currentBest_fl) ? vcat(x.actBestItr,[i]) : x.actBestItr,eachrow(joinLim_df))
					global allLimit_df = vcat(antijoin(allLimit_df, select(joinLim_df,Not([:act])), on = intCol(joinLim_df,[:limCns,:dir])),select(joinLim_df,Not([:act])))
				end
			end
		end
		CSV.write(modOpt_tup.resultDir * "/limitTracking_$(replace(top_m.options.objName,"topModel" => "")).csv",  allLimit_df)
		#endregion
	else
		lowLim_fl = lowLimTrust_fl # without quad trust region, lower limit corresponds result of standard top problem
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # summed objective of sub-problems
		global currentBest_fl = (objSub_fl + objTopTrust_fl) < currentBest_fl ? (objSub_fl + objTopTrust_fl) : currentBest_fl
	end

	#endregion

	#region # * track and delete cuts

	# tracking latest binding iteration for cuts
	top_m.parts.obj.cns[:bendersCuts][!,:actItr] .= map(x -> dual(x.cns) != 0.0 ? i : x.actItr, eachrow(top_m.parts.obj.cns[:bendersCuts]))
	# delete cuts that were not binding long enough
	delete.(top_m.optModel, filter(x -> x.actItr + solOpt_tup.delCut < i,top_m.parts.obj.cns[:bendersCuts])[!,:cns])
	filter!(x -> (x.actItr + solOpt_tup.delCut > i),top_m.parts.obj.cns[:bendersCuts])

	#endregion

	#region # * reporting on results and convergence check
	
	global gap_fl = 1 - lowLim_fl/currentBest_fl
	produceMessage(report_m.options,report_m.report, 1," - Lower: $(round(lowLim_fl, sigdigits = 8)), Upper: $(round(currentBest_fl, sigdigits = 8)), gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
	produceMessage(report_m.options,report_m.report, 1," - Time for top: $(Dates.toms(timeTop) / Dates.toms(Second(1))) Time for sub: $(Dates.toms(timeSub) / Dates.toms(Second(1)))", testErr = false, printErr = false)
	
	# write to reporting files
	push!(itrReport_df, (i = i, low = lowLim_fl, best = currentBest_fl, gap = gap_fl, solCur = objTopTrust_fl + objSub_fl, time = Dates.value(floor(now() - report_m.options.startTime,Dates.Second(1)))/60))
	CSV.write(modOpt_tup.resultDir * "/iterationBenders$(replace(top_m.options.objName,"topModel" => "")).csv",  itrReport_df)

	if gap_fl < solOpt_tup.gap
		produceMessage(report_m.options,report_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
		break
	elseif method in (:all,:fixAndQtr) # adjust trust region in case algorithm has not converged yet
		global trustReg_obj = adjustQuadTrust(top_m,allVal_dic,trustReg_obj,objSub_fl,objTopTrust_fl,lowLim_fl,lowLimTrust_fl,report_m)
	end

	global i = i + 1

	#endregion
	
end

#endregion

#region # * write final results and clean up

# run top problem with optimal values fixed
top_m = computeFeas(top_m,trustReg_obj.var,solOpt_tup.linPar.thrsAbs)
foreach(x -> reportResults(x,top_m), [:summary,:cost])
	
# obtain capacities
capaData_obj = bendersData()
capaData_obj.capa = writeResult(top_m,[:capa],true)

# run sub problems with optimal values fixed
for x in collect(sub_tup)
	runSubLevel(sub_dic[x],copy(capaData_obj),true)
end

rm(temp_dir; force = true, recursive = true) # remove temporal files again

#endregion

