
using Base.Threads, CSV, Dates, LinearAlgebra, Requires
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

using Gurobi, AnyMOD

# ! set options paramters

t_int = 4 # numbers of cores

# options of solution algorithm
solOpt_tup = (gap = 0.01, alg = :benders, heu = :both, linPar = (zeroThrs = 0.005, capaThrs = 0.05), quadPar = (startRad = 1e-2, lowRad = 1e-5 , shrThrs = 0.001, extThrs = 0.001))

# options for model creation
reso_tup = (heu = 2, mod = 4) # resolution during heuristic and full detail
b = "C:/Users/pacop/Desktop/work/git/TheModel/" # general dir
inDir_arr = [[b * "_basis",b * "_full",b * "timeSeries/" * string(x) * "days_2010"] for x in [reso_tup.heu, reso_tup.mod]] # input directories
modOpt_tup = (supTsLvl = 2, shortExp = 5, opt = Gurobi.Optimizer, topSuffix = "_2ctr_both", resultDir = b * "results", heuIn = inDir_arr[1], modIn = inDir_arr[2])

sub_tup = ((1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)) # structure of subproblems, indicating the year (first integer) and the scenario (second integer)

#region # * create top and sublevel problems 

# ! create top level problem
top_m = anyModel(modOpt_tup.modIn,modOpt_tup.resultDir, objName = "topModel" * modOpt_tup.topSuffix, supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, reportLvl = 1)
top_m.subPro = tuple(0,0)
prepareMod!(top_m,modOpt_tup.opt)
set_optimizer_attribute(top_m.optModel, "Threads", t_int)

# ! create sub level problems (geht parallel)

sub_dic = Dict{Tuple{Int,Int},anyModel}()
sub_lock = ReentrantLock()

for (id,x) in enumerate(sub_tup)
    # create sub problem
	s = anyModel(modOpt_tup.modIn,modOpt_tup.resultDir, objName = "subModel_" * string(id), supTsLvl = modOpt_tup.supTsLvl, shortExp = modOpt_tup.shortExp, reportLvl = 1)
    s.subPro = x
	prepareMod!(s,modOpt_tup.opt)
	set_optimizer_attribute(s.optModel, "Threads", t_int)
    sub_dic[x] = s
end

# create seperate variables for costs of subproblems and aggregate them (cannot be part of model creation, because requires information about subproblems) 
top_m.parts.obj.var[:cut] = map(y -> map(x -> y == 1 ? top_m.supTs.step[x] : sub_tup[x][2], 1:length(sub_tup)),1:2) |> (z -> createVar(DataFrame(Ts_disSup = z[1], scr = z[2]),"subCut",NaN,top_m.optModel,top_m.lock,top_m.sets, scaFac = 1e2))
push!(top_m.parts.obj.cns[:objEqn], (name = :aggCut, group = :benders, cns = @constraint(top_m.optModel, sum(top_m.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders,top_m.parts.obj.var[:objVar])[1,:var])))

#endregion

#region # * add linear and quadratic trust region

if solOpt_tup.heu == :linear || solOpt_tup.heu == :both
	produceMessage(top_m.options,top_m.report, 1," - Start to enforce trust-region(s)", testErr = false, printErr = false)
	# ! solve heuristic for scaled and compressed time-series (geht parallel, muss erst im zweiten teil auf sub problems warten)
	heuSca_obj, cutSca_dic = @suppress heuristicSolve(sub_dic,modOpt_tup,solOpt_tup.linPar.zeroThrs,1.0,t_int)
	heuCom_obj, cutCom_dic = @suppress heuristicSolve(sub_dic,modOpt_tup,solOpt_tup.linPar.zeroThrs,365/reso_tup.heu,t_int)

	# ! add cuts from heuristic solutions to top problem
	addCuts!(top_m,cutSca_dic,0)
	addCuts!(top_m,cutCom_dic,0)


	# ! add linear trust region 
	cFix_int = 0
	cLim_int = 0
	for sys in (:tech,:exc)
		part_dic = getfield(top_m.parts,sys)
		for sSym in keys(heuSca_obj.capa[sys])
			trust_str = part_dic[sSym].decomm == :none ? "exp" : "capa"
			for trstSym in filter(x -> occursin(trust_str,string(x)), collect(keys(heuSca_obj.capa[sys][sSym])))
				# match results from two different heuristic models
				bothCapa_df = rename(heuSca_obj.capa[sys][sSym][trstSym],:value => :value_1) |> (x -> innerjoin(x, rename(heuCom_obj.capa[sys][sSym][trstSym],:value => :value_2), on = intCol(x,:dir)))
				# determine values and types for limits
				bothCapa_df[!,:limVal], bothCapa_df[!,:limCns] = map(x -> getLinTrust(x.value_1,x.value_2,solOpt_tup.linPar.capaThrs), eachrow(bothCapa_df)) |> (w -> map(x -> getindex.(w,x),[1,2]))
				# write corresponding limits to top problem
				grpBothCapa_arr = collect(groupby(flatten(select(bothCapa_df,Not([:value_1,:value_2])),[:limVal,:limCns]),:limCns))
				trstVar_df = part_dic[sSym].var[trstSym] |> (w -> part_dic[sSym].decomm == :none ? collapseExp(w) : w)
				foreach(lim -> limitCapa!(select(rename(lim,:limVal => :value),Not([:limCns])),trstVar_df,trstSym,part_dic[sSym],top_m,lim[1,:limCns]),grpBothCapa_arr)
				fix_int = sum(map(x -> x == [:Fix],bothCapa_df[!,:limCns]))
				cFix_int = cFix_int + fix_int
				cLim_int = cLim_int + (size(bothCapa_df,1) - fix_int)		
			end
		end
	end
	currentBest_fl = heuSca_obj.objVal + sum(map(x -> x.objVal, values(cutSca_dic))) 
	produceMessage(top_m.options,top_m.report, 1," - Enforced linear trust region with $cFix_int fixed variables and $cLim_int limited variables", testErr = false, printErr = false)

	# ! add quadratic trust region
	if solOpt_tup.heu == :both
		# get variables not fixed by linear trust region and use them to define quadratic trust region
		qctVar_dic = getNonFixLin(top_m,heuSca_obj)
		trustReg_obj = quadTrust(qctVar_dic,solOpt_tup.quadPar)
		trustReg_obj.cns, trustReg_obj.coef = centerQuadTrust(trustReg_obj.exp,top_m,trustReg_obj.rad);
		trustReg_obj.objVal = currentBest_fl
		produceMessage(top_m.options,top_m.report, 1," - Initialized quadratic trust region with limited variables", testErr = false, printErr = false)
	end

else
	currentBest_fl = Inf
end
#endregion

#region # * run benders iteration

# initialize loop variables
itrReport_df = DataFrame(i = Int[], low = Float64[], best = Float64[], gap = Float64[], solCur = Float64[])
capaReport_df = DataFrame(Ts_expSup = Int[], Ts_disSup= Int[], R_exp= Int[], Te= Int[], id = Int[], scr = Int[], i = Int[], variable = String[], value = Float64[], dual = Float64[]) 

gap_fl = 1.0
i = 1
cutData_dic = Dict{Tuple{Int64,Int64},bendersData}()

while true

	produceMessage(top_m.options,top_m.report, 1," - Started iteration $i", testErr = false, printErr = false)

	#region # * solve top level problem

	startTop = now()
	capaData_obj, expTrust_dic, objTopTrust_fl, lowLimTrust_fl = runTopLevel(top_m,cutData_dic,i)
	timeTop = now() - startTop

	#endregion
	
	#region # * solve of sublevel problems	

	startSub = now()
	for x in collect(sub_tup)
		dual_etr = runSubLevel(sub_dic[x],copy(capaData_obj))
		cutData_dic[x] = dual_etr
	end
	timeSub = now() - startSub

	#endregion

	#region # * compute bounds and adjust quadratic trust region

	if solOpt_tup.heu == :both
		# run top-problem without trust region to obtain lower limits
		objTop_fl, lowLim_fl = runTopWithoutQuadTrust(top_m,trustReg_obj)
		# adjust trust region
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # summed objective of sub-problems # ! hier warten auf subprobleme
		# write current best solution
		currentBest_fl = min(objTopTrust_fl + objSub_fl,trustReg_obj.objVal)
	else
		lowLim_fl = lowLimTrust_fl # without quad trust region, lower limit corresponds result of standard top problem
		objSub_fl = sum(map(x -> x.objVal, values(cutData_dic))) # summed objective of sub-problems # ! hier warten auf subprobleme
		currentBest_fl = (objSub_fl + objTopTrust_fl) < currentBest_fl ? (objSub_fl + objTopTrust_fl) : currentBest_fl
	end

	#endregion

	#region # * reporting on results and convergence check
	
	gap_fl = 1 - lowLim_fl/currentBest_fl
	produceMessage(top_m.options,top_m.report, 1," - Lower: $(round(lowLim_fl, sigdigits = 8)), Upper: $(round(currentBest_fl, sigdigits = 8)), gap: $(round(gap_fl, sigdigits = 4))", testErr = false, printErr = false)
	produceMessage(top_m.options,top_m.report, 1," - Time for top: $(Dates.toms(timeTop) / Dates.toms(Second(1))) Time for sub: $(Dates.toms(timeSub) / Dates.toms(Second(1)))", testErr = false, printErr = false)
	
	# write to reporting files
	push!(itrReport_df, (i = i, low = lowLim_fl, best = currentBest_fl, gap = gap_fl, solCur = objTopTrust_fl + objSub_fl))
	
	#reportCapa!(i,cutData_dic,capaReport_df)
	CSV.write(modOpt_tup.resultDir * "/iterationBenders_$(replace(top_m.options.objName,"topModel" => "")).csv",  itrReport_df)

	if (1- lowLim_fl/currentBest_fl) < solOpt_tup.gap
		produceMessage(top_m.options,top_m.report, 1," - Finished iteration!", testErr = false, printErr = false)
		# TODO add checks for linear trust region and binding but rounded cuts 
		break
	elseif solOpt_tup.heu == :both # adjust trust region in case algorithm has not converged yet
		trustReg_obj = adjustQuadTrust(top_m,expTrust_dic,trustReg_obj,objSub_fl,objTopTrust_fl,lowLim_fl,lowLimTrust_fl)
	end

	i = i +1

	#endregion
	
end

#endregion

# ! STEINBRUCH 

# analyse für slack auf limits (entferne vorher trust und löse genau?)
for sys in (:tech,:exc)
	part_dic = getfield(top_m.parts,sys)
	for sSym in keys(part_dic)
		for limCns in filter(x -> any(occursin.(["BendersUp","BendersLow"],string(x))), keys(part_dic[sSym].cns))
			bla = findall(map(x -> (value(x) - normalized_rhs(x)) < 1e-6, part_dic[sSym].cns[limCns][!,:cns]))
			println(part_dic[sSym].cns[limCns][bla,:cns])
		end
	end
end


