using AnyMOD, Gurobi, CSV, YAML

b = "C:/Users/pacop/Desktop/git/EuSysMod/"

#region # * options for algorithm

# ! options for general algorithm

conSub_tup = (rng = [1e-8,1e-8], int = :log, crs = false) # range and interpolation method for convergence criteria of subproblems
numOpt_tup = (dbInf = true, numFoc = 3, addVio = 1e4) # options for handling numeric problems
distr_boo = true;

# target gap, number of iteration after unused cut is deleted, 2x see above, number of iterations report is written, time-limit for algorithm, distributed computing?, # number of threads, optimizer
algSetup_obj = algSetup(0.001, 20, conSub_tup, numOpt_tup, (bal = false, st = false), 100, 120.0, distr_boo, 4, Gurobi.Optimizer)

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

stabSetup_obj = stabSetup(meth_tup, 0.0, swt_ntup, weight_ntup, iniStab_ntup)


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
	resData_obj, stabVar_obj = @suppress runTop(benders_obj); 
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
	if !isnothing(benders_obj.stab) @suppress runTopWithoutStab!(benders_obj) end

	# get results of sub-problems
	if benders_obj.algOpt.dist
		wait.(collect(values(futData_dic)))
		for s in collect(keys(benders_obj.sub))
			time_dic[s], cutData_dic[s] = fetch(futData_dic[s])
		end
	end
	
	#endregion

	#region # * analyse results and update refinements

	# update results and stabilization
	updateIteration!(benders_obj, cutData_dic, stabVar_obj)

	# report on iteration
	reportBenders!(benders_obj, nOpt_int)

	# check convergence and finish
	rtn_boo = checkConvergence(benders_obj)
	if rtn_boo break end
	i = i + 1

	#endregion

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


