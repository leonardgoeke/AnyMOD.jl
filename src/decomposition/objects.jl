
#region # * algorithm setup

# setup for benders computation
mutable struct algSetup
	gap::Float64 # target gap
	delCut::NamedTuple{(:cnt, :thres), Tuple{Int, Float64}}  # number of iterations since cut creation or last binding before cut is deleted
	useVI::NamedTuple{(:bal, :st), Tuple{Bool, Bool}} # use vaild inequalities
	reportFreq::Int # number of iterations report files are written
	timeLim::Float64 # tuple with objectives
	dist::Bool # true if distributed computing used
	opt::DataType
	rngVio::NamedTuple{(:stab, :cut, :fix), Tuple{Float64, Float64, Float64}} # acceptable violation of target range for stabilization, cut, and fix of variables
	sub::NamedTuple{(:rng, :int, :crs, :meth, :timeLim, :dbInf, :threads, :check), Tuple{Vector{Float64}, Symbol, Bool, Symbol, Float64, Bool, Int, Bool}} # range and interpolation method for convergence criteria of subproblems, use of crossover for sub-problems when using barrier
	top::NamedTuple{(:numFoc, :dnsThrs, :crs, :stabTol, :stabTolQ, :noStabTol, :stabMeth, :noStabMeth, :threads, :check), Tuple{Array{Int64, 1}, Int64, Bool, Tuple{Symbol, Vector{Float64}}, Tuple{Symbol, Vector{Float64}}, Tuple{Symbol, Vector{Float64}}, Int, Int, Int, Bool}} # infeasible variable at start of foresight period, numeric focus for top-problem, factor by which quadratic trust-region is allowed to violate paramete range

	function algSetup(gap_fl::Float64, delCut_ntup::NamedTuple{(:cnt, :thres), Tuple{Int, Float64}}, useVI_ntup::NamedTuple{(:bal, :st), Tuple{Bool, Bool}}, repFreq_int::Int, timeLim_fl::Float64, dist_boo::Bool, opt_type::DataType, rngVio::NamedTuple{(:stab, :cut, :fix), Tuple{Float64, Float64, Float64}}, sub::NamedTuple{(:rng, :int, :crs, :meth, :timeLim, :dbInf, :threads, :check), Tuple{Vector{Float64}, Symbol, Bool, Symbol, Float64, Bool, Int, Bool}} = (rng = [1e-8, 1e-8], int = :log, crs = false, meth = :barrier, timeLim = 0.0, dbInf = true, threads = 1, check = false), top::NamedTuple{(:numFoc, :dnsThrs, :crs, :stabTol, :stabTolQ, :noStabTol, :stabMeth, :noStabMeth, :threads, :check), Tuple{Array{Int64, 1}, Int64, Bool, Tuple{Symbol, Vector{Float64}}, Tuple{Symbol, Vector{Float64}}, Tuple{Symbol, Vector{Float64}}, Int, Int, Int, Bool}} = (numFoc = [1,3], dnsThrs = 200, crs = true, stabTol = (:lin, [1e-6, 1e-6]), stabTolQ = (:lin, [1e-4, 1e-6]), noStabTol = (:lin, [1e-6, 1e-6]), stabMeth = -1, noStabMeth = -1, threads = 1, check = false))
		return new(gap_fl, delCut_ntup, useVI_ntup, repFreq_int, timeLim_fl, dist_boo, opt_type, rngVio, sub, top)
	end
end

# setup for stabilization
mutable struct stabSetup
	method::Tuple # method(s) for stabilization
	srsThr::Float64 # threshold for serious step
	ini::Symbol # rule for stabilization (:none will skip stabilization)
	lowLimVal::Float64
	solveNoStab::NamedTuple{(:upper, :inter, :sub), Tuple{Int64, Symbol, Float64}} 
	switch::NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}} # rule to switch between different methods
	weight::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), Tuple{Float64, Float64, Float64, Float64}} # weight of variables in stabilization
	repVio::Bool
	
	function stabSetup(method_tup::Tuple, srsThr_fl::Float64, ini_sym::Symbol, lowLimVal_fl::Float64, solveNoStab::NamedTuple{(:upper, :inter, :sub), Tuple{Int64, Symbol, Float64}}; repVio::Bool = false, switch::NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}} = (itr = 10, avgImp = 1e-5, itrAvg = 5), weight::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), Tuple{Float64, Float64, Float64, Float64}} = (capa = 1e0, capaStSize = 1e0, stLvl = 1e0, lim = 1e0))
		return new(method_tup, srsThr_fl, ini_sym, lowLimVal_fl, solveNoStab, switch, weight, repVio)
	end
end

# setup of near-optimal computation
struct nearOptSetup
	cutThres::Float64 # cost threshold to keep solution
	lssThres::Float64 # lss threshold to keep solution
	optThres::Float64 # cost threshold for optimization
	feasGap::Float64 # target feasibility gap
	delCut::NamedTuple{(:cnt, :thres), Tuple{Int, Float64}} # number of iterations that unused cuts are deleted during near-opt
	obj::NTuple #  tuple with objectives
	parThres::NamedTuple{(:dom, :zero), Tuple{Float64, Float64}} # thresholds for filtering pareto efficient solutions

	function nearOptSetup(cutThres::Float64, lssThres::Float64, optThres::Float64, feasGap::Float64, delCut::NamedTuple{(:cnt, :thres), Tuple{Int, Float64}} , obj::NTuple, parThres::NamedTuple{(:dom, :zero), Tuple{Float64, Float64}} = (dom = 0.005, zero = 1e-4))
		return new(cutThres, lssThres, optThres, feasGap, delCut, obj, parThres)
	end
end

mutable struct nearOptObj
	cnt::Int
	setup::Union{Nothing,nearOptSetup}
end

#endregion

#region # * result management

#  model results
mutable struct resData
	objVal::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	stLvl::Dict{Symbol,Dict{Symbol,DataFrame}}
	lim::Dict{Symbol,DataFrame}
	resData() = new(Inf, Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}(), Dict{Symbol,DataFrame}(), Dict{Symbol,DataFrame}())
end

# copy functions for model results
function copy(ben_obj::resData)
	out = resData()
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	out.stLvl = deepcopy(ben_obj.stLvl)
	out.lim = deepcopy(ben_obj.lim)
	return out
end

#endregion

#region # * iteration

# managing stabilization method
mutable struct stabObj
	method::Array{Symbol,1} # array of method names used for stabilization
	methodOpt::Array{NamedTuple,1} # array of options for adjustment of stabilization parameters
	solveNoStab::NamedTuple{(:upper, :inter, :sub), Tuple{Int64, Symbol, Float64}} 
	srsThr::Float64 # threshold for serious step
	lowLimVal::Float64 # lower limit for stabilization value (smaller values are rounded)
	ruleSw::Union{NamedTuple{(), Tuple{}}, NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}}} # rule for switching between stabilization methods
	weight::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), NTuple{4, Float64}} # weight of variables in stabilization
	actMet::Int # index of currently active stabilization method
	objVal::Float64 # array of objective value for current center
	lastSw::Int # iteration of last switch
	crossNoStab::Bool # use crossover for solving without stabilization
	dynPar::Array{Union{Dict,Float64},1} # array of dynamic parameters for each method
	repVio::Bool # report violations of range in quadratic stabilization
	var::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}} # variables subject to stabilization
	cns::ConstraintRef
	helper_var::VariableRef
	
	function stabObj(meth_tup::Tuple, srsThr_fl::Float64, lowLimVal_fl::Float64, ruleSw_ntup::NamedTuple, weight_ntup::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), NTuple{4, Float64}}, resData_obj::resData, lowBd_fl::Float64, solveNoStab_ntup::NamedTuple{(:upper, :inter, :sub), Tuple{Int64, Symbol, Float64}}, repVio_boo::Bool, top_m::anyModel)
		stab_obj = new()

		if !(isempty(ruleSw_ntup) || typeof(ruleSw_ntup) == NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64,Float64,Int64}})
			error("rule for switching stabilization method must be empty or have the fields 'itr', 'avgImp', and 'itrAvg'")
		end

		if !isempty(ruleSw_ntup) && ruleSw_ntup.itr < 2
			error("parameter 'itr' for  minimum iterations before switching stabilization method must be at least 2")
		end

		stab_obj.method, stab_obj.methodOpt, stab_obj.dynPar = writeStabOpt(meth_tup, lowBd_fl, resData_obj.objVal, top_m)
		stab_obj.solveNoStab = solveNoStab_ntup 

		# set other fields
		stab_obj.srsThr = srsThr_fl
		stab_obj.lowLimVal = lowLimVal_fl
		stab_obj.ruleSw = ruleSw_ntup
		stab_obj.weight = weight_ntup
		stab_obj.actMet = 1
		stab_obj.lastSw = 0
		stab_obj.crossNoStab = false
		stab_obj.objVal = resData_obj.objVal
		stab_obj.repVio = repVio_boo
		stab_obj.var = filterStabVar(resData_obj.capa, resData_obj.stLvl, resData_obj.lim, weight_ntup, top_m)
		
		# compute number of variables subject to stabilization
		stabCapa_arr = vcat(vcat(vcat(map(x -> stab_obj.var[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value], collect(keys(w)))), collect(keys(u)))), [:tech, :exc])...)...)...)
		stLvl_arr = vcat(vcat(map(x -> stab_obj.var[:stLvl][x] |> (u -> map(y -> u[y][!,:value], collect(keys(u)))), collect(keys(stab_obj.var[:stLvl])))...)...)
		lim_arr = vcat(map(x -> stab_obj.var[:lim][x][!,:value], collect(keys(stab_obj.var[:lim])))...)
		stabExpr_arr = vcat(stabCapa_arr, stLvl_arr, lim_arr)

		return stab_obj, size(stabExpr_arr, 1)
	end
	stabObj() = new()
end

# monitoring iteration 
mutable struct countItr
	i::Int
	srs::Int
	nextNoStab::Int
	null::Int
end

mutable struct itrStatus
	best::NamedTuple{(:var,:res,:startLvl),Tuple{resData,Dict{Symbol,DataFrame},Dict{Symbol,DataFrame}}}
	cnt::countItr
	gap::Float64
	res::Dict{Symbol,Float64} # store different results here
end

mutable struct cutObj
	active::Array{Int,1}
	prev::Array{Int,1}
	all::Array{Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}},1}
	slack::Array{Array{Float64,1},1}
	cnt::Int
end

# overall benders structure
mutable struct bendersObj
	top::anyModel
	topNoStab::NamedTuple{(:opt,:ref),Tuple{Model,GenericReferenceMap}}
	sub::Dict{Tuple{Int,Int},Union{Future,Task,anyModel}}
	cuts::cutObj
	complVar::Dict{Tuple{Int,Int},Dict{Symbol,DataFrame}}
	itr::itrStatus
	stab::Union{Nothing,stabObj}
    algOpt::algSetup
	nearOpt::nearOptObj
	trackCapa::Bool
	info::NamedTuple{(:name,:frsLvl,:supTsLvl,:repTsLvl,:shortExp), Tuple{String, Int64, Int64, Int64, Int64}}
	report::NamedTuple{(:itr,:nearOpt,:stabVio,:res,:mod),Tuple{DataFrame,DataFrame,DataFrame,NamedTuple,anyModel}}
	
	function bendersObj(info_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, scale_dic::Dict{Symbol,NamedTuple}, algSetup_obj::algSetup, stabSetup_obj::stabSetup, runSubDist::Function, getComVarDist::Function, resInfo::NamedTuple; trackCapa::Bool = false, nearOptSetup_obj::Union{Nothing,nearOptSetup} = nothing)

        #region # * checks and initialization

        benders_obj = new()
		benders_obj.info = info_ntup
		benders_obj.cuts = cutObj(Int[], Int[], Pair{Tuple{Int,Int,Int},Tuple{AffExpr,Bool}}[], Array{Array{Float64,1},1}(),0)
        benders_obj.algOpt = algSetup_obj
		benders_obj.nearOpt = nearOptObj(0, nearOptSetup_obj)
		benders_obj.trackCapa = trackCapa

		# initialize reporting
		initializeReporting!(benders_obj, stabSetup_obj, inputFolder_ntup, info_ntup, resInfo)

		#endregion
		
        #region # * create top- and sub-problems

		# start creating top-problem and extract info on sub-problem structure
		report_m = benders_obj.report.mod
		produceMessage(report_m.options, report_m.report, 1, " - Started creation of top-problem", testErr = false, printErr = false)

		top_m = anyModel(inputFolder_ntup.in, inputFolder_ntup.results, objName = "topModel_" * info_ntup.name, frsLvl = info_ntup.frsLvl, supTsLvl = info_ntup.supTsLvl, checkRng = (print = true, all = true), repTsLvl = info_ntup.repTsLvl, shortExp = info_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facTop], reportLvl = 1, createVI = algSetup_obj.useVI)
		sub_tup = tuple(sort([(x.Ts_dis, x.scr) for x in eachrow(top_m.parts.obj.par[:scrProb].data)])...) # get all time-step/scenario combinations

		# creation of sub-problems

		inputFolderSub_ntup = (in = inputFolder_ntup.in, heu = inputFolder_ntup.heu, results = inputFolder_ntup.results * "/sub")
		produceMessage(report_m.options, report_m.report, 1, " - Started creation of sub-problems", testErr = false, printErr = false)
		benders_obj.sub = Dict{Tuple{Int,Int},Union{Future,Task,anyModel}}()
		
		complCns_dic = Dict{Tuple{Int,Int},Dict{Symbol,DataFrame}}()
		for (id, s) in enumerate(sub_tup)
			subStr_tup = (top_m.sets[:Ts].nodes[s[1]].val, top_m.sets[:scr].nodes[s[2]].val)
			if benders_obj.algOpt.dist # distributed case
				benders_obj.sub[s] = @spawnat id + 1 begin
					global sub_m, comVar_dic = buildSub(myid() - 1, subStr_tup, info_ntup, inputFolderSub_ntup, scale_dic, algSetup_obj)
				end
			else # non-distributed case
				benders_obj.sub[s], complCns_dic[s] = buildSub(id, subStr_tup, info_ntup, inputFolderSub_ntup, scale_dic, algSetup_obj)
			end
		end
		benders_obj.complVar = complCns_dic

		# finish creation of top-problems
		top_m.subPro = tuple(0, 0)
		prepareMod!(top_m, benders_obj.algOpt.opt, benders_obj.algOpt.top.threads)

		# create separate variables for costs of subproblems
		top_m.parts.obj.var[:cut] = map(y -> map(x -> y == 1 ? sub_tup[x][1] : sub_tup[x][2], 1:length(sub_tup)), 1:2) |> (z -> createVar(DataFrame(Ts_dis = z[1], scr = z[2]), "subCut", NaN, top_m.optModel, top_m.lock, top_m.sets, scaFac = 1e2))
		push!(top_m.parts.obj.cns[:objEqn], (name = :aggCut, cns = @constraint(top_m.optModel, sum(top_m.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :benders, top_m.parts.obj.var[:objVar])[1,:var])))
		benders_obj.top = top_m
		
		if benders_obj.algOpt.dist 
			# wait for construction of sub-problems
			wait.(collect(values(benders_obj.sub)))
			# get information on complicating variable
			foreach(x -> complCns_dic[x[2]] = fetch(getComVarDist(1 + x[1])), enumerate(sub_tup))
		end

		produceMessage(report_m.options, report_m.report, 1, " - Finished creation of top-problem and sub-problems", testErr = false, printErr = false)
        #endregion

		# write complicating constraints into top problem
		writeComplCons!(benders_obj)

		# initialize stabilization
		prepareStab!(benders_obj, stabSetup_obj, inputFolder_ntup, info_ntup, scale_dic, runSubDist)

		return benders_obj
	end
	bendersObj() = new()
end

#endregion

