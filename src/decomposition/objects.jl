
#region # * algorithm setup

# setup for benders computation
mutable struct algSetup
	gap::Float64 # target gap
	delCut::Int # number of iterations since cut creation or last binding before cut is deleted
	useVI::NamedTuple{(:bal, :st), Tuple{Bool, Bool}} # use vaild inequalities
	reportFreq::Int # number of iterations report files are written
	timeLim::Float64 # tuple with objectives
	dist::Bool # true if distributed computing used
	threads::Int
	opt::DataType
	rngVio::NamedTuple{(:stab, :cut, :fix), Tuple{Float64, Float64, Float64}} # acceptable violation of target range for stabilization, cut, and fix of variables
	sub::NamedTuple{(:rng, :int, :crs, :meth, :timeLim, :dbInf), Tuple{Vector{Float64}, Symbol, Bool, Symbol, Float64, Bool}} # range and interpolation method for convergence criteria of subproblems, use of crossover for sub-problems when using barrier
	top::NamedTuple{(:numFoc, :dnsThrs, :crs), Tuple{Int64, Int64, Bool}} # infeasible variable at start of foresight period, numeric focus for top-problem, factor by which quadratic trust-region is allowed to violate paramete range

	function algSetup(gap_fl::Float64, delCut_int::Int, useVI_ntup::NamedTuple{(:bal, :st), Tuple{Bool, Bool}}, repFreq_int::Int, timeLim_fl::Float64, dist_boo::Bool, threads_int::Int, opt_type::DataType, rngVio::NamedTuple{(:stab, :cut, :fix), Tuple{Float64, Float64, Float64}}, sub::NamedTuple{(:rng, :int, :crs, :meth, :timeLim, :dbInf), Tuple{Vector{Float64}, Symbol, Bool, Symbol, Float64, Bool}} = (rng = [1e-8, 1e-8], int = :log, crs = false, meth = :barrier, timeLim = 0.0, dbInf = true), top::NamedTuple{(:numFoc, :dnsThrs, :crs),Tuple{Int64,Int64,Bool}} = (numFoc = 3, dnsThrs = 200, crs = true))
		return new(gap_fl, delCut_int, useVI_ntup, repFreq_int, timeLim_fl, dist_boo, threads_int, opt_type, rngVio, sub, top)
	end
end

# setup for stabilization
struct stabSetup
	method::Tuple # method(s) for stabilization
	srsThr::Float64 # threshold for serious step
	ini::NamedTuple{(:setup, :det), Tuple{Symbol, Bool}} # rule for stabilization (:none will skip stabilization)
	switch::NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}} # rule to switch between different methods
	weight::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), Tuple{Float64, Float64, Float64, Float64}} # weight of variables in stabilization
	
	function stabSetup(method_tup::Tuple, srsThr_fl::Float64, ini_ntup::NamedTuple{(:setup, :det), Tuple{Symbol, Bool}}, switch::NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}} = (itr = 10, avgImp = 1e-5, itrAvg = 5), weight::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), Tuple{Float64, Float64, Float64, Float64}} = (capa = 1e0, capaStSize = 1e0, stLvl = 1e0, lim = 1e0))
		return new(method_tup, srsThr_fl, ini_ntup, switch, weight)
	end
end

# setup of near-optimal computation
struct nearOptSetup
	cutThres::Float64 # cost threshold to keep solution
	lssThres::Float64 # lss threshold to keep solution
	optThres::Float64 # cost threshold for optimization
	feasGap::Float64 # target feasibility gap
	delCut::Int64 # number of iterations that unused cuts are deleted during near-opt
	obj::NTuple #  tuple with objectives
	parThres::NamedTuple{(:dom, :zero), Tuple{Float64, Float64}} # thresholds for filtering pareto efficient solutions

	function nearOptSetup(cutThres::Float64, lssThres::Float64, optThres::Float64, feasGap::Float64, delCut::Int64, obj::NTuple, parThres::NamedTuple{(:dom, :zero), Tuple{Float64, Float64}} = (dom = 0.005, zero = 1e-4))
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
	srsThr::Float64 # threshold for serious step
	ruleSw::Union{NamedTuple{(), Tuple{}}, NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}}} # rule for switching between stabilization methods
	weight::NamedTuple{(:capa,:capaStSize,:stLvl, :lim), NTuple{4, Float64}} # weight of variables in stabilization
	actMet::Int # index of currently active stabilization method
	objVal::Float64 # array of objective value for current center
	lastSw::Int # iteration of last switch
	dynPar::Array{Union{Dict,Float64},1} # array of dynamic parameters for each method
	var::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,DataFrame}},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}} # variables subject to stabilization
	cns::ConstraintRef
	
	function stabObj(meth_tup::Tuple, srsThr_fl::Float64, ruleSw_ntup::NamedTuple, weight_ntup::NamedTuple{(:capa, :capaStSize, :stLvl, :lim), NTuple{4, Float64}}, resData_obj::resData, lowBd_fl::Float64, top_m::anyModel)
		stab_obj = new()

		if !(isempty(ruleSw_ntup) || typeof(ruleSw_ntup) == NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64,Float64,Int64}})
			error("rule for switching stabilization method must be empty or have the fields 'itr', 'avgImp', and 'itrAvg'")
		end

		if !isempty(ruleSw_ntup) && ruleSw_ntup.itr < 2
			error("parameter 'itr' for  minimum iterations before switching stabilization method must be at least 2")
		end

		stab_obj.method, stab_obj.methodOpt, stab_obj.dynPar = writeStabOpt(meth_tup, lowBd_fl, resData_obj.objVal, top_m)
		
		# set other fields
		stab_obj.srsThr = srsThr_fl
		stab_obj.ruleSw = ruleSw_ntup
		stab_obj.weight = weight_ntup
		stab_obj.actMet = 1
		stab_obj.lastSw = 0
		stab_obj.objVal = resData_obj.objVal
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
	null::Int
end

mutable struct itrStatus
	best::NamedTuple{(:var,:res),Tuple{resData,Dict{Symbol,DataFrame}}}
	cnt::countItr
	gap::Float64
	res::Dict{Symbol,Float64} # store different results here
end

# overall benders structure
mutable struct bendersObj
	top::anyModel
	sub::Dict{Tuple{Int,Int},Union{Future,Task,anyModel}}
	cuts::Array{Pair{Tuple{Int,Int},Union{resData}},1}
	prevCuts::Array{Pair{Tuple{Int,Int},Union{resData}},1}
	itr::itrStatus
	stab::Union{Nothing,stabObj}
    algOpt::algSetup
	nearOpt::nearOptObj
	info::NamedTuple{(:name,:frsLvl,:supTsLvl,:repTsLvl,:shortExp), Tuple{String, Int64, Int64, Int64, Int64}}
	report::NamedTuple{(:itr,:nearOpt,:res,:mod),Tuple{DataFrame,DataFrame,NamedTuple,anyModel}}
	
	function bendersObj(info_ntup::NamedTuple{(:name, :frsLvl, :supTsLvl, :repTsLvl, :shortExp), Tuple{String, Int64, Int64, Int64, Int64}}, inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, scale_dic::Dict{Symbol,NamedTuple}, algSetup_obj::algSetup, stabSetup_obj::stabSetup, runSubDist::Function, getComVarDist::Function, resInfo::NamedTuple, nearOptSetup_obj::Union{Nothing,nearOptSetup} = nothing)

        #region # * checks and initialization

        benders_obj = new()
		benders_obj.info = info_ntup
        benders_obj.algOpt = algSetup_obj
		benders_obj.nearOpt = nearOptObj(0, nearOptSetup_obj)
	
		#endregion

		#region # * initialize reporting

        # dataframe for reporting during iteration
        itrReport_df = DataFrame(i = Int[], lowCost = Float64[], bestObj = Float64[], gap = Float64[], curCost = Float64[], time_ges = Float64[], time_top = Float64[], time_subTot = Float64[], time_sub = Array{Float64,1}[], numFoc = Array{Int,1}[], objName = String[])
        nearOpt_df = DataFrame(i = Int[], timestep = String[], region = String[], system = String[], id = String[], variable = Symbol[], value = Float64[], objName = String[])

        # empty model just for reporting
		report_m = @suppress anyModel(String[], inputFolder_ntup.results, objName = "decomposition" * info_ntup.name) 

		# add column for active stabilization method
		if !isempty(stabSetup_obj.method)
			itrReport_df[!,:actMethod] = fill(Symbol(), size(itrReport_df, 1))
			foreach(x -> itrReport_df[!,Symbol("dynPar_", x[1])] = Union{Float64,Vector{Float64}}[fill(Float64[], size(itrReport_df, 1))...], stabSetup_obj.method)
			select!(itrReport_df, vcat(filter(x -> x != :objName, namesSym(itrReport_df)), [:objName]))
		end

		# extend reporting dataframe in case of near-optimal
		if !isnothing(nearOptSetup_obj) itrReport_df[!,:objective] = fill("", size(itrReport_df, 1)) end

		benders_obj.report = (itr = itrReport_df, nearOpt = nearOpt_df, res = resInfo, mod = report_m)

		#endregion

        #region # * create top- and sub-problems

		# start creating top-problem and extract info on sub-problem structure
		produceMessage(report_m.options, report_m.report, 1, " - Started creation of top-problem", testErr = false, printErr = false)

		top_m = anyModel(inputFolder_ntup.in, inputFolder_ntup.results, objName = "topModel_" * info_ntup.name, frsLvl = info_ntup.frsLvl, supTsLvl = info_ntup.supTsLvl, repTsLvl = info_ntup.repTsLvl, shortExp = info_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facTop], reportLvl = 1, createVI = algSetup_obj.useVI)
		sub_tup = tuple(sort([(x.Ts_dis, x.scr) for x in eachrow(top_m.parts.obj.par[:scrProb].data)])...) # get all time-step/scenario combinations

		# creation of sub-problems

		produceMessage(report_m.options, report_m.report, 1, " - Started creation of sub-problems", testErr = false, printErr = false)
		benders_obj.sub = Dict{Tuple{Int,Int},Union{Future,Task,anyModel}}()
		
		complCns_dic = Dict{Tuple{Int,Int},Dict{Symbol,DataFrame}}()
		

		for (id, s) in enumerate(sub_tup)
			subStr_tup = (top_m.sets[:Ts].nodes[s[1]].val, top_m.sets[:scr].nodes[s[2]].val)
			if benders_obj.algOpt.dist # distributed case
				benders_obj.sub[s] = @spawnat id + 1 begin
					global sub_m, comVar_dic = buildSub(myid() - 1, subStr_tup, info_ntup, inputFolder_ntup, scale_dic, algSetup_obj)
				end
			else # non-distributed case
				benders_obj.sub[s], complCns_dic[s] = buildSub(id, subStr_tup, info_ntup, inputFolder_ntup, scale_dic, algSetup_obj)
			end
		end

		# finish creation of top-problems
		top_m.subPro = tuple(0, 0)
		@suppress prepareMod!(top_m, benders_obj.algOpt.opt, benders_obj.algOpt.threads)

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

		#region # * write complicating constraints into top problem
		
		relVar_arr::Vector{Symbol} = unique(vcat(filter(x -> !isempty(x), map(x -> collect(keys(complCns_dic[x])), collect(keys(complCns_dic))))...))
		# loop over types of complicating variables
		if !isempty(relVar_arr)
			addComplCns!(benders_obj.top, relVar_arr, complCns_dic)
			push!(top_m.report, (2, "limit", "", "enforced at least one limit across scenarios which creates a complicating constraint, Benders can not converge in case of overlapping complicating constraints (e.g., a national and system-wide emission limit)"))
			errorTest(unique(top_m.report), top_m.options, write = true)
			produceMessage(report_m.options, report_m.report, 1, " - Added complicating constraints to top-problem", testErr = false, printErr = false)
		end

		#endregion

		#region # * initialize stabilization

		benders_obj.stab, curBest_tup = initializeStab!(benders_obj, stabSetup_obj, inputFolder_ntup, info_ntup, scale_dic, complCns_dic, relVar_arr, runSubDist)
		benders_obj.itr = itrStatus(curBest_tup, countItr(isempty(benders_obj.report.itr) ? 0 : maximum(benders_obj.report.itr[!,:i]) + 1, 0, 0), 1.0, Dict{Symbol,Float64}())
		benders_obj.itr.res[:curBest] = curBest_tup.var.objVal

		#endregion

		return benders_obj
	end
	bendersObj() = new()
end

#endregion

