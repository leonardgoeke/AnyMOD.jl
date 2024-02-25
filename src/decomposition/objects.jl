# add reporting

# manage entire algorithm
mutable struct bendersObj
	top::anyModel
	sub::Union{Tuple,Dict{Tuple{Int,Int},anyModel}}
	stab::stabObj
    options::bendersSetup
	nearOpt::NamedTuple{(:cnt,:objVal,:setup), Tuple{Int,Float64,Union{Nothing,nearOptSetup}}}
	report::NamedTuple{(:itr, :nearOpt,:mod), Tuple{DataFrame, DataFrame, anyModel}}
	cost::NamedTuple{(:resData,:costOpt,:costEst,:curObj), Tuple{stabObj,Float64,Float64,Float64}} # results current best solution, global optimum, estimated cost current iteration, actual costs current iteration, objective of near optimum


	function bendersObj(genSetup_ntup::NamedTuple{(:name, :frs, :supTsLvl, :shortExp, :threads, :opt), Tuple{String, Int64, Int64, Int64, Int64, DataType}},inputFolder_ntup::NamedTuple{(:in, :heu, :results), Tuple{Vector{String}, Vector{String}, String}}, scale_dic::Dict{Symbol,NamedTuple},bendersSetup_obj::bendersSetup, stabSetup_obj::stabSetup, nearOptSetup_obj::Union{Nothing,nearOptSetup} = nothing)

        #region # * checks and initialization

        benders_obj = new()
        benders_obj.algOpt = bendersSetup_obj
		benders_obj.nearOpt = (cnt = 0, objVal = 0.0, setup = nearOptSetup_obj)

		if !isnothing(nearOptSetup_obj) && any(getindex.(meth_tup,1) .!= :qtr) error("Near-optimal can only be paired with quadratic stabilization!") end
	
		#endregion

        #region # * create top-problem

		top_m = @suppress anyModel(inputFolder_ntup.in, inputFolder_ntup.results, objName = "topModel" * genSetup_ntup.name, lvlFrs = genSetup_ntup.frs, supTsLvl = genSetup_ntup.supTs, shortExp = genSetup_ntup.shortExp, coefRng = scale_dic[:rng], scaFac = scale_dic[:facTop], reportLvl = 1, createVI = bendersSetup.useVI)
		sub_tup = tuple([(x.Ts_dis,x.scr) for x in eachrow(top_m.parts.obj.par[:scrProb].data)]...) # get all time-step/scenario combinations

		top_m.subPro = tuple(0,0)
		@suppress prepareMod!(top_m, genSetup_ntup.opt, genSetup_ntup.threads)

        benders_obj.top = top_m

        #endregion

        #region # * initialize reporting

        # dataframe for reporting during iteration
        itrReport_df = DataFrame(i = Int[], lowCost = Float64[], bestObj = Float64[], gap = Float64[], curCost = Float64[], time_ges = Float64[], time_top = Float64[], time_sub = Float64[])
        nearOpt_df = DataFrame(i = Int[], timestep = String[], region = String[], system = String[], id = String[], capacity_variable = Symbol[], capacity_value = Float64[], cost = Float64[], lss = Float64[])

        if !isnothing(nearOptSetup_obj) itrReport_df[!,:objective] = fill("",size(itrReport_df,1)) end

        # empty model just for reporting
		report_m = @suppress anyModel(String[],inputFolder_ntup.results, objName = "decomposition" * genSetup_ntup.name) 
		bendersObj.report = (itr =  itrReport_df, nearOpt = nearOpt_df, mod = report_m)

        #endregion

		# initialize stabilization
		benders_obj.stab = initializeStab(benders_obj,stabSetup_obj,inputFolder_ntup.heu,inputFolder_ntup.results,genSetup_ntup,scale_dic)

        return sub_tup

	end
end

#region # * result management

#  model results
mutable struct resData
	objVal::Float64
	capa::Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}
	stLvl::Dict{Symbol,DataFrame}
	resData() = new(Inf,Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}(),Dict{Symbol,DataFrame}())
end

# copy functions for model results
function copy(ben_obj::resData)
	out = resData()
	out.objVal = ben_obj.objVal
	out.capa = deepcopy(ben_obj.capa)
	out.stLvl = deepcopy(ben_obj.stLvl)
	return out
end

#endregion

#region # * stabilization

# managing stabilization method
mutable struct stabObj
	method::Array{Symbol,1} # array of method names used for stabilization
	methodOpt::Array{NamedTuple,1} # array of options for adjustment of stabilization parameters
	ruleSw::Union{NamedTuple{(), Tuple{}}, NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}}} # rule for switching between stabilization methods
	weight::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}} # weight of variables in stabilization
	actMet::Int # index of currently active stabilization method
	objVal::Float64 # array of objective value for current center
	dynPar::Array{Union{Dict,Float64},1} # array of dynamic parameters for each method
	var::Dict{Symbol,Union{Dict{Symbol,DataFrame},Dict{Symbol,Dict{Symbol,Dict{Symbol,DataFrame}}}}} # variables subject to stabilization
	cns::ConstraintRef
	helper_var::VariableRef
	
	function stabObj(meth_tup::Tuple, ruleSw_ntup::NamedTuple,weight_ntup::NamedTuple{(:capa,:capaStSize,:stLvl), NTuple{3, Float64}},resData_obj::resData,lowBd_fl::Float64,top_m::anyModel)
		stab_obj = new()

		if !(isempty(ruleSw_ntup) || typeof(ruleSw_ntup) == NamedTuple{(:itr,:avgImp,:itrAvg),Tuple{Int64,Float64,Int64}})
			error("rule for switching stabilization method must be empty or have the fields 'itr', 'avgImp', and 'itrAvg'")
		end

		if !isempty(ruleSw_ntup) && ruleSw_ntup.itr < 2
			error("parameter 'itr' for  minimum iterations before switching stabilization method must be at least 2")
		end

		stab_obj.method, stab_obj.methodOpt, stab_obj.dynPar = writeStabOpt(meth_tup,lowBd_fl,resData_obj.objVal,top_m)
		
		# set other fields
		stab_obj.ruleSw = ruleSw_ntup
		stab_obj.weight = weight_ntup
		stab_obj.actMet = 1
		stab_obj.objVal = resData_obj.objVal
		stab_obj.var = filterStabVar(resData_obj.capa,resData_obj.stLvl,weight_ntup,top_m)
		
		# compute number of variables subject to stabilization
		stabCapa_arr = vcat(vcat(vcat(map(x -> stab_obj.var[:capa][x] |> (u -> map(y -> u[y] |> (w -> map(z -> w[z][!,:value],collect(keys(w)))),collect(keys(u)))),[:tech,:exc])...)...)...)
		stLvl_arr = vcat(map(x -> stab_obj.var[:stLvl][x][!,:value],collect(keys(stab_obj.var[:stLvl])))...)
		stabExpr_arr = vcat(stabCapa_arr,stLvl_arr)

		return stab_obj, size(stabExpr_arr,1)
	end
end

#endregion

#region # * algorithm setup

# setup for benders computation
struct bendersSetup
	gap::Float64 # target gap
	srsThr::Float64 # threshold for serious step
	delCut::Int # number of iterations since cut creation or last binding before cut is deleted
	conSub::NamedTuple{(:rng, :int, :crs), Tuple{Vector{Float64}, Symbol, Bool}} # range and interpolation method for convergence criteria of subproblems
	solOpt::NamedTuple{(:dbInf, :numFoc, :addVio), Tuple{Bool, Int64, Float64}} # options for solving top problem
	useVI::NamedTuple{(:bal, :st), Tuple{Bool, Bool}} # use vaild inequalities
	reportFreq::Int # number of iterations report files are written
	timeLim::Float64 # tuple with objectives
end

# setup for stabilization
struct stabSetup
	method::Tuple # method(s) for stabilization
	switch::NamedTuple{(:itr, :avgImp, :itrAvg), Tuple{Int64, Float64, Int64}} # rule to switch between different methods
	weight::NamedTuple{(:capa, :capaStSize, :stLvl), Tuple{Float64, Float64, Float64}} # weight of variables in stabilization
	ini::NamedTuple{(:setup, :det), Tuple{Symbol, Bool}} # rule for stabilization (:none will skip stabilization)
end

# setup of near-optimal computation
struct nearOptSetup
	cutThres::Float64 # cost threshold to keep solution
	lssThres::Float64 # lss threshold to keep solution
	optThres::Float64 # cost threshold for optimization
	feasGap::Float64 # target feasibility gap
	cutDel::Int64 # number of iterations that unused cuts are deleted during near-opt
	obj::NTuple #  tuple with objectives
end



#endregion

