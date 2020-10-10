using CSV, DataFrames, Dates, JuMP, Statistics, LinearAlgebra, Base.Threads
using PyCall, SparseArrays, Gurobi

using Suppressor
using Gurobi

include("src/objects.jl")
include("src/tools.jl")
include("src/modelCreation.jl")

include("src/optModel/exchange.jl")
include("src/optModel/objective.jl")
include("src/optModel/other.jl")
include("src/optModel/tech.jl")

include("src/dataHandling/mapping.jl")
include("src/dataHandling/parameter.jl")
include("src/dataHandling/readIn.jl")
include("src/dataHandling/tree.jl")
include("src/dataHandling/util.jl")

#region # * functions and objects for benders specific data handling

# ! struct to save benders related results
mutable struct bendersData
	objValue::Float64
	capaTech::Dict{Symbol,Dict{Symbol,DataFrame}}
	capaExc::DataFrame
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,DataFrame}}(), DataFrame())
end

# ! copy functions for benders related results
function copy(ben_obj::bendersData)
	out = bendersData()
	out.objValue = ben_obj.objValue
	out.capaTech = deepcopy(ben_obj.capaTech)
	out.capaExc = deepcopy(ben_obj.capaExc)
	return out
end

import Base.isempty
isempty(ben_obj::bendersData) = isempty(ben_obj.capaTech) && isempty(ben_obj.capaExc)


# ! creates variables corresponding to limits on dual in the dual problem for bundle cutting
function createBoundElements(anyM::anyModel,variable_df::DataFrame)
	var_obj = JuMP.build_variable(error, VariableInfo(true, 0, false, NaN, false, NaN, false, NaN, false, false))
	cns_df = filter(x -> !isempty(x.var.terms),variable_df)
	cns_df[!,:lowDual] = map(x -> JuMP.add_variable(anyM.optModel, var_obj, "lowDual"), 1:size(cns_df,1))
	cns_df[!,:upDual] = map(x -> JuMP.add_variable(anyM.optModel, var_obj, "upDual"), 1:size(cns_df,1))
	cns_df[!,:cns] = map(x -> @constraint(anyM.optModel, collect(keys(x.var.terms))[1] - x.lowDual + x.upDual == 0.0), eachrow(cns_df))

	return cns_df
end

# ! gets the product of dual variables for bounds on duals and the upper and lower values on the duals
function getBoundObjective(var_dic::Dict{Symbol,DataFrame},bound_df::DataFrame,capa::Symbol)
	# join variables for upper and lower bound
	boundObj_df = rename(var_dic[Symbol(:ccbLow,makeUp(capa))],:var => :lowVar)
	boundObj_df = rename(innerjoin(boundObj_df,var_dic[Symbol(:ccbUp,makeUp(capa))], on = intCol(bound_df,:dir)),:var => :upVar)
	# add defined bounds and create expression for objective
	boundObj_df = innerjoin(boundObj_df,bound_df, on = intCol(bound_df,:dir))
	return sum(boundObj_df[!,:lowVar] .* boundObj_df[!,:lowBound]), sum(boundObj_df[!,:upVar] .* boundObj_df[!,:upBound])
end

# ! replaces the variable column with a column storing the value of the variable
function getCapa(capa_df::DataFrame)
	# filter entries without variable
	filter!(x -> !isempty(x.var.terms),capa_df)
	# write value of variable dataframe
	capa_df[!,:value] = map(x -> value(collect(keys(x.terms))[1]),capa_df[!,:var])
	return select(capa_df,Not([:var]))
end

# ! adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,fixCns_df::DataFrame)
	new_df = innerjoin(dual_df,fixCns_df, on = intCol(dual_df,:dir))
	new_df[!,:dual] = map(x -> dual(x), new_df[!,:cns])
	return select(new_df,Not([:cns]))
end

# ! fixes capacity variable from second dataframe according to value in the first
function fixCapa!(value_df::DataFrame,fixCns_df::DataFrame)
	if !isempty(value_df)
		capaSub_df = innerjoin(fixCns_df,value_df,on = intCol(value_df,:dir))|> (y -> y[completecases(y),:])
		foreach(x -> set_normalized_rhs(x.cns,x.value),eachrow(capaSub_df))
	end
end

# ! computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersExpr(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = innerjoin(variable_df,sub_df, on = intCol(sub_df,:dir))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *( collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
end

#endregion

#region # * benders related data analysis

# eta parts eigentlich nur in iteration nach der standard iteratio notwendig
function analyseCuts(bendersCut_arr::Array{Dict{Tuple{Int64,Int64},bendersData}},ccbCover::Float64)
		
	teCut_df =  DataFrame(var = Symbol[], Ts_expSup = Int[], Ts_disSup = Int[], R_exp = Int[], C = Int[], Te = Int[], value = Float64[], dual = Float64[])
	excCut_df = DataFrame(Ts_disSup = Int[], R_from = Int[], R_to = Int[], C = Int[], dir = Bool[], value = Float64[], dual = Float64[])

	# loop ovar all current cuts
	for bendersCut in bendersCut_arr
		for x in sub_tup
			for teSym in keys(bendersCut[x].capaTech), capaSym in keys(bendersCut[x].capaTech[teSym])
				te_df = copy(bendersCut[x].capaTech[teSym][capaSym])
				te_df[!,:var] .= capaSym
				if !(:C in namesSym(te_df))
					te_df[!,:C] .= 0
				end

				append!(teCut_df,te_df)
			end

			append!(excCut_df,bendersCut[x].capaExc)
		end
	end
	# aggregate data for different scenarios (value is always the same, duals are summed)
	teCut_df = combine([:value,:dual] => (a, b) -> (value=a[1], dual=sum(b)),groupby(teCut_df,intCol(teCut_df,:var)))
	excCut_df  = combine([:value,:dual] => (a, b) -> (value=a[1], dual=sum(b)),groupby(excCut_df,intCol(excCut_df)))

	# density of cut
	density_fl = (length(findall(teCut_df[!,:dual] .!= 0.0)) + length(findall(excCut_df[!,:dual] .!= 0.0))) / (size(teCut_df,1) + size(excCut_df,1))
	produceMessage(top_mod.options,top_mod.report, 1," - Density of cut: $(round(density_fl*100,digits = 3))%")
	# share of variables that are alpha covered
	allDual_arr = abs.(vcat(teCut_df[!,:dual],excCut_df[!,:dual]))
	maxDual_fl = maximum(allDual_arr)
	alphaCover_fl = length(findall(allDual_arr .>= maxDual_fl * ccbCover)) / (size(teCut_df,1) + size(excCut_df,1))
	produceMessage(top_mod.options,top_mod.report, 1," - Alpha covered of cut: $(round(alphaCover_fl*100,digits = 2))%")
	
	cbbEta_fl = mean(allDual_arr[findall(allDual_arr .>= maxDual_fl * ccbCover)])
	produceMessage(top_mod.options,top_mod.report, 1," - Eta of cut: $(round(cbbEta_fl,;sigdigits=3))")

	# determine a random capacity not alpha covered currently,# TODO bisher nur technologies
	#noCover = filter(x -> abs.(x.dual) < maxDual_fl * ccbCover,eachrow(teCut_df))[1,:]

	return cbbEta_fl, alphaCover_fl

end

#endregion

#region # * function to run top and sub problem of benders

# ! run top-Level problem
function runTopLevel!(top_mod::anyModel,bendersCut_arr::Array{Dict{Tuple{Int64,Int64},bendersData},1},i::Int)

	bounds_tup = (0.0,0.0)
	capaData_obj = bendersData()
	subObj_fl = Inf

	# add all benders cuts provided
	for bendersCut in bendersCut_arr
		# obtain sum of objective values for sub-problems
		subObj_fl = min(sum(map(x -> x.objValue, values(bendersCut))),subObj_fl)

		# create array of expressions with duals for sub-problems
		for sub in keys(bendersCut)
			subCut = bendersCut[sub]
			cutExpr_arr = Array{GenericAffExpr,1}()
			
			for teSym in keys(top_mod.parts.tech), capaSym in keys(subCut.capaTech[teSym])
				push!(cutExpr_arr,getBendersExpr(subCut.capaTech[teSym][capaSym],top_mod.parts.tech[teSym].var[capaSym]))
			end
			push!(cutExpr_arr,getBendersExpr(subCut.capaExc,top_mod.parts.exc.var[:capaExc]))

			# add benders cut to top problem
			newCut_cns = @constraint(top_mod.optModel, subCut.objValue + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)) <= filter(x -> x.Ts_disSup == sub[1] && x.scr == sub[2], top_mod.parts.obj.var[:cut])[1,:var])
			top_mod.parts.obj.cns[:bendersCuts] |> (x -> append!(x,DataFrame(id = i, Ts_disSup = sub[1],scr = sub[2], cns = newCut_cns, cut = "std", binds = [Int[]])))
		end

	end

	# set optimizer attributes and solve
	@suppress begin
		set_optimizer(top_mod.optModel,Gurobi.Optimizer)
		set_optimizer_attribute(top_mod.optModel, "Method", 2)
		set_optimizer_attribute(top_mod.optModel, "Crossover", 1)
		set_optimizer_attribute(top_mod.optModel, "BarOrder", 0)
		set_optimizer_attribute(top_mod.optModel, "OutputFlag", 1)
		optimize!(top_mod.optModel)
	end

	# write technology capacites to BendersData object
	for teSym in keys(top_mod.parts.tech)
		capaData_obj.capaTech[teSym] = Dict(capaSym => getCapa(copy(top_mod.parts.tech[teSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(top_mod.parts.tech[teSym].var)))
	end

	# write exchange capacities to BendersData object
	if :capaExc in keys(top_mod.parts.exc.var)
		capaData_obj.capaExc = getCapa(copy(filter(x -> x.R_from < x.R_to, top_mod.parts.exc.var[:capaExc])))
	end

	topObj_fl = value(sum(filter(x -> x.group == :costs, top_mod.parts.obj.var[:objVar])[!,:var]))
	# derive lower and upper bounds on objective value
	if size(top_mod.parts.obj.cns[:bendersCuts],1) > 0
		bounds_tup = (topObj_fl+value(filter(x -> x.name == :allCut,top_mod.parts.obj.var[:objVar])[1,:var]),bounds_tup[2])
	else
		bounds_tup = (0.0,bounds_tup[2])
	end

	return capaData_obj, bounds_tup, topObj_fl
end

# ! run sub-Level problem
function runSubLevel(sub_mod::anyModel,capaData_obj::bendersData; fixCapa::Bool = true, bounds::bendersData = bendersData())

	# ! fixes rhs of constraints fixing the capacity according to result of the top level problem
	if fixCapa
		# set rhs of constraints fixing the technology capacity
		for teSym in keys(capaData_obj.capaTech), capaSym in keys(capaData_obj.capaTech[teSym])
			# filter capacity data for respective year
			filter!(x -> x.Ts_disSup == sub_mod.subPro[1], capaData_obj.capaTech[teSym][capaSym])
			
			fixCapa!(capaData_obj.capaTech[teSym][capaSym],sub_mod.parts.tech[teSym].cns[Symbol(:ccb,makeUp(capaSym))])
		end

		# set rhs of constraints fixing the exchange capacity
		if :capaExc in keys(sub_mod.parts.exc.var)
			filter!(x -> x.Ts_disSup == sub_mod.subPro[1], capaData_obj.capaExc)
			fixCapa!(capaData_obj.capaExc,filter(x -> x.R_from < x.R_to, sub_mod.parts.exc.cns[:ccbExc]))
		end
	end

	# ! adjusts the objective function according to the limits on the dual of the constraint fixing the capacity
	baseObj_var = objective_function(sub_mod.optModel) |> (x -> typeof(x) <: VariableRef ? x : collect(x.terms)[1][1])
	if !isempty(bounds)
		lowExpr_arr = AffExpr[]
		upExpr_arr = AffExpr[]

		for teSym in keys(bounds.capaTech), capaSym in filter(x -> occursin("capa",string(x)), keys(bounds.capaTech[teSym]))
			if !isempty(bounds.capaTech[teSym][capaSym])
				low_expr,up_expr = getBoundObjective(sub_mod.parts.tech[teSym].var,bounds.capaTech[teSym][capaSym],capaSym)
				if any(lower_bound.(collect(keys(low_expr.terms))) .!= 0) && any(lower_bound.(collect(keys(up_expr.terms))) .!= 0) error() end
				push!(lowExpr_arr,low_expr)
				push!(upExpr_arr,up_expr)
			end
		end

		if :capaExc in keys(sub_mod.parts.exc.var)
			low_expr,up_expr = getBoundObjective(sub_mod.parts.exc.var,bounds.capaExc,:capaExc)
			if any(lower_bound.(collect(keys(low_expr.terms))) .!= 0) && any(lower_bound.(collect(keys(up_expr.terms))) .!= 0) error() end
			push!(lowExpr_arr,low_expr)
			push!(upExpr_arr,up_expr)
		end

		set_objective_function(sub_mod.optModel, baseObj_var + sum(upExpr_arr) - sum(lowExpr_arr))
	else	
		set_objective_function(sub_mod.optModel, baseObj_var)
	end
	# set optimizer attributes and solves
	@suppress begin
		set_optimizer(sub_mod.optModel,Gurobi.Optimizer)
		set_optimizer_attribute(sub_mod.optModel, "Method", 2)
		set_optimizer_attribute(sub_mod.optModel, "Crossover", 1)
		set_optimizer_attribute(sub_mod.optModel, "BarOrder", 0)
		set_optimizer_attribute(sub_mod.optModel, "OutputFlag", 1)
		optimize!(sub_mod.optModel)
	end

	# add duals and objective value to capacity data
	capaData_obj.objValue = objective_value(sub_mod.optModel)

	for teSym in keys(capaData_obj.capaTech), capaSym in keys(capaData_obj.capaTech[teSym])
		capaData_obj.capaTech[teSym][capaSym] = addDual(capaData_obj.capaTech[teSym][capaSym],sub_mod.parts.tech[teSym].cns[Symbol(:ccb,makeUp(capaSym))])
	end

	# add dual for exchange data
	capaData_obj.capaExc = 	addDual(capaData_obj.capaExc,sub_mod.parts.exc.cns[:ccbExc])

	return capaData_obj
end

#endregion

#region # * initialize objects

sub_tup = ((1,1),(1,2),(2,1),(2,2)) # indicating the year (first integer) and the scenario (second integer)

# create top level problem
top_mod = anyModel("examples/demo_stoch","results", objName = "top", reportLvl = 1)
top_mod.subPro = tuple(0,0)
createOptModel!(top_mod)
setObjective!(:costs,top_mod)

top_mod.parts.obj.cns[:bendersCuts][!,:binds] = Array{Array{Int64,1},1}()

# create seperate variables for costs of subproblems and aggregate them (cannot be part of model creation, because requires information about subproblems) 
top_mod.parts.obj.var[:cut] = map(y -> map(x -> sub_tup[x][y], 1:length(sub_tup)),1:2) |> (z -> createVar(DataFrame(Ts_disSup = z[1], scr = z[2]),"subCut",NaN,top_mod.optModel,top_mod.lock,top_mod.sets))
push!(top_mod.parts.obj.cns[:objEqn], (name = :aggCut, group = :benders, cns = @constraint(top_mod.optModel, sum(top_mod.parts.obj.var[:cut][!,:var]) == filter(x -> x.name == :allCut,top_mod.parts.obj.var[:objVar])[1,:var])))

# set lower limit on objective of the top problem to speed up first iterations (implementiere smarter und ggf. im algorithmus, damit echte gesamtobergrenze möglich ist, inkl. der subprobleme)
#@constraint(top_mod.optModel, sum(filter(x -> x.group == :costs, top_mod.parts.obj.var[:objVar])[!,:var]) >= 1.3e5)
# creates column to save information on when which cut is binding

# create sub level problems
sub_dic = Dict{Tuple{Int,Int},anyModel}()
sub_lock = ReentrantLock()

for (id,x) in enumerate(sub_tup)
    s = anyModel("examples/demo_stoch","results", objName = "sub_" * string(id), reportLvl = 1)
    s.subPro = x
    createOptModel!(s)
	setObjective!(:costs,s)
	
	# add new variables and constraints for ccb method to subproblems
	for teSym in keys(s.parts.tech), capaSym in filter(x -> occursin("capa",string(x)), keys(s.parts.tech[teSym].var))
		bound_df = createBoundElements(s,copy(s.parts.tech[teSym].var[capaSym]))
		s.parts.tech[teSym].var[Symbol(:ccbLow,makeUp(capaSym))] = rename(select(bound_df,Not([:var,:cns,:upDual])),:lowDual => :var)
		s.parts.tech[teSym].var[Symbol(:ccbUp,makeUp(capaSym))] = rename(select(bound_df,Not([:var,:cns,:lowDual])),:upDual => :var)
		s.parts.tech[teSym].cns[Symbol(:ccb,makeUp(capaSym))] = select(bound_df,Not([:var,:lowDual,:upDual]))
	end
	
	if :capaExc in keys(s.parts.exc.var)
		bound_df = createBoundElements(s,copy(filter(x -> x.R_from < x.R_to, s.parts.exc.var[:capaExc])))
		s.parts.exc.var[:ccbLowCapaExc] = rename(select(bound_df,Not([:var,:cns,:upDual])),:lowDual => :var)
		s.parts.exc.var[:ccbUpCapaExc] = rename(select(bound_df,Not([:var,:cns,:lowDual])),:upDual => :var)
		s.parts.exc.cns[:ccbExc] = select(bound_df,Not([:var,:lowDual,:upDual]))
	end

	
    sub_dic[x] = s
end

#endregion

# create bundle cut constraint for technology

let
	bendersCut_arr = Array{Dict{Tuple{Int64,Int64},bendersData},1}()
	fullReport_df = DataFrame(timestep_superordinate_dispatch = String[], region_dispatch = String[], technology = String[], carrier = String[], variable = Symbol[], iteration = Int[], value = Float64[])
	i = 1
	ccbCover_fl = 0.2 # percentage that controlls threshold for variable to be considered alpha covered 
	# ! run benders algorithm

	top_mod.options.startTime  = now()
	while true
		produceMessage(top_mod.options,top_mod.report, 1," - Started iteration $i", testErr = false)

		#region # * solve top level problem
		if i != 1
			startTop = now()
			capaData_obj, bounds_tup, topObj_fl = runTopLevel!(top_mod,bendersCut_arr,i)
			bendersCut_arr = Array{Dict{Tuple{Int64,Int64},bendersData},1}()
	
			timeTop = now() - startTop
			# reports on binding benders cuts
			top_mod.parts.obj.cns[:bendersCuts][!,:binds] = map(x -> dual(x.cns) != 0.0 ? vcat(x.binds,[i]) : x.binds , eachrow(top_mod.parts.obj.cns[:bendersCuts]))	

		else
			anyM = anyModel("examples/demo_stoch2","results", objName = "stoch")
			createOptModel!(anyM)
			setObjective!(:costs,anyM)

			using Gurobi
			set_optimizer(anyM.optModel,Gurobi.Optimizer)
			set_optimizer_attribute(anyM.optModel, "Method", 2)
			set_optimizer_attribute(anyM.optModel, "Crossover", 0)
			set_optimizer_attribute(anyM.optModel, "BarOrder", 1)
			optimize!(anyM.optModel)
			capaData_obj = bendersData()

			for teSym in keys(anyM.parts.tech)
				capaData_obj.capaTech[teSym] = Dict(capaSym => getCapa(copy(anyM.parts.tech[teSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(anyM.parts.tech[teSym].var)))
			end

			# write exchange capacities to BendersData object
			if :capaExc in keys(anyM.parts.exc.var)
				capaData_obj.capaExc = getCapa(copy(filter(x -> x.R_from < x.R_to, anyM.parts.exc.var[:capaExc])))
			end
			bendersCut_arr = Array{Dict{Tuple{Int64,Int64},bendersData},1}()
		end
		#endregion

		#region # * standard solve of sublevel problems
		dual_dic = Dict{Tuple{Int64,Int64},bendersData}()

		# fix capacity dual to zero for first solve
		for x in sub_tup
			s = sub_dic[x]
			for teSym in keys(s.parts.tech), capaSym in filter(x -> occursin("capa",string(x)), keys(s.parts.tech[teSym].var)), ccb in (:ccbLow,:ccbUp)
				foreach(x -> fix(x,0.0; force = true), s.parts.tech[teSym].var[Symbol(ccb,makeUp(capaSym))][!,:var])
			end

			foreach(x -> fix(x,0.0; force = true), s.parts.exc.var[:ccbLowCapaExc][!,:var])
			foreach(x -> fix(x,0.0; force = true), s.parts.exc.var[:ccbUpCapaExc][!,:var])
		end
		
		# perform first solve (could be multithreaded)
		startSub = now()
		@threads for x in collect(sub_tup)
			dual_etr = runSubLevel(sub_dic[x],copy(capaData_obj))
			lock(top_mod.lock)
			dual_dic[x] = dual_etr
			unlock(top_mod.lock)
		end
		timeSub = now() - startSub

		push!(bendersCut_arr,dual_dic)

		if i != 1
			bounds_tup = (bounds_tup[1],topObj_fl + sum(map(x -> x.objValue, values(bendersCut_arr[1]))))

			capa_df = filter(x -> x.variable in (:capaConv,:capaStIn,:capaStOut,:capaStSize),reportResults(:summary,top_mod, rtnOpt = (:csvDf,)))
			capa_df[!,:iteration] .= size(top_mod.parts.obj.cns[:bendersCuts],1)
			cost_df = DataFrame(variable = [:upperBound,:lowerBound],value = collect(bounds_tup) )
			cost_df[!,:timestep_superordinate_dispatch] .= "none"
			cost_df[!,:region_dispatch] .= "none"
			cost_df[!,:carrier] .= "none"
			cost_df[!,:technology] .= "none"
			cost_df[!,:iteration] .= size(top_mod.parts.obj.cns[:bendersCuts],1)
	
			fullReport_df = append!(fullReport_df,append!(select(capa_df,Not([:scr])),cost_df))

			if bounds_tup[2]/bounds_tup[1] - 1 < 0.01 break end

			produceMessage(top_mod.options,top_mod.report, 1," - Time for top: $(Dates.toms(timeTop) / Dates.toms(Second(1))) Time for sub: $(Dates.toms(timeSub) / Dates.toms(Second(1)))", testErr = false)
			produceMessage(top_mod.options,top_mod.report, 1," - Lower bound: $(round(bounds_tup[1], sigdigits = 5)) Upper Bound: $(round(bounds_tup[2],sigdigits = 5))", testErr = false)
		end
		i = i +1
	end

	CSV.write(joinpath("results/iteration_details.csv"), fullReport_df)
	CSV.write(joinpath("results/binding_cuts.csv"), top_mod.parts.obj.cns[:bendersCuts])
end



# TODO teste tatsächlich mal mit lösung des töglichen problems

