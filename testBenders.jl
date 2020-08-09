
# load package

using CSV, DataFrames, Dates, JuMP, Statistics, LinearAlgebra, Base.Threads
using PyCall, SparseArrays, Gurobi

# pyimport_conda("networkx","networkx")
# pyimport_conda("matplotlib.pyplot","matplotlib")
# pyimport_conda("plotly","plotly")

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

# struct to save benders related results
mutable struct bendersData
	objValue::Float64
	capaTech::Dict{Symbol,Dict{Symbol,DataFrame}}
	capaExc::DataFrame
	bendersData() = new(0.0,Dict{Symbol,Dict{Symbol,DataFrame}}(), DataFrame())
end

# copy functions for benders related results
function copy(ben_obj::bendersData)
	out = bendersData()
	out.objValue = ben_obj.objValue
	out.capaTech = deepcopy(ben_obj.capaTech)
	out.capaExc = deepcopy(ben_obj.capaExc)
	return out
end

# replaces the variable column with a column storing the value of the variable
function getCapa(capa_df::DataFrame)
	# filter entries without variable
	filter!(x -> !isempty(x.var.terms),capa_df)
	# write value of variable dataframe
	capa_df[!,:value] = map(x -> value(collect(keys(x.terms))[1]),capa_df[!,:var])
	return select(capa_df,Not([:var]))
end

# adds a dual into the first dataframe based on the matching variable column in the second
function addDual(dual_df::DataFrame,variable_df::DataFrame)
	new_df = leftjoin(dual_df,variable_df, on = intCol(dual_df,:dir))
	new_df[!,:dual] = map(x -> dual(FixRef(collect(keys(x.terms))[1])), new_df[!,:var])
	return select(new_df,Not([:var]))
end

# fixes capacity variable from second dataframe according to value in the first
function fixCapa!(value_df::DataFrame,variable_df::DataFrame)
	if !isempty(value_df)
		capaSub_df = leftjoin(variable_df,value_df,on = intCol(value_df,:dir))
		foreach(x -> x.var  |> (y -> fix(collect(keys(y.terms))[1], x.value; force = true)),eachrow(capaSub_df))
	end
end

# computes the capacity variable dependant expression of the benders cut from variables in the second datframe (using the dual and current value)
function getBendersExpr(sub_df::DataFrame, variable_df::DataFrame)
	ben_df = innerjoin(variable_df,sub_df, on = intCol(sub_df,:dir))
	return isempty(ben_df) ? AffExpr() : sum(map(x -> x.dual *( collect(keys(x.var.terms))[1] - x.value),eachrow(ben_df)))
end

# XXX run top-Level function
function runTopLevel!(top_mod::anyModel,bendersData_arr::Array{bendersData})

	bounds_tup = (0.0,0.0)
	capaData_obj = bendersData()

	# add benders cut
	if any(map(x -> !isempty(x.capaTech),bendersData_arr))
		# obtain sum of objective values for sub-problems
		sumObj_fl = sum(map(x -> x.objValue, bendersData_arr))

		# create array of expressions with duals for sub-problems
		cutExpr_arr = Array{GenericAffExpr,1}()
		for sub in bendersData_arr
			for teSym in keys(top_mod.parts.tech), capaSym in keys(bendersData_arr[1].capaTech[teSym]), sub in bendersData_arr
				push!(cutExpr_arr,getBendersExpr(sub.capaTech[teSym][capaSym],top_mod.parts.tech[teSym].var[capaSym]))
			end
			push!(cutExpr_arr,getBendersExpr(sub.capaExc,top_mod.parts.exc.var[:capaExc]))
		end

		# add benders cut to top problem
		alpha = filter(x -> x.name == :alpha,top_mod.parts.obj.var[:objVar])[1,:var]
		bendersCut = @constraint(top_mod.optModel, sumObj_fl + sum(cutExpr_arr[x] for x in 1:length(cutExpr_arr)) <= alpha)
		top_mod.parts.obj.cns[:bendersCuts] |> (x -> append!(x,DataFrame(id = size(x,1)+1, cns = bendersCut)))
	end

	# set optimizer attributes and solve
	set_optimizer(top_mod.optModel,Gurobi.Optimizer)
	set_optimizer_attribute(top_mod.optModel, "Method", 2)
	set_optimizer_attribute(top_mod.optModel, "Crossover", 1)
	set_optimizer_attribute(top_mod.optModel, "BarOrder", 0)
	set_optimizer_attribute(top_mod.optModel, "OutputFlag", 0)
	optimize!(top_mod.optModel)

	# write technology capacites to BendersData object
	for teSym in keys(top_mod.parts.tech)
		capaData_obj.capaTech[teSym] = Dict(capaSym => getCapa(copy(top_mod.parts.tech[teSym].var[capaSym])) for capaSym in filter(x -> occursin("capa",string(x)), keys(top_mod.parts.tech[teSym].var)))
	end

	# write exchange capacities to BendersData object
	if :capaExc in keys(top_mod.parts.exc.var)
		capaData_obj.capaExc = getCapa(copy(filter(x -> x.R_from < x.R_to, top_mod.parts.exc.var[:capaExc])))
	end

	# derive lower and upper bounds on objective value
	if size(top_mod.parts.obj.cns[:bendersCuts],1) > 0
		bounds_tup = (objective_value(top_mod.optModel)+value(alpha),objective_value(top_mod.optModel)+sumObj_fl)
	else
		bounds_tup = (0.0,0.0)
	end

	return capaData_obj, bounds_tup
end

function runSubLevel(sub_mod::anyModel,capaData_obj::bendersData)

	# fix capacities

	# fix capacities for technologies
	for teSym in keys(capaData_obj.capaTech), capaSym in keys(capaData_obj.capaTech[teSym])
		# filter capacity data for respective year
		filter!(x -> x.Ts_disSup == sub_mod.subPro[1], capaData_obj.capaTech[teSym][capaSym])
		# fix capacity variables to value
		fixCapa!(capaData_obj.capaTech[teSym][capaSym],sub_mod.parts.tech[teSym].var[capaSym])
	end

	# fix capacity for exchange
	if :capaExc in keys(sub_mod.parts.exc.var)
		filter!(x -> x.Ts_disSup == sub_mod.subPro[1], capaData_obj.capaExc)
		fixCapa!(capaData_obj.capaExc,filter(x -> x.R_from < x.R_to, sub_mod.parts.exc.var[:capaExc]))
	end

	# set optimizer attributes and solve
	set_optimizer(sub_mod.optModel,Gurobi.Optimizer)
	set_optimizer_attribute(sub_mod.optModel, "Method", 2)
	set_optimizer_attribute(sub_mod.optModel, "Crossover", 1)
	set_optimizer_attribute(sub_mod.optModel, "BarOrder", 0)
	set_optimizer_attribute(sub_mod.optModel, "OutputFlag", 0)
	optimize!(sub_mod.optModel)


	# add duals and objective value to capacity data
	capaData_obj.objValue = objective_value(sub_mod.optModel)

	for teSym in keys(capaData_obj.capaTech), capaSym in keys(capaData_obj.capaTech[teSym])
		capaData_obj.capaTech[teSym][capaSym] = addDual(capaData_obj.capaTech[teSym][capaSym],sub_mod.parts.tech[teSym].var[capaSym])
	end

	# add dual for exchange data
	capaData_obj.capaExc = 	addDual(capaData_obj.capaExc,sub_mod.parts.exc.var[:capaExc])

	return capaData_obj
end

using Gurobi

# XXX initialize objects

# create top level problem
top_mod = anyModel("examples/demo_stoch","results", objName = "top", reportLvl = 1)
top_mod.subPro = tuple(0,0)
createOptModel!(top_mod)
setObjective!(:costs,top_mod)

# create sub level problems
sub_dic = Dict{Tuple{Int,Int},anyModel}()
sub_tup = ((1,1),(1,2),(2,1),(2,2)) # indicating the year (first integer) and the scenario (second integer)
sub_lock = ReentrantLock()

for (id,x) in enumerate(sub_tup)
    s = anyModel("examples/demo_stoch","results", objName = "sub_" * string(id), reportLvl = 1)
    s.subPro = x
    createOptModel!(s)
    setObjective!(:costs,s)
    sub_dic[x] = s
end

bendersData_arr = fill(bendersData(), length(sub_tup))
fullReport_df = DataFrame(timestep_superordinate_dispatch = String[], region_dispatch = String[], technology = String[], carrier = String[], variable = Symbol[], iteration = Int[], value = Float64[])

# XXX run benders
while true
	# solve top level problem
	capaData_obj, bounds_tup = runTopLevel!(top_mod,bendersData_arr)

	# solve sub level problems (could be multithreaded)
	for (id,x) in enumerate(sub_tup)
		res_obj = runSubLevel(sub_dic[x],copy(capaData_obj))
		bendersData_arr[id] = res_obj
	end

	# reporting during loop
	capa_df = filter(x -> x.variable in (:capaConv,:capaStIn,:capaStOut,:capaStSize),reportResults(:summary,top_mod, rtnOpt = (:csvDf,)))
	capa_df[!,:iteration] .= size(top_mod.parts.obj.cns[:bendersCuts],1)
	cost_df = DataFrame(variable = [:upperBound,:lowerBound],value = collect(bounds_tup) )
	cost_df[!,:timestep_superordinate_dispatch] .= "none"
	cost_df[!,:region_dispatch] .= "none"
	cost_df[!,:carrier] .= "none"
	cost_df[!,:technology] .= "none"
	cost_df[!,:iteration] .= size(top_mod.parts.obj.cns[:bendersCuts],1)

	global fullReport_df = append!(fullReport_df,append!(select(capa_df,Not([:scr])),cost_df))
	println(bounds_tup)
	CSV.write(joinpath("results/test_benders.csv"), fullReport_df)
end
