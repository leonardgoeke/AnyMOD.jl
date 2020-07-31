
tSym = :photovoltaic

tInt = techInt(tSym,anyM.sets[:Te])
part = anyM.parts.tech[tSym]
prepTech_dic = prepVar_dic[tSym]
parDef_dic = copy(parDef_dic)

part.par[:avaConv]


# XXX iteration over all technologies to create variables and constraints
function createTech2!(tInt::Int,part::TechPart,prepTech_dic::Dict{Symbol,NamedTuple},parDef_dic::Dict{Symbol,NamedTuple},ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}},anyM::anyModel)

    cns_dic = Dict{Symbol,cnsCont}()
    newHerit_dic = Dict(:lowest => (:Ts_dis => :avg_any, :R_dis => :avg_any),:reference => (:Ts_dis => :up, :R_dis => :up)) # inheritance rules after presetting
    ratioVar_dic = Dict(:StIn => ("StIn" => "Conv"), :StOut => ("StOut" => "StIn"), :StSize => ("StSize" => "StIn")) # assignment of tech types for ratios stuff

    tech_str = createFullString(tInt,anyM.sets[:Te])
    # presets all dispatch parameter and obtains mode-dependant variables
    modeDep_dic = presetDispatchParameter!(part,prepTech_dic,parDef_dic,newHerit_dic,ts_dic,r_dic,anyM)

    # creates expansion and capacity variables
	if isempty(anyM.subPro) || anyM.subPro == (0,0) || anyM.options.decomm == :none
    	createExpCap!(part,prepTech_dic,anyM,ratioVar_dic)
	end

    # create expansion constraints
	if isempty(anyM.subPro) || anyM.subPro == (0,0)
		# connect capacity and expansion variables
		if part.type != :stock
			createCapaCns!(part,prepTech_dic,cns_dic)
		end
		# control operated capacity variables
		if anyM.options.decomm != :none
			createOprVarCns!(part,cns_dic,anyM)
		end
	end

    produceMessage(anyM.options,anyM.report, 3," - Created all variables and prepared all constraints related to expansion and capacity for technology $(tech_str)")

    # create dispatch variables
	if isempty(anyM.subPro) || anyM.subPro != (0,0)
	    createDispVar!(part,modeDep_dic,ts_dic,r_dic,anyM)
	    produceMessage(anyM.options,anyM.report, 3," - Created all dispatch variables for technology $(tech_str)")

	    # create conversion balance for conversion technologies
	    if keys(part.carrier) |> (x -> any(map(y -> y in x,(:use,:stIntOut))) && any(map(y -> y in x,(:gen,:stIntIn))))
	        cns_dic[:convBal] = createConvBal(part,anyM)
	        produceMessage(anyM.options,anyM.report, 3," - Prepared conversion balance for technology $(tech_str)")
	    end

	    # create storage balance for storage technologies
	    if :stLvl in keys(part.var)
	        cns_dic[:stBal] = createStBal(part,anyM)
	        produceMessage(anyM.options,anyM.report, 3," - Prepared storage balance for technology $(tech_str)")
	    end

	    # create capacity restrictions
	    createCapaRestr!(part,ts_dic,r_dic,cns_dic,anyM)
	    produceMessage(anyM.options,anyM.report, 3," - Prepared capacity restrictions for technology $(tech_str)")

	    # create ratio constraints
	    if any(map(x -> occursin("ratioEner",string(x)), collectKeys(keys(part.par))))
	        createRatioCns!(part,cns_dic,anyM)
	        produceMessage(anyM.options,anyM.report, 3," - Prepared constraints controlling energy ratios for technology $(tech_str)")
	    end
	end

    # all constraints are scaled and then written into their respective array position
    foreach(x -> scaleCnsExpr!(x[2].data,anyM.options.coefRng,anyM.options.checkRng), collect(cns_dic))

    produceMessage(anyM.options,anyM.report, 2," - Created all variables and prepared constraints for technology $(tech_str)")

    return cns_dic
end
