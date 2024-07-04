
# ! prints dataframe to csv file
"""
```julia
printObject(print_df::DataFrame, model_object::anyModel)
```

Writes a DataFrame of parameters, constraints, or variables to a `.csv` file in readable format (strings instead of ids). See [Individual elements](@ref).
"""
function printObject(print_df::DataFrame, anyM::anyModel; fileName::String = "", rtnDf::Tuple{Vararg{Symbol,N} where N} = (:csv,), filterFunc::Function = x -> true, wrtGap::Bool = false, rdcFrs = false)

	sets = anyM.sets
	options = anyM.options

	colNam_arr = namesSym(print_df)
    cntCol_int = size(colNam_arr, 1)

	# filters values according to filter function,
	print_df = copy(filter(filterFunc, print_df))

	# converts variable column to value of variable
	if :var in colNam_arr
		print_df[!,:var] = value.(print_df[!,:var])
	end

    for i = 1:cntCol_int
        lookUp_sym = Symbol(split(String(colNam_arr[i]), "_")[1])
		if !(lookUp_sym in keys(sets)) && lookUp_sym == :eqn
			print_df[!,i] = string.(print_df[!,i])
		elseif lookUp_sym == :dir
			print_df[!,i] = map(x -> x == 1 ? "yes" : "no", print_df[!,i])
        elseif lookUp_sym in keys(sets) && eltype(print_df[!,i]) <: Int
			print_df[!,i] = map(x -> createFullString(x, sets[lookUp_sym], wrtGap), print_df[!,i])
        end
    end

	# rename columns
	colName_dic = Dict(:Ts_dis => :timestep_dispatch, :Ts_exp => :timestep_expansion, :Ts_expSup => :timestep_superordinate_expansion, :Ts_disSup => :timestep_superordinate_dispatch, :Ts_frs => :timestep_foresight,
															:R => :region, :R_tech => :region, :R_dis => :region_dispatch, :R_exp => :region_expansion, :R_to => :region_to, :R_from => :region_from, :C => :carrier, :Sys => :system, :Te => :technology, :Exc => :exchange,
																:M => :mode, :dir => :directed, :scr => :scenario, :cns => :constraint, :var => :variable)

	rename!(print_df, map(x -> x in keys(colName_dic) ? colName_dic[x] : x, namesSym(print_df)) )
	
	# reduce string length of foresight periods to avoid redundance
	if :timestep_foresight in namesSym(print_df) && rdcFrs
		print_df[!,:timestep_foresight] = map(x -> split(x, "<")[end], print_df[!,:timestep_foresight])
	end
	
	if :csv in rtnDf
    	CSV.write("$(options.outDir)/$(fileName)_$(options.outStamp).csv", print_df)
	end

	if :csvDf in rtnDf return print_df end
end

# ! converts input dataframe to a parameter input file
function writeParameterFile!(in_m::anyModel, para_df::DataFrame, par_sym::Symbol, parDef_tup::NamedTuple, file::String)

	# define dictionary matching long and short set names
	set_dic = Dict(:Ts => :timestep, :R => :region, :C => :carrier, :Te => :technology, :Exc => :exchange, :scr => :scenario, :id => :id)
	# initialize matrix with parameter name and values
	wrtPara_arr = Array{Any, 2}(undef, size(para_df, 1)+1, 2)
	wrtPara_arr[1,1:2] = ["parameter", "value"]
	wrtPara_arr[2:end,1] .= string(par_sym)
	wrtPara_arr[2:end,2] = para_df[!,:value]
	
	# add specification to parameter name in case of directed exchange data
	if :dir in namesSym(para_df) wrtPara_arr[findall(para_df[!,:dir]) .+ 1, 1] .= string(par_sym, :Dir) end

	for setCol in intersect(string.(vcat(collect(parDef_tup.dim))), names(para_df))
		set_sym = Symbol(split(setCol, "_")[1]) # gets set symbol
		# creates array of arrays with strings on each level, missing levels are filled with empty strings
		str_arr = map(x -> x == 0 ? [""] : vcat(getUniName(x, in_m.sets[set_sym], true)...), para_df[!,setCol])
		colNum_int = maximum(size.(str_arr, 1))
		strExt_arr = map(x -> vcat(x, fill("", colNum_int - size(x, 1))), str_arr)
		# adds new columns for set to array
		wrtPara_arr = hcat(wrtPara_arr, permutedims(hcat(vcat([map(x -> string(set_dic[set_sym], "_", x), 1:colNum_int)], strExt_arr)...)))
	end
	# correct order and remove columns filled with empty strings
	wrtPara_arr = hcat(wrtPara_arr[:,3:size(wrtPara_arr, 2)], wrtPara_arr[:,1:2])
	# write to csv input file
	writedlm(file * ".csv", wrtPara_arr, ',')	
end

# export design factors of model to specified folder
function exportDesignFactors!(in_m::anyModel, wrt_dir::String, wrtTs_boo::Bool)

    # create directory for design factors
    rm(wrt_dir; force = true, recursive = true)
    mkdir(wrt_dir)

    # stores parameter info for fixing
    parInfo_tup = defineParameter(in_m.options, in_m.report)[:desFac]

    for tSym in keys(in_m.parts.tech)
        if haskey(in_m.parts.tech[tSym].par, :desFac)
			data_df = copy(in_m.parts.tech[tSym].par[:desFac].data)
			if !wrtTs_boo select!(data_df, Not(intersect(Symbol.(intCol(data_df)), [:Ts_disSup, :Ts_expSup]))) end
            writeParameterFile!(in_m, rename(data_df, :val => :value), :desFac, parInfo_tup, wrt_dir * "/par_" * string(tSym))
        end
    end

end

#region # * report results to csv files
"""
```julia
reportResults(reportType::Symbol, model_object::anyModel; rtnOpt::Tuple = (:csv,))
```

Writes results to `.csv` file with content depending on `reportType`. Available types are `:summary`, `:exchange`, and `:costs`. See [Analysed results](@ref).
"""
reportResults(reportType::Symbol, anyM::anyModel; kwargs...) = reportResults(Val{reportType}(), anyM::anyModel; kwargs...)

# ! summary of all capacity and dispatch results
function reportResults(objGrp::Val{:summary}, anyM::anyModel; addObjName::Bool=true, wrtSgn::Bool = true, rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,), rmvZero::Bool = true, addRep::Tuple{Vararg{Symbol,N} where N} = tuple())

	if !isempty(setdiff(addRep, (:cyc, :flh, :effConv, :capaConvOut)))
		error("Provided unsupported keywords for addRep argument. Supported are :cyc, :flh, :effConv, and :capaConvOut.")	
	end

    techSym_arr = collect(keys(anyM.parts.tech))
	allData_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], scr = Int[], Ts_frs = Int[], id = Int[], variable = Symbol[], value = Float64[])
	if anyM.scr.frsLvl == anyM.supTs.lvl || !(length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0))) select!(allData_df, Not([:Ts_frs])) end

	# ! get demand values
	if :dem in keys(anyM.parts.bal.par) && (anyM.subPro != tuple(0,0) || anyM.options.createVI.bal)
		
		dem_df = copy(anyM.parts.bal.par[:dem].data)
		if !isempty(dem_df)

			dem_df[!,:lvlR] = map(x -> anyM.cInfo[x].rDis, :C in namesSym(dem_df) ? dem_df[!,:C] : filter(x -> x != 0, getfield.(values(anyM.sets[:C].nodes), :idx)))

			# aggregates demand values

			# add superordinate timestep (and dispatch timestep if non-existing)
			if :Ts_dis in namesSym(dem_df)
				ts_dic = Dict(x => anyM.sets[:Ts].nodes[x].lvl == anyM.supTs.lvl ? x : getAncestors(x, anyM.sets[:Ts], :int, anyM.supTs.lvl)[end] for x in unique(dem_df[!,:Ts_dis]))
				dem_df[!,:Ts_disSup] = map(x -> ts_dic[x], dem_df[!,:Ts_dis])
				# replace zeros with actual resolution
				if 0 in dem_df[!,:Ts_dis] 
					zeroDem_df = filter(x -> x.Ts_dis == 0, dem_df)
					zeroDem_df[!,:Ts_dis] = map(x -> getfield.(getNodesLvl(anyM.sets[:Ts], anyM.cInfo[x.C].tsDis),:idx), eachrow(zeroDem_df))
					dem_df = vcat(flatten(zeroDem_df, :Ts_dis), filter(x -> x.Ts_dis != 0, dem_df))
				end
			else
				dem_df[!,:Ts_disSup] .= collect(anyM.supTs.step) |> (z -> map(x -> z, 1:size(dem_df, 1)))
				dem_df = flatten(dem_df, :Ts_disSup)
				dem_df[!,:Ts_dis] = dem_df[!,:Ts_disSup]
			end

			# add foresight period if applies
			if anyM.scr.frsLvl != anyM.supTs.lvl && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
				dem_df[!,:Ts_frs] = getTsFrs(dem_df[!,:Ts_dis], anyM.sets[:Ts], anyM.scr.frsLvl)
				dem_df = flatten(dem_df,:Ts_frs)
			end

			if !(:scr in namesSym(dem_df)) # artificially add scenario dimensions, if none exist and perfect foresight
				if length(anyM.scr.scrProb) > 1
					dem_df[!,:scr] = map(x -> anyM.scr.frsLvl == anyM.supTs.lvl ? [0] : anyM.scr.scr[getindex(x, anyM.scr.frsLvl != anyM.supTs.lvl ? :Ts_frs : :Ts_disSup)], eachrow(dem_df))
					dem_df = flatten(dem_df, :scr)
				else
					dem_df[!,:scr] .= 0
				end
			elseif !isempty(anyM.scr.scr) # filter non-relevant scenarios
				relScr_arr = collect(keys(anyM.scr.scrProb))
				filter!(x -> x.scr == 0 || (getindex(x, anyM.supTs.lvl < anyM.scr.lvl ? :Ts_frs : :Ts_disSup), x.scr) in relScr_arr, dem_df)
			end

			dem_df[!,:val] = dem_df[!,:val]	.*  getEnergyFac(dem_df[!,:Ts_dis], anyM.supTs)

			allR_arr = :R_dis in namesSym(dem_df) ? unique(dem_df[!,:R_dis]) : getfield.(getNodesLvl(anyM.sets[:R], 1), :idx)
			allLvlR_arr = unique(dem_df[!,:lvlR])
			r_dic = Dict((x[1], x[2]) => (anyM.sets[:R].nodes[x[1]].lvl < x[2] ? getDescendants(x[1], anyM.sets[:R], false, x[2]) : getAncestors(x[1], anyM.sets[:R], :int, x[2])[end]) for x in Iterators.product(allR_arr, allLvlR_arr))
			if :R_dis in namesSym(dem_df)
				dem_df[!,:R_dis] = map(x -> r_dic[x.R_dis, x.lvlR], eachrow(dem_df[!,[:R_dis, :lvlR]]))
			else
				dem_df[!,:R_dis] .= 0
			end

			# aggregate and add expected value in case of scenarios
			if length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0))
				dem_df = addExpVal(dem_df, anyM.scr.scrProb, anyM.sets[:Ts], anyM.scr.lvl, :val) 
			end
			dem_df = combine(groupby(dem_df, intersect(namesSym(dem_df), [:Ts_disSup, :Ts_frs, :R_dis, :C, :scr])), :val => ( x -> sum(x) / 1000) => :value)
			
			# add columns and add to overall data
			dem_df[!,:Te] .= 0
			dem_df[!,:id] .= 0
			dem_df[!,:variable] .= :demand
			if wrtSgn dem_df[!,:value] = dem_df[!,:value] .* -1 end
			append!(allData_df, flatten(dem_df, :R_dis))
		end
	end

	# ! get missing capacity variable
	if :missCapa in keys(anyM.parts.bal.var)
		missCapa_df = copy(anyM.parts.bal.var[:missCapa])
		if !isempty(missCapa_df)
			missCapa_df[!,:value] .= value.(missCapa_df[!,:var])
			missCapa_df[!,:variable] .= :missCapa
			foreach(x -> missCapa_df[!,x] .= 0, intersect(namesSym(allData_df), [:Ts_frs, :Te, :scr, :id]))
			append!(allData_df, select(missCapa_df, Not([:var])))
		end
	end

	# ! get expansion and capacity variables
	for t in techSym_arr
		part = anyM.parts.tech[t]
		tech_df = DataFrame(Ts_disSup = Int[], R_dis = Int[], Te = Int[], C = Int[], id = Int[], scr = Int[], variable = Symbol[], value = Float64[])

		# get installed capacity values
		for va in intersect(keys(part.var), (:expConv, :expStIn, :expStOut, :expStSize, :expExc, :capaConv, :capaStIn, :capaStOut,  :capaStSize, :insCapaConv, :insCapaStIn, :insCapaStOut, :insCapaStSize, :mustCapaConv, :mustCapaStOut))
			capa_df = copy(part.var[va])
			if va in (:expConv, :expStIn, :expStOut, :expStSize)
				capa_df = flatten(capa_df, :Ts_expSup)
				select!(capa_df, Not(:Ts_disSup))
				rename!(capa_df, :Ts_expSup => :Ts_disSup)
			end
			# set carrier column to zero for conversion capacities and add a spatial dispatch column
			if va in (:expConv, :capaConv, :insCapaConv, :mustCapaConv)
				capa_df[!,:id] .= 0
			end

			capa_df[!,:R_dis] = map(x -> getAncestors(x, anyM.sets[:R], :int, part.balLvl.exp[2])[end], capa_df[!,:R_exp])
			select!(capa_df, Not(:R_exp))
			# aggregate values and add to tech data frame
			capa_df = combine(groupby(capa_df, [:Ts_disSup, :R_dis, :id, :Te]), :var => ( x -> value.(sum(x))) => :value)
			capa_df[!,:variable] .= va
			capa_df[!,:scr] .= 0
			capa_df[!,:C] .= 0
			append!(tech_df, capa_df)
		end

		# add tech dataframe to overall data frame
		if :Ts_frs in namesSym(allData_df) tech_df[!,:Ts_frs] .= 0 end
		append!(allData_df, filter((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), tech_df))
	end

	# ! get fill levels of stochastic inter-annual storage
	startStLvl_df = getAllVariables(:startStLvl, anyM)
	if !isempty(startStLvl_df)
		startStLvl_df[!,:value] = value.(startStLvl_df[!,:var])
		startStLvl_df[!,:variable] .= :startStLvl
		startStLvl_df[!,:scr] .= 0
		# add tech to overall data frame
		if :Ts_frs in namesSym(allData_df) startStLvl_df[!,:Ts_frs] .= 0 end
		append!(allData_df, filter((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), select(startStLvl_df, Not([:Ts_expSup, :M, :var]))))
	end

	# ! get dispatch variables
	for va in (:use, :gen, :stIn, :stOut, :stExtIn, :stExtOut, :stIntIn, :stIntOut, :emission, :crt, :lss, :trdBuy, :trdSell, :emissionInf)

		# get all variables, group them and get respective values
		allVar_df = getAllVariables(va, anyM)
		if isempty(allVar_df) continue end

		# adds column for superordinate dispatch time-step if non existing
		if !(:Ts_disSup in intCol(allVar_df)) && :Ts_dis in intCol(allVar_df)
			allVar_df[!,:Ts_disSup] = map(x -> getAncestors(x, anyM.sets[:Ts], :int, anyM.supTs.lvl)[end], allVar_df[!,:Ts_dis])
		end

		# add foresight period if applies
		if anyM.scr.frsLvl != anyM.supTs.lvl && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
			checkScrFrs_boo = false
			# add dispatch timestep if non-existing (can only occur for emissionInf)
			if !(:Ts_dis in namesSym(allVar_df))
				allVar_df[!,:Ts_dis] .= 0;
				allVar_df[!,:Ts_frs] .= 0;
				checkScrFrs_boo = true
			else
				allVar_df[!,:Ts_frs] = getTsFrs(allVar_df[!,:Ts_dis], anyM.sets[:Ts], anyM.scr.frsLvl)
				allVar_df = flatten(allVar_df, :Ts_frs)
			end
		end

		# add expected value in case of scenarios and aggregate
		allVar_df[!,:value] .= value.(allVar_df[!,:var])
		if :Ts_dis in namesSym(allVar_df) && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
			allVar_df = vcat(addExpVal(select(filter(x -> x.scr != 0, allVar_df),Not([:var])), anyM.scr.scrProb, anyM.sets[:Ts], anyM.scr.lvl, :value), select(filter(x -> x.scr == 0, allVar_df)))
		end
		disp_df = combine(groupby(allVar_df, intersect(intCol(allVar_df), intersect(namesSym(allVar_df),[:Ts_disSup, :Ts_frs, :R_dis, :C, :Te, :scr]))), :value => (x -> sum(x)) => :value)
			
		if !(:Ts_disSup in names(disp_df)) && va == :emissionInf # add superordinate dispatch time-step if non-existing
			disp_df[!,:Ts_disSup] .= 0
		end
		
		# scales values to twh (except for emissions)
		if !(va in (:emission, :emissionInf)) disp_df[!,:value] = disp_df[!,:value]  ./ 1000 end
		disp_df[!,:variable] .= va

		# add empty values for non-existing columns
		for dim in (:Te, :C, :id, :scr)
			if !(dim in namesSym(disp_df))
				disp_df[:,dim] .= 0
			end
		end

		# adjust sign, if enabled
		if wrtSgn && va in (:use, :stIn, :stIntIn, :stExtIn, :crt, :trdSell) disp_df[!,:value] = disp_df[!,:value] .* -1 end

		# adds region column potentially missing for paramter triggered data
		if !(:R_dis in namesSym(disp_df)) disp_df[!,:R_dis] .= 0.0 end

		append!(allData_df, filter((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), disp_df))
	end

	# ! get exchange variables aggregated by import and export
	allExc_df = getAllVariables(:exc, anyM)
	
	if !isempty(allExc_df)

		allExc_arr = unique(allExc_df[!,:Exc])
	
		# add foresight period if applies
		if anyM.scr.frsLvl != anyM.supTs.lvl && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
			allExc_df[!,:Ts_frs] = getTsFrs(allExc_df[!,:Ts_dis], anyM.sets[:Ts], anyM.scr.frsLvl)
			allExc_df = flatten(allExc_df,:Ts_frs)
		end
		
		# get values and add expected value in case of scenarios
		if length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0))
			allExc_df = addExpVal(allExc_df, anyM.scr.scrProb, anyM.sets[:Ts], anyM.scr.lvl, :var)
		end
		
		# compute export and import of each region, losses are considered at import
		excFrom_df = rename(combine(groupby(allExc_df, intersect(namesSym(allExc_df), [:Ts_disSup, :Ts_frs, :R_from, :C, :scr])), :var => (x -> value(sum(x))/1000) => :value), :R_from => :R_dis)
		excFrom_df[!,:variable] .= :export; excFrom_df[!,:Te] .= 0
		if wrtSgn excFrom_df[!,:value] = excFrom_df[!,:value] .* -1 end
	
		# add losses to all exchange variables
		lossExc_df = vcat(map(x -> addLossesExc(filter(y -> y.Exc == x, allExc_df), anyM.parts.exc[sysSym(x, anyM.sets[:Exc])], anyM.sets), allExc_arr)...)
		excTo_df = rename(combine(groupby(lossExc_df, intersect(namesSym(allExc_df), [:Ts_disSup, :Ts_frs, :R_to, :C, :scr])), :var => (x -> value(sum(x))/1000) => :value), :R_to => :R_dis)
		excTo_df[!,:variable] .= :import; excTo_df[!,:Te] .= 0
	
		excFrom_df[!,:id] .= 0
		excTo_df[!,:id] .= 0
	
		append!(allData_df, filter((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), vcat(excFrom_df, excTo_df)))
	end
	
	# ! comptue full load hours
	if :flh in addRep
		flh_dic = Dict(:capaConv => :flhConv, :capaStIn => :flhStIn, :capaStOut => :flhStOut)

		for flhCapa in collect(keys(flh_dic))

			# get capacities relevant for full load hours
			capaFlh_df = filter(x -> x.variable == flhCapa, allData_df)
			
			# expand to scenarios if sensible
			if anyM.scr.frsLvl <= anyM.supTs.lvl && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
				capaFlh_df[!,:scr] = map(x -> vcat([0], anyM.scr.scr[x]), capaFlh_df[!,:Ts_disSup])
				capaFlh_df = flatten(capaFlh_df, :scr)
			end
			
			# get dispatch quantities relevant for full load hours
			if flhCapa == :capaConv
				var_arr = [:use, :gen, :stIntIn, :stIntOut]
			elseif flhCapa  == :capaStIn
				var_arr = [:stIntIn, :stExtIn]
			elseif flhCapa  == :capaStOut
				var_arr = [:stIntOut, :stExtOut]
			end
			
			relDisp_df = filter(x -> x.variable in var_arr, allData_df)
			if isempty(relDisp_df) continue end
			if flhCapa == :capaConv
				rename_dic = Dict(:use => :in, :gen => :out, :stIntIn => :out, :stIntOut => :in)
				relDisp_df[!,:variable] = map(x -> rename_dic[x], relDisp_df[!,:variable])
			else
				relDisp_df[!,:variable] .= :st
			end

			# group dispatch quantities and match with capacity data
			aggDisp_df = rename(combine(groupby(relDisp_df, [:Ts_disSup, :R_dis, :Te, :scr, :variable]), :value => (x -> sum(abs.(x))) => :value), :variable => :variable2, :value => :value2)
			
			if flhCapa == :capaConv
				# match with input quantities where they are defined, otherwise check of output quantities
				convIn_df = innerjoin(capaFlh_df, filter(x -> x.variable2 == :in, aggDisp_df), on = [:Ts_disSup, :R_dis, :Te, :scr])
				contOut_df = innerjoin(antijoin(capaFlh_df, convIn_df, on = [:Ts_disSup, :R_dis, :Te, :scr]), filter(x -> x.variable2 == :out, aggDisp_df), on = [:Ts_disSup, :R_dis, :Te, :scr])
				capaFlh_df = vcat(convIn_df, contOut_df)
			else
				capaFlh_df = innerjoin(capaFlh_df, aggDisp_df, on = [:Ts_disSup, :R_dis, :Te, :scr])	
			end

			capaFlh_df[!,:value] = capaFlh_df[!,:value2] ./ capaFlh_df[!,:value] .* 1000
			capaFlh_df[!,:variable] .= flh_dic[flhCapa]

			append!(allData_df, select(capaFlh_df, Not([:variable2, :value2])))
		end
	end

	# ! comptue storage cycles
	if :cyc in addRep
		cyc_dic = Dict(:capaStIn => :cycStIn, :capaStOut => :cycStOut)

		for cycCapa in collect(keys(cyc_dic))
			capaCyc_df = filter(x -> x.variable == :capaStSize, allData_df)
			
			# expand to scenarios if sensible
			if anyM.scr.frsLvl == anyM.supTs.lvl 
				capaCyc_df[!,:scr] = map(x -> anyM.scr.scr[x], capaCyc_df[!,:Ts_disSup])
				capaCyc_df = flatten(capaCyc_df, :scr)
			end
			
			# get dispatch quantities relevant for cycling
			if cycCapa  == :capaStIn
				var_arr = [:stIntIn, :stExtIn]
			elseif cycCapa  == :capaStOut
				var_arr = [:stIntOut, :stExtOut]
			end
			
			relDisp_df = filter(x -> x.variable in var_arr, allData_df)
			if isempty(relDisp_df) continue end
			# group dispatch quantities and match with capacity data
			aggDisp_df = rename(combine(groupby(relDisp_df, [:Ts_disSup, :R_dis, :Te, :scr]), :value => (x -> sum(abs.(x))) => :value), :value => :value2)

			capaCyc_df = innerjoin(capaCyc_df, aggDisp_df, on = [:Ts_disSup, :R_dis, :Te, :scr])	
			
			# get relevant dispatch variables for respective group
			capaCyc_df[!,:value] = capaCyc_df[!,:value2] ./ capaCyc_df[!,:value] .* 1000
			capaCyc_df[!,:variable] .= cyc_dic[cycCapa]

			append!(allData_df, select(capaCyc_df, Not([:value2])))
		end
	end

	# ! comptue conversion efficiencies

	if :effConv in addRep || :capaConvOut in addRep

		# get sum of input and output plus carrier-specific output
		in_df =  filter(x -> x.variable in (:use, :stIntIn), allData_df) |> (w -> combine(groupby(w, filter(u -> !(u in (:C, :scr)), intCol(w))), :value => (x -> abs(sum(x))) => :in))
		out_df = filter(x -> x.variable in (:gen, :stOutIn), allData_df) |> (w -> combine(groupby(w, filter(u -> u != :scr, intCol(w))), :value => (x -> abs(sum(x))) => :out))
		
		# get capacity and add carriers variables
		outCapa_df = select(rename(filter(x -> x.variable == :capaConv, allData_df), :value => :capa), Not([:variable, :C, :scr]))
		outCapa_df[!, :C] = map(x -> collect(anyM.parts.tech[sysSym(x, anyM.sets[:Te])].carrier.gen), outCapa_df[!, :Te]) # extend in with carrier column
		outCapa_df = flatten(outCapa_df, :C)
		
		# aggregate capacity and out variables
		outCapa_df[!,:out] = aggDivVar(rename(out_df, :out => :val), outCapa_df, tuple(intCol(outCapa_df)...), anyM.sets) # aggregate out to capa
		noOut_df = deepcopy(outCapa_df) # filter cases without out
		out_df[!,:capa] = aggDivVar(rename(select(noOut_df, Not([:out])), :capa => :val), out_df, tuple(intCol(in_df)...), anyM.sets) # aggregate capa to out
		outCapa_df = unique(vcat(filter(x -> x.out != 0.0, outCapa_df), filter(x -> x.capa != 0.0, out_df))) # merge both cases

		# aggregate capacity and in variables
		outCapa_df[!,:in] = aggDivVar(rename(in_df, :in => :val), outCapa_df, tuple(intCol(in_df)...), anyM.sets) # aggregate in to capa
		noIn_df = deepcopy(outCapa_df) # filter cases without in 
		in_df[!,:C] = map(x -> collect(anyM.parts.tech[sysSym(x, anyM.sets[:Te])].carrier.gen), in_df[!,:Te]) # extend in with carrier column
		in_df = flatten(in_df, :C)
		in_df[!,:capa] = aggDivVar(rename(select(noIn_df, Not([:in])), :capa => :val), in_df, tuple(intCol(in_df)...), anyM.sets) # aggregate capa to in
		in_df[!,:out] = aggDivVar(rename(select(noIn_df, Not([:in])), :out => :val), in_df, tuple(intCol(in_df)...), anyM.sets) # aggregate out to in
		inAgg_df = filter(x -> x.capa != 0.0, in_df)
		outCapa_df = vcat(filter(x -> !(x.Te in unique(inAgg_df[!,:Te])), outCapa_df), inAgg_df) # merge both cases
		filter!(x -> x.out/x.in < 1e3,  outCapa_df)

		# compute efficiencies
		outCapa_df[!,:eff] = map(x -> x.in == 0.0 ? 1.0 : x.out/x.in, eachrow(outCapa_df))
		select!(outCapa_df, Not([:out,:in]))
		outCapa_df[!,:scr] .= 0.0

		# add output capacities to output
		if :capaConvOut in addRep
			outCapa_df[!,:value] = outCapa_df[!,:capa] .* outCapa_df[!,:eff]
			outCapa_df[!,:variable] .= :capaConvOut
			append!(allData_df, select(outCapa_df, Not([:capa, :eff])))
		end

		# add efficiencies to output
		if :effConv in addRep
			outCapa_df[!,:variable] .= :effConv
			append!(allData_df, rename(select(outCapa_df, Not([:value, :capa])), :eff => :value))
		end
	end

	# removes scenario column if only one scenario is defined
	if length(unique(allData_df[!,:scr])) == 1
		select!(allData_df, Not(:scr))
	end

	# add column with name for model object/scenario
	if addObjName allData_df[!,:objName] .= anyM.options.objName end

	# return dataframes and write csv files based on specified inputs
	if :csv in rtnOpt || :csvDf in rtnOpt
		csvData_df = printObject(allData_df, anyM, fileName = "results_summary", rtnDf = rtnOpt, rdcFrs = true)
	end

	if :raw in rtnOpt
		CSV.write("$(anyM.options.outDir)/results_summary_$(anyM.options.outStamp).csv", allData_df)
	end

	if :rawDf in rtnOpt && :csvDf in rtnOpt
		return allData_df, csvData_df
	else
		if :rawDf in rtnOpt return allData_df end
		if :csvDf in rtnOpt return csvData_df end
	end
end

# ! results for costs
function reportResults(objGrp::Val{:cost}, anyM::anyModel; addObjName::Bool=true, rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,), rmvZero::Bool = true, addRep::Tuple{Vararg{Symbol,N} where N} = ())
	# prepare empty dataframe
	allData_df = DataFrame(Ts_disSup = Int[], R_tech = Int[], R_from = Int[], R_to = Int[], Te = Int[], Exc = Int[], C = Int[], scr = Int[], variable = Symbol[], value = Float64[])

	# loops over all objective variables with keyword "cost" in it
	for cst in keys(anyM.parts.cost.var)

		cost_df = copy(anyM.parts.cost.var[cst])
		# rename all dispatch and expansion regions simply to region
		if !isempty(intersect([:R_dis, :R_exp], namesSym(cost_df)))
			rename!(cost_df, (:R_dis in namesSym(cost_df) ? :R_dis : :R_exp) => :R_tech)
		end
		# add empty column for non-existing dimensions
		for dim in (:Te, :Exc, :C, :R_tech, :R_from, :R_to, :scr)
			if !(dim in namesSym(cost_df))
				cost_df[:,dim] .= 0
			end
		end
		# obtain values and write to dataframe
		cost_df[:,:variable] .= cst
		cost_df[:,:value] = value.(cost_df[:,:var])
        if :Ts_exp in namesSym(cost_df) cost_df = rename(cost_df, :Ts_exp => :Ts_disSup) end

		if !(:Ts_disSup in names(cost_df)) && cst == :costEmInf # add superordinate dispatch time-step if non-existing
			cost_df[!,:Ts_disSup] .= 0
		end

		append!(allData_df, select(cost_df, Not([:var])))
	end

	# removes scenario column if only one scenario is defined
	if length(unique(allData_df[!,:scr])) == 1
		select!(allData_df, Not(:scr))
	end

	# add column with name for model object/scenario
	if addObjName allData_df[!,:objName] .= anyM.options.objName end

	# return dataframes and write csv files based on specified inputs
	if :csv in rtnOpt || :csvDf in rtnOpt
		csvData_df = printObject(allData_df, anyM, fileName = "results_costs", rtnDf = rtnOpt, filterFunc = rmvZero ? x -> abs(x.value) > 1e-5 : x -> true)
	end

	if :raw in rtnOpt
		CSV.write("$(anyM.options.outDir)/results_costs_$(anyM.options.outStamp).csv", allData_df)
	end

	if :rawDf in rtnOpt && :csvDf in rtnOpt
		return allData_df, csvData_df
	else
		if :rawDf in rtnOpt return allData_df end
		if :csvDf in rtnOpt return csvData_df end
	end
end

# ! results for exchange
function reportResults(objGrp::Val{:exchange}, anyM::anyModel; addObjName::Bool=true, wrtNet::Bool = true, rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,), rmvZero::Bool = true, addRep::Tuple{Vararg{Symbol,N} where N} = ())
	allData_df = DataFrame(Ts_expSup = Int[], Ts_disSup = Int[], R_from = Int[], R_to = Int[], C = Int[], Exc = Int[], scr = Int[], dir = Int[], variable = Symbol[], value = Float64[])
	if isempty(anyM.parts.exc) error("No exchange data found") end

    # ! expansion variables
	exp_df = getAllVariables(:expExc, anyM)
	if !isempty(exp_df)
		# manage superordinate dispatch
		select!(exp_df, Not([:Ts_disSup]))
		rename!(exp_df, :Ts_expSup => :Ts_disSup)
		# add direction info
		dirExc_dir = Dict(sysInt(x, anyM.sets[:Exc]) => anyM.parts.exc[x].dir ? 1 : 0 for x in keys(anyM.parts.exc))
		exp_df[!,:dir] = map(x -> dirExc_dir[x], exp_df[!,:Exc])
		exp_df = combine(groupby(exp_df, [:Ts_disSup, :R_from, :R_to, :Exc, :dir]), :var => (x -> value.(sum(x))) => :value)
		exp_df[!,:variable] .= :expExc
		foreach(x -> exp_df[!,x] .= 0, [:Ts_expSup, :C, :scr])
		append!(allData_df, exp_df)
	end

	# ! capacity variables
	for capa in (:capa, :insCapa) 
		capa_df = getAllVariables(Symbol(capa, :Exc), anyM)
		if !isempty(capa_df)
			capa_df[!,:value] = value.(capa_df[!,:var])
			capa_df[!,:variable] .= Symbol(capa, :Exc)
			foreach(x -> capa_df[!,x] .= 0, [:C, :scr])
			append!(allData_df, select(capa_df, Not([:var])))
		end
	end
	# removes small capacity and expansion variables
	filter!((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), allData_df)

	# ! dispatch variables
	disp_df = getAllVariables(:exc, anyM)

	if !isempty(disp_df)
		# add expected values and aggregate
		disp_df[!,:value] = value.(disp_df[!,:var])
		
		# add foresight period if applies
		if anyM.scr.frsLvl != anyM.supTs.lvl && (length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0)))
			disp_df[!,:Ts_frs] = getTsFrs(disp_df[!,:Ts_dis], anyM.sets[:Ts], anyM.scr.frsLvl)
			disp_df = flatten(disp_df,:Ts_frs)
		end

		if length(anyM.scr.scrProb) > 1 || (!isempty(anyM.subPro) && anyM.subPro != (0,0))
			disp_df = addExpVal(select(disp_df,Not([:var])), anyM.scr.scrProb, anyM.sets[:Ts], anyM.scr.lvl, :value)
		end
		disp_df = combine(groupby(disp_df, [:Ts_expSup, :Ts_disSup, :R_from, :R_to, :C, :Exc, :scr]), :value => (x -> sum(x) ./ 1000) => :value)

		disp_df[!,:variable] .= :exc
		disp_df[!,:dir] .= 0
		filter!((rmvZero ? x -> abs(x.value) > 1e-5 : x -> true), disp_df)
		append!(allData_df, disp_df)

		# write values for net-exchange
		if wrtNet
			# join variables in both directions
			netDisp_df = joinMissing(disp_df, select(rename(disp_df, :value => :value_2), Not([:variable])), intCol(disp_df) .=> map(x -> x == :R_from ? :R_to : (x == :R_to ? :R_from : x), intCol(disp_df)), :left, Dict(:value_2 => 0.0,))
			# compute net values
			netDisp_df[!,:value] = netDisp_df[!,:value] .- netDisp_df[!,:value_2]
			# add to dataframe
			netDisp_df[!,:variable] .= :netExc
			filter!(x -> x.value > 0.0, netDisp_df)
			append!(allData_df, select(netDisp_df, Not([:value_2])))
		end

		# ! get full load hours

		# obtain relevant dispatch and capacity values
		aggDispC_df =  combine(groupby(disp_df, [:Ts_expSup, :Ts_disSup, :R_from, :R_to, :Exc, :scr]), :value => (x -> sum(x)) => :from_to)
		capa_df = filter(x -> x.variable == :capaExc, select(allData_df, Not([:C, :scr])))

		# joins energy exchanged in both direction to each capcity
		flh_df = joinMissing(capa_df, aggDispC_df, [:Ts_expSup, :Ts_disSup, :R_from, :R_to, :Exc], :left, Dict(:scr => 0, :from_to => 0.0))
		flh_df = joinMissing(flh_df, rename(switchExcCol(aggDispC_df), :from_to => :to_from), [:Ts_expSup, :Ts_disSup, :R_from, :R_to, :Exc, :scr], :left, Dict(:to_from => 0.0))

		# computs full load hours, considers energy exchanged in one or both directions depending on line type (directed or un-directed)
		flh_df[!,:value] = map(x ->  (x.dir == 0 ? (x.from_to + x.to_from) : x.from_to) / x.value * 1000 , eachrow(flh_df))
		flh_df[!,:variable] .= :flhExc
		flh_df[!,:C] .= 0


		append!(allData_df, select(flh_df, Not([:from_to, :to_from])))
	end

	# removes scenario column if only one scenario is defined
	if length(unique(allData_df[!,:scr])) == 1
		select!(allData_df, Not(:scr))
	end

	# add column with name for model object/scenario
	if addObjName allData_df[!,:objName] .= anyM.options.objName end

	# return dataframes and write csv files based on specified inputs
	if :csv in rtnOpt || :csvDf in rtnOpt
		csvData_df = printObject(allData_df, anyM, fileName = "results_exchange", rtnDf = rtnOpt)
	end

	if :raw in rtnOpt
		CSV.write("$(anyM.options.outDir)/results_exchange_$(anyM.options.outStamp).csv", allData_df)
	end

	if :rawDf in rtnOpt && :csvDf in rtnOpt
		return allData_df, csvData_df
	else
		if :rawDf in rtnOpt return allData_df end
		if :csvDf in rtnOpt return csvData_df end
	end
end

# ! merges reported results according to external yml file
function computeResults(ymlFile::String;model::Union{anyModel,Nothing}=nothing, addName::String="", rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,), csvInput::Dict{Symbol,String} = Dict(:summary => "", :exchange => "", :costs => ""), outputDir::String = "")

    # ! read in mappings and prepare variables
    allMapping_dic = YAML.load_file(ymlFile)

    varMap_dic = allMapping_dic["variables"]
    setMap_dic = allMapping_dic["sets"]
    agg_arr = allMapping_dic["aggregations"]

    if "region" in keys(setMap_dic)
        setMap_dic["region_from"] = setMap_dic["region"]
        setMap_dic["region_to"] = setMap_dic["region"]
    end

    # get relevant variables and filter which are specific
    relVar_arr = Symbol.(union(map(x -> map(y -> y["variable"], collect(values(x))), collect(values(allMapping_dic["variables"])))...))
    relFile_arr = Symbol.(union(map(x -> map(y -> y["file"], collect(values(x))), collect(values(allMapping_dic["variables"])))...))
    optVar_tup = tuple(intersect(relVar_arr, (:cyc, :flh, :effConv, :capaConvOut))...)

    if !isempty(setdiff(relFile_arr, (:summary, :cost, :exchange)))
		error("Provided unsupported file names in YAML file. Supported are :summary, :exchange, :cost.")
	end

    # ! prepare all summary output files
    allVar_df = DataFrame(timestep = String[], region = String[], region_from = String[], region_to = String[], variable = String[], value = Float64[])
    
    repData_dic = Dict{String,DataFrame}()
    for repFile in relFile_arr
    
        if csvInput[repFile] == ""
            if isnothing(model) error("No model object or file provided to obtain data for $repFile") end
            resData_df = reportResults(repFile, model, rtnOpt = (:csvDf,), addRep = optVar_tup)
        elseif !(repFile in keys(csvInput))
            error("The 'csvInput' dictionary does not provide an file for $repFile")
        else
            relVar_arr = String.(relVar_arr)
            try
                resData_df = CSV.read(csvInput[repFile], DataFrame)
            catch
                error("Unable to read the file $(csvInput[repFile]) for $repFile")
            end
        end
        
        repData_df = filter(x -> x.variable in relVar_arr, resData_df)
        # renames columns to match set names
        if repFile == :summary
            rename!(repData_df, "region_dispatch" => "region", "timestep_superordinate_dispatch" => "timestep")
        elseif repFile == :exchange
            rename!(repData_df, "timestep_superordinate_dispatch" => "timestep")
        else
            rename!(repData_df, "timestep_superordinate_dispatch" => "timestep")
        end
        
        # replaces set names with names in mapping
        for set in intersect(names(repData_df), keys(setMap_dic))
            repData_df[!,set] .= map(x -> x in keys(setMap_dic[set]) ? setMap_dic[set][x] : x, repData_df[!,set])
        end
        
        # write variable names as string for correct string comparision below
        repData_df[!,:variable] = string.(repData_df[!,:variable])
        repData_dic[string(repFile)] = combine(groupby(repData_df, filter(x -> x != "value", names(repData_df))), :value => (x -> sum(x)) => :value)
    end

    # ! loop over reporting variables that have to be created
    for reportVar in keys(varMap_dic)

        mapInfo_arr = varMap_dic[reportVar]
        reportVar_df = DataFrame(timestep = String[], region = String[], region_from = String[], region_to = String[], value = Float64[])

        # loop over elements aggregated for reporting variable
        for aggCase in mapInfo_arr

            # load dataframe and filter relevant columns
            loadCsv_df = repData_dic[aggCase["file"]]
            filtCol_arr = intersect(collect(keys(aggCase)), names(loadCsv_df))
            fltCsv_df = filter(x -> all(map(y -> occursin(aggCase[y], x[Symbol(y)]), filtCol_arr)), loadCsv_df)
            if isempty(fltCsv_df) continue end

            # apply correction factor and add to dataframe for variable
            fltCsv_df[!,:value] .= fltCsv_df[!,:value] * aggCase["factor"]

            if aggCase["file"] == "exchange"
                fltCsv_df[!,:region] .= ""
            else
                foreach(x -> fltCsv_df[!,x] .= "", [:region_from, :region_to])
            end

            append!(reportVar_df, combine(groupby(fltCsv_df, ["timestep", "region", "region_from", "region_to"]), :value => (x -> sum(x)) => :value))
        end

        # aggregate all entries for regions and timesteps
        reportVar_df = combine(groupby(reportVar_df, ["timestep", "region", "region_from", "region_to"]), :value => (x -> sum(x)) => :value)

        # add variable column
        reportVar_df[!,:variable] .= reportVar
		if !isempty(reportVar_df) append!(allVar_df, reportVar_df) end
    end
    noAggVar_df = copy(allVar_df)

    # ! obtain variables that can be aggregated from others
    if !isnothing(agg_arr)
        for aggVar in agg_arr
            aggVar_df = select(filter(x -> occursin(aggVar, x.variable) , noAggVar_df), Not([:variable]))
            aggVar_df = combine(groupby(aggVar_df, ["timestep", "region", "region_from", "region_to"]), :value => (x -> sum(x)) => :value)
            aggVar_df[!,:variable] .= aggVar
            append!(allVar_df, aggVar_df)
        end
    end
    
    # ! delete empty columns
    for x in [:region, :region_from, :region_to]
        if unique(allVar_df[!,x]) == [""] select!(allVar_df, Not([x])) end
    end

	# add column with name for model object/scenario
    if addName != "" allVar_df[!,:objName] .= addName end

	# ! return dataframes and write csv files based on specified inputs
	if :csv in rtnOpt CSV.write("$(outputDir)$(split(split(ymlFile, "/")[end], ".")[end-1])$(addName == "" ? "" : "_" * addName).csv", allVar_df) end
    if :df in rtnOpt return allVar_df end
    
end

# ! print time series for in and out into separate tables
"""
```julia
reportTimeSeries(car_sym::Symbol, model_object::anyModel)
```

Writes elements of energy balance for carrier specified by `car_sym` to `.csv` file. See [Time-series](@ref).
"""
function reportTimeSeries(car_sym::Symbol, anyM::anyModel; filterFunc::Function = x -> true, unstck::Bool = true, signVar::Tuple = (:in, :out), minVal::Number = 1e-3, mergeVar::Bool = true, forceEner::Bool = false, rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,))

	# ! converts carrier named provided to index
	node_arr = filter(x -> x.val == string(car_sym), collect(values(anyM.sets[:C].nodes)))
	if length(node_arr) != 1
		error("no carrier named '$car_sym' defined")
		return
	end
	c_int = node_arr[1].idx

	# ! initialize dictionary to save data
	allData_dic = Dict{Symbol,DataFrame}()
	for signItr in signVar
		allData_dic[signItr] = DataFrame(Ts_disSup = Int[], Ts_dis = Int[], R_dis = Int[], scr = Int[], variable = Symbol[], value = Float64[])
	end

	# ! initialize relevant dimensions and carriers
	allLvlTsDis_arr = unique(getfield.(values(anyM.cInfo), :tsDis))
	ts_dic = Dict((x[1], x[2]) => anyM.sets[:Ts].nodes[x[1]].lvl == x[2] ? [x[1]] : getDescendants(x[1], anyM.sets[:Ts], false, x[2]) for x in Iterators.product(anyM.supTs.step, allLvlTsDis_arr))
	relDim_df = filter(filterFunc, createPotDisp([c_int], ts_dic, anyM))
	sort!(relDim_df, orderDim(intCol(relDim_df)))
	relC_arr = unique([c_int, getDescendants(c_int, anyM.sets[:C])...])
	cRes_tup = anyM.cInfo[c_int] |> (x -> (Ts_dis = x.tsDis, R_dis = x.rDis, C = anyM.sets[:C].nodes[c_int].lvl))

	# ! add demand and size it
	if :out in signVar
		dem_df = matchSetParameter(relDim_df, anyM.parts.bal.par[:dem], anyM.sets, newCol = :dem)
		cSub_arr = filter(x -> x != c_int, getDescendants(c_int, anyM.sets[:C], false))
		# add demand of descendant carriers
		if !isempty(cSub_arr)
			allDim_df = createPotDisp(cSub_arr, ts_dic, anyM)
			dem_df = addSubDemand(dem_df, allDim_df, c_int, cSub_arr, anyM.sets, anyM.cInfo, anyM.parts.bal)
		end
		dem_df = rename(dem_df,:dem => :value)
		# adjust sign and add to all data
		dem_df[!,:value] = dem_df[!,:value] .* -1.0
		dem_df[!,:variable] .= :demand
		filter!(x -> abs(x.value) > minVal, dem_df)
		append!(allData_dic[:out], select!(dem_df, Not(:C)))
	end

	# ! adds all technology related variables
	cBalRes_tup = anyM.cInfo[c_int] |> (x -> (x.tsDis, x.rDis))

	for c in relC_arr
		# gets technologies relevant for respective filterCarrier
		relTech_arr = getRelTech(c, anyM.parts.tech, anyM.sets[:C])

		if isempty(relTech_arr) continue end

		for x in relTech_arr

			# gets resolution and adjusts add_df in case of an agggregated technology
			add_df = select(filter(r -> r.C == c, anyM.parts.tech[x[1]].var[x[2]]), [:Ts_disSup, :Ts_dis, :R_dis, :Te, :scr, :var])
			filter!(filterFunc, add_df); select!(add_df, Not([:Te])) # ensure filtering can include technologies	
			tRes_tup = anyM.parts.tech[x[1]].disAgg ? (cRes_tup[1], anyM.parts.tech[x[1]].balLvl.exp[2]) : (cRes_tup[1], cRes_tup[2])
			checkTechReso!(tRes_tup, cBalRes_tup, add_df, anyM.sets)

			# filter values based on filter function and minimum value reported
			add_df = combine(groupby(add_df, [:Ts_disSup, :Ts_dis, :R_dis, :scr]), :var => (x -> sum(x)) => :var)
			
            if isempty(add_df) continue end
			add_df[!,:value] = value.(add_df[!,:var]) .* (x[2] in (:use, :stExtIn) ? -1.0 : 1.0)
			add_df[!,:variable] .= Symbol(x[2], "; ", x[1])
			filter!(x -> abs(x.value) > minVal, add_df)

			# add to dictionary of dataframe for in or out
			sign_sym = x[2] in (:use, :stExtIn) ? :out : :in
			append!(allData_dic[sign_sym], select(add_df, Not([:var])))
		end
	end

	# ! add import and export variables
    if !isempty(anyM.parts.exc)

		relExc_arr = filter(x -> !isempty(intersect(relC_arr, anyM.parts.exc[x].carrier)), collect(keys(anyM.parts.exc)))
		
		if !isempty(relExc_arr)
			if :out in signVar
				excAll_df = vcat(map(x -> anyM.parts.exc[x].var[:exc], filter(z -> :exc in keys(anyM.parts.exc[z].var),relExc_arr))...)
				if !isempty(excAll_df)
					excFrom_df = filterCarrier(excAll_df, relC_arr)
					excFrom_df = combine(groupby(filter(filterFunc, rename(copy(excFrom_df), :R_from => :R_dis)), [:Ts_disSup, :Ts_dis, :R_dis, :scr]), :var => (x -> value(sum(x)) * -1) => :value)
					excFrom_df[!,:variable] .= :export
					filter!(x -> abs(x.value) > minVal, excFrom_df)
					if !isempty(excFrom_df)
						append!(allData_dic[:out], excFrom_df)
					end
				end
			end

			if :in in signVar
				excAll_df = vcat(map(x -> anyM.parts.exc[x] |> (z -> addLossesExc(z.var[:exc], z, anyM.sets)), filter(z -> :exc in keys(anyM.parts.exc[z].var),relExc_arr))...)
				if !isempty(excAll_df)
					excTo_df = filterCarrier(excAll_df, relC_arr)
					excTo_df = combine(groupby(filter(filterFunc, rename(copy(excTo_df), :R_to => :R_dis)), [:Ts_disSup, :Ts_dis, :R_dis, :scr]), :var => (x -> value(sum(x))) => :value)
					excTo_df[!,:variable] .= :import
					filter!(x -> abs(x.value) > minVal, excTo_df)
					if !isempty(excTo_df)
						append!(allData_dic[:in], excTo_df)
					end
				end
			end
		end
	end

	# ! add trade
	agg_arr = [:Ts_dis, :R_dis, :C, :scr]
	if !isempty(anyM.parts.bal.var)
		for trd in intersect(keys(anyM.parts.bal.var), (:trdBuy, :trdSell))
			trdVar_df = copy(relDim_df)
			trdVar_df[!,:value] = value.(filterCarrier(anyM.parts.bal.var[trd], relC_arr) |> (x -> aggUniVar(x, relDim_df, agg_arr, cRes_tup, anyM.sets))) .* (trd == :trdBuy ? 1.0 : -1.0)
			trdVar_df[!,:variable] .= trd
			filter!(x -> abs(x.value) > minVal, trdVar_df)
			sign_sym = :trdBuy == trd ? :in : :out
			append!(allData_dic[sign_sym], select(trdVar_df, Not(:C)))
		end
	end

	# ! add curtailment
	if :crt in keys(anyM.parts.bal.var)
		crt_df = copy(relDim_df)
		crt_df[!,:value] = value.(filterCarrier(anyM.parts.bal.var[:crt], relC_arr) |> (x -> aggUniVar(x, crt_df, agg_arr, cRes_tup, anyM.sets))) .* -1.0
		crt_df[!,:variable] .= :crt
		filter!(x -> abs(x.value) > minVal, crt_df)
		append!(allData_dic[:out], select(crt_df, Not(:C)))
	end

	# ! add losted load
	if :lss in keys(anyM.parts.bal.var)
		lss_df = copy(relDim_df)
		lss_df[!,:value] = value.(filterCarrier(anyM.parts.bal.var[:lss], relC_arr) |> (x -> aggUniVar(x, lss_df, agg_arr, cRes_tup, anyM.sets)))
		lss_df[!,:variable] .= :lss
		filter!(x -> abs(x.value) > minVal, lss_df)
		append!(allData_dic[:in], select(lss_df, Not(:C)))
	end



	# ! unstack data and write to csv
	if mergeVar
		# merges in and out files and writes to same csv file
		data_df = vcat(values(allData_dic)...)

		# convert to energy values 
		if forceEner || anyM.cInfo[c_int].tsDis <= anyM.options.repTsLvl
			data_df[!,:value] = data_df[!,:value] .* getEnergyFac(data_df[!,:Ts_dis], anyM.supTs)
		end

		if unstck && !isempty(data_df)
			data_df[!,:variable] = CategoricalArray(string.(data_df[!, :variable]))
			data_df = unstack(data_df, :variable, :value)
		end

		# removes scenario column if only one scenario is defined
		if length(unique(data_df[!,:scr])) == 1
			select!(data_df, Not(:scr))
		end


		if :csv in rtnOpt || :csvDf in rtnOpt
			csvData_df = printObject(data_df, anyM, fileName = string("timeSeries_", car_sym,), rtnDf = rtnOpt)
		end

		if :raw in rtnOpt
			CSV.write("$(anyM.options.outDir)/$(string("timeSeries_", car_sym,))_$(anyM.options.outStamp).csv", data_df)
		end
	else
		# loops over different signs and writes to different csv files
		for signItr in signVar
			data_df = allData_dic[signItr]
			
			# convert to energy values 
			if forceEner || anyM.cInfo[c_int].tsDis <= anyM.options.repTsLvl
				data_df[!,:value] = data_df[!,:value] .* getEnergyFac(data_df[!,:Ts_dis], anyM.supTs)
			end
			
			if unstck && !isempty(data_df)
				data_df[!,:variable] = CategoricalArray(string.(data_df[!,:variable]))
				data_df = unstack(data_df, :variable, :value)
			end

			# removes scenario column if only one scenario is defined
			if length(unique(data_df[!,:scr])) == 1
				select!(data_df, Not(:scr))
			end


			if :csv in rtnOpt || :csvDf in rtnOpt
				csvData_df = printObject(data_df, anyM, fileName = string("timeSeries_", car_sym, "_", signItr), rtnDf = rtnOpt)
			end

			if :raw in rtnOpt
				CSV.write("$(anyM.options.outDir)/$(string("timeSeries_", car_sym, "_", signItr))_$(anyM.options.outStamp).csv", data_df)
			end
		end
	end

	# return dataframes based on specified inputs
	if :rawDf in rtnOpt && :csvDf in rtnOpt
		return data_df, csvData_df
	else
		if :rawDf in rtnOpt return data_df end
		if :csvDf in rtnOpt return csvData_df end
	end
end

# ! write dual values for constraint dataframe
"""
```julia
printDuals(print_df::DataFrame, model_object::anyModel)
```

Writes duals of a constraint DataFrame to a `.csv` file in readable format (strings instead of ids). See [Individual elements](@ref).
"""
function printDuals(cns_df::DataFrame, anyM::anyModel; filterFunc::Function = x -> true, fileName::String = "", rtnOpt::Tuple{Vararg{Symbol,N} where N} = (:csv,))

    if !(:cns in namesSym(cns_df)) error("No constraint column found!") end
    cns_df = copy(filter(filterFunc, cns_df))
    cns_df[!,:dual] = dual.(cns_df[!,:cns])

	if :csv in rtnOpt || :csvDf in rtnOpt
    	csvData_df = printObject(select(cns_df, Not(:cns)), anyM;fileName = string("dual", fileName != "" ? "_" : "", fileName), rtnDf = rtnOpt)
	end

	if :rawDf in rtnOpt
		CSV.write("$(anyM.options.outDir)/$(string("dual", fileName != "" ? "_" : "", fileName))_$(anyM.options.outStamp).csv", data_df)
	end

	# return dataframes based on specified inputs
	if :rawDf in rtnOpt && :csvDf in rtnOpt
		return select(cns_df, Not(:cns)), csvData_df
	else
		if :rawDf in rtnOpt return data_df end
		if :csvDf in rtnOpt return csvData_df end
	end
end

function reportAggDuals(cns_dic::Dict{Symbol, Vector{Symbol}}, anyM::anyModel)

    # assigns variables to scaling factors for correting duals for scaling later
    scaFac_dic = Dict(:stExtIn => :dispSt, :stExtOut => :dispSt, :stIntIn => :dispSt, :stIntOut => :dispSt, :stLvl => :dispSt, :exc => :dispExc, :gen => :dispConv, :use => :dispConv, :trdBuy => :dispTrd, :trdSell => :dispTrd, :crt => :dispTrd , :lss => :dispTrd,
                    :capaExc => :capa, :capaConv => :capa, :capaStIn => :capa, :capaStOut => :capa, :capaStSize => :capaStSize)

    # fill dataframe with relevant duals
    dual_df = DataFrame(Ts_disSup = Int[], Ts_dis = Int[], scr = Int[], bal = Symbol[], cat = Symbol[], value = Float64[])

    for x in keys(cns_dic)
        for y in cns_dic[x]
            # get specific constraint
            if x == :enBal  
                cnsDual_df = copy(anyM.parts.bal.cns[Symbol(:enBal, makeUp(y))])
            elseif x == :excRestr
                cnsDual_df = copy(anyM.parts.exc[y].cns[:excRestr])
            elseif x == :stBal
                cnsDual_df = copy(anyM.parts.tech[y].cns[:stBal])
                cnsDual_df[!,:Ts_disSup] = map(x -> getAncestors(x, anyM.sets[:Ts], :int, anyM.supTs.lvl)[end], cnsDual_df[!,:Ts_dis])
            end

            # determine scaling applied to equations
            scaFac_arr = map(cnsDual_df[!,:cns]) do z
                firstVar_str = filter(y -> y != "", vcat(map(x -> split.(x, "-"), split(constraint_string(MIME("text/plain"), z), "+"))...))[1]
                firstFac_fl = tryparse(Float64, split(firstVar_str, " ")[1]) |> (x -> isnothing(x) ? 1.0 : x)
                firstVar_sym = Symbol(split(firstFac_fl == 1.0 ? split(firstVar_str, " ")[1] : split(firstVar_str, " ")[2], "[")[1])
                return getfield(anyM.options.scaFac, scaFac_dic[firstVar_sym])/firstFac_fl
            end
            
            # get dual and aggregate
            cnsDual_df[!,:value] .= dual.(cnsDual_df[!,:cns]) .* scaFac_arr
            cnsDual_df = combine(x -> (value = sum(x.value),), groupby(cnsDual_df, [:Ts_disSup, :Ts_dis, :scr]))
            
            # add infos and write to overall object
            cnsDual_df[!,:bal] .= x
            cnsDual_df[!,:cat] .= y
            append!(dual_df, cnsDual_df)
        end
    end

    printObject(dual_df, anyM, fileName = "aggDuals")

end

# ! write storage levels
function reportStorageLevel(anyM, writeAgg::Bool=false)

	stLvl_df = DataFrame(timestep_superordinate_expansion = String[], timestep_superordinate_dispatch = String[],  timestep_dispatch = String[], region_dispatch = String[], carrier = String[], technology = String[], mode = String[], scenario = String[], id = String[], variable = Float64[])

	for tSym in filter(x -> :stLvl in keys(anyM.parts.tech[x].var), keys(anyM.parts.tech))
		append!(stLvl_df, printObject(anyM.parts.tech[tSym].var[:stLvl],anyM, rtnDf = (:csvDf,)))
	end

	CSV.write("$(anyM.options.outDir)/results_storageLevels_$(anyM.options.outStamp).csv", rename(stLvl_df, :variable => :value))

	if writeAgg 
		aggStLvl_df = combine(x -> (value = sum(x.variable),), groupby(stLvl_df,[:timestep_dispatch, :technology, :scenario]))
		CSV.write("$(anyM.options.outDir)/results_aggStorageLevels_$(anyM.options.outStamp).csv", aggStLvl_df) 
	end

end

# ! write results specified in named tuple
function writeAllResults!(anyM::anyModel, res_ntup::NamedTuple)

	if !isempty(res_ntup)
		# write general results
		for x in res_ntup.general reportResults(x, anyM) end

		# write time-series results
		for x in res_ntup.carrierTs reportTimeSeries(x, anyM) end

		# write storage results
		if !isempty(res_ntup.storage) && res_ntup.storage.write reportStorageLevel(anyM, res_ntup.storage.agg) end

		# write aggregated duals
		if !isempty(res_ntup.duals)
			cns_dic = Dict{Symbol, Vector{Symbol}}()
			# get relevant cases for differnt types of dual to be reported
			if :enBal in res_ntup.duals
				cns_dic[:enBal] = map(x -> Symbol(replace(string(x),"enBal" => "")), filter(x -> occursin("enBal", String(x)), collect(keys(anyM.parts.bal.cns))))
			end

			if :excRestr in res_ntup.duals
				cns_dic[:excRestr] = filter(x -> :excRestr in keys(anyM.parts.exc[x].cns), collect(keys(anyM.parts.exc)))
			end

			if :stBal in res_ntup.duals
				cns_dic[:stBal] = filter(x -> :stBal in keys(anyM.parts.tech[x].cns), collect(keys(anyM.parts.tech)))
			end

			reportAggDuals(cns_dic, anyM)
		end
	end
end

#endregion

#region # * plotting tools

# ! plots tree graph for input set
"""
```julia
plotTree(tree_sym::Symbol, model_object::anyModel)
```

Plots the hierarchical tree of nodes for the set specified by `tree_sym`. See [Node trees](@ref).

"""
function plotTree(tree_sym::Symbol, anyM::anyModel; fontSize::Int = 12, useColor::Bool = true, wide::Array{Float64,1} = fill(1.0, 30))

    #region # * initialize variables
    treeName_dic = Dict(:region => :R, :timestep => :Ts, :carrier => :C, :technology => :Te)

    # convert tree object into a data frame
    tree_obj = anyM.sets[treeName_dic[tree_sym]]
    data_arr = filter(x -> x.idx != 0, collect(values(tree_obj.nodes))) |> (y -> map(x -> getfield.(y, x), (:idx, :val, :lvl, :down, :subIdx)))
    tree_df = DataFrame(idx = data_arr[1], val = data_arr[2], lvl =  data_arr[3], down = data_arr[4], subIdx = data_arr[5], up =map(x -> tree_obj.up[x], data_arr[1]))

    # sets options
    col_dic = Dict(:region => (0.133, 0.545, 0.133), :timestep => (0.251, 0.388, 0.847), :carrier => (0.584, 0.345, 0.698), :technology => (0.796, 0.235, 0.2))
    #endregion

    #region # * computes positon of nodes
    # adds a new dummy top node
    push!(tree_df, (0, "", 0, tree_obj.nodes[0].down , 0, 1))
    nodes_int = nrow(tree_df)
    idxPos_dic = Dict(zip(tree_df[:,:idx], 1:(nodes_int)))

    # create vertical position and labels from input tree
    locY_arr = float(tree_df[!,:lvl]) .+ 1.2

    # horizontal position is computed in a two step process
    locX_arr = zeros(Float64, nodes_int)

    # first step, filter all nodes at end of a respective branch and sort them correctly
    lowLvl_df = tree_df[isempty.(tree_df[!,:down]), :]
    lowLvl_df = lowLvl_df[map(y -> findall(x -> x == y, lowLvl_df[:,:idx])[1], sortSiblings(convert(Array{Int64,1}, lowLvl_df[:,:idx]), tree_obj)),:]

    # sets distance from next node on the left depending on if they are part of the same subtree
    for (idx2, lowNode) in Iterators.drop(enumerate(eachrow(lowLvl_df)), 1)
        if lowNode[:up] == lowLvl_df[idx2-1,:up] distance_fl = wide[lowNode[:lvl]] else distance_fl = 1 end
        locX_arr[idxPos_dic[lowNode[:idx]]] = locX_arr[idxPos_dic[lowLvl_df[idx2-1,:idx]]] + distance_fl
    end

    # second step, remaining horizontal nodes are placed in the middle of their children
    highLvl_df = tree_df[false .== isempty.(tree_df[!,:down]),:]
	highLvl_df = highLvl_df[map(y -> findall(x -> x == y, highLvl_df[:,:idx])[1], sortSiblings(convert(Array{Int64,1},highLvl_df[:,:idx]), tree_obj)),:]

    for highNode in reverse(eachrow(highLvl_df))
        locX_arr[idxPos_dic[highNode[:idx]]] = Statistics.mean(locX_arr[map(x -> idxPos_dic[x], highNode.down)])
    end

    locX_arr[end] = Statistics.mean(locX_arr[map(x -> idxPos_dic[x], tree_df[findall(tree_df[:,:lvl] .== 1),:idx])])
    locY_arr = abs.(locY_arr .- maximum(locY_arr))

    # compute dictionary of final node positions
    pos_dic = Dict(x => (locX_arr[x]/maximum(locX_arr), locY_arr[x]/maximum(locY_arr)) for x in 1:nodes_int)
    posIdx_dic = collect(idxPos_dic) |> (z -> Dict(Pair.(getindex.(z, 2), getindex.(z, 1))))
    #endregion

	#region # * determine node colors and labels
	name_dic = anyM.graInfo.names

	label_dic = Dict(x[1] => x[2] == "" ? "" : name_dic[x[2]] for x in enumerate(tree_df[!,:val]))

	if useColor
		col_arr = fill(col_dic[tree_sym], nodes_int)
	else
		col_arr = getNodeColors(collect(1:nodes_int), label_dic, anyM)
	end

	#endregion

    #region # * draw final tree

    # get coordinates for positions
    edgeX_arr = Union{Nothing,Float64}[]
    edgeY_arr = Union{Nothing,Float64}[]

    for rowTree in eachrow(tree_df)
        # 0 node in tree_df becomes last node in graph, because there is 0 node within the plots
        if rowTree[:up] == 0 pare_int = nodes_int else pare_int = idxPos_dic[rowTree[:up]] end
        #push!(edges_arr, (idxPos_dic[rowTree[:idx]], pare_int))
        push!(edgeX_arr, pos_dic[idxPos_dic[rowTree[:idx]]][1])
        push!(edgeY_arr, pos_dic[idxPos_dic[rowTree[:idx]]][2])
        push!(edgeX_arr, pos_dic[pare_int][1])
        push!(edgeY_arr, pos_dic[pare_int][2])
        push!(edgeX_arr, nothing)
        push!(edgeY_arr, nothing)
    end
    
    # get coordinates for nodes
    posX_arr, posY_arr = [map(x -> x[2][y], collect(sort(pos_dic))) for y in [1, 2]]

    # create plot
    label_arr = map(x -> label_dic[x], sort(collect(keys(pos_dic))))
    colStr_arr = "rgb" .* string.(map(x -> x .* 255, col_arr))
    labelPos_arr = map(x -> isempty(x) ? "bottom center" : "top center", tree_df[!,:down])

    edgesTr_obj = scatter(mode="lines", x=edgeX_arr, y=edgeY_arr, line=attr(width=0.5, color="black"))
    nodesTr_obj = scatter(x=posX_arr, y=posY_arr, text = label_arr, textfont_size = fontSize, textfont_color = "black", textfont_family = "Arial", textposition = labelPos_arr, mode="markers+text", hoverinfo = "text", marker=attr(size=24, color= colStr_arr))
    layout_obj = Layout(hovermode="closest", titlefont_size=16, showlegend=false, showarrow=false, xaxis=attr(showgrid=false, zeroline=false, showticklabels=false), yaxis=attr(showgrid=false, zeroline=false, showticklabels=false), paper_bgcolor= "rgba(0,0,0,0)", plot_bgcolor= "rgba(0,0,0,0)")

    savefig(plot([edgesTr_obj, nodesTr_obj], layout_obj), "$(anyM.options.outDir)/$(tree_sym)_$(anyM.options.outStamp).html")
    
    #endregion

end

# ! plot qualitative energy flow graph from model object or yml file
"""
```julia
plotNetworkGraph(plotType::Symbol, model_object::anyModel)
```

Plots the network graph for energy flows in a model.

"""
function plotNetworkGraph(anyM::anyModel; fontSize::Int = 12, replot::Bool = true, scaDist::Number = 0.5, maxIter::Int = 5000, initTemp::Number = 2.0, useTeColor::Bool = false, wrtYML::Bool = false, relC::Tuple = ())

	#region # * create graph and map edges
	
	# ! get relevant model and graph ids for carriers and technologies

	# graph and model ids of carriers
	graC_arr = Array{Int,1}()
	try
		graC_arr = isempty(relC) ? collect(values(anyM.graInfo.graph.nodeC)) : map(x -> anyM.graInfo.graph.nodeC[sysInt(x, anyM.sets[:C])], collect(relC))
	catch
		error("Entered wrong carrier name!")
	end
	modC_arr = isempty(relC) ? collect(keys(anyM.sets[:C].nodes)) : map(x -> sysInt(x, anyM.sets[:C]), collect(relC))

	# graph and model ids of technologies connected to relevant carriers
	actTe_arr = unique(map(x -> sysSym(x, anyM.sets[:Te]) in keys(anyM.parts.tech) ? x : getDescendants(x, anyM.sets[:Te], true)[end], collect(keys(anyM.graInfo.graph.nodeTe)))) # not all actual technologies are represented in graph, e.g. to avoid 3 different solar 
	modTe_arr = filter(x -> sysSym(x, anyM.sets[:Te]) |> (u -> u in keys(anyM.parts.tech) && !isempty(intersect(modC_arr, union(map(w -> union(w...), values(anyM.parts.tech[u].carrier))...)))), actTe_arr) # check actual technology for carriers
	modTe_arr = map(x -> x in keys(anyM.graInfo.graph.nodeTe) ? x : maximum(map(y -> y in keys(anyM.graInfo.graph.nodeTe) ? y : 0, getAncestors(x, anyM.sets[:Te], :int))), modTe_arr) # convert to technology in graph again
	graTe_arr = map(x -> anyM.graInfo.graph.nodeTe[x], modTe_arr) # get technology id in graph

	# ! create relevant edges for carriers and technologies

	flowGrap_obj = anyM.graInfo.graph

	cEdge_arr = filter(x -> x[1] in graC_arr || x[2] in graC_arr, collect.(flowGrap_obj.edgeC))
	teEdge_arr = filter(x -> x[1] in graTe_arr || x[2] in graTe_arr, flowGrap_obj.edgeTe)
	edges_arr =  vcat(cEdge_arr, collect.(teEdge_arr))

	#endregion

	#region # * obtain and order graph properties (colors, names, etc.)

	# get carriers that should be plotted, because they are connected with a technology
	relNodeC1_arr = filter(x -> x[2] in vcat(getindex.(flowGrap_obj.edgeTe, 1), getindex.(flowGrap_obj.edgeTe, 2)), collect(flowGrap_obj.nodeC))
	# get carriers that shold be plotted, because they are connected with another carrier that should be plotted
	relNodeC2_arr = filter(x -> any(map(y -> x[2] in y && !isempty(intersect(getindex.(relNodeC1_arr, 2), y)) , collect.(flowGrap_obj.edgeC))), collect(flowGrap_obj.nodeC))

	# maps node id to node names
	idToC_arr = map(x -> x[2] => anyM.sets[:C].nodes[x[1]].val, filter(y -> y[2] in union(edges_arr...), intersect(flowGrap_obj.nodeC, union(relNodeC1_arr, relNodeC2_arr))))
	idToTe_arr  = map(x -> x[2] => anyM.sets[:Te].nodes[x[1]].val, filter(y -> y[2] in union(edges_arr...), collect(flowGrap_obj.nodeTe)))
	idToName_dic = Dict(vcat(idToC_arr, idToTe_arr))

	# obtain colors of nodes
	ordC_arr = intersect(unique(vcat(edges_arr...)), getindex.(idToC_arr, 1))
	ordTe_arr = intersect(unique(vcat(edges_arr...)), getindex.(idToTe_arr, 1))
	nodeC_arr = "rgb" .* string.(map(x -> x .* 255, getNodeColors(ordC_arr, idToName_dic, anyM)))
	nodeTe_arr = "rgb" .* string.(useTeColor ? map(x -> x .* 255, getNodeColors(ordTe_arr, idToName_dic, anyM)) : fill((216.75, 216.75, 216.75), size(ordTe_arr, 1)))

	# obtain name of nodes
	cLab_dic = Dict(y[1] => anyM.graInfo.names[y[2]] for y in filter(x -> x[1] in ordC_arr, idToName_dic))
	teLab_dic = Dict(y[1] => anyM.graInfo.names[y[2]] for y in filter(x -> !(x[1] in ordC_arr), idToName_dic))

	nodesCnt_int = length(idToName_dic)

	# converts edges to sparse matrix for flowLayout function
	id_arr = vcat(getindex.(idToC_arr, 1), getindex.(idToTe_arr, 1))
	edges_mat = convert(Array{Int64,2}, zeros(nodesCnt_int, nodesCnt_int))
	foreach(x -> edges_mat[findall(id_arr .== x[1])[1], findall(id_arr .== x[2])[1]] = 1, filter(x -> x[1] in id_arr && x[2] in id_arr, edges_arr))
	edges_smat = SparseArrays.sparse(edges_mat)

	# compute position of nodes
	if replot || !(isdefined(flowGrap_obj, :nodePos))
		pos_dic = flowLayout(nodesCnt_int, edges_smat; scaDist = scaDist, maxIter = maxIter, initTemp = initTemp)
		flowGrap_obj.nodePos = Dict(id_arr[x] => pos_dic[x] for x in keys(pos_dic))
	end

	# separate into edges between technologies and carriers and between carriers, then get respective colors
	edgeColC_arr = "rgb" .* string.(map(x -> anyM.graInfo.colors[idToName_dic[x[1]]] .* 255, cEdge_arr))
	edgeColTe_arr = "rgb" .* string.(map(x -> (x[1] in ordC_arr ? anyM.graInfo.colors[idToName_dic[x[1]]] : anyM.graInfo.colors[idToName_dic[x[2]]]) .* 255, teEdge_arr))

	#endregion

	#region # * draw and save graph

	edgeX_arr = Union{Nothing, Float64}[]
	edgeY_arr = Union{Nothing, Float64}[]

	for x in vcat(cEdge_arr, teEdge_arr)
		push!(edgeX_arr, flowGrap_obj.nodePos[x[1]][1])
		push!(edgeY_arr, flowGrap_obj.nodePos[x[1]][2])
		push!(edgeX_arr, flowGrap_obj.nodePos[x[2]][1])
		push!(edgeY_arr, flowGrap_obj.nodePos[x[2]][2])
		push!(edgeX_arr, nothing)
		push!(edgeY_arr, nothing)
	end

	# get coordinates for nodes
	posX_arr, posY_arr = [map(x -> flowGrap_obj.nodePos[x][y], vcat(ordC_arr, ordTe_arr)) for y in [1, 2]]

	# create plot
	label_arr = vcat(map(x -> cLab_dic[x], ordC_arr), map(x -> teLab_dic[x], ordTe_arr))
	marker_arr = vcat(fill("square", size(ordC_arr, 1)), fill("circle", size(ordTe_arr, 1)))

	nodesTr_obj = scatter(x=posX_arr[1:2], y=posY_arr[1:2], text = label_arr[1:2], marker_symbol = marker_arr[1:2], textfont_size = fontSize, textfont_color = "black", textfont_family = "Arial", textposition = "top center", mode="markers+text", hoverinfo = "text", marker=attr(size=18, color = vcat(nodeC_arr, nodeTe_arr)))
	layout_obj = Layout(hovermode="closest", titlefont_size=16, showlegend=false, showarrow=false, xaxis=attr(showgrid=false, zeroline=false, showticklabels=false), yaxis=attr(showgrid=false, zeroline=false, showticklabels=false), paper_bgcolor= "rgba(0,0,0,0)", plot_bgcolor= "rgba(0,0,0,0)")

	graph_pl = plot(nodesTr_obj, layout_obj)
	edgeCol_arr = vcat(edgeColC_arr, edgeColTe_arr)

	for i in 1:length(edgeCol_arr)
		add_trace!(graph_pl, scatter(mode="lines+markers", marker=attr(symbol="arrow-up", size=10, angleref="previous"), x=edgeX_arr[1+(i-1)*3:3+(i-1)*3], y=edgeY_arr[1+(i-1)*3:3+(i-1)*3], line=attr(width=0.5, color = edgeCol_arr[i])))
	end

	add_trace!(graph_pl, nodesTr_obj)

	savefig(graph_pl, "$(anyM.options.outDir)/energyFlowGraph_$(anyM.options.outStamp).html")

	# write plot information to yaml file as well
	if wrtYML
		adjPos_dic = Dict(x[1] => (x[2] .+ 1) ./ 2 for x in collect(flowGrap_obj.nodePos))
		revName_dic = Dict(v => k for (k, v) in anyM.graInfo.names)
		techNode_arr = [Dict("label" => teLab_dic[n], "name" => revName_dic[teLab_dic[n]], "color" => convertCol(nodeTe_arr[length(nodeTe_arr) == 1 ? 1 : id]), "position" => adjPos_dic[n], "type" => "technology") for (id, n) in enumerate(ordTe_arr)]
		carNode_arr = [Dict("label" => cLab_dic[n], "name" => revName_dic[cLab_dic[n]], "color" => convertCol(nodeC_arr[id]), "position" => adjPos_dic[n], "type" => "carrier") for (id, n) in enumerate(ordC_arr)]
		allLab_dic = merge(teLab_dic, cLab_dic)
		YAML.write_file("$(anyM.options.outDir)/energyFlowGraph_$(anyM.options.outStamp).yml", Dict("vertices" => vcat(techNode_arr, carNode_arr), "edges" => [revName_dic[allLab_dic[e[1]]] => revName_dic[allLab_dic[e[2]]] for e in vcat(flowGrap_obj.edgeC, flowGrap_obj.edgeTe)]))
	end
    #endregion
end
function plotNetworkGraph(inFile::String; fontSize::Int = 12)

    # ! extract node data from yaml file and convert
    graph_dic = YAML.load_file(inFile)

    cData_arr, teData_arr = [filter(x -> x["type"] == z, graph_dic["vertices"]) for z in ["carrier", "technology"]]


    cNum_int = length(cData_arr)
    nodePos_dic = vcat(cData_arr, teData_arr) |> (w -> Dict(x => w[x]["position"] |> (u -> [(u[1]*2-1), u[2]+2-1]) for x in 1:length(w))) # assign positions to nodes

    # assign names to nodes
    nameC_dic = Dict(y => cData_arr[y]["name"] for y in 1:length(cData_arr))
    nameTe_dic = Dict(cNum_int+y => teData_arr[y]["name"] for y in 1:length(teData_arr))
    revName_dic = merge(Dict(v => k for (k, v) in nameC_dic), Dict(v => k for (k, v) in nameTe_dic))

    # assign colors to nodes
    colorC_dic = Dict(y => "rgb" .* string(tuple(cData_arr[y]["color"]...) .* 255) for y in 1:length(cData_arr))
    colorTe_dic = Dict(cNum_int+y => "rgb" .* string(tuple(teData_arr[y]["color"]...) .* 255) for y in 1:length(teData_arr))

    # assign labels to nodes
    labC_dic = Dict(y => cData_arr[y]["label"] for y in 1:length(cData_arr))
    labTe_dic = Dict(cNum_int+y => teData_arr[y]["label"] for y in 1:length(teData_arr))

    # prepare edges
    allEdges_arr = map(x -> revName_dic[string(x[1])] => revName_dic[x[2]], getindex.(collect.(graph_dic["edges"]), 1))
    ordC_arr = collect(keys(labC_dic)) 
    ordTe_arr = collect(keys(labTe_dic)) 

    # separate into edges between technologies and carriers and between carriers, then get respective colors
    cEdges_arr = filter(x -> x[1] in ordC_arr && x[2] in ordC_arr, allEdges_arr)
    edgeColC_arr = map(x -> colorC_dic[x[1]], cEdges_arr)

    teEdges_arr = collect(keys(labTe_dic)) |> (w -> filter(x -> x[1] in w || x[2] in w, allEdges_arr))
    edgeColTe_arr = map(x -> x[1] in ordC_arr ? colorC_dic[x[1]] : colorC_dic[x[2]], teEdges_arr)

    # ! create actual graph
    # create graph and draw nodes and edges
    edgeX_arr = Union{Nothing,Float64}[]
    edgeY_arr = Union{Nothing,Float64}[]

    for x in vcat(cEdges_arr, teEdges_arr)
        push!(edgeX_arr, nodePos_dic[x[1]][1])
        push!(edgeY_arr, nodePos_dic[x[1]][2])
        push!(edgeX_arr, nodePos_dic[x[2]][1])
        push!(edgeY_arr, nodePos_dic[x[2]][2])
        push!(edgeX_arr, nothing)
        push!(edgeY_arr, nothing)
    end

    # get coordinates for nodes
    posX_arr, posY_arr = [map(x -> nodePos_dic[x][y], vcat(ordC_arr, ordTe_arr)) for y in [1, 2]]

    # create plot
    label_arr = vcat(map(x -> labC_dic[x], ordC_arr), map(x -> labTe_dic[x], ordTe_arr))
    color_arr = vcat(map(x -> colorC_dic[x], ordC_arr), map(x -> colorTe_dic[x], ordTe_arr))
    marker_arr = vcat(fill("square", size(ordC_arr, 1)), fill("circle", size(ordTe_arr, 1)))

    nodesTr_obj = scatter(x=posX_arr, y=posY_arr, text = label_arr, marker_symbol = marker_arr, textfont_size = fontSize, textfont_color = "black", textfont_family = "Arial", textposition = "top center", mode="markers+text", hoverinfo = "text", marker=attr(size=18, color = color_arr))
    layout_obj = Layout(hovermode="closest", titlefont_size=16, showlegend=false, showarrow=false, xaxis=attr(showgrid=false, zeroline=false, showticklabels=false), yaxis=attr(showgrid=false, zeroline=false, showticklabels=false), paper_bgcolor= "rgba(0,0,0,0)", plot_bgcolor= "rgba(0,0,0,0)")

    graph_pl = plot(nodesTr_obj, layout_obj)
    edgeCol_arr = vcat(edgeColC_arr, edgeColTe_arr)

    for i in 1:length(edgeCol_arr)
        add_trace!(graph_pl, scatter(mode="lines", x=edgeX_arr[1+(i-1)*3:3+(i-1)*3], y=edgeY_arr[1+(i-1)*3:3+(i-1)*3], line=attr(width=0.5, color = edgeCol_arr[i])))
    end

    add_trace!(graph_pl, nodesTr_obj)

    savefig(graph_pl, replace(inFile, ".yml" => ".html"))
end

# ! plot quantitative energy flow sankey diagramm (from model object or file)
"""
```julia
plotNetworkGraph(plotType::Symbol, model_object::anyModel)
```

Plots the Sankey diagram for energy flows in a model.

"""
# ! plot quantitative energy flow sankey diagramm (applies python module plotly via PyCall package)
function plotSankeyDiagram(anyM::anyModel; dataIn::String = "", fontSize::Int = 12, minVal::Float64 = 0.1, filterFunc::Function = x -> true, dropDown::Tuple{Vararg{Symbol,N} where N} = (:region, :timestep, :scenario), rmvNode::Tuple{Vararg{String,N} where N} = tuple(), useTeColor::Bool = false, netExc::Bool = true, name::String = "", ymlFilter::String = "", savaData::Bool = false, wrtVal::Bool = true, digVal::Int = 1, sgnVal::String = ";")

    flowGrap_obj = anyM.graInfo.graph

    #region # * initialize data

    if !isempty(setdiff(dropDown, [:region, :timestep, :scenario]))
    error("dropDown only accepts array :region and :timestep as content")
    end

	# get mappings to create buttons of dropdown menue
	drop_dic = Dict(:region => :R_dis, :timestep => :Ts_disSup, :scenario => :scr)
	dropDim_arr = collect(map(x -> drop_dic[x], dropDown))

	if isempty(dataIn)
		# get summarised data and filter dispatch variables
		data_df = select(reportResults(:summary, anyM; rtnOpt = (:rawDf,)), Not([:objName]))
		filter!(x -> x.variable in (:demand, :gen, :use, :stExtIn, :stExtOut, :trdBuy, :trdSell, :demand, :import, :export, :lss, :crt), data_df)
		data_df[!,:variable] = map(x -> x in (:stExtIn, :stExtOut) ? Symbol(replace(string(x), "Ext" => "")) : x, data_df[!,:variable])


		# substracts demand from descendant carriers from demand of upwards carriers displayed in sankey diagram
		c_dic, r_dic = [anyM.sets[x].nodes for x in [:C, :R]]
		if :scr in namesSym(data_df)
			data_df[!,:value] = map(x -> x.value - (x.variable == :demand ? sum(filter(y -> y.scr == x.scr && y.variable == :demand && y.Ts_disSup == x.Ts_disSup && y.R_dis in vcat([x.R_dis], r_dic[x.R_dis].down) && y.C in c_dic[x.C].down, data_df)[!,:value]) : 0.0), eachrow(data_df))
		else
			data_df[!,:value] = map(x -> x.value - (x.variable == :demand ? sum(filter(y -> y.variable == :demand && y.Ts_disSup == x.Ts_disSup && y.R_dis in vcat([x.R_dis], r_dic[x.R_dis].down) && y.C in c_dic[x.C].down, data_df)[!,:value]) : 0.0), eachrow(data_df))
		end
	else
		data_df = CSV.read(dataIn, DataFrame)
		filter!(x -> x.variable in ("demand", "gen", "use", "stIn", "stOut", "trdBuy", "trdSell", "demand", "import", "export", "lss", "crt"), data_df)
		if "objName" in names(data_df) select!(data_df, Not([:objName])) end
		data_df[!,:timestep_superordinate_dispatch] = map(x -> lookupString(x, anyM.sets[:Ts]), data_df[!,:timestep_superordinate_dispatch])
		data_df[!,:region_dispatch] = map(x -> lookupString(x, anyM.sets[:R]), data_df[!,:region_dispatch])
		data_df[!,:technology] = map(x -> lookupString(x, anyM.sets[:Te]), data_df[!,:technology])
		data_df[!,:carrier] = map(x -> lookupString(x, anyM.sets[:C]), data_df[!,:carrier])
		data_df[!,:id] = map(x -> lookupString(x, anyM.sets[:id]), data_df[!,:id])
		data_df[!,:variable] = Symbol.(data_df[!,:variable]) 
		rename!(data_df, [:timestep_superordinate_dispatch => :Ts_disSup, :region_dispatch => :R_dis, :technology => :Te, :carrier => :C])
	end

	if savaData 
		printObject(data_df, anyM, wrtGap = true, fileName = "sankeyData$(name == "" ? "" : "_" * name)")
	end
	
	# converts export and import quantities into net values
	if netExc
		allExc_df = filter(x -> x.variable in (:import, :export), data_df)
		if !isempty(data_df)
			joinedExc_df = joinMissing(select(rename(filter(x -> x.variable == :export, allExc_df), :value => :export), Not([:variable])), select(rename(filter(x -> x.variable == :import, allExc_df), :value => :import), Not([:variable])), intCol(data_df), :outer, Dict(:export => 0.0, :import => 0.0))
			joinedExc_df[!,:value] = joinedExc_df[!,:export] .+ joinedExc_df[!,:import]
			select!(joinedExc_df, Not([:export, :import]))
			joinedExc_df[!,:variable] = map(x -> x > 0.0 ? :netImport : :netExport, joinedExc_df[!,:value])
			joinedExc_df[!,:value] = abs.(joinedExc_df[!,:value])
			data_df = vcat(joinedExc_df, filter(x -> !(x.variable in (:export, :import)), data_df))
		end
	end

    # filter non relevant entries
    filter!(x -> abs(x.value) > minVal, data_df)
    filter!(filterFunc, data_df)

	#endregion
	
	#region # * filter flows according to provided yaml file

	if !isempty(ymlFilter)
		graph_dic = YAML.load_file(ymlFilter)
		# filters entries that aggregates several sub-categories
		agg_arr = filter(x -> "aggregating" in keys(x), graph_dic["vertices"])
		nonAgg_arr = graph_dic["vertices"]
		aggTe_arr, aggCe_arr =  map(z -> vcat(map(y -> y["aggregating"], filter(x -> x["type"] == z, agg_arr))...), ["technology", "carrier"])
		nonAggTe_arr, nonAggCe_arr = map(z -> map(y -> y["name"], filter(x -> x["type"] == z, nonAgg_arr)), ["technology", "carrier"])
		# filters relevant technologies and carriers
		te_arr = map(vcat(aggTe_arr..., nonAggTe_arr...)) do y
			if y in getfield.(collect(values(anyM.sets[:Te].nodes)), :val)
				return sysInt(Symbol(y), anyM.sets[:Te])
			else 
				error("technology " * y * " not defined!")
			end
		end
		c_arr = map(vcat(aggCe_arr..., nonAggCe_arr...)) do y
			if y in getfield.(collect(values(anyM.sets[:C].nodes)), :val)
				return sysInt(Symbol(y), anyM.sets[:C])
			else 
				error("technology " * y * " not defined!")
			end
		end

		filter!(x -> x.C in c_arr && (x.Te == 0 || x.Te in te_arr), data_df)
		# perform aggregation
		for u in (:Te, :C)
			agg_dic = Dict(vcat(map(x -> map(y -> sysInt(Symbol(y), anyM.sets[u]) => sysInt(Symbol(x["name"]), anyM.sets[u]), vcat(x["name"], x["aggregating"]...)), filter(x -> x["type"] == (u == :Te ? "technology" : "carrier"), agg_arr))...))
			data_df[!,u] = map(x -> x in keys(agg_dic) ? agg_dic[x] : x, data_df[!,u]) 
			data_df = combine(groupby(data_df, intCol(data_df, :variable)), :value => (x -> sum(x)) => :value)
		end
	end

	#endregion
	
	#region # * prepare labels and colors

	# create dictionaries for nodes that are neither technology nor carrier
	oth_df = unique(filter(x -> x.Te == 0, data_df)[!,[:variable, :C]])
	if netExc && !(:region in dropDown)
		oth_df[!,:variable] =  map(x -> x == :netExport ? :exchangeLoss : x, oth_df[!,:variable])
	end	
	othNode_dic = maximum(values(flowGrap_obj.nodeTe)) |> (z -> Dict((x[2].C, x[2].variable) => x[1] + z for x in enumerate(eachrow(oth_df))))
	othNodeId_dic = collect(othNode_dic) |> (z -> Dict(Pair.(getindex.(z, 2), getindex.(z, 1))))

    # prepare name and color assignment
    names_dic = isempty(ymlFilter) ? anyM.graInfo.names : Dict(x["name"] => x["label"] for x in collect(graph_dic["vertices"])) |> (z -> merge(z, filter(x -> !(x[1] in keys(z)), anyM.graInfo.names)))
    revName_dic = collect(names_dic) |> (z -> Dict(Pair.(getindex.(z, 2), getindex.(z, 1))))

	# use color from yaml, if any are provided
	if !isempty(ymlFilter)
		col_dic = Dict(x["name"] => tuple(x["color"]...) for x in graph_dic["vertices"])
	else
		col_dic = anyM.graInfo.colors
	end

    sortTe_arr = getindex.(sort(collect(flowGrap_obj.nodeTe), by = x -> x[2]), 1)
    cColor_dic = Dict(x => anyM.sets[:C].nodes[x].val |> (z -> z in keys(col_dic) ? col_dic[z] : (0.85, 0.85, 0.85)) for x in sort(collect(keys(flowGrap_obj.nodeC))))

    # create array of node labels
    cLabel_arr = map(x -> anyM.sets[:C].nodes[x].val |> (z -> z in keys(names_dic) ? names_dic[z] : z), sort(collect(keys(flowGrap_obj.nodeC))))
    teLabel_arr = map(x -> anyM.sets[:Te].nodes[x].val |> (z -> z in keys(names_dic) ? names_dic[z] : z), sortTe_arr)
    othLabel_arr = map(x -> names_dic[String(othNodeId_dic[x][2])], sort(collect(keys(othNodeId_dic))))
    nodeLabelAll_arr = vcat(cLabel_arr, teLabel_arr, othLabel_arr)
    revNodelLabel_arr = map(x -> revName_dic[x], nodeLabelAll_arr)

    # create array of node colors
    cColor_arr = map(x -> cColor_dic[x], sort(collect(keys(flowGrap_obj.nodeC))))
    teColor_arr = map(x -> anyM.sets[:Te].nodes[x].val |> (z -> z in keys(col_dic) && useTeColor ? col_dic[z] : (0.85, 0.85, 0.85)), sortTe_arr)
    othColor_arr = map(x -> anyM.sets[:C].nodes[othNodeId_dic[x][1]].val |> (z -> z in keys(col_dic) ? col_dic[z] : (0.85, 0.85, 0.85)), sort(collect(keys(othNodeId_dic))))
    nodeColor_arr = vcat(map(x -> replace.(string.("rgb", string.(map(z -> z .* 255.0, x))), " " => ""), [cColor_arr, teColor_arr, othColor_arr])...)
	dropData_arr = PlotlyBase.PlotlyAttribute{Dict{Symbol, Any}}[]

	if ymlFilter != "" && "removeSankey" in keys(graph_dic)
		rmvNode = map(x -> collect(x)[1] |> (z -> string(z[1], "; ", z[2])), collect(graph_dic["removeSankey"])) |> (u ->  isempty(rmvNode) ? tuple(u...) : tuple(u..., rmvNode...))
	end

	# ! loop over potential buttons in dropdown menue
	for drop in eachrow(unique(data_df[!, intersect(namesSym(data_df), dropDim_arr)]))
	
		#region # * filter data and create flow array
		nodeLabel_arr = copy(nodeLabelAll_arr)
	
		dropData_df = copy(data_df)
		if :region in dropDown subR_arr = [drop.R_dis, getDescendants(drop.R_dis, anyM.sets[:R], true)...] end
		for d in dropDown
			filter!(x -> d == :region ? x.R_dis in subR_arr : (d == :timestep ? x.Ts_disSup == drop.Ts_disSup : length(anyM.sets[:scr].nodes) == 1 || x.scr == drop.scr), dropData_df)
		end
		
		if netExc
			allExc_df = filter(x -> x.variable in (:netImport, :netExport), dropData_df)
			if !isempty(allExc_df)
				allExc_df[!,:value] = map(x -> x.variable == :netExport ? x.value * -1 : x.value, eachrow(allExc_df))
				aggExc_df = combine(groupby(allExc_df, [:Ts_disSup, :Te, :C]), :value => (x -> sum(x)) => :value)
				aggExc_df[!,:variable] = map(x -> x.value > 0.0 ? :netImport : :netExport, eachrow(aggExc_df))
				# renames net-export into losses in case regions does not appear in drop dropDown
				if !(:region in dropDown)
					aggExc_df[!,:variable] = map(x -> x == :netExport ? :exchangeLoss : x, aggExc_df[!,:variable])
					aggExc_df[!,:R_dis] .= 0
				else
					aggExc_df[!,:R_dis] .= drop.R_dis
				end
				if length(anyM.sets[:scr].nodes) != 1 aggExc_df[!,:scr] .= !(:scenario in dropDown) ? 0 : drop.scr end
	
				aggExc_df[!,:id] .= 0
				dropData_df = vcat(filter(x -> !(x.variable in (:netImport, :netExport)), dropData_df), aggExc_df)
			end
		end
		flow_arr = Array{Tuple,1}()
	
		# write flows reported in data summary
		for x in eachrow(dropData_df)
			a = Array{Any,1}(undef,3)
		
			# technology related entries
			if x.variable in (:demand, :export, :trdSell, :crt, :netExport, :exchangeLoss)
				a[1] = flowGrap_obj.nodeC[x.C]
				a[2] = othNode_dic[(x.C, x.variable)]
			elseif x.variable in (:import, :trdBuy, :lss, :netImport)
				a[1] = othNode_dic[(x.C, x.variable)]
				a[2] = flowGrap_obj.nodeC[x.C]
			elseif x.variable in (:gen, :stOut)
		
				if x.Te in keys(flowGrap_obj.nodeTe) # if technology is not directly part of the graph, use its smallest parent that its
					a[1] = flowGrap_obj.nodeTe[x.Te]
				else
					a[1] = flowGrap_obj.nodeTe[minimum(intersect(keys(flowGrap_obj.nodeTe), getAncestors(x.Te, anyM.sets[:Te], :int)))]
				end
		
				a[2] = flowGrap_obj.nodeC[x.C]
			else
				a[1] = flowGrap_obj.nodeC[x.C]
		
				if x.Te in keys(flowGrap_obj.nodeTe)
					a[2] = flowGrap_obj.nodeTe[x.Te]
				else
					a[2] = flowGrap_obj.nodeTe[minimum(intersect(keys(flowGrap_obj.nodeTe), getAncestors(x.Te, anyM.sets[:Te], :int)))]
				end
			end
		
			a[3] = abs(x.value)
		
			push!(flow_arr, tuple(a...))
		end
	
		# create flows connecting different carriers
		idToC_dic = Dict(map(x -> x[2] => x[1], collect(flowGrap_obj.nodeC)))
		for x in filter(x -> anyM.sets[:C].up[x] != 0, intersect(union(getindex.(flow_arr, 1), getindex.(flow_arr, 2)), values(flowGrap_obj.nodeC)))
			a = Array{Any,1}(undef,3)
			a[1] = flowGrap_obj.nodeC[x]
			a[2] = flowGrap_obj.nodeC[anyM.sets[:C].up[x]]
			a[3] = (getindex.(filter(y -> y[2] == x, flow_arr), 3) |> (z -> isempty(z) ? 0.0 : sum(z))) - (getindex.(filter(y -> y[1] == x, flow_arr), 3) |> (z -> isempty(z) ? 0.0 : sum(z)))
			push!(flow_arr, tuple(a...))
		end
	
		# merges flows for different regions that connect the same nodes
		flow_arr = map(unique(map(x -> x[1:2], flow_arr))) do fl
			allFl = filter(y -> y[1:2] == fl[1:2], flow_arr)
			return (allFl[1][1], allFl[1][2], sum(getindex.(allFl, 3)))
		end

		# removes nodes accoring function input provided
		for rmv in rmvNode
			# splits remove expression by semicolon and searches for first part
			rmvStr_arr = split(rmv, "; ")
			relNodes_arr = findall(nodeLabel_arr .== rmvStr_arr[1])
			if isempty(relNodes_arr) relNodes_arr = findall(revNodelLabel_arr .== rmvStr_arr[1]) end
			if isempty(relNodes_arr) continue end

			if length(rmvStr_arr) == 2 # if rmv contains two strings seperated by a semicolon, the second one should relate to a carrier, carrier is searched for and all related flows are removed
				relC_arr = findall(nodeLabel_arr .== rmvStr_arr[2])
				if isempty(relNodes_arr) relC_arr = findall(revNodelLabel_arr .== rmvStr_arr[2]) end

				if isempty(relC_arr)
					continue
				else
					c_int = relC_arr[1]
				end

				filter!(x -> !((x[1] in relNodes_arr || x[2] in relNodes_arr) && (x[1] == c_int || x[2] == c_int)), flow_arr)
				elseif length(rmvStr_arr) > 2
				error("one remove string contained more then one semicolon, this is not supported")
				else # if rmv only contains one string, only nodes where in- and outgoing flow are equal or only one of both exists
				out_tup = filter(x -> x[1] == relNodes_arr[1], flow_arr)
				in_tup = filter(x -> x[2] == relNodes_arr[1], flow_arr)

				if length(out_tup) == 1 && length(in_tup) == 1 && out_tup[1][3] == in_tup[1][3] # in- and outgoing are the same
					filter!(x -> !(x in (out_tup[1], in_tup[1])), flow_arr)
					push!(flow_arr, (in_tup[1][1], out_tup[1][2], in_tup[1][3]))
				elseif length(out_tup) == 0 # only ingoing flows
					filter!(x -> !(x in in_tup), flow_arr)
				elseif length(in_tup) == 0 # only outgoing flows
					filter!(x -> !(x in out_tup), flow_arr)
				end
			end
		end
	
		#endregion
	
		#region # * create dictionaries for later plotting
	
		# collect data for drop in a dictionary
	
		linkColor_arr = map(x -> collect(x[1] in keys(cColor_dic) ? cColor_dic[x[1]] : cColor_dic[x[2]]) |>
			(z -> replace(string("rgba", string(tuple([255.0 .*z..., (x[1] in keys(cColor_dic) && x[2] in keys(cColor_dic) ? 0.8 : 0.5)]...))), " " => "")), flow_arr)
		link_obj = attr(source = getindex.(flow_arr, 1) .- 1, target = getindex.(flow_arr, 2) .- 1, value = getindex.(flow_arr, 3), color = linkColor_arr)


		# compute values for each node to written to graph
		if wrtVal
			for x in 1:length(nodeLabel_arr)
				relFlow_arr = filter(y -> y[1] == x, flow_arr)
				if isempty(relFlow_arr)
					relFlow_arr = filter(y -> y[2] == x, flow_arr)
					if isempty(relFlow_arr) && nodeLabel_arr[x] != ""
						nodeLabel_arr[x] = nodeLabel_arr[x] * ", " * string(0.0)
					end
				end
				if nodeLabel_arr[x] != ""
					nodeLabel_arr[x] = nodeLabel_arr[x] * sgnVal * " " * string(round(sum(getindex.(relFlow_arr, 3)), digits = digVal))
				end
			end
		end
	
		fullData_arr = [attr(link = link_obj, node = attr(label = nodeLabel_arr, color = nodeColor_arr))]
	
		# pushes dictionary to overall array
		label_str = string("<b>", join(map(y -> anyM.sets[Symbol(split(String(y), "_")[1])].nodes[drop[y]].val, intersect(namesSym(data_df), dropDim_arr)), ", "), "</b>")
		
		push!(dropData_arr, attr(args = fullData_arr, label = label_str, method = "restyle"))
	
		#endregion
	end
	#endregion
	
	#region # * create various dictionaries to define format and create plot
	
	data_obj = sankey(type = "sankey", orientation = "h", valueformat =".0f", hoverinfo = "value", textfont_size = fontSize, textfont_color = "black")
	menues_obj = attr(buttons = dropData_arr, direction = "down", pad_l = 10, pad_t = 10, font_size = 16, font_family = "Arial", textfont_color = "black", showactive = true, x = 0.01, xanchor = "center", y = 1.1, yanchor = "middle")
	layout_obj = Layout(;updatemenus = [menues_obj], font_size = 32, font_family = "Arial")
	
	savefig(plot(data_obj, layout_obj), "$(anyM.options.outDir)/energyFlowSankey_$(join(string.(dropDown), "_"))$(name == "" ? "" : "_" * name)_$(anyM.options.outStamp).html")
    
	#endregion

end

# ! define postions of nodes in energy flow graph
# function is mostly taken from [GraphPlot.jl](https://github.com/JuliaGraphs/GraphPlot.jl), who again reference the following source [IainNZ](https://github.com/IainNZ)'s [GraphLayout.jl](https://github.com/IainNZ/GraphLayout.jl)
function flowLayout(nodesCnt_int::Int, edges_smat::SparseMatrixCSC{Int64,Int64}, locsX_arr::Array{Float64,1} = 2*rand(nodesCnt_int).-1.0, locsY_arr::Array{Float64,1} = 2*rand(nodesCnt_int).-1.0; scaDist::Number = 0.5, maxIter::Int=5000, initTemp::Number=2.0)

    # optimal distance bewteen vertices
    k = scaDist * sqrt(4.0 / nodesCnt_int)
    k² = k * k

    # store forces and apply at end of iteration all at once
    force_x = zeros(nodesCnt_int)
    force_y = zeros(nodesCnt_int)

    # iterate maxIter times
    @inbounds for iter = 1:maxIter
        # Calculate forces
        for i = 1:nodesCnt_int
            force_vec_x = 0.0
            force_vec_y = 0.0
            for j = 1:nodesCnt_int
                i == j && continue
                d_x = locsX_arr[j] - locsX_arr[i]
                d_y = locsY_arr[j] - locsY_arr[i]
                dist²  = (d_x * d_x) + (d_y * d_y)
                dist = sqrt(dist²)

                if !( iszero(edges_smat[i, j]) && iszero(edges_smat[j, i]) )
                    # Attractive + repulsive force
                    # F_d = dist² / k - k² / dist # original FR algorithm
                    F_d = dist / k - k² / dist²
                else
                    # Just repulsive
                    # F_d = -k² / dist  # original FR algorithm
                    F_d = -k² / dist²
                end
                force_vec_x += F_d*d_x
                force_vec_y += F_d*d_y
            end
            force_x[i] = force_vec_x
            force_y[i] = force_vec_y
        end
        # Cool down
        temp = initTemp / iter
        # Now apply them, but limit to temperature
        for i = 1:nodesCnt_int
            fx = force_x[i]
            fy = force_y[i]
            force_mag  = sqrt((fx * fx) + (fy * fy))
            scale      = min(force_mag, temp) / force_mag
            locsX_arr[i] += force_x[i] * scale
            locsY_arr[i] += force_y[i] * scale
        end
    end

    # Scale to unit square
    min_x, max_x = minimum(locsX_arr), maximum(locsX_arr)
    min_y, max_y = minimum(locsY_arr), maximum(locsY_arr)
    function scaler(z, a, b)
        2.0*((z - a)/(b - a)) - 1.0
    end
    map!(z -> scaler(z, min_x, max_x), locsX_arr, locsX_arr)
    map!(z -> scaler(z, min_y, max_y), locsY_arr, locsY_arr)

    # converts positions into dictionary
    pos_dic = Dict(z => [locsX_arr[z], locsY_arr[z]] for z in 1:nodesCnt_int)

    return pos_dic
end

# ! returns array of colors for input nodes, which labels can be found in label_dic
function getNodeColors(node_arr::Array{Int,1}, label_dic::Dict{Int64,String}, anyM::anyModel)
	revName_dic = collect(anyM.graInfo.names) |> (z -> Dict(Pair.(getindex.(z, 2), getindex.(z, 1))))
	col_dic = anyM.graInfo.colors

	color_arr = map(node_arr) do x
		str = label_dic[x]
		if str in keys(col_dic) # label is key in color dictionary
			return col_dic[str]
		elseif str in keys(anyM.graInfo.names) && anyM.graInfo.names[str] in keys(col_dic) # internal name is key in dictionary, but label was external
			return col_dic[anyM.graInfo.names[str]]
		elseif str in keys(revName_dic) && revName_dic[str] in keys(col_dic)  # external name is key in dictionary, but label was internal
			return col_dic[revName_dic[str]]
		else # default color
			return (0.85, 0.85, 0.85)
		end
	end


	return color_arr
end

# ! move a node after positions were created within energy flow graph
"""
```julia
moveNode!(model_object::anyModel, newPos_arr::Union{Array{Tuple{String,Array{Float64,1}},1},Tuple{String,Array{Float64,1}}})
```

Moves a node within the current layout of the node graph created with `plotNetworkGraph`. See [Energy flow](@ref).

"""
function moveNode!(anyM::anyModel, newPos_arr::Union{Array{Tuple{String,Array{Float64,1}},1},Tuple{String,Array{Float64,1}}})

    flowGrap_obj = anyM.graInfo.graph

    if !isdefined(flowGrap_obj, :nodePos)
        error("Initial positions are not yet defined. Run 'plotNetworkGraph' first.")
    end

    # gets assignment between node ids and names
    edges_arr =  vcat(collect.(flowGrap_obj.edgeC), collect.(flowGrap_obj.edgeTe))

    cToId_arr = map(x -> anyM.sets[:C].nodes[x[1]].val => x[2], filter(y -> y[2] in union(edges_arr...), collect(flowGrap_obj.nodeC)))
    teToId_arr  = map(x -> anyM.sets[:Te].nodes[x[1]].val => x[2], filter(y -> y[2] in union(edges_arr...), collect(flowGrap_obj.nodeTe)))
    nameToId_dic = Dict(vcat(teToId_arr, cToId_arr))

    # if input is just a single tuple and not an array convert to array
    if typeof(newPos_arr) == Tuple{String,Array{Float64,1}}
        newPos_arr = [newPos_arr]
    end

    switchNames_dic = Dict(map(x -> x[2] => x[1], collect(anyM.graInfo.names)))

    # loops overa array of moved notes
	plotSize_tup = flowGrap_obj.plotSize
    for newPos in newPos_arr
        # get id of node depending on whether it is an orignial name or name just used in plot
        if newPos[1] in keys(nameToId_dic)
            x = nameToId_dic[newPos[1]]
        elseif newPos[1] in values(anyM.graInfo.names)
            x = nameToId_dic[switchNames_dic[newPos[1]]]
        else
            error("Node name not recognized!")
        end
        # actually adjust node position

        flowGrap_obj.nodePos[x] = [flowGrap_obj.nodePos[x][1] + newPos[2][1]*2 * plotSize_tup[1]/plotSize_tup[2], flowGrap_obj.nodePos[x][2] + newPos[2][2]*2]
    end
end


# ! convert energy flow graph in yaml file to gexf
"""
```julia
convertYML2GEXF(yamlFile::String)
```
"""
function convertYML2GEXF(yamlFile::String)

    # ! define default strings
    header_str = "<?xml version='1.0' encoding='utf-8'?>
    <gexf version=\"1.2\" xmlns=\"http://www.gexf.net/1.2draft\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd\">
    <meta lastmodifieddate=\"2022-01-18\">
        <creator>NetworkX 2.4</creator>
        </meta>
        <graph defaultedgetype=\"directed\" mode=\"static\" name=\"\">
        <nodes>
    "

    mid_str = "</nodes>
    <edges>
    "

    bot_str = "</edges>
    </graph>
    </gexf>
    "

    # ! write info on nodes and vertices

    graph_dic = YAML.load_file(yamlFile)

    node_arr = map(x -> "<node id=\"" * x["name"] *  "\" label=\"" * x["label"] * "\" /> \n", collect(values(graph_dic["vertices"])))
    edge_arr = map(x -> "<edge id=\"" * string(x[1]) * "\" source=\"" * string(collect(keys(x[2]))[1]) * "\" target=\"" * collect(values(x[2]))[1] * "\" /> \n", enumerate(collect(values(graph_dic["edges"]))))

    text_str =header_str * string(node_arr...) * mid_str * string(edge_arr...) * bot_str

    write(replace(yamlFile, "yml" => "gexf"), text_str)

end

# ! dummy function just do provide a docstring for printIIS (docstring in printIIS wont work, because read-in is conditional)
"""
```julia
printIIS(model_object::anyModel)
```

Uses Gurobi's computeIIS function to determine the constraints of the optimization problem that cause infeasibility.
"""
# ! checks termination status and computes and prints IIS if infeasible
checkIIS(mod_m::anyModel) = if termination_status(mod_m.optModel) in (MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED) && isdefined(AnyMOD, :printIIS) printIIS(mod_m) end