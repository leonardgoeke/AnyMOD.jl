
#region # * reporting of calculation progress and error handling

# ! return elapsed time since Start_dates
function getElapsed(start::DateTime)
    elapSec_per = Dates.value(floor(now() - start, Dates.Second(1)))
    if elapSec_per < 3600*24
        elap_str = Dates.format(DateTime(2015, 01, 01, Int(floor(elapSec_per / 3600)), Int(floor(elapSec_per % 3600/ 60)), elapSec_per % 60), "HH:MM:SS")
    else
        elap_str = Dates.format(DateTime(2015, 01, Int(floor(elapSec_per / (3600*24))), Int(floor(elapSec_per % (3600*24) / 3600)), Int(floor(elapSec_per % 3600/ 60)), elapSec_per % 60), "dd:HH:MM:SS")
    end
    return elap_str
end

# ! teste for errors so far and optional writes report file, even if no serious errrors occured yet
function errorTest(report::Array{Tuple,1}, options::modOptions;write::Bool = false, inCode::Bool = false)
    errStatus_dic = Dict(1 => :green, 2 => :yellow, 3 => :red)
    if any(getindex.(report, 1) .== 3)
		output_df = DataFrame(map(idx -> getindex.(report, idx), eachindex(first(report))), [:type, :section, :location, :message])
        CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(output_df[!,2:end], 1, :errStatus => map(x -> errStatus_dic[x], output_df[!,:type])))
		printstyled("$(inCode ? "" : " - " )Errors encountered! Wrote reporting_$(options.outStamp).csv for details!"; color = :light_red)
        error()
    else
		numWarn = length(findall(getindex.(report, 1) .== 2))
        if write && length(report) > 0
			output_df = DataFrame(map(idx -> getindex.(report, idx), eachindex(first(report))), [:type, :section, :location, :message])
            CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(output_df[!,2:end], 1, :errStatus => map(x -> errStatus_dic[x], output_df[!,:type])))
			printstyled("$(inCode ? "" : " - " )No errors and $numWarn warning(s) encountered. Wrote reporting_$(options.outStamp).csv for details! \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        else
			printstyled("$(inCode ? "" : " - " )No errors and $numWarn warning(s) encountered. \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        end
    end
end

# ! produces a output message and tests for errors accordingly to globally set reporting values
function produceMessage(options::modOptions, report::Array{Tuple,1}, currentLvl::Int64, fixedString::String, dynamicString::Any="";testErr::Bool = false, printErr::Bool = true)
	sty_dic = Dict(1 => :bold, 2 => :normal, 3 => :light_black)

	sty_dic[currentLvl]
    if options.reportLvl >= currentLvl
		if printErr && (options.errCheckLvl >= currentLvl || testErr)
			printstyled(options.objName; color = :underline); printstyled(" ", getElapsed(options.startTime), fixedString, dynamicString; color = sty_dic[currentLvl])
		else
			printstyled(options.objName; color = :underline); printstyled(" ", getElapsed(options.startTime), fixedString, dynamicString, "\n"; color = sty_dic[currentLvl])
		end
	end
    if printErr && (options.errCheckLvl >= currentLvl || testErr) errorTest(unique(report), options, write = options.errWrtLvl >= currentLvl) end
end

# ! abbrevation to produceMessage
function produceMessageShort(mes_str::String, report_m::anyModel; testErr=false, printErr=false) produceMessage(report_m.options, report_m.report, 1, mes_str;testErr=testErr, printErr=printErr) end

#endregion

#region # * miscellaneous data processing

# ! converts color string to rgb array
convertCol(col_str::String) = split(col_str, ", ") |> (u -> parse.(Float64, [u[1][5:end], u[2], u[3][1:end-1]])./255)

# ! merges elements of named tuples for capacity preparation
mergePrep(in_tup::NamedTuple) = isempty(in_tup.resi) ? in_tup.var : unique(vcat(in_tup.var, select(in_tup.resi, Not([:var]))))

# ! aggregates all columns in array of dataframe to first column in array
aggCol!(col_df::DataFrame, col_arr::Array{Symbol,1}) = foreach(x -> add_to_expression!.(col_df[!,col_arr[1]], col_df[!,x]), col_arr[2:end])

# ! removes column relating to system (technology or exchange)
deSelectSys(in_df::DataFrame) = select(in_df, Not([:Te in namesSym(in_df) ? :Te : :Exc]))

# ! removes entry from dictionary if it is empty 
removeEmptyDic!(rmvDic::Dict, keySys::Symbol) = if isempty(rmvDic[keySys]) delete!(rmvDic, keySys) end

# ! new plus function to avoid error when one element being added up is nothing
plus(a::Int, b::Int) = a + b
plus(a::Int, b::Nothing) = a
plus(a::Nothing, b::Int) = b

# ! creates array of string from typical input of array
makeC(in::Union{String, String1, String3, String7, String15, String31, String63, String127, String255}) = split(replace(in, " " => ""), ";")

# ! provides names of columns as array of symbols ('names' function itself was changed from strings to symbols)
namesSym(df::DataFrame) = map(x -> Symbol(x), names(df))
namesSym(df::DataFrameRow) = map(x -> Symbol(x), names(df))

# ! to add an "and $nameOfScenario" or nothing to a reporting line
getScrName(id::Int, scr_tree::Tree) = id != 0 ? " and scenario '$(createFullString(id, scr_tree))'" : ""

# ! returns dataframe columns without value column
removeVal(input_df::DataFrame) = filter(x -> !(x in (:val, :ratio)), namesSym(input_df))
removeVal(col_arr::Array{Symbol,1}) = filter(x -> !(x in (:val, :ratio)), col_arr)

# ! return an empty integer array instead of an error, if a key is not in a dictionary
getDicEmpty(dic::Dict, key::Any) = key in keys(dic) ? dic[key] : Int[]

# ! get names of column of type integer
intCol(in_df::DataFrame) = getindex.(filter(x -> eltype(x[2]) <: Int, collect(pairs(eachcol(in_df)))), 1)
intCol(in_df::DataFrame, add_sym::Symbol) = union(intCol(in_df), intersect(namesSym(in_df), [add_sym]))
intCol(in_df::DataFrame, add_sym::Array) = union(intCol(in_df), intersect(namesSym(in_df), add_sym))

# ! returns the number of different capacity groups of storage from named tuple of carriers
countStGrp(carGrp_ntup::NamedTuple) = intersect((:stExtIn, :stExtOut, :stIntIn, :stIntOut), collect(keys(carGrp_ntup))) |> (z ->  isempty(z) ? 0 : maximum(map(x -> length(getfield(carGrp_ntup, x)), z)))

# ! puts relevant dimensions in consistent order and adds remaining entries at the end
orderDim(inDim_arr::Array{Symbol,1}, intCol_arr::Array{Symbol,1}) = intersect([:Ts, :Ts_exp, :Ts_retro, :Ts_expSup, :Ts_disSup_last, :Ts_expSup_i, :Ts_expSup_j, :Ts_expSup_a, :Ts_expSup_b, :Ts_disSup, :Ts_frs, :Ts_dis, :R, :R_exp, :R_exp_i, :R_exp_j, :R_exp_from, :R_exp_to, :R_dis, :R_from, :R_to, :R_from_i, :R_to_i, :R_from_j, :R_to_j, :C, :Te, :Te_i, :Te_j, :Exc, :Exc_i, :Exc_j, :M, :scr, :variable, :value], intersect(inDim_arr, intCol_arr)) |> (x -> [x..., setdiff(inDim_arr, x)...])
orderDim(inDim_arr::Array{Symbol,1}) = intersect([:Ts, :Ts_exp, :Ts_retro, :Ts_expSup, :Ts_disSup_last, :Ts_expSup_i, :Ts_expSup_j, :Ts_expSup_a, :Ts_expSup_b, :Ts_disSup, :Ts_frs, :Ts_dis, :R, :R_exp, :R_exp_i, :R_exp_j, :R_exp_from, :R_exp_to, :R_dis, :R_from, :R_to, :R_from_i, :R_to_i, :R_from_j, :R_to_j, :C, :Te, :Te_i, :Te_j, :Exc, :Exc_i, :Exc_j, :M, :scr, :variable, :value], inDim_arr) |> (x -> [x..., setdiff(inDim_arr, x)...])

# ! puts dataframes columns in consistent order
orderDf(in_df::DataFrame) = select(in_df, orderDim(namesSym(in_df), intCol(in_df) |> (z -> isempty(z) ? Symbol[] : z)))

# ! writes all tuples occuring in a tuple of pairs and tuples
mixedTupToTup(x) = typeof(x) <: Pair ? map(y -> mixedTupToTup(y), collect(x)) :  x

# ! check if dataframe should be considered, if energy balance is created for carriers in array
filterCarrier(var_df::DataFrame, c_arr::Array{Int,1}) = :C in namesSym(var_df) ? filter(r -> r.C in c_arr, var_df) : var_df

# ! use string to get id of node itself and all descendants (only works for nodes with unique name!!) 
getDescFromName(name::Symbol, tree_obj::Tree) = lookupString(string(name), tree_obj) |> (x -> vcat([x], getDescendants(x, tree_obj, true)))

# ! makes first letter of string or symbol capital or non-capital
makeUp(in::String) = isempty(in) ? "" : string(uppercase(in[1]), in[2:end])
makeUp(in::Symbol) = Symbol(uppercase(string(in)[1]), string(in)[2:end])
makeLow(in::String) = isempty(in) ? "" : string(lowercase(in[1]), in[2:end])
makeLow(in::Symbol) = Symbol(lowercase(string(in)[1]), string(in)[2:end])

# ! compute expected value
function computeExpVal(in_df::DataFrame, scrProb_dic::Dict{Tuple{Int64, Int64}, Float64}, ts_tree::Tree, frsLvl_int::Int64, aggCol_sym::Symbol)
            
	if :scr in namesSym(in_df) && unique(in_df[!,:scr]) != [0] && !isempty(in_df)
		frs_boo = :Ts_frs in namesSym(in_df)
		# rename column for aggregation
		in_df = rename(in_df, aggCol_sym => :agg)
		# join probability
		if frsLvl_int == 0 # case of perfect foresight
			in_df[!,:prob] = map(x -> (getindex(x, frs_boo ? :Ts_frs : :Ts_disSup), x.scr) in keys(scrProb_dic) ? getScrProb(getindex(x, frs_boo ? :Ts_frs : :Ts_disSup), x.scr, scrProb_dic) : 0.0, eachrow(in_df))
		else # case of limited foresight
			in_df[!,:prob] = map(x -> getScrProb(getAncestors(x.Ts_dis, ts_tree, :int, frsLvl_int)[end], x.scr, scrProb_dic), eachrow(in_df))
		end
		# compute expected value and convert column name back again
		in_df = vcat(select(in_df, Not([:prob])), combine(y -> (scr = 0, agg = sum(y.agg .* y.prob),), groupby(in_df, filter(x -> x != :scr, intCol(in_df)))))
		in_df = rename(in_df, :agg => aggCol_sym)
	end

	return in_df
end

# ! compute scenario probability for time-step/scenario combination (average across all time-steps in case of 0)
function getScrProb(ts_int::Int, scr_int::Int, scrProb_dic::Dict{Tuple{Int64, Int64}, Float64})
	if ts_int != 0
		return scrProb_dic[(ts_int, scr_int)]
	else
		return unique(getindex.(collect(keys(scrProb_dic)),1)) |> (x -> sum(map(y -> scrProb_dic[(y, scr_int)],x))/length(x))
	end
end

# ! assigns array of relevant foresight steps
function getTsFrs(ts_arr::Array{Int64, 1}, ts_tr::Tree, frsLvl_int::Int)

    if frsLvl_int != 0
        tsLvl_dic = Dict(x => ts_tr.nodes[x].lvl |> (y -> y == frsLvl_int ? [x] : (x < frsLvl_int ? getDescendants(x, ts_tr, false, frsLvl_int) : [getAncestors(x, ts_tr, :int, frsLvl_int)[end]])) for x in unique(ts_arr))
		frsTs_arr = map(x -> tsLvl_dic[x], ts_arr)
	else
		frsTs_arr = fill([0], length(ts_arr))
	end
	return frsTs_arr
end

# ! create dataframe with all potential dimensions for carrier provided
function createPotDisp(c_arr::Array{Int,1}, ts_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, anyM::anyModel)

	lvl_arr = map(x -> anyM.cInfo[x], c_arr) |> (y -> map(z -> getfield.(y, z), [:tsDis, :rDis]))
	if anyM.options.createVI.bal lvl_arr[2] .= 0 end
	allLvl_df = DataFrame(C = c_arr, lvlTs = lvl_arr[1], lvlR = lvl_arr[2])
	rDis_dic = Dict(x => getfield.(getNodesLvl(anyM.sets[:R], x), :idx) for x in unique(lvl_arr[2]))

	# write superordinate and dispatch timesteps
	allLvl_df[!,:Ts_disSup] .= fill(collect(anyM.supTs.step), size(allLvl_df, 1))
	allLvl_df = flatten(allLvl_df, :Ts_disSup)
	allLvl_df[!,:Ts_dis] = map(x -> ts_dic[x.Ts_disSup, x.lvlTs], eachrow(allLvl_df))
	
	# write regions 
	allLvl_df[!,:R_dis] = map(x -> rDis_dic[x], allLvl_df[!,:lvlR])

	# flatten to full table and add scenarios
	var_df = flatten(flatten(select(allLvl_df, Not([:lvlTs, :lvlR])), :Ts_dis), :R_dis)
	var_df = addScenarios(var_df, anyM.sets[:Ts], anyM.scr)

	# add column for superordinate dispatch timestep
	supTs_dic =  Dict(x => getAncestors(x, anyM.sets[:Ts], :int, anyM.supTs.lvl)[end] for x in unique(var_df[!,:Ts_dis]))
	var_df[!,:Ts_disSup] = map(x -> supTs_dic[x], var_df[!,:Ts_dis])

	return var_df
end

# ! gets system (technology or exchange) name as symbol from id and the other way around
sysSym(sInt::Int, sym_tree::Tree) = Symbol(getUniName(sInt, sym_tree)[end])
sysInt(sSym::Symbol, sym_tree::Tree) = filter(x -> x.val == string(sSym), collect(values(sym_tree.nodes)))[1].idx

#endregion

#region # * data frame based manipulations

# ! finds entries where expansion or capacity would be fixed to zero
function getFix(src_df::DataFrame, par_obj::ParElement, anyM::anyModel)
	if isdefined(par_obj, :name)
		# copies parameter obj and adds ":up" to inheritance for any dimensions, otherwise variables would be created, but fixed to zero due to a zero limit on a higher level in the tree
		modPar_obj = par_obj
		modPar_obj.herit = modPar_obj.herit |> (y -> tuple(vcat(y..., map(x -> x => :up, getindex.(y, 1))...)...))
		# filter zero cases
		zero_df =  matchSetParameter(src_df, modPar_obj, anyM.sets)
	else
		zero_df = src_df[[],:]
	end
	return zero_df
end

# ! removes all entries occuring in remove array from input table
function removeEntries(remove_arr::Array{DataFrame,1}, input_df::DataFrame)
    if !isempty(remove_arr)
        remove_df = length(remove_arr) == 1 ? remove_arr[1] : vcat(remove_arr...)
        colRemove_arr = namesSym(remove_df)
		out_df = antijoin(input_df, remove_df; on = colRemove_arr)
		return out_df
    else
        return input_df
    end
end

# ! merge provided dataframe into prep_dic
function mergePrepDic!(key_sym::Symbol, prep_dic::Dict{Symbol,NamedTuple}, capaResi_df::DataFrame)
	if key_sym in keys(prep_dic)
		prep_dic[key_sym]= (var = prep_dic[key_sym].var, resi = capaResi_df)
	else
		prep_dic[key_sym] = (var = intCol(capaResi_df) |> (x -> DataFrame(Pair.(x, fill(Int[], length(x))))), resi = capaResi_df)
	end
end

# ! performs a left or outer join operation and replaces any missing values
function joinMissing(leftData_df::DataFrame, rightData_df::DataFrame, key_arr::Union{Array{Symbol,1}, Array{Pair{Symbol,Symbol},1}}, how_sym::Symbol, missVal_dic::Dict, uni_boo::Bool = false)

	# perform join operation
	if how_sym == :left
		joinData_df = leftjoin(leftData_df, rightData_df; on = key_arr, makeunique = uni_boo)
	elseif how_sym == :outer
		joinData_df = outerjoin(leftData_df, rightData_df; on = key_arr, makeunique = uni_boo)
	end

	miss_col = filter(x -> any(ismissing.(x[2])), collect(pairs(eachcol(joinData_df))))
    # check, if any column contains missing values
    if isempty(miss_col) return dropmissing(joinData_df) end

    # replace missing value, cases differ depending if data type needs to be adjusted
    for col in miss_col
        joinData_df[!,col[1]] = missVal_dic[col[1]] |> (z -> typeof(z) == GenericAffExpr{Float64,VariableRef} ? map(x -> coalesce(x, copy(z)), col[2]) : map(x -> coalesce(x, z), col[2]))
    end

    return dropmissing(joinData_df)
end

# ! get array of factors converting variables in power units to energy
getEnergyFac(ts_arr::Array{Int,1}, supDis::NamedTuple) = map(x -> supDis.sca[x], ts_arr)

# ! get scaling factor for corresponding foresight period
getEnergyFacFrs(x_int::Int, anyM::anyModel) = anyM.supTs.sca[getAncestors(x_int, anyM.sets[:Ts], :int, anyM.scr.frsLvl)[end]]

# ! get scaling factor specifically for storage (needs correction depending on level of cycling and representative period)
function getEnergyFacSt(tsDis_arr::Array{Int,1}, tsSupDis_arr::Array{Int,1}, repCyc_boo::Bool, supTs_ntup::NamedTuple{(:lvl, :step, :sca, :redFac),Tuple{Int64,Tuple,Dict{Int64,Float64},Dict{Int64, Float64}}})
	# get scaling factor to convert to energy units
	sca_arr = getEnergyFac(tsDis_arr, supTs_ntup)

	# corrects scaling factor in case of storage only within the representative period
	if repCyc_boo sca_arr = sca_arr ./ map(x -> supTs_ntup.redFac[x], tsSupDis_arr) end

	return sca_arr
end

# ! gets the upper bound used for dispatch variables
function getUpBound(in_df::DataFrame, dispBound_fl::Float64, supTs::NamedTuple, treeTs::Tree)
	if !isnan(dispBound_fl)
		upBound_arr = dispBound_fl ./ getEnergyFac(in_df[!, :Ts_dis], supTs)
	else
		upBound_arr = fill(NaN, size(in_df, 1))
	end
	return upBound_arr
end

#endregion

#region # * functions and sub-functions to aggregate variables

# ! aggregates variables in aggEtr_df to rows in srcEtr_df, function used, if all entries of search have the same resolution (all entries in a relevant column are on the same level)
function aggUniVar(aggEtr_df::DataFrame, srcEtr_df::DataFrame, agg_arr::Array{Symbol,1}, srcRes_tup::NamedTuple, sets_dic::Dict{Symbol,Tree}, avg::Bool = false)
	if isempty(aggEtr_df) return map(x -> AffExpr(), 1:size(srcEtr_df, 1)) end

	# only selects relevant columns
    aggEtr_df = select(aggEtr_df, vcat(:var, agg_arr...))
	srcEtr_df = select(srcEtr_df, agg_arr)

	# adjusts entries in aggregation dataframe to comply with resolution of search dataframe
	for dim in intersect(keys(srcRes_tup), agg_arr)
		set_sym = Symbol(split(string(dim), "_")[1])
		dim_dic = Dict(x => getAncestors(x, sets_dic[set_sym], :int, getfield(srcRes_tup, dim))[end] for x in unique(aggEtr_df[!,dim]))
		aggEtr_df[!,dim] .= map(x -> dim_dic[x], aggEtr_df[!,dim])
	end

	aggEtrGrp_df = combine(groupby(aggEtr_df, agg_arr), :var => (x -> sum(x) / (avg ? length(x) : 1)) => :var)
	joined_df = joinMissing(srcEtr_df, aggEtrGrp_df, agg_arr, :left, Dict(:var => AffExpr()))
	sort!(joined_df, orderDim(intCol(joined_df)))
	
	return joined_df[!,:var]
end

# ! aggregates variables in aggEtr_df to rows in srcEtr_df, function used, if entries of search can have different resolutions (not all entries in a relevant column are on the same level)
function aggDivVar(aggEtr_df::DataFrame, srcEtr_df::DataFrame, agg_tup::Tuple, sets_dic::Dict{Symbol,Tree}; aggFilt::Tuple = ())

	aff_boo = :var in namesSym(aggEtr_df) # detects if values (meaning Float types) or variables (meaning AffExpr types are aggregated)
	agg_sym = aff_boo ? :var : :val

	# ! sanity checks regarding columns
	if all(namesSym(aggEtr_df) |> (y -> map(x -> !(x in y), agg_tup))) error("tried to perform aggregation on column not existing in dataframe to be aggregated") end
	if all(namesSym(srcEtr_df) |> (y -> map(x -> !(x in y), agg_tup))) error("tried to perform aggregation on column not existing in dataframe to aggregate") end

	select!(aggEtr_df, intCol(aggEtr_df, agg_sym))
	# ! filter entries from aggEtr_df, that based on isolated analysis of columns will not be aggregated
	for dim in intersect(aggFilt, agg_tup)
		set_sym = Symbol(split(string(dim), "_")[1])
		allSrc_set = unique(srcEtr_df[!,dim]) |> (z -> union(BitSet(z), map(x -> BitSet(dim == :id ? Int[] : getDescendants(x, sets_dic[set_sym], true)), z)...))
		aggEtr_df = aggEtr_df[findall(map(x -> (x in allSrc_set), aggEtr_df[!,dim])), :]
	end

	if isempty(aggEtr_df)
		out_arr = aff_boo ? map(x -> AffExpr(), 1:size(srcEtr_df, 1)) : fill(0.0, size(srcEtr_df, 1)) 
	else
		# ! filter entries from srcEtr_df, that based on isolated anlysis of columns will not have any values aggregated to
		idxRel_set = BitSet(1:size(srcEtr_df, 1))
		for dim in agg_tup
			set_sym = Symbol(split(string(dim), "_")[1])
			allAgg_set = unique(aggEtr_df[!,dim]) |> (z -> union(BitSet(z), map(y -> BitSet(getAncestors(y, sets_dic[set_sym], :int, 0)), z)...))
			idxRel_set = intersect(idxRel_set, BitSet(findall(map(x -> x in allAgg_set, srcEtr_df[!,dim]))))
		end
		srcEtrAct_df = srcEtr_df[collect(idxRel_set),:]
		# group aggregation dataframe to relevant columns and removes unrequired columns
		aggEtrGrp_df = combine(groupby(aggEtr_df, collect(agg_tup)), agg_sym => (x -> sum(x)) => agg_sym)

		# ! create dictionaries in each dimension that assign rows suited for aggregation for each value
		chldRows = Dict{Symbol,Dict{Int,BitSet}}()
		for col in agg_tup
			# row that are potentially aggregated
			findCol_arr = aggEtrGrp_df[!,col]
			findCol_set = BitSet(findCol_arr)

			# entries that other entries can be aggregated to
			searchVal_set = BitSet(unique(srcEtrAct_df[!,col]))

			# to every unique value in column the value itself and its children are assigned
			set_sym = Symbol(split(string(col), "_")[1])
			idxChild_dic = Dict(x => intersect(findCol_set, [x, getDescendants(x, sets_dic[set_sym], true)...]) for x in searchVal_set)

			# for each unique value in column the rows with children are assigned
			grp_df = groupby(DataFrame(val = findCol_arr, id = 1:length(findCol_arr)), :val)
			dicVal_dic = Dict(x.val[1] => BitSet(sort(x[!,:id])) for x in grp_df) |> (dic -> Dict(x => union(map(y -> dic[y], collect(idxChild_dic[x]))...) for x in keys(idxChild_dic)))
			# excludes column from search, if based on it, every entry in find could be aggregated to every row in search
			# (if this holds true for all columns, make an exception for the last one and dont remove it to, because otherwise correct aggregation cannot happen )
			if all(length.(values(dicVal_dic)) .== length(findCol_arr)) && !(col == agg_tup[end] && length(chldRows) < 1)
				select!(srcEtrAct_df, Not(col)); continue
			else
				chldRows[col] = dicVal_dic
			end
		end

		# ! finds aggregation by intersecting suited rows in each dimension
		if isempty(chldRows)
			aggRow_arr = fill(BitSet(), size(srcEtrAct_df, 1))
		else
			aggRow_arr = collectKeys(keys(chldRows)) |> (y -> map(x -> intersect(map(y -> chldRows[y][x[y]], y)...) , eachrow(srcEtrAct_df)))
		end

		# ! aggregates values according to lookup
		out_arr = aff_boo ? Array{AffExpr}(undef, size(srcEtr_df, 1)) : Array{Float64}(undef, size(srcEtr_df, 1))
		out_arr[collect(idxRel_set)] =  map(x -> sum(aggEtrGrp_df[x,agg_sym]), collect.(aggRow_arr))
		noData_arr = setdiff(1:size(srcEtr_df, 1), idxRel_set)
		# adds an empty affine expression or zero where no values are assigned
		if aff_boo
			out_arr[noData_arr] = map(x -> AffExpr(), 1:length(noData_arr))
		else
			out_arr[noData_arr] .=  0.0
		end
	end

	return out_arr
end

#endregion

#region # * manipulate model related data frames

# ! add superordinate dispatch timestep to expansion dataframe
function addSupTsToExp(expMap_df::DataFrame, part::AbstractModelPart, type_sym::Symbol, tsYear_dic::Dict{Int,Int}, anyM::anyModel)
	if !isempty(expMap_df)
		# add lifetime to table
		if :Exc in namesSym(expMap_df)
			lftm_df = rename(matchExcParameter(Symbol(:life, type_sym), flatten(expMap_df, :Ts_expSup), part, anyM.sets, part.dir), :val => :life)
		else
			lftm_df = matchSetParameter(flatten(expMap_df, :Ts_expSup), part.par[Symbol(:life, type_sym)], anyM.sets, newCol = :life)
		end
		# add construction delay to table
		if Symbol(:del, type_sym) in keys(part.par) || Symbol(:del, type_sym, :Dir) in keys(part.par)
			if :Exc in namesSym(expMap_df) 
				lftmDel_df = rename(matchExcParameter(Symbol(:del, type_sym), lftm_df, part, anyM.sets, part.dir), :val => :del)
			else
				lftmDel_df = matchSetParameter(lftm_df, part.par[Symbol(:del, type_sym)], anyM.sets, newCol = :del)
			end
		else
			lftmDel_df = lftm_df; lftmDel_df[!,:del] .= 0.0
		end
		lftmDel_df[!,:Ts_disSup] = map(x -> filter(y -> (tsYear_dic[y] >= tsYear_dic[x.Ts_expSup] + x.del) && (tsYear_dic[y] < tsYear_dic[x.Ts_expSup] + x.life + x.del), collect(anyM.supTs.step)), eachrow(lftmDel_df))
		select!(lftmDel_df, Not([:life, :del]))
		grpCol_arr = intCol(expMap_df) |> (x -> :ratio in namesSym(expMap_df) ? vcat(:ratio, x...) : x)
		expMap_df = combine(groupby(lftmDel_df, grpCol_arr), [:Ts_expSup, :Ts_disSup] .=> (x -> [x]) .=> [:Ts_expSup, :Ts_disSup])
	else
		expMap_df[!,:Ts_disSup] = Array{Array{Int,1},1}()
	end
	return expMap_df
end

# ! expand expansion dataframe to capacity dataframe
function expandExpToCapa(in_df::DataFrame)

	noExpCol_arr = intCol(in_df, :dir)

	allDf_arr = map(eachrow(in_df)) do x
		l_arr = length.(x.Ts_disSup)
		rem_df = repeat(DataFrame(x[noExpCol_arr]), inner = sum(l_arr), outer = 1)
		ext_df = DataFrame(Ts_expSup = vcat(map(y -> fill(x.Ts_expSup[y], l_arr[y]), 1:length(l_arr))...), Ts_disSup = vcat(x.Ts_disSup...))
		return hcat(rem_df, ext_df)
	end

	if !isempty(allDf_arr)
		capa_df = select(vcat(allDf_arr...), orderDim(namesSym(allDf_arr[1])))[!,Not(:Ts_exp)]
	else
		capa_df = select(in_df, Not(:Ts_exp)); capa_df[!,:Ts_disSup] = Int[];
	end

	return orderDf(capa_df)
end

# ! expands any table including columns with temporal and spatial dispatch levels and the corresponding expansion regions and superordinate dispatch steps to full dispatch table
function expandExpToDisp(inData_df::DataFrame, ts_dic::Dict{Tuple{Int,Int},Array{Int,1}}, r_dic::Dict{Tuple{Int64,Int64},Array{Int64,1}}, ts_tr::Tree, scr_ntup::NamedTuple, preserveTsSupTs::Bool = false, defScr_arr::Array{Int,1} = Int[])

	# adds regional timesteps and check if this causes non-unique values (because spatial expansion level can be below dispatch level)
	expR_df = unique(combine(x -> (R_dis = r_dic[(x.R_exp[1], x.lvlR[1])],), groupby(inData_df, namesSym(inData_df)))[!,Not([:R_exp,:lvlR])])
	expTs_df = combine(x -> (Ts_dis = ts_dic[(x.Ts_disSup[1], x.lvlTs[1])],), groupby(expR_df, namesSym(expR_df)))[!,Not(:lvlTs)]

	expTs_df = addScenarios(expTs_df, ts_tr, scr_ntup, defScr_arr)
	
	if !preserveTsSupTs select!(expTs_df, Not(:Ts_disSup)) end

	return expTs_df
end

# ! obtains residual capacities for technologies
function checkResiCapa(var_sym::Symbol, stockCapa_df::DataFrame, part::AbstractModelPart, anyM::anyModel, addSym::Symbol = Symbol())
	resiPar_sym = Symbol(var_sym, :Resi, addSym)
	if resiPar_sym in tuple(keys(part.par)...)
		# search for defined residual values
		stock_df = matchSetParameter(stockCapa_df, part.par[resiPar_sym], anyM.sets)
	else
		stock_df = filter(x -> false, stockCapa_df); stock_df[!,:val] = Float64[]
	end

	# convers returned value to affine expression
	stock_df[!,:var] =  AffExpr.(stock_df[!,:val])
	select!(stock_df, Not(:val))

   return stock_df
end

# ! get a dataframe with all variable of the specified type
function getAllVariables(va::Symbol, anyM::anyModel; filterFunc::Function = x -> true)

	exc_boo = occursin("exc", lowercase(string(va)))
	sys_dic = getfield(anyM.parts, exc_boo ? :exc : :tech)
	sysSym_arr = collect(keys(sys_dic))
	
	if !(va in (:crt, :lss, :trdBuy, :trdSell, :emission, :emissionInf)) && !occursin("cost", string(va)) # get all variables for systems
		va_dic = Dict(:stIn => (:stExtIn, :stIntIn), :stOut => (:stExtOut, :stIntOut), :convIn => (:use, :stIntOut), :convOut => (:gen, :stIntIn))
		sysType_arr = filter(x -> !isempty(x[2]), [(vaSpec, filter(y -> vaSpec in keys(sys_dic[y].var), sysSym_arr)) for vaSpec in (va in keys(va_dic) ? va_dic[va] : (va,))])

		if !isempty(sysType_arr)
			allVar_df = vcat(map(x -> sys_dic[x[2]].var[x[1]], vcat(map(x -> collect(zip(fill(x[1], length(x[2])), x[2])), sysType_arr)...))...)
		else
			allVar_df = DataFrame()
		end

		# aggregate variables if different types where obtained
		if !isempty(allVar_df) && va in keys(va_dic)
			allVar_df = combine(groupby(allVar_df, intCol(allVar_df)), :var => (x -> sum(x)) => :var)
		end

	elseif va in (:crt, :lss, :trdBuy, :trdSell) # get variables from balance part
		if va in keys(anyM.parts.bal.var)
			allVar_df = copy(anyM.parts.bal.var[va])
		else
			allVar_df = DataFrame()
		end
	elseif occursin("cost", string(va)) && va != :cost # get single variables from cost part
		if va in keys(anyM.parts.cost.var)
			allVar_df = copy(anyM.parts.cost.var[va])
		else
			allVar_df = DataFrame()
		end
	elseif va == :cost # get merged cost variables

		costVar_arr = collect(keys(anyM.parts.cost.var))
		costDf_arr = Array{DataFrame}(undef, length(costVar_arr))

		for (idx, va) in enumerate(costVar_arr)
			var_df = copy(anyM.parts.cost.var[va])
	
			# splits costs of exchange across regions
			if :R_from in intCol(var_df)
				var_df[!,:var] = var_df[!,:var] .* 0.5
				var_df = rename(combine(groupby(flipExc(var_df), filter(x -> x != :R_to, intCol(var_df))), :var => (x -> sum(x)) => :var), :R_from => :R_dis)
			end
			
			# adjust temporal column
			tsCol_arr = intersect([:Ts_disSup, :Ts_exp], intCol(var_df))
			if !isempty(tsCol_arr)
				var_df[!,:Ts] = map(x -> getAncestors(x, anyM.sets[:Ts], :int, anyM.supTs.lvl)[end], var_df[!, tsCol_arr[1]])
			end

			# adjust spatial column
			rCol_arr = intersect([:R_exp, :R_dis], intCol(var_df))
			if !isempty(rCol_arr)
				var_df[!,:R] = map(x -> getAncestors(x, anyM.sets[:R], :int, 1)[end], var_df[!,rCol_arr[1]])
			end

			# drop unrequired columns
			var_df = select(var_df, vcat(intersect([:Ts, :R, :Te, :Exc], intCol(var_df)), [:var]))

			var_df[!,:type] .= va
			# fill non-existing columns with zero
			foreach(x -> var_df[!,x] .= 0, setdiff([:Ts, :R, :Te, :Exc], intCol(var_df)))
			costDf_arr[idx] = orderDf(var_df)
		end

		allVar_df = vcat(costDf_arr...) |> (z -> isempty(z) ? DataFrame() : combine(groupby(z, intCol(z, :type)), :var => (x -> sum(x)) => :var))

	elseif va == :emission # for emission all use variables are obtained and then already matched with emission factors

		if !(:emissionFac in keys(anyM.parts.lim.par))
			lock(anyM.lock)
			push!(anyM.report, (2, "limits", "emissionUp", "upper emission limits but no emission factors provided"))
			unlock(anyM.lock)
			allVar_df = DataFrame()
		else
			# get all carriers and technologies that might be relevant to compute emissions
			if :Te in namesSym(anyM.parts.lim.par[:emissionFac].data)
				emTe_arr = unique(vcat(map(x -> [x, getDescendants(x, anyM.sets[:Te], true)...], unique(filter(x -> x.Te != 0, anyM.parts.lim.par[:emissionFac].data)[!,:Te]))...))
				if :C in namesSym(anyM.parts.lim.par[:emissionFac].data)
					allTe_arr = unique(filter(x -> x.Te == 0, anyM.parts.lim.par[:emissionFac].data)[!,:C])
			else
					allTe_arr = union(map(x -> anyM.parts.tech[sysSym(x, anyM.sets[:Te])].carrier |> (z -> :use in keys(z) ? [z[:use]...] : Int[]), emTe_arr)...)
				end
				emC_arr = unique(vcat(map(x -> [x, getDescendants(x, anyM.sets[:C], true)...], allTe_arr)...))
			elseif :C in namesSym(anyM.parts.lim.par[:emissionFac].data)
				# add error message
				emC_arr = unique(vcat(map(x -> [x, getDescendants(x, anyM.sets[:C], true)...], unique(anyM.parts.lim.par[:emissionFac].data[!,:C]))...))
				emTe_arr = Array{Int64,1}()
			else
				push!(anyM.report, (3, "parameter read-in", "definition", "emission factors cannot be provided without specifying a technology or carrier"))
				emC_arr = Int[]
				emTe_arr = Int[]
			end

			# get use variables from technologies
			allTechVar_df = getAllVariables(:use, anyM, filterFunc = x -> x.C in emC_arr || x.Te in emTe_arr)
			#allTechVar_df[!,:Exc] .= 0

			allVar_df = allTechVar_df
			# get use expression from exchange
			#=
			allExcVar_df = getAllVariables(:useExc, anyM, filterFunc = x -> x.C in emC_arr)
			allExcVar_df[!,:var] = allExcVar_df[!,:var] .* 0.5 # attributes energy use equally to exporting and importing region
			allExcVar_df = vcat(select(rename(allExcVar_df, :R_from => :R_dis), Not([:R_to])), select(rename(allExcVar_df, :R_to => :R_dis), Not([:R_from])))
			allExcVar_df[!,:Te] .= 0; allExcVar_df[!,:M] .= 0
	
			allVar_df = vcat(allTechVar_df, allExcVar_df)
			=#

			# add expressions for storage losses, if this is enabled
			if anyM.options.emissionLoss
				allSt_arr = map(x -> map(y -> collect(x.carrier[y]), intersect(keys(x.carrier), (:stExtIn, :stExtOut, :stIntIn, :stIntOut))), values(anyM.parts.tech))
				if !all(isempty.(allSt_arr))  
					allSt_arr = union(union(union(allSt_arr...)...)...)
					if !isempty(intersect(emC_arr, allSt_arr))
						# get all storage variables where storage losses can lead to emissions
						stVar_dic = Dict((string(st) |> (y -> Symbol(uppercase(y[1]), y[2:end]))) => getAllVariables(st, anyM, filterFunc = x -> x.C in emC_arr || x.Te in emTe_arr) for st in (:stIn, :stOut))
						stLvl_df = getAllVariables(:stLvl, anyM, filterFunc = x -> x.C in emC_arr)

						# loop over relevant storage technologies to obtain loss values
						tSt_arr = unique(stLvl_df[!,:Te])
						for tInt in tSt_arr
							part = anyM.parts.tech[sysSym(tInt, anyM.sets[:Te])]
							# add expression quantifying storage losses for storage in- and and output
							for st in keys(stVar_dic)
								stVar_df = stVar_dic[st]
								stVar_df = matchSetParameter(filter(x -> x.Te == tInt, stVar_df), part.par[Symbol(:eff, st)], anyM.sets)
								stVar_df[!,:var] = stVar_df[!,:var] .* (1 .- stVar_df[!,:val])
								select!(stVar_df, Not(:val))
								append!(allVar_df, select(stVar_df, Not([:id])))
							end

							# add expression quantifying storage losses for storage discharge
							if :stDis in keys(part.par)
								sca_arr = getEnergyFac(stLvl_df[!,:Ts_dis], anyM.supTs)
								stLvl_df = matchSetParameter(filter(x -> x.Te == tInt, stLvl_df), part.par[:stDis], anyM.sets)
								stLvl_df[!,:var] = stLvl_df[!,:var] .* (1 .- (1 .- stLvl_df[!,:val]) .^ sca_arr)
								select!(stLvl_df, Not(:val))
								append!(allVar_df, select(stLvl_df, Not([:id])))
							end
						end
					end
				end
			end

			allVar_df = matchSetParameter(allVar_df, anyM.parts.lim.par[:emissionFac], anyM.sets)
		end

		if !isempty(allVar_df)
			allVar_df[!,:var] = allVar_df[!,:val]  ./ 1e6 .* allVar_df[!,:var]
			select!(allVar_df, Not(:val))
		end
	elseif va == :emissionInf
		if va in keys(anyM.parts.lim.var)
			allVar_df = copy(anyM.parts.lim.var[va])
		else
			allVar_df = DataFrame()
		end
	end

	# prevents scaling of variables that do have to be scaled or are scaled already because they are computed form scaled variables (e.g. emissions)
	if va in (:stIn, :stExtIn, :stIntIn, :stOut, :stExtOut, :stIntOut, :convIn, :use, :gen, :convOut, :crt, :lss, :trdBuy, :trdSell, :exc) && !isempty(allVar_df)
		allVar_df[!,:var] .= allVar_df[!,:var] .* getEnergyFac(allVar_df[!,:Ts_dis], anyM.supTs) 
	end

	return orderDf(filter(filterFunc, allVar_df))
end

# ! returns array of technologies and respective dispatch variables relevant for input carrier
function getRelTech(c::Int, tech_dic::Dict{Symbol,TechPart}, c_tree::Tree)

	techSym_arr = collect(keys(tech_dic))
	relTech_arr = Array{Tuple{Symbol,Symbol},1}()
	for tSym in techSym_arr
		addTe_arr = intersect((:use, :gen, :stExtIn, :stExtOut), filter(y -> c in union(tech_dic[tSym].carrier[y]...), collect(keys(tech_dic[tSym].carrier))))
		append!(relTech_arr, collect(zip(fill(tSym, length(addTe_arr)), addTe_arr)))
	end

	return filter(x -> x[2] in keys(tech_dic[x[1]].var), relTech_arr)
end

# ! get all carriers auf input carrier tuple that relate to fields specified in second input
function getCarrierFields(car_ntup::NamedTuple, field_tup::Tuple)
    extFields_arr = intersect(keys(car_ntup), field_tup)
	allCar_arr = [getproperty(car_ntup, u) |> (p -> u in (:use, :gen) ? collect(p) : collect(p...))  for u in extFields_arr]
    return isempty(allCar_arr) ? Int[] : union(allCar_arr...)
end

# ! add demand for descendant carriers to dataframe (computes average if resolution more detailed)
function addSubDemand(in_df::DataFrame, allDim_df::DataFrame, c_int::Int, cSub_arr::Array{Int64,1}, sets_dic::Dict{Symbol, Tree}, cInfo_dic::Dict{Int64,@NamedTuple{tsDis::Int64, tsExp::Int64, rDis::Int64, rExp::Int64, balSign::Symbol, stBalCapa::Symbol}}, partBal_obj::OthPart)	
    
    for cSub in cSub_arr # demand for descendant carriers that has to be aggregated
        demSub_df = filter(x -> x.val != 0.0, matchSetParameter(filter(x -> x.C == cSub, allDim_df), partBal_obj.par[:dem], sets_dic))
        if isempty(demSub_df) continue end
        # average demand for descendant carriers
        in_df[!,:demSub] = aggUniVar(rename(demSub_df, :val => :var), select(in_df, intCol(in_df)), [:Ts_dis, :R_dis, :scr], (Ts_dis =  cInfo_dic[c_int].tsDis, R_dis = cInfo_dic[c_int].rDis, scr = 1), sets_dic, true)
        # add to overall demand and remove column again
        in_df[!,:dem] = in_df[!,:dem] .+ in_df[!,:demSub]
        select!(in_df, Not(:demSub))
    end

    return in_df
end

# ! collapse input expansion dataframe to acutal variables by timestep of expansion instead of superordinate dispatch timesteps
collapseExp(exp_df::DataFrame) = unique(select(exp_df, Not([:Ts_expSup, :Ts_disSup])))

#endregion

#region # * processing for stochastic model 

# ! adds relevant scenarios based on timestep column
function addScenarios(in_df::DataFrame, ts_tr::Tree, scr_ntup::NamedTuple, defScr_arr::Array{Int,1} = Int[])
	if isempty(scr_ntup.scr)
		in_df[!,:scr] .= 0
	else
		tsToScr_dic = Dict(x => getAncestors(x, ts_tr, :int, scr_ntup.lvl)[end] for x in in_df[!,:Ts_dis])
		if isempty(defScr_arr) filter!(x -> tsToScr_dic[x.Ts_dis] in keys(scr_ntup.scr), in_df) end
		in_df[!,:scr] = map(x -> tsToScr_dic[x] in keys(scr_ntup.scr) ? scr_ntup.scr[tsToScr_dic[x]] : defScr_arr, in_df[!,:Ts_dis])
		in_df = flatten(in_df, :scr)
	end
	return in_df
end

getScrProb(ts_int::Int, scr_int::Int, ts_tr::Tree, scr_ntup::NamedTuple) = isempty(scr_ntup.scrProb) ? 1.0 : scr_ntup.scrProb[getAncestors(ts_int, ts_tr, :int, scr_ntup.lvl)[end], scr_int]

# ! extends relevant scenarios for intersection of interdependent subperiods
function getStScr(ts::Int, syCyc_int::Int, ts_tr::Tree, scr_ntup::NamedTuple)
	# get previous time-step in storage balance for input time-step
	presTs_int = getDescendants(getAncestors(ts, ts_tr, :int, syCyc_int)[end], ts_tr, false, ts_tr.nodes[ts].lvl) |> (v -> v[findall(v .== ts)[end] |> (w -> w < length(v) ? w + 1 : 1)])
	# get relevant scenarios for current and previous time-step
	return sort(union(map(x -> scr_ntup.scr[getAncestors(x, ts_tr, :int, scr_ntup.lvl)[end]], [ts, presTs_int])...))
end

# ! compute expected value for input dataframe
function computeExpDis(in_df::DataFrame, scrProb_df::DataFrame)
	in_df = innerjoin(in_df, rename(scrProb_df, :value => :prob), on = intCol(scrProb_df))
	in_df[!,:value] = in_df[!,:value] .* in_df[!,:prob]
	select!(in_df, Not([:prob]))
	in_df = combine(x -> (scr = 0, value = sum(x.value)), groupby(in_df, filter(x -> x != :scr, intCol(in_df))))
	return in_df
end

#endregion
