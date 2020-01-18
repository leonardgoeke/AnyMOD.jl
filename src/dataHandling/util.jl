
# <editor-fold desc="reporting of calculation progress and error handling"

# XXX return elapsed time since Start_date
function getElapsed(start::DateTime)
    elapSec_per = Dates.value(floor(now() - start,Dates.Second(1)))
    if elapSec_per < 3600*24
        elap_str = Dates.format(DateTime(2015,01,01,Int(floor(elapSec_per / 3600)),Int(floor(elapSec_per % 3600/ 60)),elapSec_per % 60), "HH:MM:SS")
    else
        elap_str = Dates.format(DateTime(2015,01,Int(floor(elapSec_per / (3600*24))),Int(floor(elapSec_per % (3600*24) / 3600)),Int(floor(elapSec_per % 3600/ 60)),elapSec_per % 60), "dd:HH:MM:SS")
    end
    return elap_str
end

# XXX teste for errors so far and optional writes report file, even if no serious errrors occured yet
function errorTest(report::Array{Tuple,1},options::modOptions;write::Bool = false, inCode::Bool = false)
    errStatus_dic = Dict(1 => :green, 2 => :yellow,3 => :red)
    if any(getindex.(report,1) .== 3)
		output_df = DataFrame(type = Int[], group = String[], instance = String[],  message = String[])
		push!(output_df,report...)
        CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(output_df[!,2:end], 1, :errStatus => map(x -> errStatus_dic[x],output_df[!,:type])))
		printstyled("$(inCode ? "" : " - " )Errors encountered! Wrote reporting_$(options.outStamp).csv for details!"; color = :light_red)
        error()
    else
		numWarn = length(findall(getindex.(report,1) .== 2))
        if write && length(report) > 0
			output_df = DataFrame(type = Int[], group = String[], instance = String[],  message = String[])
			push!(output_df,report...)
            CSV.write("$(options.outDir)/reporting_$(options.outStamp).csv",  insertcols!(output_df[!,2:end], 1, :errStatus => map(x -> errStatus_dic[x],output_df[!,:type])))
			printstyled("$(inCode ? "" : " - " )No errors and $numWarn warning(s) encountered. Wrote reporting_$(options.outStamp).csv for details! \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        else
			printstyled("$(inCode ? "" : " - " )No errors and $numWarn warning(s) encountered. \n"; color = numWarn > 0 ? :light_yellow : :light_green)
        end
    end
end

# XXX produces a output message and tests for errors accordingly to globally set reporting values
function produceMessage(options::modOptions,report::Array{Tuple,1},currentLvl::Int64,fixedString::String,dynamicString::Any="")
	sty_dic = Dict(1 => :bold, 2 => :normal, 3 => :light_black)

	sty_dic[currentLvl]
    if options.reportLvl >= currentLvl
		options.errCheckLvl >= currentLvl ? printstyled(options.name, getElapsed(options.startTime), fixedString, dynamicString; color = sty_dic[currentLvl]) :
															printstyled(options.name,getElapsed(options.startTime), fixedString, dynamicString, "\n"; color = sty_dic[currentLvl])
	end
    if options.errCheckLvl >= currentLvl errorTest(report,options,write = options.errWrtLvl >= currentLvl) end
end

# </editor-fold>

# <editor-fold desc="miscellaneous data processing"

# XXX returns dataframe columns without value column
removeVal(input_df::DataFrame) = filter(x -> !(x in (:val,:ratio)),names(input_df))
removeVal(col_arr::Array{Symbol,1}) = filter(x -> !(x in (:val,:ratio))l,col_arr)

# XXX get names of column of type integer
intCol(in_df::DataFrame) = getindex.(filter(x -> eltype(x[2]) <: Int, eachcol(in_df, true)),1)
intCol(in_df::DataFrame,add_sym::Symbol) = union(intCol(in_df),intersect(names(in_df),[add_sym]))

# XXX puts relevant dimensions in consistent order and adds remaining entries at the end
orderDim(inDim_arr::Array{Symbol,1},intCol_arr::Array{Symbol,1}) = intersect([:Ts_exp, :Ts_expSup, :Ts_disSup, :Ts_dis, :R_exp, :R_dis, :R_from, :R_to, :C, :Te], intersect(inDim_arr,intCol_arr)) |> (x -> [x...,setdiff(inDim_arr,x)...])
orderDim(inDim_arr::Array{Symbol,1}) = intersect([:Ts_exp, :Ts_expSup, :Ts_disSup, :Ts_dis, :R_exp, :R_dis, :R_from, :R_to, :R_a, :R_b, :C, :Te], inDim_arr) |> (x -> [x...,setdiff(inDim_arr,x)...])

# XXX puts dataframes columns in consistent order
orderDf(in_df::DataFrame) = select(in_df,orderDim(names(in_df),intCol(in_df)))

# XXX writes all tuples occuring in a tuple of pairs and tuples
mixedTupToTup(x) = typeof(x) <: Pair ? map(y -> mixedTupToTup(y),collect(x)) :  x

# XXX check if dataframe should be considered, if energy balance is created for carriers in array
filterCarrier(var_df::DataFrame,c_arr::Array{Int,1}) = :C in names(var_df) ? filter(r -> r.C in c_arr,var_df) : var_df

# XXX creates a dictionary that assigns each dispatch timestep inputed to its supordinate dispatch timestep
function assignSupTs(inputSteps_arr::Array{Int,1},time_Tree::Tree,supordinateLvl_int::Int)

	assSup_dic = Dict{Int,Int}()

	# assigns zero node to itself
	if 0 in inputSteps_arr
		assSup_dic[0] = 0
		inputSteps_arr = filter(r -> r != 0, inputSteps_arr)
	end

	# assigns entries on or above subordinate dispatch level to itselfs
	aboveSupTs_arr = filter(z -> time_Tree.nodes[z].lvl <= supordinateLvl_int, inputSteps_arr)
	for x in aboveSupTs_arr assSup_dic[x] = x end
	inputSteps_arr = setdiff(inputSteps_arr,aboveSupTs_arr)

	# assigns remaining entries
	for x in inputSteps_arr
		assSup_dic[x] = getAncestors(x,time_Tree,supordinateLvl_int)[1][1]
	end

	return assSup_dic
end

# XXX create dataframe with all potential dimensions for carrier provided
function createPotDisp(c_arr::Array{Int,1},anyM::anyModel)

	lvl_arr = map(x -> anyM.cInfo[x], c_arr) |> (y -> map(z -> getfield.(y,z),[:tsDis, :rDis]))
	allLvl_df = DataFrame(C = c_arr, lvlTs = lvl_arr[1], lvlR = lvl_arr[2])
	tsDis_dic, rDis_dic = [Dict(x => getfield.(getNodesLvl(anyM.sets[z[2]],x),:idx) for x in unique(lvl_arr[z[1]])) for z in enumerate([:Ts,:R])]

	allLvl_df[!,:Ts_dis] = map(x -> tsDis_dic[x],allLvl_df[!,:lvlTs])
	allLvl_df[!,:R_dis] = map(x -> rDis_dic[x],allLvl_df[!,:lvlR])

	var_df = flatten(flatten(select(allLvl_df,Not([:lvlTs,:lvlR])),:Ts_dis),:R_dis)

	# add column for supordinate dispatch timestep
	supTs_dic =  Dict(x => getindex(getAncestors(x,anyM.sets[:Ts],anyM.supTs.lvl)[end],1) for x in unique(var_df[!,:Ts_dis]))
	var_df[!,:Ts_disSup] = map(x -> supTs_dic[x], var_df[!,:Ts_dis])

	return var_df
end

# </editor-fold>


# <editor-fold desc="data frame based manipulations"

# XXX finds entries where expansion or capacity would be fixed to zero
function filterZero(src_df::DataFrame,par_obj::ParElement,anyM::anyModel)
	if isdefined(par_obj,:name)
	# copies parameter obj and adds ":up" to inheritance for any dimensions, otherwise variables would be created, but fixed to zero due to a zero limit on a higher level in the tree
		modPar_obj = par_obj
		modPar_obj.inherit = modPar_obj.inherit |> (y -> tuple(vcat(y..., map(x -> x => :up,getindex.(y,1))...)...))
		# filter zero cases
		zero_df = select!(filter(r -> r.val == 0, matchSetParameter(src_df, modPar_obj, anyM.sets, anyM.report)),Not(:val))
	else
		zero_df = src_df[[],:]
	end
	return zero_df
end

# XXX removes all entries occuring in remove array from input table
function removeEntries(remove_arr::Array{DataFrame,1},input_df::DataFrame)
    if !isempty(remove_arr)
        remove_df = length(remove_arr) == 1 ? remove_arr[1] : vcat(remove_arr...)
        colRemove_arr = names(input_df)
		out_df = join(input_df,remove_df; on = colRemove_arr, kind = :anti)
		return out_df
    else
        return input_df
    end
end

# XXX merges all tables within input dictionary
function mergeDicTable(df_dic::Dict{Symbol,DataFrame},outerJoin_boo::Bool=true)
	if isempty(df_dic) return DataFrame() end
	keys_arr = collect(keys(df_dic))
	mergeTable_df = df_dic[keys_arr[1]]
	joinCol_arr = filter(x -> !(x in keys_arr), names(mergeTable_df))

	for restKey in keys_arr[2:end]
		if outerJoin_boo
			mergeTable_df = join(mergeTable_df, df_dic[restKey]; on = joinCol_tup, kind = :outer)
		else
			append!(mergeTable_df, df_dic[restKey])
		end
	end

	return mergeTable_df
end

# XXX merge provided dataframe into prep_dic
function mergePrepDic!(key_sym::Symbol,prep_dic::Dict{Symbol,NamedTuple},capaResi_df::DataFrame,capaRatio_df::DataFrame = DataFrame())
	if key_sym in keys(prep_dic)
		prep_dic[key_sym]= (var = prep_dic[key_sym].var, ratio = capaRatio_df, resi = capaResi_df)
	else
		prep_dic[key_sym] = (var = intCol(capaResi_df) |> (x -> DataFrame(Pair.(x,fill(Int[],length(x))))), ratio = capaRatio_df, resi = capaResi_df)
	end
end

# XXX performs a left or outer join operation and replaces any missing values
function joinMissing(leftData_df::DataFrame, rightData_df::DataFrame, key_arr::Union{Array{Symbol,1},Array{Pair{Symbol,Symbol},1}}, how_sym::Symbol, missVal_dic::Dict, uni_boo::Bool = false)

	# perform join operation
    joinData_df = join(leftData_df,rightData_df; on = key_arr, kind = how_sym, makeunique = uni_boo)

	miss_col = filter(x -> any(ismissing.(x[2])), eachcol(joinData_df,true))
    # check, if any column contains missing values
    if isempty(miss_col) return dropmissing(joinData_df) end

    # replace missing value, cases differ depending if data type needs to be adjusted
    for col in miss_col
        joinData_df[!,col[1]] = map(x -> coalesce(x,missVal_dic[col[1]]),col[2])
    end

    return dropmissing(joinData_df)
end

# XXX get array of scaling factors for add_df
function getScale(add_df::DataFrame,time_obj::Tree,supDis::NamedTuple{(:lvl,:step,:sca),Tuple{Int,Tuple{Vararg{Int,N} where N},Dict{Tuple{Int,Int},Float64}}})
    tsDisLvl_dic = Dict(x => x == 0 ? 1 : getfield(time_obj.nodes[x],:lvl) for x in unique(add_df[!,:Ts_dis]))
	lvl_arr = map(x -> tsDisLvl_dic[x],add_df[!,:Ts_dis])
	aboveSupScale_flt = maximum(values(supDis.sca)) * length(supDis.step) # scaling value used for variables above the superordinate dispatch level
	sca_arr = map(x -> supDis.lvl > x[1] ? aboveSupScale_flt : supDis.sca[(x[2],x[1])] ,zip(lvl_arr,add_df[!,:Ts_disSup]))
    return sca_arr
end

# XXX gets the upper bound used for dispatch variables
function getUpBound(in_df::DataFrame,anyM::anyModel)
	if !isnothing(anyM.options.bound.disp)
		upBound_arr = anyM.options.bound.disp * getScale(in_df,anyM.sets[:Ts],anyM.supTs)
	else
		upBound_arr = nothing
	end
	return upBound_arr
end

# </editor-fold>


# <editor-fold desc="functions and sub-functions to aggregate variables"

# XXX creates var column for rows in srcEtr_df from rows in aggEtr_df along columns provided via aggCol_tup
# the aggregation process is speeded-up by saving the location of set combinations that were already looked up to a dictionary
# grpInter_tup provides tuples that defines for what combination of sets dictionaries are created (e.g. ((:Ts_dis, :Te), (:R_dis, :C)) saves looked up Ts_dis/Te and R_dis/C combinations to dictionaries)
# these dictionaries can also be nested indicated by a pair ( e.g. (((:Ts_inv, :Te, :Ts_dis) => (:Ts_inv, :Te))) saves looks-up for Ts_inv/Te and uses these to look-up and save Ts_inv/Te/Ts_dis
# WARNING unreasonable grpInter_tup can lead to wrong results (e.g.  (((:Ts_inv, :Te) => (:Ts_inv, :Te, :Ts_dis)))
function aggregateVar(aggEtr_df::DataFrame, srcEtr_df::DataFrame, agg_tup::Tuple, sets_dic::Dict{Symbol,Tree}; aggFilt::Tuple = (), chldRows::Dict{Symbol,Dict{Int,BitSet}} =  Dict{Symbol,Dict{Int,BitSet}}())

	# XXX sanity checks regarding columns
	if all(names(aggEtr_df) |> (y -> map(x -> !(x in y),agg_tup))) error("tried to perform aggregation on column not existing in dataframe to be aggregated") end
	if all(names(srcEtr_df) |> (y -> map(x -> !(x in y),agg_tup))) error("tried to perform aggregation on column not existing in dataframe to aggregate") end

	select!(aggEtr_df,intCol(aggEtr_df,:var))
	# XXX filter entries from aggEtr_df, that based on isolated analysis of columns will not be aggregated
	for dim in intersect(aggFilt,agg_tup)
		set_sym = Symbol(split(string(dim),"_")[1])
		allSrc_set = unique(srcEtr_df[!,dim]) |> (z -> union(BitSet(z),map(x -> BitSet(getDescendants(x,sets_dic[set_sym],true)),z)...))
		aggEtr_df = aggEtr_df[findall(map(x -> (x in allSrc_set),aggEtr_df[!,dim])),:]
	end

	if isempty(aggEtr_df) return fill(AffExpr(),size(srcEtr_df,1)), chldRows end

	# XXX filter entries from srcEtr_df, that based on isolated anlysis of columns will not have any values aggregated to
	idxRel_set = BitSet(1:size(srcEtr_df,1))
	for dim in agg_tup
		set_sym = Symbol(split(string(dim),"_")[1])
		allAgg_set = unique(aggEtr_df[!,dim]) |> (z -> union(BitSet(z),map(y -> BitSet(getindex.(getAncestors(y,sets_dic[set_sym],0),1)), z)...))
		idxRel_set = intersect(idxRel_set,BitSet(findall(map(x -> x in allAgg_set, srcEtr_df[!,dim]))))
	end
	srcEtrAct_df = srcEtr_df[collect(idxRel_set),:]
	# group aggregation dataframe to relevant columns and removes unrequired columns
	aggEtrGrp_df = by(aggEtr_df,collect(agg_tup), var = [:var] => x -> sum(x.var))

	# XXX create dictionaries in each dimension that assign rows suited for aggregation for each value
	newChldRows_arr = setdiff(agg_tup,keys(chldRows))
	for col in newChldRows_arr
		# row that are potentially aggregated
		findCol_arr = aggEtrGrp_df[!,col]
		findCol_set = BitSet(findCol_arr)

		# entries that other entries can be aggregated to
		searchVal_set = BitSet(unique(srcEtrAct_df[!,col]))

		# to every unique value in column the value itself and its children are assigned
		set_sym = Symbol(split(string(col),"_")[1])
		idxChild_dic = Dict(x => intersect(findCol_set,[x,getDescendants(x,sets_dic[set_sym],true)...]) for x in searchVal_set)

		# for each unique value in column the rows with children are assigned
		grp_df = groupby(DataFrame(val = findCol_arr, id = 1:length(findCol_arr)),:val)
		dicVal_dic = Dict(x.val[1] => BitSet(sort(x[!,:id])) for x in grp_df) |> (dic -> Dict(x => union(map(y -> dic[y],collect(idxChild_dic[x]))...) for x in keys(idxChild_dic)))
		# excludes column from search, if based on it, every entry in find could be aggregated to every row in search
		if all(length.(values(dicVal_dic)) .== length(findCol_arr))
			select!(srcEtrAct_df,Not(col)); continue
		else
			chldRows[col] = dicVal_dic
		end
	end

	# XXX finds aggregation by intersecting suited rows in each dimension
	if isempty(chldRows)
		aggRow_arr = fill(BitSet(),size(srcEtrAct_df,1))
	else
		aggRow_arr = collect(keys(chldRows)) |> (y -> map(x -> intersect(map(y -> chldRows[y][x[y]],y)...) ,eachrow(srcEtrAct_df)))
	end

	# XXX aggregates values according to lookup
	out_arr = Array{AffExpr}(undef,size(srcEtr_df,1))
	out_arr[collect(idxRel_set)] =  map(x -> sum(aggEtrGrp_df[x,:var]), collect.(aggRow_arr))
	out_arr[setdiff(1:size(srcEtr_df,1),idxRel_set)] .= AffExpr()

	return out_arr, chldRows
end

# </editor-fold>

# <editor-fold desc="manipulate model related data frames"

# XXX add supordinate dispatch timestep to expansion dataframe
function addSupTsToExp(expMap_df::DataFrame,para_obj::Dict{Symbol,ParElement},type_sym::Symbol,tsYear_dic::Dict{Int,Int},anyM::anyModel)
	if !isempty(expMap_df)
		lftm_df = matchSetParameter(flatten(expMap_df,:Ts_expSup),para_obj[Symbol(:life,type_sym)],anyM.sets,anyM.report,newCol = :life)
		lftmDel_df = matchSetParameter(lftm_df,para_obj[Symbol(:delExp,type_sym)],anyM.sets,anyM.report,newCol = :del)
		lftmDel_df[!,:Ts_disSup] = map(x -> filter(y -> (tsYear_dic[y] >= tsYear_dic[x.Ts_expSup] + x.del) && (tsYear_dic[y] <= tsYear_dic[x.Ts_expSup] + x.life + x.del),collect(anyM.supTs.step)), eachrow(lftmDel_df))
		select!(lftmDel_df,Not([:life,:del]))
		grpCol_arr = intCol(expMap_df) |> (x -> :ratio in names(expMap_df) ? vcat(:ratio,x...) : x)
		expMap_df = by(lftmDel_df,grpCol_arr, [:Ts_expSup,:Ts_disSup] => x -> (Ts_expSup = [convert(Array{Int,1},x.Ts_expSup)], Ts_disSup = [convert(Array{Array{Int,1},1},x.Ts_disSup)]))
	else
		expMap_df[!,:Ts_disSup] = Array{Array{Int,1},1}()
	end
	return expMap_df
end

# XXX expand expansion dataframe to capacity dataframe
function expandExpToCapa(in_df::DataFrame)

	noExpCol_arr = intCol(in_df)

	allDf_arr = map(eachrow(in_df)) do x
		l_arr = length.(x.Ts_disSup)
		rem_df = repeat(DataFrame(x[noExpCol_arr]), inner = sum(l_arr), outer = 1)
		ext_df = DataFrame(Ts_expSup = vcat(map(y -> fill(x.Ts_expSup[y],l_arr[y]),1:length(l_arr))...), Ts_disSup = vcat(x.Ts_disSup...))
		return hcat(rem_df,ext_df)
	end

	if !isempty(allDf_arr)
		capa_df = select(vcat(allDf_arr...),orderDim(names(allDf_arr[1])))[!,Not(:Ts_exp)]
	else
		 capa_df = select(in_df,Not(:Ts_exp)); capa_df[!,:Ts_disSup] = Int[];
	end

	return orderDf(capa_df)
end

# XXX expands any table including columns with temporal and spatial dispatch levels and the corresponding expansion regions and supordinate dispatch steps to full dispatch table
function expandExpToDisp(inData_df::DataFrame,ts_dic::Dict{Tuple{Int,Int},Array{Int,1}},r_dic::Dict{Tuple{Int,Int},Int},preserveTsSupTs::Bool = false)
    # adds regional timesteps and check if this causes non-unique values (because spatial expansion level can be below dispatch level)
	expR_df = unique(by(inData_df,names(inData_df),R_dis = [:R_exp,:lvlR] => x -> r_dic[(x[1][1],x[2][1])])[!,Not([:R_exp,:lvlR])])
	expTs_df = by(expR_df,names(expR_df),Ts_dis = [:Ts_disSup, :lvlTs] => x -> ts_dic[(x[1][1],x[2][1])])[!,Not(:lvlTs)]

    # adds dispatch timesteps to table and returns
	if !preserveTsSupTs select!(expTs_df,Not(:Ts_disSup)) end
	return expTs_df
end

# XXX obtains residual capacities for technologies
function checkResiCapa(var_sym::Symbol, stockCapa_df::DataFrame, part::AbstractModelPart, anyM::anyModel, addSym::Symbol = Symbol())
  resiPar_sym = Symbol(var_sym,:Resi,addSym)
   if resiPar_sym in tuple(keys(part.par)...)
	   # search for defined residual values
	  stock_df = filter(r -> r.val != 0.0, matchSetParameter(stockCapa_df, part.par[resiPar_sym], anyM.sets, anyM.report))
   else
	   stock_df = filter(x -> false,stockCapa_df)
	   stock_df[!,:val] = Float64[]
   end

	# convers returned value to affine expression
   stock_df[!,:var] =  AffExpr.(stock_df[!,:val])
   select!(stock_df,Not(:val))

   return stock_df
end

# XXX get a dataframe with all variable of the specified type
function getAllVariables(va::Symbol,anyM::anyModel)

	varToPart_dic = Dict(:exc => :exc, :capaExc => :exc, :expExc => :exc, :ctr => :bal, :trdSell => :trd, :trdBuy => :trd, :emission => Symbol())
	techIdx_arr = collect(keys(anyM.parts.tech))

	if !(va in keys(varToPart_dic)) # get all variables for technologies
		va_dic = Dict(:stIn => (:stExtIn, :stIntIn), :stOut => (:stExtOut, :stIntOut))
		techType_arr = filter(x -> !isempty(x[2]),[[vaSpec,filter(y -> vaSpec in keys(anyM.parts.tech[y].var), techIdx_arr)] for vaSpec in (va in keys(va_dic) ? va_dic[va] : (va,))])
		allVar_df = vcat(map(x -> anyM.parts.tech[x[2]].var[x[1]], vcat(map(x -> collect(zip(fill(x[1],length(x[2])),x[2])),techType_arr)...))...)
	elseif va != :emission # get variables from other parts
		if va in keys(getfield(anyM.parts,varToPart_dic[va]).var)
			allVar_df = getfield(anyM.parts,varToPart_dic[va]).var[va]
		else
			allVar_df = DataFrame()
		end
	else va == :emission # for emission all use variables are obtained and then already matched with emission factors
		allVar_df = vcat(map(x -> anyM.parts.tech[x].var[:use], filter(y -> :use in keys(anyM.parts.tech[y].var), techIdx_arr))...)

		if !(:emissionFac in keys(anyM.parts.lim.par))
			push!(anyM.report,(2,"limits","emissionUp","upper emission limits but no emission factors provided"))
			allVar_df = DataFrame()
		end
		allVar_df = matchSetParameter(allVar_df,anyM.parts.lim.par[:emissionFac],anyM.sets,anyM.report)
		allVar_df[!,:var] = allVar_df[!,:val]  ./ 1e6 .* allVar_df[!,:var]
		select!(allVar_df,Not(:val))
	end

	return allVar_df
end

# </editor-fold>
