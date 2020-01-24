
# XXX maps their different levels to carriers
function createCarrierMapping!(setData_dic::Dict,anyM::anyModel)
    # determines number of specified carrier levels
    lvlCar_arr = [Symbol("carrier_",i) for i in 1:anyM.sets[:C].height]

    # tuple of columns with dispatch and expansion resolutions
    resCol_tup =  (:timestep_dispatch, :timestep_expansion, :region_dispatch, :region_expansion)
    resLongShort_tup = Dict(:timestep_dispatch => :lvlTsDis, :timestep_expansion => :lvlTsExp, :region_dispatch => :lvlRDis, :region_expansion => :lvlRExp)

    anyM.cInfo = Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:eq),Tuple{Int,Int,Int,Int,Bool}}}()

    # loops over rows in carrier file and writes specific resolutions
    for row in eachrow(setData_dic[:C])
    	# gets tuple of strings for respective carrier
    	car_tup = tuple(map(x -> row[x],lvlCar_arr)...)
    	car_int = lookupTupleTree(car_tup,anyM.sets[:C])[1]
    	# gets resolution value and writes them if they can be parsed to numbers
    	resVal_dic = Dict(resLongShort_tup[x] => row[x] for x in resCol_tup)

    	# check if level values can be converted to integers
    	if any(map(x -> tryparse(Int,x), values(resVal_dic)) .== nothing)
    		push!(anyM.report,(1,"carrier mapping","","no resolutions written for $(createFullString(car_int,anyM.sets[:C])), provide as integer"))
    		continue
    	end
    	res_dic = Dict(resLongShort_tup[x] => parse(Int,row[x]) for x in resCol_tup)
    	# writes levels after consistency check
    	if res_dic[:lvlTsDis] < res_dic[:lvlTsExp]
    		push!(anyM.report,(3,"carrier mapping","","temporal resolution of expansion can not be more detailed than for dispatch for $(createFullString(car_int,anyM.sets[:C]))"))
    		continue
    	elseif res_dic[:lvlRDis] > res_dic[:lvlRExp]
    		push!(anyM.report,(3,"carrier mapping","","spatial resolution of expansion must be at least as detailed as dispatch for $(createFullString(car_int,anyM.sets[:C]))"))
    		continue
    	else
			if !(row[:carrier_equality] in ("no","yes"))
				push!(anyM.report,(3,"carrier mapping","","column carrier_equality resolution can only contain keywords 'yes' or 'no'"))
				continue
			end
    		anyM.cInfo[car_int] = (tsDis = res_dic[:lvlTsDis],tsExp = res_dic[:lvlTsExp],rDis = res_dic[:lvlRDis],rExp = res_dic[:lvlRExp], eq = row[:carrier_equality] == "yes" ? true : false)
    	end
    end
	if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end
	 #  loops over all carriers and check consistency of resolutions and tries to inherit a resolution where none was defined, cannot be carried if above they have already been errors detected
    for c in filter(x -> x != 0, keys(anyM.sets[:C].nodes))
    	anyM.cInfo = evaluateReso(c,anyM.sets[:C],anyM.cInfo,anyM.report)
    end

    if minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo))) < maximum(map(x -> getfield(x,:tsExp),values(anyM.cInfo)))
    	push!(anyM.report,(3,"carrier mapping","","one temporal dispatch resoultion is more detailed than one of the temporal expansion resolutions"))
    end

    produceMessage(anyM.options,anyM.report, 3," - Created mapping for carriers")
end

# XXX checks carrier for errors in resolution or derive resolution from lower carriers
function evaluateReso(startIdx_int::Int,car_tree::Tree,cInfo_dic::Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:eq),Tuple{Int,Int,Int,Int,Bool}}},report::Array{Tuple,1})
	# extracts all chidlren and all columns related to resolution
	carName_str = createFullString(startIdx_int,car_tree)
	allChildIdx_arr = getDescendants(startIdx_int,car_tree)

	# all entries need to have a resoultion => otherwise evaluateReso on them
	for noResoIdx = setdiff(allChildIdx_arr,collect(keys(cInfo_dic)))
		cInfo_dic = evaluateReso(noResoIdx,car_tree,cInfo_dic,report)
	end

	# tries to inherit resolutions from children, if no data exists yet
	if !haskey(cInfo_dic,startIdx_int)
		if isempty(allChildIdx_arr)
			push!(report,(3,"carrier mapping","","carrier $(carName_str) got no resolution and could not inherit from children either"))
			return cInfo_dic
		else
			newReso_dic = Dict(y => minimum([getfield(cInfo_dic[x],y) for x in allChildIdx_arr]) for y in (:tsDis,:tsExp,:rDis,:rExp))
			cInfo_dic[startIdx_int] = (tsDis = newReso_dic[:tsDis],tsExp = newReso_dic[:tsExp],rDis = newReso_dic[:rDis],rExp = newReso_dic[:rExp], eq = false)
			push!(report,(1,"carrier mapping","","carrier $(carName_str) inherited resolution from children"))
			return cInfo_dic
		end
	# checks if existing resolution is flawed
	else
		for childIdx in allChildIdx_arr
			# check if any children got a smaller resolution value
			if any(map(x -> getfield(cInfo_dic[startIdx_int],x) > getfield(cInfo_dic[childIdx],x),(:tsDis,:tsExp,:rDis,:rExp)))
				push!(report,(3,"carrier mapping","","carrier $(carName_str) got a resolution more detailed than its childrens'"))
			end
		end
	end

	return cInfo_dic
end

# XXX maps information about timesteps used
function createTimestepMapping!(anyM::anyModel)
    # XXX writes the supordinate dispatch level, the timesteps on this level and scaling factor for timesteps depending on the respective supordinate dispatch timestep and the level
    supTsLvl_int = maximum(map(x -> getfield(x,:tsExp),values(anyM.cInfo)))

	if anyM.options.supTsLvl != 0
		if minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo))) >= anyM.options.supTsLvl
			supTsLvl_int = anyM.options.supTsLvl
			push!(anyM.report,(1,"timestep mapping","","supordinate dispatch level provided via options was used"))
		else
			push!(anyM.report,(2,"timestep mapping","","supordinate dispatch level provided via options could not be used, because it was more detailed than at least one dispatch level provided"))
		end
	end

    supTs_tup = tuple(sort(getfield.(filter(x -> x.lvl == supTsLvl_int,collect(values(anyM.sets[:Ts].nodes))),:idx))...)
    scaSupTs_dic = Dict((x[1],x[2]) => 8760/length(getDescendants(x[1],anyM.sets[:Ts],false,x[2])) for x in Iterators.product(supTs_tup,filter(x -> x >= supTsLvl_int,1:anyM.sets[:Ts].height)))
    anyM.supTs = (lvl = supTsLvl_int, step = supTs_tup, sca = scaSupTs_dic)

	produceMessage(anyM.options,anyM.report, 3," - Created mapping for time steps")
end

# XXX writes basic information for each technology
function createTechInfo!(t::Int, setData_dic::Dict,anyM::anyModel)

    part = anyM.parts.tech[t]

    lvlTech_arr = [Symbol("technology_",i) for i in 1:anyM.sets[:Te].height]

    # tuple of columns with input, output and stored carriers
    carCol_tup =  (:carrier_conversion_in, :carrier_conversion_out, :carrier_stored_in, :carrier_stored_out)

    # maps selected strings of tech types to integers
    typeStringInt_dic = Dict("stock" => 0, "mature" => 1,"emerging" => 2)

    # gets index for respective technology
    name_tup = getUniName(t,anyM.sets[:Te])
    allRow_df = eachrow(setData_dic[:Te])

    for i in unique(vcat((t,anyM.sets[:Te].nodes[t].lvl),getAncestors(t,anyM.sets[:Te],1)...))
		allRow_df = filter(r -> r[Symbol(:technology_,i[2])] == anyM.sets[:Te].nodes[i[1]].val,allRow_df)
	end
    row_df = allRow_df[1]

    # XXX writes carrier info
    # gets string array of carriers for input, output and stored, looks up respective ids afterwards and writes to mapping file
    carStrArr_dic = Dict(y => map(x -> string.(split(x,"<")),split(replace(row_df[y]," " => ""),";")) for y in carCol_tup)
    carId_dic = Dict(y => map(x -> x == [""] ? false : lookupTupleTree(tuple(x...),anyM.sets[:C],1),carStrArr_dic[y]) |> (z -> z == [false] ? tuple() : tuple(z...)) for y in keys(carStrArr_dic))
    # writes all relevant type of dispatch variables and respective carriers
    carGrp_ntup = (use = carId_dic[:carrier_conversion_in], gen = carId_dic[:carrier_conversion_out], stExtIn = carId_dic[:carrier_stored_in], stExtOut = carId_dic[:carrier_stored_out],
                         stIntIn = tuple(intersect(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])...), stIntOut = tuple(intersect(carId_dic[:carrier_conversion_in],carId_dic[:carrier_stored_in])...))

    # report on suspicious looking carrier constellations
    if isempty(union(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])) push!(anyM.report,(2,"technology mapping","carrier","technology $(createFullString(t,anyM.sets[:Te])) has no output")) end

    if isempty(intersect(carId_dic[:carrier_stored_in],union(carGrp_ntup.stIntOut,carGrp_ntup.stExtOut))) && !isempty(carId_dic[:carrier_stored_in])
        push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology $(createFullString(t,anyM.sets[:Te])) can be charged but not discharged"))
    end

    if isempty(intersect(carId_dic[:carrier_stored_out],union(carGrp_ntup.stIntIn,carGrp_ntup.stExtIn))) && !isempty(carId_dic[:carrier_stored_out])
        push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology $(createFullString(t,anyM.sets[:Te])) can be discharged but not charged"))
    end

    part.carrier = filter(x -> getfield(carGrp_ntup,x) != tuple(),collect(keys(carGrp_ntup))) |> (y -> NamedTuple{Tuple(y)}(map(x -> getfield(carGrp_ntup,x), y)) )

    # detects if any in or out carrier is a parent of another in or out carrier, removes carrier in these cases and reports on it
    for type in (:in,:out)
        relCar_tup = carId_dic[Symbol(:carrier_conversion_,type)]
        inherCar_tup = relCar_tup[findall(map(x -> !(isempty(filter(z -> z != x,intersect(getDescendants(x,anyM.sets[:C],true),relCar_tup)))),relCar_tup))]
        if !isempty(inherCar_tup)
            for inher in inherCar_tup
                push!(anyM.report,(2,"technology mapping","carrier","for technology $(createFullString(t,anyM.sets[:Te])) the $(type)put carrier $(createFullString(inher,anyM.sets[:C])) is a parent of another $(type)put carrier, this is not supported, carrier was removed"))
            end
            carId_dic[Symbol(:carrier_conversion_,type)] = tuple(setdiff(collect(carId_dic[Symbol(:carrier_conversion_,type)]),collect(inherCar_tup))...)
        end
    end

    # XXX writes technology type
    # finds technology type and tries to convert to an integer
    type_str = row_df[:technology_type]
    if !haskey(typeStringInt_dic,String(type_str))
        push!(anyM.report,(3,"technology mapping","type","unknown technology type $type_str used, allowed are: $(join(keys(typeStringInt_dic),", "))"))
        return
    end
    part.type = Symbol(type_str)

    # XXX checks if dispatch variables can should be disaggregated if expansion regions are more detailed than dispatch regions
    if :region_disaggregate in names(row_df)
        daggR_str = row_df[:region_disaggregate]
        if daggR_str == "yes"
            disAgg_boo = true
        elseif daggR_str == "no"
            disAgg_boo = false
        else
            push!(anyM.report,(3,"technology mapping","spatial aggregation","unknown keyword $type_str used to control spatial aggregation, please use 'yes' or 'no'"))
            return
        end
    else
        disAgg_boo = false
    end
    part.disAgg = disAgg_boo

    # XXX writes modes of technology
    if haskey(anyM.sets,:M)
        part.modes = tuple(collect(lookupTupleTree(String(x),anyM.sets[:M])[1] for x in filter(x -> x != "",split(replace(row_df[:mode]," " => ""),";")))...)
    else
        part.modes = tuple()
    end

    # XXX determines resolution

    # determines expansion timesteps (lowest level of expansion among all carriers, temporal level can be overwritten by technology specific values)
	expR_int = maximum(map(y -> getfield(anyM.cInfo[y],:rExp), vcat(collect.(values(carGrp_ntup))...)))
	expTs_int = maximum(map(y -> getfield(anyM.cInfo[y],:tsExp), vcat(collect.(values(carGrp_ntup))...)))

	if :timestep_expansion in names(row_df)
		tsExp_int = tryparse(Int,row_df[:timestep_expansion])

		if !isnothing(tsExp_int)
			if tsExp_int > anyM.supTs.lvl
				push!(anyM.report,(2,"technology mapping","expansion level","specific temporal expansion level provided for $(createFullString(t,anyM.sets[:Te])) is below supordinate dispatch level and therefore could not be used"))
			else
				push!(anyM.report,(1,"technology mapping","expansion level","specific temporal expansion level provided for $(createFullString(t,anyM.sets[:Te])) was used instead of a carrier based value"))
				expTs_int = tsExp_int
			end
		end
	end
    expLvl_tup = (expTs_int,expR_int)
    if isempty(keys(carGrp_ntup)) return end

    # determines reference level for conversion (takes into account "region_disaggregate" by using spatial expansion instead of dispatch level if set to yes)
    if !isempty(union(carGrp_ntup.use,carGrp_ntup.gen))
		refTs_int = minimum([maximum([getproperty(anyM.cInfo[x],:tsDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
		refR_int = minimum([maximum([getproperty(anyM.cInfo[x], disAgg_boo ? :rExp : :rDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
        refLvl_tup = (refTs_int, refR_int)
    else
        refLvl_tup = nothing
    end

    part.balLvl = (exp = expLvl_tup, ref = refLvl_tup)

	produceMessage(anyM.options,anyM.report, 3," - Created mapping for technology $(createFullString(t,anyM.sets[:Te]))")
end

# XXX maps capacity constraints to technology
function createCapaRestrMap!(t::Int,anyM::anyModel)

    part = anyM.parts.tech[t]

    capaDispRestr_arr = Array{Tuple{String,Array{Int,1},Int,Int},1}()
    # extract tech info
    carGrp_ntup = part.carrier
    balLvl_ntup = part.balLvl
    disAgg_boo  = part.disAgg

    # XXX writes dimension of capacity restrictions for conversion part (even if there are no inputs)
    for side in intersect((:use,:gen),keys(carGrp_ntup))
        # get respective carrier and their reference level
        carDis_tup = map(getfield(carGrp_ntup,side)) do x
                carRow_ntup = anyM.cInfo[x]
                return x, carRow_ntup.tsDis, getproperty(carRow_ntup,disAgg_boo ? :rExp : :rDis)
        end

        carConstr_arr = Tuple{Array{Int,1},Int,Int}[]

        # writes all relevant combinations by going from finest resolution up, seperately for temporal and spatial (2 and 3)
        for j = [2,3]
            # sorts descinding by j-th column and ascending by other column
            carDisSort_arr = sort(collect(carDis_tup), by = x -> x[j], rev=true)
            carIt_arr =	map(1:length(carDis_tup)) do x
                if j == 2 ([carDisSort_arr[y][1] for y in 1:x], carDisSort_arr[x][2], minimum([carDisSort_arr[y][3] for y in 1:x]))
                else ([carDisSort_arr[y][1] for y in 1:x], minimum([carDisSort_arr[y][2] for y in 1:x]), carDisSort_arr[x][3]) end
            end
            # filters entries that are on the reference level or exceed it
            carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] > balLvl_ntup.ref[1] : x[3] > balLvl_ntup.ref[2],carIt_arr)]
            push!(carConstr_arr, carIt_arr...)
        end

        # adds balance on reference levels, if so far no capacity constraint exists
        if isempty(carConstr_arr) && (side == :gen || !haskey(carGrp_ntup,:gen))
            push!(carConstr_arr,(collect(getfield(carGrp_ntup,side)), balLvl_ntup.ref[1], balLvl_ntup.ref[2]))
        end

        #  identifies and addresses "crossings" (one carrier is temporal more  but spatially less detailed than another one)
        carConstrUni_arr = unique(carConstr_arr)
        cross_arr = filter(x -> (any(map(z -> (x[2] > z[2] && x[3] < z[3]) || (x[3] > z[3] && x[2] < z[2]), carConstrUni_arr))),carConstrUni_arr)
        if !isempty(cross_arr)
            newEntry_tup = (union(getindex.(cross_arr,1)...), minimum(getindex.(cross_arr,2)),minimum(getindex.(cross_arr,3)))
            carConstrUni_arr = unique(vcat(newEntry_tup,carConstrUni_arr))
        end

        # filter redundant and "dominated" combinations (less or the same carriers, but not more temporal or spatial detail)
        carConstrUni_arr2 = map(i -> map(x -> x[i],carConstrUni_arr),1:3)
        carConFilt_arr = filter(carConstrUni_arr) do x
                        !(any((BitArray(issubset(x[1],y) for y in carConstrUni_arr2[1]) 		.&
                        (((x[2] .<= carConstrUni_arr2[2]) .& (x[3] .< carConstrUni_arr2[3]))	.|
                        ((x[2] .< carConstrUni_arr2[2]) .& (x[3] .<= carConstrUni_arr2[3])) 	.|
                        ((x[2] .<= carConstrUni_arr2[2]) .& (x[3] .<= carConstrUni_arr2[3]))))	.& BitArray(map(y -> y != x,carConstrUni_arr)))) end

        carConFilt_arr2 = map(i -> map(x -> x[i],carConFilt_arr),1:3)

        typeCapa_sym = side == :use ? "in" : "out"

        # adds necessary capacity restrictions below reference level
        map(x -> push!(capaDispRestr_arr,(typeCapa_sym, carConFilt_arr2[1][x], carConFilt_arr2[2][x], carConFilt_arr2[3][x])),1:length(carConFilt_arr))
    end

    # XXX writes dimension of capacity restrictions for storage
    stInVar_arr, stOutVar_arr = [intersect(x,keys(carGrp_ntup)) for x in ((:stExtIn,:stIntIn),(:stExtOut,:stIntOut))]
    if !isempty(stInVar_arr) || !isempty(stOutVar_arr)
        allCar_arr = unique(vcat(collect.([getproperty(carGrp_ntup,y) for y in union(stInVar_arr,stOutVar_arr)])...))
        for x in allCar_arr
            carRow_ntup = anyM.cInfo[x]
            # storage on carrier level, but at least on reference level, if region is disaggregated balance on expansion (which is at least lower)
            tsLvl_int = balLvl_ntup.ref != nothing ? max(balLvl_ntup.ref[1], carRow_ntup.tsDis) : carRow_ntup.tsDis
            rLvl_int  = balLvl_ntup.ref != nothing ? max(balLvl_ntup.ref[2], getfield(carRow_ntup,disAgg_boo ? :rExp : :rDis)) : getfield(carRow_ntup,disAgg_boo ? :rExp : :rDis)
            if !isempty(stInVar_arr) push!(capaDispRestr_arr,("stIn", [x], tsLvl_int, rLvl_int)) end
            if !isempty(stOutVar_arr) push!(capaDispRestr_arr,("stOut", [x], tsLvl_int, rLvl_int)) end
            push!(capaDispRestr_arr,("stSize", [x], tsLvl_int, rLvl_int))
        end
    end

    part.capaRestr = categorical(rename(DataFrame(capaDispRestr_arr), :1 => :cnstrType, :2 => :car, :3 => :lvlTs, :4 => :lvlR))
end
