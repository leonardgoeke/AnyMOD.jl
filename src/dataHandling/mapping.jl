
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

		# check, if carrier got an equality constraint or not
		if :carrier_equality in namesSym(row)
			if !(row[:carrier_equality] in ("no","yes"))
				push!(anyM.report,(2,"carrier mapping","","column carrier_equality can only contain keywords 'yes' or 'no'"))
				continue
			else
				eq_boo = row[:carrier_equality] == "yes" ? true : false
			end
		else
			eq_boo = false
		end

    	# check if level values can be converted to integers
    	if any(map(x -> tryparse(Int,x), values(resVal_dic)) .== nothing)
    		push!(anyM.report,(2,"carrier mapping","","no resolutions written for $(createFullString(car_int,anyM.sets[:C])), provide as integer, carrier was skipped"))
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
    		anyM.cInfo[car_int] = (tsDis = res_dic[:lvlTsDis],tsExp = res_dic[:lvlTsExp],rDis = res_dic[:lvlRDis],rExp = res_dic[:lvlRExp], eq = eq_boo)
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
	# extracts all children and all columns related to resolution
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
    # XXX writes the superordinate dispatch level, the timesteps on this level and scaling factor for timesteps depending on the respective superordinate dispatch timestep and the level
    supTsLvl_int = maximum(map(x -> getfield(x,:tsExp),values(anyM.cInfo)))

	if anyM.options.supTsLvl != 0
		if minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo))) >= anyM.options.supTsLvl
			supTsLvl_int = anyM.options.supTsLvl
			push!(anyM.report,(1,"timestep mapping","","superordinate dispatch level provided via options was used"))
		else
			push!(anyM.report,(2,"timestep mapping","","superordinate dispatch level provided via options could not be used, because it was more detailed than at least one dispatch level provided"))
		end
	end

    supTs_tup = tuple(sort(getfield.(filter(x -> x.lvl == supTsLvl_int,collect(values(anyM.sets[:Ts].nodes))),:idx))...)
    scaSupTs_dic = Dict((x[1],x[2]) => (1/anyM.options.redStep)*8760/length(getDescendants(x[1],anyM.sets[:Ts],false,x[2])) for x in Iterators.product(supTs_tup,filter(x -> x >= supTsLvl_int,1:anyM.sets[:Ts].height)))

	anyM.supTs = (lvl = supTsLvl_int, step = supTs_tup, sca = scaSupTs_dic)

    if length(anyM.supTs.step) > 50
		push!(anyM.report,(2,"timestep mapping","","problem specification resulted in more than 50 superordinate dispatch timesteps, this looks faulty"))
	end

	produceMessage(anyM.options,anyM.report, 3," - Created mapping for time steps")
end

# XXX writes basic information for each technology
function createTechInfo!(tSym::Symbol, setData_dic::Dict,anyM::anyModel)

    part = anyM.parts.tech[tSym]
    t_int = techInt(tSym,anyM.sets[:Te])
    lvlTech_arr = Symbol.(:technology_,1:anyM.sets[:Te].height)

    # tuple of columns with input, output and stored carriers
	typeStr_dic = Dict(:carrier_conversion_in => "conversion input", :carrier_conversion_out => "conversion output", :carrier_stored_in => "storage", :carrier_stored_out => "storage")
    carCol_tup =  (:carrier_conversion_in, :carrier_conversion_out, :carrier_stored_in, :carrier_stored_out)

	# maps carrier strings to their id
	nameC_dic = Dict(collect(values(anyM.sets[:C].nodes)) |> (y -> Pair.(getfield.(y,:val),getfield.(y,:idx))))

    # maps selected strings of tech types to integers
    typeStringInt_dic = Dict("stock" => 0, "mature" => 1,"emerging" => 2)

    # gets datarow for respective technology
	row_df = anyM.sets[:Te].nodes[t_int].val |> (z -> filter(x -> any(map(y -> z == x[y],lvlTech_arr)) ,setData_dic[:Te])[1,:])

    # XXX writes carrier info
    # gets string array of carriers for input, output and stored, looks up respective ids afterwards and writes to mapping file
    carStrArr_dic = Dict(y => y in namesSym(row_df) ? split(replace(row_df[y]," " => ""),";") |> (z -> filter(x -> !isempty(x),z)) : String[] for y in carCol_tup)
	carId_dic = Dict(z => tuple(map(x -> getDicEmpty(nameC_dic,x),carStrArr_dic[z])...) for z in keys(carStrArr_dic))

	for x in filter(x -> Int[] in carId_dic[x], collectKeys(keys(carId_dic)))
		push!(anyM.report,(3,"technology mapping","carrier","$(typeStr_dic[x]) carrier of technology $(string(tSym)) not entered correctly"))
		carId_dic[x] = tuple(filter(y -> y != Int[],collect(carId_dic[x]))...)
	end

	# avoid storage of carriers that are balanced on superordinate dispatch level (e.g. if gas is balanced yearly, there is no need for gas storage)
	for type in (:carrier_stored_out, :carrier_stored_in)
		for c in carId_dic[type]
			if anyM.supTs.lvl == anyM.cInfo[c].tsDis
				carId_dic[type] = tuple(filter(x -> x != c,collect(carId_dic[type]))...)
				push!(anyM.report,(2,"technology mapping","carrier","carrier $(createFullString(c,anyM.sets[:C])) of technology $(string(tSym)) cannot be stored, because carrier is balanced on supordiante dispatch level"))
			end
		end
	end

    # writes all relevant type of dispatch variables and respective carrier
    carGrp_ntup = (use = carId_dic[:carrier_conversion_in], gen = carId_dic[:carrier_conversion_out], stExtIn = carId_dic[:carrier_stored_in], stExtOut = carId_dic[:carrier_stored_out],
                         stIntIn = tuple(intersect(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])...), stIntOut = tuple(intersect(carId_dic[:carrier_conversion_in],carId_dic[:carrier_stored_in])...))

	if :carrier_stored_active in namesSym(row_df)
		actStStr_arr = split(replace(row_df[:carrier_stored_active]," " => ""),";") |> (z -> filter(x -> !isempty(x),z))
		actSt_tup = tuple(map(x -> getDicEmpty(nameC_dic,x),actStStr_arr)...)
	else
		actSt_tup = tuple()
	end
	part.actSt = actSt_tup

	# report on suspicious looking carrier constellations
    if isempty(union(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])) push!(anyM.report,(2,"technology mapping","carrier","technology $(string(tSym)) has no output")) end

    if !isempty(setdiff(carId_dic[:carrier_stored_in],union(carGrp_ntup.stIntOut,carGrp_ntup.stExtOut))) && !isempty(carId_dic[:carrier_stored_in])
        push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology $(string(tSym)) can be charged but not discharged"))
    end

    if !isempty(setdiff(carId_dic[:carrier_stored_out],union(carGrp_ntup.stIntIn,carGrp_ntup.stExtIn))) && !isempty(carId_dic[:carrier_stored_out])
        push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology $(string(tSym)) can be discharged but not charged"))
    end

	for c in part.actSt
		if !(c in vcat(map(x -> vcat(getDescendants(x,anyM.sets[:C],true)...,x), union(carGrp_ntup.stExtIn,carGrp_ntup.stExtOut))...))
			push!(anyM.report,(3,"technology mapping","carrier","$(createFullString(c,anyM.sets[:C])) for active storage of technology $(string(tSym)) is not stored or a descendant of a stored carrier"))
		end
	end

    part.carrier = filter(x -> getfield(carGrp_ntup,x) != tuple(),collectKeys(keys(carGrp_ntup))) |> (y -> NamedTuple{Tuple(y)}(map(x -> getfield(carGrp_ntup,x), y)) )

    # detects if any in or out carrier is a parent of another in or out carrier, removes carrier in these cases and reports on it
    for type in (:carrier_conversion_in, :carrier_conversion_out)
        relCar_tup = carId_dic[type]
        inherCar_tup = relCar_tup[findall(map(x -> !(isempty(filter(z -> z != x,intersect(getDescendants(x,anyM.sets[:C],true),relCar_tup)))),relCar_tup))]
        if !isempty(inherCar_tup)
            for inher in inherCar_tup
                push!(anyM.report,(3,"technology mapping","carrier","for technology $(string(tSym)) the $(typeStr_dic[type]) carrier $(createFullString(inher,anyM.sets[:C])) is a parent of another $(typeStr_dic[type]) carrier, this is not supported"))
            end
        end
    end

    # XXX writes technology type
    # finds technology type and tries to convert to an integer
	if :technology_type in namesSym(row_df)
    	type_str = row_df[:technology_type]
	else
		type_str = "mature"
	end
    if !haskey(typeStringInt_dic,String(type_str))
        push!(anyM.report,(3,"technology mapping","type","unknown technology type $type_str used, allowed are: $(join(keys(typeStringInt_dic),", "))"))
        return
    end
    part.type = Symbol(type_str)


    # XXX writes modes of technology
    if :mode in namesSym(row_df) && length(anyM.sets[:M].nodes) > 1
        part.modes = tuple(collect(lookupTupleTree(tuple(string(x),),anyM.sets[:M],1)[1] for x in filter(x -> x != "",split(replace(row_df[:mode]," " => ""),";")))...)
    else
        part.modes = tuple()
    end

    # XXX determines resolution of expansion
    # determines carrier based expansion resolutions
	cEx_boo = true
    if isempty(vcat(collect.(values(carGrp_ntup))...))
		push!(anyM.report,(2,"technology mapping","carrier","for technology $(string(tSym)) no carriers were provided"))
		cEx_boo = false
	end

	tsExp_int = cEx_boo ? maximum(map(y -> getfield(anyM.cInfo[y],:tsExp), vcat(collect.(values(carGrp_ntup))...))) : 0
	rExp_int = cEx_boo ? maximum(map(y -> getfield(anyM.cInfo[y],:rExp), vcat(collect.(values(carGrp_ntup))...))) : 0

	# check if carrier based temporal resolution is overwritten by a technology specifc value
	if cEx_boo && :timestep_expansion in namesSym(row_df)
		tsExpSpc_int = tryparse(Int,row_df[:timestep_expansion])

		if !isnothing(tsExpSpc_int)
			if tsExpSpc_int > anyM.supTs.lvl
				push!(anyM.report,(2,"technology mapping","expansion level","specific temporal expansion level provided for $(string(tSym)) is below superordinate dispatch level and therefore could not be used"))
			else
				push!(anyM.report,(1,"technology mapping","expansion level","specific temporal expansion level provided for $(string(tSym)) was used instead of a carrier based value"))
				tsExp_int = tsExpSpc_int
			end
		end
	end

	# check if carrier based spatial resolution is overwritten by a technology specifc value
	if cEx_boo && :region_expansion in namesSym(row_df)
		rExpSpc_int = tryparse(Int,row_df[:region_expansion])

		if !isnothing(rExpSpc_int)
			if rExpSpc_int < rExp_int
				push!(anyM.report,(2,"technology mapping","expansion level","specific spatial expansion level provided for $(string(tSym)) is less detailed than default value obtained from carriers and therefore could not be used"))
			elseif rExpSpc_int == rExp_int
				push!(anyM.report,(1,"technology mapping","expansion level","specific spatial expansion level provided for $(string(tSym)) is equal to default value obtained from carriers"))
			else
				push!(anyM.report,(1,"technology mapping","expansion level","specific spatial expansion level provided for $(string(tSym)) was used instead of a carrier based value"))
				rExp_int = rExpSpc_int
			end
		end
	end

    expLvl_tup = (tsExp_int,rExp_int)

	# XXX checks if dispatch variables should be disaggregated by expansion regions
	rExpOrg_int = cEx_boo ? maximum(map(y -> getfield(anyM.cInfo[y],:rDis), vcat(collect.(values(carGrp_ntup))...))) : 0

	if :region_disaggregate in namesSym(row_df) && rExp_int > rExpOrg_int # relies on information in explicit column, if disaggregation is possible and column exists
		daggR_str = row_df[:region_disaggregate]
		if daggR_str == "yes"
			disAgg_boo = true
		elseif daggR_str == "no"
			disAgg_boo = false
		else
			push!(anyM.report,(3,"technology mapping","spatial aggregation","unknown keyword $type_str used to control spatial aggregation, please use 'yes' or 'no'"))
			return
		end
	elseif rExp_int > rExpOrg_int # disaggregate by default, if it makes sense
		disAgg_boo = true
	else
		disAgg_boo = false
	end
	part.disAgg = disAgg_boo

    # XXX determines reference resolution for conversion (takes into account "region_disaggregate" by using spatial expansion instead of dispatch level if set to yes)
    if !isempty(union(carGrp_ntup.use,carGrp_ntup.gen))
		refTs_int = minimum([minimum([getproperty(anyM.cInfo[x],:tsDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
		refR_int = disAgg_boo ? rExp_int : minimum([minimum([getproperty(anyM.cInfo[x],:rDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
        refLvl_tup = (refTs_int, refR_int)
    else
        refLvl_tup = nothing
    end

    part.balLvl = (exp = expLvl_tup, ref = refLvl_tup)

	produceMessage(anyM.options,anyM.report, 3," - Created mapping for technology $(string(tSym))")
end

# XXX maps capacity constraints to technology
function createCapaRestrMap!(tSym::Symbol,anyM::anyModel)

    part = anyM.parts.tech[tSym]

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
                return x, carRow_ntup.tsDis, disAgg_boo ? balLvl_ntup.exp[2] : carRow_ntup.rDis
        end

        carConstr_arr = Tuple{Array{Int,1},Int,Int}[]

        # writes all relevant combinations by going from finest resolution up, seperately for temporal and spatial (2 and 3)
        for j = [2,3]
            # sorts descinding by j-th column and ascending by other column
            carDisSort_arr = sort(collect(carDis_tup), by = x -> x[j], rev=true)
            carIt_arr =	map(1:length(carDis_tup)) do x
                if j == 2 (sort([carDisSort_arr[y][1] for y in 1:x]), carDisSort_arr[x][2], minimum([carDisSort_arr[y][3] for y in 1:x]))
                else (sort([carDisSort_arr[y][1] for y in 1:x]), minimum([carDisSort_arr[y][2] for y in 1:x]), carDisSort_arr[x][3]) end
            end
            # filters entries that exceed the reference level or are not below the reference level, if already a constraint on the reference level exists from the previous iteration
			if side == :use && isempty(setdiff((:use,:gen),keys(carGrp_ntup)))
				carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] > balLvl_ntup.ref[1] : x[3] > balLvl_ntup.ref[2],carIt_arr)]
			else
            	carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] >= balLvl_ntup.ref[1] : x[3] >= balLvl_ntup.ref[2],carIt_arr)]
			end
            push!(carConstr_arr, carIt_arr...)
        end

        #  identifies and addresses "crossings" (one carrier is temporal more  but spatially less detailed than another one) by converting into new restriction mappings
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
            rLvl_int  = disAgg_boo ? balLvl_ntup.exp[2] : carRow_ntup.rDis |> (z -> balLvl_ntup.ref != nothing ? max(balLvl_ntup.ref[2], z) : z)
            if !isempty(stInVar_arr) push!(capaDispRestr_arr,("stIn", [x], tsLvl_int, rLvl_int)) end
            if !isempty(stOutVar_arr) push!(capaDispRestr_arr,("stOut", [x], tsLvl_int, rLvl_int)) end
            push!(capaDispRestr_arr,("stSize", [x], tsLvl_int, rLvl_int))
        end
    end

    part.capaRestr = isempty(capaDispRestr_arr) ? DataFrame() : categorical(rename(DataFrame(capaDispRestr_arr), :1 => :cnstrType, :2 => :car, :3 => :lvlTs, :4 => :lvlR))
end
