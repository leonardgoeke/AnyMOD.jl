
# ! maps their different levels to carriers
function createCarrierMapping!(setData_dic::Dict,anyM::anyModel)
    # determines number of specified carrier levels
    lvlCar_arr = [Symbol("carrier_",i) for i in 1:anyM.sets[:C].height]

    # tuple of columns with dispatch and expansion resolutions
    resCol_tup =  (:timestep_dispatch, :timestep_expansion, :region_dispatch, :region_expansion)
    resLongShort_tup = Dict(:timestep_dispatch => :lvlTsDis, :timestep_expansion => :lvlTsExp, :region_dispatch => :lvlRDis, :region_expansion => :lvlRExp)

    anyM.cInfo = Dict{Int,NamedTuple{(:tsDis,:tsExp,:rDis,:rExp,:balSign, :stBalCapa),Tuple{Int,Int,Int,Int,Symbol,Symbol}}}()

    # loops over rows in carrier file and writes specific resolutions
    for row in eachrow(setData_dic[:C])
    	# gets tuple of strings for respective carrier
    	car_tup = tuple(map(x -> row[x],lvlCar_arr)...)
    	car_int = lookupTupleTree(car_tup,anyM.sets[:C])[1]
    	# gets resolution value and writes them if they can be parsed to numbers
    	resVal_dic = Dict(resLongShort_tup[x] => row[x] for x in resCol_tup)

		# check, if carrier got an equality constraint or not
		if :carrier_balance in namesSym(row)
			if !(row[:carrier_balance] in ("eq","ineq","none",""))
				push!(anyM.report,(2,"carrier mapping","","column carrier_equality can only contain keywords 'eq', 'ineq', or 'none', assumed 'ineq'"))
				bal_sym = :ineq
			elseif row[:carrier_balance] == ""
				bal_sym = :ineq
			else
				bal_sym = Symbol(row[:carrier_balance])
			end
		else
			bal_sym = :ineq
		end

		# check if capacity balance of carrier should include storage
		if :carrier_strCapaBal in namesSym(row)
			if !(row[:carrier_strCapaBal] in ("no","yes",""))
				push!(anyM.report,(2,"carrier mapping","","column carrier_strCapaBal can only contain keywords 'no' or 'yes', assumed 'no'"))
				st_sym = :no
			elseif row[:carrier_strCapaBal] == ""
				st_sym = :no
			else
				st_sym = Symbol(row[:carrier_strCapaBal])
			end
		else
			st_sym = :no
		end

    	# check if level values can be converted to integers
    	if any(map(x -> tryparse(Int,x), values(resVal_dic)) .== nothing)
    		push!(anyM.report,(2,"carrier mapping","","no resolutions written for '$(createFullString(car_int,anyM.sets[:C]))', provide as integer, carrier was skipped"))
    		continue
    	end
    	res_dic = Dict(resLongShort_tup[x] => parse(Int,row[x]) for x in resCol_tup)
    	# writes levels after consistency check
    	if res_dic[:lvlTsDis] < res_dic[:lvlTsExp]
    		push!(anyM.report,(3,"carrier mapping","","temporal resolution of expansion can not be more detailed than for dispatch for '$(createFullString(car_int,anyM.sets[:C]))'"))
    		continue
    	elseif res_dic[:lvlRDis] > res_dic[:lvlRExp]
    		push!(anyM.report,(3,"carrier mapping","","spatial resolution of expansion must be at least as detailed as dispatch for '$(createFullString(car_int,anyM.sets[:C]))'"))
    		continue
    	else
    		anyM.cInfo[car_int] = (tsDis = res_dic[:lvlTsDis],tsExp = res_dic[:lvlTsExp],rDis = res_dic[:lvlRDis],rExp = res_dic[:lvlRExp], balSign = bal_sym, stBalCapa = st_sym)
    	end
    end

	if any(getindex.(anyM.report,1) .== 3) print(getElapsed(anyM.options.startTime)); errorTest(anyM.report,anyM.options) end

	 #  loops over all carriers and check consistency of resolutions and tries to inherit a resolution where none was defined, cannot be carried if above they have already been errors detected
	for c in filter(x -> x != 0, keys(anyM.sets[:C].nodes))
    	evaluateReso!(c,anyM)
    end

    if minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo))) < maximum(map(x -> getfield(x,:tsExp),values(anyM.cInfo)))
    	push!(anyM.report,(3,"carrier mapping","","one temporal dispatch resoultion is more detailed than one of the temporal expansion resolutions"))
    end

    produceMessage(anyM.options,anyM.report, 3," - Created mapping for carriers")
end

# ! checks carrier for errors in resolution or derive resolution from lower carriers
function evaluateReso!(startIdx_int::Int,anyM::anyModel)
	# extracts all children and all columns related to resolution
	carName_str = createFullString(startIdx_int,anyM.sets[:C])
	allChildIdx_arr = getDescendants(startIdx_int,anyM.sets[:C])

	# all entries need to have a resoultion => otherwise evaluateReso on them
	for noResoIdx in setdiff(allChildIdx_arr,collect(keys(anyM.cInfo)))
		evaluateReso!(noResoIdx,anyM)
	end

	# tries to inherit resolutions from children, if no data exists yet
	if !haskey(anyM.cInfo,startIdx_int)
		if isempty(allChildIdx_arr)
			push!(anyM.report,(3,"carrier mapping","","carrier '$(carName_str)' got no resolution and could not inherit from children either"))
			return anyM.cInfo
		else
			newReso_dic = Dict(y => minimum([getfield(anyM.cInfo[x],y) for x in allChildIdx_arr]) for y in (:tsDis,:tsExp,:rDis,:rExp))
			anyM.cInfo[startIdx_int] = (tsDis = newReso_dic[:tsDis],tsExp = newReso_dic[:tsExp],rDis = newReso_dic[:rDis],rExp = newReso_dic[:rExp], balSign = :ineq, stBalCapa = :no)
			push!(anyM.report,(1,"carrier mapping","","carrier '$(carName_str)' inherited resolution from children"))
			return anyM.cInfo
		end
	# checks if existing resolution is flawed
	else
		for childIdx in allChildIdx_arr
			# check if any children got a smaller resolution value
			if any(map(x -> getfield(anyM.cInfo[startIdx_int],x) > getfield(anyM.cInfo[childIdx],x),(:tsDis,:tsExp,:rDis,:rExp)))
				push!(anyM.report,(3,"carrier mapping","","carrier '$(carName_str)' got a resolution more detailed than its childrens'"))
			end
		end
	end

end

# ! maps information about timesteps used
function createTimestepMapping!(anyM::anyModel)
    
	# ! writes the superordinate dispatch level, the timesteps on this level and scaling factor for timesteps depending on the respective superordinate dispatch timestep and the level
    supTsLvl_int = maximum(map(x -> getfield(x,:tsExp),values(anyM.cInfo)))
	minDis_int = minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo)))
	
	if anyM.options.supTsLvl != 0
		if minDis_int >= anyM.options.supTsLvl
			supTsLvl_int = anyM.options.supTsLvl
			push!(anyM.report,(1,"timestep mapping","","superordinate dispatch level provided via options was used"))
		else
			push!(anyM.report,(2,"timestep mapping","","superordinate dispatch level provided via options could not be used, because it was more detailed than at least one dispatch level provided"))
		end
	end

	supTs_tup = tuple(sort(getfield.(filter(x -> x.lvl == supTsLvl_int,collect(values(anyM.sets[:Ts].nodes))),:idx))...)
	ts_tr = anyM.sets[:Ts]
	scaSupTs_dic = Dict{Int,Float64}()
	
	for sup in supTs_tup
		# compute scaling factors on lowest level
		tsBase_arr = getDescendants(sup,anyM.sets[:Ts],false,ts_tr.height)
		foreach(x -> scaSupTs_dic[x] = 8760/(length(tsBase_arr)), tsBase_arr)
		# compute other scaling factors as sum of lower levels
		if ts_tr.height > supTsLvl_int
			for l in reverse(supTsLvl_int:(ts_tr.height-1))
				foreach(x -> scaSupTs_dic[x] = sum(map(y -> scaSupTs_dic[y], ts_tr.nodes[x].down)), getDescendants(sup,anyM.sets[:Ts],false,l))
			end
		end
	end	

	anyM.supTs = (lvl = supTsLvl_int, step = supTs_tup, sca = scaSupTs_dic)

    if length(anyM.supTs.step) > 50
		push!(anyM.report,(2,"timestep mapping","","problem specification resulted in more than 50 superordinate dispatch timesteps, this looks faulty"))
	end

	# adjust temporal resolution of dispatch for valid inequalities
	if anyM.options.createVI 
		for c in keys(anyM.cInfo)
			res_dic = anyM.cInfo[c]
			anyM.cInfo[c] = (tsDis = anyM.supTs.lvl, tsExp = res_dic[:tsExp], rDis = res_dic[:rDis], rExp = res_dic[:rExp], balSign = res_dic[:balSign], stBalCapa = res_dic[:stBalCapa])
		end
	end
		
	produceMessage(anyM.options,anyM.report, 2," - Adjusted temporal resolution for valid inequalities")
	
	produceMessage(anyM.options,anyM.report, 3," - Created mapping for time steps")
end

# ! writes basic information for all systems (technology and exchange)
function createSysInfo!(sys::Symbol,sSym::Symbol, setData_dic::Dict,anyM::anyModel)

	# ! prepares data
	sysLong = sys == :Te ? :technology : :exchange

    part = getfield(anyM.parts,sys == :Te ? :tech : :exc)[sSym]
    s_int = sysInt(sSym,anyM.sets[sys])
    lvlTech_arr = Symbol.(Symbol(sysLong,"_"),1:anyM.sets[sys].height)

    # tuple of columns with input, output and stored carriers
	typeStr_dic = Dict(:carrier_conversion_in => "conversion input", :carrier_conversion_out => "conversion output", :carrier_stored_in => "storage", :carrier_stored_out => "storage")

	# maps carrier strings to their id
	nameC_dic = Dict(collect(values(anyM.sets[:C].nodes)) |> (y -> Pair.(getfield.(y,:val),getfield.(y,:idx))))

    # maps selected strings of tech types to integers
    typeStringInt_dic = Dict("stock" => 0, "mature" => 1,"emerging" => 2, "unrestricted" => 3)

    # gets datarow for respective technology
	row_df = anyM.sets[sys].nodes[s_int].val |> (z -> filter(x -> any(map(y -> z == x[y],lvlTech_arr)) ,setData_dic[sys])[1,:])

	# ! writes carrier info
	if sys == :Te
		carId_dic = Dict{Symbol,Tuple}()
		foreach(y -> isempty(row_df[y]) ? row_df[y] = "()" : nothing,(:carrier_stored_in, :carrier_stored_out))
		
		# gets string array of carriers for input, output and stored, looks up respective ids afterwards and writes to mapping file
		convStr_dic = Dict(y => y in namesSym(row_df) ? makeC(row_df[y]) |> (z -> filter(x -> !isempty(x),z)) : String[] for y in (:carrier_conversion_in, :carrier_conversion_out))
		stStr_dic = Dict(y => y in namesSym(row_df) ? map(a ->  makeC(a) |> (z -> filter(x -> !isempty(x),z)),map(z -> replace(z,")" => ""), String.(filter(x -> x != "",split(row_df[y],"("))))) : String[] for y in (:carrier_stored_in, :carrier_stored_out))
		
		if length(stStr_dic[:carrier_stored_in]) != length(stStr_dic[:carrier_stored_out])
			push!(anyM.report,(3,"technology mapping","carrier","for technology '$(string(sSym))' different numbers of groups for charged and discharged carriers are provided"))
		end

		foreach(z -> carId_dic[z] = tuple(map(x -> getDicEmpty(nameC_dic,x),convStr_dic[z])...) ,(:carrier_conversion_in, :carrier_conversion_out))
		foreach(z -> carId_dic[z] = tuple(map(y -> tuple(map(x -> getDicEmpty(nameC_dic,x),y)...),stStr_dic[z])...),(:carrier_stored_in, :carrier_stored_out))

		# reports on typos in assigned in carriers
		for x in filter(x -> Int[] in carId_dic[x] || any(map(y -> Int[] in y,collect(carId_dic[x]))), collectKeys(keys(carId_dic)))
			push!(anyM.report,(3,"technology mapping","carrier","$(typeStr_dic[x]) carrier of technology '$(string(sSym))' not entered correctly"))
			carId_dic[x] = collect(carId_dic[x]) |> (i -> tuple((x in (:carrier_stored_in,:carrier_stored_out) ? map(x -> filter(y -> y != Int[],x),i) : filter(y -> y != Int[],i))...))
		end

		# avoid storage of carriers that are balanced on superordinate dispatch level (e.g. if gas is balanced yearly, there is no need for gas storage)
		if !anyM.options.createVI
			for type in (:carrier_stored_out, :carrier_stored_in)
				for c in union(carId_dic[type]...)
					if anyM.supTs.lvl == anyM.cInfo[c].tsDis
						carId_dic[type] = tuple(map(z -> filter(x -> x != c,z),collect(carId_dic[type]))...)
						push!(anyM.report,(2,"technology mapping","carrier","carrier '$(createFullString(c,anyM.sets[:C]))' of technology '$(string(sSym))' cannot be stored, because carrier is balanced on superordinate dispatch level"))
					end
				end 
			end
		end

		# writes all relevant type of dispatch variables and respective carrier
		grpSt_int = length(carId_dic[:carrier_stored_out])
		carGrp_ntup = (use = carId_dic[:carrier_conversion_in], gen = carId_dic[:carrier_conversion_out], 
							stExtIn = carId_dic[:carrier_stored_in], stExtOut = carId_dic[:carrier_stored_out],
								stIntIn = tuple(map(x -> tuple(intersect(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out][x])...),1:grpSt_int)...), 
									stIntOut = tuple(map(x -> tuple(intersect(carId_dic[:carrier_conversion_in],carId_dic[:carrier_stored_in][x])...),1:grpSt_int)...))
		
		# report on suspicious looking carrier constellations
		if isempty(union(carId_dic[:carrier_conversion_out],union(carId_dic[:carrier_stored_out]...))) push!(anyM.report,(2,"technology mapping","carrier","technology '$(string(sSym))' has no output")) end

		for z in 1:grpSt_int
			if !isempty(setdiff(carId_dic[:carrier_stored_in][z],union(carGrp_ntup.stIntOut[z],carGrp_ntup.stExtOut[z]))) && !isempty(carId_dic[:carrier_stored_in])
				push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology '$(string(sSym))' $(grpSt_int == 1 ? "" : string("in group " ,z," of stored carriers")) can be charged but not discharged"))
			end

			if !isempty(setdiff(carId_dic[:carrier_stored_out][z],union(carGrp_ntup.stIntIn[z],carGrp_ntup.stExtIn[z]))) && !isempty(carId_dic[:carrier_stored_out])
				push!(anyM.report,(2,"technology mapping","carrier","some carrier of technology '$(string(sSym))' $(grpSt_int == 1 ? "" : string("in group " ,z," of stored carriers")) can be discharged but not charged"))
			end
		end
		
		part.carrier = filter(x -> !(typeof(getfield(carGrp_ntup,x)) <: Tuple{Vararg{<:Tuple{}}}),collectKeys(keys(carGrp_ntup))) |> (y -> NamedTuple{Tuple(y)}(map(x -> getfield(carGrp_ntup,x), y)) )

		# detects if any in or out carrier is a parent of another in or out carrier and reports on it
		for type in (:carrier_conversion_in, :carrier_conversion_out, :carrier_stored_out, :carrier_stored_in)
			relCar_tup = type in (:carrier_conversion_in, :carrier_conversion_out) ? carId_dic[type] : tuple(union(carId_dic[type]...)...)
			inherCar_tup = relCar_tup[findall(map(x -> !(isempty(filter(z -> z != x,intersect(getDescendants(x,anyM.sets[:C],true),relCar_tup)))),relCar_tup))]
			if !isempty(inherCar_tup)
				for inher in inherCar_tup
					push!(anyM.report,(3,"technology mapping","carrier","for technology '$(string(sSym))' the $(typeStr_dic[type]) carrier '$(createFullString(inher,anyM.sets[:C]))' is a parent of another $(typeStr_dic[type]) carrier $(grpSt_int == 1 ? "" : "of the same group"), this is not supported"))
				end
			end
		end
	elseif sys == :Exc
		
		# ! checks carrier name
		carId_arr = map(x -> getDicEmpty(nameC_dic,x),split(replace(row_df[:carrier_exchange]," " => ""),";") |> (z -> filter(x -> !isempty(x),z)))
		if isempty(carId_arr) 
			push!(anyM.report,(3,"exchange mapping","carrier","no exchange carrier provided for '$(string(sSym))'"))
			return
		elseif any(isempty.(carId_arr))
			push!(anyM.report,(3,"exchange mapping","carrier","carrier of exchange '$(string(sSym))' not entered correctly"))
			filter!(x -> !isempty(x), carId_arr)
			return
		end

		# ! reports on exchanged carriers with different spatial resolutions
		rDis_arr = map(x -> anyM.cInfo[x].rDis,carId_arr)
		if length(unique(rDis_arr)) > 1
			push!(anyM.report,(1,"exchange mapping","carrier","carriers that can be exchanged by '$(string(sSym))' have different spatial resolutions"))
		end

		part.carrier = tuple(carId_arr...)

		# ! detects if any exchanged carrier is a parent of another exchanged carrier and reports on it
		inherCar_arr = carId_arr[findall(map(x -> !(isempty(filter(z -> z != x, intersect(getDescendants(x,anyM.sets[:C],true),carId_arr)))),carId_arr))]
		if !isempty(inherCar_arr)
			for inher in inherCar_arr
				push!(anyM.report,(3,"exchange mapping","carrier","for exchange '$(string(sSym))' the carrier '$(createFullString(inher,anyM.sets[:C]))' is a parent of another exchanged carrier, this is not supported"))
			end
		end

		# ! writes exchange symmetry
		# finds technology type and tries to convert to an integer
		if Symbol(sysLong,"_symmetry") in namesSym(row_df)
			dir_str = row_df[Symbol(sysLong,"_symmetry")]
		else
			dir_str = "undirected"
		end
		if !(dir_str in ("directed","undirected"))
			push!(anyM.report,(3,string(sysLong) * " mapping","symmetry","unknown symmetry specification '$type_str' used, allowed are undirected and directed"))
			return
		end
		part.dir = dir_str == "directed"

	end

    # ! writes system type
    # finds technology type and tries to convert to an integer
	if Symbol(sysLong,"_type") in namesSym(row_df)
    	type_str = row_df[Symbol(sysLong,"_type")]
	else
		type_str = "mature"
	end
    if !haskey(typeStringInt_dic,String(type_str))
        push!(anyM.report,(3,string(sysLong) * " mapping","type","unknown type '$type_str' used, allowed are: $(join(keys(typeStringInt_dic),"', '"))"))
        return
    end
    part.type = Symbol(type_str)

	# ! writes decommissioning option for system
	if Symbol(sysLong,"_decomm") in namesSym(row_df)
		type_str = row_df[Symbol(sysLong,"_decomm")]
	else
		type_str = "none"
	end
	if !(type_str in ("none","decomm","recomm"))
		push!(anyM.report,(3,string(sysLong) * " mapping","decomm","unknown decommissioning type '$type_str' used, allowed are: 'none','decomm', and 'recomm'"))
		return
	end
	part.decomm = Symbol(type_str)

	# ! writes specific info for technologies
	if sys == :Te
		# writes modes
		if :mode in namesSym(row_df) && length(anyM.sets[:M].nodes) > 1
			part.modes = tuple(collect(lookupTupleTree(tuple(string(x),),anyM.sets[:M],1)[1] for x in filter(x -> x != "",split(replace(row_df[:mode]," " => ""),";")))...)
		else
			part.modes = tuple()
		end

		# writes types of conversion and storage balance
		for bal in (:conv,:st)
			# extracts value
			bal_sym = Symbol(:technology_,bal,:Balance)
			if bal_sym in namesSym(row_df)
				if !(row_df[bal_sym] in ("eq","ineq","none",""))
					push!(anyM.report,(2,"technology mapping","","column carrier_equality can only contain keywords 'eq', 'ineq', or 'none', assumed 'eq'"))
					bal_sym = :eq
				elseif row_df[bal_sym] == ""
					bal_sym = :eq
				else
					bal_sym = Symbol(row_df[bal_sym])
				end
			else
				bal_sym = :eq
			end
			# writes to object
			if bal == :conv
				part.balSign = (conv = bal_sym, st = :ineq)
			else
				part.balSign = (conv = part.balSign.conv, st = bal_sym)
			end
		end
	end

	# ! determines relevant resolutions for systen
	if sys == :Te

		allC_arr = union(map(x -> typeof(x) in (Tuple{},Tuple{Int64}) ? collect(x) :  vcat(collect.(x)...),collect(values(carGrp_ntup)))...)

		# ! determines resolution of expansion
		# determines carrier based expansion resolutions
		cEx_boo = true
		if isempty(allC_arr)
			push!(anyM.report,(2,"technology mapping","carrier","for technology '$(string(sSym))' no carriers were provided"))
			cEx_boo = false
		end

		tsExp_int = cEx_boo ? maximum(map(y -> getfield(anyM.cInfo[y],:tsExp), allC_arr)) : 0
		rExp_int = cEx_boo ? maximum(map(y -> getfield(anyM.cInfo[y],:rExp), allC_arr)) : 0

		# check if carrier based temporal resolution is overwritten by a technology specifc value
		if cEx_boo && :timestep_expansion in namesSym(row_df)
			tsExpSpc_int = tryparse(Int,row_df[:timestep_expansion])

			if !isnothing(tsExpSpc_int)
				if tsExpSpc_int > anyM.supTs.lvl
					push!(anyM.report,(2,"technology mapping","expansion level","specific temporal expansion level provided for technology '$(string(sSym))' is below superordinate dispatch level and therefore could not be used"))
				else
					push!(anyM.report,(1,"technology mapping","expansion level","specific temporal expansion level provided for technology '$(string(sSym))' was used instead of a carrier based value"))
					tsExp_int = tsExpSpc_int
				end
			end
		end

		# ! check if carrier based spatial resolution is overwritten by a technology specifc value
		if cEx_boo && :region_expansion in namesSym(row_df) && row_df[:region_expansion] != ""
			rExpSpc_int = tryparse(Int,row_df[:region_expansion])

			if !isnothing(rExpSpc_int)
				if rExpSpc_int < rExp_int
					push!(anyM.report,(2,"technology mapping","expansion level","specific spatial expansion level provided for technology '$(string(sSym))' is less detailed than default value obtained from carriers and therefore could not be used"))
				elseif rExpSpc_int == rExp_int
					push!(anyM.report,(1,"technology mapping","expansion level","specific spatial expansion level provided for technology '$(string(sSym))' is equal to default value obtained from carriers"))
				else
					push!(anyM.report,(1,"technology mapping","expansion level","specific spatial expansion level provided for technology '$(string(sSym))' was used instead of a carrier based value"))
					rExp_int = rExpSpc_int
				end
			else
				push!(anyM.report,(2,"technology mapping","expansion level","specific spatial expansion level provided for technology '$(string(sSym))' could not parsed into a integer, value ignored"))
			end
		end

		expLvl_tup = (tsExp_int,rExp_int)

		# ! checks if dispatch variables should be disaggregated by expansion regions
		rExpOrg_int = cEx_boo ? minimum(map(y -> getfield(anyM.cInfo[y],:rDis), allC_arr)) : 0

		if :region_disaggregate in namesSym(row_df) && rExp_int > rExpOrg_int && row_df[:region_disaggregate] != "" # relies on information in explicit column, if disaggregation is possible and column exists
			daggR_str = row_df[:region_disaggregate]
			if daggR_str == "yes"
				disAgg_boo = true
			elseif daggR_str == "no"
				disAgg_boo = false
			else
				push!(anyM.report,(3,"technology mapping","spatial aggregation","unknown keyword '$daggR_str' used to control spatial aggregation, please use 'yes' or 'no'"))
				return
			end
		else
			disAgg_boo = false
		end
		part.disAgg = disAgg_boo

		# ! check if internal storage variables should be subject to capacity restriction for storage
		if :technology_intCapaRestr in namesSym(row_df) && row_df[:technology_intCapaRestr] != ""
			intCapaRestr_str = row_df[:technology_intCapaRestr]
			if intCapaRestr_str == "yes"
				intCapaRestr_boo = true
			elseif intCapaRestr_str == "no"
				intCapaRestr_boo = false
			else
				push!(anyM.report,(3,"technology mapping","internal capacity restriction","unknown keyword '$intCapaRestr_str' used to control if interal storage variables are part of capacity restriction for storage, please use 'yes' or 'no'"))
				return
			end
		else
			intCapaRestr_boo = true
		end
		part.intCapaRestr = intCapaRestr_boo

		# ! check if a specific resolution is enforced for the cyclic constraint of storage
		if :timestep_cyclic in namesSym(row_df) && row_df[:timestep_cyclic] != ""
			stCyc_int =tryparse(Int,row_df[:timestep_cyclic])
			if isnothing(stCyc_int)
				stCyc_int = anyM.supTs.lvl
				push!(anyM.report,(2,"technology mapping","storage cycling","specific storage cycling level provided for technology '$(string(sSym))' could not parsed into a integer, value was ignored"))
			else
				if stCyc_int < anyM.supTs.lvl
					push!(anyM.report,(3,"technology mapping","storage cycling","specific storage cycling level provided for technology '$(string(sSym))' is less detailed than the superordinate dispatch timestep"))
					return
				end
			end
		else
			stCyc_int = anyM.supTs.lvl
		end
		part.stCyc = stCyc_int

		# ! check if a specific resolution is enforced for tracking the storage level
		if :timestep_tracked in namesSym(row_df) && row_df[:timestep_tracked] != ""
			stTrack_int =tryparse(Int,row_df[:timestep_tracked])
			if isnothing(stTrack_int)
				stTrack_int = nothing
				push!(anyM.report,(2,"technology mapping","storage tracked","specific resolution for tracking storage level provided for technology '$(string(sSym))' could not parsed into a integer, value was ignored"))
			else
				if stTrack_int < anyM.supTs.lvl
					push!(anyM.report,(3,"technology mapping","storage tracked","specific resolution for tracking storage level provided for technology '$(string(sSym))' is less detailed than the superordinate dispatch timestep"))
					return
				end
			end
		else
			stTrack_int = nothing
		end
		part.stTrack = stTrack_int

		# ! determines reference resolution for conversion (takes into account "region_disaggregate" by using spatial expansion instead of dispatch level if set to yes)
		if !isempty(part.carrier)
			if !isempty(union(carGrp_ntup.use,carGrp_ntup.gen))
				refTs_int = minimum([minimum([getproperty(anyM.cInfo[x],:tsDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
				refR_int = disAgg_boo ? rExp_int : minimum([minimum([getproperty(anyM.cInfo[x],:rDis) for x in getproperty(carGrp_ntup,z)]) for z in intersect(keys(part.carrier),(:gen, :use))])
				refLvl_tup = (refTs_int, refR_int)
			else
				refTs_int = minimum([minimum([getproperty(anyM.cInfo[x],:tsDis) for x in union(getproperty(carGrp_ntup,z)...)]) for z in intersect(keys(part.carrier),(:stExtIn, :stExtOut))])
				refR_int = disAgg_boo ? rExp_int : minimum([minimum([getproperty(anyM.cInfo[x],:rDis) for x in union(getproperty(carGrp_ntup,z)...)]) for z in intersect(keys(part.carrier),(:stExtIn, :stExtOut))])
				refLvl_tup = (refTs_int, refR_int)
			end
			part.balLvl = (exp = expLvl_tup, ref = refLvl_tup)
		end
	elseif sys == :Exc
		part.expLvl = (maximum(map(y -> getfield(anyM.cInfo[y],:tsExp), carId_arr)),maximum(map(y -> getfield(anyM.cInfo[y],:rExp), carId_arr)))
	end

	produceMessage(anyM.options,anyM.report, 3," - Created mapping for technology $(string(sSym))")
end

# ! maps capacity constraints to technology or exchange
function createCapaRestrMap!(part::AbstractModelPart,anyM::anyModel)

    capaDispRestr_arr = Array{Tuple{String,Array{Int,1},Int,Int},1}()
    # extract tech info
    carGrp_ntup = part.carrier
    balLvl_ntup = part.balLvl
    disAgg_boo  = part.disAgg

	# ! writes dimension of capacity restrictions for conversion part (even if there are no inputs)
	
	# for actual conversion capacities (capacities that do not solely use or generate) the general capacity restriction can be either enfored on the use or generation side, 
	# the code here tests which options will ultimately lead to the fewer number of constraints and uses it 
	collDim_arr = Array{Array{Tuple{String,Array{Int,1},Int,Int},1}}(undef,isempty(setdiff((:use,:gen),keys(carGrp_ntup))) ? 2 : 1)
	collRmvOut_arr = Array{Array{Int,1}}(undef,isempty(setdiff((:use,:gen),keys(carGrp_ntup))) ? 2 : 1)

	# determine relevant capacity constraints, for actual conversion this is performed for both sides
	for (idx,ctrSide) in enumerate(intersect((:use,:gen),keys(carGrp_ntup)))
		snglDim_arr = Array{Tuple{String,Array{Int,1},Int,Int},1}() # collects capacity restrictions for both sides
		snglRmv_arr = Array{Int,1}() # collects carriers where an output conversion restriction was removed due to must run
		for side in intersect((:use,:gen),keys(carGrp_ntup))

			# get respective carrier and their reference level
			carDis_arr = map(collect(getfield(carGrp_ntup,side))) do x
				carRow_ntup = anyM.cInfo[x]
				return x, carRow_ntup.tsDis, disAgg_boo ? balLvl_ntup.exp[2] : carRow_ntup.rDis
			end
			
			restrInfo_arr = mapCapaRestr(carDis_arr,side,anyM,carGrp_ntup,balLvl_ntup,ctrSide)
			typeCapa_str = side == :use ? "convIn" : "convOut"

			# adds necessary capacity restrictions below reference level
			map(x -> push!(snglDim_arr,(typeCapa_str, restrInfo_arr[x][1], restrInfo_arr[x][2], restrInfo_arr[x][3])),1:length(restrInfo_arr))

			# adjusts restrictions with regard to must run
			if :mustOut in keys(part.par)
				# adds a restriction to fix the relative output
				fixC_arr = unique(part.par[:mustOut].data[!,:C])
				for c in fixC_arr
					push!(snglDim_arr,("must",[c],anyM.cInfo[c].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[c].rDis))
				end
				# removes restrictions on out that become redundant due to the fixed output (out can only become redundant if carrier is not subject to storage and no seperate must capa variable exists)
				if side == :gen && !(:mustCapaConv in keys(part.var))
					redC_arr = setdiff(fixC_arr,vcat(map(x -> collect(getfield(carGrp_ntup,x)...),intersect((:stIntIn,:stExtOut),keys(carGrp_ntup)))...))
					for c in redC_arr
						if ("convOut",[c],anyM.cInfo[c].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[c].rDis) in snglDim_arr push!(snglRmv_arr,c) end # saves information that on capacity restriction about to be removed
						filter!(x -> x != ("convOut",[c],anyM.cInfo[c].tsDis, part.disAgg ? part.balLvl.exp[2] : anyM.cInfo[c].rDis),snglDim_arr) # filters redundant entry from array of all capacity restrictions
					end
				end
			end
		end
		collDim_arr[idx] = unique(snglDim_arr)
		collRmvOut_arr[idx] = unique(snglRmv_arr)
	end

	# for no acutal conversion just one array of capacity restrictions was written and is used subsequently, for acutal conversion the number of constraints in both cases is computed
	if length(collDim_arr) == 1
		if isempty(intersect((:use,:gen),keys(carGrp_ntup)))
			capaDispRestr_arr = Array{Tuple{String,Array{Int,1},Int,Int},1}()
			rmvOutC_arr = Int[]
		else
			capaDispRestr_arr = collDim_arr[1]
			rmvOutC_arr = collRmvOut_arr[1]
		end
	else 
		# get number of elements at temporal and spatial levels occuring in the written dimensions
		tsLvl_dic, rLvl_dic = [Dict(w => length(getNodesLvl(anyM.sets[z],w)) for w in union(map(x -> map(y -> y[z == :Ts ? 3 : 4],x),collDim_arr)...)) for z in [:Ts,:R]]
		# compute number of resulting restrictions
		numbRestr_arr = map(x -> sum(map(y -> tsLvl_dic[y[3]] * rLvl_dic[y[4]],x)),collDim_arr)

		# select collected restrictions with fewer elements
		capaDispRestr_arr = collDim_arr[findall(minimum(numbRestr_arr) .== numbRestr_arr)][1]
		rmvOutC_arr = collRmvOut_arr[findall(minimum(numbRestr_arr) .== numbRestr_arr)][1]
	end

	# ! writes dimension of capacity restrictions for storage
	for g in 1:countStGrp(carGrp_ntup)
		stInVar_arr, stOutVar_arr = [intersect(x,keys(carGrp_ntup)) for x in ((:stExtIn,:stIntIn),(:stExtOut,:stIntOut))]
		if isempty(stInVar_arr) && isempty(stOutVar_arr) continue end
		for st in (:stIn,:stOut,:stSize)
			stC_arr = unique(vcat(collect.([getproperty(carGrp_ntup,y)[g] for y in (st == :stSize ? union(stInVar_arr,stOutVar_arr) : (st == :stIn ? stInVar_arr : stOutVar_arr))])...))
			if isempty(stC_arr) continue end
			carDis_arr = map(x -> [x, anyM.cInfo[x].tsDis, anyM.cInfo[x].rDis], stC_arr)
			restrInfo_arr = mapCapaRestr(carDis_arr,:exc,anyM,carGrp_ntup,balLvl_ntup)
			map(x -> push!(capaDispRestr_arr,(string(st,"_",g), restrInfo_arr[x][1], restrInfo_arr[x][2], restrInfo_arr[x][3])),1:length(restrInfo_arr))
		end
	end

	part.capaRestr = isempty(capaDispRestr_arr) ? DataFrame() : rename(DataFrame(capaDispRestr_arr), :1 => :cnstrType, :2 => :car, :3 => :lvlTs, :4 => :lvlR)
	
	return rmvOutC_arr
end

# ! create scenario
function createScenarioMapping!(anyM::anyModel)

	allScr_arr = filter(x -> x != 0,getfield.(collect(values(anyM.sets[:scr].nodes)),:idx))

	# checks if actually any scenarios are defined
	if !(isempty(allScr_arr))

		minDis_int = minimum(map(x -> getfield(x,:tsDis),values(anyM.cInfo)))
	
		if anyM.options.lvlFrs != 0 
			if anyM.options.supTsLvl >= anyM.options.lvlFrs
				anyM.options.lvlFrs = 0
				push!(anyM.report,(2,"scenario mapping","","specified foresight level is not more detailed than superordinate dispatch level, therefore model still uses perfect foresight"))
			elseif minDis_int < anyM.options.lvlFrs
				anyM.options.lvlFrs = minDis_int
				push!(anyM.report,(1,"scenario mapping","","specified foresight level exceeds least detailed dispatch resolution, model uses level $(minDis_int) instead"))
			end
		end

		# gets level for scenarios
		lvl_int = anyM.options.lvlFrs == 0 ? anyM.supTs.lvl : anyM.options.lvlFrs
		prop_df = flatten(flatten(DataFrame(Ts_dis  = [getfield.(getNodesLvl(anyM.sets[:Ts],lvl_int),:idx)], scr = [allScr_arr]),:Ts_dis),:scr)

		# assigns probabilities defined as parameters
		if :scrProb in collectKeys(keys(anyM.parts.obj.par))
			propPar_df = matchSetParameter(prop_df,anyM.parts.obj.par[:scrProb],anyM.sets)
		else
			propPar_df = filter(x -> false,prop_df)
			propPar_df[!,:val] = Float64[]
		end

		# compute default values in other cases
		propDef_df = antijoin(prop_df,propPar_df,on = [:scr,:Ts_dis])
		propDef_df[!,:val] .= 1/length(allScr_arr)

		# merges collected data
		prop_df = vcat(propPar_df,propDef_df)

		# controls sum of probabilities
		control_df = combine(groupby(prop_df, [:Ts_dis]), :val => (x -> sum(x)) => :val)
		sca_dic = Dict(control_df[!,:Ts_dis] .=> control_df[!,:val])

		for x in eachrow(filter(x -> x.val != 1.0,control_df))
			push!(anyM.report,(2,"scenario","probability","for timestep '$(createFullString(x.Ts_dis,anyM.sets[:Ts]))' scenario probabilities do not sum up to 1.0, values were adjusted accordingly"))
		end
		prop_df[!,:val] .= map(x -> x.val/sca_dic[x.Ts_dis] ,eachrow(prop_df))

		# creates final assignments
		filter!(x -> x.val != 0.0,prop_df)

		tsToScr_dic = Dict(y => sort(filter(x -> x.Ts_dis == y,prop_df)[!,:scr]) for y in unique(prop_df[!,:Ts_dis]))
		tsScrToProp_dic = Dict((x.Ts_dis,x.scr) => x.val for x in eachrow(prop_df))

		# re-defines into a deterministic model for the most likely scenario or specified scenario
		if !isnothing(anyM.options.forceScr)
			# identify relevant scenario
			if anyM.options.forceScr == Symbol()
				avgPropScr_arr = collect(keys(tsScrToProp_dic)) |> (u -> map(z -> (z,maximum(map(y -> tsScrToProp_dic[y],filter(x -> x[2] == z, u)))),unique(getindex.(u,2))))
				propScr_int = maximum(getindex.(avgPropScr_arr,2)) |> (u -> filter(x -> x[2] == u,avgPropScr_arr)[1][1])
			else
				propScr_int = sysInt(anyM.options.forceScr,anyM.sets[:scr])
			end
			# adjust elements to solve deterministic for one scenario
			relTs_arr = unique(prop_df[!,:Ts_dis])
			tsScrToProp_dic = Dict((x,propScr_int) => 1.0 for x in relTs_arr)
			tsToScr_dic = Dict(x => [propScr_int] for x in relTs_arr)
		end
	else
		tsToScr_dic = Dict{Int64, Vector{Int64}}()
		tsScrToProp_dic = Dict{Tuple{Int64, Int64}, Float64}()
		lvl_int = 0
		if anyM.options.lvlFrs != 0
			anyM.options.lvlFrs = 0
			push!(anyM.report,(2,"scenario mapping","","foresight level set but not scenarios specified"))
		end
	end

	# assigns mappings to final object
	anyM.scr = (lvl = lvl_int, scr = tsToScr_dic, scrProb = tsScrToProp_dic)
end

# ! adjusts model object according to distributed generation
function distributedMapping!(anyM::anyModel,prepSys_dic::Dict{Symbol,Dict{Symbol,Dict{Symbol,NamedTuple}}})

	subPro = anyM.subPro

	if subPro != (0,0) # ! case of sub-problem
		supTs_int = anyM.supTs.step[subPro[1]]

		# get tuple of unrequired time-steps and scenarios
		rmvId_tup = (Ts_dis = map(x -> getDescendants(x,anyM.sets[:Ts],true),filter(x -> x != supTs_int,collect(anyM.supTs.step))) |> (y -> isempty(y) ? Int[] : union(y...)),
							Ts_exp = map(x -> getDescendants(x,anyM.sets[:Ts],true),filter(x -> x > supTs_int,collect(anyM.supTs.step))) |> (y -> isempty(y) ? Int[] : union(y...)),
																scr = filter(x -> x != subPro[2] && x != 0,getfield.(values(anyM.sets[:scr].nodes),:idx)))

		# remove unrequired nodes from trees of scenarios
		foreach(y ->  delete!(anyM.sets[:scr].nodes,y),rmvId_tup.scr)

		# rewrite information on superordinate time-steps
		anyM.supTs =  (lvl = anyM.supTs.lvl, step = tuple(supTs_int,), sca = filter(x -> getAncestors(x[1],anyM.sets[:Ts],:int,anyM.supTs.lvl)[end] == supTs_int, scaSupTs_dic))
		anyM.scr =  (lvl = anyM.scr.lvl, scr = Dict(supTs_int => [subPro[2],]), scrProb = filter(x -> x[1] == (supTs_int,subPro[2]), anyM.scr.scrProb))
		
		# ! adjust dictionaries for expansion preparation

		# only keep capacity entries from 
		for sys in collect(keys(prepSys_dic)), sSym in collect(keys(prepSys_dic[sys]))
			# delete all fields from preparation except for capa
			foreach(y -> delete!(prepSys_dic[sys][sSym],y), filter(x -> string(x)[1:4] != "capa",collectKeys(keys(prepSys_dic[sys][sSym]))))
			# remove other superordinate dispatch timesteps from field for capacity variables
			for etr in collectKeys(keys(prepSys_dic[sys][sSym]))
				var_df = filter(x -> x.Ts_disSup in anyM.supTs.step,prepSys_dic[sys][sSym][etr].var)
				resi_df = filter(x -> x.Ts_disSup in anyM.supTs.step,prepSys_dic[sys][sSym][etr].resi)
				if isempty(var_df) && isempty(resi_df)
					delete!(prepSys_dic[sys][sSym],etr)
				else
					prepSys_dic[sys][sSym][etr] = (var = var_df,resi = resi_df)
				end

				# checks carrier attribute of part again in case some fields are not relevant anymore, because capcity does not exist in respective year
				if sys == :Te
					part = anyM.parts.tech[sSym]
					relDisp_arr = intersect(keys(part.carrier),vcat(:capaConv in keys(prepSys_dic[sys][sSym]) ? [:gen,:use] : Symbol[], :capaStOut in keys(prepSys_dic[sys][sSym]) ? [:stExtOut,:stExtIn,:stIntIn,:stIntOut] : Symbol[]))
					part.carrier = filter(x -> !(typeof(getfield(part.carrier,x)) <: Tuple{Vararg{<:Tuple{}}}),relDisp_arr) |> (y -> NamedTuple{Tuple(y)}(map(x -> getfield(part.carrier,x), y)) )
				end

				# remove technology fully, if no capacities exist
				if isempty(prepSys_dic[sys][sSym])
					delete!(prepSys_dic[sys],sSym)
				end
			end
		end

		# ! remove parameter data
		# remove unrequired parameter data from technology and exchange parts
		for sys in (:exc,:tech), pName in collectKeys(keys(getfield(anyM.parts,sys)))
			allPar_arr = collectKeys(keys(getfield(getfield(anyM.parts,sys)[pName],:par)))
			for parName in allPar_arr
				if getfield(anyM.parts,sys)[pName].par[parName].problem == :top # completely delete parameters not relating to subproblems
					delete!(getfield(anyM.parts,sys)[pName].par,parName)
				elseif ((:mustOut in allPar_arr) && !(parName in (:avaConv,:avaStOut,:effConv,:effStOut,:ratioConvOutFix))) || !(:mustOut in allPar_arr) # remove unrequired data, but keep the parameter itself (parameter relevant to must-run are always kept) 
					parData_df = getfield(anyM.parts,sys)[pName].par[parName].data
					rmv_arr = intersect(namesSym(parData_df),[:Ts_dis,:scr])
					if isempty(rmv_arr)
						continue
					else
						getfield(anyM.parts,sys)[pName].par[parName].data  = filter(x -> !any(map(y -> x[y] in getfield(rmvId_tup,y),rmv_arr)),parData_df)
					end
				end
			end
		end
		# remove unrequired parameter data from all other objects
		for pName in (:bal,:lim,:cost,:obj), parName in collectKeys(keys(getfield(getfield(anyM.parts,pName),:par)))
			if getfield(anyM.parts,pName).par[parName].problem == :top
				delete!(getfield(anyM.parts,pName).par,parName)
			else
				parData_df = getfield(anyM.parts,pName).par[parName].data
				rmv_arr = intersect(namesSym(parData_df),[:Ts_dis,:scr])
				if isempty(rmv_arr)
					continue
				else
					getfield(anyM.parts,pName).par[parName].data  = filter(x -> !any(map(y -> x[y] in getfield(rmvId_tup,y),rmv_arr)),parData_df)
				end
			end
		end

		produceMessage(anyM.options,anyM.report, 1," - Adjusted model to be a sub-problem for time-step '$(createFullString(supTs_int,anyM.sets[:Ts]))'$(getScrName(subPro[2],anyM.sets[:scr]))")
	else # ! case of top-problem
		# ! remove parameter data
		# remove unrequired parameter data from technology parts
		for sys in (:exc,:tech), pName in collectKeys(keys(getfield(anyM.parts,sys))), parName in collectKeys(keys(getfield(getfield(anyM.parts,sys)[pName],:par)))
			if getfield(anyM.parts,sys)[pName].par[parName].problem == :sub # completely delete parameters not relating to subproblems
				delete!(getfield(anyM.parts,sys)[pName].par,parName)
			end
		end
		# remove unrequired parameter data from all other objects
		for pName in (:bal,:lim,:cost,:obj), parName in collectKeys(keys(getfield(getfield(anyM.parts,pName),:par)))
			if getfield(anyM.parts,pName).par[parName].problem == :sub
				delete!(getfield(anyM.parts,pName).par,parName)
			end
		end
		produceMessage(anyM.options,anyM.report, 1," - Adjusted model to be the top-problem")
	end
end

# ! maps capacity restriction for one type (e.g. gen, use, st, or exc)
function mapCapaRestr(carDis_arr::Array,type::Symbol, anyM::anyModel, carGrp_ntup::NamedTuple = NamedTuple(), balLvl_ntup::NamedTuple = (exp = (0, 0), ref = (0, 0)),ctrSide::Symbol = :gen)

	carConstr_arr = Tuple{Array{Int,1},Int,Int}[]

	# writes all relevant combinations by going from finest resolution up, separately for temporal and spatial (2 and 3)
	for j = [2,3]
		# sorts descinding by j-th column and ascending by other column
		carDisSort_arr = sort(carDis_arr, by = x -> x[j], rev=true)
		carIt_arr =	map(1:length(carDis_arr)) do x
			if j == 2 (sort([carDisSort_arr[y][1] for y in 1:x]), carDisSort_arr[x][2], minimum([carDisSort_arr[y][3] for y in 1:x]))
			else (sort([carDisSort_arr[y][1] for y in 1:x]), minimum([carDisSort_arr[y][2] for y in 1:x]), carDisSort_arr[x][3]) end
		end
		# filters entries that exceed the reference level or are not below the reference level, if already a constraint on the reference level exists from the previous iteration
		if type == ctrSide && isempty(setdiff((:use,:gen),keys(carGrp_ntup)))
			carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] > balLvl_ntup.ref[1] : x[3] > balLvl_ntup.ref[2],carIt_arr)]
		elseif type in (:use,:gen)
			carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] >= balLvl_ntup.ref[1] : x[3] >= balLvl_ntup.ref[2],carIt_arr)]
		end
		push!(carConstr_arr, carIt_arr...)
	end
	carConstrUni_arr = unique(carConstr_arr)

	# filter redundant and "dominated" combinations (less or the same carriers, but not more temporal or spatial detail)
	carConstrUni_arr2 = map(i -> map(x -> x[i],carConstrUni_arr),1:3)
	restrInfo_arr = filter(carConstrUni_arr) do x
					!(any((BitArray(issubset(x[1],y) for y in carConstrUni_arr2[1]) 		.&
					(((x[2] .<= carConstrUni_arr2[2]) .& (x[3] .< carConstrUni_arr2[3]))	.|
					((x[2] .< carConstrUni_arr2[2]) .& (x[3] .<= carConstrUni_arr2[3])) 	.|
					((x[2] .<= carConstrUni_arr2[2]) .& (x[3] .<= carConstrUni_arr2[3]))))	.& BitArray(map(y -> y != x,carConstrUni_arr)))) end
					
	return restrInfo_arr

end