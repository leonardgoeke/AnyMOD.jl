
function createAllMappings(Set_dic::Dict{Symbol,DataFrame}, Parameter_dic::Dict{Symbol,ParElement}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})

	Mapping_dic = Dict{Symbol,IndexedTable}()

	# <editor-fold desc="mapping data defined in technology and carrier set files"

	Mapping_dic[:TechData], SaveLookup_dic 	= createMapping(:TechData, Set_dic::Dict{Symbol,DataFrame},SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})
	Mapping_dic[:C_lvl], SaveLookup_dic 			= createMapping(:C_lvl, Set_dic::Dict{Symbol,DataFrame}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})

	# </editor-fold>

	Mapping_dic[:TechInfo] = createMapping(:TechInfo, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})

	Mapping_dic[:invConv],  Mapping_dic[:invSt],  Mapping_dic[:invExc]	= createMapping(:invConvStExc,  Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})
	Mapping_dic[:capaConv], Mapping_dic[:capaSt], Mapping_dic[:capaExc]	= createMapping(:capaConvStExc, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})

	Mapping_dic[:capaDispRestr] = createMapping(:capaDispRestr, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})
	Mapping_dic[:dispVar] 		= createMapping(:dispVar, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})

	return Mapping_dic, SaveLookup_dic
end

createMapping(name::Symbol, Set_dic::Dict{Symbol,DataFrame}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}}) = createMapping(Val{name}(), Set_dic::Dict{Symbol,DataFrame}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})
createMapping(name::Symbol, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable}) = createMapping(Val{name}(), Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})


# <editor-fold desc="mapping data defined in technology and carrier set files (can be executed in any order)"

# XXX maps carriers, modes and investment types to technologies from input
function createMapping(name::Val{:TechData},Set_dic::Dict{Symbol,DataFrame}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})

	# creates empty data frame for results
	mapTechCar_tab = table(Array{Int16}[], Array{Int16}[], Array{Int16}[], Array{Int16}[], Int16[], Bool[], Array{Int16}[], Int16[],
																names=[:cConvIn, :cConvOut, :cStExtIn, :cStExtOut, :type, :daggR, :modes, :Te])

	# determines number of specified technology levels
	LvlTech_arr = [Symbol("technology_",i) for i in 1:maximum(Set_dic[:Te][:,:lvl])]

	# tuple of columns with input, output and stored carriers
	CarCol_tup =  (:carrier_conversion_in, :carrier_conversion_out, :carrier_stored_in,:carrier_stored_out)

	# maps selected strings of tech types to integers
	TypeStringInt_dic = Dict("stock" => 0, "mature" => 1,"emerging" => 2)

	# loops over rows of technologies
    for row in eachrow(SetData_dic[:Te])
		# gets index for respective technolgoy
		Tech_arr = filter(x -> x != "",[row[tech] for tech in LvlTech_arr])
		Tech_int, SaveLookup_dic[:Te] = lookupTupleTree(Tech_arr,Set_dic[:Te],SaveLookup_dic[:Te])

		# gets string array of carriers for input, output and stored, looks up respective ids afterwards and writes to mapping file
		carStrArr_dic = Dict(y => map(x -> split(x,"<"),split(replace(row[y]," " => ""),";")) for y in CarCol_tup)
		carId_dic = Dict(y => map(x -> lookupTupleTree(x,Set_dic[:C],SaveLookup_dic[:C],1,false)[1],carStrArr_dic[y]) for y in keys(carStrArr_dic))
		carId_dic = Dict(y => (carId_dic[y] == [false] ? [] : carId_dic[y]) for y in keys(carStrArr_dic))
		# defines internal storage possibilities: either discharge carrier to serve as input to transformation or charge with an output of the transformation
		carrierStoredIntIn_arr = intersect(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])
		carrierStoredIntOut_arr = intersect(carId_dic[:carrier_conversion_in],carId_dic[:carrier_stored_in])

		# report on suspicious looking carrier constellations
		if isempty(union(carId_dic[:carrier_conversion_out],carId_dic[:carrier_stored_out])) push!(Report_df,(2,:map,:TechCar,"technology $(createFullString(Tech_int[1],Set_dic[:Te])) has no output")) end

		if isempty(intersect(carId_dic[:carrier_stored_in],union(carrierStoredIntOut_arr,carId_dic[:carrier_stored_out]))) && !isempty(carId_dic[:carrier_stored_in])
			push!(Report_df,(2,:map,:TechCar,"some carrier of technology $(createFullString(Tech_int[1],Set_dic[:Te])) can be charged but not discharged"))
		end

		if isempty(intersect(carId_dic[:carrier_stored_out],union(carrierStoredIntIn_arr,carId_dic[:carrier_stored_in]))) && !isempty(carId_dic[:carrier_stored_out])
			push!(Report_df,(2,:map,:TechCar,"some carrier of technology $(createFullString(Tech_int[1],Set_dic[:Te])) can be discharged but not charged"))
		end

		# finds technology type and tries to convert to an integer
		type_str = row[:technology_type]
		if !(String(type_str) in keys(TypeStringInt_dic))
			push!(Report_df,(3,:map,:technology_type,"unknown technology type $type_str used, allowed are: $(join(keys(TypeStringInt_dic),", "))"))
			continue
		end

		# checks if dispatch variables can should be disaggregated if investment regions are more detailed than dispatch regions
		daggR_str = row[:region_disaggregate]
		if daggR_str == "yes"
			daggR_boo = true
		elseif daggR_str == "no"
			daggR_boo = false
		else
			push!(Report_df,(3,:map,:technology_region_aggregation,"unknown keyword $type_str used to control spatial aggregation, please use 'yes' or 'no'"))
			continue
		end

		modes_arr = [lookupTupleTree(String(x),Set_dic[:M])[1] for x in split(replace(row[:mode]," " => ""),";")]

		push!(rows(mapTechCar_tab), (cConvIn = carId_dic[:carrier_conversion_in],cConvOut = carId_dic[:carrier_conversion_out],
							cStExtIn = carId_dic[:carrier_stored_in], cStExtOut = carId_dic[:carrier_stored_out], type = TypeStringInt_dic[type_str], daggR = daggR_boo, modes = modes_arr, Te = Tech_int[1]))
	end

	return mapTechCar_tab, SaveLookup_dic
end

# XXX maps temporal and spatial resolution to carriers
function createMapping(name::Val{:C_lvl},Set_dic::Dict{Symbol,DataFrame}, SetData_dic::Dict{Symbol,DataFrame}, SaveLookup_dic::Dict{Symbol,Dict{Tuple,Array}})

	# creates empty data frame for results
	mapCarRes_tab = table(Int16[], Int16[], Int16[], Int16[], Int16[], names=[:C, :lvlTsDis, :lvlTsInv, :lvlRDis, :lvlRInv])

    # determines number of specified technology levels
	LvlCar_arr = [Symbol("carrier_",i) for i in 1:maximum(Set_dic[:C][:,:lvl])]

	# tuple of columns with dispatch and investment resolutions
	ResCol_tup =  (:timestep_dispatch, :timestep_invest, :region_dispatch, :region_invest)
	ResLongShort_tup = Dict(:timestep_dispatch => :lvlTsDis, :timestep_invest => :lvlTsInv, :region_dispatch => :lvlRDis, :region_invest => :lvlRInv)

	# loops over rows in carrier file and writes specific resolutions
    for row in eachrow(SetData_dic[:C])
		# gets index for respective carrier
		Car_arr = map(x -> row[x],LvlCar_arr)
		Car_int = lookupTupleTree(Car_arr,Set_dic[:C],SaveLookup_dic[:C])[1][1]
		# gets resolution value and writes them if they can be parsed to numbers
		ResVal_dic = Dict(ResLongShort_tup[x] => row[x] for x in ResCol_tup)

		# check if level values can be converted to integers
		if any(map(x -> tryparse(Int16,x), values(ResVal_dic)) .== nothing)
			push!(Report_df,(1,:map,:Car_lvl,"no resolutions written for $(createFullString(Car_int,Set_dic[:C])), provide as integer"))
			continue
		end
		Res_dic = Dict(ResLongShort_tup[x] => parse(Int16,row[x]) for x in ResCol_tup)
		# writes levels after consistency check
		if Res_dic[:lvlTsDis] < Res_dic[:lvlTsInv]
			push!(Report_df,(3,:map,:Car_lvl,"temporal resolution of invest can not be more detailed than for dispatch for $(createFullString(Car_int,Set_dic[:C]))"))
			continue
		elseif Res_dic[:lvlRDis] > Res_dic[:lvlRInv]
			push!(Report_df,(3,:map,:Car_lvl,"spatial resolution of invest must be at least as detailed as dispatch for $(createFullString(Car_int,Set_dic[:C]))"))
			continue
		else
			push!(rows(mapCarRes_tab), (C = Car_int[1], lvlTsDis = Res_dic[:lvlTsDis], lvlTsInv = Res_dic[:lvlTsInv], lvlRDis = Res_dic[:lvlRDis], lvlRInv = Res_dic[:lvlRInv]))
		end
	end

	if minimum(mapCarRes_tab.columns.lvlTsDis) < maximum(mapCarRes_tab.columns.lvlTsInv)
		push!(Report_df,(3,:map,:Car_lvl,"one temporal dispatch resoultion is more detailed than one of the temporal investment resolutions"))
	end


	#  if values were written check for invalid values, else try to write values via inheritance
	for row in eachrow(Set_dic[:C])
		# tries to extract resolution data from carrier
		parent_tab = filter(r -> r.C ==  row[:idx], mapCarRes_tab)
		carName_str = createFullString(row[:idx][1],Set_dic[:C])
		Res_dic = evaluateReso([row[:idx]],parent_tab,Set_dic[:C],mapCarRes_tab,carName_str)

		if Res_dic != nothing
			push!(rows(mapCarRes_tab), (C = row[:idx], lvlTsDis = Res_dic[:lvlTsDis], lvlTsInv = Res_dic[:lvlTsInv], lvlRDis = Res_dic[:lvlRDis], lvlRInv = Res_dic[:lvlRDis]))
			push!(Report_df,(1,:map,:Car_lvl,"carrier $(createFullString(row[:idx],Set_dic[:C])) inherited resolution from children"))
		end
	end

	return mapCarRes_tab, SaveLookup_dic
end

# </editor-fold>


# <editor-fold desc="more elaborate mappings based on data defined in technology and carrier set files (must be executed in this order)"

# XXX determines a number of technology characteristics frequently used
function createMapping(name::Val{:TechInfo}, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})

	mapTechInfo_tab = table(NamedTuple[], NamedTuple[], Union{Nothing,NamedTuple}[], Bool[], Array{Int16,1}[], Int16[], names=[:allCar, :invLvl, :refLvl, :daggR, :M,:Te])

	for row in rows(Mapping_dic[:TechData])
		# writes all relevant type of dispatch variables and respective carriers
		carVarAll_ntup = (use = row[:cConvIn], gen = row[:cConvOut], stExtIn = row[:cStExtIn], stExtOut =row[:cStExtOut],
							 stIntIn = intersect(row[:cConvOut],row[:cStExtOut]), stIntOut = intersect(row[:cConvIn],row[:cStExtIn]))
		relType_tup = tuple(filter(x -> !isempty(carVarAll_ntup[x]),collect(keys(carVarAll_ntup)))...)
		carVar_ntup = 	NamedTuple{relType_tup}([carVarAll_ntup[x] for x in relType_tup])

		# determines investment timestps
		allCarObj_dic = Dict(x => DB.select(Mapping_dic[:C_lvl],DB.Not(:C))[findall(DB.select(Mapping_dic[:C_lvl],:C => y -> y == x))[1]] for x in unique(vcat(values(carVar_ntup)...)))
		invLvl_ntup = NamedTuple{(:Ts,:R)}([maximum(map(x -> getproperty(x,y)[1],values(allCarObj_dic))) for y in (:lvlTsInv, :lvlRInv)])

		# determines reference level for conversion (takes into account "region_disaggregate" by using spatail investment instead of dispatch level if set to yes)
		if !isempty(union(row[:cConvIn],row[:cConvOut]))
			refTs_int = minimum([maximum([getproperty(allCarObj_dic[x],:lvlTsDis) for x in getproperty(carVar_ntup,z)]) for z in intersect(relType_tup,(:gen, :use))])
			refR_int = minimum([maximum([getproperty(allCarObj_dic[x], row.daggR ? :lvlRInv : :lvlRDis) for x in getproperty(carVar_ntup,z)]) for z in intersect(relType_tup,(:gen, :use))])
			refLvl_ntup = (Ts = refTs_int, R = refR_int)
		else
			refLvl_ntup = nothing
		end


		push!(rows(mapTechInfo_tab), (allCar = carVar_ntup, invLvl = invLvl_ntup, refLvl = refLvl_ntup, daggR = row.daggR, M = isempty(row.modes) ? [0] : row.modes, Te = row[:Te]))
	end

	return mapTechInfo_tab
end

# XXX map dimension of capacity investment variables for conversion, storage and exchange
function createMapping(name::Val{:invConvStExc}, Set_dic::Dict{Symbol,DataFrame}, Mapping_dic::Dict{Symbol,IndexedTable})

	mapInvCapaConv_tab = table([], [], Int16[], names=[:Ts_inv, :R_inv, :Te])
	mapInvCapaSt_tab = table([], [], [], Int16[], names=[:Ts_inv, :R_inv, :C, :Te])
	mapInvCapaExc_tab = table([], [], [], Int16[], names=[:Ts_inv, :R_a, :R_b, :C])

	# XXX creates all possible dimension for expansion of conversion and storage capacities
	for row in rows(filter(r -> r.type != 0,Mapping_dic[:TechData]))

		techInfo_ntup = filter(r -> r.Te == row[:Te],Mapping_dic[:TechInfo])[1]

		if isempty(techInfo_ntup.allCar) continue end

		TsInv_arr, RInv_arr   = [Set_dic[x][Set_dic[x][:lvl] .== getproperty(techInfo_ntup.invLvl,x),:idx] for x in (:Ts, :R)]

		stCarAll_arr = unique(vcat(map(x -> getproperty(techInfo_ntup.allCar,x),intersect(keys(techInfo_ntup.allCar),(:stExtIn,:stExtOut,:stIntIn,:stIntOut)))...))

		TsRInv_mat = [[i,j] for (i,j) in Iterators.product(TsInv_arr,RInv_arr)]
		TsRInv_arr = hcat(reshape(TsRInv_mat, length(TsInv_arr)*length(RInv_arr))...)

		if !isempty(intersect((:gen,:use),keys(techInfo_ntup.allCar)))
			push!(rows(mapInvCapaConv_tab), ( Ts_inv = TsInv_arr, R_inv = RInv_arr, Te = row[:Te]))
		end

		if !isempty(stCarAll_arr)
			push!(rows(mapInvCapaSt_tab), ( Ts_inv = TsInv_arr, R_inv = RInv_arr, C = stCarAll_arr, Te = row[:Te]))
		end
	end

	mapInvCapaConv_tab = DB.flatten(DB.flatten(mapInvCapaConv_tab,:Ts_inv),:R_inv)
	mapInvCapaSt_tab = DB.flatten(DB.flatten(DB.flatten(mapInvCapaSt_tab,:Ts_inv),:R_inv),:C)

	# XXX creates all posible dimension for expansion of exchange capacities
	carCapaDim_tab = DB.select(Mapping_dic[:C_lvl],(:C, :lvlRDis, :lvlTsInv))
	excFullDim_tab = DB.select(IT.transform(carCapaDim_tab,:R_a => DB.select(carCapaDim_tab,:lvlRDis),:R_b => DB.select(carCapaDim_tab,:lvlRDis)),DB.Not(:lvlRDis))
	excAll_tab = expandSetColumns(excFullDim_tab,(:R_a,:R_b),Set_dic)

	# filters combinations of regions where any parameter regarding exchange capacity is provided
	parExc_tup = (:invExcUp, :invExcLow, :invExcFix, :capaExcUp, :capaExcLow, :capaExcFix, :capaExcResi)
	regExc_tab = table(Int16[],Int16[],Int16[], names = (:R_a, :R_b, :C))

	for par in intersect(parExc_tup,keys(Parameter_dic))
		regExc_tab = merge(regExc_tab,DB.select(matchSetParameter(DB.select(excAll_tab,(:R_a,:R_b,:C)) ,Parameter_dic[par],Set_dic),(:R_a,:R_b,:C)))
	end

	regTsExc_tab = join(excAll_tab,addDummyCol(table(unique(regExc_tab))); lkey = (:R_a,:R_b,:C), rkey = (:R_a,:R_b,:C), rselect = (:R_a,:R_b,:C), how =:inner)
	mapInvCapaExc_tab = rmvDummyCol(DB.reindex(addDummyCol(expandSetColumns(DB.rename(regTsExc_tab,:lvlTsInv => :Ts_inv),(:Ts_inv,),Set_dic)),(:Ts_inv,:R_a,:R_b,:C)))

	return mapInvCapaConv_tab, mapInvCapaSt_tab, mapInvCapaExc_tab
end

# XXX dimension of installed capacities for conversion, storage and exchange
function createMapping(name::Val{:capaConvStExc},Set_dic::Dict{Symbol,DataFrame},Mapping_dic::Dict{Symbol,IndexedTable})

	typeTech_tab = JuliaDB.groupby(unique,Mapping_dic[:TechData],:type; select=:Te)

	# supordinate timestep of dispatch calculatins (like every 5h year of example)
	lvlSupDis_int = maximum(Mapping_dic[:C_lvl].columns.lvlTsInv)
	supDis_arr = Set_dic[:Ts][Set_dic[:Ts][:,:lvl] .== lvlSupDis_int ,:idx]
	cntSupDis_int = length(supDis_arr)

	# creates empty tables for results
	mapCapaConv_tab = table(Int16[], Int16[], Int16[], Int16[], names=[:Ts_inv, :Ts_supDis, :R_inv, :Te])
	mapCapaSt_tab = table(Int16[], Int16[], Int16[], Int16[], Int16[], names=[:Ts_inv, :Ts_supDis, :R_inv, :C, :Te])
	mapCapaExc_tab = table(Int16[], Int16[], Int16[], Int16[], names=[:Ts_supDis, :R_a, :R_b, :C])

	# XXX creates all possible dimension for capacity of conversion and storage
	for row in rows(typeTech_tab)
		allTech_arr = row.unique
		cntTech_int = length(allTech_arr)

		if isempty(allTech_arr) continue end

		# creates array of regional and temporal investment level, stored carriers and if technology does any conversion
		subInfo_tab = filter(r -> r.Te in allTech_arr,Mapping_dic[:TechInfo])
		tempLvl_arr, regLvl_arr  = [map(x -> getproperty(x,y),DB.select(subInfo_tab,:invLvl)) for y in (:Ts,:R)]

		stCarAll_arr = map(x -> unique(vcat(map(y -> getproperty(x,y),intersect(keys(x),(:stExtIn,:stExtOut,:stIntIn,:stIntOut)))...)),DB.select(subInfo_tab,:allCar))
		convBoo_arr =  map(x -> !isempty(vcat(map(y -> getproperty(x,y),intersect(keys(x),(:use,:gen)))...)),DB.select(subInfo_tab,:allCar))

		# determines if there is any conversion or storage technology to cover at all
		anySt_boo = any(map(x -> !isempty(x),stCarAll_arr))
		anyConv_boo = any(convBoo_arr)

		# determines relevant investment regions for each technology by using the respective regional investment level
		relReInv_dic = Dict(x => Set_dic[:R][Set_dic[:R][:,:lvl] .== regLvl_arr[index],:idx] for (index,x) in enumerate(allTech_arr))
		ctrRelReInv_dic = Dict(x => length(relReInv_dic[x]) for x in keys(relReInv_dic))

		# creates permutation of technologies and supordinate dispatch timesteps for conversion
		if anyConv_boo
			convTech_arr = allTech_arr[findall(convBoo_arr)]
			relSupDisTeConv_mat = [[i,j] for (i,j) in Iterators.product(supDis_arr,convTech_arr)]
			relSupDisTeConv_arr = hcat(reshape(relSupDisTeConv_mat, length(convTech_arr)*cntSupDis_int)...)
		end

		# creates permutation of technologies, carriers and supordinate dispatch timesteps for storage
		if anySt_boo
			stTechIdx_arr = findall(map(x -> !isempty(x),stCarAll_arr))
			stTech_arr = allTech_arr[stTechIdx_arr]
			stCar_arr = stCarAll_arr[stTechIdx_arr]

			# different carriers are added to the final array as a last row
			techCar_mat = hcat(stCar_arr,stTech_arr)
			techCar_vew = collect(eachrow(hcat(vcat(techCar_mat[:,1]...), vcat(map(x -> fill(x[2],length(x[1])),eachrow(techCar_mat))...))))

			relSupDisTeCarSt_mat = [[i,j] for (i,j) in Iterators.product(supDis_arr,techCar_vew)]
			relSupDisTeCarSt_arr = hcat(reshape(relSupDisTeCarSt_mat, cntSupDis_int*length(techCar_vew))...)
			relSupDisTeCarSt_arr = vcat(relSupDisTeCarSt_arr[1,:]',  hcat(relSupDisTeCarSt_arr[2,:]...)[2,:]', hcat(relSupDisTeCarSt_arr[2,:]...)[1,:]')
		end


		if row.type == 1 || row.type ==  0
			# adds construction timesteps of zero to matrix for conversion
			if anyConv_boo
				relSupDisTeConstReConv_mat = map(x -> hcat(fill(x[1],ctrRelReInv_dic[x[2]]), fill(x[2],ctrRelReInv_dic[x[2]]), fill(0,ctrRelReInv_dic[x[2]]) ,relReInv_dic[x[2]]),eachcol(relSupDisTeConv_arr))
				relSupDisTeConstReConv_arr::Array{Int16,2} = vcat(relSupDisTeConstReConv_mat...)
			end

			# adds construction timesteps of zero to matrix for storage
			if anySt_boo
				relSupDisTeConstCarReSt_mat = map(x -> hcat(fill(x[1],ctrRelReInv_dic[x[2]]), fill(x[2],ctrRelReInv_dic[x[2]]), fill(0,ctrRelReInv_dic[x[2]]), fill(x[3],ctrRelReInv_dic[x[2]]) ,relReInv_dic[x[2]]),eachcol(relSupDisTeCarSt_arr))
				relSupDisTeConstCarReSt_arr::Array{Int16,2} = vcat(relSupDisTeConstCarReSt_mat...)
			end
		elseif row.type == 2
			# determines temporal investment level for each technology
			relTsInv_dic = Dict(x => tempLvl_arr[index] for (index,x) in enumerate(allTech_arr))

			# create function that loops over supordinate dispatch timesteps and technologies to obtain relevant construction years for conversion
			if anyConv_boo
				relSupDisTeConstConv_mat = map(x -> getConstTs(x,supDis_arr,Set_dic[:Ts],relTsInv_dic[x[2]] == lvlSupDis_int ? nothing : relTsInv_dic[x[2]]),eachcol(relSupDisTeConv_arr))
				relSupDisTeConstConv_arr = vcat(relSupDisTeConstConv_mat...)

				# lastly adds relevant regions depending on the technology for conversion, Final Array: supordinate dispatch timestep, technology, construction timestep, region invest
				relSupDisTeConstReConv_mat = map(x -> hcat(fill(x[1],ctrRelReInv_dic[x[2]]), fill(x[2],ctrRelReInv_dic[x[2]]), fill(x[3],ctrRelReInv_dic[x[2]]) ,relReInv_dic[x[2]]),eachrow(relSupDisTeConstConv_arr))
				relSupDisTeConstReConv_arr = vcat(relSupDisTeConstReConv_mat...)
			end

			# create function that loops over supordinate dispatch timesteps and technologies to obtain relevant construction years for storage
				if anySt_boo
				relSupDisTeConstSt_mat = map(x -> getConstTs(x,supDis_arr,Set_dic[:Ts],relTsInv_dic[x[2]] == lvlSupDis_int ? nothing : relTsInv_dic[x[2]]),eachcol(relSupDisTeCarSt_arr))
				relSupDisTeConstSt_arr = vcat(relSupDisTeConstSt_mat...)

				# lastly adds relevant regions depending on the technology for storage, Final Array: supordinate dispatch timestep, technology, construction timestep, carrier, region invest
				relSupDisTeConstCarReSt_mat = map(x -> hcat(fill(x[1],ctrRelReInv_dic[x[2]]), fill(x[2],ctrRelReInv_dic[x[2]]), fill(x[3],ctrRelReInv_dic[x[2]]), fill(x[4],ctrRelReInv_dic[x[2]]), relReInv_dic[x[2]]),eachrow(relSupDisTeConstSt_arr))
				relSupDisTeConstCarReSt_arr = vcat(relSupDisTeConstCarReSt_mat...)
			end
		end

		# writes concentrated array into table
		if anyConv_boo
			mapCapaConv_tab = merge(mapCapaConv_tab,
				table(relSupDisTeConstReConv_arr[:,1], relSupDisTeConstReConv_arr[:,3], relSupDisTeConstReConv_arr[:,4], relSupDisTeConstReConv_arr[:,2],
																																						names=[:Ts_supDis, :Ts_inv, :R_inv, :Te]))
		end

		if anySt_boo
		mapCapaSt_tab = merge(mapCapaSt_tab,
			table(relSupDisTeConstCarReSt_arr[:,1], relSupDisTeConstCarReSt_arr[:,3], relSupDisTeConstCarReSt_arr[:,5],  relSupDisTeConstCarReSt_arr[:,4], relSupDisTeConstCarReSt_arr[:,2],
																																						names=[:Ts_supDis, :Ts_inv, :R_inv, :C, :Te]))
		end
	end

	# XXX creates all possible dimension for capacity of exchange
	uniExc_tab = table(unique(DB.select(Mapping_dic[:invExc],DB.Not(:Ts_inv))))
	mapCapaExc_tab = rmvDummyCol(DB.reindex(addDummyCol(DB.flatten(IT.transform(uniExc_tab,:Ts_supDis => fill(supDis_arr,length(uniExc_tab))),:Ts_supDis)),(:Ts_supDis,:R_a,:R_b,:C)))

	return mapCapaConv_tab, mapCapaSt_tab, mapCapaExc_tab
end

# XXX determines properties of all dispatch capacity restrictions required for conversion technologies
function createMapping(name::Val{:capaDispRestr},Set_dic::Dict{Symbol,DataFrame},Mapping_dic::Dict{Symbol,IndexedTable})

	capaDispRestr_arr = Array{Tuple{Int16,Symbol,Array{Int16,1},Int16,Int16},1}()

	for row in rows(Mapping_dic[:TechInfo])
		# XXX writes dimension of capacity restrictions for conversion part (even if there are not inputs)
		for side in intersect((:use,:gen),keys(row.allCar))
			# get respective carrier and their reference level
			carDis_arr = map(getproperty(row.allCar,side)) do x
					carRow_ntup = Mapping_dic[:C_lvl][findall(DB.select(Mapping_dic[:C_lvl],:C => r -> r == x))[1]]
					return x, carRow_ntup.lvlTsDis, getproperty(carRow_ntup,row.daggR ? :lvlRInv : :lvlRDis)
			end

			CarConstr_arr = []

			# writes all relevant combinations by going from smallest resolution up, seperately for temporal and spatial (2 and 3)
			for j = [2,3]
				sort!(carDis_arr, by = x -> x[j], rev=true)
				carIt_arr =	map(1:length(carDis_arr)) do x
							if j == 2 ([carDis_arr[y][1] for y in 1:x], carDis_arr[x][2], max(maximum(carDis_arr[y][3] for y in 1:x),row.refLvl.R))
							else ([carDis_arr[y][1] for y in 1:x], max(maximum(carDis_arr[y][2] for y in 1:x),row.refLvl.Ts), carDis_arr[x][3]) end
						end
				# filters entries that exceed the reference level
				carIt_arr = carIt_arr[findall(x -> j == 2 ? x[2] > row.refLvl.Ts : x[3] > row.refLvl.R,carIt_arr)]
				push!(CarConstr_arr,carIt_arr...)
			end

			# adds balance on reference levels, preferably on use side
			if side == :use
				push!(CarConstr_arr,(getproperty(row.allCar,side), row.refLvl.Ts, row.refLvl.R))
			elseif !(:use in keys(row.allCar))
				push!(CarConstr_arr,(getproperty(row.allCar,side), row.refLvl.Ts, row.refLvl.R))
			end

			#filter redundant and "dominated" combinations (less or the same carriers, but not more temporal or spatial detail)
			CarConstrUni_arr = unique(CarConstr_arr)
			CarConstrUni_arr2 = map(i -> map(x -> x[i],CarConstrUni_arr),1:3)
			CarConFilt_arr = filter(CarConstrUni_arr) do x
							!(any((BitArray(issubset(x[1],y) for y in CarConstrUni_arr2[1]) 		.&
							(((x[2] .<= CarConstrUni_arr2[2]) .& (x[3] .< CarConstrUni_arr2[3]))	.|
							((x[2] .< CarConstrUni_arr2[2]) .& (x[3] .<= CarConstrUni_arr2[3])) 	.|
							((x[2] .<= CarConstrUni_arr2[2]) .& (x[3] .<= CarConstrUni_arr2[3]))))	.& BitArray(map(y -> y != x,CarConstrUni_arr)))) end

			CarConFilt_arr2 = map(i -> map(x -> x[i],CarConFilt_arr),1:3)

			typeCapa_sym = side == :use ? :in : :out

			# adds necessary capacity restrictions below reference level
			map(x -> push!(capaDispRestr_arr,(row.Te, typeCapa_sym, CarConFilt_arr2[1][x], CarConFilt_arr2[2][x], CarConFilt_arr2[3][x])),1:length(CarConFilt_arr))
		end

		# XXX writes dimension of capacity restrictions for storage
		stInVar_arr, stOutVar_arr = [intersect(x,keys(row.allCar)) for x in ((:stExtIn,:stIntIn),(:stExtOut,:stIntOut))]
		if !isempty(stInVar_arr) || !isempty(stOutVar_arr)
			allCar_arr = unique(vcat([getproperty(row.allCar,y) for y in union(stInVar_arr,stOutVar_arr)]...))
			for x in allCar_arr
				carRow_ntup = Mapping_dic[:C_lvl][findall(DB.select(Mapping_dic[:C_lvl],:C => r -> r == x))[1]]
				# storage on carrier level, but at least on reference level, if region is disaggregated balance on investment (which is at least lower)
				tsLvl_int = row.refLvl != nothing ? max(row.refLvl.Ts, carRow_ntup.lvlTsDis) : carRow_ntup.lvlTsDis
				rLvl_int  = row.refLvl != nothing ? max(row.refLvl.R, getproperty(carRow_ntup,row.daggR ? :lvlRInv : :lvlRDis)) : getproperty(carRow_ntup,row.daggR ? :lvlRInv : :lvlRDis)
				if !isempty(stInVar_arr) push!(capaDispRestr_arr,(row.Te, :stIn, [x], tsLvl_int, rLvl_int)) end
				if !isempty(stOutVar_arr) push!(capaDispRestr_arr,(row.Te, :stOut, [x], tsLvl_int, rLvl_int)) end
				push!(capaDispRestr_arr,(row.Te, :stSize, [x], tsLvl_int, rLvl_int))
			end
		end
	end

	return DB.rename(table(capaDispRestr_arr),:1 => :Te, :2 => :cnstrType, :3 => :car, :4 => :lvlTs, :5 => :lvlR)
end

# XXX maps all relevant dispatch variables to temporal and spatial levels
function createMapping(name::Val{:dispVar},Set_dic::Dict{Symbol,DataFrame},Mapping_dic::Dict{Symbol,IndexedTable})

	dispVar_arr = Array{Tuple{Symbol,Int16,Int16,Int16,Int16},1}()

	carDisLvl_dic = Dict(x.C => (Ts_dis = x.lvlTsDis , R_dis = x.lvlRDis , R_inv = x.lvlRInv) for x in rows(Mapping_dic[:C_lvl]))

	# XXX creates variables required for the 3 kinds of balancing equations (conversion, storage and market balance)
	for row in rows(Mapping_dic[:TechInfo])
		 relVar_tup = keys(row.allCar)


		# pushes entries for conversion balance
		for convType in intersect(relVar_tup,(:use,:gen,:stIntIn,:stIntOut))
			foreach(x -> push!(dispVar_arr,(convType, x, row.refLvl.Ts, row.refLvl.R, row.Te)::Tuple{Symbol,Int16,Int16,Int16,Int16}),row.allCar[convType])
		end

		# pushes entries for storage and market balance
		for type in relVar_tup
			map(row.allCar[type]) do x
				lvl_tup = carDisLvl_dic[x]
				# storage balance => on carrier level, but at least on reference level
				if type in (:stExtIn, :stExtOut, :stIntIn, :stIntOut)
					tsLvl_int = row.refLvl != nothing ? max(row.refLvl.Ts, lvl_tup.Ts_dis) : lvl_tup.Ts_dis
					rLvl_int  = row.refLvl != nothing ? max(row.refLvl.R, getproperty(lvl_tup,row.daggR ? :R_inv : :R_dis)) : getproperty(lvl_tup,row.daggR ? :R_inv : :R_dis)
					push!(dispVar_arr,(type, x, tsLvl_int, rLvl_int, row.Te)::Tuple{Symbol,Int16,Int16,Int16,Int16})
				end
				# market balance => everything on carrier level
				if type in (:stExtIn, :stExtOut, :gen, :use) push!(dispVar_arr,(type, x, lvl_tup.Ts_dis, lvl_tup.R_dis, row.Te)::Tuple{Symbol,Int16,Int16,Int16,Int16}) end
			end
		end

		# add entries for storage level variable (and capacity constraint restricting it)
		storCar_arr = unique(vcat(map(x -> row.allCar[x] ,intersect((:stExtIn, :stExtOut, :stIntIn, :stIntOut),relVar_tup))...))
		if !isempty(storCar_arr)
			map(storCar_arr) do x
				lvl_tup = carDisLvl_dic[x]
				tsLvl_int = row.refLvl != nothing ? max(row.refLvl.Ts, lvl_tup.Ts_dis) : lvl_tup.Ts_dis
				rLvl_int  = row.refLvl != nothing ? max(row.refLvl.R, getproperty(lvl_tup,row.daggR ? :R_inv : :R_dis)) : getproperty(lvl_tup,row.daggR ? :R_inv : :R_dis)
				push!(dispVar_arr,(:stSize, x, tsLvl_int, rLvl_int, row.Te)::Tuple{Symbol,Int16,Int16,Int16,Int16})
			end
		end
	end

	# XXX creates variables for capacity constraints
	typeConstrVar_dic = Dict(:in => (:use,:stIntOut), :out => (:gen,:stIntIn), :stIn => (:stIntIn,:stExtIn), :stOut => (:stIntOut,:stExtOut), :stSize => (:stSize,))
	for row in rows(Mapping_dic[:capaDispRestr])
		allTypes_tup = Mapping_dic[:TechInfo][findall(DB.select(Mapping_dic[:TechInfo],:Te => x -> x == row.Te))[1]].allCar
		# maps type of varibales used for technology to carriers appearing in respective capacity constraint and adds to array
		typeCar_dic = Dict(x => intersect(getproperty(allTypes_tup,x),row.car) for x in intersect(typeConstrVar_dic[row.cnstrType],keys(allTypes_tup)))
		map(x -> [push!(dispVar_arr,(x, y, row.lvlTs, row.lvlR, row.Te)) for y in typeCar_dic[x]],collect(keys(typeCar_dic)))
	end

	# finds unique entries and convert them into a table
	dispVarUni_arr = unique(dispVar_arr)
	return table(map(y -> map(x -> x[y],dispVarUni_arr),1:5)...; names = (:varType, :C, :lvlTs, :lvlR, :Te))
end

# </editor-fold>


# <editor-fold desc="collection of subfunctions"

# XXX loops over (grand...) children to check for errors in resolution or derive resolution from lower carriers
function evaluateReso(startIdx_arr::Array{Int16,1},parent_tab::IndexedTable,setDic_df::DataFrame,mapCarRes_tab::IndexedTable,carName_str::String)
	# extracts all chidlren and all columns related to resolution
	allChildrenIdx_arr = setDic_df[BitArray([j in startIdx_arr for j in setDic_df[!,:pare]]),:idx]
	allRelCol_arr = filter(x -> x != :C,collect(colnames(mapCarRes_tab)))

	# tries to inherit resolutions from children, if no data exists yet
	if isempty(parent_tab)
		if isempty(allChildrenIdx_arr)
			push!(Report_df,(3,:map,:Car_lvl,"carrier $(carName_str) got no resolution and could not inherit from children either"))
			return
		end
		allRelCol_arr = filter(x -> x != :C,collect(colnames(mapCarRes_tab)))

		RelChildrenTab_arr = filter(y -> length(y) >0 ,map(x -> filter(r -> r.C == x, mapCarRes_tab),allChildrenIdx_arr))

		if isempty(RelChildrenTab_arr)
			evaluateReso(allChildrenIdx_arr,parent_tab,setDic_df,mapCarRes_tab,carName_str)
		else
			maxReso_dic = Dict(y => minimum(map(x -> getproperty(x.columns,y)[1],RelChildrenTab_arr)) for y in allRelCol_arr)
			return maxReso_dic
		end
	# checks if existing resoltuion is flawed
	else
		for childIdx in allChildrenIdx_arr
			child_tab = filter(r -> r.C == childIdx, mapCarRes_tab)
			# goes on to grandchildren, if child is empty
			if isempty(child_tab)
				evaluateReso([childIdx],parent_tab,setDic_df,mapCarRes_tab,carName_str)
			# check if any children got a smaller resolution value
			elseif any(map(x -> (getproperty(child_tab.columns,x)[1] < getproperty(parent_tab.columns,x)[1]),allRelCol_arr))
				 push!(Report_df,(3,:map,:Car_lvl,"carrier $(carName_str) got a resolution more detailed than its childrens'"))
			end
		end
	end
end

# XXX gets relevant construction timesteps depending on supordinate dispatch timestep and on the technology
function getConstTs(supDisTe_arr::SubArray,supDis_arr::Array{Int16,1},setDicTs_df::DataFrame,techInvestLvl_int::Union{Integer,Nothing})
	# hard-coded "1" = investment level of technology, dispatch level = investment level of tech => getHertianceLine ist x
	if isnothing(techInvestLvl_int)
		constTs_arr = filter(x -> (x  <= supDisTe_arr[1]), supDis_arr)
	else
		y = getHeritanceLine(supDisTe_arr[1],setDicTs_df,techInvestLvl_int)
		supDis_arr = setDicTs_df[setDicTs_df[:,:lvl].==techInvestLvl_int,:idx]
		constTs_arr = filter(x -> x <= y,supDis_arr)
	end

	cntConstTsArr_int = length(constTs_arr)
	if length(supDisTe_arr) == 3
		return hcat(fill(supDisTe_arr[1],cntConstTsArr_int),fill(supDisTe_arr[2],cntConstTsArr_int),constTs_arr, fill(supDisTe_arr[3],cntConstTsArr_int))
	else
		return hcat(fill(supDisTe_arr[1],cntConstTsArr_int),fill(supDisTe_arr[2],cntConstTsArr_int),constTs_arr)
	end
end

# </editor-fold>
