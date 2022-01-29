
using Base.Threads, CSV, Dates, LinearAlgebra, Requires, YAML
using MathOptInterface, Reexport, Statistics, PyCall, SparseArrays
using DataFrames, JuMP, Suppressor

pyimport_conda("networkx","networkx")
pyimport_conda("matplotlib.pyplot","matplotlib")
pyimport_conda("plotly","plotly")

include("src/objects.jl")
include("src/tools.jl")
include("src/modelCreation.jl")
include("src/decomposition.jl")

include("src/optModel/technology.jl")
include("src/optModel/exchange.jl")
include("src/optModel/system.jl")
include("src/optModel/cost.jl")
include("src/optModel/other.jl")
include("src/optModel/objective.jl")

include("src/dataHandling/mapping.jl")
include("src/dataHandling/parameter.jl")
include("src/dataHandling/readIn.jl")
include("src/dataHandling/tree.jl")
include("src/dataHandling/util.jl")

include("src/dataHandling/gurobiTools.jl")

function writeModulation(aggRelCol_dic::Dict{Symbol, Array{Pair}},anyM::anyModel)
   

    allHour_df = DataFrame(hour = collect(1:24))
    allMonth_df = DataFrame(month = collect(1:12))
   
    for c in (:electricity,:districtHeat)

        # get time-series data
        timeSeries_df = reportTimeSeries(c,anyM, rtnOpt = (:rawDf,))
        relCol_arr = filter(x -> !(x in ("Ts_disSup","Ts_dis","R_dis")), names(timeSeries_df))
        foreach(x -> replace!(timeSeries_df[!,x], missing => 0), relCol_arr);

        # map time-steps to month and hour
        allTsDis_arr = sort(unique(timeSeries_df[!,:Ts_dis]))
        timeSeries_df[!,:tsIdx] = map(x -> findall(x .== allTsDis_arr)[1],timeSeries_df[!,:Ts_dis])
        scaCar_int = Int(round(8760/length(allTsDis_arr),digits = 0))
        timeSeries_df[!,:hour] = replace(Int.(floor.(timeSeries_df[!,:tsIdx] .* scaCar_int .% 24)),0 => 24)
        timeSeries_df[!,:month] = map(x -> ceil(x ./ (length(allTsDis_arr)/12)), timeSeries_df[!,:tsIdx])

        # aggregate monthly values
        monthTimeSeries_gdf = groupby(select(timeSeries_df,Not([:Ts_disSup,:Ts_dis,:R_dis,:hour])),[:month])
        month_df = vcat(map(monthTimeSeries_gdf) do x
            out_df = DataFrame(month = x.month[1])
            foreach(y -> out_df[y] = mean(x[y])/scaCar_int,relCol_arr)
            return out_df
        end...)

        # aggregate hourly values
        hourTimeSeries_gdf = groupby(select(timeSeries_df,Not([:Ts_disSup,:Ts_dis,:R_dis,:month])),[:hour])
        hour_df = vcat(map(hourTimeSeries_gdf) do x
            out_df = DataFrame(hour = x.hour[1])
            foreach(y -> out_df[y] = mean(x[y])/scaCar_int,relCol_arr)
            return out_df
        end...)
        
        # aggregate according to defined categories
        for cat in aggRelCol_dic[c]
            month_df[!,cat[1]] .= 0.0
            hour_df[!,cat[1]] .= 0.0

            if !isempty(intersect(names(month_df),cat[2]))
                for y in 1:size(month_df,1)
                    month_df[y,cat[1]] = sum(map(x -> month_df[y,x],intersect(names(month_df),cat[2])))
                end
                select!(month_df,Not(intersect(names(month_df),cat[2])))
            end

            if !isempty(intersect(names(hour_df),cat[2]))
                for y in 1:size(hour_df,1)
                    hour_df[y,cat[1]] = sum(map(x -> hour_df[y,x],intersect(names(hour_df),cat[2])))     
                end
                select!(hour_df,Not(intersect(names(hour_df),cat[2])))
            end

        end

        # sort and remove extra columns
        sort!(month_df,:month)
        sort!(hour_df,:hour)
        select!(month_df,vcat(["month"],intersect(names(month_df),getindex.(aggRelCol_dic[c],1))))
        select!(hour_df,vcat(["hour"],intersect(names(hour_df),getindex.(aggRelCol_dic[c],1))))

        # extend values to all hours
        expHour_dic = Dict(x => collect(x:(x+scaCar_int-1)) for x in hour_df[!,:hour])
        hour_df[!,:hour] = map(x -> expHour_dic[x],hour_df[!,:hour])
        hour_df = flatten(hour_df,:hour)

        allHour_df = hcat(allHour_df,select(hour_df,Not([:hour])))
        allMonth_df = hcat(allMonth_df,select(month_df,Not([:month])))
    end

    # filter columns with small values
    select!(allMonth_df ,Not(filter(x -> x != "month" && abs(sum(allMonth_df[!,x])) < 1e-7, names(allMonth_df))))
    select!(allHour_df ,Not(filter(x -> x != "hour" && abs(sum(allHour_df[!,x])) < 1e-7, names(allHour_df))))
    
    # write profile
    CSV.write("$(anyM.options.outDir)/results_yearlyProfile_$(anyM.options.outStamp).csv", allMonth_df)
    CSV.write("$(anyM.options.outDir)/results_dailyProfile_$(anyM.options.outStamp).csv", allHour_df)

    # compute and write modulation
    for col in filter(x -> x != "month", names(allMonth_df)) allMonth_df[!,col] = mean(allMonth_df[!,col]) |> (x -> abs.(allMonth_df[!,col]) .- abs(x)) end
    for col in filter(x -> x != "hour", names(allHour_df)) allHour_df[!,col] = mean(allHour_df[!,col]) |> (x -> abs.(allHour_df[!,col]) .- abs(x)) end

    CSV.write("$(anyM.options.outDir)/results_yearlyModulation_$(anyM.options.outStamp).csv", allMonth_df)
    CSV.write("$(anyM.options.outDir)/results_dailyModulation_$(anyM.options.outStamp).csv", allHour_df)

end

using Gurobi

b = "C:/Users/pacop/Desktop/work/git/TheModel/" # add the model dir here
input_arr = [b * "_basis",b * "_full",b * "timeSeries/96hours_2008"]
resultDir_str = b * "results"

# create and solve model
anyM = anyModel(input_arr, resultDir_str, objName = "TheModel", supTsLvl = 2, reportLvl = 2, shortExp = 5, emissionLoss = false)
createOptModel!(anyM)
setObjective!(:cost,anyM)


set_optimizer(anyM.optModel, Gurobi.Optimizer)
set_optimizer_attribute(anyM.optModel, "Method", 2);
set_optimizer_attribute(anyM.optModel, "Crossover", 0);
optimize!(anyM.optModel)

writeModulation(aggRelCol_dic,anyM)

reportResults(:summary,anyM, addRep = (:capaConvOut,))
reportResults(:exchange,anyM)
reportResults(:cost,anyM)



reportTimeSeries(:electricity,anyM)
reportTimeSeries(:spaceHeat,anyM)
reportTimeSeries(:districtHeat,anyM)


plotEnergyFlow(:graph,anyM, plotSize= (25.57,14.95), wrtYML = true, wrtGEXF = true)


# all
plotGraphYML(resultDir_str * "/powerAll.yml"; plotSize = (25.57,14.95), fontSize = 16)
plotGraphYML(resultDir_str * "/spaceAndDistrictHeat.yml"; plotSize = (25.57,14.95), fontSize = 16)

plotGraphYML(resultDir_str * "/processHeat.yml"; plotSize = (25.57,14.95), fontSize = 16)
plotGraphYML(resultDir_str * "/all.yml"; plotSize = (25.57,14.95), fontSize = 16)
plotGraphYML(resultDir_str * "/transport.yml"; plotSize = (25.57,14.95), fontSize = 16)
plotGraphYML(resultDir_str * "/powerToX.yml"; plotSize = (25.57,14.95), fontSize = 16)


tSym = :pvOpenspace_b
tInt = sysInt(tSym,anyM.sets[:Te])
part = anyM.parts.tech[tSym]
prepTech_dic = prepSys_dic[:Te][tSym]

anyM.parts.bal