module AnyMOD

    # ! enforce use of Julia's own python distribution to avoid interference with local python installations
    using Pkg

    using Base.Threads, CSV, Dates, LinearAlgebra, Requires, DelimitedFiles, YAML, CategoricalArrays, Plotly
    using MathOptInterface, Reexport, Statistics, SparseArrays, Suppressor
    @reexport using DataFrames, JuMP, Dates, Suppressor

    include("objects.jl")
    include("tools.jl")
    include("modelCreation.jl")
    include("decomposition.jl")
    
    include("optModel/technology.jl")
    include("optModel/exchange.jl")
    include("optModel/system.jl")
    include("optModel/cost.jl")
    include("optModel/other.jl")
    include("optModel/objective.jl")
    
    include("dataHandling/mapping.jl")
    include("dataHandling/parameter.jl")
    include("dataHandling/readIn.jl")
    include("dataHandling/tree.jl")
    include("dataHandling/util.jl")

    export anyModel, initializeModel, createOptModel!, setObjective!
    export reportResults, reportTimeSeries, printObject, printDuals, computeResults, writeParameterFile!, plotGraphYML, convertYAML2GEXF
    export plotTree, plotSankeyDiagram, plotNetworkGraph, moveNode!, produceMessage, produceMessageShort
    export intCol, collapseExp, createVar, defineParameter, makeUp, removeEmptyDic!

    export resData, runSub, runTop, deleteCuts!, getConvTol
    export prepareMod!, heuristicSolve, writeResult
    export stabObj, centerStab!, filterStabVar, adjustDynPar!, runTopWithoutStab 

    # ! define function to print subset of infeasible constraints, if gurobi can be used (has to be installed separately)
    function __init__()
        @require Gurobi="2e9cd046-0924-5485-92f1-d5272153d98b" include("dataHandling/gurobiTools.jl")
    end
end
