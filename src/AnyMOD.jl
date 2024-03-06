module AnyMOD

    # ! enforce use of Julia's own python distribution to avoid interference with local python installations
    using Pkg

    using Base.Threads, CSV, Dates, LinearAlgebra, Requires, DelimitedFiles, YAML, CategoricalArrays, Plotly
    using MathOptInterface, Reexport, Statistics, SparseArrays, Suppressor
    @reexport using DataFrames, JuMP, Dates, Suppressor, Distributed, ParallelDataTransfer

    include("objects.jl")
    include("tools.jl")
    include("modelCreation.jl")
    
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

    include("decomposition/objects.jl")
    include("decomposition/algorithm.jl")
    include("decomposition/refinements.jl")


    # general functions and objects
    export anyModel, initializeModel, createOptModel!, setObjective! # basic model functions
    export reportResults, reportTimeSeries, printObject, printDuals, computeResults, writeParameterFile!, plotGraphYML, convertYAML2GEXF # write results
    export plotTree, plotSankeyDiagram, plotNetworkGraph, moveNode!, produceMessage, produceMessageShort # reporting
    export intCol, collapseExp, createVar, defineParameter, makeUp, removeEmptyDic! # low-level data management
    export evaluateHeu, getFeasResult, writeFixToFiles, exportDesignFactors! # functions for heuristic pre-solves
    
    # stochastic optimization
    export algSetup, stabSetup, nearOptSetup, bendersObj, resData # objects
    export buildSub, initializeStab!, prepareMod!, runSub, runTop, updateIteration! # low-level processing

    # ! define function to print subset of infeasible constraints, if gurobi can be used (has to be installed separately)
    function __init__()
        @require Gurobi="2e9cd046-0924-5485-92f1-d5272153d98b" include("dataHandling/gurobiTools.jl")
    end
end