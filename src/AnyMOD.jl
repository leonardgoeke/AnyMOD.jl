module AnyMOD

    # ! enforce use of Julia's own python distribution to avoid interference with local python installations
    using Pkg
    # save current value of environment variable
    if "PYTHON" in keys(ENV)
        envPy = ENV["PYTHON"]
    else
        envPy = ""
    end
    # build python package with Julia distribution
    ENV["PYTHON"]=""
    Pkg.build("PyCall")
    # re-sets environment to former status
    if envPy == ""
        delete!(ENV,"PYTHON")
    else
        ENV["PYTHON"] = envPy
    end

    using Base.Threads, CSV, Dates, LinearAlgebra, Requires, DelimitedFiles
    using MathOptInterface, Reexport, Statistics, PyCall, SparseArrays, Suppressor
    @reexport using DataFrames, JuMP, Dates, Suppressor

    pyimport_conda("networkx","networkx")
    pyimport_conda("matplotlib.pyplot","matplotlib")
    pyimport_conda("plotly","plotly")

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
    export reportResults, reportTimeSeries, printObject, printDuals, writeParameterFile!
    export plotTree, plotEnergyFlow, moveNode!, produceMessage
    export intCol, collapseExp, createVar, defineParameter, makeUp, removeEmptyDic!

    export trustRegion, bendersData, quadTrust
    export prepareMod!, runSubLevel, runTopLevel, addCuts!, computeFeas, writeResult, writeFixToFiles
    export heuristicSolve, getFeasResult, evaluateHeu, filterQtrVar, addLinearTrust!
    export runTopWithoutQuadTrust, adjustQuadTrust, centerQuadTrust, checkLinearTrust, adjustLinearTrust!
    

    # ! define function to print subset of infeasible constraints, if gurobi can be used (has to be installed separately)
    function __init__()
        @require Gurobi="2e9cd046-0924-5485-92f1-d5272153d98b" include("dataHandling/gurobiTools.jl")
    end
end
