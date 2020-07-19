# AnyMOD

[AnyMOD.jl](https://github.com/leonardgoeke/AnyMOD.jl) is a [Julia](https://julialang.org/) framework to create large scale energy system models with multiple periods of capacity expansion. It was developed to address the challenges in modelling high-levels of intermittent generation and sectoral integration. Its documentation can be found [here](https://leonardgoeke.github.io/AnyMOD.jl/dev/).

A comprehensive description of the framework's graph based methodology can found in the working paper [Göke (2020), AnyMOD - A graph-based framework for energy system modelling with high levels of renewables and sector integration  ](https://arxiv.org/abs/2004.10184). Its key characteristic is that all sets (time-steps, regions, energy carriers, and technologies) are organized within a hierarchical tree structure. This allows for two unique features:
* The level of temporal and spatial granularity can be varied by energy carrier. For instance, electricity can be modelled with hourly resolution, while supply and demand of gas is balanced daily. As a result, a substantial decrease of computational effort can be achieved. In addition, flexibility inherent to the system, for example in the gas network, can be accounted for.
* The degree to which energy carriers are substitutable when converted, stored, transported, or consumed can be modelled. As an example, residential and district heat can both equally satisfy heat demand, but technologies to produce these carriers are different.

The framework uses [DataFrames](https://juliadata.github.io/DataFrames.jl/stable/) to store model elements and relies on [JuMP](https://github.com/JuliaOpt/JuMP.jl) as a back-end. In addition, Julia's multi-threading capabilities are heavily deployed to increase performance. Since models entirely consist of .csv files, they can be developed open and collaboratively using version control (see [Repositories](@ref) for examples).

# Installation

The current version of [AnyMOD](https://github.com/leonardgoeke/AnyMOD.jl) was developed for [Julia 1.3.1](https://julialang.org/downloads/oldreleases/). AnyMOD is installed by switching into Julia's package mode by typing `]` into the console and then run `add AnyMOD`.

# Getting started

To introduce the packages’ workflow and core functions, a small-scale example model is created, solved and analyzed. The files of this model can either be found in the installation directory of the package (user/.julia/packages/AnyMOD/.../examples) or manually loaded from the GitHub repository.

Before we can start working with AnyMOD it need to be imported via the `using` command. Afterwards, the function `anyModel` constructs an AnyMOD model object by reading in the csv files found within the directory specified by the first argument. The second argument specifies a directory all model outputs are written to. Furthermore, default model options can be overwritten via optional arguments. In this case, the optional argument `objName` is used to name the model "demo". This name will appear during reporting and added to each output file.

```
using AnyMOD
anyM = anyModel("../demo","results", objName = "demo")
```

During the construction process, all input files are read-in and checked for errors. Afterwards sets are mapped to each other and parameter data is assigned to the different model parts. During the whole process status updates are printed to the console and comprehensive reports are written to a dedicated csv file. Since after construction, all qualitative model information, meaning all sets and their interrelations, is written, several graphs describing a models´ structure can be plotted.

```
plotTree(:region,anyM)
plotTree(:carrier,anyM)
plotTree(:tech,anyM, plotSize = (28.0,5.0))
plotEnergyFlow(:graph,anyM)
```

All of these plots will be written to the specified results folder. The first three graphs plotted by `plotTree` show the rooted tree defining the sets of regions, carriers, and technologies, respectively. As an example, the rooted tree for carriers is displayed below.

![](assets/carrier.png)

The fourth graph created by using `plotEnergyFlow` with keyword `:graph` gives a qualitative overview of all energy flows within a model. Nodes either correspond to technologies (grey dots) or energy carriers (colored squares). Edges between technology and energy carrier nodes indicate the carrier is either an input (entering edge) or an output (leaving edge) of the respective technology. Edges between carriers indicate the same relationships as displayed in the tree above.

![](assets/energyFlowGraph.png)

To create the variables and constraints of the model's underlying optimization problem, the model object is passed to the `createOptModel!` function. Afterwards, the `setObjective!` function is used to set the objective function for optimizing. The function requires a keyword input to indicate what is optimized, but so far only `:costs` has been implemented. Again, updates and reports are written to the console and to a dedicated reporting file.

```
createOptModel!(anyM)
setObjective!(:costs,anyM)
```

To actually solve the created optimization problem, the field of the model structure containing the corresponding JuMP object is passed to the functions of the [JuMP](https://github.com/JuliaOpt/JuMP.jl) package used for this purpose. The JuMP package itself is part of AnyMOD’s dependencies and therefore does not have to be added separately, but the solver does. In this case we used Gurobi, but CPLEX or a non-commercial solver could have been used as well.

```
using Gurobi
set_optimizer(anyM.optModel,Gurobi.Optimizer)
optimize!(anyM.optModel)
```

Once a model is solved, results can be obtained and analyzed by the following functions:

```
reportResults(:summary,anyM)
reportTimeSeries(:electricity, anyM)
plotEnergyFlow(:sankey,anyM)
```

Depending on the keyword provided, `reportResults` writes aggregated results to a csv file. `:summary` gives an overview of installed capacities and yearly use and generation of energy carriers. Other keywords available are `:costs` and `:exchange`. `reportTimeSeries` will write the energy balance and the value of each term within the energy balance of the carrier provided as a keyword. Finally, `plotEnergyFlow` used with the keyword `:sankey` creates a sankey diagram that visualizes the quantitative energy flows in the solved model.

![](assets/sankey.png)
