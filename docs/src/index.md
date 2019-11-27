anyMOD
=================

[anyMOD.jl](https://github.com/leonardgoeke/anyMOD.jl) is a [Julia](https://julialang.org/) framework to set up large scale linear energy system models with a focus on multi-period capacity expansion. It was developed to address the challenges in modelling high-levels of intermittent generation and sectoral integration.

The framework's key characteristic is, that all sets (time-steps, regions, energy carriers, and technlogies) are each organized within a hierarchical tree structure. This allows for several unique features:

* The spatial and temporal resolution at which generation, use and transport of energy is modelled can be varied by energy carrier. For example, within the same model electricity can be modelled at an hourly, but gas at a daily resolution, while still allowing for technologies that convert gas to electricity, or vice versa. As a result, a substantial decrease of computational effort can be achieved.
* The substitution of energy carriers with regard to conversion, consumption and transport can be modelled. As an example, residential and district heat can both equally satisfy overall heat demand, but technologies to produce these carriers and how they are constrained are different.
* Inheritance within the trees can be exploited to dynamically obtain the model's parameters from the input data provided. In case of a technology’s efficiency for instance, parameters can vary by hour, day or be irrespective of time, depending on whether input data was provided hourly, daily or without any temporal dimension specified.

The tool relies on the [JuMP](https://github.com/JuliaOpt/JuMP.jl) package to create optimization problems and uses [JuliaDB](https://juliadb.org/) to store and process their elements.


Quick Start
=================

The example project "demo" is used to introduce the packages’ top-level functions. After adding anyMOD to your project, the function `anyModel` constructs an anyMOD model object by reading in the csv files found within the directory specified by the first argument. The second argument specifies a directory all model outputs are written to. Furthermore, default model options can be overwritten via optional arguments. In this case, the distance between investment time-steps is set to 10 instead of 5 years as per default and the level of reporting is extended to 3 to produce more detailed output messages.

```
using anyMOD
anyM = anyModel("examples/demo","output"; shortInvest = 10, reportLvl = 3)
```

`addVariables!` and `addConstraints!` determine, which optimization variables and constraints the specific model requires and adds them.

```
addVariables!(anyM)
addConstraints!(anyM)
```

Afterwards, `setObjective!` sets the objective function of the optimization problem. The first argument serves as a key for the respective objective. To enable multi-objective optimization, instead of a single symbol this can also be a dictionary that assigns a respective keyword to its weight in the final objective function. So far only costs have been implemented as an objective.

```
setObjective!(:costs,anyM)
```

Finally, the JuMP model object of is be passed to a solver. Afterwards, the value of optimization variables can be printed to csv files via the `printObject` command.
```
using Gurobi
JuMP.optimize!(anyM.optModel,with_optimizer(Gurobi.Optimizer, OutputFlag=1, Method = 1))
printObject(anyM.variables[:capaConv],anyM.sets , anyM.options)
```
