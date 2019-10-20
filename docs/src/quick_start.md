Quick Start
=================

The example project "precompile" is used to introduce the packages’ top-level functions. After adding anyMOD to your project, the function `anyModel` constructs an anyMOD model object by reading in the csv files found within the directory specified by the first argument. The second argument specifies a directory all model outputs are written to. Furthermore, default model options can be replaced via optional arguments.

```
using anyMOD
anyM = anyModel("examples/precompile","output")
```

`addVariables!` and `addConstraints!` determine, which optimization variables and constraints the specific model requires and adds them.

```
addVariables!(anyM)
addConstraints!(anyM)
```

Afterwards, `setObjective!` sets the objective function of the optimization problem. The first argument serves as a key for the respective objective. To enable multi-objective optimization, instead of a single symbol this could also be a dictionary that assigns a respective keyword to its weight in the final objective function. So far only costs have been implemented as an objective.

```
setObjective!(:costs,anyM)
```

Finally, the JuMP model object of is be passed to a solver. Afterwards, the value of optimization variables can be printed to csv files via the `printObject` command.
```
using Gurobi
JuMP.optimize!(anyM.optModel,with_optimizer(Gurobi.Optimizer, OutputFlag=1))
printObject(anyM.sets.variables[:capaConv],anyM.sets , anyM.options)
```
