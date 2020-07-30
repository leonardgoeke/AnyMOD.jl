add link to API for printIIS function
# Tips

actually read the <a href="../error/#Error-handling">reporting file</a>

[Error handling](@ref)

## Debugging & Performance
- BarConvTool & Crossover option
- obere grenze variable
- "slack" variablen (lss am besten)

## Workflow
- folders
- remember jump model can be expanded
- parameter data can be manipulated after read-in
- choose a coarse temporal resolution for debugging to speed up the process
- do not read in time-series data
- if your model is infeasible and you use Gurobi try AnyMODs `printIIS(obj::anyModel)` function


## Common mistakes
(or at least mistakes that i believe to be common, because i tend to make them)
- input capacity (mit hinweis auf umrechung kosten)
- csv dateiendung
- regions without offshore potential do not have a capacity limit of zero + no availability meaning default is used: unlimited offshore wind with 8760 full load hours
