# Plots
einleitung

verweis auf color objekte des model objects (und namen)

`plotSize` = (16.0,9.0)

# Node trees

```@raw html
<p style="text-align:center;"><img src="../assets/carrier.png" width="80%"/>
```

```julia
plotTree(tree_sym::Symbol, anyM::anyModel; plotSize = (8.0,4.5),
                                           fontSize = 12, useColor = true, wide = fill(1.0,30))
```


- `tree_sym`
- `fontSize`
- `useColor`
- `wide`

# Energy flow

`plotEnergyFlow`

`useTeColor`

### Node graph

```@raw html
<p style="text-align:center;"><img src="../assets/energyFlowGraph.png"/>
```

```julia
plotEnergyFlow(:graph,anyM::anyModel; plotSize = (16.0,9.0),
                              fontSize = 12, useTeColor = false,
                                  replot = true, scaDist = 0.5, maxIter = 5000, initTemp = 2.0)
```

optik
- `fontSize`
- `useTeColor`

anordnung
- `replot`
- `scaDist`
- `maxIter`
- `initTemp`

```julia
moveNode!(anyM::anyModel,newPos_arr::Array{Tuple{String,Array{Float64,1}},1})
```

### Sankey diagramm

![](assets/sankey.png)

```@raw html
<a href="../assets/sankey_example.html">example</a>
```

```julia
plotEnergyFlow(:sankey,anyM::anyModel; plotSize = (16.0,9.0),
                            minVal = 0.1, filterFunc = x -&gt; true, rmvNode = tuple(),
                                             dropDown = (:region,:timestep), useTeColor = true)
```


praktisch
- `minVal`
- `filterFunc`
- `rmvNode`

anordnung
- `dropDown`
- `useTeColor`

# Styling

`graInfo`

graph::flowGraph
names::Dict{String,String}
colors::Dict{String,Tuple{Float64,Float64,Float64}}
