```@raw html
<style>
table.tabelle2 td {
  padding-left: 0.57em;
  padding-right: 0.57em;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-size: small;
}
ul.liste {
  list-style-position: outside;
  padding-left: 1em;
  margin-left: 0em;
  margin-top: 0em;
  white-space: nowrap;
  display:inline-block;
  text-align: left;
}
pre.inline {
   display: inline;
}
</style>
```
# Plots

The graphs that build the conceptual basis for the modelling approach pursued by AnyMOD can be plotted. All plots are created with [plotly](https://github.com/plotly/plotly.py) which is accessed from Julia using [PyCall](https://github.com/JuliaPy/PyCall.jl).


# Node trees

The `plotTree` function is used to plot the hierarchical trees of sets as introduced in [Sets and Mappings](@ref).

```julia
plotTree(tree_sym::Symbol, anyM::anyModel)                                
```


The input `tree_sym` indicates which set should be plotted (`:region`,`:timestep`,`:carrier`, or `:technology`). As an example, the tree for `:carrier` from the [demo problem](https://github.com/leonardgoeke/AnyMOD.jl/tree/master/examples/demo) is plotted below.

```@raw html
<p style="text-align:center;"><img src="../assets/carrier.png" width="80%"/>
```

Optional arguments include:

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>argument</strong></td>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>default</strong></td>
</tr>
<tr>
<td><code>plotSize</code></td>
<td>
<ul class="liste">
<li>length and width of the plot in inches</li>
</ul>
</td>
<td style="border-right:none"><code>(8.0,4.5)</code></td>
</tr>
<tr>
<td><code>fontSize</code></td>
<td>
<ul class="liste">
<li>font size of labels in plot</li>
</ul>
</td>
<td style="border-right:none"><code>12</code></td>
</tr>
<tr>
<td><code>useColor</code></td>
<td>
<ul class="liste">
<li>if <code>true</code>, tries to obtain <a href="#Styling">node specific colors</a>, otherwise the same color is used for all nodes</li>
</ul>
</td>
<td style="border-right:none"><code>true</code></td>
</tr>
<tr>
<td><code>wide</code></td>
<td>
<ul class="liste">
<li>controls ratio of horizontal distances between nodes that are on the same level, but have <br> different ancestors (e.g. distance between 'synthetic gas' and 'natural gas' relative to <br> distance between 'natural gas' and 'district heat')</li>
<li>first element of input array refers to ratio on the first level and so on</li>
</ul>
</td>
<td style="border-right:none"><code>fill(1.0,30)</code></td>
</tr>
</tbody>
</table>
```


# Energy flow

The `plotEnergyFlow` function provides two ways to visualize the flow of energy within a model: Either as a qualitative node graph or as a quantitative Sankey diagram.

### Node graph

To plot a qualitative node graph use the `plotEnergyFlow` command with the `:graph` argument.

```julia
plotEnergyFlow(:graph,anyM::anyModel)
```

Nodes either correspond to technologies (grey dots) or energy carriers (colored squares). Edges between technology and energy carrier nodes indicate the carrier is either an input (entering edge) or an output (leaving edge) of the respective technology. Edges between carriers result from inheritance relationships between carriers. These are included, because, according to AnyMOD's graph-based approach, descendants can satisfy the demand for an ancestral carrier (see [Göke (2020)](https://arxiv.org/abs/2004.10184) for details).

```@raw html
<p style="text-align:center;"><img src="../assets/energyFlowGraph.png"/>
```

The layout of the graph is created using a [force-directed drawing algorithm](https://en.wikipedia.org/wiki/Force-directed_graph_drawing) originally implemented in [GraphLayout.jl](https://github.com/IainNZ/GraphLayout.jl).

In many cases the resulting layout will be sufficient to get an overview of energy flows for debugging, but inadequate for publication. For this reason, the `moveNode!` function can be used to adjust the layout.

```julia
moveNode!(anyM::anyModel,newPos_arr::Array{Tuple{String,Array{Float64,1}},1})
```

`moveNode!` requires an initial layout within that specific nodes are moved. In the example below, an initial layout is created by calling `plotEnergyFlow`. Afterwards, the node for 'ocgt' is moved 0.2 units to the right and 0.1 units up. The node for 'coal' is moved accordingly. Afterwards, the graph is plotted again with the new layout.

```julia
plotEnergyFlow(:graph,anyM)
moveNode!(anyM,[("ocgt",[0.2,0.1]),("coal",[0.15,0.0])])
plotEnergyFlow(:graph,anyM, replot = false)
```

When plotting again, it is important to set the optional `replot` argument to `false`. Otherwise, the original layout algorithm will be run again and overwrite any changes made manually.

Optional arguments for plotting the qualitative node graph are listed below:

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>argument</strong></td>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>default</strong></td>
</tr>

<tr>
<td><code>plotSize</code></td>
<td>
<ul class="liste">
<li>length and width of the plot in inches</li>
</ul>
</td>
<td style="border-right:none"><code>(16.0,9.0)</code></td>
</tr>

<tr>
<td><code>fontSize</code></td>
<td>
<ul class="liste">
<li>font size of labels in plot</li>
</ul>
</td>
<td style="border-right:none"><code>12</code></td>
</tr>

<tr>
<td><code>useTeColor</code></td>
<td>
<ul class="liste">
<li>if <code>true</code>, tries to obtain <a href="#Styling">node specific colors</a> for technologies, otherwise <br> technology nodes are displayed in grey</li>
</ul>
</td>
<td style="border-right:none"><code>false</code></td>
</tr>

<tr>
<td><code>replot</code></td>
<td>
<ul class="liste">
<li>if <code>false</code>, the current layout of the plot is used instead of computing a new one</li>
</ul>
</td>
<td style="border-right:none"><code>true</code></td>
</tr>

<tr>
<td><code>scaDist</code></td>
<td rowspan="3"; style="border-bottom:none">
<ul class="liste">
<li>control parameters for the graph's layout algorithm (see <a href="https://github.com/IainNZ/GraphLayout.jl/blob/master/src/spring.jl">spring.jl</a> for original <br> implementation)</li>
</ul>
</td>
<td style="border-right:none"><code>0.5</code></td>
</tr>

<tr>
<td><code>maxIter</code></td>
<td style="border-right:none"><code>5000</code></td>
</tr>

<tr>
<td><code>initTemp</code></td>
<td style="border-right:none"><code>2.0</code></td>
</tr>

</tbody>
</table>
```



### Sankey diagram

To plot the qualitative energy flows for a solved model use `plotEnergyFlow` command with the `:sankey` argument:

```julia
plotEnergyFlow(:sankey,anyM::anyModel)
```

```@raw html
<p class="norm">
The command will create an entire <a href="../assets/sankey_example.html">html application</a> including a dropdown menu and drag-and-drop capabilities. Below is a screenshot of one graph from the solved <a href="https://github.com/leonardgoeke/AnyMOD.jl/tree/master/examples/demo">demo problem</a>.
</p>
```

![](assets/sankey.png)

Optional arguments for plotting a Sankey diagram are:

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>argument</strong></td>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>default</strong></td>
</tr>

<tr>
<td><code>plotSize</code></td>
<td>
<ul class="liste">
<li>length and width of the plot in inches</li>
</ul>
</td>
<td style="border-right:none"><code>(16.0,9.0)</code></td>
</tr>

<tr>
<td><code>minVal</code></td>
<td>
<ul class="liste">
<li>threshold for energy flows being plotted</li>
</ul>
</td>
<td style="border-right:none"><code>0.1</code></td>
</tr>

<tr>
<td><code>filterFunc</code></td>
<td>
<ul class="liste">
<li>function to filter certain carriers and technologies from graph</li>
<li>for example <code>x -> x.C == 1</code> will only include flows associated to the energy carrier with <br> id 1 (see documentation of <a href="../api/#AnyMOD.Tree"><code>tree objects</code></a> on how to obtain ids)</li>
</ul>
</td>
<td style="border-right:none"><code>x -> true</code></td>
</tr>

<tr>
<td><code>dropDown</code></td>
<td>
<ul class="liste">
<li>for each relevant set of the dimensions specified here separate diagrams are created, sets <br> then appear in the dropdown menu within the html output</li>
<li>for example <code>dropDown = (:timestep)</code> will create diagrams aggregating values <br> across all regions, instead of creating a separate diagram for each region</li>
</ul>
</td>
<td style="border-right:none"><code>(:region,:timestep)</code></td>
</tr>

<tr>
<td><code>rmvNode</code></td>
<td>
<ul class="liste">
<li>removes specified nodes from the Sankey diagram</li>
<li>removal is only possible for nodes that either only have an in- or outgoing flow or exactly <br> one in- and exactly one outgoing flow of equal size</li>
<li>in the diagram above for instance, the nodes for 'district heat' or 'curtailment' could be <br> removed, but not for 'ocgt'</li>
</ul>
</td>
<td style="border-right:none"><code>tuple()</code></td>
</tr>

<tr>
<td><code>useTeColor</code></td>
<td>
<ul class="liste">
<li>if <code>true</code>, tries to obtain <a href="#Styling">node specific colors</a> for technologies, otherwise technology nodes <br> are displayed in grey</li>
</ul>
</td>
<td style="border-right:none"><code>true</code></td>
</tr>

</tbody>
</table>
```

# Styling

The colors and labels of nodes within plots can be adjusted using the `graInfo` field of the model object.

The sub-field `graInfo.names` provides a dictionary that maps node names as specified in the input files to node labels used for plotting. By default, some names occurring in the [demo problem](https://github.com/leonardgoeke/AnyMOD.jl/tree/master/examples/demo) are already assigned labels within the dictionary.

Analogously, `graInfo.colors` assigns colors used for plotting to nodes. Both the actual node name or an assigned label can serve as a key. The assigned value is a tuple of three numbers between 0 and 1 corresponding to a RGB color code.
