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
# Model object
```@raw html
<p class="norm">
The <a href="../api/#AnyMOD.anyModel"><code>anyModel</code></a> object is the overarching structure that contains all data and objects of particular energy system model created with AnyMOD.
</p>
```

```julia
anyModel(inDir::Union{String,Array{String,1}},outDir::String; kwargs)
```

## In- and output files
```@raw html
<p class="norm">
The constructor function above has two mandatory arguments that the directories for in- and output files. <code class="language-julia">inDir::Union{String,Array{String,1}}</code> specifies the directory (or directories) of input files. This can either be a string or an array of strings, if input files are spread across different directories.
</p>
<p class="norm">
All <code>.csv</code> files within the provided directories (and their sub-directories) starting with <code>set_</code> or <code>par_</code> will be read-in as an input file. Other files are ignored and can be used for documentation. Within the specific files, only columns whose name contains one of the following keywords are actually read-in: <code>parameter</code>, <code>variable</code>, <code>value</code>, <code>id</code>, <code>region</code>, <code>timestep</code>, <code>carrier</code>, <code>technology</code>, and <code>mode</code>. Other columns can be used freely for documentation.
</p>
```

!!! warning "Reserved keywords"
    Within the input files `all` is a reserved keyword. For an explanation on how it is used, see [Time-steps](@ref).

```@raw html
<p class="norm">
<code class="language-julia">outDir::String</code>: defines the directory of output files. All reporting files including status reports, results, or graphs are written to this directory.
</p>
```

## Optional arguments

Additionally, the constructor accepts a list of optional arguments listed in the table below.

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>argument</strong></td>
<td><strong>explanation</strong></td>
<td><strong>default</strong></td>
<td style="border-right:none"><strong>group</strong></td>
</tr>



<tr>
<td><code>objName</code></td>
<td>
<ul class="liste">
<li>internal name of the model object</li>
<li>added to the name of output files and printed during reporting</li>
</ul>
</td>
<td><code>""</code></td>
<td rowspan="2"; style="border-right:none">data handling</td>
</tr>
<tr>
<td><code>csvDelim</code></td>
<td>
<ul class="liste">
<li>specifies the delimiter used within the read-in csv files</li>
</ul>
</td>
<td><code>","</code></td>
</tr>

<tr>
<td><code>shortExp</code></td>
<td>
<ul class="liste">
<li>interval in years between steps of capacity expansion</li>
</ul>
</td>
<td><code>10</code></td>
<td rowspan="5"; style="border-right:none">model generation</td>
</tr>
<tr>
<td><code>supTsLvl</code></td>
<td>
<ul class="liste">
<li>depth in the tree of time-steps that defines the superordinate dispatch level (usually years)</li>
</ul>
</td>
<td><code>0</code></td>
<tr>
<td><code>redStep</code></td>
<td>
<ul class="liste">
<li>scales down energy quantities within the model, relevant when working <br> with reduced time-series</li>
</ul>
</td>
<td><code>1.0</code></td>
</tr>

<tr>
<td><code>decomm</code></td>
<td>
<ul class="liste">
<li>controls if the model performs endogenous decommissioning, see <a href="https://arxiv.org/abs/2004.10184">Göke (2020)</a> for details</li>
<li>available options are:
<ul style="margin-top:0px">
<li style="margin-top:0px"><code>decomm</code>: capacities are decommissioned endogenously, once decommissioned <br> capacities cannot be put into operation again </li>
<li><code>recomm</code>: no endogenous decommissioning, operated capacities equal installed capacities</li>
<li><code>none</code>: capacities are decommissioned endogenously and can be put back into operation</li>
</ul>
</ul>
</ul>
</td>
<td><code>:decomm</code></td>
</td>
</tr>

<tr>
<td><code>interCapa</code></td>
<td>
<ul class="liste">
<li>capacity expansion can be modelled at a resolution less detailed than yearly, this option <br> determines how capacities are distributed among the subsequent years in this case</li>
<li>available options are:
<ul style="margin-top:0px">
<li style="margin-top:0px"><code>linear</code>:  expansion is equally distributed among years resulting in a linear increase</li>
<li><code>none</code>: all expansion occurs in the first year</li>
</ul>
</ul>
</ul>
</td>
<td><code>:linear</code></td>
</td>
</tr>



<tr>
<td><code>reportLvl</code></td>
<td>
<ul class="liste">
<li>controls the frequency of writing updates to the console</li>
</ul>
</td>
<td><code>2</code></td>
</td>
<td rowspan="3"; style="border-right:none"> <a href="../error/#Error-handling">reporting</a> </td>
</tr>
<tr>
<td><code>errCheckLvl</code></td>
<td>
<ul class="liste">
<li>controls the frequency of checking for errors</li>
</ul>
</td>
<td><code>2</code></td>
</td>
</tr>
</tr>
<tr>
<td><code>errWrtLvl</code></td>
<td>
<ul class="liste">
<li>controls the frequency of writing to the report file</li>
</ul>
</td>
<td><code>1</code></td>
</tr>

<tr>
<td><code>avaMin</code></td>
<td>
<ul class="liste">
<li>availabilities smaller than this value are set to zero</li>
<li>avoids high coefficients in <a href="../constraints/#Conversion-capacity-restriction">conversion</a> and <a href="../constraints/#Storage-capacity-restriction">storage capacity restriction</a>, because availabilities <br> are inversed</li>
</ul>
</td>
<td><code>0.01</code></td>
<td rowspan="6"; style="border-right:none;border-bottom:none"> <a href="../performance/#Performance-and-stability">performance and stability</a> </td>
</tr>

<tr>
<td><code>emissionLoss</code></td>
<td>
<ul class="liste">
<li>determines if losses from exchange and self-discharge of storage are subject to emissions</li>
</ul>
</td>
<td><code>true</code></td>
</tr>


<tr>
<td><code>checkRng</code></td>
<td>
<ul class="liste">
<li>if set, reports all constraints whose range exceeds the specified value</li>
<li>type is <code>Float64</code></li>
</ul>
</td>
<td><code>NaN</code></td>
</tr>


<tr>
<td><code>scaFac</code></td>
<td colspan = "2">
<ul class="liste">
<li>scales different groups of variables within the model</li>
<li>format for argument is <code>(capa = 1e1, oprCapa = 1e2, dispConv = 1e3, ...)</code> </li>
<li>see <a href="../performance/#Scaling">Column scaling</a> for details and defaults</li>
</ul>
</tr>

<tr>
<td><code>coefRng</code></td>
<td colspan = "2">
<ul class="liste">
<li>specifies the maximum range of coefficients in the matrix and right-hand side of the model's underlying <br> optimization problem</li>
<li>format for argument is <code>(mat = (1e-2,1e5), rhs = (1e-2,1e2))</code> </li>
<li>see <a href="../performance/#Scaling">Row scaling</a> for details and defaults</li>
</ul>
</td>
</td>
</tr>


<tr>
<td><code>bound</code></td>
<td colspan = "2">
<ul class="liste">
<li>sets external bounds for all capacities and dispatch variables (in GW) and for the objective value (in Mil. €)</li>
<li>see <a href="../performance/#Variable-limits">Variable limits</a> for details and defaults</li>
</ul>
</td>
</tr>
</tr>

</tbody>
</table>
```

## Fields

```@raw html
Relevant fields of the model object include:
<ul>
<li><code class="language-julia">sets::Dict{Symbol,Tree}</code>: sets defined within the model and their tree structure each saved as an `Tree` objects (see <a href="../sets_and_mappings">Sets and mappings</a> for details)</li>
<li><code class="language-julia">parts::NamedTuple</code>: all parts of the model, these contain the specific parameters, variables, and constraints (see <a href="../parts">Parts</a> for details)</li>
<li><code class="language-julia">report::Array{Tuple,1}</code>: status reports of model execution (see <a href="../error/#Error-handling">Error handling</a> for details)</li>
<li><code class="language-julia">graInfo::graInfo</code>: properties for creation of plots and graphics, can be used to adjust colors and labels (see <a href="../plots/#Styling">Styling</a> for details)</li>
<li><code class="language-julia">optModel::Model</code>: the actual <a href="https://github.com/JuliaOpt/JuMP.jl">JuMP</a> object of the models underlying optimization problem</li>
</ul>
```
