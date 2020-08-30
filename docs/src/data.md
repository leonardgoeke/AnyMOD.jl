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
</style>
```

# Data files

AnyMOD includes several functions to obtain the results of a solved model.

# Analysed results

Different analysis can be printed with the `reportResults` functions depending on the `reportType` keyword.

```julia
reportResults(reportType::Symbol, model_object::anyModel; rtnOpt::Tuple = (:csv,))
```

The keyword argument `rtnOpt` controls the output format. Available options are:

- `:csv`: writes a "readable" `.csv` file
- `:csvDf`: returns the same "readable" data as a DataFrame
- `:raw`: writes a `.csv` file with "raw" data, this means sets are not indicated by their name, but their internal id
- `:rawDf`: returns the same "raw" data as a DataFrame


### Summary

Using the `reportType` keyword `:summary` will provide a general overview of results. If the optional argument `wrtSgn` is set to `true`, output quantities (e.g. use or storage input) are given a negative sign. The table below lists all variables included.

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>definition</strong></td>
</tr>
<tr>
<td><code>exp{Conv/StIn/StOut/StSize}</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Expansion-1">expansion variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>capa{Conv/StIn/StOut/StSize}</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Installed-capacity-1">installed capacity variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>oprCapa{Conv/StIn/StOut/StSize}</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated capacity variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>demand</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../parameter_list/#Demand-1">demand parameter</a></li>
</ul></td>
</tr>
<tr>
<td><code>use</code> & <code>gen</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../variables/#Generation-and-use-1">generation and use variables</a></li>
</ul></td>
</tr>
<tr>
<td><code>stIn</code> & <code>stOut</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../variables/#Charging-and-discharging-1">charging and discharging variables</a> (both in- and external)</li>
</ul></td>
</tr>
<tr>
<td><code>stExt{In/Out}</code> & <code>stInt{In/Out}</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../variables/#Charging-and-discharging-1">charging and discharging variables</a></li>
</ul></td>
</tr>
<tr>
<td><code>import</code> & <code>export</code></td>
<td style="border-right:none"><ul class="liste">
<li>net aggregation of <a href="../variables/#Exchange-1">exchange variables</a></li>
</ul></td>
</tr>
<tr>
<td><code>trdBuy</code> & <code>trdSell</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../variables/#Buy-and-sell-1">buy and sell variables</a></li>
</ul></td>
</tr>
<tr>
<td><code>crt</code> & <code>lss</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated <a href="../variables/#Curtailment-and-loss-of-load-1">curtailment and loss-of-load variables</a></li>
</ul></td>
</tr>
<tr>
<td><code>emission</code></td>
<td style="border-right:none"><ul class="liste">
<li>aggregated emissions</li>
<li>defined as $emFac \cdot \bm{use} \cdot 10^3$</li>
</ul></td>
</tr>
<tr>
<td><code>flhConv</code></td>
<td style="border-right:none">
<ul class="liste">
<li>measure of conversion capacity utilization</li>
<li>defined as $\frac{oprCapaConv}{\sum use + stIntOut}$ or $\frac{oprCapaConv}{\sum gen + stIntIn}$ if no carriers are used</li>
</ul>
</td>
</tr>
<tr>
<td><code>flhStIn</code></td>
<td style="border-right:none">
<ul class="liste">
<li>measure of capacity utilization regarding storage input</li>
<li>defined as $\frac{oprCapaStIn}{\sum stExtIn + stIntIn}$</li>
</ul>
</td>
</tr>
<tr>
<td><code>flhStOut</code></td>
<td style="border-right:none">
<ul class="liste">
<li>measure of capacity utilization regarding storage output</li>
<li>defined as $\frac{oprCapaStOut}{\sum stExtOut + stIntOut}$</li>
</ul>
</td>
</tr>
<tr>
<td><code>cycStIn</code></td>
<td style="border-right:none">
<ul class="liste">
<li>measure to characterize utilization of storage size based on charged quantities</li>
<li>small values indicate long- and large values short-term storage</li>
<li>defined as $\frac{oprCapaStSize}{\sum stExtIn + stIntIn}$</li>
</ul>
</td>
</tr>
<tr>
<td><code>cycStOut</code></td>
<td style="border-right:none">
<ul class="liste">
<li>measure to characterize utilization of storage size based on discharged quantities</li>
<li>small values indicate long- and large values short-term storage</li>
<li>defined as $\frac{oprCapaStSize}{\sum stExtOut + stIntOut}$</li>
</ul>
</td>
</tr>
</tbody>
</table><h3 id="Exchange"><a class="docs-heading-anchor" href="#Exchange">Exchange</a><a id="Exchange-1"></a><a class="docs-heading-anchor-permalink" href="#Exchange" title="Permalink"></a></h3><p>The keyword <code>:exchange</code> gives detailed results on exchange capacities and quantities. Again, reported variables are listed below.</p><table class="tabelle2">
<tbody>
<tr>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>definition</strong></td>
</tr>
<tr>
<td><code>expExc</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Expansion-1">exchange expansion variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>capaExc</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Installed-capacity-1">installed exchange capacity variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>oprCapaExc</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated exchange capacity variable</a></li>
</ul></td>
</tr>
<tr>
<td><code>flhExc</code></td>
<td style="border-right:none"><ul class="liste">
<li>utilization of exchange capacities</li>
<li>defined as $\frac{oprCapaExc}{exc}$</li>
</ul></td>
</tr>
<tr>
<td><code>exc</code></td>
<td style="border-right:none"><ul class="liste">
<li><a href="../variables/#Exchange-1">exchange variables</a></li>
</ul></td>
</tr>
</tbody>
</table>
```


### Costs

```@raw html
The keyword <code>:costs</code> provides the values of all <a href="../variables/#Costs">cost variables</a>. All costs are provided in million Euros.
```

# Time-series

The `reportTimeSeries` function writes a table with the values of all elements occuring in the energy balance of a respective `carrier`.

```julia
reportTimeSeries(carrier::Symbol, model_object::anyModel)
```

Optional arguments include:

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>argument</strong></td>
<td><strong>explanation</strong></td>
<td><strong>default</strong></td>
</tr>



<tr>
<td><code>filterFunc</code></td>
<td>
<ul class="liste">
<li>function to filter certain time-series data</li>
<li>for example <code>x -> x.R_dis == 1</code> will only provide time-series data for the region with id 1 (see <br> documentation of <a href="../api/#AnyMOD.Tree"><code>tree objects</code></a> on how to obtain ids)</li>
</ul>
</td>
<td><code>x -> true</code></td>
</tr>

<tr>
<td><code>unstck</code></td>
<td>
<ul class="liste">
<li>controls if data is provided as an unstacked table or in pivot format</li>
</ul>
</td>
<td><code>true</code></td>
</tr>

<tr>
<td><code>signVar</code></td>
<td>
<ul class="liste">
<li>specifies groups of variables to write to output table</li>
<li><code>:in</code> refers to energy inputs (e.g. <a href="../variables/#Generation-and-use-1">generated</a> or <a href="../variables/#Buy-and-sell-1">bought</a> quantities) and <code>:out</code> refers to energy out- <br> puts (e.g. <a href="../variables/#Generation-and-use-1">used</a>, <a href="../parameter_list/#Demand-1">demanded</a>, or  <a href="../variables/#Buy-and-sell-1">sold</a> quantities)</li>
</ul>
</td>
<td><code>(:in,:out)</code></td>
</tr>

<tr>
<td><code>mergeVar</code></td>
<td>
<ul class="liste">
<li>if set to <code>false</code> results for energy in- and output are written to separate files</li>
</ul>
</td>
<td><code>true</code></td>
</tr>

<tr>
<td><code>minVal</code></td>
<td>
<ul class="liste">
<li>threshold for values to be included</li>
<li>useful to filter out really small but non-zero values that result from using Barrier without Crossover</li>
</ul>
</td>
<td><code>1e-3</code></td>
</tr>

<tr>
<td><code>rtnOpt</code></td>
<td>
<ul class="liste">
<li>controls the output format as documentated for <a href="#Analysed-results">Analysed results</a></li>
</ul>
</td>
<td><code>(:csv,)</code></td>
</tr>


</tbody>
</table>
```


# Individual elements

```@raw html
<p class="norm">
In addition to the <code>reportResults</code> and <code>reportTimeSeries</code> that aggregate various model elements and report on them, individual variables or constraints can also be printed directly. In this case, the DataFrames used to store variables and constraints within the <a href="../api/#AnyMOD.TechPart"><code>model part</code></a> objects serve as inputs.
</p>
```

The `printObject` function writes a copy of the respective inputted DataFrame, but replaces the internal node ids with their written name.  
```julia
printObject(element::DataFrame, model_object::anyModel)
```
For variables, the table will provide their value and for constraints the corresponding constraint expression.


The `printDuals` function works analogously, but returns the duals or shadow prices for the respective elements.  
```julia
printDuals(element::DataFrame, model_object::anyModel)
```

```@raw html
<p class="norm">
For both functions the optional arguments <code>filterFunc</code> and <code>rtnOpt</code> as introduced for <a href="#Analysed-results">Analysed results</a> and <a href="#Time-series">Time series</a> are available. In addition, the argument <code>fileName</code> can be used to specify the name of the output file.
</p>
```
