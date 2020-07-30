```@raw html
<style>
table.tabelle2 td {
  padding-left: 0.57em;
  padding-right: 0.57em;
  border-right: solid 1px;
  border-bottom: none;
  border-color: #dbdbdb;
  font-size: small;
  font-weight: normal;
}
table.tabelle3 td {
  padding-left: 0.57em;
  padding-right: 0.57em;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-size: small;
  font-weight: normal;
}
ol.nest {
  list-style-position: inside;
}
ol.nest2 {
  counter-reset: item;
  list-style-position: inside;
}
li.nest {
  display: block;
}
li.nest li:before {
  content: "2." counter(item, decimal);
  counter-increment: item;
}
li.nest2 li:before {
  content: "2." counter(item, decimal);
  counter-increment: item;
}
p.norm {
font-weight: normal;
font-size: medium;
}
</style>
```
# Parameter overview

```@raw html
<p class="norm">
Parameter data is provided in <code>.csv</code> csv files starting with <code>par_</code>  and transferred into <a href="../api/#AnyMOD.ParElement"><code>ParElement</code></a> objects. The read-in data is used by the inheritance algorithm to compile the parameter data being used in the model's underlying optimization problem. This avoids huge but largely redundant input files and allows for lean and flexible models.
</p>
```

# Read-in
```@raw html
<p class="norm">
In contrast to sets, the naming of parameter files and what data is provided in which file is entirely up to the user. For instance, data for the same parameter can be spread across different files and within one file multiple parameters can be defined.
</p>
```
### Pivot format
```@raw html
<p class="norm">
Parameter data is provided in a pivot format where the column <code>parameter</code> determines the respective parameter and the column <code>value</code> its respective value. Additional columns specify the nodes within the hierarchical trees of sets a value applies to. Therefore, names of these columns are comprised of the set's name and, if the set is organized in a tree with multiple levels, the number of the respective level.
</p>

<p class="norm">
The <code>all</code> keyword can be used analogously to sets (see <a href="../sets/#Time-steps">Time-steps</a>). Columns that do not contain a set name (i.e. <code>region</code>, <code>timestep</code>, <code>carrier</code>, <code>technology</code>, <code>mode</code>, or <code>id</code>) or keyword (i.e. <code>parameter</code> and <code>value</code>) are not read in and can be used for documentation. To define multiple parameters within the same row, several numbered <code>parameter</code>/<code>value</code> columns can be provided (see <a href="https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/par_techCost.csv"><code>par_techCost.csv</code></a>).
</p>


<p class="norm">
As an example, the table below shows the definition of the <a href="../parameter_list/#Discount-rate-1">discount rate</a> parameter in the demo model found in the <a href="https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/par_techInvest.csv"><code>par_techInvest.csv</code></a> file.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>region_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>region_2</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none"></td>
<td style="border-right:none">West</td>
<td></td>
<td style="border-right:none">rateDisc</td>
<td style="border-right:none;text-align:center">0.0</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">West</td>
<td>WestSouth</td>
<td style="border-right:none">rateDisc</td>
<td style="border-right:none;text-align:center">0.015</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">East</td>
<td></td>
<td style="border-right:none">rateDisc</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
</tbody>
</table>
```
In case, a set is included in the dimensions of a parameter, but no node of that set is assigned for a respective value, the very top node of the tree is assigned instead.

### Multiple dependencies on same set

```@raw html
<p class="norm">
Parameter can depend on multiple instances of the same set. In case of an <code>emerging</code> technology (see section on optional mappings for <a href="../sets/#Technologies">technologies</a>)
<a href="..#Efficiency">efficiency</a> for instance can depend on two different kinds of time-steps: $Ts_{dis}$, the time-step a technology is being used, and $Ts_{exp}$, the time-step a technology was built. Following the order of sets in the definition, the first time-step specified in the input data will always relate to $Ts_{dis}$ and the second to expansion $Ts_{exp}$.
</p>

<p class="norm">
Accordingly, in table below the first column relates to $Ts_{dis}$ and the second to $Ts_{exp}$. Consequently, the specified efficiency applies to the first hour of every year for all <code>heatpumps</code> constructed <code>2020</code>.
</p>
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_4</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>technology_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">h0001</td>
<td style="border-right:none">2020</td>
<td>heatpump</td>
<td style="border-right:none">effConf</td>
<td style="border-right:none;text-align:center">5.0</td>
</tr>
</tbody>
</table>
<p class="norm">
This concept equally applies if one of the sets is defined by more then one column. In the table below, the first and the second column are attributed to specify $Ts_{dis}$. Since the third column goes back up to the first level, AnyMOD realizes it refers to a different dimension and attributes it to $Ts_{exp}$. As a result, both efficiencies apply to <code>heatpumps</code> constructed <code>2020</code>, but one row relates to the first hour of <code>2020</code> and the other to the first hour of <code>2030</code>. So, this example also shows how the aging process of technologies can be modelled.
</p>
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_4</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>technology_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">h0001</td>
<td style="border-right:none">2020</td>
<td>heatpump</td>
<td style="border-right:none">effConf</td>
<td style="border-right:none;text-align:center">5.0</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">h0001</td>
<td style="border-right:none">2020</td>
<td>heatpump</td>
<td style="border-right:none">effConf</td>
<td style="border-right:none;text-align:center">4.5</td>
</tr>
</tbody>
</table>
```

# Inheritance
```@raw html
<p class="norm">
The combinations of sets or instances parameters are provided for within the input data do not need to match the instances required within the model. The required values are automatically compiled from the  data provided. This facilitates the provision of input data and allows for a high level of flexibility in modelling. </p>

<p class="norm">
If, for example, for one technology <a href="../parameter_list/#Efficiency-1">efficiency</a> should be dependent on the time-step and for another irrespective of the time-step, this can simply be achieved in the model by providing efficiencies with an hourly granulation in the first and without specifying the temporal domain in the other case. In the demo problem for instance, heat-pump efficiencies are provided hourly in <a href="https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/timeseries/par_heatpumps.csv"><code>par_heatpumps.csv</code></a> while for all other technologies <a href="https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/par_techDispatch.csv"><code>par_techDispatch.csv</code></a> defines efficiencies without specifying a dispatch time-step. </p>
```

!!! warning "Do not overuse inheritance"
    Poorly provided input data, especially time-series data, can massively increase the run-time of the inheritance algorithm. For example, if your raw data is provided quarter-hourly, but the most detailed resolution you actually want to model is hourly, you should aggregate the data to hours yourself before providing it to the model. To define a quarter-hourly resolution below the hourly resolution, feed-in the unaltered data and have the inheritance algorithm aggregate it instead, is possible but highly inefficient.

### Modes of inheritance

When data is required but not defined for a specific combination of nodes, the algorithm moves along the vertices of the set trees to derive it. There are four different modes to do this, that will be explained based on the graph below. It shows the hierarchical tree of time-steps in the demo problem with data specified for some nodes (green), but not for the one required (red circle).

```@raw html
<img src="../assets/inheritance.png" width="80%"/>

<p class="norm">
<ul>
<li><em><strong>upwards</em></strong><br>Moving upwards the tree until a node with data is reached. In the example this means to assign <code>d001</code> the value <code>8.3</code>. If data would already be provided for <code>2020</code>, the direct ancestor of <code>d001</code>, this value would be used instead. </li>
<li><em><strong>average</em></strong><br>Moving downward until nodes with some data are reached and take the average of those value. In the example this means to assign <code>d001</code> the value <code>2.9</code> ($=\frac{2.7 + 3.1}{2}$).
</li>
<li><em><strong>sum</em></strong><br> Moving downward until nodes with some data are reached and take the sum of those value. In the example this means to assign <code>d001</code> the value <code>5.8</code> ($=2.7 + 3.1$). </li>
<li><em><strong>sum*</em></strong><br> Moving downward until nodes who <u>all</u> have data assigned are reached and take the sum of those value. In the example this means to assign <code>d001</code> the value <code>16.6</code> ($=2.1+6.8+4.5+3.2$).</li>
</ul>
</p>
```


### Inheritance algorithm

```@raw html
<p class="norm">
The actual algorithm is outlined for the example of the <a href="../parameter_list/#Discount-rate-1">discount rate</a> parameter from the demo problem. The instances of the parameter required with in the model are given below, the provided input data was shown above.
</p>

<table class="tabelle3">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_superordinate_dispatch</strong></td>
<td style="border-right:none"><strong>region_expansion</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">West < WestNorth</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">West < WestNorth</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">West < WestSouth</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">West < WestSouth</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">East < EastNorth</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">East < EastNorth</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">East < EastSouth</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">East < EastSouth</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">East</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">East</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none">West</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">West</td>
</tr>
</tbody>
</table>
```
Three steps are taken to populate the table with the required data:
```@raw html
<ol class="nest"; >
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbspDirect matches</font-size><br>
<p class="norm">
First, values are assigned where instances of the required data exactly match the provided input data. For example, data for <code>2020</code> and <code>WestSouth</code> is specifically provided and can be used directly.
<table class="tabelle3">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_superordinate_dispatch</strong></td>
<td><strong>region_expansion</strong></td>
<td style="border-right:none"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.015</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.03</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West</td>
<td style="border-right:none;text-align:center"></td>
</tr>
</tbody>
</table>
</p>
</li>
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbspGo along inheritance rules<br>
<p class="norm"; style = "text-indent:0em">
Next, the algorithm consecutively applies the inheritance rules of the respective parameter. For each parameter these rules are documented in the <a href="../parameter_list">parameter list</a>. These rules assign sets to the modes of inheritance introduced above.
<ol class="nest2">
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$Ts_{sup}$	&#8594; <em>upwards</em><br>
<p class="norm">
For discount rates, the first rule is to try to obtain additional values by moving upwards in the hierarchical trees of time-steps. In the first row of input data, a value was provided irrespective of time-step, but only for the region <code>West</code>. This value is now assigned to the missing entries of that region.
<table class="tabelle3">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_superordinate_dispatch</strong></td>
<td><strong>region_expansion</strong></td>
<td style="border-right:none"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center">0.015</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.00</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.00</strong></td>
</tr>
</tbody>
</table>
</li>
</p>
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$R_{exp}$ &#8594; <em>upwards</em><br>
<p class="norm">
Next, the concept is analogously applied to regions. By moving up the tree the value provided for <code>East</code> is now assigned for the descendant regions <code>EastNorth</code> and <code>EastSouth</code> as well.
<table class="tabelle3">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_superordinate_dispatch</strong></td>
<td><strong>region_expansion</strong></td>
<td style="border-right:none"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.00</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.00</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.00</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center">0.015</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.03</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.03</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East</td>
<td style="border-right:none;text-align:center"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
</tbody>
</table>
</p>
</li>
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$Ts_{sup}$ &#8594; <em>average</em> and $R_{exp}$	&#8594; <em>average</em><br>
<p class="norm">
The <em>average</em> mode tries to inherit values from descendant carriers. In this case, for none of these any data is defined, and consequently no additional data can be compiled.
</p>
</li>
</ol></li>
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbspUse default value<br>
<p class="norm"; style = "text-indent:0em">
Finally, for all cases where no data was assigned, the default value is used instead. In case a parameter does not have a default, cases are dropped.
<table class="tabelle3">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_superordinate_dispatch</strong></td>
<td><strong>region_expansion</strong></td>
<td style="border-right:none"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestNorth</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West < WestSouth</td>
<td style="border-right:none;text-align:center">0.015</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.02</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastNorth</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.02</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East < EastSouth</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>East</td>
<td style="border-right:none;text-align:center"><strong style="color:#60ad51">0.02</strong></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>East</td>
<td style="border-right:none;text-align:center">0.03</td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td>West</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td>West</td>
<td style="border-right:none;text-align:center">0.00</td>
</tr>
</tbody>
</table>
</p>
</li>
</ol>
```
