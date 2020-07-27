```@raw html
<style>
table.tabelle2 td {
  padding-left: 0.57em;
  padding-right: 0.57em;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-size: small;
  font-weight: normal;
}
ol.nest {
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

Parameter data is provided in `.csv` csv files starting with `par_` and transferred into `ParElement` objects (LINK). An inheritance algorithm that exploits the tree structure of sets obtains the parameter being used in the model's underlying optimzation problem from the read-in data. This avoids huge but largely redundant input files and allows for lean and flexible models.

# Read-in

In contrast to sets, the naming of parameter files and what data is provided in which file is entirely up to the user. For instance, data for the same parameter can be spread across different files and within one file multiple parameters can be defined. Parameter data is provided in a pivot format where the column `parameter` determines the respective parameter and the column `value` its respective value. Additional columns specify the nodes within the hierarchical trees of sets a value applies to. Therefore names of these coloumns are comprised of the set's name and, if the set is organized in a tree with multiple levels, the number of the respective level.

The `all` keyword can be used analgously to sets (see here for description). Columns that do not contain a set name (i.e. `region`, `timestep`, `carrier`, `technology`, `mode`, or `id`) or keyword (i.e. `parameter` and `value`) are not read in and can be used for documentation. To define multiple parameters within the same row, several numbered `parameter`/`value` columns can be provided (see as example).

As an example, the table below shows the defintion of the discount rate parameter in the example model found in the `par_techInvest.csv` file. (LINKS)
```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_1</strong></td>
<td style="border-right:none"><strong>region_1</strong></td>
<td><strong>region_2</strong></td>
<td style="border-right:none"><strong>parameter</strong></td>
<td style="border-right:none"><strong>value</strong></td>
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

# Inheritance

The cominations of sets or instances parameters are provided for within the input data do not need to match the instances the same parameter is required within the model. The inheritance algorithm automatically obtains the required values from the provided data for each parameter.

MEHR WARUM DAS IN DER ANWENDUNG SO PRAKTISCH IST

### Modes of inheritance

As already discussed when sketching the inheritance algorithm, there are different ways to obtain data from other nodes in the tree. These include:

*upwards*
inherit from the closests ancestral node that is assigned a value

*average*
going downwards in the tree level-by-level and take the average

*sum*
sdfs

*average*
sdfs

*sum*
sdfsd


### Inheritance procedure

As an example, the algorithm is sketched for the parameter discount rates from the demo problem. To this end, the instances actually required for this parameter are given by the table below.

```@raw html
<table class="tabelle2">
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

This table is now populated with data in a 3-step process:
```@raw html
<ol class="nest">
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Direct matches</font-size><br>
<p class="norm"; style = "text-indent:0em">
First, values are assigned where instances of the required data exactly match with the input data. For example, data for `2020` and `WestSouth` is specifically provided and can be used directly.
<table class="tabelle2">
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
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Go along inheritance rules<br>
<p class="norm"; style = "text-indent:0em">
Next, the algorithm consecutively applies the inheritance rules of the respective parameter. For each parameter these rules are documented in the <a href="../parameter_list">parameter list</a>
<ol class="nest">
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$Ts_{sup}$	&#8594; upwards<br>
<p class="norm">
For discount rates, the first rule is to try to obtain additional values by moving upwards in the hierarchical trees of time-steps.
In case of the first row of input data with value `0.0`, the very top node within the tree of time-steps is assigned, because no time-step is specified. Therefore, just based on time-steps this value could now be assigned anywhere data is still missing. However, while applying for all time-steps, the entry does only require to a specifc region, which is `West`. As a result, the value is only inheritited for the entries with matching regions:
<table class="tabelle2">
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
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$R_{exp}$ &#8594; upwards<br>
<p class="norm">
Next, the same concept is applied analgously to regions. By moving up the tree of regions, the input value specified for `East` is now assigned for the descendant regions `EastNorth` and `EastSouth` as well.
<table class="tabelle2">
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
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$Ts_{sup}$ &#8594; average<br>
<p class="norm">
While the previous steps moved upwards the tree to obtain additional data from ancestral nodes, it is also possible to move downwards and assign the average of values defined for ancestral nodes. This would for example be applied, if efficiencies are provided hourly in the input data, but required daily within the model (see the corresponding inheritance rules for the <a href="../parameter_list/#Efficiency-1"> efficiency parameter</a>). In this example, this does not occur, because no data was defined for any descendant nodes.
</p>
</li>
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$R_{exp}$	&#8594; average<br>
<p class="norm">
analgously to the former step.
</p>
</li>
</ol></li>
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Use default value<br>
<p class="norm"; style = "text-indent:0em">
At last, all cases that were not assigned a value via inheritance are assigned the default value of the respective parameter. In case a parameter does not have a default these cases are dropped.
<table class="tabelle2">
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

!!! warning "Do not overuse inheritance"
    Poorly provided input data, especially time-series data, can massively increase the run-time of the inheritance algorithm. For example, if your raw data is provided quarter-hourly, but the most detailed resolution you acutually want to model is hourly, you should aggregate the data to hours yourself before providing it to the model. To define a quarter-hourly resolution below the hourly resolution, feed-in the unaltered data and have the inheritance algorithm aggregate it instead, is possible but highly inefficient.
