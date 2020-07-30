```@raw html
<style>
table.tabelle td {
  border-left: 1px solid;
  border-color: #dbdbdb;
}
table.tabelle td:first-child {
  border-right: 2.5px solid;
  border-color: #dbdbdb;
  border-left: none;
}
ul.liste {
  list-style-position: inside;
  margin-left: 0em;
  margin-top: 0em;
  white-space: nowrap;
  display:inline-block;
  text-align: left;
}
</style>
```
# Variables

In the following, all variables used in AnyMOD are listed. Information includes the name used throughout the model, the variables' unit, and its dimensions according to the symbols introduced in [Sets and Mappings](@ref). Also, it is specified what determines the instances a variable is actually created for, in which constraints a variable appears, and the model part it is assigned to.

```@raw html
<p class="norm">
To increase performance, AnyMOD stores variables within DataFrames instead of using JuMPs native containers. Each variable dimension is represented by a column and integers in these columns relate to nodes within the hierarchical trees of sets (see <a href="../data/#printObject"><code>printObject</code></a> on how to export these in a readable format). An additional <code>var</code> column stores the corresponding variables. These variables are not JuMP variable objects, but JuMP expressions, that already include <a href="../performance/#Scaling">scaling factors</a>.
</p>
```

All variables are defined positive.

# Dispatch of technologies

### Generation and use

Quantities generated and used by technology.

```@raw html
<p class="norm">
To reduce model size variables are not created, if the <a href="../parameter_list/#Availability-1">availability</a> is zero. Also, mode dependant variables are only created where required. For example, for a technology with mode specific <a href="../parameter_list/#Ratios-of-carrier-use-and-generation-1">generation ratios</a>, different variables for each mode are only created for <code>gen</code>, but not for <code>use</code>.  
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>gen</td>
<td>use</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $C$, $Te$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:center"><ul class="liste">
<li>dispatch resolution of carrier</li>
<li>expansion time-steps of technology</li>
<li>relevant modes of technology</li>
<li><a href="../parameter_list/#Availability-1">respective availability</a> exceeds zero</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
<li><a href="../constraints/#Conversion-balance-1">conversion balance</a></li>
<li><a href="../constraints/#Conversion-capacity-restriction-1">conversion capacity restriction</a></li>
<li><a href="../constraints/#Energy-ratio-restriction-1">energy ratio restriction</a></li>
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td  colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

### Charging and discharging

Externally and internally charged and discharged quantities (see [Technologies](@ref) for explanation).

```@raw html
<p class="norm">
To reduce model size variables are not created, if the <a href="../parameter_list/#Availability-1">availability</a> is zero. Also, mode dependant variables are only created where required.  
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stExt{In/Out}</td>
<td>stInt{In/Out}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $C$, $Te$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td  colspan="2"; style="text-align:center"><ul class="liste">
<li>dispatch resolution of carrier</li>
<li>expansion time-steps of technology</li>
<li>relevant modes of technology</li>
<li><a href="../parameter_list/#Availability-1">respective availability</a> exceeds zero</li>
</ul></td>
</tr>
<tr>
<td rowspan="2"><strong>related constraints</strong></td>
<td colspan="2"; style= "text-align:left;border-bottom:none;padding-bottom:0px">
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
<li><a href="../constraints/#Storage-balance-1">storage balance</a></li>
<li><a href="../constraints/#Storage-capacity-restriction-1">storage capacity restriction</a></li>
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste">
</ul>
</td>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste">
<li><a href="../constraints/#Conversion-balance-1">conversion balance</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td  colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

### Storage level

Quantity stored by storage system.

```@raw html
<p class="norm">
To reduce model size variables are not created, if the <a href="../parameter_list/#Availability-1">availability</a> is zero. Also, mode dependant variables are only created where required.  
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stLvl</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $C$, $Te$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>dispatch resolution of carrier</li>
<li>expansion time-steps of technology</li>
<li>relevant modes of technology</li>
<li><a href="../parameter_list/#Availability-1">availability</a> exceeds zero</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Storage-balance-1">storage balance</a></li>
<li><a href="../constraints/#Storage-capacity-restriction-1">storage capacity restriction</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

# Non-technology dispatch


### Exchange quantities

Quantity exchanged from one region to the other. The variable is directed meaning it only denotes exchange into one direction.

```@raw html
<p class="norm">
Variables are only created between regions that can actually exchange energy, which depends on the definition of <a href="../parameter_list/#Residual-capacities-1">residual capacity</a>.
</p>
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>exc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>dispatch resolution of carrier</li>
<li><a href="../parameter_list/#Residual-capacities-1">residual capacity</a> between regions defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
<li><a href="../constraints/#Exchange-capacity-restriction-1">exchange capacity restriction</a></li>
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Buy and sell

Quantities bought or sold on an external market.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>trdBuy</td>
<td>trdSell</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li>dispatch resolution of carrier</li>
<li><a href="../parameter_list/#Trade-price-1">trade price</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
<li><a href="../constraints/#Trade-capacity-restriction-1">trade capacity restriction</a></li>
<li><a href="../constraints/#Trade-cost-equation-1">trade cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"><a href="../parts/#Trade-1">trade</a></td>
</tr>
</tbody>
</table>
```

### Curtailment and loss-of-load

Curtailed quantities and unmet demand.


```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>lss</td>
<td>crt</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:left">$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td  colspan="2"; style="text-align:left"><ul class="liste">
<li>dispatch resolution of carrier</li>
<li><a href="../parameter_list/#Cost-of-curtailment-and-loss-of-load-1">respective costs</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
<li><a href="../constraints/#Curtailment-and-loss-of-load-cost-equation-1">curtailment and loss-of-load cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td  colspan="2"><a href="../parts/#Balance-1">balance</a></td>
</tr>
</tbody>
</table>
```

# Capacity expansion

### Expansion

Expansion of conversion, storage-input, storage-output, storage-size, and exchange capacity.

```@raw html
<p class="norm">
As explained <a href="../parameter_list/#Capacity-expansion">here</a>, capacity refers to <strong>capacity before efficiency</strong>!
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>expConv</td>
<td>expSt{In/Out/Size}</td>
<td>expExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td rowspan="2"><strong>instances</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0px">
<ul class="liste">
<li>expansion resolution of technology</li>
</ul>
</td>
<td colspan="1"; rowspan="2">
<ul class="liste">
<li>regions can exchange carrier</li>
<li>expansion resolution of carrier</li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste"; start="4">
<li>if uses or generates carriers</li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px">
<ul class="liste"; start="4">
<li>each stored carrier</li>
</ul>
</td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
<li><a href="../constraints/#Expansion-cost-equation-1">expansion cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
<td style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Installed capacity

Installed capacity of conversion, storage-input, storage-output, storage-size, and exchange capacity.

```@raw html
<p class="norm">
As explained <a href="../parameter_list/#Capacity-expansion">here</a>, capacity refers to <strong>capacity before efficiency</strong>!
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>capaConv</td>
<td>capaSt{In/Out/Size}</td>
<td>capaExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td rowspan="3"><strong>instances</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0px">
<ul class="liste">
<li>superordinate dispatch resolution (usually years)</li>
</ul>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ul class="liste">
<li>spatial expansion resolution of technology</li>
</ul>
</td>
<td colspan="1"; rowspan="2"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste">
<li>spatial expansion resolution of carrier</li>
<li>regions can exchange carrier</li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste"; start="4">
<li>if uses or generates carriers</li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px">
<ul class="liste"; start="4">
<li>each stored carrier</li>
</ul>
</td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
<li><a href="../constraints/#Decommissioning-of-operated-capacitiy-1">decommissioning of operated capacitiy</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
<td style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Operated capacity

Operated capacity of conversion, storage-input, storage-output, storage-size, and exchange capacity.

```@raw html
<p class="norm">
As explained <a href="../parameter_list/#Capacity-expansion">here</a>, capacity refers to <strong>capacity before efficiency</strong>!
</p>
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>oprCapaConv</td>
<td style="padding-right:0.5em"><nobr>oprCapaSt{In/Out/Size}</nobr></td>
<td>oprCapaExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td rowspan="3"><strong>instances</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0px">
<ul class="liste">
<li>superordinate dispatch resolution (usually years)</li>
</ul>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ul class="liste">
<li>spatial expansion resolution of technology</li>
</ul>
</td>
<td colspan="1"; rowspan="2"; style="text-align:left;border-right:none;padding-top:0px;padding-right:0.0px">
<ul class="liste">
<li>spatial expansion resolution of carrier</li>
<li>regions can exchange carrier</li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste"; start="4">
<li>if uses or generates carriers</li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px">
<ul class="liste"; start="4">
<li>each stored carrier</li>
</ul>
</td>
</tr>
<tr>
<td rowspan="2"><strong>related constraints</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0.2em">
<ul class="liste">
<li><a href="../constraints/#Decommissioning-of-operated-capacitiy-1">decommissioning of operated capacitiy</a></li>
<li><a href="../constraints/#Operating-cost-equation-1">operating cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste">
<li><a href="../constraints/#Conversion-capacity-restriction-1">conversion capacity <br> restriction</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px">
<ul class="liste">
<li><a href="../constraints/#Storage-capacity-restriction-1">storage capacity <br> restriction</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;border-right:none;padding-top:0px";padding-right:0.0px>
<ul class="liste">
<li><a href="../constraints/#Exchange-capacity-restriction-1">exchange capacity <br> restriction</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
<td style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

# Costs

The variables listed here solely serve the purpose to aggregate different kinds of costs and do not have any other function within the model. Therefore, their structure is a bit arbitrary and was chosen to facilitate reporting without adversely affecting performance.

### Expansion cost

Costs of capacity expansion.

```@raw html
<p class="norm">
As explained <a href="../parameter_list/#Capacity-expansion">here</a>, capacity and thus also costs of capacity expansion, refer to <strong>capacity before efficiency</strong>!
</p>
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costExp{Conv,StIn,StOut,StSize}</td>
<td>costExp{Exc}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">Mil.€</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li><a href="../parameter_list/#Expansion-cost-1">expansion cost</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Expansion-cost-equation-1">expansion cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```


### Operating cost

Costs of operating capacity.

```@raw html
<p class="norm">
As explained <a href="../parameter_list/#Capacity-expansion">here</a>, capacity and thus also operating costs, refer to <strong>capacity before efficiency</strong>!
</p>
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costOpr{Conv,StIn,StOut,StSize}</td>
<td>costOpr{Exc}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">Mil.€</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li><a href="../parameter_list/#Operation-cost-1">operating cost</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Operating-cost-equation-1">operating cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Variable cost

```@raw html
<p class="norm">
Variable costs associated with quantities dispatched. Costs incurred by <a href="../parameter_list/#Emission-price-1">emission prices</a> are included in <code>costVarUse</code>.
</p>
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costVar{Use,Gen,StIn,StOut}</td>
<td>costVar{Exc}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">Mil.€</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li><a href="../parameter_list/#Variable-cost-1">variable cost</a> defined</li>
<li><a href="../parameter_list/#Emission-factor-1">emission factor</a> and <a href="../parameter_list/#Emission-price-1">emission price</a> defined in case of use</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Trade cost

Costs and revenues from buying or selling carriers on an external market.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costTrdBuy</td>
<td>costTrdSell</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">Mil.€</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{sup}$, $R_{exp}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li><a href="../parameter_list/#Trade-price-1">trade price</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Trade-cost-equation-1">trade cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Curtailment and loss-of-load cost

Cost of curtailment and unmet demand.

To allow for revenues and costs from curtailment, `costCrt` is the only model variable that can also take negative values.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costCrt</td>
<td>costLss</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">Mil.€</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{sup}$, $R_{exp}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:left"><ul class="liste">
<li><a href="../parameter_list/#Cost-of-curtailment-and-loss-of-load-1">respective costs</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>related constraints</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Curtailment-and-loss-of-load-cost-equation-1">curtailment and loss-of-load cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```
