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
Variables
=================

erklärung grundsätzliches format, dataframe mit werten und ids, erklären wie das ganze ausgegeben werden kann, hinweis, dass zelle nicht einfach eine JuMP variable sondern eine expression enthält

all positive defined as positive

note: anyMOD benutzt nicht die jump container, da variablen extrem sparse highly inefficient

# Dispatch of technologies

[Technologies](@ref)

### Generation and use

sdf, not all modes relevant for each technology, no variables für ava gleich 0

```@raw html
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

```@raw html
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

```@raw html
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

directed!
explain what determines potential interconnection

```@raw html
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
<li>regions can exchange carrier</li>
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

fully analogous to trade buy and sell, just created to faciliate reporting since aims to represent other aspects

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

```@raw html
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

```@raw html
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

```@raw html
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

emissionPrc is included via use,

dimensions just picked for internal purposes, more detailed information via reporting functions

### Expansion cost
sdf
```@raw html
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
sdf
```@raw html
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

and revenues!

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

costs can be negative

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
