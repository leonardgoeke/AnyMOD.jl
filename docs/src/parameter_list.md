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
ol.liste {
  list-style-position: outside;
  padding-left: 1em;
  margin-left: 0em;
  margin-top: 0em;
  white-space: nowrap;
  display:inline-block;
  text-align: left;
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
Parameter list
=================

einleitung

capacities are gross/brutto/input capacities, auch für storage erklären, am besten mit zeichnung


# Dispatch of technologies

alles hier kann vom modus abhängen

### Availability

bla

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>avaConv</td>
<td>avaSt{In/Out/Size}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">percent as decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $Te$, $M$</td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">1.0</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
<ol class="liste"; start="4">
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>upwards</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Conversion-capacity-restriction-1">conversion capacity restriction</a></li>
<li><a href="../variables/#Generation-and-use-1">conversion variables</a> only created <br> where availability is not zero</li>
</ul>
</td>
<td>
<ul class="liste">
<li><a href="../constraints/#Storage-capacity-restriction-1">storage capacity restriction</a></li>
<li><a href="../variables/#Charging-and-discharging-1">storage variables</a> only created <br> where availability is not zero</li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```


### Efficiency

erwähne emissions

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>effConv</td>
<td>effSt{In/Out}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">percent as decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $Te$, $M$</td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">1.0</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
<ol class="liste"; start="4">
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>upwards</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Conversion-balance-1">conversion balance</a></li>
<li><a href="../constraints/#Conversion-capacity-restriction-1">conversion capacity restriction</a></li>
</ul>
</td>
<td>
<ul class="liste">
<li><a href="../constraints/#Storage-balance-1">storage balance</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
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
<td>costVar{Use/Gen/StIn/StOut}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>€/MWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>0.0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$C$ &#8594; <em>upwards</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related constraints</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
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


### Ratios of carrier use and generation

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>ratioEnerUse{Fix/Low/Up}</td>
<td>ratioEnerGen{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">percent as decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center">
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Energy-ratio-restriction-1">energy ratio restriction</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

### Storage self-discharge

erwähne emissions

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stDis</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>percent as decimal per hour</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>0.0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>upwards</em></li>
<li>$C$ &#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Storage-Balance-1">storage balance</a></li>
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

### Storage inflow

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stInflow</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>0.0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$C$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$ &#8594; <em>sum</em></li>
<li>$R_{dis}$ &#8594; <em>sum</em></li>
<li>$Te$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Storage-Balance-1">storage balance</a></li>
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

# Dispatch of exchange and trade

### Exchange availability

directed overwrites undirected

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>avaExc</td>
<td>avaExcDir</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">percent in decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td style="text-align:center">$Ts_{dis}$, $R_{a}$, $R_{b}$, $C$</td>
<td style="text-align:center">$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">1.0</td>
</tr>
<tr>
<td rowspan="3"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{a}$ &#8594; <em>upwards</em></li>
<li>$R_{b}$ &#8594; <em>upwards</em></li>
<li>$R_{a}$ &#8594; <em>average</em></li>
<li>$R_{b}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{from}$ &#8594; <em>upwards</em></li>
<li>$R_{to}$ &#8594; <em>upwards</em></li>
<li>$R_{from}$ &#8594; <em>average</em></li>
<li>$R_{to}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0.1875em">
<ol class="liste"; start="6">
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Exchange-capacity-restriction-1">exchange capacity restriction</a></li>
<li><a href="../variables/#Exchange-quantities-1">exchange variables</a> only created where availability is not zero</li>
</ul>
</td>
</tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Exchange losses
directed overwrites undirected

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>lossExc</td>
<td>lossExcDir</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">percent in decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td style="text-align:center">$Ts_{dis}$, $R_{a}$, $R_{b}$, $C$</td>
<td style="text-align:center">$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">0.0</td>
</tr>
<tr>
<td rowspan="3"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{a}$ &#8594; <em>upwards</em></li>
<li>$R_{b}$ &#8594; <em>upwards</em></li>
<li>$R_{a}$ &#8594; <em>average</em></li>
<li>$R_{b}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{from}$ &#8594; <em>upwards</em></li>
<li>$R_{to}$ &#8594; <em>upwards</em></li>
<li>$R_{from}$ &#8594; <em>average</em></li>
<li>$R_{to}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0.1875em">
<ol class="liste"; start="6">
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Conversion-balance-1">conversion balance</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Exchange cost

directed overwrites undirected

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costVarExc</td>
<td>costVarExcDir</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">€/MWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td style="text-align:center">$Ts_{dis}$, $R_{a}$, $R_{b}$, $C$</td>
<td style="text-align:center">$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="3"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{a}$ &#8594; <em>upwards</em></li>
<li>$R_{b}$ &#8594; <em>upwards</em></li>
<li>$R_{a}$ &#8594; <em>average</em></li>
<li>$R_{b}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{from}$ &#8594; <em>upwards</em></li>
<li>$R_{to}$ &#8594; <em>upwards</em></li>
<li>$R_{from}$ &#8594; <em>average</em></li>
<li>$R_{to}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0.1875em">
<ol class="liste"; start="6">
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Trade price

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>trdBuyPrc</td>
<td>trdSellPrc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">€/MWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$, $id$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Trade-cost-equation-1">trade cost equation</a></li>
<li><a href="../variables/#Buy-and-sell-1">trade variables</a> only created where costs defined</li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Trade-1">trade</a></td>
</tr>
</tbody>
</table>
```

### Trade capacity

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>trdBuyCap</td>
<td>trdSellCap</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$, $id$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Trade-capacity-restriction-1">trade capacity restriction</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Trade-1">trade</a></td>
</tr>
</tbody>
</table>
```



# Other dispatch

### Demand
bla
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>dem</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>0.0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>sum</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li><a href="../constraints/#Energy-balance-1">energy balance</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Balance-1">balance</a></td>
</tr>
</tbody>
</table>
```

### Cost of curtailment and loss of load

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
<td colspan="2"; style="text-align:center">€/MWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
<li>$R_{dis}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
<li>$R_{dis}$ &#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Curtailment-and-loss-of-load-cost-equation-1">curtailment and loss-of-load cost equation</a></li>
<li><a href="../variables/#Curtailment-and-loss-of-load-1">respective variables</a> only created where costs defined</li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Balance-1">balance</a></td>
</tr>
</tbody>
</table>
```

# Capacity expansion

### Discount rate

bla
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>rateDisc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>percent as decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>0.02</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{sup}$	&#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{sup}$ &#8594; <em>average</em></li>
<li>$R_{exp}$	&#8594; <em>average</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li>all <a href="../constraints/#Cost-equations-1">cost equations</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Interest rate

bla

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>rateExpConv</td>
<td>rateExpSt{In/Out/Size}</td>
<td>rateExpExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">percent as decimal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">respective discount rate is used as a default</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{exp}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Expansion-cost-equation-1">expansion cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Expansion cost

bla

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costExpConv</td>
<td>costExpSt{In/Out/Size}</td>
<td>costExpExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>Mil.€/GW</td>
<td>Mil.€/GWh</td>
<td>Mil.€/GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{exp}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Expansion-cost-equation-1">expansion cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Operating cost

maintainence costs  

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costOprConv</td>
<td>costOprSt{In/Out/Size}</td>
<td>costOprExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>Mil.€/GW/a</td>
<td>Mil.€/GWh/a</td>
<td>Mil.€/GW/a</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Ts_{sup}$	&#8594; <em>upwards</em></li>
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{sup}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$R_{a}$ &#8594; <em>upwards</em></li>
 <li>$R_{b}$ &#8594; <em>upwards</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Operating-cost-equation-1">operating cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```

### Technical lifetime

bla

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>lifeConv</td>
<td>lifeSt{In/Out/Size}</td>
<td>lifeExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">years</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">20</td>
<td>50</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{exp}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
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

### Economic lifetime

bla

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>lifeEcoConv</td>
<td>lifeEcoSt{In/Out/Size}</td>
<td>lifeEcoExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">years</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">respective technical lifetime is used as a default</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{exp}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
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

### Construction time

delay expansion, useful for nuclear, costs are disocunted toward timeperiod for investment

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>delConv</td>
<td>delSt{In/Out/Size}</td>
<td>delExc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">years</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">0</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center" >
<div style = "width: 50%; margin: 0 auto;">
<ol class="liste">
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</div>
</td>
<td>
<ol class="liste">
 <li>$Ts_{exp}$	&#8594; <em>upwards</em></li>
 <li>$R_{a}$ &#8594; <em>average</em></li>
 <li>$R_{b}$ &#8594; <em>average</em></li>
 <li>$C$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
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

# Limits on expansion and capacity

### Storage ratios

blabla

- `stInToConv`: ratio between conversion and storage input capacity
- `stOutToStIn`: ratio between storage out- and input capacity
- `sizeToStIn`: ratio between storage size and storage input capacity, also known as energy-to-power ratio

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stInToConv</td>
<td>stOutToStIn</td>
<td>sizeToStIn</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">dimensionless</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="3"; style="text-align:center">$Ts_{exp}$, $R_{exo}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="3"; style="text-align:center">
<ol class="liste">
<li>$Te$ &#8594; <em>upwards</em></li>
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$R_{exp}$ &#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li><a href="../variables/#Installed-capacity-1">capacity variables</a> substituted by product of ratio and connected variable</li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```



### Residual capacities

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>capaConvResi</td>
<td>capa{StIn/StOut/StSize}Resi</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$R_{exp}$ &#8594; <em>sum</em></li>
<li>$Te$ &#8594; <em>sum</em></li>
<li>$Ts_{sup}$ &#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
<ol class="liste"; start="4">
<li>$Ts_{exp}$ &#8594; <em>sum</em></li>
<li>$Ts_{sup}$ &#8594; <em>upwards</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>sum</em></li>
<li>$Ts_{exp}$ &#8594; <em>sum</em></li>
<li>$Ts_{sup}$ &#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
<li><a href="../constraints/#Decommissioning-of-operated-capacitiy-1">decommissioning of operated capacitiy</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

directed adds to undirecrted
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>capaExcResi</td>
<td>capaExcResiDir</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td style="text-align:center">$Ts_{dis}$, $R_{a}$, $R_{b}$, $C$</td>
<td style="text-align:center">$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="3"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{a}$ &#8594; <em>sum</em></li>
<li>$R_{b}$ &#8594; <em>sum</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;border-bottom:none;padding-top:0px;padding-bottom:0px">
<ol class="liste"; start="2">
<li>$R_{from}$ &#8594; <em>sum</em></li>
<li>$R_{to}$ &#8594; <em>sum</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0.1875em">
<ol class="liste"; start="4">
<li>$Ts_{dis}$	&#8594; <em>average</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:left">
<ul class="liste">
<li><a href="../constraints/#Definition-of-installed-capacity-1">definition of installed capacity</a></li>
<li><a href="../constraints/#Decommissioning-of-operated-capacitiy-1">decommissioning of operated capacitiy</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Limits on expansion

for upper limits sum only if inheritance of all descendants are defined
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>expConv{Fix/Low/Up}</td>
<td>exp{StIn/StOut/StSize}{Fix/Low/Up}</td>
<td>expExc{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Te$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
<td colspan="1"; rowspan="2">
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{a}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{b}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;padding-top:0px">
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Limits on capacity
for upper limits sum only if inheritance of all descendants are defined
warning: problem capalimits und jahre  
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>capaConv{Fix/Low/Up}</td>
<td>capa{StIn/StOut/StSize}{Fix/Low/Up}</td>
<td>capaExc{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$,$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{sup}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0px">
<ol class="liste">
<li>$R_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Te$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Ts_{sup}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; rowspan="2">
<ol class="liste">
<li>$Ts_{sup}$	&#8594; <em>average</em></li>
<li>$R_{a}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{b}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;padding-top:0px">
<ol class="liste"; start="4">
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Limits on operated capacity
for upper limits sum only if inheritance of all descendants are defined
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>oprCapaConv{Fix/Low/Up}</td>
<td>oprCapa{StIn/StOut/StSize}{Fix/Low/Up}</td>
<td>oprCapaExc{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="3"; style="text-align:center">GW</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$,$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{sup}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="3"; style="text-align:center">none</td>
</tr>
<tr>
<td rowspan="2"><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0px">
<ol class="liste">
<li>$R_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Te$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Ts_{sup}$ &#8594; <em>average</em></li>
</ol>
</td>
<td colspan="1"; rowspan="2">
<ol class="liste">
<li>$Ts_{sup}$	&#8594; <em>average</em></li>
<li>$R_{a}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{b}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none">
<ol class="liste"; start="4">
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
<td colspan="1"; style="text-align:center">
<ol class="liste"; start="4">
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td>
</tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="3"; style="text-align:center;vertical-align:middle">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="3"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

# Limits on dispatch quantities

### Limits on technology dispatch
for upper limits sum only if inheritance of all descendants are defined
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>use{Fix/Low/Up}</td>
<td>gen{Fix/Low/Up}</td>
<td>stOut{Fix/Low/Up}</td>
<td>stIn{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="4"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="4"; style="text-align:center">$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="4"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="4"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Ts_{exp}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{dis}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$Te$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$M$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="4"; style="text-align:center">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="4"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Limits on exchange
for upper limits sum only if inheritance of all descendants are defined
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>exc{Fix/Low/Up}</td>
<td>ExcDir{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="2"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{a}$, $R_{b}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="2"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="2"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{a}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{b}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$ &#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td></tr>
<tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Limits on trade, curtailment and loss of load
for upper limits sum only if inheritance of all descendants are defined
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>trdBuy{Fix/Low/Up}</td>
<td>trdSell{Fix/Low/Up}</td>
<td>crt{Fix/Low/Up}</td>
<td>lss{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td colspan="4"; style="text-align:center">GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="4"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td colspan="4"; style="text-align:center">none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td colspan="4"; style="text-align:center">
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>sum</em>/<em>sum*</em></li>
<li>$R_{dis}$ &#8594; <em>sum</em>/<em>sum*</em></li>
<li>$C$	&#8594; <em>sum</em>/<em>sum*</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td colspan="4"; style="text-align:center">
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="4"; style="text-align:center"><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

# Emissions

### Emission limit

bla
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>emissionUp</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>Mil. tCO<sub>2</sub></td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{dis}$	&#8594; <em>sum*</em></li>
<li>$Ts_{exp}$ &#8594; <em>sum*</em></li>
<li>$R_{dis}$	&#8594; <em>sum*</em></li>
<li>$C$	&#8594; <em>sum*</em></li>
<li>$Te$	&#8594; <em>sum*</em></li>
<li>$M$	&#8594; <em>sum*</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Emission factor

bla
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>emissionFac</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>tCO<sub>2</sub>/GWh</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
<li>$R_{dis}$	&#8594; <em>upwards</em></li>
<li>$C$	&#8594; <em>upwards</em></li>
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$M$	&#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Limit-1">limit</a></td>
</tr>
</tbody>
</table>
```

### Emission price

bla
```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>emissionPrc</td>
</tr>
<tr>
<td><strong>unit</strong></td>
<td>€/tCO<sub>2</sub></td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $Ts_{exp}$, $R_{dis}$, $C$, $Te$, $M$</td>
</tr>
<tr>
<td><strong>default value</strong></td>
<td>none</td>
</tr>
<tr>
<td><strong>inheritance rules</strong></td>
<td>
<ol class="liste">
<li>$Ts_{exp}$ &#8594; <em>upwards</em></li>
<li>$Ts_{dis}$	&#8594; <em>upwards</em></li>
<li>$R_{dis}$	&#8594; <em>upwards</em></li>
<li>$C$	&#8594; <em>upwards</em></li>
<li>$Te$	&#8594; <em>upwards</em></li>
<li>$M$	&#8594; <em>upwards</em></li>
</ol>
</td></tr>
<tr>
<td><strong>related elements</strong></td>
<td>
<ul class="liste">
<li>see <a href="../constraints/#Limiting-constraints-1">limiting constraints</a></li>
<li><a href="../constraints/#Variable-cost-equation-1">variable cost equation</a></li>
</ul>
</td>
</ul>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Objective-1">objective</a></td>
</tr>
</tbody>
</table>
```
