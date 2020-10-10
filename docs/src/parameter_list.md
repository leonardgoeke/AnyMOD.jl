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
  list-style-position: inside;
  margin-left: 0em;
  margin-top: 0em;
  white-space: nowrap;
  display:inline-block;
  text-align: left;
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
# Parameter list

In the following all parameters available in AnyMOD are listed. Information includes the name used in the input files and throughout the model, the parameters' unit, its dimensions according to the symbols introduced in [Sets and Mappings](@ref), the default value and the inheritance rules. In addition, related model elements and the part a parameter is assigned are documented.

# Dispatch of technologies

The parameters listed here describe the conversion and storage of energy carriers by technologies. As a result, each of these parameters can vary by operational mode. If any mode specific values are provided, these replace mode unspecific data.

The following two diagrams serve as a remainder on how conversion and storage are generally modelled in AnyMOD.

```@raw html
<p style="text-align:center;"><img src="../assets/convTech.svg" width="64%"/>
<p style="text-align:center;"><img src="../assets/stTech.svg" width="80%"/>
```

### Availability

Technical availability of the operated capacity.

Since operated capacity is split into conversion, storage-input, storage-output, and storage-size, the same distinction applies to availabilities.

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

Efficiency of converting or storing energy carriers.

For conversion the parameter controls the ratio between in- and output carriers. For storage it determines the losses charging to and discharging from the storage system is subjected to.

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

Costs imposed on different types of quantities dispatched.

Note that for storage costs are incurred on quantities as specified in the diagram above. This means `stIn` quantities still include charging losses, while `stOut` quantities are already corrected for losses from discharging.

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

Restricting the share of a single carrier on total use or generation. The share can either be fixed or imposed as a lower or upper limit.

One practical example for the application of this parameter is modelling the power-to-heat ratio of cogeneration plants (see [`par_techDispatch.csv`](https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/par_techDispatch.csv)).

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

```@raw html

<p class="norm">
Automatic reduction of stored energy within a storage system.
</p>

<p class="norm">
If the stored carrier is assigned an <a href="../parameter_list/#Emission-factor-1">emission factor</a> and <a href="../model_object/#Optional-arguments-1"><code>emissionLoss</code></a> is set to <code>true</code>, these losses are subject to emissions.
</p>

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
<p class="norm">
External charging of the storage system. Inflows can also be negative and are not subject to charging losses.
</p>

<p class="norm">
Flows have to be provided in power units and are converted into energy quantities according to the temporal resolution of the respective carrier (e.g. at a daily resolution 2 GW translate into of 48 GWh). This approach ensures parameters do not need to be adjusted when the temporal resolution is changed. The most important application of this parameter are natural inflows into hydro storages.
</p>


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

Technical availability of exchange capacities. The parameter `avaExc` applies for both directions and will be overwritten by the directed `avaExcDir`.

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

```@raw html

<p class="norm">
Losses occurring when energy is exchanged between two regions. The parameter <code>lossExc</code> applies for both directions and will be overwritten by the directed <code>lossExcDir</code>.
</p>
<p class="norm">
If the exchanged carrier is assigned an <a href="../parameter_list/#Emission-factor-1">emission factor</a> and <a href="../model_object/#Optional-arguments-1"><code>emissionLoss</code></a> is set to <code>true</code>, these losses are subject to emissions.
</p>

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

Costs imposed on the exchange of quantities. Cost are equally split between the exporting and importing region.

The parameter `costVarExc` applies for both directions and will be overwritten by the directed `costVarExcDir`.

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
<p class="norm">
Price for buying or selling an energy carrier on an external market.
</p>

<p class="norm">
Can be combined with the parameter <a href="../parameter_list/#Trade-capacity-1">trade capacity</a> to create stepped demand and supply curves (see following documentation for details).
</p>

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
<p class="norm">
Capacity available for buying or selling an energy carrier on an external market.
</p>

<p class="norm">
Capacity has to be provided in power units and is converted into energy quantities according to the temporal resolution of the respective carrier (e.g. at a daily resolution 2 GW translate into 48 GWh). This approach ensures parameters do not need to be adjusted when the temporal resolution is changed.
</p>

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

<p class="norm">
By assigning the same <code>id</code> to a <a href="../parameter_list/#Trade-price-1">trade price</a> and capacity the amount of energy that can be bought or sold at the given price can be limited. As a result, stepped supply and demand curves for energy carriers can be created.
</p>

<p class="norm">
For example, the table below enables the import of <code>hydrogen</code> to the region <code>West</code> at 100 €/MWh, but limits the import capacity to 20 GW. When imposing this limit, the capacity is scaled according to the temporal resolution hydrogen is modelled at. So, at a yearly resolution 20 GW would translate to 175.2 TWh (= 20 GW	&times; 8760 h).
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>region_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>id</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">West</td>
<td style="border-right:none">hydrogen</td>
<td>1</td>
<td style="border-right:none">trdBuyPrc</td>
<td style="border-right:none;text-align:center">100.0</td>
</tr>
<tr>
<td style="border-right:none">West</td>
<td style="border-right:none">hydrogen</td>
<td>1</td>
<td style="border-right:none">trdBuyCap</td>
<td style="border-right:none;text-align:center">20.0</td>
</tr>
</tbody>
</table>

<p class="norm">
Alternatively, this definition creates an additional electricity demand of 2.0 and 1.0 GW with a willingness-to-pay of 60 and 90 €/MWh, respectively. By adding more columns values could be further differentiated by time-step and region.

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>id</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">electricity</td>
<td>1</td>
<td style="border-right:none">trdSellPrc</td>
<td style="border-right:none;text-align:center">60.0</td>
</tr>
<tr>
<td style="border-right:none">electricity</td>
<td>2</td>
<td style="border-right:none">trdSellPrc</td>
<td style="border-right:none;text-align:center">90.0</td>
</tr>
<tr>
<td style="border-right:none">electricity</td>
<td>1</td>
<td style="border-right:none">trdSellCap</td>
<td style="border-right:none;text-align:center">2.0</td>
</tr>
<tr>
<td style="border-right:none">electricity</td>
<td>2</td>
<td style="border-right:none">trdSellCap</td>
<td style="border-right:none;text-align:center">1.0</td>
</tr>
</tbody>
</table>
</p>
```

# Other dispatch

### Demand
Inelastic demand for an energy carrier.

Capacity has to be provided in power units and is converted into energy quantities according to the temporal resolution of the respective carrier (e.g. at a daily resolution 20 GW translate into 480 GWh). This approach ensures parameters do not need to be adjusted when the temporal resolution is changed.


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

Variable costs excess generation or unmet demand is subjected to. Costs can also be negative (=revenues).

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

```@raw html
<p class="norm">
Here, all parameters relevant to the expansion of conversion, storage, and exchange capacity are listed.
</p>
<p class="norm">
At this point it is important to stress that, as displayed in the <a href="#Dispatch-of-technologies">technology diagrams</a>, <strong>AnyMOD always indicates capacities before efficiency losses!</strong> For instance capacity of a gas power plant does not denote its maximum electricity output, but the maximum gas input. This approach is pursued, because <a href="../parameter_list/#Efficiency-1">efficiency</a> is not a constant and can differ by time-step, region, and mode. As a result, maximum output varies within the dispatch too and is not suited to universally describe installed capacities.
</p>
```

### Discount rate

Overall rate to discount all costs to the present. See [Cost equations](@ref) for details on use.

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

Interest rate to compute annuity costs of investments. See [Cost equations](@ref) for details on use.

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

Costs of capacity expansion (or investment).

!!! warning "Cost data before efficiency"
    Ensure the cost data provided relates to capacity **before efficiency** (see beginning of section)! Costs before efficiency can be obtained by multiplying costs after efficiency with a nominal efficiency ``K_{before} = K_{after} \cdot \eta``.

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

Costs of operating installed capacities.

!!! warning "Cost data before efficiency"
    Ensure the cost data provided relates to capacity **before efficiency** (see beginning of section)! Costs before efficiency can be obtained by multiplying costs after efficiency with a nominal efficiency ``K_{before} = K_{after} \cdot \eta``.


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

Time in years a capacity can be operated after construction.

To avoid distortions lifetimes are advised to be divisible by the steps-size of capacity modelling (e.g rather using 20 or 25 instead of 23 when using 5-year steps).

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

Time in years to compute annuity costs of investment. Also determines the time-frame annuity costs are incurred over.

To avoid distortions lifetimes are advised to be divisible by the steps-size of capacity modelling (e.g rather using 20 or 25 instead of 23 when using 5-year steps).

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

Time in years for construction of capacity. This parameter introduces an offset between the start of the economic and technical lifetime.

To avoid distortions lifetimes are advised to be divisible by the steps-size of capacity modelling (e.g rather using 0 or 5 instead of 3 when using 5-year steps).

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


# Limits on quantities dispatched

```@raw html

<p class="norm">
Limits on variables also utilize the <a href="../parameter_overview/#Inheritance">inheritance algorithm</a>. Therefore, the way parameter data is provided determines how limits are enforced. For example, in the table below the upper limit of 100 GWh on the use of <code>biomass</code> will be imposed on the sum of use across <u>all years</u>, because the time-step dimension is undefined.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">biomass</td>
<td></td>
<td style="border-right:none">useUp</td>
<td style="border-right:none;text-align:center">100.0</td>
</tr>
</tbody>
</table>

<p class="norm">
If instead the limit should apply to <u>each year</u> seperately, each of these years needs to be specified.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none;border-right:none">biomass</td>
<td>2020</td>
<td style="border-right:none">useUp</td>
<td style="border-right:none;text-align:center">100.0</td>
</tr>
<tr>
<td style="border-right:none;border-right:none">biomass</td>
<td>2030</td>
<td style="border-right:none">useUp</td>
<td style="border-right:none;text-align:center">100.0</td>
</tr>
</tbody>
</table>
<p class="norm">
As an abbrevation we could also apply the keyword <code>all</code> (see <a href="../sets/#Time-steps">Time-steps</a> for details) to reduce the number of required rows.
</p>
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">biomass</td>
<td>all</td>
<td style="border-right:none">useUp</td>
<td style="border-right:none;text-align:center">100.0</td>
</tr>
</tbody>
</table>

<p class="norm">
So far, the limit for each year still applies to the summed use of biomass across all regions. This could again be altered by adding a respective column.
</p>

<p class="norm">
Applying limits on the sum of variables across different years can be insightful in some case (for example in case of an emission budget from now until 2050). But it also is a likely and severe mistake to make if unfamiliar with AnyMOD's specific mechanics. For this reason defining a limit that sums up variables from different years will cause a warning within the <a href="../error/#Error-handling">reporting file</a>  
</p>
```

### Limits on technology dispatch

```@raw html
<p class="norm">
Limits on technology dispatch. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>

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

```@raw html
<p class="norm">
Limits on exchange quantities. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>

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

```@raw html
<p class="norm">
Limits on traded and curtailed quantities as well as on unmet demand. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>

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

# Limits on expansion and capacity

```@raw html
<p class="norm">
Limits on expansion and capacity are enforced analogously to <a href="#Limits-on-quantities-dispatched">limits on dispatch quantities</a>. Therefore, the same caution with regard to how limits are defined should be exercised. As explained for dispatched quantities in greater detail, the table below will impose an upper limit of 80 GW on the installed capacity of <code>wind</code> summed across <u>all years</u>.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>technology_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">wind</td>
<td></td>
<td style="border-right:none">capaConvUp</td>
<td style="border-right:none;text-align:center">80.0</td>
</tr>
</tbody>
</table>

<p class="norm">
While this table will actually enforce separate limits of 80 GW on the installed capacity of <code>wind</code> in <u>each year</u>.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>technology_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">wind</td>
<td>all</td>
<td style="border-right:none">capaConvUp</td>
<td style="border-right:none;text-align:center">80.0</td>
</tr>
</tbody>
</table>
```

### Storage ratios

```@raw html
One technology can have four different kinds of capacity variables (see <a href="../sets/#Technologies">Technologies</a> for details): conversion, storage-input, storage-output, and storage-size. The ratios between these capacities can be fixed by the following parameters:
```
- `stInToConv`: ratio between conversion and storage-input capacity
- `stOutToStIn`: ratio between storage-output and storage-input capacity
- `sizeToStIn`: ratio between storage-size and storage-input capacity, commonly referred to energy-to-power ratio

```@raw html
<p class="norm">
Ratios are not directly applied to <a href="../variables/#Installed-capacity-1">installed capacities</a>, but to <a href="../variables/#Expansion-1">expansion variables</a> instead. Consequently, acutally installed capacities can deviate from the specified ratios, if any <a href="../parameter_list/#Residual-capacities-1">residual capacities</a> are provided. In case of <code>stock</code> technologies, which are not expanded, ratios are directly enforced to capacities. In this case any deviating <a href="../parameter_list/#Residual-capacities-1">residual capacities</a> are ignored.
</p>
```

!!! note "Upper and lower limits on ratios"
    So far, AnyMOD does not support the setting of upper and lower limits on these ratios instead of fixing them. As a workaround, the code below shows how an upper limit of 10 on the energy-to-power ratio can be manually added to a model.

    ```julia
    for x in 1:size(model_object.parts.tech[:battery].var[:capaStIn],1)
      var = model_object.parts.tech[:battery].var
      stIn, stSize = [var[y][x,:var] for y in [:capaStIn,:capaStSize]]
      @constraint(model_object.optModel, fixEP, stIn*10 >= stSize)
    end
    ```

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

Installed capacities for technologies that already exist without any expansion.

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

Installed exchange capacities that already exist without any expansion.

Defining a residual capacity between two regions generally enables exchange of a specific carrier between these regions. If exchange should be enabled, but no pre-existing capacity exists, a residual capacity of zero can be provided.

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

`capaExcResi` refers to capacity in both directions, while `capaExcResiDir` refers to directed capacities and is added to any undirected values. Consequently, the table below will result in an residual capacity of 4 GW from `East` to `West` and 3 GW from `West` to `East`.

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>region_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>region_1</strong></td>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>carrier_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td style="border-right:none">East</td>
<td style="border-right:none">West</td>
<td>electricity</td>
<td style="border-right:none">capaExcResi</td>
<td style="border-right:none;text-align:center">3.0</td>
</tr>
<tr>
<td style="border-right:none">East</td>
<td style="border-right:none">West</td>
<td>electricity</td>
<td style="border-right:none">capaExcResiDir</td>
<td style="border-right:none;text-align:center">1.0</td>
</tr>
</tbody>
</table>
```

### Limits on expansion

```@raw html
<p class="norm">
Limits on capacity expansion. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>
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

```@raw html
<p class="norm">
Limits on installed capacity. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>
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

```@raw html
<p class="norm">
Limits on operated capacity. In the inheritance rules <em>sum*</em> only applies for upper limits.
</p>
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

# Emissions

### Emission limit

Upper limit on carbon emissions.

```@raw html
<p class="norm">
Upper limits on emissions are enforced analogously to <a href="#Limits-on-quantities-dispatched">limits on dispatch quantities</a>. Therefore, the same caution with regard to how limits are defined should be exercised. As explained for dispatched quantities in greater detail, the table below will impose a carbon budget, meaning an upper limit on the sum of carbon emitted across <u>all years</u>.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td></td>
<td style="border-right:none">emissionUp</td>
<td style="border-right:none;text-align:center">80.0</td>
</tr>
</tbody>
</table>

<p class="norm">
While this table will enforce separate limits for <u>each year</u>.
</p>

<table class="tabelle2">
<tbody>
<tr>
<td style="border-bottom: solid 1px;border-color: #dbdbdb"><strong>timestep_1</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>parameter</strong></td>
<td style="border-right:none;border-bottom: solid 1px;border-color: #dbdbdb"><strong>value</strong></td>
</tr>
<tr>
<td>all</td>
<td style="border-right:none">emissionUp</td>
<td style="border-right:none;text-align:center">80.0</td>
</tr>
</tbody>
</table>


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

Relative emissions associated with the use of a carrier.

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

Costs imposed on emitting carbon.

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
