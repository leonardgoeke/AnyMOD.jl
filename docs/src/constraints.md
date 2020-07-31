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
# Constraints

```@raw html
<p class="norm">
In the following, all constraints used in AnyMOD are listed. Information includes the name used throughout the model, its dimensions and a stylized formulation of the constraint itself (see <a href="https://arxiv.org/abs/2004.10184)">Göke (2020)</a> for details). Within these constraints, variables are printed in bold. In addition, the parameters, variables, and the model part associated with the constraints are listed.
</p>
<p class="norm">
To increase performance, AnyMOD stores constraints within DataFrames instead of using JuMPs native containers. Each dimension is represented by a column and integers in these columns relate to nodes within the hierarchical trees of sets (see <a href="../data/#printObject"><code>printObject</code></a> on how to export these in a readable format). An additional <code>cns</code> column stores the corresponding constraint. Note that final constraints will look unintuitive, because they are <a href="../performance/#Scaling">scaled</a> to increase performance and converted to standard form by JuMP.
</p>
<p class="norm">
New constraints beyond those listed here can freely be added to a model by using standard JuMP commands.
</p>
```

# Balances

### Energy balance

Constraints supply to at least exceed demand.

The energy balance can alternatively be enforced as an equality constraint (see [Carriers](@ref) for details). Instead of having a fixed name the constraint is always assigned the name of the carrier being balanced.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td><em>carrier name</em></td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $R_{dis}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>dispatch resolution of carrier</li>
</ul></td>
</tr>
<tr>
<td rowspan = "2"><strong>formulation</strong></td>
<td style="text-align:left;border-bottom:none;padding-bottom:0px">$ \scriptstyle dem \, + \, \sum\bm{use} \, + \, \sum\bm{stExt_{in}} \, + \, \sum \bm{exc_{exp}}\, + \, \sum\bm{trd_{sell}} \, + \, \bm{lss}  \, \leq \, $</td>
</tr>
<tr>
<td style="text-align:right;border-right:none;border-top:none;padding-top:0px">$ \scriptstyle \; \; \; \; \sum\bm{gen} \, + \, \sum\bm{stExt_{out}} \, + \, \sum (1-lossExc) \bm{exc_{imp}} \, + \, \sum\bm{trd_{buy}} \, + \, \bm{crt} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td><ul class="liste">
<li><a href="../parameter_list/#Demand-1">demand</a></li>
<li><a href="../parameter_list/#Exchange-losses-1">exchange losses</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td><ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
<li><a href="../variables/#Exchange-1">exchange</a></li>
<li><a href="../variables/#Buy-and-sell-1">buy and sell</a></li>
<li><a href="../variables/#Curtailment-and-loss-of-load-1">curtailment and loss-of-load</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Balance-1">balance</a></td>
</tr>
</tbody>
</table>
```

### Conversion balance

Controls the ratio between used and generated quantities.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>convBal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $Te$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>reference resolution of technology</li>
<li>expansion time-steps of technology</li>
<li>relevant modes of technology</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle eff_{conv} (\sum\bm{use} \, + \, \sum\bm{stInt_{out}}) \, = \, \sum\bm{gen} \, + \,  \sum\bm{stInt_{in}} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td><ul class="liste">
<li><a href="../parameter_list/#Efficiency-1">conversion efficiency</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td><ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

### Storage Balance

Ensures storage levels comply with charged and discharged quantities.

```@raw html

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stBal</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $Te$, $C$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>dispatch resolution of carrier</li>
<li>expansion time-steps of technology</li>
<li>relevant modes of technology</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \bm{stLvl_{t}} \, = \, \frac{\bm{stLvl_{t-1}}}{1\,-\,stDis} \, + \,  stInflow \, + \, \frac{\bm{stExt_{in}} \, + \, \bm{stInt_{in}}}{1/effSt_{in}} \, - \, \frac{\bm{stExt_{out}} \, + \, \bm{stInt_{out}}}{effSt_{out}} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td><ul class="liste">
<li><a href="../parameter_list/#Efficiency-1">storage efficiencies</a></li>
<li><a href="../parameter_list/#Storage-self-discharge-1">storage self-discharge</a></li>
<li><a href="../parameter_list/#Storage-inflow-1">storage inflow</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td><ul class="liste">
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
<li><a href="../variables/#Storage-level-1">storage level</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```


# Dispatch restrictions


### Conversion capacity restriction

Ensures quantities converted comply with the operated conversion capacity.

```@raw html
<p class="norm">
The graph-based approach that allows to vary resolution by energy carrier  within the same model complicates the formulation of capacity constraints for conversion technologies. What kind of constraints are necessary is specified in the <code>capaRestr</code> of the technology <a href="../api/#AnyMOD.TechPart"><code>part object</code></a> (see <a href="https://arxiv.org/abs/2004.10184)">Göke (2020)</a> on how this is derived). Capacity constraints are either enforced on used or generated quantities. In the latter case, quantities have to be corrected for the respective efficiency since AnyMOD always denotes capacities <u>after efficiency</u>.
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>inRestr</td>
<td>outRestr</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $Te$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:center"><ul class="liste">
<li>required capacity restrictions according to <code>capaRestr</code> field of part</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \frac{\bm{use} \, + \, \bm{stInt_{out}}}{ava_{conv}} \, \leq \, \bm{oprCapa_{conv}} $</td>
<td>$ \scriptstyle \frac{\bm{gen} \, + \, \bm{stInt_{in}}}{eff_{conv} \, ava_{conv}} \, \leq \, \bm{oprCapa_{conv}} $</td>
</tr>
<tr>
<td rowspan="2"><strong>parameter</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ul class="liste">
<li><a href="../parameter_list/#Availability-1">conversion availability</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ul class="liste">
<li><a href="../parameter_list/#Efficiency-1">conversion efficiency</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:10.25em"><ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
<li><a href="../variables/#Operated-capacity-1">operated conversion capacity</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```

### Storage capacity restriction

Ensures quantities stored comply with the operated storage-input, storage-output, storage-size capacity.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>stInRestr</td>
<td>stOutRestr</td>
<td>stSizeRestr</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="3"; style="text-align:center">$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $Te$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="3"; style="text-align:center"><ul class="liste">
<li>dispatch resolution of carrier</li>
<li>expansion time-steps of technology</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \frac{\bm{stExt_{in}} \, + \, \bm{stInt_{in}}}{ava_{stIn}} \, \leq \, \bm{oprCapa_{stIn}} $</td>
<td>$ \scriptstyle \frac{\bm{stExt_{out}} \, + \, \bm{stInt_{out}}}{ava_{stOut}} \, \leq \, \bm{oprCapa_{stOut}} $</td>
<td>$ \scriptstyle \frac{\bm{stLvl}}{ava_{stOut}} \, \leq \, \bm{oprCapa_{stSize}} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="3"; style="text-align:center"><ul class="liste">
<li><a href="../parameter_list/#Availability-1">storage availability</a></li>
</ul></td>
</tr>
<tr>
<td rowspan="2"><strong>variables</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated storage capacity</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
<ul class="liste">
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px">
<ul class="liste">
<li><a href="../variables/#Storage-level-1">storage level</a></li>
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

### Energy ratio restriction

Ensures used and generated quantities comply with the specified ratios.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>ratioEnerUse{Fix/Low/Up}</td>
<td>ratioEnerGen{Fix/Low/Up}</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{exp}$, $Ts_{dis}$, $R_{dis}$, $Te$, $C$, ($M$)</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:center"><ul class="liste">
<li><a href="../parameter_list/#Ratios-of-carrier-use-and-generation-1">ratios</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \bm{use} \; \begin{smallmatrix} = \\[0.5pt] \geq \\[2pt] \leq \end{smallmatrix} \; ratioEner^{use}_{fix/low/up} \sum \bm{use} $</td>
<td>$ \scriptstyle \bm{gen} \; \begin{smallmatrix} = \\[0.5pt] \geq \\[2pt] \leq \end{smallmatrix} \; ratioEner^{gen}_{fix/low/up} \sum \bm{gen} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:center">
<ul class="liste">
<li><a href="../variables/#Ratios-of-carrier-use-and-generation">generation and use ratios</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:8.25em">
<ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
</tr>
</tbody>
</table>
```


### Exchange capacity restriction

Ensures exchanged quantities comply with the operated exchange capacity.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>excRestr</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{dis}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td><ul class="liste">
<li>dispatch resolution of carrier and descendant carriers</li>
<li>regions can exchange carrier</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \sum \frac{\bm{exc}}{ava_{exc}} \, \leq \, \bm{oprCapa_{exc}} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td><ul class="liste">
<li><a href="../parameter_list/#Exchange-availability-1">exchange availablity</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td><ul class="liste">
<li><a href="../variables/#Exchange-1">exchange</a></li>
<li><a href="../variables/#Operated-capacity-1">operated exchange capacity</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Trade capacity restriction

Ensures traded quantities comply with the specified trade capacity.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>trdBuyCap</td>
<td>trdSellCap</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{dis}$, $R_{dis}$, $C$, $id$</td>
</tr>
<tr>
<td><strong>instances</strong></td>
<td colspan="2"; style="text-align:center"><ul class="liste">
<li><a href="../parameter_list/#Trade-price-1">trade price</a> defined</li>
<li><a href="../parameter_list/#Trade-capacity-1">trade capacity</a> defined</li>
</ul></td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td>$ \scriptstyle \bm{trdBuy} \, \leq \, trdBuyCap $</td>
<td>$ \scriptstyle \bm{trdSell} \, \leq \, trdSellCap $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:center"><ul class="liste">
<li><a href="../parameter_list/#Trade-capacity-1">trade capacity</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:6.4em"><ul class="liste">
<li><a href="../variables/#Buy-and-sell-1">buy and sell</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"><a href="../parts/#Trade-1">trade</a></td>
</tr>
</tbody>
</table>
```


# Capacity expansion

### Definition of installed capacity

```@raw html
<p class="norm">
Connects installed capacities to expansion variables and residual capacities.
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>capaConv</td>
<td><nobr>capaSt{In/Out/Size}</nobr></td>
<td>capaExc</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td rowspan="3"><strong>instance</strong></td>
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
<td colspan="1"; rowspan="2"; style="text-align:center;border-right:none;padding-top:0px;padding-right:0.0px">
<ul class="liste">
<li>spatial expansion resolution of carrier</li>
<li>regions can exchange carrier</li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;padding-top:0px">
<ul class="liste"; start="4">
<li>if uses or generates carriers</li>
</ul>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ul class="liste"; start="4">
<li>each stored carrier</li>
</ul>
</td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td colspan="3"; style="text-align:center">$ \scriptstyle \bm{capa_{t}} \, = \, resiCapa_{t} \, + \, \sum\limits_{t' \in (t \, + \, del, t \, +\,del \, + \, life]} \bm{exp_{t'}} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="3"; style="text-align:left;padding-left:18.5em"><ul class="liste">
<li><a href="../parameter_list/#Residual-capacities-1">residual capacity</a></li>
<li><a href="../parameter_list/#Technical-lifetime-1">technical lifetime</a></li>
<li><a href="../parameter_list/#Construction-time-1">construction time</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="3"; style="text-align:left;padding-left:18.5em"><ul class="liste">
<li><a href="../variables/#Installed-capacity-1">installed capacity</a></li>
<li><a href="../variables/#Expansion-1">capacity expansion</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
<td style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```

### Decommissioning of operated capacitiy

Ensures operated capacities comply with installed capacities.


```@raw html
<p class="norm">
Decommissioning behaviour is determined by the <code>decomm</code> argument of the <a href="../model_object">model constructor</a>. For <code>none</code>, the equations listed here are not enforced. Instead operated and installed capacities are identical. When the argument is set to <code>recomm</code>, only the first equation that limits operated capacities to installed capacities is enforced.
</p>
<p class="norm">
Lastly, for <code>decomm</code> both equations apply. The second equation will then ensure, that once decommissioned capacities cannot be re-decommissioned again. The expression $\Delta Resi_{+}$ in the equation denotes any increase of residual capacities from $t-1$ to $t$.
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>oprCapaConv</td>
<td style="padding-right:0.52em"><nobr>oprCapaSt{In/Out/Size}</nobr></td>
<td>oprCapaExc</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{exp}$, $R_{exp}$, $Te$</td>
<td>$Ts_{exp}$, $R_{exp}$, $C$, $Te$</td>
<td>$Ts_{exp}$, $R_{from}$, $R_{to}$, $C$</td>
</tr>
<tr>
<td rowspan="3"><strong>instance</strong></td>
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
<td colspan="1"; rowspan="2"; style="text-align:center;border-right:none;padding-top:0px;padding-right:0px">
<ul class="liste">
<li>spatial expansion resolution of carrier</li>
<li>regions can exchange carrier</li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;padding-top:0px;padding-right:0.52em">
<ul class="liste"; start="4">
<li>if uses or generates carriers</li>
</ul>
</td>
<td colspan="1"; style="text-align:center;padding-top:0px">
<ul class="liste"; start="4">
<li>each stored carrier</li>
</ul>
</td>
</tr>
<tr>
<td rowspan = "2"><strong>formulation</strong></td>
<td colspan = "3"; style="text-align:center;border-bottom:none">$ \scriptstyle \bm{oprCapa} \, \leq \,  \bm{capa}$</td>
</tr>
<tr>
<td colspan = "3"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">$ \scriptstyle \bm{oprCapa_{t}} \, \leq \, \bm{oprCapa_{t\,-\,1}} \, + \, \bm{exp_{t}} \, + \, \Delta Resi_{+} $</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="3"; style="text-align:left;padding-left:18.1em"><ul class="liste">
<li><a href="../parameter_list/#Residual-capacities-1">residual capacity</a></li>
</ul></td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="3"; style="text-align:left;padding-left:18.1em"><ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated capacity</a></li>
<li><a href="../variables/#Installed-capacity-1">installed capacity</a></li>
<li><a href="../variables/#Expansion-1">capacity expansion</a></li>
</ul></td>
</tr>
<tr>
<td><strong>part</strong></td>
<td colspan="2"; style="text-align:center"><a href="../parts/#Technology-1">technology</a></td>
<td style="text-align:center"><a href="../parts/#Exchange-1">exchange</a></td>
</tr>
</tbody>
</table>
```


# Cost equations

```@raw html
<p class="norm">
Within the cost equations the discount factor is used to discount costs to the present. The discount factor for a year $t$ is computed from the <a href="../parameter_list/#Discount-rate-1">discount rates</a> of the current and the previous years as follows:
</p>
<p class="norm">
$discFac_{t} = \displaystyle \prod_{t' = t_{0}}^{t}(1 \, + \, rateDisc_{t'})^{-1}$
</p>
```

### Expansion cost equation

Determines costs of capacity expansion.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costExp{Conv,StIn,StOut,StSize}</td>
<td>costExp{Exc}</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $C$</td>
</tr>
<tr>
<td rowspan="2"><strong>formulation</strong></td>
<td colspan="2"; style="text-align:center;border-bottom:none">
$ \scriptstyle \bm{costExp\{...\}} \, = \, \sum discFac \, \cdot \,  ann\{...\} \, \cdot \,  \bm{exp\{...\}} $
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
$ \scriptstyle ann\{...\} \, = \, costExp\{...\} \, \cdot \, \frac{rateExp\{...\} \, (1\,+\,rateExp\{...\})^{lifeEco\{...\}}}{(1\,+\,rateExp\{...\})^{lifeEco\{...\}}\,-\,1}$
</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:left;padding-left:8em">
<ul class="liste">
<li><a href="../parameter_list/#Discount-rate-1">discount rate</a></li>
<li><a href="../parameter_list/#Interest-rate-1">interest rate</a></li>
<li><a href="../parameter_list/#Expansion-cost-1">expansion cost</a></li>
<li><a href="../parameter_list/#Economic-lifetime-1">economic lifetime</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:8em">
<ul class="liste">
<li><a href="../variables/#Expansion-1">expansion</a></li>
<li><a href="../variables/#Expansion-cost-1">expansion cost</a></li>
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


### Operating cost equation

Determines costs of operating capacity.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costOpr{Conv,StIn,StOut,StSize}</td>
<td>costOpr{Exc}</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $C$</td>
</tr>
<tr>
<td><strong>formulation</strong></td>
<td colspan="2"; style="text-align:center">
$ \scriptstyle \bm{costOpr\{...\}} \, = \, \sum discFac \, \cdot \,  costOpr\{...\} \, \cdot \,  \bm{oprCapa\{...\}} $
</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:left;padding-left:8em">
<ul class="liste">
<li><a href="../parameter_list/#Discount-rate-1">discount rate</a></li>
<li><a href="../parameter_list/#Operating-cost-1">operating cost</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:8em">
<ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated capacity</a></li>
<li><a href="../variables/#Operating-cost-1">operating cost</a></li>
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

### Variable cost equation

```@raw html
<p class="norm">
Determines costs associated with quantities dispatched. Costs incurred by <a href="../parameter_list/#Emission-price-1">emission prices</a> are included in <code>costVarUse</code>.
</p>

<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costVar{Use,Gen}</td>
<td>costVar{StIn,StOut}</td>
<td>costVar{Exc}</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $R_{exp}$, $Te$</td>
<td>$Ts_{sup}$, $C$</td>
</tr>
<tr>
<td rowspan="2"><strong>formulation</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0px">
$ \scriptstyle \bm{costVar\{...\}} \, = \, \sum discFac \, \cdot \,  costVar\{...\} \, \cdot \,  \{use,gen,stIn,stOut,exc\} \, \cdot \, 10^3 $
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:center;border-right:none;border-top:none;padding-top:0px">
$ \scriptstyle + \, \sum emFac \, \cdot \,  emPrc \, \cdot \, \bm{use} \, \cdot \,  10^3$
</td>
<td colspan="2"; style="text-align:left;padding-top:0px">
</td>
</tr>
<tr>
<td rowspan="3"><strong>parameter</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ul class="liste">
<li><a href="../parameter_list/#Discount-rate-1">discount rate</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="2"; style="text-align:center;border-right:none;border-top:none;padding-top:0px;border-bottom:none;padding-bottom:0.1875em">
<ul class="liste">
<li><a href="../parameter_list/#Variable-cost-1">variable cost of technologies</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px;border-bottom:none">
<ul class="liste">
<li><a href="../parameter_list/#Exchange-cost-1">exchange cost</a></li>
</ul>
</td>
</tr>

<tr>
<td style="text-align:center;border-right:none;border-top:none;padding-top:0px">
<ul class="liste">
<li><a href="../parameter_list/#Emission-factor-1">emission factor</a></li>
<li><a href="../parameter_list/#Emission-price-1">emission price</a></li>
</ul>
</td>
<td></td>
<td></td>
</tr>

<tr>
<td rowspan="2"><strong>variables</strong></td>
<td colspan="3"; style="text-align:center;border-bottom:none;padding-bottom:0.1875em">
<ul class="liste">
<li><a href="../variables/#Variable-cost-1">variable cost</a></li>
</ul>
</td>
</tr>
<tr>
<td colspan="1"; style="text-align:left;border-right:none;border-top:none;padding-top:0px">
<ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;border-right:none;border-top:none;padding-top:0px">
<ul class="liste">
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
</ul>
</td>
<td colspan="1"; style="text-align:left;padding-top:0px">
<ul class="liste">
<li><a href="../variables/#Exchange-quantities-1">exchange quantities</a></li>
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

### Trade cost equation

Determines costs and revenues from buying or selling carriers on an external market.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costTrdBuy</td>
<td>costTrdSell</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{sup}$, $R_{exp}$, $C$</td>
</tr>
<tr>
<td rowspan="2"><strong>formulation</strong></td>
<td style="text-align:left;border-bottom:none;padding-bottom:0px">
$ \scriptstyle \bm{costTrdBuy} \, = $
</td>
<td style="text-align:left;border-bottom:none;padding-bottom:0px">
$ \scriptstyle \bm{costTrdSell} \, =  $
</td>
</tr>
<tr>
<td style="text-align:right;border-right:none;border-top:none;padding-top:0px">
$ \scriptstyle \sum discFac \, \cdot \,  trdBuyPrc \, \cdot \,  \bm{trdBuy} \, \cdot \, 10^3  $
</td>
<td style="text-align:right;border-top:none;padding-top:0px">
$ \scriptstyle \sum discFac \, \cdot \,  trdSellPrc \, \cdot \,  \bm{trdSell} \, \cdot \, 10^3 $
</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:left;padding-left:13.2em">
<ul class="liste">
<li><a href="../parameter_list/#Discount-rate-1">discount rate</a></li>
<li><a href="../parameter_list/#Trade-price-1">trade price</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:13.2em">
<ul class="liste">
<li><a href="../variables/#Buy-and-sell-1">buy and sell</a></li>
<li><a href="../variables/#Trade-cost-1">trade cost</a></li>
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



### Curtailment and loss-of-load cost equation

Determines costs of curtailment and unmet demand.

```@raw html
<table class="tabelle">
<tbody>
<tr>
<td><strong>name</strong></td>
<td>costCrt</td>
<td>costLss</td>
</tr>
<tr>
<td><strong>dimension</strong></td>
<td colspan="2"; style="text-align:center">$Ts_{sup}$, $R_{exp}$, $C$</td>
</tr>
<tr>
<td rowspan="2"><strong>formulation</strong></td>
<td style="text-align:left;border-bottom:none;padding-bottom:0px">
$ \scriptstyle \bm{costCrt} \, = $
</td>
<td style="text-align:left;border-bottom:none;padding-bottom:0px">
$ \scriptstyle \bm{costLss} \, =  $
</td>
</tr>
<tr>
<td style="text-align:right;border-right:none;border-top:none;padding-top:0px">
$ \scriptstyle \sum discFac \, \cdot \,  costCrt \, \cdot \,  \bm{crt} \, \cdot \, 10^3  $
</td>
<td style="text-align:right;border-top:none;padding-top:0px">
$ \scriptstyle \sum discFac \, \cdot \,  costLss \, \cdot \,  \bm{lss} \, \cdot \, 10^3 $
</td>
</tr>
<tr>
<td><strong>parameter</strong></td>
<td colspan="2"; style="text-align:left;padding-left:5.3em">
<ul class="liste">
<li><a href="../parameter_list/#Discount-rate-1">discount rate</a></li>
<li><a href="../parameter_list/#Cost-of-curtailment-and-loss-of-load-1">cost of curtailment and loss of load</a></li>
</ul>
</td>
</tr>
<tr>
<td><strong>variables</strong></td>
<td colspan="2"; style="text-align:left;padding-left:5.3em">
<ul class="liste">
<li><a href="../variables/#Curtailment-and-loss-of-load-1">curtailment and loss-of-load</a></li>
<li><a href="../variables/#Curtailment-and-loss-of-load cost-1">curtailment and loss-of-load cost</a></li>
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


# Limiting constraints

```@raw html

<p class="norm">
Limiting constraints are largely shaped by the corresponding limit parameters on <a href="../parameter_list/#Limits-on-quantities-dispatched">quantities dispatched</a>, <a href="../parameter_list/#Limits-on-expansion-and-capacity">expansion and capacity</a>, and by the <a href="../parameter_list/#Emission-limit">emission limit</a>.
</p>

<p class="norm">
The name of the constraint will correspond to the name of the respective parameter. Dimension and formulation depend on the way parameter data was provided, as explained <a href="../parameter_list/#Limits-on-quantities-dispatched">here</a>. Lastly, all constraints are stored in the <a href="../parts/#Limit">limit part</a>.
</p>
```
