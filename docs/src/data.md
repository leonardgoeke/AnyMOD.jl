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



# Aggregated results

blubbla

* `reportType`: blabla `:summary`,`:costs`,`:exchange`
* `rtnOpt`: blabla
    - `:csv`: asdf
    - `:raw`: asdf
    - `:rawDf`: sdf
    - `:csvDf`: as

### Summary

mention dimensions
wrtSgn = true


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
</table>
```
### Exchange

mention dimension

```@raw html
<table class="tabelle2">
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


siehe kosten variablen
```@raw html
<a href="../variables/#Costs">cost</a>
```




## Time-series


## Individual elements


`printObject`

`reportDuals`
