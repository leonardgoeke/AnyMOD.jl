```@raw html
<style>
p.norm {
font-weight: normal;
font-size: medium;
}
table.tabelle3 td {
  padding-left: 0.57em;
  padding-right: 0.57em;
  font-size: small;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-weight: normal;
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

Performance and stability
=================

mention numerical stability, no silver bullet, reference gurobi tips https://www.gurobi.com/documentation/9.0/refman/num_advanced_user_scaling.html

auch losses storage and exchange und minimum value for availability, aber beides nur kurz im eingangsabsatz

# Scaling


bla factors range in matrix, ziel 10^(-3),10^6 => maximum range of coefficients is 10^9


```@raw html
<img src="../assets/matrix1.svg" width="65%"/>
```

```@raw html
<ol style="list-style-position:inside">
<li style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Column scaling</font-size><br>
<p class="norm"; style = "text-indent:1.85em" >
according to factors provided, solves problem: to large range of coefficients withn a single equation
findet alles innerhalb des feldes der dataframes statt (wert der variable des optimierungsproblem ändert sich, aber wert des entsprechenden objekts im feld des variablen dataframes bleibt gleich)
first row: coefficients out of range => substitution
</p>

<img src="../assets/matrix2.svg" width="70%"/>

<p class="norm" >
list all variables scaling, erfahrungswert
</p>

<table class="tabelle3">
<tbody>
<tr>
<td><strong>field</strong></td>
<td><strong>scaled variables</strong></td>
<td style="border-right:none"><strong>default factor</strong></td>
</tr>
<tr>
<td><code>capa</code></td>
<td><ul class="liste">
<li><a href="../variables/#Installed-capacity-1">installed capacity</a></li>
<li><a href="../variables/#Expansion-1">capacity expansion</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{1}$</td>
</tr>
<tr>
<td><code>oprCapa</code></td>
<td><ul class="liste">
<li><a href="../variables/#Operated-capacity-1">operated capacity</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{2}$</td>
</tr>
<tr>
<td><code>dispConv</code></td>
<td><ul class="liste">
<li><a href="../variables/#Generation-and-use-1">generation and use</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{3}$</td>
</tr>
<tr>
<td><code>dispSt</code></td>
<td><ul class="liste">
<li><a href="../variables/#Charging-and-discharging-1">charging and discharging</a></li>
<li><a href="../variables/#Storage-level-1">storage level</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{4}$</td>
</tr>
<tr>
<td><code>dispExc</code></td>
<td><ul class="liste">
<li><a href="../variables/#Exchange-1">exchange</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{3}$</td>
</tr>
<tr>
<td><code>dispTrd</code></td>
<td><ul class="liste">
<li><a href="../variables/#Buy-and-sell-1">buy and sell</a></li>
<li><a href="../variables/#Curtailment-and-loss-of-load-1">curtailment and loss-of-load</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{3}$</td>
</tr>
<tr>
<td><code>costDisp</code></td>
<td><ul class="liste">
<li><a href="../variables/#Variable-costs-1">variable costs</a></li>
<li><a href="../variables/#Trade-costs-1">trade costs</a></li>
<li><a href="../variables/#Curtailment-and-loss-of-load-costs-1">curtailment and loss of load costs</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{3}$</td>
</tr>
<tr>
<td><code>costCapa</code></td>
<td><ul class="liste">
<li><a href="../variables/#Operating-costs-1">operating costs</a></li>
<li><a href="../variables/#Expansion-costs-1">expansion costs</a></li>
</ul></td>
<td style="text-align:center;border-right:none">$10^{3}$</td>
</tr>
<td><code>obj</code></td>
<td>objective variable</td>
<td style="text-align:center;border-right:none">$10^{0}$</td>
</tr>
</tbody>
</table>

</li>
<li style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Row scaling<br>
<p class="norm"; style = "text-indent:1.85em" >
applies same factor to all elements of a single equation => moves different coefficients into specified intervall, but not change absolute range
</p>

<img src="../assets/matrix3.svg" width="70%"/>

<p class="norm" >
sdf
</p>


<img src="../assets/matrix4.svg" width="70%"/>

<p class="norm" >
ggf. geht das nicht, dafür spezielle reporting option
</p>

</li>
</ol>
```



# Variable limits
