```@raw html
<style>
table.tabelle2 td {
  padding-left: 7px;
  padding-right: 7px;
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
Parameter overview
=================

hier wird auch read-in allgemein behandelt zum teil  

# Read-in

analgous to sets, example input table: explain what gets ignored (here technology and comments)
additonal parameter/value columns possible

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
<td style="border-right:none">2020</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">rateDisc</td>
<td style="border-right:none;text-align:center">0.02</td>
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

# Inheritance

tipp: dont overdo it with the inheritance, will decrease performance

granularity of parameters acutally required within model
show inheritance process step-by-step for example

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

### Inheritance procedure

how inheritance algorithm populates data
```@raw html
<ol class="nest">
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Direct matches</font-size><br>
<p class="norm"; style = "text-indent:1.85em">
blablalksdjfkl aslkdjflksdjflk
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
<p class="norm"; style = "text-indent:1.85em">
sdfsdfsdf
<ol class="nest">
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$Ts_{sup}$	&#8594; upwards<br>
<p class="norm">
sdfsdf
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
sdf
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
sdf
</p>
</li>
<li class="nest"; style="font-size:medium;font-weight:bold">&nbsp;&nbsp;&nbsp;$R_{exp}$	&#8594; average<br>
<p class="norm">
sdf
</p>
</li>
</ol></li>
<li class="nest2"; style="font-size:large;font-weight:bold">&nbsp;&nbsp;&nbsp;Use default value<br>
<p class="norm"; style = "text-indent:1.85em">
sdf
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

### Modes of inheritance

list other types of inheritance:

- upwards
- average
- sum
- average*
- sum*
