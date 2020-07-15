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

# Sets and Mappings

## Regions


erkläre read-in


```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none !important"><strong>region_1</strong></td>
<td style="border-right:none"><strong>region_2</strong></td>
</tr>
<tr>
<td style="border-right:none">East</td>
<td style="border-right:none">EastNorth</td>
</tr>
<tr>
<td style="border-right:none">East</td>
<td style="border-right:none">EastSouth</td>
</tr>
<tr>
<td style="border-right:none">West</td>
<td style="border-right:none">WestNorth</td>
</tr>
<tr>
<td style="border-right:none">West</td>
<td style="border-right:none">WestSouth</td>
</tr>
</tbody>
</table>
```
sdf

```@raw html
<img src="../assets/region.png" width="80%"/>
```

### Application context

- ``R_{exp}``: expansion regions
- ``R_{disp}``: dispatch regions
- ``R_a``, ``R_b``: exchange regions, undirected
- ``R_{from}``, ``R_{to}``​: exchange regions, directed

## Time-steps


link zu plotting funktionen

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none"><strong>timestep_1</strong></td>
<td style="border-right:none"><strong>timestep_2</strong></td>
<td style="border-right:none"><strong>timestep_3</strong></td>
<td style="border-right:none"><strong>timestep_4</strong></td>
</tr>
<tr>
<td style="border-right:none">2020</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">all</td>
<td style="border-right:none">d001</td>
<td style="border-right:none">hh0001</td>
<td style="border-right:none">h0001</td>
</tr>
<tr>
<td style="border-right:none">all</td>
<td style="border-right:none">d001</td>
<td style="border-right:none">hh0001</td>
<td style="border-right:none">h0002</td>
</tr>
<tr>
<td colspan="4"; style="text-align:center;border-right:none">&#8942;</td>
</tr>
</tbody>
</table>
```
sdf

```@raw html
<img src="../assets/timestep.png" width="80%"/>
```

### Application context

- ``Ts_{exp}``: expansion time-steps
- ``Ts_{dis}``: dispatch time-steps
- ``Ts_{sup}``: superordinate dispatch time-steps (usually years)



## Carriers


carrier names müssen unique sein

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none"><strong>carrier_1</strong></td>
<td><strong>carrier_2</strong></td>
<td style="border-right:none"><strong>timestep_dispatch</strong></td>
<td style="border-right:none"><strong>timestep_expansion</strong></td>
<td style="border-right:none"><strong>region_dispatch</strong></td>
<td style="border-right:none"><strong>region_expansion</strong></td>
</tr>
<tr>
<td style="border-right:none">electricity</td>
<td></td>
<td style="text-align:center;border-right:none">4</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">2</td>
</tr>
<tr>
<td style="border-right:none">heat</td>
<td>districtHeat</td>
<td style="text-align:center;border-right:none">3</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">2</td>
<td style="text-align:center;border-right:none">2</td>
</tr>
<tr>
<td style="border-right:none">gas</td>
<td>naturalGas</td>
<td style="text-align:center;border-right:none">2</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">2</td>
<td style="text-align:center;border-right:none">2</td>
</tr>
<tr>
<td style="border-right:none">gas</td>
<td>synthGas</td>
<td style="text-align:center;border-right:none">2</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
</tr>
<tr>
<td style="border-right:none">coal</td>
<td></td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
</tr>
<tr>
<td style="border-right:none">hydrogen</td>
<td></td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
<td style="text-align:center;border-right:none">1</td>
</tr>
</tbody>
</table>
```
sdf

```@raw html
<img src="../assets/carrier2.png" width="80%"/>
```

### Mapping

!!! Set a coarse temporal resolution for debugging
    Creating and especially solving a model is much faster if you decrease the temporal resolution of dispatch. Therefore, it is advisable to first test new models at less detailed temporal resolution. This will help you to spot and fix mistakes or unintended effects more efficiently.

**Optional mappings**
`carrier\_equality`: optionen: ("no","yes"), default: "no"

## Technologies


```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td style="border-right:none"><strong>technology_1</strong></td>
<td style="border-right:none"><strong>technology_2</strong></td>
<td><strong>technology_3</strong></td>
<td style="border-right:none"><strong>carrier_conversion_in</strong></td>
<td style="border-right:none"><strong>carrier_conversion_out</strong></td>
<td style="border-right:none"><strong>carrier_stored_in</strong></td>
<td style="border-right:none"><strong>carrier_stored_out</strong></td>
</tr>
<tr>
<td style="border-right:none">wind</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none"></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">solar</td>
<td style="border-right:none">openspace</td>
<td></td>
<td style="border-right:none"></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">solar</td>
<td style="border-right:none">rooftop</td>
<td>photovoltaic</td>
<td style="border-right:none"></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none"></td>
<td style="border-right:none">electricity</td>
</tr>
<tr>
<td style="border-right:none">solar</td>
<td style="border-right:none">rooftop</td>
<td>solarThermal</td>
<td style="border-right:none"></td>
<td style="border-right:none">heat</td>
<td style="border-right:none"></td>
<td style="border-right:none">heat</td>
</tr>
<tr>
<td style="border-right:none">gasPlant</td>
<td style="border-right:none">ccgt</td>
<td>cccgtNoCHP</td>
<td style="border-right:none">gas</td>
<td style="border-right:none">electricity</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">gasPlant</td>
<td style="border-right:none">ccgt</td>
<td>cccgtCHP</td>
<td style="border-right:none">gas</td>
<td style="border-right:none">electricity; districtHeat</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">gasPlant</td>
<td style="border-right:none">ocgt</td>
<td></td>
<td style="border-right:none">gas</td>
<td style="border-right:none">electricity</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">coalPlant</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">coal</td>
<td style="border-right:none">electricity; districtHeat</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">heatpump</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none">heat</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">gasStorage</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
<td style="border-right:none">gas</td>
<td style="border-right:none">gas</td>
</tr>
<tr>
<td style="border-right:none">gasBoiler</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">gas</td>
<td style="border-right:none">heat</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">hydro</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none">electricity</td>
</tr>
<tr>
<td style="border-right:none">electrolysis</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">electricity</td>
<td style="border-right:none">hydrogen</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">methanation</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">hydrogen</td>
<td style="border-right:none">synthGas</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
<tr>
<td style="border-right:none">fuelCell</td>
<td style="border-right:none"></td>
<td></td>
<td style="border-right:none">hydrogen</td>
<td style="border-right:none">electricity; districtHeat</td>
<td style="border-right:none"></td>
<td style="border-right:none"></td>
</tr>
</tbody>
</table>
```
sdf

```@raw html
<img src="../assets/tech.png" width="100%"/>
```

```@raw html
<p style="text-align:center;"><img src="../assets/convTech.svg" width="80%"/>
```

```@raw html
<p style="text-align:center;"><img src="../assets/stTech.svg" width="100%"/>
```

```@raw html
<p style="text-align:center;"><img src="../assets/mixTech1.svg" width="100%"/>
```

```@raw html
<p style="text-align:center;"><img src="../assets/mixTech2.svg" width="100%"/>
```

### Mapping
grafik zu tech tech kram




**Optional mappings**

```@raw html
<table class="tabelle2">
<tbody>
<tr>
<td><strong>column name</strong></td>
<td><strong>explanation</strong></td>
<td style="border-right:none"><strong>default</strong></td>
</tr>
<tr>
<td><code>modes</code></td>
<td>
<ul class="liste">
<li>different operational modes separated by a semicolon and a space</li>
<li>e.g. <code>moreHeat; moreElec</code> for CHP plant in the example</li>
</ul>
</td>
<td style="border-right:none">none</td>
</tr>
<tr>
<td><code>technology_type</code></td>
<td>
<ul class="liste">
<li>types controll expansion behaviour of technology, see <a href="https://arxiv.org/abs/2004.10184">Göke (2020)</a> for details</li>
<li>available options are:
<ul style="margin-top:0px">
<li style="margin-top:0px"><code>stock</code>: no expansion</li>
<li><code>mature</code>: expansion without differentiation by time-step of construction</li>
<li><code>emerging</code>: expansion with differentiation by time-step of construction</li>
</ul>
</ul>
</li>
</ul>
</td>
<td style="border-right:none"><code>mature</code></td>
</tr>
<tr>
<td><code>region_disaggregate</code></td>
<td>
<ul class="liste">
<li>if expansion is spatially more detailed than dispatch, by default the resolution of dispatch <br> is increased to the expansion level</li>
<li>using the keyword <code>no</code> prevents this behaviour and enforces the orginal dispatch resolution</li>
</ul>
</td>
<td style="border-right:none"><code>yes</code></td>
</tr>
<tr>
<td><code>timestep_expansion</code></td>
<td>
<ul class="liste">
<li>sets the resolution of expansion time-steps</li>
<li>cannot be more detailed than the superordinate dispatch level (usually years)</li>
</ul>
</td>
<td style="border-right:none">most detailed resolution <br> of carriers</td>
</tr>
<tr>
<td><code>region_expansion</code></td>
<td><ul class="liste">
<li>sets the resolution of expansion regions</li>
<li>default corresponds to the smallest value feasible</li>
</ul></td>
<td style="border-right:none">most detailed resolution <br> of carriers</td>
</tr>
<tr>
<td><code>carrier_stored_active</code></td>
<td><ul class="liste">
<li>by default, only leafs (nodes without any descendants) of  stored carriers are actively <br> stored (see <a href="https://arxiv.org/abs/2004.10184">Göke (2020)</a> for details)</li>
<li>non-leaf carriers to be stored actively can be added here</li>
<li>carriers are separated by a semicolon and a space just like modes</li>
</ul></td>
<td style="border-right:none">none</td>
</tr>
</tbody>
</table>
```
