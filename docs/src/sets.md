```@raw html
<style>
table.tabelle2 td {
  padding-left: 7px;
  padding-right: 7px;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-size: small;
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
<td style="border-right:none"><strong>region_1</strong></td>
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

Optional mappings
- carrier\_equality: optionen: ("no","yes"), default: "no"

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




Optional mappings
- modes
- carrier\_stored\_active
- technology\_type
- region\_disaggregate
- timestep_expansion
- region_expansion
