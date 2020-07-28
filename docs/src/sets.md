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
```@raw html
<p class="norm">
In AnyMOD sets like regions, time-steps, carriers, and technologies are organized as <a href="../api/#AnyMOD.Node"><code>Nodes</code></a> within hierarchical <a href="../api/#AnyMOD.Tree"><code>Trees</code></a>. They are defined by a mandatory input files named <code>set_region.csv</code>, <code>set_timestep.csv</code>, <code>set_carrier.csv</code>, and <code>set_technology.csv</code>, respectively. Relations between different sets (e.g. between carriers and technologies) are represented by mapping nodes within these trees to each other. This graph-based modelling approach is explained in detail in <a href="https://arxiv.org/abs/2004.10184">Göke (2020)</a>.</p>
```

Next, these key sets and how their tree structure and mappings are obtained from input files are introduced based on the [demo problem](https://github.com/leonardgoeke/AnyMOD.jl/tree/master/examples/demo). Also, additional sets not organized as hierarchical trees are briefly listed as well.


## Regions

The `set_region.csv` file defines all modelling regions. The file consists out of consecutive columns named `region_1`,  `region_2` etc. with each column relating to a level in the resulting tree. A region on a specific level is defined by writing its name into the respective column. To connect regions from different levels, they both need to be written into the same column. Names of regions are not required to be unique, not even on the same level.

The set file for regions from the demo problem is provided below.
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
Consequently, this file defines six regions on two different levels. `East` and `West` are on level 1 and their descendants `EastNorth` and `EastSouth` as well as `WestNorth` and `WestSouth` are on level 2. Since names of regions are not required to be unique, alternatively each of the four descendant regions could have also been named `North` or `South`. By using the `plotTree` function the corresponding hierarchical tree can be visualized and exported to the output directory:

```@raw html
<img src="../assets/region.png" width="80%"/>
```

### Application context

Throughout the model regions are used in different contexts and depending on the context different symbols are used:
- ``R_{exp}``:  Region of capacity expansion, used in all investment related model elements
- ``R_{disp}``: Region of dispatch, used in all dispatch related model elements
- ``R_a``, ``R_b``: Regions of exchange, element is not differentiated by direction of exchange (e.g. exchange capacities from ``R_a`` to ``R_b`` also apply from ``R_b`` to ``R_a``)
- ``R_{from}``, ``R_{to}``​: Regions of exchange, element is differentiated by direction of exchange (e.g. the exchange loses from ``R_{from}`` to ``R_{to}``​ do not apply from ``R_{to}`` to ``R_{from}``)

## Time-steps

The `set_timestep.csv` file defines all modelling time-steps, for both capacity expansion (usually years) and dispatch (for example hours). The file is structured analogously to the file for regions. Just as regions, names of time-steps are not required to be unique.

The first lines of the corresponding file in the demo problem are provided below.
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
This file makes use of the `all` keyword to reduce the number of rows required in the input file. This keyword serves as a placeholder for all nodes defined on the respective level. Consequently, in this case the following row
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
<td style="border-right:none">all</td>
<td style="border-right:none">d001</td>
<td style="border-right:none">hh0001</td>
<td style="border-right:none">h0001</td>
</tr>
</tbody>
</table>
```
is equivalent to these two rows:
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
<td style="border-right:none">d001</td>
<td style="border-right:none">hh0001</td>
<td style="border-right:none">h0001</td>
</tr>
<tr>
<td style="border-right:none">2030</td>
<td style="border-right:none">d001</td>
<td style="border-right:none">hh0001</td>
<td style="border-right:none">h0001</td>
</tr>
</tbody>
</table>
```
The keyword `all` can be extended to assign specific nodes:
* `all(node1,node2,node3)` assigns all listed nodes. Accordingly, in the example above `all(2020,2030)` would have achieved the same result as using `all`.
* `all(node1:node2)` assigns not only `node1` and `node2`, but also all nodes in between according to alphabetical order.

In the example above, the use of `all` results in each year having descendant nodes that represent days (level 2), 4-hour steps (level 3) and hours (level 4). Since names of time-steps are not required to be unique, these different nodes for each year can share the same names. A reduced version of the corresponding tree is plotted below:

```@raw html
<img src="../assets/timestep.png" width="80%"/>
```

### Application context

The following symbols are used to refer to time-steps depending on the context:

- ``Ts_{exp}``: Time-steps of capacity expansion
- ``Ts_{dis}``: Time-steps of dispatch
- ``Ts_{sup}``: Superordinate dispatch time-steps (usually years)

## Carriers

The hierarchical tree of energy carriers is defined analogously to regions and time-steps. The respective `.csv` table from the demo problem is given below. Unlike regions and timestep, carrier names are required to be unique. Carriers are always represented by the symbol ``C``.

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
The table above corresponds to the following tree:

```@raw html
<img src="../assets/carrier2.png" width="80%"/>
```

### Mapping

**Carrier resolution**

In addition to defining carriers, the `set_carrier.csv` file also maps them to regions and time-steps to specify the resolution there are modelled at. This is done separately for dispatch and expansion in the columns `timestep_dispatch`,`timestep_expansion`,`region_dispatch`, and `region_expansion`. The numbers in these columns correspond to levels of the respective trees. In the example displayed below, `4` in the `timestep_dispatch` column for `electricity` means, dispatch for electricity is modelled for each time-step on level 4. Going back to the definition of time-steps above, this corresponds to an hourly resolution.

!!! tip "Less detailed resolution for debugging"
    Creating and especially solving a model is much faster if the temporal resolution of dispatch is decreased. Therefore, it is advisable to first test new models at a less detailed temporal resolution. In the example this would be achieved by replacing the ´4´ for electricity with ´2´ to switch to a daily resolution. This will help you to spot and fix mistakes or unintended effects more efficiently.

AnyMOD checks the specified resolutions and will throw an error, if any logical inconsistencies are detected. Resolutions provided in a specific row only apply to the last carrier in that row. However, carrier on higher levels without a specified resolution, like `gas` in the example,  automatically inherit a resolution from their descendants.

**Optional mappings**

For reasons elaborated in [Göke (2020)](https://arxiv.org/abs/2004.10184), be default energy balances in AnyMOD are not formulated as equality constraints meaning supply can exceed demand. To overwrite this behaviour, an optional column named `carrier_equality` using the keywords `yes` and `no` can be added to the file, where `yes` will enforce an equality constraint. Carriers are represented by the symbol ``Te``.

## Technologies

The hierarchical tree of technologies is defined analogously to regions and time-steps. The respective `.csv` table from the demo problem is given below. Unlike regions and timesteps, technology names are required to be unique.

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
Within a model only nodes without any descendants are actual technologies. The remaining nodes have the sole purpose of organizing them. This facilitates the read-in of parameter data and formulation of certain constraints (e.g. the available rooftop area limiting the summed capacity of `photovoltaic` and `solarThermal`). The resulting hierarchical tree from the table above is displayed below.
```@raw html
<img src="../assets/tech.png" width="100%"/>
```

### Mapping

**Converted and stored carriers**

The `.csv` table above does not only define the hierarchical tree of technologies, but also maps carriers to these technologies. This assignment differentiates between in- and output carriers and between conversion and storage.

```@raw html
<p class="norm">
In AnyMOD conversion generally refers to one or multiple carrier being used or generated as displayed in the diagram below. In the diagram expressions in italic refer to <a href="../variables">model variables</a> and expressions in boxes to <a href="../parameter_list">parameters</a>.
</p>
<p style="text-align:center;"><img src="../assets/convTech.svg" width="80%"/>
```
The technology `ccgtCHP` from the example is a pure conversion technology converting `gas` to `electricity` and `districtHeat`, because `conversion_input` and `conversion_output` are assigned accordingly. It is not necessary for a conversion technology to have both, an in- and and output carrier. For instance, in the example renewables like `wind` only have an output.

Storage is analogously described by the diagram below. In the example, `hydro` is a pure storage technology, because `electricity` is assigned to both `storage_input` and `storage_output`. Storage is not limited to a single carrier and a technology can store an arbitrary number of different carriers.
```@raw html
<p style="text-align:center;"><img src="../assets/stTech.svg" width="100%"/>
```

Assigning carriers as a storage output (or input) means they can be discharged to (or charged from) the general energy balance. Technologies that can only discharge but not charge a carrier from the energy balance are conceivable as well. However, this only makes sense, if the carrier can be generated internally to charge the storage in the first place. The interplay of conversion and storage in such a case is illustrated by the following diagram:
```@raw html
<p style="text-align:center;"><img src="../assets/mixTech1.svg" width="100%"/>
```
Such a technology is created by assigning the same carrier to `conversion_output` and `storage_output`, but not to `storage_output`. In the example, this is the case for the technology `photovoltaic`. It is intended to represent a residental photovoltaic panel combined with a home battery that cannot be charged from the gird, but from the panel.

Also the opposite case can be modelled: A carrier charged from outside, but only discharged within the technology. This only makes sense if the carrier is being used within the technology's conversion process. The corresponding diagram is given below and could represent a gas power plant with an onside gas storage.
```@raw html
<p style="text-align:center;"><img src="../assets/mixTech2.svg" width="100%"/>
```

**Optional mappings**

The following table lists all optional columns that can be specified within in `set_technology.csv` file to overwrite a default behaviour.

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

## Other Sets

Other sets in AnyMOD are not organized in hierarchical trees and might even be empty in a specific model. These are listed here.

### Modes

The set of modes includes operational modes defined for technologies. Modes are represented by the symbol ``M``.
