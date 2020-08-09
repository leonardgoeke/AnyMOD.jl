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
# Error handling

To report on errors during model initialization and construction, status reports are printed into the console and a detailed report is written to the output directory. In the report are three different types of entries:

```@raw html
<ul class="liste">
<li><span style="color:rgb(0,188,0);font-weight:bold">green</span>: information on some automated modelling decision</li>
<li><span style="color:rgb(240,237,27);font-weight:bold">yellow</span>: some aspects of the model setup look suspicious</li>
<li><span style="color:rgb(205,49,49);font-weight:bold">red</span>: current model setup contains logical inconsistencies and will lead to an infeasible model</li>
</ul>

<p class="norm">
Entries of the third kind will throw an error and cause termination. If for example a wrong name is provided for the <a href="../parameter_list/#Emission-factor-1">emission factor</a> parameter in <a href="https://github.com/leonardgoeke/AnyMOD.jl/blob/master/examples/demo/par_emissions.csv"><code>par_emissions.csv</code></a>, the following reporting file results:
</p>

<table class="tabelle2">
<tbody>
<tr>
<td><strong>errStatus</strong></td>
<td><strong>section</strong></td>
<td><strong>location</strong></td>
<td style="border-right:none"><strong>message</strong></td>
</tr>
<tr>
<td><p style="color:rgb(0,188,0)">green</p></td>
<td>carrier mapping</td>
<td></td>
<td style="border-right:none">carrier heat inherited resolution from children</td>
</tr>
<tr>
<td><p style="color:rgb(0,188,0)">green</p></td>
<td>carrier mapping</td>
<td></td>
<td style="border-right:none">carrier gas inherited resolution from children</td>
</tr>
<tr>
<td><p style="color:rgb(205,49,49)">red</p></td>
<td>parameter read-in</td>
<td>definition</td>
<td style="border-right:none">parameter with the name emissionFactor does not exist</td>
</tr>
</tbody>
</table>

<p class="norm">
Optional arguments of the <a href="../model_object">model constructor</a> can be set to values between 1 and 3 to adjust the frequency of reporting:
</p>
```
- `reportLvl`: frequency of writing updates to the console
- `errWrtLvl`: frequency of writing to the report file
- `errCheckLvl`: frequency of checking for errors
