# Tips

In addition to the tips listed here, it is always strongly advised to check the reporting files as introduced in the [Error handling](@ref) section.

### Performance

See the [Performance and stability](@ref) section for greater detail on this issue.

```@raw html
<p class="norm">
<ul>
<li>Use a commercial solver and the Barrier algorithm. Also, consider to skip Crossover and set a greater conversion tolerance ('BarConvTool' option in Gurobi).</li>
<li>Set <a href="../performance/#Variable-limits">general upper limits</a>, ideally only for the objective value.</li>
<li>Define <a href="../parameter_list/#Cost-of-curtailment-and-loss-of-load-1">loss-of-load costs</a> in order to create <a href="../variables/#Curtailment-and-loss-of-load-1">loss-of-load variables</a> for each energy carrier.</li>
<li>Reduce the temporal resolution for selected <a href="../sets/#Carriers">carriers</a> and set the temporal resolution of expansion to years.</li>
<li>Disable endogenous decommissioning by setting the <code>decomm</code> argument of the <a href="../model_object">model constructor</a> to <code>:none</code>.</li>
</ul>
</p>
```

### Debugging

```@raw html
<p class="norm">
<ul>
<li>Do not read-in your time-series data until the rest of your model is fully functioning.</li>
<li>Set a less detailed temporal resolution for debugging to speed up the process.</li>
<li>If your model is infeasible and you use Gurobi, try AnyMODs <a href="../api/#AnyMOD.printIIS"><code>printIIS(model_object)</code></a> function. It uses Gurobi's IIS function to obtain a set of constraints causing the infeasibility. Generally, this works better the more obvious a contradiction is (e.g. upper limits on expansion contradicts lower limit on installed capacity).</li>
<li>Define <a href="../parameter_list/#Cost-of-curtailment-and-loss-of-load-1">loss-of-load costs</a> in order to create <a href="../variables/#Curtailment-and-loss-of-load-1">loss-of-load variables</a> for each energy carrier.</li>
</ul>
</p>
```

### Workflow

```@raw html
<p class="norm">
<ul>
<li>Input files can be taken from different directories. For example, if you have different models with different regional scope, you could have a shared directory for technology data, that each of these models uses.</li>
<li>Additional columns within the input files and additional files within the input directories can be used for documentation.</li>
<li>The read-in parameter data can be manipulated between the construction of the <a href="../model_object">model object</a> and creation of the optimization problem using <code>createOptModel!(model_object)</code>. This is particular useful for sensitivity analyses.</li>
<li>Models can be extended using standard JuMP commands.</li>
<li>Version control can be used for model development.</li>
</ul>
</p>
```



### Common mistakes

(or at least mistakes that I believe to be common, because I made them when working with AnyMOD)

```@raw html
<p class="norm">
<ul>
<li><a href="../parameter_list/#Expansion-cost-1">Expansion</a> or <a href="../parameter_list/#Operation-cost-1">operating costs</a> not relating to capacity **before efficiency**.</li>
<li>Capacity limits of zero for certain technologies in certain regions (e.g. for wind offshore in Austria) are not explicitly set and as a result capacity is unlimited.</li>
<li>Limiting parameters are not defined correctly and limit the sum of variables across all years (see <a href="../parameter_list/#Limits-on-quantities-dispatched">Limits on quantities dispatched</a>).</li>
</ul>
</p>
```
