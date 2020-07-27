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

The by far fastest way to solve large linear problems are commercial solvers using the Barrier algorithm. Barrier is very sensitive to the numerical properties of the problem and often the algorithm is slowed or aborted due to numerical issues. For this reason, AnyMOD provides a range of tools that adjust the optimization problem in order to improve solver performance. These measures are based on recommendations from the [Gurobi documentation](https://www.gurobi.com/documentation/9.0/refman/num_advanced_user_scaling.html).

# Scaling


```@raw html
<p class="norm">
The coefficients within the matrix of an optimization problem are best be kept between $10^{-3}$ to $10^6$, which implies a maximum range of coefficients of $10^9$ within each row. To achieve this, AnyMOD automatically applies a two-step scaling process to each model that is created. This process is outlined based on the simplyfied optimization problem given below.
</p>

<p class="norm">
<img src="../assets/matrix1.svg" width="70%"/>
</p>

<p class="norm">
In the example, currently the targeted range is obviously violated in the first and second row. Also, the maximum range of coefficients in the second row is $10^{11}$ (= $\frac{10^{2}}{10^{-9}}$), which exceeds $10^9$.
</p>
```

### 1. Column scaling
```@raw html
<p class="norm">
The first step scales the columns (or variables) of the optimization problem using substitution. In the example, the variable $x_1$ is substitued with $x_{10^3} x'_1$:
</p>

<p>
<img src="../assets/matrix2.svg" width="70%"/>
</p>

<p class="norm">
After substitution the range of coefficients still does not lie within $10^{-3}$ to $10^6$, but the range of coefficients within each row does not exceed $10^9$ anymore. This is a prequisite to move all coefficients to the desired range in the next step.
</p>
<p class="norm">
In AnyMOD substitution is done directly within the <code>var</code> column of a variable's dataframe. As a result, only the value of the variable within the optimization problem is affected, but the value accessed by the user is already corrected and always complies with the units provided in the <a href="../variables">Variables section</a>.
</p>
<p class="norm">
The optional argument <code>scaFac</code> of the <a href="../model_object">model constructor</a> overwrites the default factors used for column scaling. As long as no numerical problems occur, it is not advised to do this.
</p>
<p class="norm">
The optional argument <code>checkRng</code> can be used to specify a maximum range of coefficients for each row. Rows (= constraints of the optimization problem) that violate this range after column scaling will be printed to the repl. This feature helps to find appropriate factors for column scaling.  
</p>
<p>
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
</p>
```

### 2. Row scaling
```@raw html

<p class="norm">
The second step scales the rows (or constraints) of the optimization problem by multiplying them with a constant factor. If the scaling of columns successfully decreased the range of coefficients to $10^9$, this allows to move coefficients into a range from $10^{-3}$ to $10^6$. In the example, the used factors are $10^{2}$ and $10^4$ for the first and second row, respectively, which results in the following optimization problem:
</p>

<p>
<img src="../assets/matrix4.svg" width="70%"/>
</p>

<p class="norm" >
By default, the range coefficients are scaled to in AnyMOD are even tighter: matrix coefficients range from $10^{-2}$ to $10^{5}$ and values on the right-hand side from $10^{-2}$ to $10^{2}$. Again, these defaults can be overwritten by the <code>coefRng</code> argument of the <a href="../model_object">model constructor</a>. If the scaling of columns earlier did not archieve an adequate a range of coefficients in each row, the scaling of rows will also fail to achieve the finally desired.
</p>
```

# Variable limits

upper limits einfach
