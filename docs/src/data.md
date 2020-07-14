```@raw html
<style>
p.norm {
font-weight: normal;
font-size: medium;
}
table.tabelle3 td {
  padding-left: 7px;
  padding-right: 7px;
  font-size: small;
  border-right: solid 1px;
  border-color: #dbdbdb;
  font-weight: normal;
}
</style>
```

Data files
=================

sdfdsfsdf, wie reporting, printObject und getDuals?

# Direct results



# Summarized results

```@docs
reportResults
```

### Summary

wrtSgn = true

* `expConv`, `expStIn`, `expStOut`, `expStSize`
* `capaConv`, `capaStIn`, `capaStOut`,  `capaStSize`
* `oprCapaConv`, `oprCapaStIn`, `oprCapaStOut`, `oprCapaStSize`
* `use`, `gen`, `stIn`, `stOut`, `stExtIn`, `stExtOut`, `stIntIn`, `stIntOut`,
*  `import`,`export`,`crt`, `lss`, `trdBuy`, `trdSell`
* `emission`

```@raw html
<table class="tabelle3">
<tbody>
<tr>
<td><strong>variable</strong></td>
<td style="border-right:none">><strong>definition</strong></td>
</tr>

<tr>
<td><code>flhConv</code></td>
<td style="border-right:none">$ \frac{oprCapaConv}{\sum use + stIntOut} $ oder $ \frac{oprCapaConv}{\sum gen + stIntIn} $</td>
</tr>

<tr>
<td><code>flhStIn</code></td>
<td style="border-right:none">$ \frac{oprCapaStIn}{\sum stExtIn + stIntIn} $</td>
</tr>

<tr>
<td><code>flhStOut</code></td>
<td style="border-right:none">$ \frac{oprCapaStOut}{\sum stExtOut + stIntOut} $</td>
</tr>

<tr>
<td><code>cycStIn</code></td>
<td style="border-right:none">$ \frac{oprCapaStSize}{\sum stExtIn + stIntIn} $</td>
</tr>

<tr>
<td><code>cycStOut</code></td>
<td style="border-right:none">$ \frac{oprCapaStSize}{\sum stExtOut + stIntOut} $</td>
</tr>


</tbody>
</table>
```

### Exchange

### Costs

cost variables like listed

# Time-series
