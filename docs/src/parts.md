# Parts

The `parts` field of the `anyModel` object structures the elements of a model's underlying optimization problem. Each of these parts has again three fields:

-	`par::Dict{Symbol,ParElement}` → parameter data (links zu list)
-	`var::Dict{Symbol,DataFrame}` → variables
-	`cns::Dict{Symbol,DataFrame}` → constraints

### Technology

The `part` objects for technologies are accessed via `modelObject.parts.tech[:techName]`. These objects include all expansion and dispatch related elements for the respective technology. Also, technology parts have additional fields to store information specific to technologies. Many of them directly related to the optional mappings for technologies. (LINK)
* `name::Tuple`: full name of technology as a series of nodes from the technology tree
* `carrier::NamedTuple`: energy carriers by index assigned to technology by groups (e.g. generation, use, ...)
* `balLvl::NamedTuple`: temporal and spatial resolution for expansion and balance of the technology
* `capaRestr::DataFrame`: specification of capacity restrictions required for technology
* `actSt::Tuple`: actively stored carriers  although they are not leafs by index (LINK)
* `type::Tuple`: type of technology (stock, mature, or evolving) (LINK)
* `disAgg::Bool`: if true, dispatch is modelled at expansion resolution instead of dispatch resolution
* `modes::Tuple`: different operational modes of technology   

### Exchange

The `part` object for exchange is accessed via `modelObject.parts.exc`. It includes all model elements relating to the exchange of energy carriers between regions. Exchange between two regions is enabled, if a value for the residual exchange capacity parameter can be obtained between these two regions. (LINK)

### Trade

For trade the `part` object is accessed via `modelObject.parts.trd`. It includes all model elements relating to buying and selling energy carriers from "outside" the model. Most importantly these are trade prices and variables for traded quantities. (LINKS)

### Balance

The `part` objects for energy balances is accessed via `modelObject.parts.bal`. It is used to store all model elements relevant for the energy balance. For example, this includes the demand parameter, curtailment variables or the energy balance constraint itself. (LINKS)

### Limit

Model elements used to impose certain limits on model variables are stored in `modelObject.parts.lim`. These include limiting parameters and the corresponding constraings enforcing these limits. (LINKS)

### Objective

The field `modelObject.parts.obj` gathers elements relating to the objective function of a model's underlying optimization problem. So far, the only available objective in AnyMOD is cost minimization and set by the following command.

```julia
setObjective!(:costs,anyM)
```
An objective function has to be set after the optimization problem itself was created.
