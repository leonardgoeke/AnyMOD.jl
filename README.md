
<img src="docs/src/assets/schriftzug_plus_logo.png" alt="logo" width="950px"/>

[anyMOD.jl](https://github.com/leonardgoeke/anyMOD.jl) is a [Julia](https://julialang.org/) framework to set up large scale linear energy system models with a focus on multi-period capacity expansion. It was developed to address the challenges in modelling high-levels of intermittent generation and sectoral integration. The documentation can be found here https://leonardgoeke.github.io/anyMOD.jl/dev/.
A working paper introducing the framework’s underlying method in greater detail can be found [here](https://github.com/leonardgoeke/anyMOD.jl/raw/master/docs/src/assets/anyMOD_working_paper.pdf).

The framework's key characteristic is, that all sets (time-steps, regions, energy carriers, and technologies) are each organized within a hierarchical tree structure. This allows for several unique features:

* The spatial and temporal resolution at which generation, use and transport of energy is modelled can be varied by energy carrier. For example, within the same model electricity can be modelled at an hourly, but gas at a daily resolution, while still allowing for technologies that convert gas to electricity, or vice versa. As a result, a substantial decrease of computational effort can be achieved.
* The substitution of energy carriers with regard to conversion, consumption and transport can be modelled. As an example, residential and district heat can both equally satisfy overall heat demand, but technologies to produce these carriers and how they are constrained are different.
* Inheritance within the trees can be exploited to dynamically obtain the model's parameters from the input data provided. In case of a technology’s efficiency for instance, parameters can vary by hour, day or be irrespective of time, depending on whether input data was provided hourly, daily or without any temporal dimension specified.

In addition, the framework provides functionalities to ensure numerical stability of the underlying optimization problem and assess it results. The tool relies on the [JuMP](https://github.com/JuliaOpt/JuMP.jl) package to create optimization problems and uses [DataFrames](https://juliadata.github.io/DataFrames.jl/stable/) to store and process their elements. The framework requires [Julia 1.3](https://julialang.org/downloads/) since all computationally intensive steps are parallelized using multi-threading.

The current roadmap for development is to extend documentation and introduce more technology specific features (ramping, demand-side management, etc.). The development of anyMOD is receiving funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 773406.

If you have any questions, need help or found a bug, please file a GitHub issue.
