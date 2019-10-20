
[anyMOD](https://github.com/leonardgoeke/anyMOD) is a [Julia](https://julialang.org/) framework to set up large scale linear energy system models with a focus on multi-period capacity expansion. It was developed to address the challenges in modelling high-levels of intermittent generation and sectoral integration.

The framework's key characteristic is, that all sets (time-steps, regions, energy carriers, and technlogies) are each organized within a hierarchical tree structure. This allows for several unique features:

1) The spatial and temporal resolution at which generation, use and transport of energy is modelled can be varied by energy carrier. For example, within the same model electricity can be modelled at an hourly, but gas at a daily resolution, while still allowing for technologies that convert gas to electricity, or vice versa. As a result, a substantial decrease of computational effort can be achieved.

2) The substitution of energy carriers with regard to conversion, consumption and transport can be modelled. As an example, residential and district heat can both equally satisfy heat demand, but technologies to produce these carriers and how they are restricted are different.

3) Inheritance within the trees can be exploited to dynamically obtain the model's parameters from the input data provided. In case of a technology’s efficiency for instance, parameters can vary by hour, day or be irrespective of time, depending on weather input data was provided hourly, daily or without any temporal dimension specified.

The tool relies on the [JuMP](https://github.com/JuliaOpt/JuMP.jl) package to create optimization problems and uses [JuliaDB](https://juliadb.org/) to store and process their elements. The programs current preliminary stage has not been extensively tested yet. With the release of Julia 1.3, a utilization of multi-threading to speed-up model generation and an interface with packages for distributed decomposition methods for linear problems is planned.
