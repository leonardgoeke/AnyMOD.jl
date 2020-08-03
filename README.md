[![Build Status](https://travis-ci.org/leonardgoeke/AnyMOD.jl.svg?branch=master)](https://travis-ci.org/leonardgoeke/AnyMOD.jl)
[![Gitter chat](https://badges.gitter.im/leonardgoeke/AnyMOD.jl.png)](https://gitter.im/AnyMOD-jl/community "Gitter chat")
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<img src="docs/src/assets/schriftzug_plus_logo.png" alt="logo" width="950px"/>


[AnyMOD.jl](https://github.com/leonardgoeke/AnyMOD.jl) is a [Julia](https://julialang.org/) framework to create large scale energy system models with multiple periods of capacity expansion formulated as linear optimization problems. It was developed to address the challenges in modelling high-levels of intermittent generation and sectoral integration. A comprehensive documentation can be found [here](https://leonardgoeke.github.io/AnyMOD.jl/dev/).

A comprehensive description of the framework's graph based methodology can found in the working paper [Göke (2020), AnyMOD - A graph-based framework for energy system modelling with high levels of renewables and sector integration](https://arxiv.org/abs/2004.10184). Its key characteristic is that all sets (time-steps, regions, energy carriers, and technologies) are organized within a hierarchical tree structure. This allows for two unique features:
* The level of temporal and spatial granularity can be varied by energy carrier. For instance, electricity can be modelled with hourly resolution, while supply and demand of gas is balanced daily. As a result, a substantial decrease of computational effort can be achieved. In addition, flexibility inherent to the system, for example in the gas network, can be accounted for.
* The degree to which energy carriers are substitutable when converted, stored, transported, or consumed can be modelled. As an example, residential and district heat can both equally satisfy heat demand, but technologies to produce these carriers are different.

The framework uses [DataFrames](https://juliadata.github.io/DataFrames.jl/stable/) to store model elements and relies on [JuMP](https://github.com/JuliaOpt/JuMP.jl) as a back-end. In addition, Julia's multi-threading capabilities are heavily deployed to increase performance. Since models entirely consist of .csv files, they can be developed open and collaboratively using version control (see [AnyMOD_example_model](https://github.com/leonardgoeke/AnyMOD_example_model) for example). The development of AnyMOD is receiving funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 773406.

If you have any questions, need help, or found a bug, please file a GitHub issue.
