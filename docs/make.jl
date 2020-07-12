using Documenter
using AnyMOD

makedocs(sitename="AnyMOD.jl",
    authors = "Leonard Goeke",
    pages = [
        "Introduction" => "index.md",
        "Model Elements" => Any["Sets and Mappings" => "sets.md",
                                "Parts" => "parts.md",
                                "Parameter" => Any["Parameter overview" => "parameter_overview.md","Parameter list" => "parameter_list.md"],
                                "Variables" => "variables.md",
                                "Constraints" => "constraints.md"],
        "Tools" => Any["Reporting" => Any["Plots" => "plots.md","Data files" => "data.md"],
                        "Performance and stability" => "performance.md"],
        "Annex" => Any["Functions and types" => "functions.md",
                        "Common mistakes" => "common_mistakes.md"]
        ],
    )

deploydocs(
    repo = "github.com/leonardgoeke/AnyMOD.jl.git",
)
