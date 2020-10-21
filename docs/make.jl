using Documenter
using AnyMOD

makedocs(sitename="AnyMOD.jl",
    authors = "Leonard Goeke",
    pages = [
        "Introduction" => "index.md",
        "Model Elements" => Any["Model object" => "model_object.md",
                                "Sets and Mappings" => "sets.md",
                                "Parts" => "parts.md",
                                "Parameter" => Any["Parameter overview" => "parameter_overview.md","Parameter list" => "parameter_list.md"],
                                "Variables" => "variables.md",
                                "Constraints" => "constraints.md"],
        "Tools" => Any["Reporting" => Any["Error handling" => "error.md","Data files" => "data.md","Plots" => "plots.md",],
                        "Performance and stability" => "performance.md"],
        "Annex" => Any["Tips" => "tips.md",
                        "API" => "api.md",
                        "Related material" => "related.md"]
        ],
    )

deploydocs(
    repo = "github.com/leonardgoeke/AnyMOD.jl.git",
    devbranch = "master"
)
