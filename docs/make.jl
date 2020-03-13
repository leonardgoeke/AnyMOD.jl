using Documenter
using anyMOD

makedocs(sitename="anyMOD.jl",
    authors = "Leonard Goeke",
    pages = [
        "Introduction" => "index.md",
        "Model Elements" => Any["Sets" => "sets.md",
                                "Parameter" => "parameter.md",
                                "Parts" => "parts.md"],
        "Tools" => Any["Reporting" => "reporting.md",
                        "Results" => "results.md",
                        "Plots" => "plots.md",
                        "Numerical stability" => "numerical_stability.md"],
        "Annex" => Any["References" => "references.md", "Parameter list" => "parameter_list.md"]
        ],
    )

deploydocs(
    repo = "github.com/leonardgoeke/anyMOD.jl.git",
)
