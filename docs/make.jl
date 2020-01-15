using Documenter
using anyMOD

makedocs(sitename="anyMOD.jl",
    authors = "Leonard Goeke",
    pages = [
        "Introduction" => "index.md",
        "Model Elements" => Any["Sets" => "sets.md",
                                "Parameter" => "parameter.md",
                                "Variables and Constraints" => "var_const.md",
                                "Objective" => "objective.md"],
        "Obtain results" => Any["Reporting and Options" => "reportOpt.md",
                                "Visualizations" => "vis.md",
                                "CSV Tables" => "csvtab.md"]
        ],
    )

deploydocs(
    repo = "github.com/leonardgoeke/anyMOD.jl.git",
)
