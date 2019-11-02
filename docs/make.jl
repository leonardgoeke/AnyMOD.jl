using Documenter
using anyMOD

makedocs(sitename="anyMOD.jl",
    authors = "Leonard Goeke",
    pages = [
        "Introduction" => "index.md",
        "Quick start" => "quick_start.md"
        ],
    )

deploydocs(
    repo = "github.com/leonardgoeke/anyMOD.jl.git",
)
