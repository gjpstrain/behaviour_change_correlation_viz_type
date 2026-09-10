These supplementary materials are minimally reproduced from https://anonymous.4open.science/r/behaviour_change_correlation_viz_type-FAF1/README.md, where an anonymised version of the repository for this paper is hosted.

The paper is written using Quarto in RStudio. All text, data wrangling, visualisation, modelling, analysis, and formatting is handled by the .qmd file in this repo. The .Rproject can be run locally, or a Docker container can be created to reproduce the precise computational environment -- Docker instructions are provided in the README here: (https://anonymous.4open.science/r/behaviour_change_correlation_viz_type-FAF1/README.md).

## Files and Folders

 - _extensions: quarto extensions, partials, and lua filters for ACM formatting.
 - additional_scripts: R scripts for processing experimental data for payment and anonymising it.
 - behaviour_change_correlation_viz_type_cache: computationally expensive models are cached to decrease render time. Setting `eval_models = TRUE` in the qmd rebuilds models from scratch.
 - behaviour_change_correlation_viz_type_files: rendered .pdf graphics.
 - data: data used to run the experiments, along with the resulting gathered data, in .csv format.
 - graphics: provided pre-rendered graphics.
 - renv: the project uses the `renv()` package in R to record package versions and dependencies. The Docker implementation uses this to install the correct package versions.
 - stimuli: the experimental stimuli. Provided here as linked in the paper.
 - behaviour_change_correlation_viz_type.qmd: location of all text, code, etc.
 - behaviour_change_correlation_viz_type.Rproj: R project file to handle paths.
 - Dockerfile: used by Docker to create the container.
 - renv.lock: renv() lockfile used to record package versions and dependencies.

NB: the YAML frontmater has been removed from the .qnd file to anonymise it, meaning it won't render. It is provided here illustratively, but the manuscript is reproducible from the anonymous.4open.science link provided above.