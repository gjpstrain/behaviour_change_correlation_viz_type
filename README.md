# Using Data Visualisation to Influence Donation Behaviour 

Files, data, and scripts required to fully recreate our study investigating how donation behaviour in an online experiment can be influenced by scatterplot design.

This paper can be accessed here:  placeholder

## Data

Raw data (.csv format) is located in the `data` folder. The R script `data_processing_for_payment.R` from the `additional_scripts` folder takes raw data straight from pavlovia.org, and writes a .csv file with a list of passed and failed participants to the `data` folder.

Once participants have been paid or excluded, use the `anonymisation.R` script in `additional_scripts` to remove identifiable prolific IDs.

NB: Aside from anonymisation, data are provided *as-is*. The **wrangle** function *must* be run to make the dataset usable.

### Analysis Within a Fully-Reproducible Computational Environment

Resources are provided for the full reproduction of the computational environment (R, Rstudio, and package versions) that was used for data wrangling, visualisation, statistical modelling, and reporting.

To begin, clone this repository to your local machine. With a [Podman machine started](https://podman.io/docs/installation) in the background, use a terminal (or cmd on Windows) to navigate to the cloned repository and type the following command:

```podman build -t donation_behaviour  .```

If you're using a machine with an ARM processor, such as Apple Silicon, use the following command to build an image:

```podman buildx build --platform linux/amd64 -t donation_behaviour .```

Then, type:

```podman run --rm -p 8787:8787 -e PASSWORD=password -e RUNROOTLESS=false donation_behaviour```

Once the container is running, open a web browser and type `localhost:8787` in the address bar. Enter the username `rstudio` and the password `password`. This will generate a fully functioning Rstudio session running from the Podman container.

### Re-creating the manuscript

Opening `donation_behaviour_correlation_viz_type.qmd` and using the 'Knit' button (or shift+k) will allow you to re-create a .pdf of the manuscript.

IMPORTANT: Models have been cached to increase performance. The cache will not be recognised automatically when using RStudio within the Podman container. eval_models must be set to FALSE in line Y in order to use the cached models. This will prevent knitr from executing the code for each model, but will 'lazyload' all cached models so they can be used in manuscript generation. Setting eval_models to TRUE in line Y will result in all models being re-generated.

The manuscript was written using the ACM CHI template.

Files used in generating manuscript:

 - `donation_behaviour_correlation_viz_type.qmd`: Full Quarto markdown script including text and all code.
 - `donation_behaviour_correlation_viz_type_cache/pdf`: folder containing cached models.
 - `data` 
	- `candidate_charities.csv`: used for generating Table X.
	- `exp_data_final.csv`: the raw data taken from Pavlovia.
	- `exp_trials.csv`: the data used to run the experiment.
 - `donation_behaviour.bib` for referencing.
 - `_extensions`: templates etc required for quarto to correctly render manuscript.

Knitting the manuscript may take some time depending on the performance of your computer.

### Other Files

 - `item_preparation.R`: script to generate all experimental items, practice items, and visual masks.

## Experiment Code and Materials

 - Experiment: https://pavlovia.org/Strain/donation_behaviour
 
## Pre-Registration

Pre-registrations for hypotheses with the OSF can be found here: OSF_link