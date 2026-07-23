# Difference-in-differences project

## Usage

Everything in this repo runs using `bash run.sh`. 
- This will perform all scripts in `/02_rscripts`, i.e.,
  -   downloading data into `/01_data`,
  -   generating figures and tables saved into `/03_output/figures` and `/03_output/tables`,
  -   rendering the quarto (`.qmd`) documents in `/03_output` to `html`.


## This repository is binderized on [mybinder.org](https://mybinder.org/v2/gh/brueckmann/diff-in-diffs/HEAD)

In Binder [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/brueckmann/diff-in-diffs/HEAD) open a terminal and run `bash run.sh` . 


<details>

<summary> Open step-by-step instructions with screenshots</summary>

1) Open [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/brueckmann/diff-in-diffs/HEAD) and navigate to the Terminal (at the bottom left) <img width="1365" height="823" alt="Binder_JN_1" src="https://github.com/user-attachments/assets/08611b75-afd0-41b5-9c2b-050b860787c5" />
2) open a terminal <img width="1365" height="823" alt="Binder_Terminal_1" src="https://github.com/user-attachments/assets/84f4a0f0-eb7f-418f-bd63-047f73d3576d" />
3) and, then, run the script <img width="1365" height="823" alt="Binder_Terminal_2" src="https://github.com/user-attachments/assets/6ae0271a-cc0f-4e65-bf21-7a7a3e8f9f9b" />

</details>

Alternatively, if you want to use `RStudio` on [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/brueckmann/diff-in-diffs/HEAD?urlpath=rstudio), you may run `run.R`, e.g. using `source("run.R")`.

<details>

<summary> Open step-by-step instructions with screenshots</summary>

1) Launch the RStudio [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/brueckmann/diff-in-diffs/HEAD?urlpath=rstudio) <img width="1365" height="823" alt="Binder_Rstudio_1" src="https://github.com/user-attachments/assets/b19d6f08-51c4-44d5-b6ce-11efa3011081" />
2) and, then, run  `run.R` <img width="1365" height="823" alt="Binder_Rstudio_2" src="https://github.com/user-attachments/assets/75aec17e-b424-4b52-8c7f-26d784e26669" />

</details>


## Directory Structure
Below is an overview of the repository layout:

```
├── .binder/                    # Binder configuration files
│   ├── install.R                   # R packages to install
│   ├── postBuild                   # Post-build script
│   └── runtime.txt                 # R version specification
├── 01_data/                    # All project data 
│   ├── raw/                        # Original, immutable data (do not modify)
│   ├── processed/                  # Cleaned/transformed data (outputs from scripts)
│   └── README.md                   # Data sources, descriptions, processing steps
├── 02_rscripts/                # Analysis scripts and utilities
│   ├── 00_get_data.R               # Data download script (R)
│   ├── 01_data_cleaning.R          # Data input and preparation script (R)
│   ├── 02_descriptives.R           # Descriptive results (R)
│   ├── 03_modeling.R               # Diff-in-diff modeling script (R)
│   ├── 98_write_session_info.R     # Write R session information
│   ├── install.R                   # install R packages
│   └── README.md                   # Analysis workflow and script purposes
├── 03_output/                  # Output figures, tables and Quarto and HTML outpus
│   ├── figures/                    # To store figures generated from R Code
│   ├── tables/                     # To store tables generated from R Code
│   ├── Basic_diff_in_diffs.qmd     # Basic diff-in-diffs presentation
│   ├── Comparison.qmd              # Comparison analysis document
│   ├── references.yaml             # Bibliography references
│   └── README.md                   # Output documentation
├── diff_in_diffs.Rproj         # The RStudio R project
├── github_info.qmd             # Explains the very basics of Git and Github usage
├── .gitignore                  # Specifies what git should ignore
├── run.R                       # R script to run everything
├── run.sh                      # Shell script to run everything
└── session_info.txt            # Information on the R session
└── README.md                   # Project overview and setup instructions (this file)
```



# Acknowledgements: 

Research Project Template stems from [LMU OSC](https://github.com/lmu-osc/research-project-template). 
