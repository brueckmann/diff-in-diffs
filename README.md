# Example of a Difference-in-differences project


## Directory Structure

Below is an overview of the repository layout:

```
.
├── 01_data/                # All project data
│   ├── raw/                # Original, immutable data (do not modify)
│   ├── processed/          # Cleaned/transformed data (outputs from scripts)
│   └── README.md           # Data sources, descriptions, processing steps
├── 02_rscripts/            # Analysis scripts and utilities
│   ├── 01_data_cleaning.R  # Data input and preparation script (R)
│   ├── 02_descriptives.R   # Descriptive results (R)
│   ├── 03_modeling.R       # Diff-in-diff modeling script (R)
│   ├── utils/              # Reusable functions or modules
│   └── README.md           # Analysis workflow and script purposes
├── 03_output/              # Quarto output drafts, figures, documents
│   └── README.md           # tbd.
├── 04_presentation/        # Materials for talks, posters, slides
│   └── README.md           # Summarize presentations (title, date, format)
├── 05_misc/                # Supplementary files (proposals, notes, etc.)
│   └── README.md           # Describe contents
├── renv/                   # For replicability a renv.lock is used
│   └── README.md           # Describe contents
renv.lock
├── Diff-in-diffs.Rproj     # The R project (open it using R Studio)
└── README.md               # Project overview and setup instructions (this file)
└── session-info.txt        # Information on the R session
└── software.bib            # Cites R packages used
```

# Acknowledgements: 

Research Project Template stems from [LMU OSC](https://github.com/lmu-osc/research-project-template). 
