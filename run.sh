#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

timestamp() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }

LOG=run.log
echo "$(timestamp) START run.sh" | tee "$LOG"

# Ensure renv is installed and restore project library
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos='https://cran.rstudio.com'); renv::restore()"

# Run analysis scripts
Rscript 02_rscripts/01_data_cleaning.R 2>&1 | tee -a "$LOG"
Rscript 02_rscripts/02_descriptives.R 2>&1 | tee -a "$LOG"
Rscript 02_rscripts/03_modeling.R 2>&1 | tee -a "$LOG"

echo "$(timestamp) FINISH run.sh" | tee -a "$LOG"
