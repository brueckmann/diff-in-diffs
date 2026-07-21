# run.R

# this works also when you are using binder's RStudio
# https://mybinder.org/v2/gh/brueckmann/did-binder-test/HEAD?urlpath=rstudio

cat("==========================================")
cat("Starting Analysis Pipeline")
cat("==========================================")
source("02_rscripts/install.R") 
source("02_rscripts/00_get_data.R")
source("02_rscripts/01_data_cleaning.R")
source("02_rscripts/02_descriptives.R")
source("02_rscripts/03_modeling.R")
cat("==========================================")
cat("Analysis Pipeline Completed Successfully!")
cat("==========================================")

cat("==========================================")
cat("Start Rendering Quarto Files to HTML")
cat("==========================================")

quarto::quarto_render("03_output/Comparison.qmd", quiet = TRUE)
quarto::quarto_render("03_output/Basic_diff_in_diffs.qmd", quiet = TRUE)

cat("==========================================")
cat("Rendered Quartos to HTML Successfully!")
cat("==========================================")


# Run session info generating code
cat(  "Executing: 98_write_sessioninfo.R") 
source("02_rscripts/98_write_sessioninfo.R")
cat(  "✓ Make session info completed successfully") 

