### install packages
packs <- c(
  "dataverse",
  "causaldata",
  "tidyverse",
  "data.table",
  "estimatr",
  "evaluate",
  "fixest",
  "ggplot2",
  "haven",
  "magrittr",
  "modelsummary",
  "openssl",
  "ragg",
  "this.path",
  "tinytable",
  "xfun"
)

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-07-17"))

# only install missing packages 
missing <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(missing)) install.packages(missing)
