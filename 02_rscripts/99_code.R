# Note current script location
current_script_dir <- this.path::this.dir()
cat("Script directory:", current_script_dir, "\n")

# Set project root
project_root <- this.path::dirname2(current_script_dir)
setwd(project_root)
cat("Project root set to:", getwd(), "\n")


### For citing used R Packages
# https://github.com/vincentarelbundock/softbib
softbib::softbib(output = "software.bib")


#### Session Info 
writeLines(
  capture.output(sessionInfo()),
  ( "session-info.txt")
)


### Use renv for replicability

renv::init() #initialise it
renv::load() # to see if you are in sync
### install packages
packs <- c('causaldata', 'data.table', 'estimatr', 'evaluate', 'fixest', 'ggplot2',
           'haven', 'lfe', 'magrittr', 'modelsummary', 'openssl', 'ragg',
           'softbib', 'stringr', 'this.path', 'tinytable', 'xfun', 'renv'           )
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

renv::status() # to see sync status  
renv::install(packs) # to write packages to the renv file

renv::snapshot() # create the lock file.  


