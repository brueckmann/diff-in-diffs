
### Use renv for replicability

renv::init() #initialise it
renv::load() # to see if you are in sync
### install packages
packs <- c('dataverse', 'causaldata', 'data.table', 'estimatr', 'evaluate', 'fixest', 'ggplot2',
           'haven', 'lfe', 'magrittr', 'modelsummary', 'openssl', 'ragg',
           'softbib', 'stringr', 'this.path', 'tinytable', 'xfun', 'renv'           )
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

renv::status() # to see sync status  
renv::install(packs) # to write packages to the renv file

renv::snapshot() # create the lock file.  


