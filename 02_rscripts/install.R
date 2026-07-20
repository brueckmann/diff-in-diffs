### install packages


packs <- c('dataverse', 'causaldata', 'tidyverse',
           'data.table', 'estimatr', 'evaluate', 'fixest', 
           'haven', 
           #'lfe', #Package which is only available in source form, and may need compilation of C/C++/Fortran: ‘lfe’
            'magrittr', 'modelsummary', 'openssl', 'ragg',
           'softbib',  'this.path', 'tinytable', 'xfun' )

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-07-17"))

install.packages(packs)

