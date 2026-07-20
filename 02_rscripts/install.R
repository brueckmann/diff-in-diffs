### install packages


packs <- c('dataverse', 'causaldata', 'data.table', 'estimatr', 'evaluate', 'fixest', 'ggplot2',
           'haven', 
           #'lfe', #Package which is only available in source form, and may need compilation of C/C++/Fortran: ‘lfe’
            'magrittr', 'modelsummary', 'openssl', 'ragg',
           'softbib', 'stringr', 'this.path', 'tinytable', 'xfun' )

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-07-17"))

install.packages(packs)

