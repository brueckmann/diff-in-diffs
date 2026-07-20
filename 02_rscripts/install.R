### install packages


packs <- c('dataverse', 'causaldata', 'tidyverse',
           'data.table', 'estimatr', 'evaluate', 'fixest', 
           'haven', 
            'magrittr', 'modelsummary', 'openssl', 'ragg',
           'softbib',  'this.path', 'tinytable', 'xfun' )

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-07-17"))

install.packages(packs)


# 'lfe', #Package which is only available in source form, and may need compilation of C/C++/Fortran: ‘lfe’
# Make sure you have a gfortran version on your computer
# in the terminal 
# use gfortran --version to see it's version 
# mine is: GNU Fortran (Homebrew GCC 16.1.0) 16.1.0
# in the terminal 
# use which gfortran to get its location  and add it below
# Sys.setenv(FC = "/opt/homebrew/bin/gfortran")
# install.packages("lfe")

# Another approach, that uses the fact we already installed gfortran is installing pak and trying it again: 
# install.packages("pak")
# pak::pkg_install("MatthieuStigler/lfe")