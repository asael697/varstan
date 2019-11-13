##############################################################################################
#                                    Script for varstan
##############################################################################################

# Working directory
setwd("~/")

#   version 0.1.000
library("rstantools")
library(devtools)

# Creating Package  skeleton
rstan_create_package(path = 'varstan',roxygen = TRUE)

# List of all the current files aviliable
setwd("~/varstan")
list.files(all.files = TRUE)

# Show the description file
file.show("DESCRIPTION")


file.show("Read-and-delete-me")
file.remove('Read-and-delete-me')


## Load all the current functions to the package

Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:\\Rtools\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

pkgbuild::compile_dll() # see note below
roxygen2::roxygenize(clean = TRUE)

devtools::missing_s3()

install.packages("../varstan", repos = NULL, type = "source")

#### Add vignettes to explain the R code

use_vignette("Intro-to-varstan")
use_vignette("Use-Priors")
use_vignette("varma-models")
use_vignette("arima-models")
use_vignette("garch-models")


