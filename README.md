
### Introduction

Varstan is a a package for bayesian estimation of structured time series models,using a Hamiltonian monte carlo, implemented with the so popular package "rstan". The aim of varstan is to have an interface of the most popular time series modeles such as: arima, garch, sarima, stochastic Volatility models (SVM), Hiden Markov models(HMM), seasonal fouier regresión, additive non-linear models (*via prophet package*), univariate kalman Filters, varma and bekk models.

On the beta version 0.5.0.000, the avaliable models are:
 
  + arima
  + garch
  + varma
  + Generalized t-student varma

The dynamic of varstan is to build your own model using one of the avaliable model constructor, personalize your own priors (*check the Use Priors vignette*), and fit your model using the varstan function. On the next example we show you how to create and fit a simple bayesian arima model.

### Troubleshooting Rstan / Rtools install for Windows:

Ensure recent version of R and Rtools is installed.

try including these lines in home/.R/makevars. :

    CXX14 = g++ -std=c++1y
    CXX14FLAGS = -O3 -Wno-unused-variable -Wno-unused-function

If makevars does not exist, run this code within R:

``` r
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else
    if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=native -mtune=native" else
    "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)
```


### Install varstan from git


For installing varstan package for git use the code on the next chunk:

```r
library(devtools)
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:\\Rtools\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

install_github("asael697/varstan")
```
 
