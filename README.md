
### Introduction

Varstan is a package for analyze time series with common structured
models such as arima, garch, var, bekk, SVM, HMM, ETS and others. On
this beta version the models varbekk and varbekkM(1) are currently
available.

The dynamic is, first to create the model structure using a
constructure, then define the prior parameters, next call the varstan
function to estimate the chosen model in a bayesian scheme using STAN.
Finally the summary, point\_estimate, model\_diagnostic, forecast
functions are used to evaluate the obtained results.

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
 
