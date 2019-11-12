
setwd("~/varstan 0.0.5.000/R")

source("varma.R")
source("arima.R")
source("Misc.R")
source("prior.R")
source("garch.R")

setwd("~/varstan 0.0.5.000/inst/stan_files")

library(forecast)
library(rstan)
library(fGarch)
library(varstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

m1 <- stan_model(file = "varma.stan")  # Bayesian estimation
m2 <- stan_model(file = "tvarma.stan")  # Bayesian estimation

dat = varma(Astrovan,p = 2,q = 0,sd = mbekk(0,0,0))
report(dat)


fit <- sampling(object = m1,data = dat,iter = 4000,chains = 1,
              pars = get_params.varma(dat)$exclude,
              include = FALSE,control=list(adapt_delta=0.9))

# Fit compare Classical vs Bayesian
fit

summary.varma(dat,fit = fit)
