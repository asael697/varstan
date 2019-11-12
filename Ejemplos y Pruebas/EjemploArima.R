
setwd("~/varstan/R")

source("arima.R")
source("Misc.R")
source("prior.R")
source("garch.R")

setwd("~/varstan/inst/stan")

library(forecast)
library(rstan)
library(fGarch)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

m1 <- stan_model(file = "arima.stan")  # Bayesian estimation
m2 <- stan_model(file = "garch.stan")  # Bayesian estimation
m3 <- stan_model(file = "varma.stan")  # Bayesian estimation
m4 <- stan_model(file = "tvarma.stan") # Bayesian estimation


spec = garchSpec(model = list(ar = 0.25, ma =0.33,alpha = 0.2, beta = 0.7))
y = garchSim(spec, n = 200)

dat = arima(y,p = 1,d = 0,q = 1,sd = mgarch())


dat = set_prior(dat,type = "ar",par1 = 3,par2 =3,dist = "beta")
dat = set_prior(dat,type = "ma",par1 = 3,par2 =3,dist = "beta")

varstan::report(dat)


fit <- sampling(object = m1,data = dat,iter = 4000,chains = 1,
              pars = c("phi0","theta0","mu","epsilon,sigma"),
              include = FALSE,control=list(adapt_delta=0.9))

mc = stats::arima(y,order = c(1,1,1)) # Classic estimation

# Fit compare Classical vs Bayesian
mc
fit

# Residual extract
post = extract(fit)

fv =apply(post$fit,2,"median")
mr = apply(post$residual,2,"mean")

# Summary residuals
summary(mc$residuals)
summary( mr )

plot( y )
 lines( fv,col = "blue")
