
setwd("~/varstan 0.0.5.000/R")


source("arima.R")
source("Misc.R")
source("prior.R")
source("garch.R")


library(fGarch)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.5))
y = garchSim(spec, n = 200)
rm(spec)

dat = garch(y)

report(dat)


setwd("~/varstan 0.0.5.000/inst/stan_files")

m <- stan_model(file = "garch.stan")

f <- sampling(object = m,data = dat,iter = 4000,chains = 1,
              pars = c("sigma","mu"),
              include = FALSE,control=list(adapt_delta=0.9))

# Classi estimation
garchFit(~ garch(1,1), data = y, trace = FALSE)
# Estimation varstan
f





