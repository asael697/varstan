<img src="man/figures/varstan.png" width = 120 alt="brms Logo"/>[<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" align="right" width=120 alt="Stan Logo"/>](http://mc-stan.org)

**varstan**
===========

Varstan is a a package for bayesian estimation of structured time series
models,using the Hamiltonian monte carlo method, implemented with
[Stan](http://mc-stan.org/), a probabilistic language model in C++. The
aim of varstan is to have an interface of the most popular time series
modeles such as: sarima,garch, stochastic Volatility models (*SVM*),
Hiden Markov models(*HMM*), Dynamic Harmonic regresion, additive
non-linear models (*via*
[prophet](https://github.com/facebook/prophet)), univariate kalman
Filters, varma and bekk models.

On the beta version 0.5.0.000, the avaliable models are:

-   arima
-   garch
-   varma
-   bekk

The dynamic of varstan is to build your own model using one of the
avaliable model constructor, personalize your own priors (*check the Use
Priors vignette*), and fit your model using the varstan function. On the
next example we show you how to create and fit a simple bayesian arima
model.

### Installing varstan

Varstan is stil a beta version package, so currently installing it could
be challenging, we recomend to first install the package rstan, you can
follow the instalation procedure
[here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)

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

``` r
library(devtools)
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:\\Rtools\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

install_github("asael697/varstan")
```

### Simulated arma model

First step is make a simulation of a simple arma model with 200
observations as follows:

$$Y_t = \mu_0 + 0.338Y_{t-1} - 0.2279\epsilon_{t-1} - 0.2488\epsilon_{t-2}, \text{ } \epsilon_t \sim N(0,\sigma^2_0)$$

``` r
y = arima.sim(n = 205, list(ar =0.35, ma = c(0.2279, 0.2488)),sd = sqrt(0.1796))

yh1 = y[201:205]
y = ts(y[1:200])

autoplot(y)+labs(x = "time",title = "Simulated ARMA Process")
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

Proceding to built the arima model using the varstan constructor:

``` r
model1 = arima(y,p = 1,d = 0,q = 2)
```

Automatically varstan builds a bayesian arima model, with default normal
priors, you can check the model using the report function or just
printing the current model

``` r
model1
```

    ## 
    ## y ~ arima( 1 , 0 , 2 ) 
    ## Priors: 
    ##  Intercept:
    ## mu0 ~ normal (loc = 0 , scl = 1 )
    ## 
    ##  Scale Parameter: 
    ## sigma0 ~ half_t (loc = 0 ,scl = 1 ,df = 7 )
    ## 
    ##  ar parameters: 
    ## ar[ 1 ] ~ normal (mu =  0 , sd =  1 ) 
    ## 
    ##  ma parameters: 
    ## ma[ 1 ] ~ normal (mu =  0 , sd =  1 ) 
    ## ma[ 2 ] ~ normal (mu =  0 , sd =  1 ) 
    ## sigma ~ garch( 0 , 0 , 0 ) 
    ## 
    ##  Volatility components:
    ##  arch parameters: 
    ## There is no arch part defined 
    ## 
    ##  garch parameters: 
    ## There is no garch part defined 
    ## 
    ##  mgarch parameters: 
    ## There is no mgarch part defined 
    ## NULL

### Change prior distribution

To Change the default prior of one of the model parameter, just use the
*set\_prior* and *get\_prior* functions, in this example we change the
second ma component for a beta distribution on the $\Theta = [-1,1]$
parameter space.

$$\theta_2 \sim beta(2.5,2.5)$$

``` r
model1 = set_prior(model1,type = "ma",par1 = 2.5,par2 = 2.5,lag = 2,dist = "beta")
get_prior(model1,type = "ma")
```

    ## ma[ 1 ] ~ normal (mu =  0 , sd =  1 ) 
    ## ma[ 2 ] ~ beta (form1 =  2.5 , form2 =  2.5 )

To see more details of the avaliable priors and the model structure see
the vignettes *Use\_prior* and *arima\_models*, respectively.

### Estimation and parameter diagnositc

Fitting the personalized model defined above, is as simple as call the
varstan function, it will estimate the posterior sample using a
Hamiltonian montecarlo implemented using the NUTS algorithm in the rstan
package. In varstan function you can choose the number of chains, the
total amount of iterations for each chain, the iterations in the warm-up
face, and the adapt delta of the alogrithm.

In this example a hmc is run with 1 chain of 2000 iterations

``` r
sfit = varstan(model1,chains = 1,iter = 2000)
```

The function **summary**, provides a a full description of all the
fitted parameters in the model, the robust option, prints the median,
mad, and quantiles. If the robust option is false, the mean, se and
estimated credible intervals are printed. The *R**h**a**t* and efective
sample size for preliminary diagnostic if the simulated chains have
converged. More detail for parameter diagnostics could be found
[here](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html).

``` r
summary(sfit,robust= TRUE,conf = 0.95)
```

    ##            median    mad      2.5%     97.5%       ess   Rhat
    ## mu0       -0.0515 0.0519   -0.1638    0.0437  891.0121 0.9996
    ## sigma0     0.1894 0.0184    0.1555    0.2303  935.3320 0.9993
    ## phi        0.0742 0.1793   -0.2847    0.4148  991.6449 0.9999
    ## theta.1    0.4510 0.1673    0.1065    0.7674 1051.0946 0.9995
    ## theta.2    0.3030 0.1032    0.0769    0.4832  919.9781 0.9995
    ## loglik  -116.1694 1.3328 -120.2128 -114.5293 1039.0184 0.9994

You can plot the fitted values and posterior intervals using the
posterior\_fit and posterior\_intervals functions

``` r
fit = posterior_fit(sfit)
pe = data.frame(extract_stan(obj = sfit,pars = "fit"))
pe = posterior_interval(as.matrix(pe),prob = 0.95)

pe =  data.frame(t = 1:length(y),Estimate = fit, q2.5 = pe[,1],q97.5 = pe[,2])

ggplot(pe, aes(x = t, y = Estimate)) +
 geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey70") +
  geom_line(aes(y = Estimate),col = "blue")+
  labs(x = "time",title = "Fitted model")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

### Parameter Diagnostic

To get the simulated chain of an specific parameter use the
extract\_stan function, this is a replication of the
[extract](https://mc-stan.org/rstan/reference/stanfit-method-extract.html)
function in rstan for varstan objects, an it gets the simulated chains
of specified parameters.

``` r
post = extract_stan(sfit,pars = "phi",permuted = TRUE,inc_warmup = FALSE,include = TRUE)
post = as.data.frame(post)
```

A simple diagnostic plot for the ar $\phi$ parameter is possible, using the
[bayesplot package](https://mc-stan.org/bayesplot/) that visualize
posterior distributions and other diagnosis.

``` r
 color_scheme_set("viridis")

  p1 = mcmc_trace(post,  pars = "phi",
        facet_args = list(nrow = 2, labeller = label_parsed)) + 
        facet_text(size = 15)
  p2 = mcmc_hist(post, pars = "phi",facet_args = list(nrow = 2))+
    facet_text(size = 15)
  p3 = mcmc_acf(post, pars = "phi", lags = 10,)
  grid.arrange(p1,p2,p3,nrow = 2,layout_matrix = matrix(c(1,3,2,3),ncol=2,byrow=TRUE))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

For further exploration and diagnostic use the **get\_stan** function to
extract the whole rstan fit object and personalize diagnosis using other
packages ( [bloo](https://mc-stan.org/loo),
[bayesplot](https://mc-stan.org/bayesplot/) ,
[tidybayes](https://github.com/mjskay/tidybayes),
[posterior](https://github.com/jgabry/posterior) ).

``` r
stanfit = get_rstan(sfit)
class(stanfit)
```

    ## [1] "stanfit"
    ## attr(,"package")
    ## [1] "rstan"

### Forecasting h-step ahead

For making an h-step ahead forecast, you can use the
**posterior\_predict** function and you can compare it with the real
with the predictive\_error function, the last one gives a sample
distribution of the diference between the real values and the h-step
ahead predictive distribution of the model. In the next example, a 6
steps ahead forecast is presented

``` r
yh = posterior_predict(obj = sfit,h = 6,robust = TRUE)
yh <- cbind(
  Estimate = colMeans(yh), 
  Q5 = apply(yh, 2, quantile, probs = 0.05),
  Q95 = apply(yh, 2, quantile, probs = 0.95)
)
```

So the 6-steps ahead prediction of the model are:

``` r
yh
```

    ##          Estimate         Q5       Q95
    ## yh.1 -0.002792465 -0.7097385 0.7031202
    ## yh.2  0.032253432 -0.6731531 0.7398842
    ## yh.3 -0.021387415 -0.7283437 0.7173315
    ## yh.4 -0.064833582 -0.7732703 0.6141754
    ## yh.5 -0.060054876 -0.7735186 0.6519860
    ## yh.6 -0.077788218 -0.8098853 0.6881128

As well you can estimate the predictive\_errors, be aware that at the
begining we extract the last 5 observation of our simulated series, so
we can only compare the first 5 predictive errors as following:

``` r
eh  = predictive_error(sfit,newdata = yh1)
pred_error <- cbind(
  Estimate = colMeans(eh), 
  Q5 = apply(eh, 2, quantile, probs = 0.05),
  Q95 = apply(eh, 2, quantile, probs = 0.95)
)
pred_error
```

    ##        Estimate         Q5         Q95
    ## yh.1 -0.2697744 -0.9980883  0.42122617
    ## yh.2 -1.2046116 -1.8688320 -0.53133892
    ## yh.3 -0.9683216 -1.6962402 -0.26754793
    ## yh.4 -0.7071946 -1.4566310  0.05086277
    ## yh.5  0.2326692 -0.5366576  0.93207220

### The classical arima estimation

Finally lets compare our results with the classical arima estimation, as
we can see we have similar estimations to the classical model (*due to
the low informative prior*)

``` r
mc = stats::arima(y,order = c(1,0,2))
mc
```

    ## 
    ## Call:
    ## stats::arima(x = y, order = c(1, 0, 2))
    ## 
    ## Coefficients:
    ##          ar1     ma1     ma2  intercept
    ##       0.0626  0.4639  0.3140    -0.0622
    ## s.e.  0.1986  0.1872  0.1089     0.0572
    ## 
    ## sigma^2 estimated as 0.1832:  log likelihood = -114.26,  aic = 238.51

We can compare our residuals with the ones obtained in classical model,
and compare. As you will see in the next chunks, they both models have
similar results.

The residuals of the classical estimation are:

``` r
summary(mc$residuals)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -1.1974679 -0.2973318 -0.0104121 -0.0006024  0.2778172  1.5344517

The posterior mean of the residual statistics of the bayesian model are:

``` r
resid = posterior_residuals(sfit)
summary(resid)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -1.194332 -0.294985 -0.021783 -0.003146  0.284935  1.507109

And the residual plot for both models are:

``` r
r1=ts(cbind(resid,mc$residuals))
p1 = autoplot(r1)+
  scale_colour_discrete(name = "Dose", labels = c("Posterior mean", "Classical"))+
  labs(x = "time",y = "Residuals",title = "Residuals Compare")+
  guides(col = guide_legend(title =" "))
p2 = autoplot(ts(resid) )+labs(x = "time",y = "Residuals",title = "Posterior mean residuals")
p3 = autoplot(mc$residuals)+labs(x = "time",y = "Residuals",title = "Posterior mean residuals")

grid.arrange(p1,p2,p3,nrow = 2,layout_matrix = matrix(c(1,1,2,3),ncol=2,byrow=TRUE))
```

![](README_files/figure-markdown_github/unnamed-chunk-20-1.png)

### References

For further readings and references you can check

-   Bob Carpenter, Andrew Gelman, Matthew D. Hoffman, Daniel Lee, Ben
    Goodrich, Michael Betancourt, Marcus Brubaker, Jiqiang Guo, Peter
    Li, and Allen Riddell. 2017. Stan: A probabilistic programming
    language. Journal of Statistical Software 76(1). DOI
    10.18637/jss.v076.i01

-   Stan Development Team. 2018. Stan Modeling Language Users Guide and
    Reference Manual, Version 2.18.0.
    <a href="http://mc-stan.org" class="uri">http://mc-stan.org</a>

-   Rob J Hyndman and George Athanasopoulos. Forecasting: Principles and
    practice Monash University, Australia

-   Rob J. Hyndman, Y. Khandakar, Automatic Time Series Forecasting: The
    forecast Package for R
