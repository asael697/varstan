#' Constructor garch(s,k,h) object
#'
#' Constructor of the garch(s,k,h) object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage arima(ts,s,k)
#'
#' @param ts an multivariate time series
#' @param s an integer with the order of the arch(s) part
#' @param k an integer with the order of the garch(k) part
#' @param h an integer with the order of the mean part
#' @param mu0: a list with the mgarch parameters
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @seealso \code{\link{arima}}
#'
#' @return  a list with the components
#' \itemize{
#'  \item n: the length of the time series
#'  \item p: an integer with the order of the ar coefficients
#'  \item d: an integert with the order of diferences
#'  \item q: an integer with the order of the ma coefficients
#'  \item y: vector with the multivariate time series
#'  \item prior_ar: a matrix with the hyper-parameters for the var coeff icient
#'  \item prior_ma: a matrix with the hyper-parameters for the ma  coefficients
#' }
#'
garch = function(ts,s = 1,k = 1, h = 0){
  n = length(as.numeric(ts))
  y = as.numeric(ts)
  m1 = list(n = n,
            s = no_negative_check(s),
            k = no_negative_check(k),
            h = no_negative_check(h),
            y = y)
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_arch    = matrix(rep(c(3,3,1,2),s),ncol = 4,byrow = TRUE)
  m1$prior_garch   = matrix(rep(c(3,3,1,2),k),ncol = 4,byrow = TRUE)
  m1$prior_mgarch  = matrix(rep(c(3,3,1,2),h),ncol = 4,byrow = TRUE)

  attr(m1,"class") = "garch"
  return(m1)
}
#' Checks if is a garch object
#' @param obj: a  garch object
#' @export
#'
is.garch = function(obj){
  y = FALSE
  if(class(obj) == "garch") y = TRUE
  return (y)
}
#' Print a report of the constructed model
#' @param dat: a garch object
#'
#' @method report garch
#' @export
#'
report.garch = function(dat){
  cat("\n")
  cat("y ~ garch(",dat$s,",",dat$k,",",dat$h,") \n")
  cat("Priors: \n Intercept:\n")
  get_prior(dat,type = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(dat,type = "sigma0")
  cat("\n arch parameters: \n")
  get_prior(dat,type = "arch")
  cat("\n garch parameters: \n")
  get_prior(dat,type = "garch")
  cat("\n mgarch parameters: \n")
  get_prior(dat,type = "mgarch")
}
#' Excluded parameters in a  Garch model
#' @export
#'
#' Excluded parameters in a  Garch model
#'
get_params_garch = function(dat,...){
  include = c("mu0","sigma0")
    if(dat$s > 0) include = c(include,"alpha")
    if(dat$k > 0) include = c(include,"beta")
    if(dat$h > 0) include = c(include,"mgarch")

  exclude = c("mu","epsilon","sigma")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' Fit a mgarch model
#'
#' Fit a garch(s,k,h) model  in STAN
#'
#' The function returns a list with the fitted model in stan
#'
#' @usage  fit.garch(model,chains,iter,warmup,...)
#'
#' @param model A time series object for the varstan models
#' @param chains the number of chains to be run
#' @param iter the number of iteration per chain
#' @param warmup the number of initial iteration to be burned
#' @param adapt.delta the thin of the jumps in a HMC method
#'
#' @import rstan
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a stanfit object
#'
fit.garch = function(model,
                     chains=4,
                     iter=2000,
                     warmup=floor(iter/2),
                     adapt.delta = 0.90,...){
  if(is.garch(model)){
    stanfit = rstan::sampling(stanmodels$garch,
                              data = model,
                              chains = chains,
                              iter = iter,
                              warmup = warmup,
                              control = list(adapt_delta = adapt.delta),
                              par = get_params.garch(model)$exclude,
                              include = FALSE)
  }
  else{
    stanfit = NULL
    print("There is no accurate data to fit an garch model \n")
  }
  return(stanfit)
}
#' Summary of  an arima model
#'
#' Summary of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  summary.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
summary.garch = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params.garch(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
#' point estimate of an garch model
#'
#' get the point estimate of an garch(s,k,h) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  point_estimate.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
point_estimate.garch = function(model,fit,robust = FALSE,...){
  par1 = get_params.garch(model)$include
  post = data.frame(rstan::extract(fit,par1, permuted = TRUE))
  if(robust) pe = apply(post,2,median)
  else pe = apply(post,2,mean)
  return( as.list(pe) )
}
#' Get the fitted values of an garch model
#'
#' get the fitted values of an garch(s,k,h) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: the arima model
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit.garch = function(model,fit,robust = FALSE,...){
  qq = c(1-conf,conf)*100
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
#' Get the residuals of a garch model
#'
#' get the residuals of a garch(s,k,h) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals.garch = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
