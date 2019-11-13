#' Constructor arima(p,d,q) object
#'
#' Constructor of the arima(p,d,q) object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage arima(ts,p,d,q)
#'
#' @param ts an multivariate time series
#' @param p an integer with the order of the ar(p) part
#' @param d an integer with the order of the diference (d)
#' @param q an integer with the order of the ma(q) part
#' @param sd: a list with the mgarch parameters
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @seealso \code{\link{garch}}
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
arima = function(ts,p = 1,d = 0, q = 1,sd = mgarch(s=0,k =0,h = 0)){
  n = length(as.numeric(ts))
  y = as.numeric(ts)
  m1 = list(n = n,
            p = no_negative_check(p),
            d = no_negative_check(d),
            q = no_negative_check(q),
            y = y)
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_ar  = matrix(rep(c(0,1,1,1),p),ncol = 4,byrow = TRUE)
  m1$prior_ma  = matrix(rep(c(0,1,1,1),q),ncol = 4,byrow = TRUE)
  m1$sd = "none"
  if( !is.null(sd) ){
    m1$s = sd$garch_order[1]
    m1$k = sd$garch_order[2]
    m1$h = sd$garch_order[3]
    m1$prior_arch =  sd$prior_arch
    m1$prior_garch =  sd$prior_garch
    m1$prior_mgarch =  sd$prior_mgarch
    m1$sd = "mgarch"
  }
  attr(m1,"class") = "arima"
  return(m1)
}
#' Checks if is an arima object
#'
#' @param obj: an arima object
#'
#' @export
#'
is.arima = function(obj){
  y = FALSE
  if(class(obj) == "arima") y = TRUE
  return (y)
}
#' Print a report of the constructed model
#'
#' @param dat: an arima object
#'
#' @method report arima
#' @export
#'
report.arima = function(dat){
  cat("\n")
  cat("y ~ arima(",dat$p,",",dat$d,",",dat$q,") \n")
  cat("Priors: \n Intercept:\n")
  get_prior(dat,type = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(dat,type = "sigma0")
  cat("\n ar parameters: \n")
  get_prior(dat,type = "ar")
  cat("\n ma parameters: \n")
  get_prior(dat,type = "ma")
  if(dat$sd == "mgarch"){
    cat("sigma ~ garch(",dat$s,",",dat$k,",",dat$h,") \n")
    cat("\n Volatility components:")
    cat("\n arch parameters: \n")
    get_prior(dat,type = "arch")
    cat("\n garch parameters: \n")
    get_prior(dat,type = "garch")
    cat("\n mgarch parameters: \n")
    get_prior(dat,type = "mgarch")
  }
}
#' Adds a garch(s,k,h) object to an arima model
#'
#' Adds a garch(s,k,h) object to an arima model
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage garch(s,k,h)
#'
#' @param s an integer with the order of the arch(s) part
#' @param k an integer with the order of the garch(k) part
#' @param h an integer with the order of the mgarch(h) part
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export

mgarch = function(s=1,k=1,h=0){
  ml = list()
  ml$garch_order = c(no_negative_check(s),no_negative_check(k),no_negative_check(h))
  ml$prior_arch    = matrix(rep(c(3,3,1,2),s),ncol = 4,byrow = TRUE)
  ml$prior_garch   = matrix(rep(c(3,3,1,2),k),ncol = 4,byrow = TRUE)
  ml$prior_mgarch  = matrix(rep(c(3,3,1,2),h),ncol = 4,byrow = TRUE)
  ml$sd = "mgarch"
  return(ml)
}
#' Excluded parameters in a  varbekk model
#'
#' @export
#'
get_params_arima = function(dat,...){
    include = c("mu0","sigma0")
    if(dat$p > 0) include = c(include,"phi")
    if(dat$q > 0) include = c(include,"theta")
    if(dat$sd == "mgarch"){
      if(dat$s > 0) include = c(include,"alpha")
      if(dat$k > 0) include = c(include,"beta")
      if(dat$h > 0) include = c(include,"mgarch")
    }
    exclude = c("phi0","theta0","mu","epsilon","sigma")
    pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' Fit an arima model
#'
#' Fit a arima(p,d,q) model  in STAN
#'
#' The function returns a list with the fitted model in stan
#'
#' @usage  fit.arima(model,chains,iter,warmup,...)
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
fit.arima = function(model,
                       chains=4,
                       iter=2000,
                       warmup=floor(iter/2),
                       adapt.delta = 0.90,...){
  if(is.arima(model)){
      stanfit = rstan::sampling(stanmodels$arima,
                                data = model,
                                chains = chains,
                                iter = iter,
                                warmup = warmup,
                                control = list(adapt_delta = adapt.delta),
                                par = get_params.arima(model)$exclude,
                                include = FALSE)
    }
  else{
    stanfit = NULL
    print("There is no accurate data to fit an arima model \n")
  }
  return(stanfit)
}
#' Summary of  an arima model
#'
#' Summary of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  summary.arima(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired probability in the
#' credible intervals
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
summary.arima = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params.arima(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
#' point estimate of an arima model
#'
#' get the point estimate of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  point_estimate.arima(model,fit)
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
point_estimate.arima = function(model,fit,robust = FALSE,...){
  par1 = get_params.arima(model)$include
  post = data.frame(rstan::extract(fit,par1, permuted = TRUE))
  if(robust) pe = apply(post,2,median)
  else pe = apply(post,2,mean)
  return( as.list(pe) )
}
#' Get the fitted values of an arima model
#'
#' get the fitted values of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.arima(model,fit)
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
get_fit.arima = function(model,fit,robust = FALSE,...){
  qq = c(1-conf,conf)*100
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
#' Get the fitted values of an arima model
#'
#' get the fitted values of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.arima(model,fit)
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
get_residuals.arima = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
