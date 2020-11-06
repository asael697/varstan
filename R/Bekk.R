#' Constructor VARMA(p,q)-Bekk(s,k) object.
#'
#' Constructor of the VARMA(p,q)-Bekk(s,k) object for Bayesian estimation in STAN.
#'
#' The function returns  a list with the data for running \code{stan()} function of
#'  \pkg{rstan} package
#'
#' @usage Bekk(ts,order = c(1,1,0),varma = c(0,0),genT = FALSE,series.name = NULL)
#'
#' @param ts an multivariate time series.
#' @param order A numeric vector for specify the Bekk  part for variance, same as
#' order parameter:  the three components (s, k, h) are the arch order,
#' the garch order and the garch for the mean (mgarch) order.
#' @param varma an optional numeric vector for specification of a VARMA model: the two
#' components (p, q) are the VAR order, the number of differences, and the VMA order.
#' @param genT a boolean value to specify a Generalized a t-student model Cruz (2015).
#' @param series.name an optional string vector with the time series names.
#'
#' @details
#' If a Bekk model is specified, then sigma0 represents the triangular inferior matrix
#' alpha0 of a Bekk model.
#'
#' The default priors used in VARMA(p,q)-MBekk(s,k,h) model are:
#'
#' \itemize{
#'  \item{"ar"}{ar ~ normal(0,0.5)}
#'  \item{"ma"}{ma ~ normal(0,0.5)}
#'  \item{"mu0"}{mu0 ~ normal(0,1)}
#'  \item{"sigma0"}{sigma0 ~ t-student(0,1,7)}
#'  \item{"arch"}{arch ~ normal(0,0.5)}
#'  \item{"garch"}{garch ~ normal(0,0.5)}
#'  \item{"mgarch"}{mgarch ~ normal(0,0.5)}
#'  \item{"dfv"}{dfv ~ gamma(2,0.1)}
#' }
#'
#' For changing the default prior use the \code{set_prior()} function.
#'
#' @author  Asael Alonzo Matamoros
#' @importFrom  stats as.ts
#' @export
#'
#' @references
#' Polasek, W. (2000). A Multivariate GARCH-M Model for Exchange Rates in the US,
#' Germany and Japan. \emph{Springer Berlin Heidelberg}. 355-363.
#' \code{doi: 10.1007/978-3-642-57280-7_39}
#'
#' Fioruci, J. and Ehlers, R. and Andrade, M. (2014). Bayesian multivariate GARCH
#' models with dynamic correlations and asymmetric error distributions.
#' \emph{Journal of Applied Statistics}. 41(2), 320 - 331.
#'
#' Berg, T. O. (2016) Multivariate Forecasting with BVARs and DSGE Models.
#' \emph{J. Forecast}. 35(1), 718- 740.
#' \code{doi: 10.1002/for.2406}.
#'
#' @seealso \code{\link{garch}} \code{\link{Sarima}} \code{\link{varma}}
#'
#' @examples
#' # Declare a Bekk model for the Astrovan data
#' model = Bekk(Astrovan,order = c(1,1,0),varma = c(1,0))
#' model
#'
#'
Bekk = function(ts,order = c(1,1,0),varma = c(0,0),genT = FALSE,series.name = NULL){
  n = dim(ts)[1]
  d = dim(ts)[2]
  y = matrix(ts,nrow = n)
  time = as.numeric(time(ts))
  yreal = stats::as.ts(ts)

  if(n < d){
    n = d
    d = dim(ts)[2]
    y = t(y)
    yreal = t( stats::as.ts(ts) )
    time = as.numeric(time(ts))
  }
  if(!is.null(series.name)){
    if(length(series.name) == d){
      sn = series.name
      colnames(yreal) = as.character(series.name)
    }
    else
      sn = colnames(yreal)
  }
  else sn = colnames(yreal)

  m1 = list(n = n,dimension = d,time = time,d = d,
            p = no_negative_check(varma[1]),
            q = no_negative_check(varma[2]),m = d*(d+1)/2,
            y = y,yreal = yreal,series.name = sn)
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_ar  = matrix(rep(c(0,1,1,1),varma[1]),ncol = 4,byrow = TRUE)
  m1$prior_ma  = matrix(rep(c(0,1,1,1),varma[2]),ncol = 4,byrow = TRUE)

  m1$s = no_negative_check(order[1])
  m1$k = no_negative_check(order[2])
  m1$h = no_negative_check(order[3])
  m1$prior_arch =  matrix(rep(c(0,1,1,1),m1$s),ncol = 4,byrow = TRUE)
  m1$prior_garch = matrix(rep(c(0,1,1,1),m1$k),ncol = 4,byrow = TRUE)
  m1$prior_mgarch =matrix(rep(c(0,1,1,1),m1$h),ncol = 4,byrow = TRUE)
  m1$prior_lkj = c(7,1,1,1)


  m1$genT = genT
  m1$prior_dfv = c(2,0.1,1,9)

  attr(m1,"class") = "Bekk"
  return(m1)
}
#' Checks if is a Bekk object
#'
#' @param object a Bekk object
#' @noRd
#'
is.Bekk = function(object){
  y = FALSE
  if(identical(class(object),"Bekk")) y = TRUE
  return (y)
}
#' Get the degree freedom values of a Bekk model
#'
#' get the degree freedom values of a Bekk(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_df_varma(fit,model,robust = FALSE,...)
#'
#' @param fit a stanfit object
#' @param model a model object
#' @param robust a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @importFrom stats median
#'
#' @return  a data frame with all the important fitted parameters
#'
#' @noRd
#'
get_df_varma = function(fit,model,robust = FALSE,...){
    post = as.data.frame(rstan::extract(fit,"lambda", permuted = TRUE) )
    # Dimension extraction
    if(is.garch(model)) d1 = 1
    else d1 = model$d
    # Estimation
    if(robust) sum1 = t(matrix(apply(post,2,stats::median),nrow = d1,byrow = TRUE))
    else sum1 = t(matrix(apply(post,2,mean),nrow = d1,byrow = TRUE))
    return(sum1)
}
#' Get the lag parameters of a varma model
#'
#' get the degree freedom values of a t-student varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_lag_varma(par,model,fit,robust,...)
#'
#' @param par the parameter to be extracted
#' @param fit a stanfit object
#' @param model a model object
#' @param robust a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @importFrom stats median
#'
#' @return  a data frame with all the important fitted parameters
#' @noRd
#'
get_lag_varma = function(par,fit,model,robust = FALSE){
  if(par %in% get_params_varma(model)$include ){
    post = as.data.frame(rstan::extract(fit,pars = par, permuted = TRUE) )
    if(robust) pe = apply(post,2,stats::median)
    else pe = apply(post,2,mean)
    return(pe)
  }
  else cat(par,"is not a fitted parameter")
}
#' Extracts all the order coefficients in a list
#'
#' @param dat a varma model
#' @noRd
#'
get_order_varma= function(dat){
  return(list(p = dat$p,q=dat$q,s=dat$s,k=dat$k,h=dat$h))

}
#' Max order  coefficients in a varma model
#'
#' @param dat a varma model
#' @noRd
#'
max_order_varma= function(dat){
  return(max(c(dat$p,dat$q,dat$s,dat$k,dat$h)))
}
