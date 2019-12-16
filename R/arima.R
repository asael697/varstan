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
#'
mgarch = function(s=1,k=1,h=0){
  ml = list()
  ml$garch_order = c(no_negative_check(s),no_negative_check(k),no_negative_check(h))
  ml$prior_arch    = matrix(rep(c(3,3,1,2),s),ncol = 4,byrow = TRUE)
  ml$prior_garch   = matrix(rep(c(3,3,1,2),k),ncol = 4,byrow = TRUE)
  ml$prior_mgarch  = matrix(rep(c(3,3,1,2),h),ncol = 4,byrow = TRUE)
  ml$sd = "mgarch"
  return(ml)
}
#'
#' Excluded parameters in a arima model
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
    exclude = c("phi0","theta0")
    pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' @export
print <- function(obj, ...) {
  UseMethod("print")
}
#' @method print arima
#' @export print
#' @export
#'
print.arima = function(obj){
  if(is.arima(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not an arima model")
  }
}
