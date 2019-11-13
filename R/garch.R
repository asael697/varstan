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
#'
#' @export
#'
is.garch = function(obj){
  y = FALSE
  if(class(obj) == "garch") y = TRUE
  return (y)
}
#' Excluded parameters in a  Garch model
#'
#' @export
#'
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
