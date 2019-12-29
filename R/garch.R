#' Constructor garch(s,k,h) object
#'
#' Constructor of the garch(s,k,h) object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage garch(ts,s,k)
#'
#' @param ts an univariateivariate time series
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
#'  \item s: an integer with the order of the arch coefficients
#'  \item k: an integer with the order of the garch coefficients
#'  \item h: an integer with the order of the mgarch coefficients
#'  \item y: vector with the multivariate time series
#'  \item prior_ar: a matrix with the hyper-parameters for the var coeff icient
#'  \item prior_ma: a matrix with the hyper-parameters for the ma  coefficients
#'  \item p: an integer with the order of ar coefficients
#'  \item q: an inter withe the order of ma coefficients
#' }
#'
garch = function(ts,s = 1,k = 1, h = 0,mean = arma(p=0,q=0),genT = FALSE,aysm = FALSE){
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
  # arma representation
  if( !is.null(mean) ){
    m1$p = mean$arma_order[1]
    m1$q = mean$arma_order[2]
    m1$prior_ar =  mean$prior_ar
    m1$prior_ma =  mean$prior_ma
    m1$mean = "arma"
  }
  else   m1$mean = "none"
  # Generalized t distribution
  if(genT == TRUE){
    m1$genT = TRUE
    m1$prior_dfv = c(2,0.1,1,9)
  }
  else m1$genT = FALSE
  attr(m1,"class") = "garch"
  return(m1)
}
#' Checks if is a garch object
#' @param obj: a  garch object
#'
#'
is.garch = function(obj){
  y = FALSE
  if(class(obj) == "garch") y = TRUE
  return (y)
}
#' Adds an arma(p,q) object to a garch model
#'
#' Adds an arma(p,q) object to a garch model
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage arma(p,q)
#'
#' @param p an integer with the order of the ar(p) part
#' @param q an integer with the order of the ma(q) part
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
arma = function(p=1,q=1){
  ml = list()
  ml$arma_order = c(no_negative_check(p),no_negative_check(q))
  ml$prior_ar   = matrix(rep(c(0,10,1,1),p),ncol = 4,byrow = TRUE)
  ml$prior_ma   = matrix(rep(c(0,10,1,1),q),ncol = 4,byrow = TRUE)
  ml$mean = "arma"
  return(ml)
}
#' Excluded parameters in a  Garch model
#'
#'
get_params_garch = function(dat,...){
  include = c("mu0","sigma0")
    if(dat$s > 0) include = c(include,"alpha")
    if(dat$k > 0) include = c(include,"beta")
    if(dat$h > 0) include = c(include,"mgarch")
    if(dat$p > 0) include = c(include,"phi")
    if(dat$q > 0) include = c(include,"theta")
    if(dat$genT == TRUE) include = c(include,"v")

    exclude = c("phi0","theta0")
    pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' Extracts all the order coeffients in a list
#'
get_order_garch= function(dat){
  if (is.garch(dat)){
    return(list(p = dat$p,q=dat$q,s=dat$s,k=dat$k,h=dat$h))
  }
  else print("The object is not a garch model")
}
#' Max order  coeffients in a garch model
#'
max_order_garch= function(dat){
  if (is.garch(dat)){
    return(max(c(dat$p,dat$q,dat$s,dat$k,dat$h)))
  }
  else print("The object is not a garch model")
}
