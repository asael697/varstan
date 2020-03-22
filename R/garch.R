#' A  constructor for a garch(s,k,h) model
#'
#' Constructor of the garch(s,k,h) object for Bayesian estimation in STAN
#'
#' The function returns a list with the data for running stan() function of
#'  rstan package
#'
#' @usage garch(ts,order = (1,1,0),arma = c(0,0),genT = FALSE)
#'
#' @param ts an univariate time series
#' @param order A specification of the garch  model: the three components (s, k, h)
#' are the arch order, the garch order, and the mgarch order.
#' @param arma A specification of the  ARMA model,same as order parameter:  the two
#' components (p, q) are the AR order,and the  MA order.
#' @param xreg	Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param genT a boolean value to specify for a generalized t-student garch model
#' @param series.name an optional string vector with the series names.
#'
#' @details The default priors used in Sarima are:
#'
#' \itemize{
#'  \item{ar ~ normal(0,0.5)}
#'  \item{ma ~ normal(0,0.5)}
#'  \item{mu0 ~ t-student(0,2.5,6)}
#'  \item{sigma0 ~ t-student(0,1,7)}
#'  \item{arch ~ normal(0,0.5)}
#'  \item{garch ~ normal(0,0.5)}
#'  \item{mgarch ~ normal(0,0.5)}
#'  \item{dfv ~ gamma(2,0.1)}
#'  \item{breg ~ t-student(0,2.5,6)}
#' }
#'
#' For changing the default prior use the function \code{set_prior}
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @references
#'   Engle, Robert F. (1982). ?Autoregressive Conditional Heteroscedasticity with
#'   Estimates of the Variance of United Kingdom Inflation.
#'   Econometrica 50 (4): 987-1007. JSTOR 1912773.
#'
#' @seealso \code{\link{Sarima}} \code{\link{auto.arima}} \code{\link{set_prior}}
#'
#' @examples
#' \dontrun{
#' dat = garch(birth,order = c(1,1,0),arma = c(1,1))
#' dat
#' }
#'
garch = function(ts,order = c(1,1,0),arma = c(0,0),xreg = NULL,
                 genT = FALSE,series.name = NULL){

  n = length(as.numeric(ts))
  y = as.numeric(ts)

  # series name
  if(is.null(series.name))
    sn = deparse(substitute(ts))
  else
    sn = as.character(series.name)

  m1 = list(n = n,dimension = 1,time = as.numeric(time(ts)),
            s = no_negative_check(order[1] ),
            k = no_negative_check(order[2]),
            h = no_negative_check(order[3]),
            y = y,yreal = as.ts(ts),series.name = sn)

  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_arch    = matrix(rep(c(3,3,1,2),order[1]),ncol = 4,byrow = TRUE)
  m1$prior_garch   = matrix(rep(c(3,3,1,2),order[2]),ncol = 4,byrow = TRUE)
  m1$prior_mgarch  = matrix(rep(c(0,0.5,1,1),order[3]),ncol = 4,byrow = TRUE)

  # arma representation
  m1$p = arma[1]
  m1$q = arma[2]
  m1$prior_ar =  matrix(rep(c(3,3,1,2),arma[1]),ncol = 4,byrow = TRUE)
  m1$prior_ma =  matrix(rep(c(3,3,1,2),arma[2]),ncol = 4,byrow = TRUE)

  # Generalized t distribution
  m1$genT = genT
  m1$prior_dfv = c(2,0.1,1,9)

  # arima regression model
  if( !is.null(xreg) ){

    if(!is.matrix(xreg))
      stop("xreg has to be a matrix with row dimension as same as the length of the time serie")

    if(nrow(xreg) != n)
      stop("The length of xreg don't match with the length of the time serie")

    m1$d1 = ncol(xreg)
    m1$xreg = xreg
  }
  else{
    m1$d1 = 0
    m1$xreg = matrix(rep(0,m1$d1*n ),ncol = m1$d1,nrow = n)
  }

  m1$prior_breg  = matrix(rep(c(0,2.5,6,4),m1$d1),ncol = 4,byrow = TRUE)

  attr(m1,"class") = "garch"
  return(m1)
}
#' Checks if is a garch object
#'
#' @param obj a  garch object
#' @noRd
#'
is.garch = function(obj){
  y = FALSE
  if(is(obj,"garch")) y = TRUE
  return (y)
}
#' Extracts all the order coefficients in a list
#'
#' @param dat A garch model
#' @noRd
#'
get_order_garch= function(dat){
    return(list(p = dat$p,q=dat$q,d1 = dat$d1,s=dat$s,k=dat$k,h=dat$h))
}
#' Max order  coefficients in a garch model
#'
#' @param dat A garch model
#' @noRd
#'
max_order_garch= function(dat){

  return(max(c(dat$p,dat$q,dat$s,dat$k,dat$h)))
}
