#' Constructor Seasonal arima model
#'
#' Constructor of the Sarima model  for bayesian estimation in STAN
#'
#' The function returns  a list with  all the neccesary data for running a
#' Seasonal arima model in stan
#'
#' @usage Sarima(ts,order = c(0,0,0),seasonal = c(0,0,0),xreg = NULL,period = 0)
#'
#' @param ts a univariate time series
#' @param order: A specification of the non-seasonal part of the ARIMA model: the
#' three components (p, d, q) are the AR order, the degree of differencing, and the
#' MA order.
#' @param seasonal: A specification of the seasonal part of the ARIMA model,same as
#' order parameter:  the three components (p, d, q) are the seasonal AR order,
#' the degree of seasonal differencing, and the seasonal MA order.
#' @param xreg:	Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param period: an integer specifying the periodicity of the time series by
#' deafault the value frequency(ts) is used.
#'
#' @details If \code{xreg} option is used, the model by default will cancel the
#' seasonal differences adjusted (D = 0). If a value \code{d} > 0 is used, all
#' the regressor variables in xreg will be differenced aswell
#'
#' The default priors used in Sarima are:
#'
#' \itemize{
#'  \item{ar ~ normal(0,0.5)}
#'  \item{ma ~ normal(0,0.5)}
#'  \item{mu0 ~ t-student(0,2.5,6)}
#'  \item{{sigma0 ~ t-student(0,1,7)}
#'  \item{sar ~ normal(0,0.5)}
#'  \item{sma ~ normal(0,0.5)}
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
#'  Box, George; Jenkins, Gwilym (1970).
#'  Time Series Analysis: Forecasting and Control. San Francisco: Holden-Day.
#'
#' @seealso \code{\link{garch}} \code{\link{auto.arima}} \code{\link{set_prior}}
#'
#' @examples
#' # Declare a seasonal arima model for the birth data of asta package
#'
#' model = Sarima(bth,order = c(0,1,2),seasonal = c(1,1,1))
#' model
#'
#'
Sarima = function(ts,order = c(1,0,0),seasonal = c(0,0,0),xreg = NULL,period = 0){
  n = length(as.numeric(ts))
  y = as.numeric(ts)
  m1 = list(n = n,
            p = no_negative_check(order[1]),
            d = no_negative_check(order[2]),
            q = no_negative_check(order[3]),
            yreal = as.numeric(ts),y = as.numeric(ts))

  m1$prior_mu0 = c(0,2.5,6,4)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_ar  = matrix(rep(c(0,0.5,1,1),m1$p),ncol = 4,byrow = TRUE)
  m1$prior_ma  = matrix(rep(c(0,0.5,1,1),m1$q),ncol = 4,byrow = TRUE)
  m1$n1 = m1$n

  if(period == 0) m1$period = frequency(ts)
  else m1$period = period

  m1$P = seasonal[1]
  m1$D = seasonal[2]
  m1$Q = seasonal[3]

  if(m1$period <= 1){
    m1$P=0
    m1$D=0
    m1$Q=0
  }

  m1$prior_sar = matrix(rep(c(0,0.5,1,1),m1$P),ncol = 4,byrow = TRUE)
  m1$prior_sma=  matrix(rep(c(0,0.5,1,1),m1$Q),ncol = 4,byrow = TRUE)

  # arima regression model
  if( !is.null(xreg) ){

    if(!is.matrix(xreg))
      stop("xreg has to be a matrix with row dimension as same as the length of the time serie")

    if(nrow(xreg) != n)
      stop("The length of xreg don't match with the length of the time serie")

    # seasonal adjustment
    if(m1$D > 0) {
      cat("seasonal difference is not allowed in dynamic regressions \n")
      m1$D = 0
    }

    m1$d1 = ncol(xreg)
    m1$xreg = xreg
    m1$prior_breg  = matrix(rep(c(0,2.5,6,4),m1$d1),ncol = 4,byrow = TRUE)

    if(m1$d > 0){
      m1$xreg = diff(m1$xreg,differences = m1$d)
      m1$xlast = matrix(,nrow = m1$d,ncol = m1$d1)
      m1$xlast[1,] = tail(xreg,n=1)
      if(m1$d > 1)
        for(i in 2:m1$d) m1$xlast[i,] = tail( diff(xreg,differences = i-1),n=1)
    }
  }
  else{
    m1$d1 = 0
    n2 = m1$n1-m1$d -(m1$period*m1$D)
    m1$xreg = matrix(rep(0,m1$d1*n2 ),ncol = m1$d1,nrow = n2)
    m1$prior_breg  = matrix(rep(c(0,2.5,6,4),m1$d1),ncol = 4,byrow = TRUE)
  }

  sc = dif(ts = as.numeric(ts),d = m1$d,D = m1$D,period = m1$period)
  m1$y = sc$y
  m1$n1 = length(m1$y)
  m1$init = sc$init
  m1$inits = sc$inits

  attr(m1,"class") = "Sarima"

  return(m1)
}
#' Checks if is a Sarima object
#'
#' @param obj: an arima object
#'
#'
is.Sarima = function(obj){
  y = FALSE
  if(is(obj,"Sarima")) y = TRUE
  return (y)
}
#' Extracts all the order coeffients in a list
#'
get_order_arima= function(dat){
  return(list(p = dat$p,d =dat$d,q=dat$q,
              P=dat$P,D = dat$D,Q=dat$Q,
              d1 = dat$d1,
              period = dat$period))

}
#' Max order  coeffients in an Sarima model
#'
max_order_arima= function(dat){
  return(max(c(dat$p,dat$q,dat$period*dat$P,dat$period*dat$Q)))
}
