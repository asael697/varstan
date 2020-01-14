#' A  constructor for a garch(s,k,h) model
#'
#' Constructor of the garch(s,k,h) object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage garch(ts,s,k)
#'
#' @param ts an univariateivariate time series
#' @param order: A specification of thegarch  model: thethree components (s, k, h)
#' are the arch order, the garch order, and the mgarch order.
#' @param arma: A specification of the  ARMA model,same as order parameter:  the two
#' components (p, q) are the AR order,and the  MA order.
#' @param genT a boolean value to specify for a generalized t-student garch model
#'
#'
#' The default priors used in Sarima are:
#'
#' \itemize{
#'  \item{"ar"}{ar ~ normal(0,0.5)}
#'  \item{"ma"}{ma ~ normal(0,0.5)}
#'  \item{"mu0"}{mu0 ~ t-student(0,2.5,6)}
#'  \item{"sigma0"}{sigma0 ~ t-student(0,1,7)}
#'  \item{"arch"}{arch ~ normal(0,0.5)}
#'  \item{"garch"}{garch ~ normal(0,0.5)}
#'  \item{"mgarch"}{mgarch ~ normal(0,0.5)}
#'  \item{"dfv"}{dfv ~ gamm(2,0.1)}
#' }
#'
#' @details  For changing the default prior use the function \code{set_prior}
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @references
#'   Engle, Robert F. (1982). ?Autoregressive Conditional Heteroscedasticity with
#'   Estimates of the Variance of United Kingdom Inflation?.
#'   Econometrica 50 (4): 987-1007. JSTOR 1912773.
#'
#' @seealso \code{\link{Sarima}} \code{\link{auto.arima}} \code{\link{set_prior}}
#'
#'
garch = function(ts,order = c(1,1,0),arma = c(0,0),genT = FALSE,aysm = FALSE){
  n = length(as.numeric(ts))
  y = as.numeric(ts)
  m1 = list(n = n,
            s = no_negative_check(order[1] ),
            k = no_negative_check(order[2]),
            h = no_negative_check(order[3]),
            y = y)
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_arch    = matrix(rep(c(3,3,1,2),order[1]),ncol = 4,byrow = TRUE)
  m1$prior_garch   = matrix(rep(c(3,3,1,2),order[2]),ncol = 4,byrow = TRUE)
  m1$prior_mgarch  = matrix(rep(c(3,3,1,2),order[3]),ncol = 4,byrow = TRUE)

  # arma representation
  m1$p = arma[1]
  m1$q = arma[2]
  m1$prior_ar =  matrix(rep(c(3,3,1,2),arma[1]),ncol = 4,byrow = TRUE)
  m1$prior_ma =  matrix(rep(c(3,3,1,2),arma[2]),ncol = 4,byrow = TRUE)

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
  if(is(obj,"garch")) y = TRUE
  return (y)
}
#' Extracts all the order coeffients in a list
#'
get_order_garch= function(dat){
    return(list(p = dat$p,q=dat$q,s=dat$s,k=dat$k,h=dat$h))
}
#' Max order  coeffients in a garch model
#'
max_order_garch= function(dat){

  return(max(c(dat$p,dat$q,dat$s,dat$k,dat$h)))
}
