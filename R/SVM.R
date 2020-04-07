#' Constructor of an Stochastic volatility model object
#'
#' Constructor of the Sorhcastic Volatility model for Bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage SVM(ts,arma = c(1,0),xreg = NULL,series.name = NULL)
#'
#' @param ts an multivariate time series
#' @param arma A specification of the  ARMA model,same as order parameter:  the two
#' components (p, q) are the AR order,and the  MA order.
#' @param xreg	Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param series.name an optional string vector with the series names.
#'
#'
#' The default priors used in Bekk are:
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
#' For changing the default prior use the function \code{set_prior}
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @references
#'  Polasek, Ren(1999).
#'  A multivariate GARCH-M model for exchange rates in the US,Germany and Japan
#'
#'  Fonseca,Ferreira, Migon (2008)
#'  Objective Bayesian analysis for the Student-t regression model
#'
#' @seealso \code{\link{garch}} \code{\link{varma}} \code{\link{set_prior}}
#'
#' @examples
#' # Declares a varma model for the Astrovan data
#'
#' model = SVM(ipc,arma = c(1,1))
#' model
#'
#'
SVM = function(ts,arma = c(0,0),xreg = NULL,series.name = NULL){

  m1 = garch(ts = ts,order = c(1,1,1),arma = arma,xreg = xreg,
             genT = FALSE,series.name = series.name)

  m1$prior_alpha = m1$prior_mgarch;m1$prior_garch = NULL;
  m1$prior_beta = m1$prior_arch; m1$prior_arch = NULL
  m1$h = 0;
  attr(m1,"class") = "SVM"
  return(m1)
}
#' Checks if is a varma object
#'
#' @param obj a varma object
#' @noRd
#'
is.SVM = function(obj){
  y = FALSE
  if( is(obj,"SVM")) y = TRUE
  return (y)
}
