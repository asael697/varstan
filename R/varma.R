#' Constructor VARMA(p,q) object
#'
#' Constructor of the VARMA(p,q) model for Bayesian estimation in\pkg{Stan}.
#'
#' The function returns a list with the data for running \code{stan()} function of
#'  \pkg{rstan} package.
#'
#' @usage varma(ts,order = c(1,0),series.name = NULL)
#'
#' @param ts a matrix or a mts object with the multivariate time series.
#' @param order A specification of the  VARMA model,same as order parameter:
#' the two components (p, q) are the AR order,and the  MA order.
#' @param series.name an optional string vector with the time series names.
#'
#' @details
#' The default priors used in VARMA are:
#'
#' \itemize{
#'  \item{"ar"}{ar ~ normal(0,0.5)}
#'  \item{"ma"}{ma ~ normal(0,0.5)}
#'  \item{"mu0"}{mu0 ~ normal(0,1)}
#'  \item{"sigma0"}{sigma0 ~ t-student(0,1,7)}
#' }
#'
#' For changing the default prior use the\code{set_prior()} function.
#'
#' @author Asael Alonzo Matamoros.
#'
#' @export
#'
#' @references
#' Pfaff, B. (2008). VAR, SVAR and SVEC Models: Implementation Within R Package vars.
#' \emph{Journal of Statistical Software}. 27(4),  1--32.
#' \code{doi: 10.18637/jss.v027.i04}.
#'
#' Berg, T. O. (2016) Multivariate Forecasting with BVARs and DSGE Models.
#' \emph{J. Forecast}. 35(1), 718- 740.
#' \code{doi: 10.1002/for.2406}.
#'
#' Polasek, W. (2000). A Multivariate GARCH-M Model for Exchange Rates in the US,
#' Germany and Japan. \emph{Springer Berlin Heidelberg}. 355-363.
#' \code{doi: 10.1007/978-3-642-57280-7_39}.
#'
#' @seealso \code{\link{garch}} \code{\link{varma}} \code{\link{set_prior}}
#'
#' @examples
#' # Declares a varma model for the Astrovan data
#'
#' model = varma(Astrovan,order = c(1,1))
#' model
#'
varma = function(ts,order = c(1,0),series.name = NULL){

  m1 = Bekk(ts = ts,varma = order,order = c(0,0,0),genT = FALSE,series.name = series.name)
  attr(m1,"class") = "varma"
  return(m1)
}
#' Checks if is a varma object.
#'
#' @param obj a varma object.
#' @noRd
#'
is.varma = function(obj){
  y = FALSE
  if( is(obj,"varma")) y = TRUE
  return (y)
}
