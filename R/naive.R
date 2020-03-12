#' naive and Random Walk Forecasts
#'
#' naive is the model constructor for a random walk  model applied to \code{y}.
#' This is equivalent to an ARIMA(0,1,0) model. \code{naive()} is simply a wrapper
#' to  mantain forecast package similitude. \code{seasonal} returns the model constructor
#' for a seasonal random walkequivalent to an ARIMA(0,0,0)(0,1,0)m model where m is the
#' seasonal period.
#'
#' The random walk with drift model is \deqn{Y_t=c + Y_{t-1} + Z_t}{Y[t]=c +
#' Y[t-1] + Z[t]} where \eqn{Z_t}{Z[t]} is a normal iid error. Forecasts are
#' given by \deqn{Y_n(h)=ch+Y_n}{Y[n+h]=ch+Y[n]}.
#'
#' The seasonal naive model is \deqn{Y_t= Y_{t-m} + Z_t}{Y[t]=Y[t-m] + Z[t]}
#' where \eqn{Z_t}{Z[t]} is a normal iid error.
#'
#' @aliases naive
#'
#' @usage  naive(ts,seasonal = FALSE,m = 0)
#'
#' @param ts a numeric vector or time series of class \code{ts}
#' @param seasonal a boolean value for select a seasonal random walk instead
#' @param m  an integer value
#'
#' @return An object of class naive.
#'
#' @author Asael ALonzo Matamoros
#'
#' @seealso \code{\link{Sarima}}
#'
#' @examples
#'
#' model = naive(birth,seasonal = TRUE)
#' model
#'
#' @export
#'
naive = function(ts,seasonal = FALSE,m = 0){
  if(seasonal == FALSE)
    dat = Sarima(ts = ts,order = c(0,1,0),xreg = NULL,period = 0)
  else
    dat = Sarima(ts = ts,order = c(0,0,0),seasonal = c(0,1,0),xreg = NULL,period = m)

  attr(dat,"class") = "naive"
  return(dat)
}
#' Checks if is a naive object
#'
#' @param obj a naive object
#' @noRd
#'
is.naive = function(obj){
  y = FALSE
  if( is(obj,"naive")) y = TRUE
  return (y)
}
