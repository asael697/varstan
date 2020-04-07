#' Print a Sarima model
#'
#' @param  x a Sarima model from the varstan package
#' @param ... additional values need in print methods
#'
#' @method print Sarima
#' @export
#'
print.Sarima = function(x,...){
  if(!is.Sarima(x))
    stop("The current object is not an arima model")
  report(x)
}
#' Print a naive model
#'
#' @param  x a navie model from the varstan package
#'
#' @method print naive
#' @export
#'
print.naive = function(x,...){
  if(!is.naive(x))
    stop("The current object is not a naive model")
  report(x)
}
#' Print a garch model
#'
#' @param x a garch model from the varstan package
#'
#' @method print garch
#' @export
#'
print.garch = function(x,...){
  if(!is.garch(x))
    stop("The current object is not a garch model")
  report(x)
}
#' Print a Stochastic Volatility model
#'
#' @param x a SVM model from the varstan package
#'
#' @method print SVM
#' @export
#'
print.SVM = function(x,...){
  if(!is.SVM(x))
    stop("The current object is not a SVM model")
  report(x)
}
#' Print a varma model
#'
#' @param x a varma model from the varstan package
#'
#' @method print varma
#' @export
#'
print.varma = function(x,...){
  if(!is.varma(x))
    stop("The current object is not a varma model")
  report(x)
}
#' Print a Bekk model
#'
#' @param x a Bekk model from the varstan package
#'
#' @method print Bekk
#' @export
#'
print.Bekk= function(x,...){
  if(!is.Bekk(x))
    stop("The current object is not a Bekk model")
  report(x)
}
#' Print a varstan object
#'
#' @param x a varstan object
#'
#' @method print varstan
#' @export
#'
print.varstan = function(x,...){
  if(!is.varstan(x))
    stop("The current object is not a varstan model")

  model(x)
  print(summary(x))
  cat("\n Samples were drawn using sampling(NUTS). For each parameter, ess")
  cat("\n is the effective sample size, and Rhat is the potential")
  cat("\n scale reduction factor on split chains (at convergence, Rhat = 1). \n")
}
