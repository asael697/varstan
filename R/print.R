#' Print a Sarima model
#'
#' @param  obj a Sarima model from the varstan package
#'
#' @method print Sarima
#' @export
#'
print.Sarima = function(obj,...){
  if(!is.Sarima(obj))
    stop("The current object is not an arima model")
  report(obj)
}
#' Print a naive model
#'
#' @param  obj a navie model from the varstan package
#'
#' @method print naive
#' @export
#'
print.naive = function(obj,...){
  if(!is.naive(obj))
    stop("The current object is not a naive model")
  report(obj)
}
#' Print a garch model
#'
#' @param obj a garch model from the varstan package
#'
#' @method print garch
#' @export
#'
print.garch = function(obj,...){
  if(!is.garch(obj))
    stop("The current object is not a garch model")
  report(obj)
}
#' Print a Stochastic Volatility model
#'
#' @param obj a SVM model from the varstan package
#'
#' @method print SVM
#' @export
#'
print.SVM = function(obj,...){
  if(!is.SVM(obj))
    stop("The current object is not a SVM model")
  report(obj)
}
#' Print a varma model
#'
#' @param obj a varma model from the varstan package
#'
#' @method print varma
#' @export
#'
print.varma = function(obj,...){
  if(!is.varma(obj))
    stop("The current object is not a varma model")
  report(obj)
}
#' Print a Bekk model
#'
#' @param obj a Bekk model from the varstan package
#'
#' @method print Bekk
#' @export
#'
print.Bekk= function(obj,...){
  if(!is.Bekk(obj))
    stop("The current object is not a Bekk model")
  report(obj)
}
#' Print a varstan object
#'
#' @param obj a varstan object
#'
#' @method print varstan
#' @export
#'
print.varstan = function(obj,...){
  if(!is.varstan(obj))
    stop("The current object is not a varstan object")

  model(obj)
  print(summary(obj))
  cat("\n Samples were drawn using sampling(NUTS). For each parameter, ess")
  cat("\n is the effective sample size, and Rhat is the potential")
  cat("\n scale reduction factor on split chains (at convergence, Rhat = 1). \n")
}
