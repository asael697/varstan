#' Print a Sarima model
#'
#' @param  obj a Sarima model from the varstan package
#'
#' @method print Sarima
#' @export
#'
print.Sarima = function(obj,...){
  if(is.Sarima(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not an arima model")
  }
}
#' Print a naive model
#'
#' @param  obj a navie model from the varstan package
#'
#' @method print naive
#' @export
#'
print.naive = function(obj,...){
  if(is.naive(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not an arima model")
  }
}
#' Print a garch model
#'
#' @param obj a garch model from the varstan package
#'
#' @method print garch
#' @export
#'
print.garch = function(obj,...){
  if(is.garch(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a garch model")
  }
}
#' Print a varma model
#'
#' @param obj a varma model from the varstan package
#'
#' @method print varma
#' @export
#'
print.varma = function(obj,...){
  if(is.varma(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a varma model")
  }
}
#' Print a Bekk model
#'
#' @param obj a Bekk model from the varstan package
#'
#' @method print Bekk
#' @export
#'
print.Bekk= function(obj,...){
  if(is.Bekk(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a Bekk model")
  }
}
#' Print a varstan object
#'
#' @param obj a varstan object
#'
#' @method print varstan
#' @export
#'
print.varstan = function(obj,...){
  if(is.varstan(obj)){
    model(obj)
    print(summary(obj))
    cat("\n Samples were drawn using sampling(NUTS). For each parameter, ess")
    cat("\n is the effective sample size, and Rhat is the potential")
    cat("\n scale reduction factor on split chains (at convergence, Rhat = 1). \n")
  }
  else{
    print("The current object is not a varstan object")
  }
}
