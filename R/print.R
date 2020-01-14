#' @method print Sarima
#' @export
#'
print.Sarima = function(obj){
  if(is.Sarima(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not an arima model")
  }
}
#' @method print garch
#' @export
#'
print.garch = function(obj){
  if(is.garch(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a garch model")
  }
}
#' @method print varma
#' @export
#'
print.varma = function(obj){
  if(is.varma(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a varma model")
  }
}
#' @method print varstan
#' @export
#'
print.varstan = function(obj){
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
