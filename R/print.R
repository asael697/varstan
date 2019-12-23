#' @method print arima
#' @export
#'
print.arima = function(obj){
  if(is.arima(obj)){
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
    print(summary(obj))
  }
  else{
    print("The current object is not a varstan object")
  }
}
