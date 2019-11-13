#' Declare the method
#'
#' @export
#'
fit_values<- function(obj, ...) {
  UseMethod("fit_values")
}
#' Get the fitted values of the fitted model
#'
#' The function returns a matrix with the fitted values
#'
#' @usage  point_estimate.varma(fit)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence (not for
#' varma models)
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
fit_values.varstan = function(obj,robust = FALSE,conf = 0.975,...){
  if(is.varstan(obj) ){
    if(is.arima(obj$model)) fit = get_fit_arima(model = obj$model,fit = obj$stanfit,robust)
    if(is.garch(obj$model)) fit = get_fit_garch(model = obj$model,fit = obj$stanfit,robust)
    if(is.varma(obj$model)) fit = get_fit_varma(model = obj$model,fit = obj$stanfit,robust)
  }
  else{
    fit = NULL
    print("The current object is not a varstan object")
  }
  return(fit)
}
#' Get the fitted values of an garch model
#'
#' get the fitted values of an garch(s,k,h) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: the arima model
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit_garch = function(model,fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
#' Get the fitted values of an arima model
#'
#' get the fitted values of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.arima(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: the arima model
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit_arima = function(model,fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum = apply(post,2,mean)
  else sum = apply(post,2,median)
  return(sum)
}
#' Get the fitted values of an varma model
#'
#' get the fitted values of an varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.varma(fit)
#'
#' @param model: a varma model object
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit_varma = function(model,fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d1,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
  return(sum1)
}
