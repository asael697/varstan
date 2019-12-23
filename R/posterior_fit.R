#' Generic function and method for extract the fitted values
#'
#' The function returns the posterior estimate of the fitted values
#' of a varstan model, similar to the fit_values functions of other
#' packages.
#'
#' @usage  posterior_fit(obj)
#'
#' @param obj: A varstan object, \code{\link[=varstan]{varstan}}
#' @param robust: A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' and if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  An array with the fitted values of the time series model, if a varma model is used, then
#' the return will be a matrix with columns as the dimension of the time series, and rows as the length
#'
posterior_fit<- function(obj, ...) {
  UseMethod("posterior_fit")
}
#'
#'
#' @method posterior_fit varstan
#' @export
#'
posterior_fit.varstan = function(obj,robust = FALSE,...){
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
