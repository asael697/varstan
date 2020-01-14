#' Generic function and method for extract the residual of a varstan object
#'
#' The function returns the posterior estimate of the residuals
#' of a varstan model, similar to the residual functions of other
#' packages.
#'
#' @usage  posterior_residuals(obj,robust = FALSE)
#'
#' @param obj: A varstan object, \code{\link[=varstan]{varstan}}
#' @param robust: A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' and if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  An array with the posterior estimate of the residuals of the time series model,
#' if a varma model is used, then the return will be a matrix with columns as the dimension of
#' the time series, and rows as the length
#'
posterior_residuals <- function(obj,...) {
  UseMethod("posterior_residuals")
}
#'
#' @method posterior_residuals varstan
#' @export
#'
posterior_residuals.varstan = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
    if(is.Sarima(obj$model)) resd = get_residuals_arima(fit = obj$stanfit,robust)
    if(is.garch(obj$model))  resd = get_residuals_garch(fit = obj$stanfit,robust)
    if(is.varma(obj$model))  resd = get_residuals_varma(fit = obj$stanfit,d = obj$model$d,robust)
  }
  else{
    resd = NULL
    print("The current object is not a varstan object")
  }
  return(resd)
}

##################################################################################
# Internals
##################################################################################


#' Get the residuals of a garch model
#'
#' get the residuals of a garch(s,k,h) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals_garch = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
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
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals_arima = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
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
#' @usage  get_residuals.varma(fit)
#'
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals_varma = function(fit,d = 1,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow =d,byrow = TRUE))
  return(sum1)
}
