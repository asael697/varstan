##################################################################################
# Summary functions in varstan
##################################################################################

#' Declare the method
#'
#' @export
#'
residuals <- function(obj, ...) {
  UseMethod("residuals")
}

#' Get the residual values of the fitted model
#'
#' The function returns a matrix with the residual values
#'
#' @usage  get_residuals(obj)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
residuals.varstan = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
    if(is.arima(obj$model)) resd = get_residuals_arima(fit = obj$stanfit,robust)
    if(is.garch(obj$model)) resd = get_residuals_garch(fit = obj$stanfit,robust)
    if(is.varma(obj$model)) resd = get_residuals_varma(fit = obj$stanfit,robust)
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
get_residuals_varma = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d1,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
  return(sum1)
}
