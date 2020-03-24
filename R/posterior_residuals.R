#' Generic function and method for extract the residual of a varstan object
#'
#' The function returns the posterior estimate of the residuals
#' of a varstan model, similar to the residual functions of other
#' packages.
#'
#' @usage  posterior_residuals(obj,robust = FALSE,...)
#'
#' @aliases posterior_residuals posterior_residuals-varstan
#'
#' @param obj A varstan object, \code{\link[=varstan]{varstan}}
#' @param robust A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' and if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#'
#' @details This function only extracts the pointwise estimate of the time series resiudals
#' for extracting all the data use extract_stan or posterior_intervals function
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
  if( !is.varstan(obj) )
    stop("The current object is not a varstan class")

  post = as.data.frame(extract_stan(obj,"residuals", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = obj$dimension,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow =obj$dimension,byrow = TRUE))

  resd =ts(sum1,start =  min(obj$time),frequency = obj$period)

  return(resd)
}
