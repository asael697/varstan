#' Generic function and method for extract the residual of a varstan object
#'
#' The function returns the posterior estimate of the residuals
#' of a varstan model, similar to the residual functions of other
#' packages.
#'
#' @usage  posterior_residuals(object,robust = FALSE,...)
#'
#' @param object A varstan object, \code{\link[=varstan]{varstan}}
#' @param robust A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' and if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#' @param ... Further arguments passed to  \code{posterior_residual}.
#'
#' @details
#' This function only extracts the pointwise estimate of the time series resiudals
#' for extracting all the data use extract_stan or posterior_intervals function
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#' @importFrom stats median ts
#'
#' @return
#' An array with the posterior estimate of the residuals of the time series model,
#' if a varma model is used, then the return will be a matrix with columns as the
#' dimension of the time series, and rows as the length
#'
#' @method posterior_residuals varstan
#' @export
#'
posterior_residuals.varstan = function(object,robust = FALSE,...){
  if( !is.varstan(object) )
    stop("The current object is not a varstan class")

  post = as.data.frame(extract_stan(object,"residuals", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,stats::median),nrow = object$dimension,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow = object$dimension,byrow = TRUE))

  resd = stats::ts(sum1,start =  min(object$time),frequency = object$period)

  return(resd)
}
#'
#' @rdname posterior_residuals.varstan
#' @export
#'
posterior_residuals <- function(object,robust = FALSE,...) {
  UseMethod("posterior_residuals")
}
