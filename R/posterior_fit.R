#' Generic function and method for extract the fitted values of a varstan object
#'
#' The function returns the posterior estimate of the fitted values
#' of a varstan model, similar to the fit_values functions of other
#' packages.
#'
#' @usage  posterior_fit(object, robust = FALSE,...)
#'
#' @param object A varstan object, \code{\link[=varstan]{varstan}}
#' @param robust A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' and if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#' @param ... Further arguments passed to  \code{posterior_fit}.
#'
#' @details
#' This function only extracts the pointwise estimate of the time series fitted values
#' for extracting all the data use \code{extract_stan()} or posterior_intervals function
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return
#' An array with the posterior estimate of the fitted values of the time series model,
#' if a varma model is used, then the return will be a matrix with columns as the dimension of
#' the time series, and rows as the length.
#'
#' @importFrom stats median ts
#' @export
#'
posterior_fit.varstan = function(object,robust = FALSE,...){
  if( !is.varstan(object) )
    stop("The current object is not a varstan class")

  post = as.data.frame(extract_stan(object,"fit", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,stats::median),nrow = object$dimension,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow =object$dimension,byrow = TRUE))

  fit = stats::ts(sum1,start =  min(object$time),frequency = object$period)

  return(fit)
}
#'
#' @rdname posterior_fit.varstan
#' @export
#'
posterior_fit<- function(object,robust = FALSE, ...) {
  UseMethod("posterior_fit")
}
