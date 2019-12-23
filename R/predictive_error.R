#' Out-of-sample predictive errors
#'
#' This is a convenience function for computing \eqn{y - y_{h}}{y - yh}
#' The method for stanreg objects  calls \code{\link{posterior_predict}}
#' internally, where as the method accepts the data.frame returned by
#' \code{posterior_predict} as input and can be used to avoid multiple calls to
#' \code{posterior_predict}.
#'
#' @export
#'
#' @param obj Either a fitted model object returned by one of the
#'   \pkg{rstanarm} modeling functions (a \link[=stanreg-objects]{stanreg
#'   object}) or, for the \code{"ppd"} method, a matrix of draws from the
#'   posterior predictive distribution returned by
#'   \code{\link{posterior_predict}}.
#' @param newdata An array with the newdata vector
#' @param draws,seed,offset,re.form Optional arguments passed to
#'   \code{\link{posterior_predict}}. For binomial models, please see the
#'   \strong{Note} section below if \code{newdata} will be specified.
#'
#' @return A \code{draws} by \code{nrow(newdata)} data.frame.
#'
#' @note If \code{obj} is a \strong{varstan} object of a varma model then newdata has to be a matrix
#' with number of \strong{cols} as the dimension of the time serie and number of \strong{rows}
#' as the number new elements.
#'
#' @note If \code{obj} is a \code{posterior_predict} data.frame, then the
#' length of \code{newdata} has to be equal to the \code{ncol} of \code{obj}.
#'
#' @note If \code{obj} is a \code{posterior_predict} data.frame, for a \strong{varma} model,
#' then the dimension product of \code{newdata} matrix has to be equal to
#' the \code{ncol} of \code{obj}.
#'
#' @seealso \code{\link[=posterior_predict.stanreg]{posterior_predict}} to draw
#'   from the posterior predictive distribution without computing predictive
#'   errors.
#'
predictive_error = function(obj,...){
  UseMethod("predictive_error")
}
#'
#' @method predictive_error varstan
#' @export
#'
predictive_error.varstan = function(obj,newdata,draws = 1000,seed = NULL){

  if( !is.varstan(obj) & !is.data.frame(obj) )
     stop("The current object is not a varstan class or a data.frame containing
          the posterior_predict returns")

  if(is.null(newdata))
    stop("There is no data to estimate the predictive errors")

  if (is.matrix(newdata))
    nd = as.vector(matrix(newdata,nrow = 1))
  else
    nd = newdata

  if(is.varstan(obj))
    yh = posterior_predict(obj = obj,h = length(nd),draws = draws,seed = seed)

  else yh = obj

  if(length(nd) != ncol(yh))
    stop("The newdata structute dont match withe the posterior_predict data.frame")

  return( sweep(-1 *yh, MARGIN = 2, STATS = nd, FUN = "+") )
}





