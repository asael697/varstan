#' Out-of-sample predictive errors
#'
#' This is a convenience function for computing \eqn{y - y_{h}}{y - yh}
#' The method for stanreg objects  calls \code{\link{posterior_predict}}
#' internally, where as the method accepts the data.frame returned by
#' \code{posterior_predict} as input and can be used to avoid multiple calls to
#' \code{posterior_predict}.
#'
#' @aliases predictive_error
#'
#' @param object Either a fitted model object returned by one of the \pkg{rstanarm}
#' modeling functions (a stanreg object) or, for the \code{"ppd"} method, a matrix
#' of draws from the posterior predictive distribution returned by \code{\link{posterior_predict}}.
#' @param xreg Optional, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param newdata An array with the newdata vector.
#' @param draws,seed Optional arguments passed to \code{\link{posterior_predict}}.
#' Please see the \strong{Note} section below if \code{newdata} will be specified.
#' @param ... Further arguments passed to  \code{predictive_error}.
#'
#'
#' @return
#' A \code{draws} by \code{nrow(newdata)} data.frame.
#'
#' @note
#' If \code{object} is a \strong{varstan} object of a varma model then newdata has to be a matrix
#' with number of \strong{cols} as the dimension of the time series and number of \strong{rows}
#' as the number new elements.
#'
#' @note If \code{object} is a \code{posterior_predict} data.frame, then the
#' length of \code{newdata} has to be equal to the \code{ncol} of \code{object}.
#'
#' @note If \code{object} is a \code{posterior_predict} data.frame, for a \strong{varma} model,
#' then the dimension product of \code{newdata} matrix has to be equal to
#' the \code{ncol} of \code{object}.
#'
#' @seealso \code{posterior_predict} function from rstanarm package, to draw
#'   from the posterior predictive distribution without computing predictive
#'   errors.
#'
#' @importFrom rstantools predictive_error
#' @method predictive_error varstan
#' @export
#' @export predictive_error
#'
predictive_error.varstan = function(object,newdata,xreg = NULL,draws = 1000,
                                    seed = NULL,...){

  if( !is.varstan(object) & !is.data.frame(object) )
     stop("The current object is not a varstan class or a data.frame containing
          the posterior_predict returns")

  if(is.null(newdata))
    stop("There is no data to estimate the predictive errors")

  if (is.matrix(newdata))
    nd = as.vector(matrix(newdata,nrow = 1))
  else
    nd = newdata

  if(is.varstan(object))
    yh = posterior_predict(object = object,h = length(nd),xreg = xreg,draws = draws,seed = seed)

  else yh = object

  if(length(nd) != ncol(yh))
    stop("The newdata structute dont match withe the posterior_predict data.frame")

  return( sweep(-1 *yh, MARGIN = 2, STATS = nd, FUN = "+") )
}
