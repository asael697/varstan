#' Random Multivariate time series Data.
#'
#' A dataset containing randomly generated multivariate(d = 2) time series data
#' that follows a var(1)-Bekk(1,1)-m(1) model as follows. the VAR process is a 0.1 matrix,
#' the ARCH process has \code{alpha1 = 0.1*I}  and \code{alpha1 = 0.3*I}, the GARCH process
#' has a \code{beta1 = 0.2*I} and the M-GARCH has a \code{H = 0.15} matrix. Where I is the
#' identity matrix.
#'
#' @docType data
#'
#' @format A matrix with of 100 rows and 2 columns
#' \describe{
#'  \item{Y1}{First time series}
#'  \item{Y2}{Second time series}
#' }
#' @source Randomly generated data
#'
"Astrovan"
