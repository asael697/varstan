#' Bayesian version of auto.arima  function
#'
#' Returns the best \code{Sarima} model using a \code{bic} value, this
#' function is a bayesian adaptation of the auto.arima() function of the
#' forecast package. Used as a starting point of your bayesian analysis.
#'
#'
#' @usage auto.sarima(ts,xreg = NULL,chains=1,iter=4000,warmup=floor(iter/2),
#'                 adapt.delta = 0.9,tree.depth =10,stepwise = TRUE,...)
#'
#' @param ts an univariate time series
#' @param xreg	Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param chains An integer of the number of Markov Chains chains to be run,
#' by default 1 chains is run
#' @param iter An integer of total iterations per chain including the warm-up,
#' by default  the number of iterations are 2000
#' @param warmup  A positive integer specifying number of warm-up (aka burn-in)
#'   iterations. This also specifies the number of iterations used for stepsize
#'   adaptation, so warmup samples should not be used for inference. The number
#'   of warmup should not be larger than \code{iter} and the default is
#'   \code{iter/2}
#' @param adapt.delta An optional real value between 0 and 1, the thin of the jumps
#' in a HMC method. By default is 0.9
#' @param  tree.depth An integer of the maximum  depth of the trees  evaluated
#' during each iteration. By default is 10
#' @param stepwise	If TRUE, will do stepwise selection (faster). Otherwise, it searches
#' over all models. Non-stepwise selection can be very slow, especially for seasonal models.
#'
#' @details
#' Bayesian version of the automatic arima model fitting proposed by
#' Robj Hyndman, this function finds the best Sarima model using \code{bic},
#' and then proceeds to fit the model using \code{varstan} function and the default
#' priors of a \code{Sarima} model constructor
#'
#' This function provides an initial model fit for beginning the bayesian analysis
#' of the univariate time series. For better fit and model selection try different
#' models and other moodel selection criteria such as \code{loo} or \code{bayes_factor}
#'
#' The default arguments are designed for rapid estimation of models for many time series.
#' If you are analysing just one time series, and can afford to take some more time,
#' it is recommended that you set \code{stepwise}=\code{FALSE} and reduce the number of iterations
#' per chain (\code{iter})
#'
#' For more information look at auto.arima function of forecast package.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @importFrom forecast auto.arima
#' @export
#'
#' @return  a varstan model
#'
#' @seealso \code{Sarima} \code{varstan}.
#'
#' @references
#'  Hyndman and Khandakar, Automatic Time Series Forecasting: The forecast
#'  package for R.
#'
#'  Hyndman Athanasopoulos, Forecasting: Principles and Practice
#'
#' @examples
#' \dontrun{
#' # Automatic Sarima model for the birth data
#' auto.sarima(birth)
#'
#' # Dyanimc Harmonic regression
#' auto.sarima(birth,xreg = fourier(birth,K= 6))
#'}
#'
auto.sarima = function(ts,xreg= NULL,
                       chains = 1,
                       iter = 4000,
                       warmup = floor(iter/2),
                       adapt.delta = 0.9,
                       tree.depth = 10,
                       stepwise = TRUE,...){
  y  = auto.arima(y = ts,xreg = xreg,ic = "bic",stepwise = stepwise,...)
  arma = y$arma
  ord = c(arma[1],arma[6],arma[2])
  season = c(arma[3],arma[7],arma[4])
  dat = Sarima(ts,order = ord,
               seasonal = season,
               period = arma[5],
               xreg = xreg)
  sf1 = varstan(model = dat,
                chains = chains,
                iter = iter,
                warmup = warmup,
                adapt.delta = adapt.delta,
                tree.depth = tree.depth)
  return(sf1)
}
#' Fourier terms for modelling seasonality
#'
#' \code{fourier} returns a matrix containing terms from a Fourier series, up
#' to order \code{K}, suitable for use in \code{\link{Sarima}} or
#' \code{\link{auto.sarima}}.
#'
#'
#' The period of the Fourier terms is determined from the time series
#' characteristics of \code{x}. When \code{h} is missing, the length of
#' \code{x} also determines the number of rows for the matrix returned by
#' \code{fourier}. Otherwise, the value of \code{h} determines the number of
#' rows for the matrix returned by \code{fourier}, typically used for
#' forecasting. The values within \code{x} are not used.
#'
#' Typical use would omit \code{h} when generating Fourier terms fitting a model
#' and include \code{h} when generating Fourier terms for forecasting.
#'
#' When \code{x} is a \code{ts} object, the value of \code{K} should be an
#' integer and specifies the number of sine and cosine terms to return. Thus,
#' the matrix returned has \code{2*K} columns.
#'
#' When \code{x} is a \code{msts} object, then \code{K} should be a vector of
#' integers specifying the number of sine and cosine terms for each of the
#' seasonal periods. Then the matrix returned will have \code{2*sum(K)}
#' columns.
#'
#' @param x Seasonal time series: a \code{ts} or a \code{msts} object
#' @param K Maximum order(s) of Fourier terms
#' @param h Number of periods ahead to forecast (optional)
#'
#' @return Numerical matrix.
#'
#' @author Rob J Hyndman
#'
#' @seealso \code{\link{seasonaldummy}}
#'
#' @keywords forecast
#'
#' @examples
#'
#' library(ggplot2)
#' \dontrun{
#' # Using Fourier series for a "ts" object
#' # K is chosen to minimize the AICc
#' deaths.model  <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths,K=5), seasonal=FALSE)
#' deaths.fcast <- forecast(deaths.model, xreg=fourier(USAccDeaths, K=5, h=36))
#' autoplot(deaths.fcast) + xlab("Year")
#'
#' # Using Fourier series for a "msts" object
#' taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
#' taylor.fcast <- forecast(taylor.lm,
#'     data.frame(fourier(taylor, K = c(3, 3), h = 270)))
#' autoplot(taylor.fcast)
#' }
#' @importFrom forecast fourier
#' @export
#'
fourier <- function(x, K, h = NULL) {
  return(forecast::fourier(x = x,K = K,h = h))
}
