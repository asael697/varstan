#' Extract posterior sample of the pointwise log-likelihood from a varstan object
#'
#' Convenience function for extracting the pointwise log-likelihood
#' matrix or array from a fitted Stan model.
#'
#' @param obj A varstan object of the time series fitted model.
#' @param permuted A logical scalar indicating whether the draws after
#' the warmup period in each chain should be permuted and merged.
#' If FALSE, the original order is kept. For each stanfit object,
#' the permutation is fixed (i.e., extracting samples a second time
#' will give the same sequence of iterations).
#'
#' @return Usually, an S x N matrix containing the pointwise log-likelihood
#'  samples, where S is the number of samples and N is the number
#'  of observations in the data. If
#'  \code{permuted} is \code{FALSE}, an S x N x R array is returned,
#'  where R is the number of fitted chains.
#'
#' @aliases log_lik
#'
#'
#' @importFrom rstantools log_lik
#' @importFrom loo  extract_log_lik
#' @method log_lik varstan
#' @export
#' @export log_lik
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' log1 <- log_lik(fit1)
#' log1
#' }
#'
log_lik.varstan = function(obj,permuted = TRUE,...){
  if(!is.varstan(obj))
    stop("The current object is not varstan class")

  log_lik = loo::extract_log_lik(obj$stanfit,merge_chains = permuted)
  return(log_lik)
}
#' Extract posterior sample of the accumulated log-likelihood from a varstan object
#'
#' Convenience function for extracting the posterior sample of the accumulated
#' log-likelihood array from a fitted varstan object.
#'
#' @param obj A varstan object of the time series fitted model.
#' @param permuted A logical scalar indicating whether the draws after
#' the warmup period in each chain should be permuted and merged.
#' If FALSE, the original order is kept. For each stanfit object,
#' the permutation is fixed (i.e., extracting samples a second time
#' will give the same sequence of iterations).
#'
#' @return Usually, A nxS array containing the accumulated log-likelihood
#'  samples, where S is the number of samples. If \code{permuted} is \code{FALSE},
#'  an SxR array is returned, where R is the number of fitted chains.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' log1 <- loglik(fit1)
#' log1
#' }
#'
loglik = function(obj,permuted = TRUE){
  if(!is.varstan(obj))
    stop("The current object is not varstan class")

  loglik = data.frame(extract_stan(obj = obj,pars = "loglik",permuted = permuted))

  if(permuted)
    loglik = as.numeric(loglik$loglik)
  else{
    colnames(loglik) = paste0("loglik.",1:ncol(loglik))
    loglik = as.matrix(loglik)
  }
  return(loglik)
}
#' Leave-one-out cross-validation
#'
#' The \code{loo} method for varstan objects. Computes approximate
#' leave-one-out cross-validation using Pareto smoothed importance sampling
#' (PSIS-LOO CV).
#'
#' @aliases  loo
#'
#' @param obj A varstan object
#'
#' @seealso
#'  * The **loo** package [vignettes](https://mc-stan.org/loo/articles/index.html)
#'    for demonstrations.
#'  * [psis()] for the underlying Pareto Smoothed Importance Sampling (PSIS)
#'    procedure used in the LOO-CV approximation.
#'  * [pareto-k-diagnostic] for convenience functions for looking at diagnostics.
#'  * [loo_compare()] for model comparison.
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' loo1 <- loo(fit1)
#' loo1
#' }
#' @importFrom rstan loo
#' @method loo varstan
#' @export loo
#' @export
#'
#'
loo.varstan = function(obj,...){
  if (!is.varstan(obj))
    stop("The current object is not a varstan class")

  return(rstan::loo(obj$stanfit) )
}
#' Widely Applicable Information Criterion (WAIC)
#'
#' Compute the widely applicable information criterion (WAIC)
#' based on the posterior likelihood using the \pkg{loo} package.
#' For more details see \code{\link[loo:waic]{waic}}.
#'
#' @param obj: A varstan object
#'
#' @aliases  waic
#'
#' @details See the \code{loo_compare} function of the \pkg{loo} package
#' for more details on model comparisons.
#'
#' @return An object of class \code{loo}.
#'
#' @references
#' Vehtari, A., Gelman, A., & Gabry J. (2016). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC. In Statistics
#' and Computing, doi:10.1007/s11222-016-9696-4. arXiv preprint arXiv:1507.04544.
#'
#' Gelman, A., Hwang, J., & Vehtari, A. (2014).
#' Understanding predictive information criteria for Bayesian models.
#' Statistics and Computing, 24, 997-1016.
#'
#' Watanabe, S. (2010). Asymptotic equivalence of Bayes cross validation
#' and widely applicable information criterion in singular learning theory.
#' The Journal of Machine Learning Research, 11, 3571-3594.
#'
#' @importFrom loo waic
#' @method waic varstan
#' @export waic
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' waic1 <- waic(fit1)
#' waic1
#' }
#'
waic.varstan = function(obj,...){
  if (!is.varstan(obj))
    stop("The current object is not a varstan class")

  return(loo::waic(log_lik.varstan(obj)) )
}
#' Computes posterior sample of the pointwise AIC method from a varstan object
#'
#' Convenience function for computing the pointwise Akaike Information Criteria
#' method from a varstan object.
#'
#' @param obj A varstan object of the time series fitted model.
#'
#' @return A numeric array  of size R, containing the posterior samples of the AICc
#' for a varstan object, where R is the number of iterations. If multiple chains are
#' fitted, then the array is of length M*R, where M is the number of chains
#'
#' @export
#'
#' @author  Asael Alonzo Matamoros
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' aic1 <- aic(fit1)
#' mean(aic1)
#' }
#'
aic = function(obj){
  if (!is.varstan(obj))
    stop("The current object is not a varstan class")
  k = Total_order(obj)
  aic = 2*k - 2*loglik(obj)
  return(aic)
}
#' Computes posterior sample of the pointwise BIC method from a varstan object
#'
#' Convenience function for computing the pointwise Bayesian Information Criteria
#' method from a varstan object.
#'
#' @param obj A varstan object of the time series fitted model.
#'
#' @return A numeric array  of size R, containing the posterior samples of the aic
#' for a varstan object, where R is the number of iterations. If multiple chains are
#' fitted, then the array is of length M*R, where M is the number of chains
#'
#' @export
#'
#' @author  Asael Alonzo Matamoros
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' bic1 <- bic(fit1)
#' mean(bic1)
#' }
#'
bic = function(obj){
  if (!is.varstan(obj))
    stop("The current object is not a varstan class")
  k = Total_order(obj)
  n = obj$model$n
  bic = 2*k*log(n) - 2*loglik(obj)
  return(bic)
}
#' Computes posterior sample of the pointwise corrected AIC method from a varstan object
#'
#' Convenience function for computing the pointwise corrected  Akaike Information Criteria
#' method from a varstan object.
#'
#' @param obj A varstan object of the time series fitted model.
#'
#' @return A numeric array  of size R, containing the posterior samples of the AICc
#' for a varstan object, where R is the number of iterations. If multiple chains are
#' fitted, then the array is of length M*R, where m is the number of chains
#'
#' @export
#'
#' @author  Asael Alonzo Matamoros
#'
#' @examples
#'
#' \dontrun{
#' library(astsa)
#' model = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(model,chains = 1)
#'
#' aic1 <- aic(fit1)
#' mean(aic1)
#' }
#'
AICc = function(obj){
  if (!is.varstan(obj))
    stop("The current object is not a varstan class")
  k = Total_order(obj)
  n = obj$model$n
  m = 2*(k^2 +k )/(n-k-1)
  aicc = 2*k - 2*loglik(obj) +m
}
