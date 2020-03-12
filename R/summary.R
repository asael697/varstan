#' Summary method for a varstan object
#'
#' Summaries of parameter estimates and MCMC convergence diagnostics
#' (Monte Carlo error, effective sample size, Rhat).
#'
#' @usage  summary(obj,robust = FALSE,prob = 0.95,...)
#'
#' @aliases summary summary.varstan
#'
#' @param obj a varstan object
#' @param robust A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' And if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#' @param prob A number \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#'   probability mass to include in the intervals. The default is to report
#'   \eqn{90}\% intervals (\code{prob=0.9}) rather than the traditionally used
#'   \eqn{95}\%.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  A data.frame with the posterior mean, standard error, credible intervals, effective sample
#' size (ess),and Rhat for all the model parameters in a varstan model, if \code{robust} is \code{TRUE}
#' then the posterior mean and standard error, are replaced by the posterior mean and MAD.
#'
#' @import rstan
#'
#' @method summary varstan
#' @export
#'
summary.varstan = function(obj,robust = FALSE,prob = 0.95,...){

  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  conf <- 1- ((1 - prob) / 2)
  qq = c(1-conf,conf)
  par1 = get_params(obj)$include
  post = as.data.frame(rstan::extract(obj$stanfit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))

  if(robust)
    names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess","Rhat")
  else
    names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess","Rhat" )

  return(sum)
}
