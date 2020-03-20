#' plot methods for varstan models
#'
#' Preliminar plot methods for varstan models only valid for univariate time series models.
#' The function prints the fitted values time series, the trace and density plots for the
#' sampled model parameters, or the residuals' posterior mean time series.
#'
#' @param x An object of class \code{varstan}.
#' @param type a character string with the desired plot. Valid values are \code{"fit"} for the
#'  model's fitted values plot, \code{"residuals"} for the residuals posterior mean plot,
#'  and \code{"parameters"} for density and trace plots for the sampled model parameter.
#'  The default value is \code{"fit"}.
#' @param ... Further arguments passed to  \code{mcmc_combo}.
#'
#' @return A plot object from ggplot2 class.
#'
#' @examples
#' \dontrun{
#'  sf1 = auto.sarima(ts = birth)
#'  # fitted model
#'  plot(sf1)
#'
#'  # residuals
#'  plot(sf1,type = "residuals)
#'
#'  # parameters
#'  plot(sf1,type = "parameter)
#' }
#'
#' @method plot varstan
#' @import ggplot2
#' @importFrom bayesplot mcmc_combo
#' @export
#'
plot.varstan = function(object,type = "fit",...){

  if(object$model$dimension > 1)
    stop("Only valid for univariate time series")

  if( !(type %in% c("fit","parameter","residuals") ))
    stop("par argument is not valid, please enter a valid argument: parameter, residuals or fit")

  if(type == "parameter"){
    par_ret = mod_parameter(object$model)
    stanfit2 = as.stan(object)
    p = bayesplot::mcmc_combo(stanfit2,pars = par_ret,combo = c("dens","trace"))
  }
  else if(type == "fit"){
    dat_temp = data.frame(time =object$model$time,ts = object$model$yreal)
    preds <-  data.frame(extract_stan(obj = object,pars = "fit"))
    dat_temp$Estimate = colMeans(preds)
    dat_temp$Q5 = apply(preds, 2, quantile, probs = 0.05)
    dat_temp$Q95 = apply(preds, 2, quantile, probs = 0.95)
    p = ggplot2::ggplot(dat_temp, aes(x = time, y = Estimate)) +
      ggplot2::geom_smooth(aes(ymin = Q5, ymax = Q95), stat = "identity", size = 0.5) +
      ggplot2::geom_point(aes(y = ts)) + ggplot2::labs(x = "time",y = "series",title = "model fit")+
      ggplot2::theme_classic()
  }
  else if(type == "residuals"){
    dat_temp = data.frame(time =object$model$time,residuals = posterior_residuals(object))
    p = ggplot2::ggplot(dat_temp, aes(x = time, y = residuals)) +
      ggplot2::geom_line() + ggplot2::labs(x = "time",y = "residuals",title = "residuals posterior mean")+
      ggplot2::theme_classic()
  }
  return(p)
}
#'  Internal function for plot method
#' @noRd
#'
mod_parameter = function(model){
  par_ret = c("mu0","sigma0")
  if(is.Sarima(model)){
    if(model$p > 0) par_ret = c(par_ret,paste0("phi[",1:model$p,"]"))
    if(model$q > 0) par_ret = c(par_ret,paste0("theta[",1:model$q,"]"))
    if(model$P > 0) par_ret = c(par_ret,paste0("sphi[",1:model$P,"]"))
    if(model$Q > 0) par_ret = c(par_ret,paste0("stheta[",1:model$Q,"]"))
    if(model$d1> 0) par_ret = c(par_ret,paste0("breg[",1:model$d1,"]"))
  }
  if(is.garch(model) ){
    if(model$p > 0) par_ret = c(par_ret,paste0("phi[",1:model$p,"]"))
    if(model$q > 0) par_ret = c(par_ret,paste0("theta[",1:model$q,"]"))
    if(model$s > 0) par_ret = c(par_ret,paste0("alpha[",1:model$s,"]"))
    if(model$k > 0) par_ret = c(par_ret,paste0("beta[",1:model$k,"]"))
    if(model$h > 0) par_ret = c(par_ret,paste0("mgarch[",1:model$h,"]"))
    if(model$genT > 0) par_ret = c(par_ret,"v")
  }
  return(par_ret)
}
