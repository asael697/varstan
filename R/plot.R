#' plot methods for varstan models
#'
#' Preliminar plot methods for varstan models only valid for univariate time series models.
#' The function prints the fitted values time series, the trace and density plots for the
#' sampled model parameters, or the residuals' posterior mean time series.
#'
#' @param object An object of class \code{varstan}.
#' @param par a character string with the desired plot. Valid values are \code{"fit"} for the
#'  model's fitted values plot, \code{"residuals"} for the residuals posterior mean plot,
#'  and \code{"parameters"} for density and trace plots for the sampled model parameter.
#'  The default value is \code{"fit"}.
#' @param prob A number \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#'   probability mass to include in the intervals. The default is to report
#'   \eqn{90}\% intervals (\code{prob=0.9}) rather than the traditionally used
#'   \eqn{95}\%.
#' @param ... Further arguments passed to  \code{mcmc_combo}.
#' @param combo A character vector with at least two elements. Each element of combo corresponds
#' to a column in the resulting graphic and should be the name of one of the available MCMC functions.
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
plot.varstan = function(object,par = "fit",prob = 0.9,combo = c("dens","trace"),...){

  if( !is.varstan(object))
    stop("The current object is not a varstan class")

  if(par == "parameter"){
    if(object$dimension > 1)
      stop("Only valid for univariate time series")

    par_ret = mod_parameter(object$model)
    stanfit2 = as.stan(object)
    p = bayesplot::mcmc_combo(stanfit2,pars = par_ret,combo = combo)
  }
  else if(par == "fit"){
    p = plot_ts(obj = object,par = "fit",prob = prob,real = TRUE)
  }
  else if(par == "residuals"){
    p = plot_ts(obj = object,par = "residual",prob =  probs,real = FALSE)
  }
  else if(par %in% get_params(object)$include){
    md = data.frame(extract_stan(object,pars = par))
    p = bayesplot::mcmc_combo(x = md,combo = c("dens","trace"))
  }
  else{
    stop("par argument is not valid, please enter a models valid argument")
  }
  return(p)
}
#' autoplot methods for varstan models
#'
#' Preliminar autoplot methods for varstan models only valid for univariate time series models.
#' The function prints the fitted values time series, the trace and density plots for the
#' sampled model parameters, or the residuals' posterior mean time series.
#'
#' @param object An object of class \code{varstan}.
#' @param par a character string with the desired plot. Valid values are \code{"fit"} for the
#'  model's fitted values plot, \code{"residuals"} for the residuals posterior mean plot,
#'  and \code{"parameters"} for density and trace plots for the sampled model parameter.
#'  The default value is \code{"fit"}.
#' @param prob A number \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#'   probability mass to include in the intervals. The default is to report
#'   \eqn{90}\% intervals (\code{prob=0.9}) rather than the traditionally used
#'   \eqn{95}\%.
#' @param ... Further arguments passed to  \code{mcmc_combo}.
#' @param combo A character vector with at least two elements. Each element of combo corresponds
#' to a column in the resulting graphic and should be the name of one of the available MCMC functions.
#'
#' @return An autoplot object from ggplot2 class.
#'
#' @examples
#' \dontrun{
#'  sf1 = auto.sarima(ts = birth)
#'  # fitted model
#'  autoplot(sf1)
#'
#'  # residuals
#'  autoplot(sf1,type = "residuals)
#'
#'  # parameters
#'  autoplot(sf1,type = "parameter)
#' }
#'
#' @method autoplot varstan
#' @import ggplot2
#' @importFrom bayesplot mcmc_combo
#' @export
#'
autoplot.varstan = function(object,par = "fit",prob = 0.9,combo = c("dens","trace"),...){

  if( !is.varstan(object))
    stop("The current object is not a varstan class")

  if(par == "parameter"){
    if(object$dimension > 1)
      stop("Only valid for univariate time series")

    par_ret = mod_parameter(object$model)
    stanfit2 = as.stan(object)
    p = bayesplot::mcmc_combo(stanfit2,pars = par_ret,combo = c("dens","trace"))
  }
  else if(par == "fit"){
    p = plot_ts(obj = object,par = "fit",prob = prob,real = TRUE)
  }
  else if(par == "residuals"){
    p = plot_ts(obj = object,par = "residuals",prob =  prob,real = FALSE)
  }
  else if(par %in% get_params(object)$include){
    md = data.frame(extract_stan(object,pars = par))
    p = bayesplot::mcmc_combo(x = md,combo =combo)
  }
  else{
    stop("par argument is not valid, please enter a models valid argument")
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
  if(is.garch(model) | is.SVM(model) ){
    if(model$p > 0) par_ret = c(par_ret,paste0("phi[",1:model$p,"]"))
    if(model$q > 0) par_ret = c(par_ret,paste0("theta[",1:model$q,"]"))
    if(model$s > 0) par_ret = c(par_ret,paste0("alpha[",1:model$s,"]"))
    if(model$k > 0) par_ret = c(par_ret,paste0("beta[",1:model$k,"]"))
    if(model$h > 0) par_ret = c(par_ret,paste0("mgarch[",1:model$h,"]"))
    if(model$d1> 0) par_ret = c(par_ret,paste0("breg[",1:model$d1,"]"))
    if(model$genT > 0) par_ret = c(par_ret,"v")
  }
  return(par_ret)
}

#'  Internal function for plot fit and residuals
#' @import ggplot2
#' @noRd
#'
plot_ts = function(obj,par = "fit",prob = 0.90,real = FALSE){

  p1 = (1-prob)/2;s1 = obj$series.name;s = NULL;

  f2 = data.frame(extract_stan(obj = obj,pars = par))
  f2mean = apply(f2,2,mean)
  f21 = apply(f2,2,quantile, probs = p1)
  f22 = apply(f2,2,quantile, probs =1 - p1)
  for(i in s1)  s = c(s,rep(i,obj$model$n))
  time1 = rep(obj$time,obj$dimension)
  yreal = matrix(obj$ts,ncol = 1)

  dat_temp = data.frame(fit = f2mean,q1 = as.numeric(f21),q2 = as.numeric(f22),time = time1,type = s)

  if(real == TRUE){
    dat_temp$yreal = yreal
    colors <- c("fit" = "#0000CC", "yreal" = "#000000")

    g = ggplot2::ggplot(aes(x = time1,y = fit),data = dat_temp)+
      ggplot2::geom_line(aes(y = fit,color = "fit"))+
      ggplot2::geom_line(aes(y = yreal,color = "yreal"))+
      ggplot2::geom_smooth(aes(ymin = q1, ymax = q2),fill="#333333", color="#0000CC",
                           stat = "identity",size = 0.5)+
      ggplot2::labs(x = "time",y = " ",color = "Legend") +
      ggplot2::scale_color_manual(values = colors)
  }
  else{
    g = ggplot2::ggplot(aes(x = time1,y = fit),data = dat_temp)+
      ggplot2::geom_smooth(aes(ymin = q1, ymax = q2,),fill="#333333", color="#0000CC",
                           stat = "identity",size = 0.5) +ggplot2::labs(x = "time",y = " ")
  }
  if(obj$dimension > 1) g = g + ggplot2::facet_grid(type~.)


  return(g)
}


