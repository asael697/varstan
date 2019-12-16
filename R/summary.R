##################################################################################
# Summary functions in varstan
##################################################################################

#' @export
summary <- function(obj, ...) {
  UseMethod("summary")
}
#' Summary of  a varstan object
#'
#' Summary of the model estimates in a varstan object
#'
#'
#' @usage  summary(obj)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired probability in the
#' credible intervals
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a list with the components
#' \itemize{
#'  \item summary with all the important fitted parameters
#' }
#' @method summary varstan
#' @export summary
#' @export
#'
summary.varstan = function(obj,robust = FALSE,conf = 0.975,...){
  if(is.varstan(obj)){

    if(is.arima(obj$model)) resume = summary_arima(model=obj$model,fit = obj$stanfit,robust,conf)
    if(is.garch(obj$model)) resume = summary_garch(model=obj$model,fit = obj$stanfit,robust,conf)
    if(is.varma(obj$model)) resume = summary_varma(model=obj$model,fit = obj$stanfit,robust,conf)
  }
  else{
    resume = NULL
    print("The current object is not a varstan object")
  }
  return(resume)
}

##################################################################################
# Internals
##################################################################################

#' Summary of  a varma model
#'
#' Summary of an varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  summary.varma(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
summary_varma = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params_varma(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
#' Summary of  an arima model
#'
#' Summary of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  summary.garch(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence
#'
#' @return  a data frame with all the important fitted parameters
#'
summary_garch = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params_garch(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
#' Summary of  an arima model
#'
#' Summary of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  summary.arima(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired probability in the
#' credible intervals
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
summary_arima = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params_arima(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
