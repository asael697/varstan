#' Constructor varstan) object
#'
#' Constructor of the varstan object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage varstan(model)
#'
#' @param model A time series object for the varstan models
#' @param chains the number of chains to be run
#' @param iter the number of iteration per chain
#' @param warmup the number of initial iteration to be burned
#' @param adapt.delta the thin of the jumps in a HMC method
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a list with the components
#' \itemize{
#'  \item stanfit: An stanfit object returned by rstan
#'  \item model: the current fitted model
#'  \item stanparams: The stan parameters used in the HMC NUTS algorithm
#' }
#'
varstan = function(model,chains=4,iter=2000,warmup=floor(iter/2),adapt.delta = 0.90,...){
  if(is.model(model)){
    m = list()
<<<<<<< HEAD
    sft = fit(model,chains,iter,warmup,adapt.delta)
=======
    if(is.arima(model)) sft = fit_arima(model,chains,iter,warmup,adapt.delta)
    if(is.garch(model)) sft = fit_garch(model,chains,iter,warmup,adapt.delta)
    if(is.varma(model)) sft = fit_varma(model,chains,iter,warmup,adapt.delta)

>>>>>>> s3 methods corrected
    sp = list(Algorithm = "HMC NUTS",chains = chains,iter = iter,warmup = warmup,adapt.delta =adapt.delta)
    m = list(stanfit = sft,model = model,stan_parmas = sp)
    attr(m,"class") = "varstan"
  }else{
    m = NULL
    cat(model,"is not a available current model in varstan")
  }
  return(m)
}
#' Checks if is a varstan object
#'
#' @method is varstan
#'
#' @param obj: a varstan object
#'
is.varstan = function(obj){
  y = FALSE
  if(class(obj) == "varstan") y = TRUE
  return (y)
}
<<<<<<< HEAD
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
#' @export
#'
#' @return  a list with the components
#' \itemize{
#'  \item summary with all the important fitted parameters
#' }
#'
summary.varstan = function(obj,robust = FALSE,conf = 0.975,...){
 if(is.varstan(obj)){
   resume = summary(model=obj$model,fit = obj$stanfit,robust,conf)
 }
  else{
    resume = NULL
   print("The current object is not a varstan object")
  }
  return(resume)
}
#' Point estimate of  a varstan object
#'
#' point estimates of the model  in a varstan object
#'
#' @usage  point_estimate(obj)
#'
#' @param obj: a varstan object
#' @param robust: A boolean value, if its true it returns the posterior median
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a list with the components
#' \itemize{
#'  \item A matrix with the posterior value of the var coefficients
#'  \item A matrix with the posterior value of the scale parameter
#'  \item A vector with the posterior value of the degree freedom
#'  \item A time series with the posterior value of the correction parameter
#'  for the variance
#' }
#'
point_estimate.varstan = function(obj,robust = FALSE,...){
if(is.varstan(obj)){
  resume = point_estimate(model = obj$model,fit = obj$stanfit,roubst = robust)
}
else{
  resume = NULL
  print("The current object is not a varstan object")
}
return(resume)
}
=======
>>>>>>> s3 methods corrected
#' Print a varstan object
#' @export
#'
print.varstan = function(obj){
  if(is.varstan(obj)){
    print(summary.varstan(obj))
  }
  else{
    print("The current object is not a varstan object")
  }
}
<<<<<<< HEAD
#' Get the fitted values of the fitted model
#'
#' The function returns a matrix with the fitted values
#'
#' @usage  point_estimate.varma(fit)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence (not for
#' varma models)
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit.vastan = function(obj,robust = FALSE,conf = 0.975,...){
  if(is.varstan(obj) ){
    fit = get_fit(model = obj$model,fit = obj$stanfit,robust,conf)
  }
  else{
    chains = NULL
    print("The current object is not a varstan object")
  }
  return(chains)
}
#' Get the residual values of the fitted model
#'
#' The function returns a matrix with the residual values
#'
#' @usage  get_residuals(obj)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals.vastan = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
    resd = get_residuals(fit = obj$stanfit,robust)
  }
  else{
    resd = NULL
    print("The current object is not a varstan object")
  }
  return(resd)
}
#' Get the degree freedom values of a varma model
#'
#' get the degree freedom values of a varma(p,q) model  in STAN
=======
#' Get the degree freedom values of a Generalized t-student varma model
#'
#' get the degree freedom values of a Generalized t-student varma(p,q) model
>>>>>>> s3 methods corrected
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_df.varma(fit)
#'
#' @param obj: a varstan object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
<<<<<<< HEAD
get_df.varstan = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
   if(obj$model$genT == TRUE) resd = get_df(model = obj$model,fit = obj$stanfit,robust)
=======
get_df = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
   if(obj$model$genT == TRUE) resd = get_df_varma(model = obj$model,fit = obj$stanfit,robust)
>>>>>>> s3 methods corrected
   else print("The current model is not a Generalized t-student varma model")
  }
  else{
    resd = NULL
    print("The current object is not a varstan object")
  }
  return(resd)
}
#' Extract chains of an stanfit object implemented in rstan package
#'
#'
#' @usage  extract_stan(obj)
#'
#' @param obj: a varstan object
#' @param pars: n optional character vector providing the parameter names
#' (or other quantity names) of interest. If not specified, all parameters
#' and other quantities are used. The log-posterior with name lp__ is also
#' included by default.
#' @param permuted: A logical scalar indicating whether the draws
#' after the warmup period in each chain should be permuted and merged.
#' If FALSE, the original order is kept. For each stanfit object,
#' the permutation is fixed (i.e., extracting samples a second time will
#' give the same sequence of iterations).
#' @param inc_warmup: A logical scalar indicating whether to include the
#' warmup draws. This argument is only relevant if permuted is FALSE.
#' @param include:A logical scalar indicating whether the parameters named
#' in pars should be included (TRUE) or excluded (FALSE).
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
extract_stan = function(obj,pars,permuted = TRUE, inc_warmup = FALSE,include = TRUE){
  if(is.varstan(obj) ){
    chains = rstan::extract(obj$stanfit,permuted,inc_warmup,include)
  }
  else{
    chains = NULL
    print("The current object is not a varstan object")
  }
  return(chains)
}
#' Extract a stanfit object implemented in rstan package
#'
#' @usage  get_rstan(obj)
#'
#' @param obj: a varstan object
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a stanfit object
#'
#' @export
#'
get_rstan = function(obj){
  if(is.varstan(obj) ){
    stanfit = obj$stanfit
  }
  else{
    stanfit = NULL
    print("The current object is not a varstan object")
  }
  return(stanfit)
}
