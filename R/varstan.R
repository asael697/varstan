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
    if(is.arima(model)) sft = fit_arima(model,chains,iter,warmup,adapt.delta)
    if(is.garch(model)) sft = fit_garch(model,chains,iter,warmup,adapt.delta)
    if(is.varma(model)) sft = fit_varma(model,chains,iter,warmup,adapt.delta)

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
#' Print a varstan object
#' @export
#'
print.varstan = function(obj){
  if(is.varstan(obj)){
    print(summary_varstan(obj))
  }
  else{
    print("The current object is not a varstan object")
  }
}

#' Get the degree freedom values of a varma model
#'
#' get the degree freedom values of a varma(p,q) model  in STAN
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
get_df = function(obj,robust = FALSE,...){
  if(is.varstan(obj) ){
   if(obj$model$genT == TRUE) resd = get_df_varma(model = obj$model,fit = obj$stanfit,robust)
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
