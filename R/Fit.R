#' Fit an arima model
#'
#' Fit a arima(p,d,q) model  in STAN
#'
#' The function returns a list with the fitted model in stan
#'
#' @usage  fit.arima(model,chains,iter,warmup,...)
#'
#' @param model A time series object for the varstan models
#' @param chains the number of chains to be run
#' @param iter the number of iteration per chain
#' @param warmup the number of initial iteration to be burned
#' @param adapt.delta the thin of the jumps in a HMC method
#'
#' @import rstan
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a stanfit object
#'
fit_arima = function(model,chains=4,iter=2000,warmup=floor(iter/2),adapt.delta = 0.90,...){
  if(is.arima(model)){
    stanfit = rstan::sampling(stanmodels$arima,
                              data = model,
                              chains = chains,
                              iter = iter,
                              warmup = warmup,
                              control = list(adapt_delta = adapt.delta),
                              par = get_params_arima(model)$exclude,
                              include = FALSE)
  }
  else{
    stanfit = NULL
    print("There is no accurate data to fit an arima model \n")
  }
  return(stanfit)
}
#' Fit a mgarch model
#'
#' Fit a garch(s,k,h) model  in STAN
#'
#' The function returns a list with the fitted model in stan
#'
#' @usage  fit.garch(model,chains,iter,warmup,...)
#'
#' @param model A time series object for the varstan models
#' @param chains the number of chains to be run
#' @param iter the number of iteration per chain
#' @param warmup the number of initial iteration to be burned
#' @param adapt.delta the thin of the jumps in a HMC method
#'
#' @import rstan
#'
#' @author  Asael Alonzo Matamoros
#'
#'
#' @return  a stanfit object
#'
fit_garch = function(model,chains=4,iter=2000,warmup=floor(iter/2),adapt.delta = 0.90,...){
  if(is.garch(model)){
    stanfit = rstan::sampling(stanmodels$garch,
                              data = model,
                              chains = chains,
                              iter = iter,
                              warmup = warmup,
                              control = list(adapt_delta = adapt.delta),
                              par = get_params_garch(model)$exclude,
                              include = FALSE)
  }
  else{
    stanfit = NULL
    print("There is no accurate data to fit an garch model \n")
  }
  return(stanfit)
}
#' Fit a  varma model
#'
#' Fit a var(p)-ma(q) model  in STAN
#'
#' The function returns a list with the fitted model in stan
#'
#' @usage  fit.varma(model)
#'
#' @param model A time series object for the varstan models
#' @param t.student  a boolean if the model has generalized t-student distribution
#' @param chains the number of chains to be run
#' @param iter the number of iteration per chain
#' @param warmup the number of initial iteration to be burned
#' @param adapt.delta the thin of the jumps in a HMC method
#'
#' @import rstan
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a stanfit object
#'
fit_varma = function(model,
                     chains=4,
                     iter=2000,
                     warmup=floor(iter/2),
                     adapt.delta = 0.90,...){
  if(is.varma(model)){

    pars = get_params_varma(model)$exclude

    if(model$genT == FALSE){

      stanfit = rstan::sampling(stanmodels$varma,
                                data = model,
                                chains = chains,
                                iter = iter,
                                warmup = warmup,
                                control = list(adapt_delta = adapt.delta),
                                par = pars,
                                include = FALSE)
    }
    else{

      stanfit = rstan::sampling(stanmodels$tvarma,
                                data = model,
                                chains = chains,
                                iter = iter,
                                warmup = warmup,
                                control = list(adapt_delta = adapt.delta),
                                par = pars,
                                include = FALSE)
    }
  }
  else{
    stanfit = NULL
    print("There is no accurate data to fit a varma model \n")
  }
  return(stanfit)
}
