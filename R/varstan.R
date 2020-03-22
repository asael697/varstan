#' Constructor varstan object
#'
#' Constructor of the varstan object for Bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage varstan(model,chains=4,iter=2000,warmup=floor(iter/2),
#' adapt.delta = 0.90,tree.depth =10,...)
#'
#' @param model One of the varstan model classes defined in the package
#' @param chains An integer of the number of Markov Chains chains to be run,
#' by default 4 chains are run
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
#'
#' @details This is the principal package's function and the link with Stan,
#' this function fits the posterior distribution of every
#'
#'   Default priors are chosen to be non or very weakly informative so that their
#'   influence on the results will. However, after getting more familiar with Bayesian
#'   statistics, I recommend you to start thinking about reasonable informative priors
#'   for your model parameters. For more information see \code{set_prior}
#'
#'   \bold{Adjusting the sampling behavior of \pkg{Stan}}
#'
#'   In addition to choosing the number of iterations, warmup samples, and
#'   chains, users can control the behavior of the NUTS sampler, by using the
#'   \code{control} argument. The most important reason to use \code{control} is
#'   to decrease (or eliminate at best) the number of divergent transitions that
#'   cause a bias in the obtained posterior samples. Whenever you see the
#'   warning "There were x divergent transitions after warmup." you should
#'   really think about increasing \code{adapt_delta}.  Increasing
#'   \code{adapt_delta} will slow down the sampler but will decrease the number
#'   of divergent transitions threatening the validity of your posterior
#'   samples.
#'
#'   Another problem arises when the depth of the tree being evaluated in each
#'   iteration is exceeded. This is less common than having divergent
#'   transitions, but may also bias the posterior samples. When it happens,
#'   \pkg{Stan} will throw out a warning suggesting to increase
#'   \code{max_treedepth}. For more details on the \code{control} argument see
#'   \code{\link[rstan:stan]{stan}}.
#'
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a list with the components
#' \itemize{
#'  \item stanfit An stanfit object returned by rstan
#'  \item model the current fitted model
#'  \item stanparams The stan parameters used in the HMC NUTS algorithm
#' }
#'
#' @seealso \code{\link[rstan:stan]{rstan:stan}}.
#'
#' @references
#'  Carpenter,Gelman,Hoffman, Lee, Goodrich, Betancourt, Brubaker, Guo, Li, and Riddell. 2017.
#'  Stan: A probabilistic programming language. Journal of Statistical Software 76(1).
#'  DOI 10.18637/jss.v076.i01
#'
#'  Stan Development Team. 2018.
#'  Stan Modeling Language Users Guide and Reference Manual, Version 2.18.0. http://mc-stan.org
#'
#' @examples
#' \dontrun{
#' # model with the treatment effect
#' library("astsa")
#' library("forecast")
#'
#' # Fitting a seasonal arima model
#' mod1 = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' fit1 = varstan(mod1,chains = 1)
#'
#' fit1
#'}
#'
varstan = function(model,chains=4,iter=2000,warmup=floor(iter/2),adapt.delta = 0.90,tree.depth =10,...){
  if(!is.model(model))
    stop(class(model),"is not an available current model in varstan")

  m = list()

  if(is.Sarima(model)) sft = fit_Sarima(model,chains,iter,warmup,adapt.delta,tree.depth)
  if(is.naive(model))  sft = fit_Sarima(model,chains,iter,warmup,adapt.delta,tree.depth)
  if(is.garch(model))  sft = fit_garch(model,chains,iter,warmup,adapt.delta,tree.depth)
  if(is.varma(model))  sft = fit_varma(model,chains,iter,warmup,adapt.delta,tree.depth)
  if(is.Bekk(model))   sft = fit_Bekk(model,chains,iter,warmup,adapt.delta,tree.depth)

  sp = list(Algorithm = "HMC NUTS",chains = chains,iter = iter,warmup = warmup,
            adapt.delta =adapt.delta,max_treedepth = tree.depth)
  m = list(stanfit = sft,model = model,stan_parmas = sp,
           time = model$time,
           period = frequency(model$yreal),
           dimension = model$dimension,
           ts = model$yreal)

  attr(m,"class") = "varstan"

  m$model.parameters = c(get_params(m)$include,"log_lik","fit","residuals")

  return(m)
}
#' Checks if is a varstan object
#'
#' @method is varstan
#'
#' @param obj a varstan object
#'
#' @noRd
#'
is.varstan = function(obj){
  y = FALSE
  if(is(obj,"varstan") ) y = TRUE
  return (y)
}
#' Get the degree freedom values of a varma model
#'
#' get the degree freedom values of a varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_df(fit,robust = FALSE,...)
#'
#' @param obj a varstan object
#' @param robust a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with the degree freedom time series parameter
#'
get_df = function(obj,robust = FALSE,...){
  if( !is.varstan(obj) )
    stop("The current object is not a varstan class")

  if(obj$model$genT != TRUE)
    stop("The current model is not a Generalized t-student varma model")

  resd = get_df_varma(model = obj$model,fit = obj$stanfit,robust)

  return(resd)
}
#' Extract chains of an stanfit object implemented in rstan package
#'
#'
#' @usage  extract_stan(obj,permuted = TRUE,inc_warmup = FALSE,include = TRUE)
#'
#' @param obj a varstan object
#' @param pars n optional character vector providing the parameter names
#' (or other quantity names) of interest. If not specified, all parameters
#' and other quantities are used. The log-posterior with name lp__ is also
#' included by default.
#' @param permuted A logical scalar indicating whether the draws
#' after the warmup period in each chain should be permuted and merged.
#' If FALSE, the original order is kept. For each stanfit object,
#' the permutation is fixed (i.e., extracting samples a second time will
#' give the same sequence of iterations).
#' @param inc_warmup A logical scalar indicating whether to include the
#' warmup draws. This argument is only relevant if permuted is FALSE.
#' @param include A logical scalar indicating whether the parameters named
#' in pars should be included (TRUE) or excluded (FALSE).
#'
#' @author  Asael Alonzo Matamoros
#'
#' @import rstan
#' @export
#'
extract_stan = function(obj,pars,permuted = TRUE, inc_warmup = FALSE,include = TRUE){

  if(!is.varstan(obj) )
    stop("The current object is not a varstan class")

  chains = rstan::extract(obj$stanfit,pars,permuted,inc_warmup,include)

  return(chains)
}
#' Get the stanfit object generated by the rstan package
#'
#' @usage  as.stan(obj)
#'
#' @param obj a varstan object
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a stanfit object
#'
#' @export
#'
as.stan = function(obj){
  if( !is.varstan(obj) )
    stop("The current object is not a varstan class")

  stanfit = obj$stanfit

  return(stanfit)
}
#' Extracts all the order coefficients in a list
#'
#' @param obj A varstan object
#' @noRd
#'
get_order = function(obj){
  if(!is.varstan(obj))
    stop("The object is not a varstan class")

  if(is.Sarima(obj$model)) return(get_order_arima(obj$model))
  if(is.garch(obj$model))  return(get_order_garch(obj$model))
  if(is.varma(obj$model))  return(get_order_varma(obj$model))
  if(is.Bekk(obj$model))   return(get_order_varma(obj$model))
}
#' Max order  coefficients in a varma model
#'
#' @param obj A varstan object
#' @noRd
#'
max_order = function(obj){
  if(!is.varstan(obj))
    stop("The object is not a varstan class")

  if(is.Sarima(obj$model)) return(max_order_arima(obj$model))
  if(is.garch(obj$model))  return(max_order_garch(obj$model))
  if(is.varma(obj$model))  return(max_order_varma(obj$model))
  if(is.Bekk(obj$model))   return(get_order_varma(obj$model))
}
#' Extracts all the order coefficients in a list
#'
#' @param obj A varstan object
#' @noRd
#'
Total_order = function(obj){
  if(!is.varstan(obj))
    stop("The object is not a varstan class")

  order = as.numeric(get_order(obj))
  if(is.Sarima(obj$model)){
    n = length(order)-1
    s1 = sum(order[1:n])+2
  }
  else s1 = sum(order)+2

  return(s1)
}

