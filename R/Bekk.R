#' Constructor varma(p,q)-Bekk(s,k) object
#'
#' Constructor of the varma(p,q)-Bekk(s,k) object for Bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage Bekk(ts,order = c(1,1,0),varma = c(0,0),genT = FALSE)
#'
#' @param ts an multivariate time series
#' @param varma A specification of VARMA model: the two components (p, q) are the AR
#' order, the number of differences, and the MA order.
#' @param order an optional value for specify the Bekk  part for variance, same as
#' order parameter:  the three components (s, k, h) are the arch order,
#' the garch order and the garch on the mean (mgarch) order.
#' @param genT a boolean value to specify a Generalized a t-student model Cruz (2015)
#'
#' @details If \code{sd} option can only be used with the mbekk function, and it  adds
#' a m-Bekk model for volatility.
#'
#' If a mbekk model is not used, then sigma0 represents the covariance matrix
#' of the model.
#'
#' If a bekk model is specified, then sigma0 represents the triangular inferior matrix
#' alpha0 of a bekk model
#'
#' The default priors used in varma are:
#'
#' \itemize{
#'  \item{"ar"}{ar ~ normal(0,0.5)}
#'  \item{"ma"}{ma ~ normal(0,0.5)}
#'  \item{"mu0"}{mu0 ~ normal(0,1)}
#'  \item{"sigma0"}{sigma0 ~ t-student(0,1,7)}
#'  \item{"arch"}{arch ~ normal(0,0.5)}
#'  \item{"garch"}{garch ~ normal(0,0.5)}
#'  \item{"mgarch"}{mgarch ~ normal(0,0.5)}
#'  \item{"dfv"}{dfv ~ gamma(2,0.1)}
#' }
#'
#' For changing the default prior use the function \code{set_prior}
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @references
#'  Polasek, Ren(1999).
#'  A multivariate GARCH-M model for exchange rates in the US,Germany and Japan
#'
#'  Fonseca,Ferreira, Migon (2008)
#'  Objective Bayesian analysis for the Student-t regression model
#'
#' @seealso \code{\link{garch}} \code{\link{Sarima}} \code{\link{set_prior}}
#'
#' @examples
#' # Declare a Bekk model for the Astrovan data
#'
#' model = Bekk(Astrovan,p=1,q=1,sd=mbekk(1,1,0))
#' model
#'
#'
Bekk = function(ts,order = c(1,1,0),varma = c(0,0),genT = FALSE){
  n = dim(ts)[2]
  d = dim(ts)[1]
  y = matrix(ts,nrow = d)
  time = as.numeric(time(t(ts)))

  if(n < d){
    n = d
    d = dim(ts)[2]
    y = t(y)
    time = as.numeric(time(ts))
  }
  m1 = list(n = n,dimension = d,time = time,d = d,
            p = no_negative_check(varma[1]),
            q = no_negative_check(varma[2]),m = d*(d+1)/2,
            y = t(y))
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_ar  = matrix(rep(c(0,10,1,1),varma[1]),ncol = 4,byrow = TRUE)
  m1$prior_ma  = matrix(rep(c(0,10,1,1),varma[2]),ncol = 4,byrow = TRUE)

  m1$s = no_negative_check(order[1])
  m1$k = no_negative_check(order[2])
  m1$h = no_negative_check(order[3])
  m1$prior_arch =  matrix(rep(c(0,10,1,1),m1$s),ncol = 4,byrow = TRUE)
  m1$prior_garch = matrix(rep(c(0,10,1,1),m1$k),ncol = 4,byrow = TRUE)
  m1$prior_mgarch =matrix(rep(c(0,10,1,1),m1$h),ncol = 4,byrow = TRUE)
  m1$prior_lkj = c(7,1,1,1)


  if(genT == TRUE){
    m1$genT = TRUE
    m1$prior_dfv = c(2,0.1,1,9)
  }
  else m1$genT = FALSE
  attr(m1,"class") = "Bekk"
  return(m1)
}
#' Checks if is a Bekk object
#'
#' @param obj a Bekk object
#' @noRd
#'
is.Bekk = function(obj){
  y = FALSE
  if(class(obj) == "Bekk") y = TRUE
  return (y)
}
#' Get the degree freedom values of a Bekk model
#'
#' get the degree freedom values of a Bekk(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_df_varma(fit,model,robust = FALSE,...)
#'
#' @param fit a stanfit object
#' @param model a model object
#' @param robust a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
#' @noRd
#'
get_df_varma = function(fit,model,robust = FALSE,...){
    post = as.data.frame(rstan::extract(fit,"lambda", permuted = TRUE) )
    if(robust) sum1 = t(matrix(apply(post,2,median),nrow = model$d,byrow = TRUE))
    else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
    return(sum1)
}
#' Get the lag parameters of a varma model
#'
#' get the degree freedom values of a t-student varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_lag_varma(type,model,fit,robust,...)
#'
#' @param type the parameter to be extracted
#' @param fit a stanfit object
#' @param model a model object
#' @param robust a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#' @noRd
#'
get_lag_varma = function(type,fit,model,robust = FALSE,...){
  if(type %in% get_params_varma(model)$include ){
    post = as.data.frame(rstan::extract(fit,type, permuted = TRUE) )
    if(robust) pe = apply(post,2,median)
    else pe = apply(post,2,mean)
    return(pe)
  }
  else cat(Type,"is not a fitted parameter")
}
#' Extracts all the order coefficients in a list
#'
#' @param dat a varma model
#' @noRd
#'
get_order_varma= function(dat){
  return(list(p = dat$p,q=dat$q,s=dat$s,k=dat$k,h=dat$h))

}
#' Max order  coefficients in a varma model
#'
#' @param dat a varma model
#' @noRd
#'
max_order_varma= function(dat){
  return(max(c(dat$p,dat$q,dat$s,dat$k,dat$h)))
}
