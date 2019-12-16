#' Constructor var(p)-bekk(s,k) object
#'
#' Constructor of the var(p)-bekk(s,k) object for bayesian estimation in STAN
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage varbekk(ts,p,s,k)
#'
#' @param ts an multivariate time series
#' @param p an integer with the order of the var(p) part
#' @param s an integer with the order of the arch(s) part
#' @param k an integer with the order of the arch(k) part
#' @param genT a boolean value to specify a Generalized a t-student model Cruz (2015)
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a list with the components
#' \itemize{
#'  \item n: the length of the time series
#'  \item d: the dimension of the time series
#'  \item p: an integer with the order of the var coefficients
#'  \item s: an integer with the order of the arch coefficients
#'  \item k: an interger with the order of th garch coefficients
#'  \item v: a real with degree freedom for a t-student sample
#'  \item y: vector with the multivariate time series
#'  \item prior_var:   a vector with the hyper-parameters for the var   coefficient
#'  \item prior_arch:  a vector with the hyper-parameters for the arch  coefficient
#'  \item prior_garch: a vector with the hyper-parameters for the garch coefficient
#' }
#'
varma = function(ts,p = 1,q  = 1,sd = mbekk(s=0,k=0,h=0),genT = FALSE){
  n = dim(ts)[2]
  d = dim(ts)[1]
  y = matrix(ts,nrow = d)
  m1 = list(n = n,d = d,
            p = positive_check(p,abs(p)),
            q = no_negative_check(q),m = d*(d+1)/2,
            y = t(y))
  m1$prior_mu0 = c(0,1,0,1)
  m1$prior_sigma0 = c(0,1,7,4)
  m1$prior_ar  = matrix(rep(c(0,10,1,1),p),ncol = 4,byrow = TRUE)
  m1$prior_ma  = matrix(rep(c(0,10,1,1),q),ncol = 4,byrow = TRUE)
  if( !is.null(sd) ){
    m1$s = sd$garch_order[1]
    m1$k = sd$garch_order[2]
    m1$h = sd$garch_order[3]
    m1$prior_arch =  sd$prior_arch
    m1$prior_garch =  sd$prior_garch
    m1$prior_mgarch =  sd$prior_mgarch
    m1$sd = "mgarch"
  }
  else   m1$sd = "none"
  if(genT == TRUE){
    m1$genT = TRUE
    m1$prior_dfv = c(2,0.1,1,9)
  }
  else m1$genT = FALSE
  attr(m1,"class") = "varma"
  return(m1)
}
#' Checks if is a varma object
#' @param obj: a varma object
#' @export
#'
is.varma = function(obj){
  y = FALSE
  if(class(obj) == "varma") y = TRUE
  return (y)
}
#' Adds a Bekk(s,k,h) object to an arima model
#'
#' Adds a Bekk(s,k,h) object to an arima model
#'
#' The function returns  a list with the data for running stan() function of
#'  rstan package
#'
#' @usage bekk(s,k,h)
#'
#' @param s an integer with the order of the arch(s) part
#' @param k an integer with the order of the garch(k) part
#' @param h an integer with the order of the mgarch(h) part
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
mbekk = function(s=1,k=1,h=0){
  ml = list()
  ml$garch_order = c(no_negative_check(s),no_negative_check(k),no_negative_check(h))
  ml$prior_arch    = matrix(rep(c(0,10,1,1),s),ncol = 4,byrow = TRUE)
  ml$prior_garch   = matrix(rep(c(0,10,1,1),k),ncol = 4,byrow = TRUE)
  ml$prior_mgarch  = matrix(rep(c(0,10,1,1),h),ncol = 4,byrow = TRUE)
  ml$sd = "mgarch"
  return(ml)
}
#' Excluded parameters in a  varbekk model
#'
#'
get_params_varma = function(dat,...){
  include = c("mu0","sigma0")
  if(dat$p > 0) include = c(include,"phi")
  if(dat$q > 0) include = c(include,"theta")
  if(dat$sd == "mgarch"){
    if(dat$s > 0) include = c(include,"alpha")
    if(dat$k > 0) include = c(include,"beta")
    if(dat$h > 0) include = c(include,"mgarch")
    if(dat$genT == TRUE) include = c(include,"v")
  }
  exclude = c("phi0","theta0","Msigma0","vsigma0",
              "sigma1","Lsigma","vsigma","lambda1")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}

#' Get the degree freedom values of a varma model
#'
#' get the degree freedom values of a varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_df.varma(fit)
#'
#' @param fit: a stanfit object
#' @param model: a model object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
get_df_varma = function(fit,model,robust = FALSE,...){
  if(model$genT == TRUE){
    post = as.data.frame(rstan::extract(fit,"lambda", permuted = TRUE) )
    if(robust) sum1 = t(matrix(apply(post,2,median),nrow = model$d,byrow = TRUE))
    else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
    return(sum1)
}
  else cat("The current model is not a Generalized t-student varma model")
}
#' Get the lag parameters of a varma model
#'
#' get the degree freedom values of a tvarma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the degree freedom values
#'
#' @usage  get_lag.varma(type,model,fit,robust)
#'
#' @param type: the parameter to be extracted
#' @param fit: a stanfit object
#' @param model: a model object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a data frame with all the important fitted parameters
#'
#' @return  a data frame with all the important fitted parameters
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
#' @method print varma
#' @export print
#' @export
#'
print.varma = function(obj){
  if(is.varma(obj)){
    print(report(obj))
  }
  else{
    print("The current object is not a varma model")
  }
}
