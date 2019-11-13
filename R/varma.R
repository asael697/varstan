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
    m1$prior_dfv = c(2,0.1,1,6)
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
<<<<<<< HEAD
#' Print a report of the constructed model
#'
#' @param dat: an varma object
#'
#' @method report varma
#' @export
#'
report.varma = function(dat){
  cat("\n")
  cat("y ~ varma(",dat$p,",",dat$q,") of dimension d =",dat$d,"and length d =",dat$n, "\n")
  cat("Priors: \n Intercept:\n")
  get_prior(dat,type = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(dat,type = "sigma0")
  cat("\n ar parameters: \n")
  get_prior(dat,type = "ar")
  cat("\n ma parameters: \n")
  get_prior(dat,type = "ma")
  if(dat$sd == "mgarch"){
    cat("sigma ~ Bekk(",dat$s,",",dat$k,",",dat$h,") \n")
    cat("\n Volatility components:")
    cat("\n arch parameters: \n")
    get_prior(dat,type = "arch")
    cat("\n garch parameters: \n")
    get_prior(dat,type = "garch")
    cat("\n mgarch parameters: \n")
    get_prior(dat,type = "mgarch")
  }
  if(dat$genT == TRUE){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ G(v/2,v/2) \n")
    get_prior(dat,type = "dfv")
  }
}
=======
>>>>>>> s3 methods corrected
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
<<<<<<< HEAD
#' @export
#'
get_params.varma = function(dat,...){
=======
#'
get_params_varma = function(dat,...){
>>>>>>> s3 methods corrected
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
              "mu","epsilon","sigma1","sigma","Lsigma","vsigma","lambda1")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
<<<<<<< HEAD
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
#' @export
#'
#' @return  a stanfit object
#'
fit.varma = function(model,
                       chains=4,
                       iter=2000,
                       warmup=floor(iter/2),
                       adapt.delta = 0.90,...){
    if(is.varma(model)){
      if(model$genT == FALSE){
        pars = get_params.varma()$exclude

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
        pars = get_params.varma()$exclude

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
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
summary.varma = function(model,fit,robust = FALSE,conf = 0.975,...){
  qq = c(1-conf,conf)
  par1 = get_params.varma(model)$include
  post = as.data.frame(rstan::extract(fit,par1, permuted = TRUE) )
  sum = as.data.frame(t(apply(post,2,my_sum,robust,conf)))
  if(robust) names(sum) = c("median","mad", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  else names(sum) = c("mean","se", paste0(qq[1]*100,"%"), paste0(qq[2]*100,"%"),"ess_bulk","Rhat" )
  return(sum)
}
#' point estimate of an varma model
#'
#' Get the point estimate of an varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted parameters
#'
#' @usage  point_estimate.varma(model,fit)
#'
#' @param fit: a stanfit object
#' @param model: a varbekk model
#' @param robust: a boolean for obtain the robust estimation
#' @param par: the wanted parameters, by default the option all is used
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
point_estimate.varma = function(model,fit,robust = FALSE,...){
  l1 = list()
  # mu0 Parameter
  l1$mu0 = get_lag.varma("mu0",fit,model,robust = FALSE)
  # sigmua0 Parameter
  l1$sigma0 =vector_to_matrix(get_lag.varma("sigma0",fit,model,robust = FALSE),d = model$d,p = 1)
  # VAR Parameter
  if(model$p > 0 ){
    l1$phi = vector_to_matrix(get_lag.varma("phi",fit,model,robust = FALSE),d = model$d,p = model$p)
  }
  # MA Parameter
  if(model$q > 0 ){
    l1$theta = vector_to_matrix(get_lag.varma("theta",fit,model,robust = FALSE),d = model$d,p = model$q)
  }
  # alpha Parameter
  if(model$s > 0 ){
    l1$alpha = vector_to_matrix(get_lag.varma("alpha",fit,model,robust = FALSE),d = model$d,p = model$s)
  }
  # beta Parameter
  if(model$k > 0 ){
    l1$beta = vector_to_matrix(get_lag.varma("beta",fit,model,robust = FALSE),d = model$d,p = model$k)
  }
  # mgarch Parameter
  if(model$h > 0 ){
    l1$mgarch = vector_to_matrix(get_lag.varma("mgarch",fit,model,robust = FALSE),d = model$d,p = model$h)
  }
  if(model$genT){
    if(robust) pe = apply(post$v,2,median)
    else pe = apply(post$v,2,mean)
    l1$v = pe
  }
  return(l1)
}
#' Get the fitted values of an varma model
#'
#' get the fitted values of an varma(p,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  point_estimate.varma(fit)
#'
#' @param model: a varma model object
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#' @param conf: a value between 0 and 1 with the desired confidence
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_fit.varma = function(model,fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"fit", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d1,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
  return(sum1)
}
#' Get the fitted values of an arima model
#'
#' get the fitted values of an arima(p,d,q) model  in STAN
#'
#' The function returns a data.frame object with the fitted values
#'
#' @usage  get_residuals.varma(fit)
#'
#' @param fit: a stanfit object
#' @param robust: a boolean for obtain the robust estimation
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_residuals.varma = function(fit,robust = FALSE,...){
  post = as.data.frame(rstan::extract(fit,"residual", permuted = TRUE) )
  if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d1,byrow = TRUE))
  else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
  return(sum1)
}
=======
>>>>>>> s3 methods corrected
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
<<<<<<< HEAD
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_df.varma = function(fit,model,robust = FALSE,...){
=======
#' @return  a data frame with all the important fitted parameters
#'
get_df_varma = function(fit,model,robust = FALSE,...){
>>>>>>> s3 methods corrected
  if(model$genT == TRUE){
    post = as.data.frame(rstan::extract(fit,"lambda", permuted = TRUE) )
    if(robust) sum1 = t(matrix(apply(post,2,median),nrow = d1,byrow = TRUE))
    else sum1 = t(matrix(apply(post,2,mean),nrow = model$d,byrow = TRUE))
    return(sum1)
}
  else cat("The current model is not a Generalized t-student varma model")
}
#' Get the lag parameters of a varma model
#'
#' get the degree freedom values of a varma(p,q) model  in STAN
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
<<<<<<< HEAD
#' @export
#'
#' @return  a data frame with all the important fitted parameters
#'
get_lag.varma = function(type,fit,model,robust = FALSE,...){
  if(type %in% get_params.varma(model)$include ){
=======
#' @return  a data frame with all the important fitted parameters
#'
get_lag_varma = function(type,fit,model,robust = FALSE,...){
  if(type %in% get_params_varma(model)$include ){
>>>>>>> s3 methods corrected
    post = as.data.frame(rstan::extract(fit,type, permuted = TRUE) )
    if(robust) pe = apply(post,2,median)
    else pe = apply(post,2,mean)
    return(pe)
  }
  else cat(Type,"is not a fitted parameter")
}
