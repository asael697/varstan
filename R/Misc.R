###############################################################################################
#                  Misc functions in varstan
################################################################################################
#
#' Extract lags from vector
#'
#' Extract a Matrix from an stan fit summary
#'
#'
#' The function returns a matrix with the extracted coefficients of the stan fit
#' summary
#'
#' @usage  vector_to_matrix(vect,d = 2,m = d, p = 1)
#'
#' @param vect  a vector with the posterior estimate
#' @param d an integer with the number of columns
#' @param p   the number of lags to be extracted
#'
#' @author  Asael Alonzo Matamoros
#'
#' @noRd
#'
#' @return  a  list of matrices with the extracted lags
#'
vector_to_matrix = function(vect,d = 2,m = d,p = 1){
  l1 = list()
  n1 = length(vect)
  x = matrix(1:n1,ncol = d*m,byrow = TRUE)
  for(i in 1:p) l1[[i]] = matrix(vect[x[i,]],nrow = m,ncol = d)
  return(l1)
}
#' point estimate of univariate parameters
#'
#' get the point estimate of univariate parameters in a STAN model
#'
#' The function returns a vector with the fitted parameters
#'
#' @usage  extract_estimate(model,fit)
#'
#' @param fit  a stanfit object
#' @param model a varma, Sarima or garch model
#' @param par the desired parameter
#' @param robust a boolean for obtain the robust estimation
#'
#' @noRd
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with all the elected parameters
#'
extract_estimate = function(model,fit,par,robust = FALSE,...){
  post = data.frame(rstan::extract(fit,par, permuted = TRUE))
  if(robust) pe = apply(post,2,median)
  else pe = apply(post,2,mean)
  return( as.numeric(pe) )
}
#' Extract estimate time series parameters from a stanfit object
#'
#'  @param obj varstan object
#'  @param par a string with the desired parameter
#'  @param robust a boolean value for robust estimate
#'  @param lag the max lags desired
#'
#'  @author Asael Alonzo Matamoros
#'
#'  @noRd
#'
#'  @return An univariate time series as a numeric vector
#'
extract_ts = function(obj,par,lag=1,robust=FALSE){
  if (is.varstan(obj)){
    post = data.frame(extract_stan(obj,pars = par, permuted = TRUE))
    n = ncol(post)
    n1 =n-lag+1
    post = post[,n1:n]
    if(robust) pe = apply(as.data.frame(post),2,median)
    else pe = apply(post,2,mean)
    return(pe)
  }
  else print("The current object is not a varstan object")
}
#' Extract estimate of multivariate time series parameters from a stanfit object
#'
#'  @param obj varstan object
#'  @param par a string with the desired parameter
#'  @param d the dimension of the time series
#'  @param robust a boolean value for robust estimate
#'  @param lag the max lags desired
#'
#'  @author Asael Alonzo Matamoros
#'
#'  @noRd
#'
#'  @return A multivariate time series as a numeric matrix
#'
extract_mts = function(obj,par,d=1,lag=1,robust=FALSE){

  if (! is.varstan(obj))
    stop("The current object is not a varstan object")

  post = data.frame(extract_stan(obj = obj,pars = par, permuted = TRUE))
  pe = matrix(,nrow = d,ncol = lag)
  n = obj$model$n
  n1 = n-lag

  for(i in 1:d){

    post = data.frame(post[,-c(1:n1)])
    post1 = as.data.frame(post[,c(1:lag)])

    if(robust)
      pe[i,] = apply(post1,2,median)
    else
      pe[i,] = apply(post1,2,mean)

    post =post[,-c(1:lag)]
  }
  return(pe)
}
#' Extract the initial values of a differenced time series
#'
#'  @param ts An univariate time series
#'  @param d the number of differences
#'  @param D the number of seasonal differences
#'  @param period the time series period
#'
#'  @author Asael Alonzo Matamoros
#'
#'  @return A list with the differenced time series, the initials values
#'  for inverse differences
#'
#'  @noRd
#'
dif = function(ts,d = 0,D = 0,period = 1){

  y_diff = ts;init  = NULL;inits = NULL

  if(d > 0){
    for(i in 1:d){
      init = c(tail(y_diff,n = 1),init)
      y_diff = diff(y_diff)
    }
  }
  if(D > 0){
    for(i in 1:D){
      inits = c(tail(y_diff,n = period),inits)
      y_diff = diff(y_diff,lag = period)
    }
  }
  if (D > 1) inits  = matrix(inits,nrow = D,ncol = period,byrow = TRUE)
  m = list(y = y_diff,init = init,inits = inits)
  return(m)
}
#' Inverse difference time series to forecasted values
#'
#' @param ts An univariate time series
#' @param init the initial values for normal difference
#' @param inits the initial values for seasonal difference
#'
#' @author Asael Alonzo Matamoros
#'
#' @return A vector with the inverse difference time series
#'
#' @noRd
#'
inv_dif = function(ts,init,inits){
  y = ts; n = length(ts);d = 0;D = 0; period = 0

  if(!is.null(init)) d = length(init)
  if(!is.null(inits)) {
    if(!is.matrix(inits)){
      D = 1
      period = length(inits)
    }
    else{
      D = nrow(inits)
      period = ncol(inits)
    }
  }

  if(D > 0){
    if(D == 1) for(i in 1:D) y = tail(diffinv(y,lag = period,xi = inits),n = n)
    else for(i in 1:D) y = tail(diffinv(y,lag = period,xi = inits[i,]),n = n)
  }
  if(d > 0){
    for(i in 1:d) y = tail(diffinv(y,xi = init[i]),n = n)
  }
  return(y)
}
#'
#' Replicate Elements of Vector
#'
#' replicate a value d times if the value is different than x0
#'
#' @usage  repeat_value(x,d,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimension
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with repeated values
#'
#' @noRd
#'
repeat_value = function(x,d,x0){
  if(length(x) == 1 && x != x0){
    y = rep(x,d)
  }
  else{
    y = x
  }
  return(y)
}
#' Complete vector
#'
#' Completes the prior vector hiper parameters to their default value
#'
#' @usage  complete(x,d,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimension
#' @param x0 a real with the preliminary value for the hyper-parameter
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the hyper-parameter value
#'
#' @noRd
#'
complete = function(x,d,x0){
  if(d > 0){
    n =length(x)
    y = 1:d
    if(n < d){
      for(i in 1:d){
        if(i <= n) y[i] = x[i]
        else y[i] = x0
      }
    }
    else{
      for(i in 1:d)
        y[i] = x[i];
    }
  }
  else{
    y=x0
  }
  return(y);
}
#' Checks if is a model object
#'
#' @param obj a  model object
#'
#' @noRd
#'
is.model = function(obj){
  y = FALSE
  if(is(obj,"Sarima")) y = TRUE
  if(is(obj,"arima"))  y = TRUE
  if(is(obj,"garch"))  y = TRUE
  if(is(obj,"varma"))  y = TRUE
  if(is(obj,"Bekk"))   y = TRUE
  if(is(obj,"DWR"))    y = TRUE
  if(is(obj,"naive"))  y = TRUE
  if(is(obj,"SVM"))    y = TRUE
  return (y)
}
#' A function with all the desired indicators in summary function
#'
#' @param x a numeric vector
#' @param robust a boolean value for robust indicators
#' @param conf the confidence level
#'
#' @noRd
#'
my_sum = function(x,robust = FALSE,conf){
  if(robust){
    sum = c(quantile(x,0.5),
            mad(x),
            quantile(x,1-conf),
            quantile(x,conf),
            rstan::ess_bulk(x),
            rstan::Rhat(x)
    )
  }
  else{
    qq = qnorm(c(1-conf,conf))
    sum = c(mean(x),
            sd(x)/sqrt(length(x)),
            mean(x)+qq[1]*sd(x)/sqrt(length(x)),
            mean(x)+qq[2]*sd(x)/sqrt(length(x)),
            rstan::ess_bulk(x),
            rstan::Rhat(x)
    )
  }
  return( round(sum,4) )
}
#' Alternative function with all the desired indicators in summary function
#'
#' @param x a numeric vector
#' @param robust a boolean value for robust indicators
#' @param conf the confidence level
#'
#' @noRd
#'
my_sum1 = function(x,robust = FALSE,conf){
  if(robust){
    sum = c(quantile(x,0.5),
            quantile(x,1-conf),
            quantile(x,conf)
    )
  }
  else{
    qq = qnorm(c(1-conf,conf))
    sum = c(mean(x),
            mean(x)+qq[1]*sd(x)/sqrt(length(x)),
            mean(x)+qq[2]*sd(x)/sqrt(length(x))
    )
  }
  return( round(sum,4) )
}

###############################################################################################
#                  Check functions
################################################################################################
#


#' Positive values in vector
#'
#' checks for positive values in a vector (x_i > 0) useful for
#' checking scale parameters
#'
#' @usage  positive_check(x,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the checked values
#'
#' @noRd
#'
positive_check = function(x,x0 = 1){
  y = x
  d = length(x)
  for(i in 1:d){
    if(x[i] > 0){
      y[i] = x[i]
    }
    else{
      if(x[i]< 0) cat("Value lower than 0, the default value",x0,"will be used \n")
      y[i] = x0
    }
  }
  return(y)
}
#' No negative values in vector
#'
#' checks for no negative values in a vector (x_i >= 0 ) useful for checking
#' lags in models
#'
#' @usage  no_negative_check(x,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the checked values
#'
#' @noRd
#'
no_negative_check = function(x,x0 = 0){
  y = x
  d = length(x)
  for(i in 1:d){
    if(x[i] >= 0){
      y[i] = x[i]
    }
    else{
      if(x[i]< 0) cat("Value lower than 0, the default value",x0,"will be used \n")
      y[i] = x0
    }
  }
  return(y)
}
#'
#' Check the arma coefficients
#'
#' checks for values in between -1 and 1 in a real vector
#'
#' @usage  arma_check(x)
#'
#' @param x is a vector with the hyper-parameter coefficients
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the checked values
#'
#' @noRd
#'
arma_check = function(x){
  y = x
  d = length(x)
  for(i in 1:d){
    if(x[i] <= -1 || x[i] >= 1 ){
      print( "Value not in range, 0 will be used")
      y[i] = 0
    }
  }
  return(y)
}
#' Check if the value is in the domain of a location parameter
#'
#' @param x a location parameter
#' @noRd
#'
check_loc <- function(x) {
  return(x)
}
#' Check if the value is in the domain of a scale parameter
#'
#' @param x an scale parameter
#' @noRd
#'
check_scl <- function(x) {
  if(x > 0 ){
    return(x)
  }
  else{
    return(1)
  }
}
#' Check if the value is in the domain of a form parameter
#'
#' @param x a form parameter
#' @noRd
#'
check_form <- function(x) {
  if(x > 0 ){
    return(x)
  }
  else{
    return(1)
  }
}
#' Check if the value is in the domain of a degree freedom parameter
#'
#' @param x a degree freedom parameter
#' @noRd
#'
check_df <- function(x) {
  if(x >= 1 ){
    return(x)
  }
  else{
    return(1)
  }
}
#' Check if the value is in the domain of a scale parameter
#'
#' @param  x the distribution
#' @param  par the parameter
#'
#' @noRd
#'
check_dist <- function(x,par) {
  y = FALSE
  if(identical(par,"ar")){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(identical(par,"breg")){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"student")) y = TRUE
    if(identical(x,"cauchy"))  y = TRUE
    if(identical(x,"gamma"))   y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(identical(par,"mgarch")){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"student")) y = TRUE
    if(identical(x,"cauchy"))  y = TRUE
    if(identical(x,"gamma"))   y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(identical(par,"mu")){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"student")) y = TRUE
    if(identical(x,"cauchy"))  y = TRUE
    if(identical(x,"gamma"))   y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(identical(par,"mu0")){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"student")) y = TRUE
    if(identical(x,"cauchy"))  y = TRUE
    if(identical(x,"gamma"))   y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(identical(par,"sigma0")){
    if(identical(x,"normal"))      y = TRUE
    if(identical(x,"student"))     y = TRUE
    if(identical(x,"cauchy"))      y = TRUE
    if(identical(x,"gamma"))       y = TRUE
    if(identical(x,"inv_chi_square"))  y = TRUE
    if(identical(x,"inv_gamma"))       y = TRUE
  }
  if(identical(par,"dfv")){
    if(identical(x,"normal"))      y = TRUE
    if(identical(x,"gamma"))       y = TRUE
    if(identical(x,"Jeffrey"))     y = TRUE
    if(identical(x,"inv_gamma"))   y = TRUE
  }
  return(y)
}
#' Check if the value is a type of parameter
#'
#' @param x a parameter
#' @noRd
#'
check_type <- function(x) {
  y = FALSE
    if(identical(x,"sma"))    y = TRUE
    if(identical(x,"ma"))     y = TRUE
    if(identical(x,"ar"))     y = TRUE
    if(identical(x,"sar"))    y = TRUE
    if(identical(x,"arch"))   y = TRUE
    if(identical(x,"garch"))  y = TRUE
    if(identical(x,"mgarch")) y = TRUE
    if(identical(x,"mu0"))    y = TRUE
    if(identical(x,"breg"))   y = TRUE
    if(identical(x,"breg"))   y = TRUE
    if(identical(x,"sigma0")) y = TRUE
    if(identical(x,"dfv"))    y = TRUE
  return(y)
}
