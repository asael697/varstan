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
#' @usage  extractm(indx,fit,n,d)
#'
#' @param vect  a vector with the posterior estimate
#' @param d an integer with the number of columns
#' @param p   the number of lags to be extracted
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a  list of matrices with the extracted lags
#'
vector_to_matrix = function(vect,d = 2,p = 1){
  l1 = list()
  n1 = length(vect)
  x = matrix(1:n1,ncol = d^2,byrow = TRUE)
  for(i in 1:p) l1[[i]] = matrix(vect[x[i,]],nrow = d)
  return(l1)
}
#'
#' Replicate Elements of Vector
#'
#' replicate a value d times if the value is different than x0
#'
#' @usage  repeat_value(x,d,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimention
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with repeated values
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
#'
#' Complete vector
#'
#' Completes the prior vector hiper parameters to their default value
#'
#' @usage  complete(x,d,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimention
#' @param x0 a real with the preliminar value for the hyper-parameter
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the hyper-parameter value
#'
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
#' @param obj: a  model object
#'
#' @export
#'
is.model = function(obj){
  y = FALSE
  if(class(obj) == "arima") y = TRUE
  if(class(obj) == "garch") y = TRUE
  if(class(obj) == "varma") y = TRUE
  if(class(obj) == "bekk")  y = TRUE
  return (y)
}
#'  summary function
#'  A report of the desired modelt
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

#'  summary function
#'  A report of the desired model
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
#'
#' Positive values in vector
#'
#' checks for positive values in a vector (x_i > 0) usefull for
#' checking scale parameters
#'
#' @usage  positive_check(x,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimention
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the checked values
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
#'
#' No negative values in vector
#'
#' checks for no negative values in a vector (x_i >= 0 ) usefull for checking
#' lags in models
#'
#' @usage  no_negative_check(x,x0)
#'
#' @param x is a vector with the hyper-parameter coefficients
#' @param d an integer with the vector dimention
#' @param x0 a real value to replace in a non positive value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a vector with the checked values
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
#'  Check if the value is in the domain of a location parameter
#'
check_loc <- function(x) {
  return(x)
}
#'  Check if the value is in the domain of a scale parameter
#'
#'
check_scl <- function(x) {
  if(x > 0 ){
    return(x)
  }
  else{
    return(1)
  }
}
#'  Check if the value is in the domain of a form parameter
#'
#'
check_form <- function(x) {
  if(x > 0 ){
    return(x)
  }
  else{
    return(1)
  }
}
#'  Check if the value is in the domain of a degree freedom parameter
#'
#'
check_df <- function(x) {
  if(x >= 1 ){
    return(x)
  }
  else{
    return(1)
  }
}
#'  Check if the value is in the domain of a scale parameter
#'
check_dist <- function(x,par) {
  y = FALSE
  if(par == "ar"){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"beta"))    y = TRUE
    if(identical(x,"uniform")) y = TRUE
  }
  if(par == "mu"){
    if(identical(x,"normal"))  y = TRUE
    if(identical(x,"student")) y = TRUE
    if(identical(x,"cauchy"))  y = TRUE
  }
  if(par == "sigma"){
    if(identical(x,"normal"))      y = TRUE
    if(identical(x,"student"))     y = TRUE
    if(identical(x,"cauchy"))      y = TRUE
    if(identical(x,"gamma"))       y = TRUE
    if(identical(x,"chi_square"))  y = TRUE
  }
  if(par == "dfv"){
    if(identical(x,"normal"))      y = TRUE
    if(identical(x,"gamma"))       y = TRUE
    if(identical(x,"Jeffrey"))     y = TRUE
  }
  return(y)
}
#'  Check if the value is a type of parameter
#'
check_type <- function(x) {
  y = FALSE
    if(identical(x,"ma"))     y = TRUE
    if(identical(x,"ar"))     y = TRUE
    if(identical(x,"arch"))   y = TRUE
    if(identical(x,"garch"))  y = TRUE
    if(identical(x,"mgarch")) y = TRUE
    if(identical(x,"mu0"))    y = TRUE
    if(identical(x,"sigma0")) y = TRUE
    if(identical(x,"dfv"))    y = TRUE
  return(y)
}
###############################################################################################
#                  Generic Functions
###############################################################################################

#'  Set the generic function for print a varstan class
#'
#'  @export
#'
#'
print = function(obj,...){
  UseMethod("print")
}
