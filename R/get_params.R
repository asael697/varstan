#'
#' Excluded and included parameters in a defined time series model
#' @param obj a varstan object
#'
get_params = function(obj,...){
  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  if(is.Sarima(obj$model)) gp = get_params_arima(obj$model)
  if(is.garch(obj$model))  gp = get_params_garch(obj$model)
  if(is.varma(obj$model))  gp = get_params_varma(obj$model)

  return(gp)
}
#'
#' Excluded parameters in a Sarima model
#'
get_params_arima = function(dat,...){
  include = c("mu0","sigma0")
  if(dat$p > 0) include = c(include,"phi")
  if(dat$q > 0) include = c(include,"theta")
  if(dat$P > 0) include = c(include,"sphi")
  if(dat$Q > 0) include = c(include,"stheta")
  if(dat$d1 >0) include = c(include,"breg")
  exclude = c("phi0","theta0","Phi0","Theta0")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' Excluded parameters in a  Garch model
#'
#'
get_params_garch = function(dat,...){
  include = c("mu0","sigma0")
  if(dat$s > 0) include = c(include,"alpha")
  if(dat$k > 0) include = c(include,"beta")
  if(dat$h > 0) include = c(include,"mgarch")
  if(dat$p > 0) include = c(include,"phi")
  if(dat$q > 0) include = c(include,"theta")
  if(dat$genT == TRUE) include = c(include,"v")

  exclude = c("phi0","theta0")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
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
