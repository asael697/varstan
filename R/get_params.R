#'
#' Excluded and included parameters in a defined time series model
#'
#' @param obj a varstan object
#' @noRd
#'
get_params = function(obj,...){
  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  if(is.Sarima(obj$model)) gp = get_params_arima(obj$model)
  if(is.naive(obj$model))  gp = get_params_arima(obj$model)
  if(is.garch(obj$model))  gp = get_params_garch(obj$model)
  if(is.SVM(obj$model))    gp = get_params_garch(obj$model)
  if(is.varma(obj$model))  gp = get_params_varma(obj$model)
  if(is.Bekk(obj$model))   gp = get_params_varma(obj$model)

  return(gp)
}
#'
#' Excluded parameters in a Sarima model
#'
#' @param dat a Sarima model
#' @noRd
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
#' Excluded parameters in a  garch model
#'
#' @param dat a garch model
#' @noRd
#'
get_params_garch = function(dat,...){
  include = c("mu0","sigma0")
  if(dat$s > 0) include = c(include,"alpha")
  if(dat$k > 0) include = c(include,"beta")
  if(dat$h > 0) include = c(include,"mgarch")
  if(dat$p > 0) include = c(include,"phi")
  if(dat$q > 0) include = c(include,"theta")
  if(dat$d1 >0) include = c(include,"breg")
  if(dat$genT)  include = c(include,"v")
  if(dat$asym1) include = c(include,"gamma")

  exclude = c("phi0","theta0")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
#' Excluded parameters in a  varma model
#'
#' @param dat a varma model
#' @noRd
#'
get_params_varma = function(dat,...){
  include = c("mu0","sigma0")
  if(dat$p > 0) include = c(include,"phi")
  if(dat$q > 0) include = c(include,"theta")
  if(dat$s > 0) include = c(include,"alpha")
  if(dat$k > 0) include = c(include,"beta")
  if(dat$h > 0) include = c(include,"mgarch")
  if(dat$genT)  include = c(include,"v")

  exclude = c("phi0","theta0","Msigma0","vsigma0",
              "sigma1","Lsigma","vsigma","lambda1")
  pars = list(include = c(include,"loglik"),exclude = exclude)
  return(pars)
}
