#' Generic function and method for the posterior point estimate of all  parameters in varstan object
#'
#' It returns a list with the point estimate value of the posterior distribution
#' of \strong{all} the model parameters of the varstan object. By defaults it returns the
#' posterior mean.
#'
#' @aliases posterior_estimate posterior_estimate.varstan
#'
#' @usage  posterior_estimate(obj,..)
#'
#' @param obj a varstan object
#' @param robust A boolean value, if its \code{TRUE} it returns the median of the posterior distribution,
#' And if its \code{FALSE} it returns the mean, by default is the \code{FALSE} value
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return  a list with the components of mu0, sigma0, ar, ma, sar, sma, arch, garch, mgarch, and breg,
#' of one of the defined varstan models
#'
#' @export
#'
posterior_estimate <- function(obj, ...) {
  UseMethod("posterior_estimate")
}
#'
#' @export
#'
posterior_estimate.varstan = function(obj,robust = FALSE,...){
  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  if(is.Sarima(obj$model)) resume = point_estimate_arima(model = obj$model,fit = obj$stanfit,roubst = robust)
  if(is.naive(obj$model))  resume = point_estimate_arima(model = obj$model,fit = obj$stanfit,roubst = robust)
  if(is.garch(obj$model))  resume = point_estimate_garch(model = obj$model,fit = obj$stanfit,roubst = robust)
  if(is.varma(obj$model))  resume = point_estimate_varma(model = obj$model,fit = obj$stanfit,roubst = robust)
  if(is.Bekk(obj$model))   resume = point_estimate_varma(model = obj$model,fit = obj$stanfit,roubst = robust)

  return(resume)
}
#' posterior estimate method for garch model
#'
#' @param model a garch model
#' @param fit a stanfit object returned by the rstan package
#' @param robust a boolean value for robust indicators
#'
#' @noRd
#'
point_estimate_garch = function(model,fit,robust = FALSE,...){
  l1 = list()
  # mu0 Parameter
  l1$mu0 = extract_estimate(fit = fit,model = model,par = "mu0",robust)
  # sigma0 Parameter
  l1$sigma0 =extract_estimate(fit = fit,model = model,par = "sigma0",robust)
  # VAR Parameter
  if(model$p > 0 ){
    l1$phi = extract_estimate(fit = fit,model = model,par = "phi",robust)
  }
  # MA Parameter
  if(model$q > 0 ){
    l1$theta = extract_estimate(fit = fit,model = model,par = "theta",robust)
  }
  # alpha Parameter
  if(model$s > 0 ){
    l1$alpha = extract_estimate(fit = fit,model = model,par = "alpha",robust)
  }
  # beta Parameter
  if(model$k > 0 ){
    l1$beta = extract_estimate(fit = fit,model = model,par = "beta",robust)
  }
  # mgarch Parameter
  if(model$h > 0 ){
    l1$mgarch = extract_estimate(fit = fit,model = model,par = "mgarch",robust)
  }
  return(l1)
}
#' posterior estimate method for Sarima models
#'
#' @param model a Sarima model
#' @param fit a stanfit object returned by the rstan package
#' @param robust a boolean value for robust indicators
#'
#' @noRd
#'
point_estimate_arima = function(model,fit,robust = FALSE,...){
    l1 = list()
    # mu0 Parameter
    l1$mu0 = extract_estimate(fit = fit,model = model,par = "mu0",robust)
    # sigma0 Parameter
    l1$sigma0 =extract_estimate(fit = fit,model = model,par = "sigma0",robust)
    # VAR Parameter
    if(model$p > 0 ){
      l1$phi = extract_estimate(fit = fit,model = model,par = "phi",robust)
    }
    # MA Parameter
    if(model$q > 0 ){
      l1$theta = extract_estimate(fit = fit,model = model,par = "theta",robust)
    }
    # sar Parameter
    if(model$P > 0 ){
      l1$sphi = extract_estimate(fit = fit,model = model,par = "sphi",robust)
    }
    # sma Parameter
    if(model$Q > 0 ){
      l1$stheta = extract_estimate(fit = fit,model = model,par = "stheta",robust)
    }
    # bregParameter
    if(model$d1 > 0 ){
      l1$breg = extract_estimate(fit = fit,model = model,par = "breg",robust)
    }
    return(l1)
}
#' posterior estimate method for varma models
#'
#' @param model a varma model
#' @param fit a stanfit object returned by the rstan package
#' @param robust a boolean value for robust indicators
#'
#' @noRd
#'
point_estimate_varma = function(model,fit,robust = FALSE,...){
  l1 = list()
  # mu0 Parameter
  l1$mu0 = get_lag_varma("mu0",fit,model,robust = FALSE)
  # sigmua0 Parameter
  l1$sigma0 =vector_to_matrix(get_lag_varma("sigma0",fit,model,robust = FALSE),d = model$dimension,p = 1)
  # VAR Parameter
  if(model$p > 0 ){
    l1$phi = vector_to_matrix(get_lag_varma("phi",fit,model,robust = FALSE),d = model$dimension,p = model$p)
  }
  # MA Parameter
  if(model$q > 0 ){
    l1$theta = vector_to_matrix(get_lag_varma("theta",fit,model,robust = FALSE),d = model$dimension,p = model$q)
  }
  # alpha Parameter
  if(model$s > 0 ){
    l1$alpha = vector_to_matrix(get_lag_varma("alpha",fit,model,robust = FALSE),d = model$dimension,p = model$s)
  }
  # beta Parameter
  if(model$k > 0 ){
    l1$beta = vector_to_matrix(get_lag_varma("beta",fit,model,robust = FALSE),d = model$dimension,p = model$k)
  }
  # mgarch Parameter
  if(model$h > 0 ){
    l1$mgarch = vector_to_matrix(get_lag_varma("mgarch",fit,model,robust = FALSE),d = model$dimension,m = model$m,p = model$h)
  }
  if(model$genT){
    post = data.frame(rstan::extract(fit,"v", permuted = TRUE))
    if(robust) pe = apply(post,2,median)
    else pe = apply(post,2,mean)
    l1$v = pe
  }
  return(l1)
}
