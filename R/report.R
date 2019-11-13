#' Print a report of the constructed model
#'
#' @param dat: an arima object
#'
#' @export
#'
report = function(dat){
  if( is.arima(dat)){
    cat("\n")
    cat("y ~ arima(",dat$p,",",dat$d,",",dat$q,") \n")
    cat("Priors: \n Intercept:\n")
    get_prior(dat,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(dat,type = "sigma0")
    cat("\n ar parameters: \n")
    get_prior(dat,type = "ar")
    cat("\n ma parameters: \n")
    get_prior(dat,type = "ma")
    if(dat$sd == "mgarch"){
      cat("sigma ~ garch(",dat$s,",",dat$k,",",dat$h,") \n")
      cat("\n Volatility components:")
      cat("\n arch parameters: \n")
      get_prior(dat,type = "arch")
      cat("\n garch parameters: \n")
      get_prior(dat,type = "garch")
      cat("\n mgarch parameters: \n")
      get_prior(dat,type = "mgarch")
    }
  }
  if(is.garch(dat)){
    cat("\n")
    cat("y ~ garch(",dat$s,",",dat$k,",",dat$h,") \n")
    cat("Priors: \n Intercept:\n")
    get_prior(dat,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(dat,type = "sigma0")
    cat("\n arch parameters: \n")
    get_prior(dat,type = "arch")
    cat("\n garch parameters: \n")
    get_prior(dat,type = "garch")
    cat("\n mgarch parameters: \n")
    get_prior(dat,type = "mgarch")
  }
  if(is.varma(dat)){
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
}
