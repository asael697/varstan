#' Print a full report of the time series model in a  varstan object.
#'
#' The function returns a report with the users defined model for the given time series data
#' and all the current defined priors of the model.
#'
#' @usage  report(object,...)
#'
#' @aliases report report.varstan report.Sarima report.garch report.varma report.Bekk report.naive
#'
#' @param object an object varstan object or one of the defined current defined reports in varstan package
#' @param ... additional values need in print methods
#'
#' @details if \code{object} is a varstan object the function will print the information of the
#' defined model inside of the object. If \code{object} is one of the model classes (like Sarima or garch)
#' then it will print the report information as well.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a  string with the defined time series report
#'
#' @examples
#' library(astsa)
#' dat2 = garch(birth,order = c(1,1,0))
#' report(dat2)
#'
#' dat = varma(Astrovan,order = c(1,1))
#' report(dat)
#'
#'
report <- function(object,...) {
  UseMethod("report")
}
#'
#' @method report varstan
#' @export
#'
report.varstan = function(object,...){
  if(!is.varstan(object))
    stop("The current object is not a varstan class")

  if( is.Sarima(object$model)) report.Sarima(object$model)
  if( is.naive(object$model))  report.naive(object$model)
  if( is.garch(object$model))  report.garch(object$model)
  if( is.SVM(object$model))    report.SVM(object$model)
  if( is.varma(object$model))  report.varma(object$model)
  if( is.Bekk(object$model))   report.Bekk(object$model)
}
#'
#' @method report Sarima
#' @export
#'
report.Sarima = function(object,...){
  if( !is.Sarima(object))
    stop("The object is not a Sarima model \n")

  model.Sarima(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n")
  if(object$p  > 0 )get_prior(model = object,par = "ar")
  if(object$q  > 0 )get_prior(model = object,par = "ma")
  if(object$P != 0 || object$Q != 0 || object$D != 0){
    cat("\n Seasonal Parameters: \n")
    if(object$P  > 0 )get_prior(model = object,par = "sar")
    if(object$Q  > 0 )get_prior(model = object,par = "sma")
  }
  if(object$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = object,par = "breg")
  }
}
#'
#' @method report naive
#' @export
#'
report.naive = function(object,...){
  if( !is.naive(object))
    stop("The object is not a naive model \n")

  model.naive(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n")
  if(object$D > 0 ){
    cat("\n period:",object$period,"\n")
  }
}
#'
#' @method report garch
#' @export
#'
report.garch = function(object,...){
  if(!is.garch(object))
    stop("The object is not a garch model \n")

  model.garch(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n")
  if(object$s  > 0 )get_prior(model = object,par = "arch")
  if(object$k  > 0 )get_prior(model = object,par = "garch")
  if(object$h  > 0 )get_prior(model = object,par = "mgarch")
  if(object$p != 0 || object$q != 0 ){
    cat("\n mean Parameters: \n")
    if(object$p  > 0 )get_prior(model = object,par = "ar")
    if(object$q  > 0 )get_prior(model = object,par = "ma")
  }
  if(object$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = object,par = "breg")
  }
  if(object$genT){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ G(v/2,v/2) \n")
    get_prior(model = object,par = "dfv")
  }
  if(object$asym1){
    if(identical(object$asym,1)) cat("\n logistic asymmetric \n")
    if(identical(object$asym,1)) cat("\n exponential asymmetric \n")
    get_prior(model = object,par = "gamma")
  }
}
#'
#' @method report SVM
#' @export
#'
report.SVM = function(object,...){
  if(!is.SVM(object))
    stop("The object is not a stochastic volatility model \n")

  model.SVM(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n log Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n")
  get_prior(model = object,par = "alpha")
  get_prior(model = object,par = "beta")
  if(object$p != 0 || object$q != 0 ){
    cat("\n mean Parameters: \n")
    if(object$p  > 0 )get_prior(model = object,par = "ar")
    if(object$q  > 0 )get_prior(model = object,par = "ma")
  }
  if(object$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = object,par = "breg")
  }
}
#'
#' @method report varma
#' @export
#'
report.varma = function(object,...){
  if(!is.varma(object))
    stop("The object is not a varma model \n")

  model.varma(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n mean Parameters: \n")
  if(object$p  > 0 )get_prior(model = object,par = "ar")
  if(object$q  > 0 )get_prior(model = object,par = "ma")

  if(object$s != 0 || object$k != 0 || object$h != 0 ){
    cat("\n Bekk Parameters: \n")
    if(object$s  > 0 )get_prior(model = object,par = "arch")
    if(object$k  > 0 )get_prior(model = object,par = "garch")
    if(object$h  > 0 )get_prior(model = object,par = "mgarch")
  }
  if(object$genT){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ inverse.gamma(v/2,v/2) \n")
    get_prior(object,par = "dfv")
  }
}
#'
#' @method report Bekk
#' @export
#'
report.Bekk = function(object,...){
  if(!is.Bekk(object))
    stop("The object is not a Bekk model \n")

  model.Bekk(object)
  cat("Priors: \n Intercept:\n")
  get_prior(object,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(object,par = "sigma0")
  cat("\n mean Parameters: \n")
  if(object$p  > 0 )get_prior(model = object,par = "ar")
  if(object$q  > 0 )get_prior(model = object,par = "ma")

  if(object$s != 0 || object$k != 0 || object$h != 0 ){
    cat("\n Bekk Parameters: \n")
    if(object$s  > 0 )get_prior(model = object,par = "arch")
    if(object$k  > 0 )get_prior(model = object,par = "garch")
    if(object$h  > 0 )get_prior(model = object,par = "mgarch")
  }
  if(object$genT){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ inverse.gamma(v/2,v/2) \n")
    get_prior(object,par = "dfv")
  }
}
