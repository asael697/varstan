#' Print a full report of the time series model in a  varstan object
#'
#' The function returns a report with the users defined model for the given time series data
#' and all the current defined priors of the model
#'
#' @usage  report(obj,...)
#'
#' @aliases report report.varstan report.Sarima report.garch report.varma report.Bekk report.naive
#'
#' @param obj an object varstan object or one of the defined current defined reports in varstan package
#' @param ... additional values need in print methods
#'
#' @details if \code{obj} is a varstan object the function will print the information of the
#' defined model inside of the object. If \code{obj} is one of the model classes (like Sarima or garch)
#' then it will print the report information as well.
#'
#'
#' @author  Asael Alonzo Matamoros
#'
#' @seealso \code{\link{report}} \code{\link{prior}}
#'
#' @export
#'
#' @return  a  string with the defined time series report
#'
#' @examples
#'
#' dat2 = garch(birth,order = c(1,1,0))
#' report(dat2)
#'
#' dat = varma(Astrovan,p=1,q=1)
#' report(dat)
#'
#'
report <- function(obj,...) {
  UseMethod("report")
}
#'
#' @method report varstan
#' @export
#'
report.varstan = function(obj){
  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  if( is.Sarima(obj$model)) report.Sarima(obj$model)
  if( is.naive(obj$model))  report.naive(obj$model)
  if( is.garch(obj$model))  report.garch(obj$model)
  if( is.SVM(obj$model))    report.SVM(obj$model)
  if( is.varma(obj$model))  report.varma(obj$model)
  if( is.Bekk(obj$model))   report.Bekk(obj$model)
}
#'
#' @method report Sarima
#' @export
#'
report.Sarima = function(obj,...){
  if( !is.Sarima(obj))
    stop("The object is not a Sarima model \n")

  model.Sarima(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n")
  if(obj$p  > 0 )get_prior(model = obj,par = "ar")
  if(obj$q  > 0 )get_prior(model = obj,par = "ma")
  if(obj$P != 0 || obj$Q != 0 || obj$D != 0){
    cat("\n Seasonal Parameters: \n")
    if(obj$P  > 0 )get_prior(model = obj,par = "sar")
    if(obj$Q  > 0 )get_prior(model = obj,par = "sma")
  }
  if(obj$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = obj,par = "breg")
  }
}
#'
#' @method report naive
#' @export
#'
report.naive = function(obj,...){
  if( !is.naive(obj))
    stop("The object is not a naive model \n")

  model.naive(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n")
  if(obj$D > 0 ){
    cat("\n period:",obj$period,"\n")
  }
}
#'
#' @method report garch
#' @export
#'
report.garch = function(obj,...){
  if(!is.garch(obj))
    stop("The object is not a garch model \n")

  model.garch(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n")
  if(obj$s  > 0 )get_prior(model = obj,par = "arch")
  if(obj$k  > 0 )get_prior(model = obj,par = "garch")
  if(obj$h  > 0 )get_prior(model = obj,par = "mgarch")
  if(obj$p != 0 || obj$q != 0 ){
    cat("\n mean Parameters: \n")
    if(obj$p  > 0 )get_prior(model = obj,par = "ar")
    if(obj$q  > 0 )get_prior(model = obj,par = "ma")
  }
  if(obj$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = obj,par = "breg")
  }
  if(obj$genT == TRUE){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ G(v/2,v/2) \n")
    get_prior(obj,par = "dfv")
  }
}
#'
#' @method report SVM
#' @export
#'
report.SVM = function(obj,...){
  if(!is.SVM(obj))
    stop("The object is not a stochastic volatility model \n")

  model.SVM(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n log Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n")
  get_prior(model = obj,par = "alpha")
  get_prior(model = obj,par = "beta")
  if(obj$p != 0 || obj$q != 0 ){
    cat("\n mean Parameters: \n")
    if(obj$p  > 0 )get_prior(model = obj,par = "ar")
    if(obj$q  > 0 )get_prior(model = obj,par = "ma")
  }
  if(obj$d1 > 0 ){
    cat("\n Regression Parameters: \n")
    get_prior(model = obj,par = "breg")
  }
}
#'
#' @method report varma
#' @export
#'
report.varma = function(obj,...){
  if(!is.varma(obj))
    stop("The object is not a varma model \n")

  model.varma(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n mean Parameters: \n")
  if(obj$p  > 0 )get_prior(model = obj,par = "ar")
  if(obj$q  > 0 )get_prior(model = obj,par = "ma")

  if(obj$s != 0 || obj$k != 0 || obj$h != 0 ){
    cat("\n Bekk Parameters: \n")
    if(obj$s  > 0 )get_prior(model = obj,par = "arch")
    if(obj$k  > 0 )get_prior(model = obj,par = "garch")
    if(obj$h  > 0 )get_prior(model = obj,par = "mgarch")
  }
  if(obj$genT == TRUE){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ inverse.gamma(v/2,v/2) \n")
    get_prior(obj,par = "dfv")
  }
}
#'
#' @method report Bekk
#' @export
#'
report.Bekk = function(obj,...){
  if(!is.Bekk(obj))
    stop("The object is not a Bekk model \n")

  model.Bekk(obj)
  cat("Priors: \n Intercept:\n")
  get_prior(obj,par = "mu0")
  cat("\n Scale Parameter: \n")
  get_prior(obj,par = "sigma0")
  cat("\n mean Parameters: \n")
  if(obj$p  > 0 )get_prior(model = obj,par = "ar")
  if(obj$q  > 0 )get_prior(model = obj,par = "ma")

  if(obj$s != 0 || obj$k != 0 || obj$h != 0 ){
    cat("\n Bekk Parameters: \n")
    if(obj$s  > 0 )get_prior(model = obj,par = "arch")
    if(obj$k  > 0 )get_prior(model = obj,par = "garch")
    if(obj$h  > 0 )get_prior(model = obj,par = "mgarch")
  }
  if(obj$genT == TRUE){
    cat("\n Generalized t-student \n")
    cat("\n lambda ~ inverse.gamma(v/2,v/2) \n")
    get_prior(obj,par = "dfv")
  }
}
