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
report.varstan = function(obj,...){
  if(is.varstan(obj)){
    if( is.Sarima(obj$model)) report.Sarima(obj$model)
    if( is.naive(obj$model))  report.naive(obj$model)
    if( is.garch(obj$model))  report.garch(obj$model)
    if( is.varma(obj$model))  report.varma(obj$model)
    if( is.Bekk(obj$model))   report.Bekk(obj$model)
  }
  else cat("The current object is not a varstan class")
}
#'
#' @method report Sarima
#' @export
#'
report.Sarima = function(obj,...){
  if( is.Sarima(obj)){
    model.Sarima(obj)
    cat("Priors: \n Intercept:\n")
    get_prior(obj,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(obj,type = "sigma0")
    cat("\n")
    if(obj$p  > 0 )get_prior(dat = obj,type = "ar")
    if(obj$q  > 0 )get_prior(dat = obj,type = "ma")
    if(obj$P != 0 || obj$Q != 0 || obj$D != 0){
      cat("\n Seasonal Parameters: \n")
      if(obj$P  > 0 )get_prior(dat = obj,type = "sar")
      if(obj$Q  > 0 )get_prior(dat = obj,type = "sma")
    }
    if(obj$d1 > 0 ){
      cat("\n Regression Parameters: \n")
      get_prior(dat = obj,type = "breg")
    }
  }
  else cat("The object is not a Sarima model \n")
}
#'
#' @method report naive
#' @export
#'
report.naive = function(obj,...){
  if( is.naive(obj)){
    model.naive(obj)
    cat("Priors: \n Intercept:\n")
    get_prior(obj,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(obj,type = "sigma0")
    cat("\n")
    if(obj$D > 0 ){
      cat("\n period:",obj$period,"\n")
    }
  }
  else cat("The object is not a naive model \n")
}
#'
#' @method report garch
#' @export
#'
report.garch = function(obj,...){
  if(is.garch(obj)){
    model.garch(obj)
    cat("Priors: \n Intercept:\n")
    get_prior(obj,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(obj,type = "sigma0")
    cat("\n")
    if(obj$s  > 0 )get_prior(dat = obj,type = "arch")
    if(obj$k  > 0 )get_prior(dat = obj,type = "garch")
    if(obj$h  > 0 )get_prior(dat = obj,type = "mgarch")
    if(obj$p != 0 || obj$q != 0 ){
      cat("\n mean Parameters: \n")
      if(obj$p  > 0 )get_prior(dat = obj,type = "ar")
      if(obj$q  > 0 )get_prior(dat = obj,type = "ma")
    }
    if(obj$d1 > 0 ){
      cat("\n Regression Parameters: \n")
      get_prior(dat = obj,type = "breg")
    }
    if(obj$genT == TRUE){
      cat("\n Generalized t-student \n")
      cat("\n lambda ~ G(v/2,v/2) \n")
      get_prior(obj,type = "dfv")
    }
  }
  else cat("The object is not a garch model \n")
}
#'
#' @method report varma
#' @export
#'
report.varma = function(obj,...){
  if(is.varma(obj)){
    model.varma(obj)
    cat("Priors: \n Intercept:\n")
    get_prior(obj,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(obj,type = "sigma0")
    cat("\n mean Parameters: \n")
    if(obj$p  > 0 )get_prior(dat = obj,type = "ar")
    if(obj$q  > 0 )get_prior(dat = obj,type = "ma")

    if(obj$s != 0 || obj$k != 0 || obj$h != 0 ){
      cat("\n Bekk Parameters: \n")
      if(obj$s  > 0 )get_prior(dat = obj,type = "arch")
      if(obj$k  > 0 )get_prior(dat = obj,type = "garch")
      if(obj$h  > 0 )get_prior(dat = obj,type = "mgarch")
    }
    if(obj$genT == TRUE){
      cat("\n Generalized t-student \n")
      cat("\n lambda ~ G(v/2,v/2) \n")
      get_prior(obj,type = "dfv")
    }
  }
  else cat("The object is not a varma model \n")
}
#'
#' @method report Bekk
#' @export
#'
report.Bekk = function(obj,...){
  if(is.Bekk(obj)){
    model.Bekk(obj)
    cat("Priors: \n Intercept:\n")
    get_prior(obj,type = "mu0")
    cat("\n Scale Parameter: \n")
    get_prior(obj,type = "sigma0")
    cat("\n mean Parameters: \n")
    if(obj$p  > 0 )get_prior(dat = obj,type = "ar")
    if(obj$q  > 0 )get_prior(dat = obj,type = "ma")

    if(obj$s != 0 || obj$k != 0 || obj$h != 0 ){
      cat("\n Bekk Parameters: \n")
      if(obj$s  > 0 )get_prior(dat = obj,type = "arch")
      if(obj$k  > 0 )get_prior(dat = obj,type = "garch")
      if(obj$h  > 0 )get_prior(dat = obj,type = "mgarch")
    }
    if(obj$genT == TRUE){
      cat("\n Generalized t-student \n")
      cat("\n lambda ~ G(v/2,v/2) \n")
      get_prior(obj,type = "dfv")
    }
  }
  else cat("The object is not a Bekk model \n")
}
