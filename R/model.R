#' Print the defined model of a varstan object
#'
#' The function returns a string with the users defined model for the given time series data
#'
#' @usage  model(obj)
#'
#' @param obj a varstan object or one of the defined current defined models in varstan package
#'
#' @details if \code{obj} is a varstan object the function will print the information of the
#' defined model inside of the object. If \code{obj} is one of the model classes (like Sarima or garch)
#' then it will print the model information as well.
#'
#' For full information of the model with the used priors use the function report or just
#' print the object
#'
#' @author  Asael Alonzo Matamoros
#'
#' @seealso \code{report} \code{print}
#'
#' @aliases model model.varstan model.Sarima model.garch model.varma model.Bekk model.SVM
#'
#'
#' @export
#'
#' @return  a  string with the defined time series model
#'
#' @examples
#'
#'
#' model1 = Sarima(birth,order = c(0,1,2),seasonal = c(1,1,1))
#' model(model1)
#'
#'
model <- function(obj,...) {
  UseMethod("model")
}
#'
#' @method  model varstan
#' @export model
#' @export
#'
model.varstan = function(obj,...){
  if(!is.varstan(obj))
    stop("The current object is not a varstan class")

  if( is.Sarima(obj$model)) model.Sarima(obj$model)
  if( is.naive(obj$model))  model.naive(obj$model)
  if( is.garch(obj$model))  model.garch(obj$model)
  if( is.SVM(obj$model))    model.SVM(obj$model)
  if( is.varma(obj$model))  model.varma(obj$model)
}
#'
#' @method  model Sarima
#' @export model
#' @export
#'
model.Sarima = function(obj,...){
  if( !is.Sarima(obj))
    stop("The object is not a Sarima model \n")

  cat("\n")
  log = paste0("y ~ Sarima(",obj$p,",",obj$d,",",obj$q,")")

  if(obj$P != 0 || obj$Q != 0 || obj$D != 0)
    log = paste0(log,"(",obj$P,",",obj$D,",",obj$Q,")[",obj$period,"]")

  if(obj$d1 > 0) log = paste0(log,".reg[",obj$d1,"]")

  cat(log,"\n")
  cat(obj$n,"observations and 1 dimension \n")
  cat("Differences:",obj$d,"seasonal Differences:",obj$D,"\n")
  cat("Current observations:",obj$n1,"\n \n")
}
#'
#' @method  model naive
#' @export model
#' @export
#'
model.naive = function(obj,...){
  if( !is.naive(obj))
    stop("The object is not a naive model \n")

  cat("\n")
  log = paste0("y ~ Sarima(",obj$p,",",obj$d,",",obj$q,")")

  if(obj$d == 0)
    log = paste0("y ~ Random Walk()")
  else
    log = paste0("y ~ Random Walk(",obj$period,")")

  cat(log,"\n")
  cat(obj$n,"observations and 1 dimension \n")
  cat("Differences:",obj$d,"seasonal Diferences:",obj$D,"\n")
  cat("Current observations:",obj$n1,"\n \n")
}
#'
#' @method  model garch
#' @export model
#' @export
#'
model.garch = function(obj,...){
  if(!is.garch(obj))
    stop("The object is not a garch model \n")

  cat("\n")
  if(obj$p != 0 || obj$q != 0){
    log = paste0("y ~ arma(",obj$p,",",obj$q,")")
    log = paste0(log,"+garch(",obj$s,",",obj$k,",",obj$h,")")
  }
  else log = paste0("y ~ garch(",obj$s,",",obj$k,",",obj$h,")")

  if(obj$d1 > 0) log = paste0(log,".reg[",obj$d1,"]")
  cat(log,"\n")
  if(obj$genT) cat("Generalized t-student model \n")
  cat(obj$n,"observations and 1 dimension \n \n")
}
#' @method  model SVM
#' @export model
#' @export
#'
model.SVM = function(obj,...){
  if(!is.SVM(obj))
    stop("The object is not a garch model \n")

  cat("\n")
  if(obj$p != 0 || obj$q != 0){
    log = paste0("y ~ arma(",obj$p,",",obj$q,")")
    log = paste0(log,"+SVM")
  }
  else log = paste0("y ~ SVM")

  if(obj$d1 > 0) log = paste0(log,".reg[",obj$d1,"]")
  cat(log,"\n")
  cat(obj$n,"observations and 1 dimension \n \n")
}
#'
#' @method  model varma
#' @export model
#' @export
#'
model.varma = function(obj,...){
  if(!is.varma(obj))
    stop("The object is not a varma model \n")

  cat("\n")
  log = paste0("y ~ varma(",obj$p,",",obj$q,")")
  if(obj$s != 0 || obj$k != 0 || obj$h != 0){
    log = paste0(log,"+mbekk(",obj$s,",",obj$k,",",obj$h,")")
  }
  cat(log,"\n")
  if(obj$genT) cat("Generalized t-student model \n")
  cat(obj$n,"observations and",obj$d ,"dimensions \n  \n")
}
#'
#' @method  model Bekk
#' @export model
#' @export
#'
model.Bekk = function(obj,...){
  if(!is.Bekk(obj))
    stop("The object is not a Bekk model \n")

  cat("\n")
  log = paste0("y ~ Bekk(",obj$s,",",obj$k,",",obj$h,")")
  if(obj$p != 0 || obj$q != 0){
    log = paste0(log,"+varma(",obj$p,",",obj$q,")")
  }
  cat(log,"\n")
  if(obj$genT) cat("Generalized t-student model \n")
  cat(obj$n,"observations and",obj$d ,"dimensions \n  \n")
}
