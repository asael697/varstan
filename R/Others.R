#' print the current available models in the varstan package version
#'
#' The functions prints the name,version, algorithm and current models implemented in
#' the varstan package
#'
#'
#' @usage version(View = FALSE)
#'
#' @param View a boolean value, by default is False, if \code{View} is TRUE then
#' a data frame with the current models will visualize in the RStudio console
#'
#'
#' @author Asael Alonzo Matamoros.
#'
#' @export
#'
#' @return a string
#'
#' @examples
#'  version()
#'  parameters(classes = "Sarima")
#'  distribution(par = "ar")
#'
version = function(View = FALSE){
  m1 = c("SARIMA",
         "ARIMAX",
         "mGARCH",
         "VARMA",
         "Bekk",
         "Harmonic ARIMAX",
         "Random-walk",
         "Stochastic Volatility model",
         "Asymmetric-GARCH")
  f1 = c("Sarima(ts,order = c(p,d,q), seasonal = c(P,D,Q) )",
         "Sarima(ts,order = c(p,d,q), xreg != NULL )",
         "garch(ts,order=c(s,k,h),arma = c(p,q),xreg != NULL)",
         "varma(mts,order=c(p,q),bekk = c(s,k,h) )",
         "Bekk(mts,order=c(s,k,h),varma = c(p,q) )",
         "Sarima(ts,order = c(p,d,q), xreg= fourier(ts,K) )",
         "naive(ts,seasonal = FALSE)","SVM(ts,arma = c(p,q),xreg != NULL )",
         "garch(ts,order=c(s,k,h),arma = c(p,q),xreg != NULL,asym ='logit')")
  df = data.frame(model = m1,functions = f1)
  if(View){
    View(df)
  }
  else{
    cat("package: varstan \n")
    cat("version: 1.0.1.000 \n")
    cat("Algorithm: Stan-NUTS \n")
    cat("Current classes: varstan, Sarima, garch, varma, Bekk,SVM \n")
    cat("Current models: \n")
    print(df)
    cat("\n * model column represent the available model \n")
    cat(" * functions column represent the function structure \n")
    cat(" * GenT column represent if the model admits a generalized t-student distribution \n")
    cat(" * Report a bug in asael_am@hotmail.com \n \n")
  }
}
#' Print the  available prior distribution for the model parameters
#'
#' The functions prints the available distributions for the model
#' parameters in \pkg{varstan}.
#'
#' @usage distribution(par)
#'
#' @param par a string value with the desired parameter, if not know the parameter
#' use the function \code{parameters} with the model class.
#'
#' @author Asael Alonzo Matamoros
#'
#' @export
#'
#' @return a string with the prior distributions for the desired parameter
#'
#' @seealso \code{parameters}, \code{version}
#'
#' @examples
#'  version()
#'  parameters(classes = "Sarima")
#'  distribution(par = "ar")
#'
distribution = function(par){

  param = c("ar","ma","sar","sma","arch","garch","dfv","sigma0","mu0","breg","mgarch","gamma")

  if( !(par %in% param) )
    stop("Invalid par argument")

  if(par %in% c("ar","ma","sar","sma","arch","garch") ){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ normal(loc,sd) \n")
    cat(par,"~ beta(shape1,shape2) *DONT USE In VARMA MODELS \n")
    cat(par,"~ Uniform(min,max)   *DONT USE In VARMA MODELS \n" )
  }
  if(par %in% c("mu0","breg","mgarch","sigma0","dfv","df","alpha","gamma") ){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ normal(loc,sd) \n")
    cat(par,"~ beta(shape1,shape2) *DONT USE In VARMA MODELS \n")
    cat(par,"~ Uniform(min,max)   *DONT USE In VARMA MODELS \n" )
    cat(par,"~ student(mu,sd,df) \n")
    cat(par,"~ cauchy(mu,sd) \n")
    cat(par,"~ inverse.gamma(shape,rate) \n")
    cat(par,"~ inverse.chisq(df) \n")
    cat(par,"~ jeffrey() \n")
    cat(par,"~ gamma(shape,rate) \n")
    cat(par,"~ exponential(rate) \n")
    cat(par,"~ chisq(df) \n")
    cat(par,"~ laplace(mu,sd) \n")
  }
  if(identical(par,"lkj")){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ lkj(df) \n")
  }
}
#' Print the parameters of a model class
#'
#' The functions prints the parameters in one of the defined model classes in varstan.
#'
#' @usage parameters(classes = NULL)
#'
#' @param classes a string value with the desyred model class (Sarima,garch,varma,...),
#' for knowing the current available model classes use the \code{version()} function.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#' @return  a string with the prior distributions for the desired parameter
#'
#' @seealso  \code{parameters}, \code{version}
#'
#' @examples
#'  version()
#'  parameters(classes = "Sarima")
#'  distribution(par = "ar")
#'
parameters = function(classes = NULL){

  x = c("ar","ma","sar","sma","arch","garch","mgarch","mu0","breg","sigma0","dfv","alpha","beta","gamma")

  if(is.null(classes)){
    cat("All the current parameters are: \n")
    print(x)
  }
  else{
    if(classes == "Sarima"){
      cat(classes,"parameters are: \n")
      print(c("mu0","sigma0","ar","ma","sar","sma","breg"))
    }
    if(classes == "garch" || classes =="varma" || classes =="Bekk"){
      cat(classes,"parameters are: \n")
      print(c("mu0","sigma0","ar","ma","arch","garch","mgarch","dfv","gamma"))
    }
    if(classes == "naive"){
      cat(classes,"parameters are: \n")
      print(c("mu0","sigma0"))
    }
    if(classes == "SVM"){
      cat(classes,"parameters are: \n")
      print(c("mu0","sigma0","ar","ma","breg","alpha","beta"))
    }
  }
}
