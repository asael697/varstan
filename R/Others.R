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
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a string
#'
#' @examples
#'  version()
#'  parameters(classes = "Sarima")
#'  distribution(par = "ar")
#'
version = function(View = FALSE){
  m1 = c("Seasonal arima","Dynamic regression","arma-mgarch","varma-mbekk","Bekk",
         "Dynamic Harmonic Regression","Random-walk")
  f1 = c( "Sarima(order = c(p,d,q), seasonal = c(P,D,Q) )",
          "Sarima(order = c(p,d,q), xreg != NULL )",
          "garch(order=c(s,k,h),arma = c(p,q) )",
          "varma(order=c(p,q),bekk = c(s,k,h) )",
          "Bekk(order=c(s,k,h),varma = c(p,q) )",
          "Sarima(order = c(p,d,q), xreg= fourier(ts,K) )",
          "naive(seasonal = FALSE)")
  G1 = c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE)
  df = data.frame(model = m1,functions = f1,GenT = G1)
  if(View){
    View(df)
  }
  else{
    cat("package: varstan \n")
    cat("version: 1.0.0.000 \n")
    cat("Algorithm: Stan-NUTS \n")
    cat("Current classes: varstan, Sarima, garch, varma, Bekk,DWR \n")
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
#' parameters in varstan
#'
#'
#' @usage distribution(par)
#'
#' @param par a string value with the desired parameter, if not know the parameter
#' use the function \code{parameters} with the model class
#'
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#' @return  a string with the prior distributions for the desired parameter
#'
#' @seealso  \code{parameters}, \code{version}
#'
#' @examples
#'  version()
#'  parameters(classes = "Sarima")
#'  distribution(par = "ar")
#'
distribution = function(par){
  if(par %in% c("ar","ma","sar","sma","arch","garch") ){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ normal(loc,scl) \n")
    cat(par,"~ beta(par1,par2) *DONT USE In VARMA MODELS \n")
    cat(par,"~ Uniform(-1,1)   *DONT USE In VARMA MODELS \n" )
  }
  if(par %in% c("mu0","breg","mgarch") ){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ normal(loc,scl) \n")
    cat(par,"~ cauchy(loc,scl) \n")
    cat(par,"~ beta(par1,par2) \n")
    cat(par,"~ Uniform(-1,1)   \n" )
    cat(par,"~ gamma(par1,par2) \n")
    cat(par,"~ t-student(loc,scl,df) \n")
  }
  if(par == "sigma0"){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ chi_square(df) \n")
    cat(par,"~ gamma(par1,par2) \n")
    cat(par,"~ inv_chi_square(df) \n")
    cat(par,"~ half normal(loc,scl) \n")
    cat(par,"~ half cauchy(loc,scl) \n")
    cat(par,"~ inv_gamma(par1,par2) \n")
    cat(par,"~ half t-student(loc,scl,df) \n")
  }
  if(par == "dfv"){
    cat("\nThe available prior  distribution for the ",par," parameter are: \n")
    cat(par,"~ jeffrey() \n")
    cat(par,"~ normal(loc,scl) \n")
    cat(par,"~ gamma(par1,par2) \n")
    cat(par,"~ inv_gamma(par1,par2) \n")
  }
}
#' Print the parameters of a model class
#'
#' The functions prints the parameters in one of the defined model classes in varstan
#'
#' @usage parameters(classes = NULL)
#'
#' @param classes a string value with the desyred model class (Sarima,garch,varma,...),
#' for knowing the current available model classes use the \code{version()} function
#'
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
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

  x = c("ar","ma","sar","sma","arch","garch","mgarch","mu0","breg",
        "sigma0","dfv")

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
      print(c("mu0","sigma0","ar","ma","arch","garch","mgarch","dfv"))
    }
    if(classes == "naive"){
      cat(classes,"parameters are: \n")
      print(c("mu0","sigma0"))
    }
  }
}
