#' Draw from posterior predictive h steps ahead distribution
#'
#' The posterior predictive distribution is the distribution of the outcome
#' implied by the model after using the observed data to update our beliefs
#' about the unknown parameters in the model. Simulating data from the posterior
#' predictive distribution using the observed predictors is useful for checking
#' the fit of the model. Drawing from the posterior predictive distribution at
#' interesting values of the predictors also lets us visualize how a
#' manipulation of a predictor affects (a function of) the outcome(s). With new
#' observations of predictor variables we can use the posterior predictive
#' distribution to generate predicted outcomes.
#'
#' @usage  posterior_predict(obj)
#'
#' @param obj: a varstan object
#' @param h: An integer indicating the number of predictions. The default number
#'    of predictions is 1.
#' @param robust: A boolean for obtain the robust estimation. The default
#' @param draws: An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed: An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#'@export
#'
#' @return  A \code{draws} by \code{h} data.frame of simulations from the
#'   posterior predictive distribution. Each row of the data.frame is a vector of
#'   predictions generated using a single draw of the model parameters from the
#'   posterior distribution.
#'
posterior_predict = function(obj,...){
  UseMethod("posterior_predict")
}
#' @method posterior_predict varstan
#' @export posterior_predict
#' @export
#'
posterior_predict.varstan = function(obj,h = 1,robust = TRUE,draws = 1000,seed = NULL){

  if (! is.varstan(obj))
    stop("The current object is not a varstan class",call. = FALSE)

  if(is.arima(obj$model))
    fc = posterior_predict_garch(obj = obj,h = h,robust = robust,draws = draws,seed = seed)

  if(is.garch(obj$model))
    fc = posterior_predict_garch(obj = obj,h = h,robust = robust,draws = draws,seed = seed)

  if(is.varma(obj$model))
    fc = posterior_predict_varma(obj = obj,h = h,robust = robust,draws = draws,seed = seed)

  return(fc)
}
#' Draw from posterior predictive distribution of an arima-garch model
#'
#' @usage  posterior_predict_garch(obj)
#'
#' @param obj: a varstan object
#' @param h: An integer indicating the number of predictions. The default number
#'    of predictions is 1.
#' @param robust: A boolean for obtain the robust estimation. The default
#' @param draws: An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed: An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#'
#' @return  A \code{draws} by \code{h} data.frame of simulations from the
#'   posterior predictive distribution. Each row of the data.frame is a vector of
#'   predictions generated using a single draw of the model parameters from the
#'   posterior distribution.
#'
posterior_predict_garch = function(obj,h = 1,robust = TRUE,draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  # Extract the necessary lags for predict
  order = get_order(obj);n = obj$model$n;n1 = max_order(obj);

  # Extract the posterior values
  sigma =   extract_ts(obj,"sigma",lag = n1,robust = robust)
  epsilon = extract_ts(obj,"epsilon",lag = n1,robust = robust)

  # point etimate of the model parameters
  pe = posterior_estimate(obj,robust = robust)
  # The previous data
  y1 = obj$model$y[(n-n1+1):n];
  yh = matrix(,nrow = draws,ncol = h)

  for (i in 1:h){

    mu = pe$mu0;
    sigma[n1+i] = pe$sigma0;

    #  ar factor
    if(order$p > 0) for(j in 1:order$p) mu=mu+y1[n1+i-j]*pe$phi[j];

    # ma factor
    if(order$q > 0) for(j in 1:order$q) mu=mu+epsilon[n1+i-j]*pe$theta[j];

    # arch factor
    if(order$s > 0) for(j in 1:order$s)sigma[n1+i]=sigma[n1+i]+pe$alpha[j]*epsilon[n1+i-j]^2;

    # garch factor
    if(order$k > 0) for(j in 1:order$k)sigma[n1+i]=sigma[n1+i]+pe$beta[j]*sigma[n1+i-j]^2;

    # Standard desviation
    sigma[n1+i] = sqrt(sigma[n1+i]);

    # mgarch factor
    if(order$h > 0) for(j in 1:order$h)mu=mu+pe$mgarch[n1+i]*sigma[n1+i-j];

    # posterior predict draws
    yh[,i] =rnorm(draws,mu,sigma[n1+i]);

    # posterior estimate
    if(robust == TRUE)y1=c(y1, median(yh[,i]))
    else y1=c(y1, mean(yh[,i]))
    epsilon[n1+i] = y1[n1+i] - mu;
  }
  colnames(yh) = paste0("yh.",1:h)
  yh = as.data.frame(yh)
  return(yh);
}
#' Draw from posterior predictive distribution of a tvarma model
#'
#' @usage  posterior_predict_varma(obj)
#'
#' @param obj: a varstan object
#' @param h: An integer indicating the number of predictions. The default number
#'    of predictions is 1.
#' @param robust: A boolean for obtain the robust estimation. The default
#' @param draws: An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed: An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#'
#' @return  A \code{draws} by \code{h} data.frame of simulations from the
#'   posterior predictive distribution. Each row of the data.frame is a vector of
#'   predictions generated using a single draw of the model parameters from the
#'   posterior distribution.
#'
posterior_predict_varma = function(obj,h = 1,robust = TRUE,draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  order = get_order(obj)
  d = obj$model$d;n1 = max_order(obj);n = obj$model$n

  # Extract the posterior values
  epsilon = extract_mts(obj=obj,par="epsilon",lag=n1,d =d,robust=robust)

  sigma =   extract_mts(obj,"sigma",lag=n1,d = d^2,robust=robust)
  sigma = vector_to_matrix(sigma,d=d,p=n1)

  #  Generalized t student covariance matrix
  if(obj$model$genT){
    lambda = extract_mts(obj = obj,par = "lambda",lag=1,d=d,robust=robust)
    lambda = diag(as.numeric(lambda)^(-0.5))
    for (i in 1:n1) sigma[[i]] = lambda%*%sigma[[i]]%*%lambda
  }

  # point etimate of the model parameters
  pe = posterior_estimate(obj,robust = robust)

  # The previous data
  y1 = t(tail(obj$model$y,1))

  # draw matrix

  yh =list()
  for(i in 1:d) yh[[i]] =  matrix(,nrow = draws,ncol = h)

  for(i in 1:h){

    # mu asignation
    mu = pe$mu0
    sigma[n1+i] = pe$sigma0;

    #  ar factor
    if(order$p > 0) for(j in 1:order$p) mu=mu+y1[,n1+i-j]%*%pe$phi[[j]];

    # ma factor
    if(order$q > 0) for(j in 1:order$q) mu=mu+epsilon[,n1+i-j]%*%pe$theta[[j]];

    # arch factor
    if(order$s > 0){

      for(j in 1:order$s){
        A =  t(pe$alpha[[j]])%*%epsilon[,n1+i-j]
        sigma[[n1+i]]=sigma[[n1+i]]+A%*%t(A)
      }
    }
    # garch factor
    if(order$k > 0){
      for(j in 1:order$k)
        sigma[[n1+i]]=sigma[[n1+i]]+t(pe$beta[[j]])%*%sigma[[n1+i-j]]%*%pe$beta[[j]]
    }

    if(obj$model$genT) sigma[[n1+i]] = lambda%*%sigma[[n1+i]]%*%lambda

    # mgarch factor
    if(order$h > 0) for(j in 1:order$h)mu=mu+vech(sigma[[n1+i-j]])%*%pe$mgarch[[j]];

    # posterior predict draws

    ytemp =MASS::mvrnorm(n=draws,mu = mu,Sigma = sigma[[n1+i]])

    for(j in 1:d) yh[[j]][,i] = ytemp[,j]

    # posterior estimate
    if(robust == TRUE)
      y1 = cbind(y1,apply(ytemp,2,median))
    else
      y1 = cbind(y1,apply(ytemp,2,median))

    # ARMA residual estimation
    epsilon = cbind(epsilon,y1[,n1+i] - t(mu))
  }
  # column name and data.frame format
  for (i in 1:d) colnames(yh[[i]]) = paste0("yh.",1:h,".",i)
  yh = as.data.frame(yh)

  return(yh);
}
