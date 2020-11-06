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
#' @aliases posterior_predict
#'
#' @param object a varstan object
#' @param h An integer indicating the number of predictions. The default number
#'    of predictions is 12.
#' @param xreg	Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as ts. It should not be a data frame.
#' @param robust A boolean for obtain the robust estimation. The default
#' @param draws An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed An optional \code{\link[=set.seed]{seed}} to use.
#' @param ... Further arguments passed to  \code{posterior_predict}.
#'
#' @author Asael Alonzo Matamoros
#'
#' @return
#' A \code{draws} by \code{h} data.frame of simulations from the
#' posterior predictive distribution. Each row of the data.frame
#' is a vector of predictions generated using a single draw of
#' the model parameters from the posterior distribution.
#'
#' @importFrom stats arima predict
#' @importFrom MASS mvrnorm
#'
#' @importFrom rstantools posterior_predict
#' @method posterior_predict varstan
#' @export
#' @export posterior_predict
#'
posterior_predict.varstan = function(object,h = 12,xreg = NULL,robust = FALSE,
                                     draws = 1000,seed = NULL,...){

  if (! is.varstan(object))
    stop("The current object is not a varstan class",call. = FALSE)

  if(is.garch(object$model))
    fc = posterior_predict_garch(object = object,h = h,xreg = xreg,robust = robust,draws = draws,seed = seed)

  if(is.SVM(object$model) )
    fc = posterior_predict_SVM(object = object,h = h,xreg = xreg,robust = robust,draws = draws,seed = seed)

  if(is.varma(object$model))
    fc = posterior_predict_varma(object = object,h = h,robust = robust,draws = draws,seed = seed)

  if(is.Bekk(object$model))
    fc = posterior_predict_varma(object = object,h = h,robust = robust,draws = draws,seed = seed)

  if(is.Sarima(object$model))
    fc = posterior_predict_Sarima(object = object,h = h,xreg = xreg,robust = robust,draws = draws,seed = seed)

  if(is.naive(object$model))
    fc = posterior_predict_Sarima(object = object,h = h,xreg = xreg,robust = robust,draws = draws,seed = seed)

  return(fc)
}
#' Draw from posterior predictive distribution of a tvarma model
#'
#' @usage  posterior_predict_varma(object,h = 1,robust = TRUE,draws = 1000,seed = NULL)
#'
#' @param object a varstan object
#' @param h An integer indicating the number of predictions. The default number
#'    of predictions is 1.
#' @param robust A boolean for obtain the robust estimation. The default
#' @param draws An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#'
#' @return
#' A \code{draws} by \code{h} data.frame of simulations from the
#' posterior predictive distribution. Each row of the data.frame is a vector of
#' predictions generated using a single draw of the model parameters from the
#' posterior distribution.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats median
#' @noRd
#'
posterior_predict_varma = function(object,h = 1,robust = TRUE,draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  order = get_order(object)
  d = object$model$dimension;n1 = max_order(object);n = object$model$n

  # Extract the posterior values
  epsilon = extract_mts(object = object,par = "epsilon",lag = n1,d = d,robust = robust)

  sigma =   extract_mts(object,"sigma",lag = n1,d = d^2,robust = robust)
  sigma = vector_to_matrix(sigma,d = d,p = n1)

  #  Generalized t student covariance matrix
  if(object$model$genT){
    lambda = extract_mts(object = object,par = "lambda",lag = 1,d = d,robust = robust)
    lambda = diag(as.numeric(lambda)^(-0.5))
    for (i in 1:n1) sigma[[i]] = lambda%*%sigma[[i]]%*%lambda
  }

  # point etimate of the model parameters
  pe = posterior_estimate(object,robust = robust)

  # The previous data
  y1 = t(utils::tail(object$model$y,1))

  # draw matrix

  yh =list()
  for(i in 1:d) yh[[i]] =  matrix(0,nrow = draws,ncol = h)

  for(i in 1:h){

    # mu asignation
    mu = pe$mu0
    sigma[n1+i] = pe$sigma0;

    #  ar factor
    if(order$p > 0) for(j in 1:order$p) mu = mu + y1[,n1+i-j]%*%pe$phi[[j]];

    # ma factor
    if(order$q > 0) for(j in 1:order$q) mu = mu - epsilon[,n1+i-j]%*%pe$theta[[j]];

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

    if(object$model$genT) sigma[[n1+i]] = lambda%*%sigma[[n1+i]]%*%lambda

    # mgarch factor
    if(order$h > 0) for(j in 1:order$h) mu = mu +vech(sigma[[n1+i-j]])%*%pe$mgarch[[j]];

    # posterior predict draws

    ytemp = MASS::mvrnorm(n=draws,mu = mu,Sigma = sigma[[n1+i]])

    for(j in 1:d) yh[[j]][,i] = ytemp[,j]

    # posterior estimate
    if(robust == TRUE)
      y1 = cbind(y1,apply(ytemp,2,stats::median))
    else
      y1 = cbind(y1,apply(ytemp,2,stats::median))

    # ARMA residual estimation
    epsilon = cbind(epsilon,y1[,n1+i] - t(mu))
  }
  # column name and data.frame format
  for (i in 1:d) colnames(yh[[i]]) = paste0("yh.",1:h,".",i)
  yh = as.data.frame(yh)

  return(yh);
}
#' Draw from posterior predictive distribution of an Seasonal arima model
#'
#' @usage  posterior_predict_Sarima(object,h = 1,robust = TRUE,draws = 1000,seed = NULL)
#'
#' @param object a varstan object
#' @param h An integer indicating the number of predictions. The default number
#'    of predictions is 1.
#' @param xreg Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as h. It should not be a data frame.
#' @param robust A boolean for obtain the robust estimation. The default
#' @param draws An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @return
#' A \code{draws} by \code{h} data.frame of simulations from the
#' posterior predictive distribution. Each row of the data.frame is a vector of
#' predictions generated using a single draw of the model parameters from the
#' posterior distribution.
#'
#' @importFrom stats arima predict rnorm
#' @noRd
#'
posterior_predict_Sarima = function(object,h = 1,xreg = NULL,robust = TRUE,
                                    draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  nm = object$stan.parmaters$chains*object$stan.parmaters$iter-object$stan.parmaters$warmup
  draw = draws
  if( nm < draws) draw = nm

  sp = sample(1:nm,size = draw)

  # Extract the necessary lags for predict
  order = get_order(object);
  fix = NULL
  if(order$p > 0) fix =c(fix,"phi")
  if(order$q > 0) fix =c(fix,"theta")
  if(order$P > 0) fix =c(fix,"sphi")
  if(order$Q > 0) fix =c(fix,"stheta")
  if(order$d1 > 0)fix =c(fix,"breg")
  y = object$ts;

  # point etimate of the model parameters
  par0 = data.frame(extract_stan(object,pars = c("mu0","sigma0")))
  fix = data.frame(extract_stan(object,pars = fix))
  par0 = par0[sp,]
  fix = fix[sp,]


  # The previous data
  yh = matrix(0,nrow = draw,ncol = h)
  reg = NULL
  xh = NULL

  #preliminary checks
  if( order$d1 > 0){
    reg = object$model$reg
    # Check xreg
    if(is.null(xreg)){
      warning("No xreg specified, the forecast wont be accurate \n")
      xh  = matrix(0,nrow = h,ncol = order$d1)
    }
    else if( dim(xreg)[1] != h ||  dim(xreg)[2] != order$d1){
      # Check xreg dimensions
      warning("The dimension of xreg are not correct, the forecast wont be accurate \n")
      xh = matrix(0,nrow = h,ncol = order$d1)
    }
    else xh = xreg
  }
  for(i in 1:draw){
    modi = suppressWarnings(stats::arima(x = y,order = c(order$p,order$d,order$q),
                                         seasonal =list(order = c(order$P,order$D,order$Q),period = object$period),
                                         xreg = reg,include.mean = FALSE,
                                         fixed = fix[i,]))
    yh[i,] = suppressWarnings(as.numeric(stats::predict(modi,n.ahead = h,newxreg = xh)$pred))
    for (j in 1:h) yh[i,j] = stats::rnorm(n = 1,mean = yh[i,j] +par0[i,1],sd = par0[i,2])
  }
  colnames(yh) = paste0("yh.",1:h)
  yh = as.data.frame(yh)

  return(yh);
}
#' Draw from posterior predictive distribution of an arma-garch model
#'
#' @usage  posterior_predict_garch(object,h = 1,robust = TRUE,draws = 1000,seed = NULL)
#'
#' @param object a varstan object
#' @param h An integer indicating the number of predictions. The default number
#'    of predictions is 1
#' @param xreg Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as h. It should not be a data frame.
#' @param robust A boolean for obtain the robust estimation. The default
#' @param draws An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author  Asael Alonzo Matamoros
#'
#'
#' @return
#' A \code{draws} by \code{h} data.frame of simulations from the
#' posterior predictive distribution. Each row of the data.frame is a vector of
#' predictions generated using a single draw of the model parameters from the
#' posterior distribution.
#'
#' @importFrom stats arima predict rnorm
#' @noRd
#'
posterior_predict_garch = function(object,h = 1,xreg = NULL,robust = TRUE,
                                   draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  nm = object$stan.parmaters$chains*(object$stan.parmaters$iter-object$stan.parmaters$warmup)
  draw = draws
  if( nm < draws) draw = nm

  sp = sample(1:nm,size = draw)

  # Extract the necessary lags for predict
  order = get_order(object);n = object$series.length

  # Extract the posterior values
  sigma =   extract_ts(object,"sigma",lag = object$model$n,robust = robust)
  sigma = as.numeric(sigma^2)
  y = object$ts;
  reg = NULL
  xh = NULL

  fixmean = NULL
  if(order$p > 0) fixmean =c(fixmean,"phi")
  if(order$q > 0) fixmean =c(fixmean,"theta")
  if(order$d1 > 0)fixmean =c(fixmean,"breg")


  fixsig = NULL
  if(order$s > 0) fixsig =c(fixsig,"alpha")
  if(order$k > 0) fixsig =c(fixsig,"beta")

  fixmgarch = NULL

  # point etimate of the model parameters
  par0 = data.frame(extract_stan(object,pars = c("mu0","sigma0")))
  fixsig = data.frame(extract_stan(object,pars = fixsig))
  fixmean = data.frame(extract_stan(object,pars = fixmean))

  if(order$h > 0){
    fixmgarch = data.frame(extract_stan(object,pars = "mgarch"))
    fixmgarch = data.frame(fixmgarch[sp,])
  }


  fixsig = fixsig[sp,]
  fixmean = fixmean[sp,]
  par0 = par0[sp,]

  #preliminary checks
  if( order$d1 > 0){
    reg = object$model$xreg
    # Check xreg
    if(is.null(xreg)){
      warning("No xreg specified, the forecast wont be accurate \n")
      xh  = matrix(0,nrow = h,ncol = order$d1)
    }
    else if( dim(xreg)[1] != h ||  dim(xreg)[2] != order$d1){
      # Check xreg dimensions
      warning("The dimension of xreg are not correct, the forecast wont be accurate \n")
      xh = matrix(0,nrow = h,ncol = order$d1)
    }
    else xh = xreg
  }

  # The previous data
  yh  =  matrix(0,nrow = draw,ncol = h)
  muh =  matrix(0,nrow = draw,ncol = h)
  sigh = matrix(0,nrow = draw,ncol = h)

  for(i in 1:draw){
    modi = stats::arima(x = y,
                        order = c(order$p,0,order$q),
                        xreg = reg,include.mean = FALSE,
                        fixed = fixmean[i,])

    muh[i,] = as.numeric(stats::predict(modi,n.ahead = h,newxreg = xh)$pred)

    modi = stats::arima(x = sigma,
                        order = c(order$s,0,order$k),
                        include.mean = FALSE,
                        fixed = fixsig[i,])

    sigh[i,] = as.numeric(stats::predict(modi,n.ahead = h)$pred)
    sigh[i,] = sqrt(abs(sigh[i,])+par0[i,2])

    if(order$h > 0){
      for(j in 1:h){
        for(k in 1:order$h){
          if(j-k+1 > 0)
            muh[i,j] = muh[i,j] + fixmgarch[i,k]*sigh[i,j-k+1]
          else
            muh[i,j] = muh[i,j] + fixmgarch[i,k]*sqrt(sigma[n-(j-k+1)])
        }
      }
    }
    for (j in 1:h) yh[i,j] = stats::rnorm(n = 1,mean = muh[i,j]+par0[i,1],sd = sigh[i,j])
  }
  colnames(yh) = paste0("yh.",1:h)
  yh = as.data.frame(yh)
  return(yh)
}
#' Draw from posterior predictive distribution of an arma-SVM model
#'
#' @usage  posterior_predict_SVM(object,h = 1,robust = TRUE,draws = 1000,seed = NULL)
#'
#' @param object a varstan object
#' @param h An integer indicating the number of predictions. The default number
#'    of predictions is 1
#' @param xreg Optionally, a numerical matrix of external regressors,
#' which must have the same number of rows as h. It should not be a data frame.
#' @param robust A boolean for obtain the robust estimation. The default
#' @param draws An integer indicating the number of draws to return. The default
#'    number of draws is 1000
#' @param seed An optional \code{\link[=set.seed]{seed}} to use.
#'
#' @author Asael Alonzo Matamoros
#'
#'
#' @return
#' A \code{draws} by \code{h} data.frame of simulations from the
#' posterior predictive distribution. Each row of the data.frame is a vector of
#' predictions generated using a single draw of the model parameters from the
#' posterior distribution.
#'
#' @importFrom stats arima predict rnorm
#' @noRd
#'
posterior_predict_SVM = function(object,h = 1,xreg = NULL,robust = TRUE,
                                 draws = 1000,seed = NULL){

  if (!is.null(seed))
    set.seed(seed)

  nm = object$stan.parmaters$chains*(object$stan.parmaters$iter-object$stan.parmaters$warmup)
  draw = draws
  if( nm < draws) draw = nm

  sp = sample(1:nm,size = draw)

  # Extract the necessary lags for predict
  order = get_order(object);n = object$series.length

  # Extract the posterior values
  ht =   extract_ts(object,"h",lag = object$model$n,robust = robust)

  y = object$ts;
  reg = NULL
  xh = NULL

  fixmean = NULL
  if(order$p > 0) fixmean =c(fixmean,"phi")
  if(order$q > 0) fixmean =c(fixmean,"theta")
  if(order$d1 > 0)fixmean =c(fixmean,"breg")


  fixsig =c("beta","alpha")

  fixmgarch = NULL

  # point etimate of the model parameters
  par0 = data.frame(extract_stan(object,pars = c("mu0","sigma0")))
  fixsig = data.frame(extract_stan(object,pars = fixsig))
  fixmean = data.frame(extract_stan(object,pars = fixmean))


  fixsig = fixsig[sp,]
  fixmean = fixmean[sp,]
  par0 = par0[sp,]

  #preliminary checks
  if( order$d1 > 0){
    reg = object$model$xreg
    # Check xreg
    if(is.null(xreg)){
      warning("No xreg specified, the forecast wont be accurate \n")
      xh  = matrix(0,nrow = h,ncol = order$d1)
    }
    else if( dim(xreg)[1] != h ||  dim(xreg)[2] != order$d1){
      # Check xreg dimensions
      warning("The dimension of xreg are not correct, the forecast wont be accurate \n")
      xh = matrix(0,nrow = h,ncol = order$d1)
    }
    else xh = xreg
  }

  # The previous data
  yh  =  matrix(0,nrow = draw,ncol = h)
  muh =  matrix(0,nrow = draw,ncol = h)
  sigh = matrix(0,nrow = draw,ncol = h)

  for(i in 1:draw){
    modi = stats::arima(x = y,
                        order = c(order$p,0,order$q),
                        xreg = reg,include.mean = FALSE,
                        fixed = fixmean[i,])

    muh[i,] = as.numeric(stats::predict(modi,n.ahead = h,newxreg = xh)$pred)

    modi = stats::arima(x = ht,
                        order = c(1,0,0),
                        include.mean = TRUE,
                        fixed = fixsig[i,])

    sigh[i,] = as.numeric(stats::predict(modi,n.ahead = h)$pred)
    for (j in 1:h) sigh[i,j] = stats::rnorm(n=1,mean = sigh[i,j],sd = par0[i,2])
    sigh[i,] = exp(sigh[i,]/2)

    for (j in 1:h) yh[i,j] = stats::rnorm(n = 1,mean = muh[i,j]+par0[i,1],sd = sigh[i,j])
  }
  colnames(yh) = paste0("yh.",1:h)
  yh = as.data.frame(yh)
  return(yh)
}
