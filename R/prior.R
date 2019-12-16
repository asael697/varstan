#################################################################################################
#                           Prior Functions
#################################################################################################
#
#' Prior objects for arima models
#'
#' Generic function for setting a prior to an specify parameter
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param type: the type of parameter wich a prior is defined could be mu, sigma0, ar, ma, arch, garch, mgarch
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#' @param df: The parameter for the degree freedom only for t-student and chi-square
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#'
set_prior = function(dat,type,lag = 0,par1 = 0,par2 = 1,dist = "normal",df = 1){
  if(check_type(type)){
    if(type=="ar")    dat = set_prior_ar(dat = dat,lag = lag,par1 = par1,par = par2,dist = dist)
    if(type=="ma")    dat = set_prior_ma(dat = dat,lag = lag,par1 = par1,par = par2,dist = dist)
    if(type=="arch")  dat = set_prior_arch(dat = dat,lag = lag,par1 = par1,par = par2,dist = dist)
    if(type=="garch") dat = set_prior_garch(dat = dat,lag = lag,par1 = par1,par = par2,dist = dist)
    if(type=="mgarch")dat = set_prior_mgarch(dat = dat,lag = lag,par1 = par1,par = par2,dist = dist)
    if(type=="mu0")   dat = set_prior_mu0(dat = dat,par1 = par1,par = par2,dist = dist,df = df)
    if(type=="sigma0")dat = set_prior_sigma0(dat = dat,par1 = par1,par = par2,dist = dist,df = df)
    if(type=="dfv")dat = set_prior_dfv(dat = dat,par1 = par1,par = par2,dist = dist,df = df)
  }
  else{
    cat(type, "is not a defined parameter")
  }
  return(dat)
}
#'
#' Generic function for setting a prior to an specify parameter
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param type: the type of parameter wich a prior is defined could be mu, sigma0, ar, ma, arch, garch, mgarch
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
#' @export
#'
#'
get_prior = function(dat,type,lag = 0){
  if(check_type(type)){
    if(type=="ar")     get_prior_ar(dat = dat,lag = lag)
    if(type=="ma")     get_prior_ma(dat = dat,lag = lag)
    if(type=="arch")   get_prior_arch(dat = dat,lag = lag)
    if(type=="garch")  get_prior_garch(dat = dat,lag = lag)
    if(type=="mgarch") get_prior_mgarch(dat = dat,lag = lag)
    if(type=="mu0") get_prior_mu0(dat = dat)
    if(type=="sigma0")get_prior_sigma0(dat = dat)
    if(type=="dfv")get_prior_dfv(dat = dat)
  }
  else{
    cat(type, "is not a defined parameter \n")
  }

}

#  ----------------------------------------------------------------
#                                 Intercept prior
#  ----------------------------------------------------------------

#' Set Prior distribution for mu parameter
#'
#' Set a prior distribution for a mu parameter
#'
#' @usage set_prior(dat,par1,par2,dist,df)
#'
#' @param dat: an arima model speficied in varstan.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the dist of the prior distribution, could be: a normal,beta, and uniform
#' @param df: The parameter for the degree freedom only for t-student and chi-square
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_mu0 = function(dat,par1,par2,dist = "normal",df){
  if(check_dist(dist,par ="mu")){
    if(identical(dist,"normal") ) dat$prior_mu0 = c(check_loc(par1),check_scl(par2),1,1)
    if(identical(dist,"student")) dat$prior_mu0 = c(check_loc(par1),check_scl(par2),check_df(df),4)
    if(identical(dist,"cauchy"))  dat$prior_mu0 = c(check_loc(par1),check_scl(par2),1,5)
  }
  else{
    cat(dist,"is not a family defined for mu, a default normal prior is used")
    dat$prior_mu0 = c(0,1,1,1)
  }
  return(dat)
}

#' Get Prior distribution for a mu parameter
#'
#' Generic function for setting a prior to the mean parameters
#'
#' @usage get_prior_mu0(dat)
#'
#' @param dat: an arima model speficied in varstan.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_mu0 = function(dat){
  if(is.model(dat)){
    if(dat$prior_mu0[4]==1)cat("mu0 ~ normal","(loc =", dat$prior_mu0[1],", scl =", dat$prior_mu0[2],")\n")
    if(dat$prior_mu0[4]==4)cat("mu0 ~ t","(loc =",dat$prior_mu0[1],",scl =", dat$prior_mu0[2],",df =",dat$prior_mu0[3],")\n")
    if(dat$prior_mu0[4]==5)cat("mu0 ~ cauchy","(loc =", dat$prior_mu0[1],", scl =" ,dat$prior_mu0[2],")\n")
  }
  else cat(class(dat), "is not an arima model \n")
}

#  ----------------------------------------------------------------
#                                 Variance prior
#  ----------------------------------------------------------------

#' Set Prior distribution forsigma0 parameter
#'
#' Set a prior distribution for a sigma0 parameter
#'
#' @usage set_prior(dat,par1,par2,dist,df)
#'
#' @param dat: an arima model speficied in varstan.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the dist of the prior distribution, could be: a normal,beta, and uniform
#' @param df: The parameter for the degree freedom only for t-student and chi-square
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_sigma0 = function(dat,par1,par2,dist = "normal",df){
  if(check_dist(dist,par ="sigma0")){
    if(identical(dist,"normal") )    dat$prior_sigma0 = c(check_loc(par1),check_scl(par2),1,1)
    if(identical(dist,"student"))    dat$prior_sigma0 = c(check_loc(par1),check_scl(par2),check_df(df),4)
    if(identical(dist,"cauchy"))     dat$prior_sigma0 = c(check_loc(par1),check_scl(par2),1,5)
    if(identical(dist,"inv_gamma"))      dat$prior_sigma0 = c(check_form(par1),check_form(par2),1,6)
    if(identical(dist,"inv_chi_square")) dat$prior_sigma0 = c(0,0,check_df(df),7)
    if(identical(dist,"gamma"))      dat$prior_sigma0 = c(check_form(par1),check_form(par2),1,9)
  }
  else{
    cat(family,"is not a family defined for sigma0, a default half t-student prior is used")
    dat$prior_sigma0 = c(0,1,6,4)
  }
  return(dat)
}
#' Get Prior distribution for a sigma parameter
#'
#' Generic function for setting a prior to the variance parameters
#'
#' @usage get_prior_sigma(dat)
#'
#' @param dat: an arima model speficied in varstan.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_sigma0 = function(dat){
  if(is.model(dat)){
    if(dat$prior_sigma0[4]==1)cat("sigma0 ~ half_normal","(loc =", dat$prior_sigma0[1],", scl =", dat$prior_sigma0[2],")\n")
    if(dat$prior_sigma0[4]==4)cat("sigma0 ~ half_t","(loc =",dat$prior_sigma0[1],",scl =", dat$prior_sigma0[2],",df =",dat$prior_sigma0[3],")\n")
    if(dat$prior_sigma0[4]==5)cat("sigma0 ~ half_cauchy","(loc =", dat$prior_sigma0[1],", scl =" ,dat$prior_sigma0[2],")\n")
    if(dat$prior_sigma0[4]==6)cat("sigma0 ~ inv_gamma","(form1 =", dat$prior_sigma0[1],", form2 =" ,dat$prior_sigma0[2],")\n")
    if(dat$prior_sigma0[4]==7)cat("sigma0 ~ inv_chi_square","(df =", dat$prior_sigma0[3],")\n")
    if(dat$prior_sigma0[4]==9)cat("sigma0 ~ gamma","(form1 =", dat$prior_sigma0[1],", form2 =" ,dat$prior_sigma0[2],")\n")
  }
  else cat(class(dat), "is not an arima model \n")
}

#  ----------------------------------------------------------------
#                     degree freedom Prior
#  ----------------------------------------------------------------

#' Set Prior distribution for a degree gfreedom v parameter
#'
#' Set a prior distribution for a degree freedom v parameter
#'  in a Generalized t-student varma model
#'
#' @usage set_prior(dat,par1,par2,dist,df)
#'
#' @param dat: a varma model speficied in varstan.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the dist of the prior distribution, could be: a normal,beta, and uniform
#' @param df: The parameter for the degree freedom only for t-student and chi-square
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_dfv = function(dat,par1,par2,dist = "normal",df){
  if(check_dist(dist,par ="dfv")){
    if(identical(dist,"normal") )    dat$prior_dfv = c(check_loc(par1),check_scl(par2),1,1)
    if(identical(dist,"inv_gamma"))  dat$prior_dfv = c(check_form(par1),check_form(par2),1,6)
    if(identical(dist,"Jeffrey"))    dat$prior_dfv = c(0,1,1,8)
    if(identical(dist,"gamma"))      dat$prior_dfv = c(check_form(par1),check_form(par2),1,9)
  }
  else{
    cat(family,"is not a family defined for dfv, a default gamma prior is used")
    dat$prior_dfv = c(2,0.1,1,9)
  }
  return(dat)
}
#' Set Prior distribution for a degree gfreedom v parameter
#'
#' Set a prior distribution for a degree freedom v parameter
#'  in a Generalized t-student varma model
#'
#' @usage get_prior_dfv(dat)
#'
#' @param dat: an varma model speficied in varstan.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_dfv = function(dat){
  if(is.model(dat)){
    if(dat$prior_dfv[4]==1)cat("dfv ~ half_normal","(loc =", dat$prior_dfv[1],", scl =", dat$prior_dfv[2],")\n")
    if(dat$prior_dfv[4]==6)cat("dfv ~ inv_gamma","(form1 =", dat$prior_dfv[1],", form2 =" ,dat$prior_dfv[2],")\n")
    if(dat$prior_dfv[4]==8)cat("dfv ~ Jeffrey_prior(v)\n")
    if(dat$prior_dfv[4]==9)cat("dfv ~ gamma","(form1 =", dat$prior_dfv[1],", form2 =" ,dat$prior_dfv[2],")\n")
  }
  else cat(class(dat), "is not an varma model \n")
}
#
#' Set one Prior distribution for a lagged parameter
#'
#' Generic function for setting a prior to ar parameters
#'
#' @usage set_prior(dat,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_one_prior_lag = function(par1,par2,dist = "normal"){
  if(check_dist(dist,par = "ar")){
    if(identical(dist,"normal") )  x = c(check_loc(par1),check_scl(par2),1,1)
    if(identical(dist,"beta"))     x = c(check_form(par1),check_form(par2),1,2)
    if(identical(dist,"uniform"))  x = c(1,1,1,3)
  }
  else{
    cat(dist,"is not a family defined for lagged models, a default normal prior is used")
    x = c(0,1,1,1)
  }
  return(x)
}

##################################################################################################################
#                                     AR Priors
##################################################################################################################


#' Set Prior distribution for an ar model
#'
#' Generic function for setting a prior to ar parameters
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_ar = function(dat,type,par1,par2,dist = "normal",lag=0){
  if(is.model(dat) ){
    if(dat$p == 0 ){
      cat(class(dat),"doesnt have an ar part defined")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$p) dat$prior_ar[i,] = set_one_prior_lag(par1 = par1,par2 = par2,dist = dist)
      }
      else{
        if(lag <= dat$p) dat$prior_ar[lag,] = set_one_prior_lag(par1[1],par2[1],dist[1])
        else cat("lag = ",lag,"is not lower than p = ",dat$p)
      }
    }
  }
  else{
    cat(class(dat),"is not a defined  model")
  }
  return(dat)
}
#' Get one Prior distribution for an ar model
#'
#'
#' @usage get_one_prior(dat,lag = 1)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_one_prior_ar = function(dat,lag = 1){
  if(is.model(dat)){
    if(dat$p == 0){
      cat("There is no ar part defined \n")
    }
    else{
      if(lag <= dat$p){
        prior_ar = dat$prior_ar[lag,]
        if(prior_ar[4] ==1) cat("ar[",lag,"] ~ normal","(mu = ",prior_ar[1],", sd = ",prior_ar[2],") \n")
        if(prior_ar[4] ==2) cat("ar[",lag,"] ~ beta","(form1 = ",prior_ar[1],", form2 = ",prior_ar[2],") \n")
        if(prior_ar[4] ==3) cat("ar[",lag,"] ~ uniform","(form1 = ",prior_ar[1],", form2 = " ,prior_ar[2],") \n")
      }
      else cat("lag = ",lag,"is not lower than p = ",dat$p)
    }
  }
  else cat(class(dat), "is not an arima model \n")
}
#' Get Prior distribution for an ar model
#'
#'
#' @usage get_prior(dat,lag)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_ar = function(dat,lag = 0){
  if(is.model(dat)){
    if(dat$p == 0 ){
      cat("There is no ar part defined \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$p) get_one_prior_ar(dat = dat,lag = i)
      }
      else get_one_prior_ar(dat = dat,lag = lag)
    }
  }
  else cat(class(dat), "is not an arima model \n")
}

##################################################################################################################
#                                     ma Priors
##################################################################################################################


#' Set Prior distribution for a ma model
#'
#' Generic function for setting a prior to ma parameters
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_ma = function(dat,type,par1,par2,dist = "normal",lag=0){
  if(is.model(dat) ){
    if(dat$q == 0 ){
      cat(class(dat),"doesnt have a ma part \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$q) dat$prior_ma[i,] = set_one_prior_lag(par1 = par1,par2 = par2,dist = dist)
      }
      else{
        if(lag <= dat$q) dat$prior_ma[lag,] = set_one_prior_lag(par1[1],par2[1],dist[1])
        else cat("lag = ",lag,"is not lower than q = ",dat$q)
      }
    }
  }
  else{
    cat(class(dat),"is not a defined  model \n")
  }
  return(dat)
}
#' Get one Prior distribution for a ma model
#'
#'
#' @usage get_one_prior(dat,lag = 1)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_one_prior_ma = function(dat,lag = 1){
  if(is.model(dat)){
    if(dat$q == 0){
      cat("There is no ma part defined \n")
    }
    else{
      if(lag <= dat$q){
        prior_ar = dat$prior_ma[lag,]
        if(prior_ar[4] ==1) cat("ma[",lag,"] ~ normal","(mu = ",prior_ar[1],", sd = ",prior_ar[2],") \n")
        if(prior_ar[4] ==2) cat("ma[",lag,"] ~ beta","(form1 = ",prior_ar[1],", form2 = ",prior_ar[2],") \n")
        if(prior_ar[4] ==3) cat("ma[",lag,"] ~ uniform","(form1 = ",prior_ar[1],", form2 = " ,prior_ar[2],") \n")
      }
      else cat("lag = ",lag,"is not lower than q = ",dat$q)
    }
  }
  else cat(class(dat), "is not an arima model \n")
}
#' Get Prior distribution for an ar model
#'
#'
#' @usage get_prior(dat,lag)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_ma = function(dat,lag = 0){
  if(is.model(dat)){
    if(dat$q == 0 ){
      cat("There is no ma part defined \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$q) get_one_prior_ma(dat = dat,lag = i)
      }
      else get_one_prior_ma(dat = dat,lag = lag)
    }
  }
  else cat(class(dat), "is not an arima model \n")
}


##################################################################################################################
#                                     arch Priors
##################################################################################################################


#' Set Prior distribution for a arch model
#'
#' Generic function for setting a prior to arch parameters
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_arch = function(dat,type,par1,par2,dist = "normal",lag=0){
  if(is.model(dat) ){
    if(dat$s == 0 ){
      cat(class(dat),"doesnt have an arch part \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$s) dat$prior_arch[i,] = set_one_prior_lag(par1 = par1,par2 = par2,dist = dist)
      }
      else{
        if(lag <= dat$s) dat$prior_arch[lag,] = set_one_prior_lag(par1[1],par2[1],dist[1])
        else cat("lag = ",lag,"is not lower than s = ",dat$k)
      }
    }
  }
  else{
    cat(class(dat),"is not a defined  model \n")
  }
  return(dat)
}
#' Get one Prior distribution for a arch model
#'
#'
#' @usage get_one_prior(dat,lag = 1)
#'
#' @param dat: a garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_one_prior_arch = function(dat,lag = 1){
  if(is.model(dat)){
    if(dat$s == 0){
      cat("There is no arch part defined \n")
    }
    else{
      if(lag <= dat$s){
        prior_ar = dat$prior_arch[lag,]
        if(prior_ar[4] ==1) cat("arch[",lag,"] ~ normal","(mu = ",prior_ar[1],", sd = ",prior_ar[2],") \n")
        if(prior_ar[4] ==2) cat("arch[",lag,"] ~ beta","(form1 = ",prior_ar[1],", form2 = ",prior_ar[2],") \n")
        if(prior_ar[4] ==3) cat("arch[",lag,"] ~ uniform","(form1 = ",prior_ar[1],", form2 = " ,prior_ar[2],") \n")
      }
      else cat("lag = ",lag,"is not lower than s = ",dat$s)
    }
  }
  else cat(class(dat), "is not an garch model \n")
}
#' Get Prior distribution for an arch model
#'
#'
#' @usage get_prior(dat,lag)
#'
#' @param dat: an arima model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_arch = function(dat,lag = 0){
  if(is.model(dat)){
    if(dat$s == 0 ){
      cat("There is no arch part defined \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$s) get_one_prior_arch(dat = dat,lag = i)
      }
      else get_one_prior_arch(dat = dat,lag = lag)
    }
  }
  else cat(class(dat), "is not an arch model \n")
}

##################################################################################################################
#                                     garch Priors
##################################################################################################################


#' Set Prior distribution for a garch model
#'
#' Generic function for setting a prior to garch parameters
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: a garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_garch = function(dat,type,par1,par2,dist = "normal",lag=0){
  if(is.model(dat) ){
    if(dat$k == 0 ){
      cat(class(dat),"doesnt have an garch part \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$k) dat$prior_garch[i,] = set_one_prior_lag(par1 = par1,par2 = par2,dist = dist)
      }
      else{
        if(lag <= dat$k) dat$prior_garch[lag,] = set_one_prior_lag(par1[1],par2[1],dist[1])
        else cat("lag = ",lag,"is not lower than k = ",dat$k)
      }
    }
  }
  else{
    cat(class(dat),"is not a defined  model \n")
  }
  return(dat)
}
#' Get one Prior distribution for a garch model
#'
#'
#' @usage get_one_prior(dat,lag = 1)
#'
#' @param dat: a garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_one_prior_garch = function(dat,lag = 1){
  if(is.model(dat)){
    if(dat$k == 0){
      cat("There is no garch part defined \n")
    }
    else{
      if(lag <= dat$k){
        prior_ar = dat$prior_garch[lag,]
        if(prior_ar[4] ==1) cat("garch[",lag,"] ~ normal","(mu = ",prior_ar[1],", sd = ",prior_ar[2],") \n")
        if(prior_ar[4] ==2) cat("garch[",lag,"] ~ beta","(form1 = ",prior_ar[1],", form2 = ",prior_ar[2],") \n")
        if(prior_ar[4] ==3) cat("garch[",lag,"] ~ uniform","(form1 = ",prior_ar[1],", form2 = " ,prior_ar[2],") \n")
      }
      else cat("lag = ",lag,"is not lower than k = ",dat$k)
    }
  }
  else cat(class(dat), "is not an garch model \n")
}
#' Get Prior distribution for an garch model
#'
#'
#' @usage get_prior(dat,lag)
#'
#' @param dat: an garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_garch = function(dat,lag = 0){
  if(is.model(dat)){
    if(dat$k == 0 ){
      cat("There is no garch part defined \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$k) get_one_prior_garch(dat = dat,lag = i)
      }
      else get_one_prior_garch(dat = dat,lag = lag)
    }
  }
  else cat(class(dat), "is not an garch model \n")
}

##################################################################################################################
#                                     mgarch Priors
##################################################################################################################


#' Set Prior distribution for a mgarch model
#'
#' Generic function for setting a prior to garch parameters
#'
#' @usage set_prior(type,lag,par1,par2,dist)
#'
#' @param dat: a garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#' @param par1 The first hyper-parameter of the prior distribution
#' @param par2 The second hyper-parameter of the prior distribution
#' @param dist: the distribution of the prior distribution, could be: a normal,beta, and uniform
#'
#' @author  Asael Alonzo Matamoros
#'
set_prior_mgarch = function(dat,type,par1,par2,dist = "normal",lag=0){
  if(is.model(dat) ){
    if(dat$h == 0 ){
      cat(class(dat),"doesnt have a mgarch part \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$h) dat$prior_mgarch[i,] = set_one_prior_lag(par1 = par1,par2 = par2,dist = dist)
      }
      else{
        if(lag <= dat$h) dat$prior_mgarch[lag,] = set_one_prior_lag(par1[1],par2[1],dist[1])
        else cat("lag = ",lag,"is not lower than h = ",dat$h)
      }
    }
  }
  else{
    cat(class(dat),"is not a defined  model \n")
  }
  return(dat)
}
#' Get one Prior distribution for a mgarch model
#'
#'
#' @usage get_one_prior(dat,lag = 1)
#'
#' @param dat: a garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_one_prior_mgarch = function(dat,lag = 1){
  if(is.model(dat)){
    if(dat$k == 0){
      cat("There is no mgarch part defined \n")
    }
    else{
      if(lag <= dat$h){
        prior_ar = dat$prior_mgarch[lag,]
        if(prior_ar[4] ==1) cat("mgarch[",lag,"] ~ normal","(mu = ",prior_ar[1],", sd = ",prior_ar[2],") \n")
        if(prior_ar[4] ==2) cat("mgarch[",lag,"] ~ beta","(form1 = ",prior_ar[1],", form2 = ",prior_ar[2],") \n")
        if(prior_ar[4] ==3) cat("mgarch[",lag,"] ~ uniform","(form1 = ",prior_ar[1],", form2 = " ,prior_ar[2],") \n")
      }
      else cat("lag = ",lag,"is not lower than h = ",dat$h)
    }
  }
  else cat(class(dat), "is not an garch model \n")
}
#' Get Prior distribution for an garch model
#'
#'
#' @usage get_prior(dat,lag)
#'
#' @param dat: an garch model speficied in varstan.
#' @param lag: the lag of the parameter which the prior is defined if lag = 0, then the prior will be applied
#' for all the lags.
#'
#' @author  Asael Alonzo Matamoros
#'
get_prior_mgarch = function(dat,lag = 0){
  if(is.model(dat)){
    if(dat$h == 0 ){
      cat("There is no mgarch part defined \n")
    }
    else{
      if(lag == 0){
        for(i in 1:dat$h) get_one_prior_mgarch(dat = dat,lag = i)
      }
      else get_one_prior_mgarch(dat = dat,lag = lag)
    }
  }
  else cat(class(dat), "is not an garch model \n")
}
