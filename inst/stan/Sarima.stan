data {
  // Model data specify
  int<lower=0> n;          // number of data items
  int<lower=0> n1;         // number of data items
  int<lower=0> p;          // number of predictors  ar
  int<lower=0> d;          // number of simple differences
  int<lower=0> q;          // number of predictions ma
  int<lower=0> P;          // number of predictors  arch
  int<lower=0> D;          // number of seasonal differences
  int<lower=0> Q;          // number of predictions garch
  int<lower=0> period;     // time series period
  int<lower=0>d1;          // number of independent variables
  //  data
  vector[n1] y;            // outcome time series
  vector[n] yreal;         // outcome not differenced time series
  matrix[n1,d1] xreg;      // matrix with independent variables
  // prior data
  vector[4] prior_mu0;     // prior location parameter
  vector[4] prior_sigma0;  // prior scale parameter
  matrix[p,4] prior_ar;    // ar location hyper parameters
  matrix[q,4] prior_ma;    // ma location hyper parameters
  matrix[P,4] prior_sar;   // prior arch hyper parameters
  matrix[Q,4] prior_sma;   // prior ma hyper parameters
  matrix[d1,4] prior_breg; // prior ma hyper parameters
}
transformed data{
  int dinits = d+(period*D);
}
parameters{
  real<lower=0> sigma0;    // Variance parameter
  real mu0;                // location parameter
  vector[d1] breg;         // regression parameters
  vector[p] phi0;          // ar parameters
  vector[q] theta0;        // ma parameters
  vector[P] Phi0;          // sar parameters
  vector[Q] Theta0;        // sma parameters
}
transformed parameters{
  //***********************************************
  //             Model Parameters
  //***********************************************
  // coefficient parameters
  vector[p] phi;     // ar parameters
  vector[q] theta;   // ma parameters
  vector[P] sphi;    // ar parameters
  vector[Q] stheta;  // ma parameters

  // Temporal mean and residuals
  vector[n1] mu;           // Mean Parameter
  vector[n1] epsilon;      // residual parameter


  //***********************************************
  //         Transformation coeficients
  //***********************************************

  for( i in 1:p){
    if(prior_ar[i,4]== 1) phi[i] = phi0[i]*0.5;
    else phi[i] = (2*phi0[i] - 1);
  }
  for(i in 1:q){
    if(prior_ma[i,4] == 1) theta[i] = theta0[i]*0.5;
    else theta[i] = (2*theta0[i]-1);
  }
    for( i in 1:P){
    if(prior_sar[i,4]== 1) sphi[i] = Phi0[i]*0.5;
    else sphi[i] = (2*Phi0[i] - 1);
  }
  for(i in 1:Q){
    if(prior_sma[i,4] == 1) stheta[i] = Theta0[i]*0.5;
    else stheta[i] = (2*Theta0[i]-1);
  }

  //***********************************************
  //         ARMA estimations
  //***********************************************

  // regression estimation
  if(d1 > 0) mu = xreg*breg;
  else mu = rep_vector(0.0,n1);

  for(i in 1:n1){
    mu[i] += mu0;
    //  ar Estimation
    if(p > 0) for(j in 1:p) if(i > j) mu[i] +=  y[i-j]*phi[j];
    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += -epsilon[i-j]*theta[j];
    //  sar Estimation
    if(P > 0) for(j in 1:P) if(i >(period*j)) mu[i] +=  y[i-(period*j)]*sphi[j];
    // ma estimation
    if(Q > 0) for(j in 1:Q) if(i >(period*j)) mu[i] += -epsilon[i-(period*j)]*stheta[j];
    epsilon[i] = y[i] - mu[i];
  }
}
model {

  //  prior for \mu0
  if(prior_mu0[4] == 1)    target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==2) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==3) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==9) target += gamma_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma
  if(prior_sigma0[4] == 1)      target += normal_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 4) target += student_t_lpdf(sigma0|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 5) target += cauchy_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 6) target += inv_gamma_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 7) target += inv_chi_square_lpdf(sigma0|prior_sigma0[3]);

  // prior breg
  if(d1 > 0){
    for(i in 1:d1){
      if(prior_breg[i,4] == 1)      target += normal_lpdf(breg[i]|prior_breg[i,1],prior_breg[i,2]);
      else if(prior_breg[i,4] == 2) target += beta_lpdf(breg[i]|prior_breg[i,1],prior_breg[i,2]);
      else if(prior_breg[i,4] == 3) target += cauchy_lpdf(breg[i]|prior_breg[i,1],prior_breg[i,2]);
      else if(prior_breg[i,4] == 4) target += student_t_lpdf(breg[i]|prior_breg[i,3],prior_breg[i,1],prior_breg[i,2]);
      else if(prior_breg[i,4] == 5) target += cauchy_lpdf(breg[i]|prior_breg[i,1],prior_breg[i,2]);
      else if(prior_breg[i,4] == 9) target += gamma_lpdf(breg[i]|prior_breg[i,1],prior_breg[i,2]);
    }
  }

  // prior ar
  if(p > 0){
    for(i in 1:p){
     if(prior_ar[i,4]==1) target += normal_lpdf(phi0[i]|prior_ar[i,1],prior_ar[i,2]);
     else  target += beta_lpdf(phi0[i]|prior_ar[i,1],prior_ar[i,2]);
    }
  }
  // prior ma
  if(q > 0){
    for(i in 1:q){
      if(prior_ma[i,4]==1) target += normal_lpdf(theta0[i]|prior_ma[i,1],prior_ma[i,2]);
     else  target += beta_lpdf(theta0[i]|prior_ma[i,1],prior_ma[i,2]);
    }
  }
  // prior sar
  if(P > 0){
    for(i in 1:P){
     if(prior_sar[i,4]==1) target += normal_lpdf(Phi0[i]|prior_sar[i,1],prior_sar[i,2]);
     else  target += beta_lpdf(Phi0[i]|prior_sar[i,1],prior_sar[i,2]);
    }
  }
  // prior sma
  if(Q > 0){
    for(i in 1:Q){
      if(prior_sma[i,4]==1) target += normal_lpdf(Theta0[i]|prior_sma[i,1],prior_sma[i,2]);
     else  target += beta_lpdf(Theta0[i]|prior_sma[i,1],prior_sma[i,2]);
    }
  }
  // Likelihood
  target += normal_lpdf(epsilon|0,sigma0);
}
generated quantities{
  real loglik = 0;
  vector[n1] log_lik;
  vector[n] fit;
  vector[n] residual;

  for(i in 1:n){
    if(i<=dinits) residual[i] = normal_rng(0,sigma0);
    else residual[i] = normal_rng(epsilon[i-dinits],sigma0);
    fit[i] = yreal[i]-residual[i];
    if(i <=n1){
      log_lik[i] = normal_lpdf(y[i]|mu[i],sigma0);
      loglik += log_lik[i];
    }
  }
}
