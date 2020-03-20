functions{
  real Jpv(real v){
    real y;
    y =trigamma(v/2) -trigamma((v+1)/2) - 2*(v+3)/(v*(v+1)*(v+1));
    y = (v/(v+3))*y;
    return sqrt(y);
  }
}
data {
  // Model data
  int<lower=0> n;     // number of data items
  int<lower=0> s;     // number of predictors  arch
  int<lower=0> k;     // number of predictions garch
  int<lower=0> h;     // number of predictions mgarch
  int<lower=0> p;     // number of predictors  ar
  int<lower=0> q;     // number of predictions ma
  vector[n] y;        // outcome time series
  // prior data
  vector[4] prior_mu0;       // prior location parameter
  vector[4] prior_sigma0;    // prior scale parameter
  matrix[p,4] prior_ar;      // ar location hyper parameters
  matrix[q,4] prior_ma;      // ma location hyper parameters
  matrix[s,4] prior_arch;    // prior arch hyper parameters
  matrix[k,4] prior_garch;   // prior ma hyper parameters
  matrix[h,4] prior_mgarch;  // prior ma hyper parameters
}
parameters{
  vector<lower=-1,upper=1>[p] phi0;   // ar parameters
  vector<lower=-1,upper=1>[q] theta0; // ma parameters
  real<lower=0> sigma0;               // Variance parameter
  real mu0;                           // location parameter
  vector<lower=0,upper=1>[s] alpha;   // arch parameters
  vector<lower=0,upper=1>[k] beta;    // garch parameters
  vector [h] mgarch;                  // mean garch parameters
}
transformed parameters{
  vector[p] phi;          // ar parameters
  vector[q] theta;        // ma parameters

  // Temporal mean and residuals
  vector[n] mu;                     // Mean Parameter
  vector[n] epsilon;
  vector<lower=0>[n] sigma;         // Variance parameter

  //***********************************************
  //         Transformation coeficients
  //***********************************************

  for( i in 1:p){
    if(prior_ar[i,4]== 1) phi[i] = phi0[i];
    else phi[i] = 2*phi0[i] - 1;
  }
  for(i in 1:q){
    if(prior_ma[i,4] == 1) theta[i] = theta0[i];
    else theta[i] = 2*theta0[i]-1;
  }

  //***********************************************
  //         ARMA estimations
  //***********************************************

  for(i in 1:n){
     mu[i] = mu0;
     sigma[i] = sigma0;
    //  ar Estimation
    if(p > 0) for(j in 1:p) if(i > j) mu[i] +=  y[i-j]*phi[j];
    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += -epsilon[i-j]*theta[j];
    epsilon[i] = y[i] - mu[i];
    // Garch Iteration
    if(s >= k){
       // arch estimation
      if(s > 0) for(j in 1:s) if(i > j) sigma[i] += alpha[j]*pow(epsilon[i-j],2);
       // garch estimation
      if(k > 0) for(j in 1:k) if(i > j)  sigma[i] += beta[j]*pow(sigma[i-j], 2);
    }
    sigma[i] = sqrt(sigma[i]);
    // mgarch estimation
    if(h > 0) for(j in 1:h) if(i > j) mu[i] += mgarch[j]*sigma[i-j];
  }
}
model {
  //  prior for \mu0
  if(prior_mu0[4]==1)      target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==2) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==3) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==9) target += gamma_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma
  if(prior_sigma0[4] == 1)    target += normal_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==4) target += student_t_lpdf(sigma0|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==5) target += cauchy_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==6) target += inv_gamma_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==7) target += inv_chi_square_lpdf(sigma0|prior_sigma0[3]);
  else if(prior_sigma0[4]==9) target += gamma_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);

  // prior ar
  if(p > 0){
    for(i in 1:p){
     if(prior_ar[i,4]==1) target += normal_lpdf(phi0[i]|prior_ar[i,1],prior_ar[i,2]);
     else  target += beta_lpdf(fabs(phi0[i])|prior_ar[i,1],prior_ar[i,2]);
    }
  }
  // prior ma
  if(q > 0){
    for(i in 1:q){
      if(prior_ma[i,4]==1) target += normal_lpdf(theta0[i]|prior_ma[i,1],prior_ma[i,2]);
      else  target += beta_lpdf(fabs(theta0[i])|prior_ma[i,1],prior_ma[i,2]);
    }
  }

  // prior arch
  if(s > 0){
    for(i in 1:s){
     if(prior_arch[i,4]==1) target += normal_lpdf(alpha[i]|prior_arch[i,1],prior_arch[i,2]);
     else target += beta_lpdf(alpha[i]|prior_arch[i,1],prior_arch[i,2]);
    }
  }
  // prior garch
  if(k > 0){
    for(i in 1:k){
     if(prior_garch[i,4]==1) target += normal_lpdf(beta[i]|prior_garch[i,1],prior_garch[i,2]);
     else target += beta_lpdf(beta[i]|prior_garch[i,1],prior_garch[i,2]);
    }
  }
  // prior mean_garch
  if(h > 0){
    for(i in 1:h){
      if(prior_mgarch[i,4]==1)      target += normal_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
      else if(prior_mgarch[i,4]==2) target += beta_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
      else if(prior_mgarch[i,4]==3) target += beta_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
      else if(prior_mgarch[i,4]==4) target += student_t_lpdf(mgarch[i]|prior_mgarch[i,3],prior_mgarch[i,1],prior_mgarch[i,2]);
      else if(prior_mgarch[i,4]==5) target += cauchy_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
      else if(prior_mgarch[i,4]==9) target += gamma_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    }
  }
  // Likelihood
  target += normal_lpdf(y|mu,sigma);
}
generated quantities{
  real loglik = 0;
  vector[n] log_lik;
  vector[n] fit;
  vector[n] residual;

  for(i in 1:n){
     fit[i] = normal_rng(mu[i],sigma[i]);
     log_lik[i] = normal_lpdf(y[i]|mu[i],sigma[i]);
     loglik += log_lik[i];
  }
  residual = y - fit;
}
