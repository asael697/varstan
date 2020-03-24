data {
  int<lower=0> n;     // number of data items
  int<lower=0> p;     // number of predictors  ar
  int<lower=0> q;     // number of predictions ma
  int<lower=0>d1;     // number of independent variables
  matrix[n,d1] xreg;  // matrix with independent variables
  vector[n] y;        // outcome time series
  // prior data
  vector[4] prior_mu0;       // prior location parameter
  vector[4] prior_sigma0;    // prior scale parameter
  matrix[p,4] prior_ar;      // ar location hyper parameters
  matrix[q,4] prior_ma;      // ma location hyper parameters
  matrix[1,4] prior_arch;    // prior arch hyper parameters
  matrix[1,4] prior_mgarch;    // prior arch hyper parameters
  matrix[d1,4] prior_breg;   // prior ma hyper parameters
}
parameters {
  real mu0;                           // models location parameter
  real alpha;                         // mean log volatility
  vector[d1] breg;                    // regression parameters
  vector<lower=-1,upper=1>[p] phi0;   // ar parameters
  vector<lower=-1,upper=1>[q] theta0; // ma parameters
  real<lower=-1,upper=1> beta0;       // persistence of volatility
  real<lower=0> sigma0;               // white noise volatility shocks
  vector[n] hstd;                     // std log volatility at time t
}
transformed parameters {
  // Transformed prior parameters
  vector[p] phi;                      // ar parameters
  vector[q] theta;                    // ma parameters
  real beta;                          // beta parameter
  // Model parameters
  vector[n] mu;                      // model mean
  vector[n] epsilon;                 // model errors
  vector[n] h = hstd*sigma0;         // log volatility at time t

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

  if(prior_arch[1,4] == 1) beta = beta0;
  else beta = 2*beta0-1;

  // regression estimation
  if(d1 > 0) mu = mu0 +  xreg*breg;
  else mu = rep_vector(0,n);

  // volatility initial value
  mu[1] = mu0;                       // rescale mu[1]
  h[1] += alpha;                     // rescale h[1]
  epsilon[1] = y[1] - mu[1];

  // models means residuals
  for(i in 2:n){
    mu[i] = mu0;
    h[i] += alpha;
    //  ar Estimation
    if(p > 0) for(j in 1:p) if(i > j) mu[i] += y[i-j]*phi[j];
    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += epsilon[i-j]*theta[j];
    epsilon[i] = y[i] - mu[i];
    // SVM estimation
    h[i] += beta*(h[i-1] - alpha);
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

  //  prior for \omega0
  if(prior_mgarch[1,4]==1)      target += normal_lpdf(alpha|prior_mgarch[1,1],prior_mgarch[1,2]);
  else if(prior_mgarch[1,4]==2) target += beta_lpdf(alpha|prior_mgarch[1,1],prior_mgarch[1,2]);
  else if(prior_mgarch[1,4]==3) target += beta_lpdf(alpha|prior_mgarch[1,1],prior_mgarch[1,2]);
  else if(prior_mgarch[1,4]==4) target += student_t_lpdf(alpha|prior_mgarch[1,3],prior_mgarch[1,1],prior_mgarch[1,2]);
  else if(prior_mgarch[1,4]==5) target += cauchy_lpdf(alpha|prior_mgarch[1,1],prior_mgarch[1,2]);
  else if(prior_mgarch[1,4]==9) target += gamma_lpdf(alpha|prior_mgarch[1,1],prior_mgarch[1,2]);

  // prior garch

  if(prior_arch[1,4]==1) target += normal_lpdf(beta0|prior_arch[1,1],prior_arch[1,2]);
  else target += beta_lpdf(fabs(beta0)|prior_arch[1,1],prior_arch[1,2]);


  // std volatility errors
  target += normal_lpdf(hstd|0,1);
  // likelihood
  target += normal_lpdf(epsilon|0,exp(h/2));
}
generated quantities{
  vector[n] sigma = exp(h/2);
  vector[n] fit;
  vector[n] residuals;
  vector[n] log_lik;
  real loglik = 0;

  for (i in 1:n){
    residuals[i] = normal_rng(epsilon[i],sigma[i]);
    log_lik[i] = normal_lpdf(y[i]|mu[i],sigma[i]);
  }
  fit = y - residuals;
  loglik = sum(log_lik);
}
