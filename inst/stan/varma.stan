data {
  int<lower=0> n;         // number of data items
  int<lower=1> d;         // number of dimensions
  int<lower=0> p;         // number of predictors var
  int<lower=0> q;         // number of predictors var
  matrix[n,d] y;          // outcome matrix time series
  vector[4] prior_mu0;    // prior location parameter
  vector[4] prior_sigma0; // prior scale parameter
  vector[4] prior_lkj;    // prior scale parameter
  matrix[p,4] prior_ar;   // ar location hyper parameters
  matrix[q,4] prior_ma;   // ma location hyper parameters
}
parameters{
  //      Parameter VAR Bekk Model
  row_vector[d]mu0;                         // Var constant
  matrix <lower=-1,upper=1>[d,d] phi[p];   // *temp coefficients VAR
  matrix <lower=-1,upper=1>[d,d] theta[q]; // *temp coefficients VMA
  cholesky_factor_corr[d] SigCorr;         // *arch constant correlation
  vector<lower=0>[d]  sigma1;               // *arch constant scale
}
transformed parameters {
  //***********************************************
  //             Model Parameters
  //***********************************************
  // Temporal mean and residuals
  matrix[n,d] mu;                   // *VAR mean
  matrix[n,d] epsilon;              // *residual mean
  matrix[d,d] sigma;              // arch constant
  matrix[d,d] sigma0;               // *covariance matrix sigma


  //***********************************************
  //      Sigma0 transformation
  //***********************************************

  sigma   = diag_pre_multiply(sigma1,SigCorr);
  sigma0  = multiply_lower_tri_self_transpose(sigma);

  //***********************************************
  //         VARMA estimations
  //***********************************************

  for(i in 1:n){
    // initial constant
    mu[i] = mu0;

    //  VAR Iteration
    if(p > 0) for(j in 1:p) if(i > j) mu[i] += y[i-j]*phi[j];

    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += epsilon[i-j]*theta[j];
    epsilon[i] = y[i] - mu[i];
  }
}
model{
  //      Priors  definition

  //  prior for \mu0
  if(prior_mu0[4]==1)      target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==2) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==3) target += beta_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==9) target += gamma_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma0 diagonal vector
  if(prior_sigma0[4] == 1)      target += normal_lpdf(sigma1|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 4) target += student_t_lpdf(sigma1|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 5) target += cauchy_lpdf(sigma1|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 6) target += inv_gamma_lpdf(sigma1|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 7) target += inv_chi_square_lpdf(sigma1|prior_sigma0[3]);

  //   sigma0 constant correlation Matrix
  target += lkj_corr_cholesky_lpdf(SigCorr|prior_lkj[1]);

  // prior ar
  if(p > 0){
    for(i in 1:p){
     if(prior_ar[i,4]==1) target += normal_lpdf(to_vector(phi[i])|prior_ar[i,1],prior_ar[i,2]);
     else  target += normal_lpdf(to_vector(phi[i])|0,1);
    }
  }
  // prior ma
  if(q > 0){
    for(i in 1:q){
     if(prior_ma[i,4]==1) target += normal_lpdf(to_vector(theta[i])|prior_ma[i,1],prior_ma[i,2]);
     else  target += normal_lpdf(to_vector(theta[i])|0,1);
    }
  }
  //      Likelihood
  for(i in 1:n)target += multi_normal_cholesky_lpdf(epsilon[i]|rep_vector(0,d), sigma );
}
generated quantities{
  real loglik = 0;
  vector[n] log_lik;
  matrix[n,d] fit;
  matrix[n,d] residual;

  for(i in 1:n){
    residual[i] = multi_normal_cholesky_rng(epsilon[i],sigma)';
    fit[i] = y[i]-residual[i];
    log_lik[i] = multi_normal_cholesky_lpdf(y[i]|mu[i],sigma);
    loglik += log_lik[i];
  }
}
