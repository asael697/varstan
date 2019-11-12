data {
  // Model data
  int<lower=0> n;     // number of data items
  int<lower=0> s;     // number of predictors  arch
  int<lower=0> k;     // number of predictions garch
  int<lower=0> h;     // number of predictions mgarch
  vector[n] y;        // outcome time series
  // prior data
  vector[4] prior_mu0;    // prior location parameter
  vector[4] prior_sigma0; // prior scale parameter
  matrix[s,4] prior_arch;    // prior arch hyper parameters
  matrix[k,4] prior_garch;   // prior ma hyper parameters
  matrix[h,4] prior_mgarch;   // prior ma hyper parameters
}
parameters{
  real mu0;                         // location parameter
  real<lower=0> sigma0;             // constant parameter garch
  vector<lower=0,upper=1>[s] alpha; // arch parameters
  vector<lower=0,upper=1>[k] beta;  // garch parameters
  vector [h] mgarch;            // mean garch parameters
}
transformed parameters{
  vector<lower=0>[n] sigma;         // Variance parameter
  vector[n] mu;                     // Mean Parameter

  // Garch iteration
  for(i in 1:n){
    mu[i] = mu0;
     if(i<=k){
       sigma[i] = sigma0;
     }
     else{
       sigma[i] = sigma0;
       if(s > 0){
          for(j in 1:s) sigma[i] +=  alpha[j]*pow(y[i-j]-mu[i-j],2);
       }
       // garch estimation
       if(k > 0){
          for(j in 1:k) sigma[i] += beta[j]*pow(sigma[i-j], 2);
       }
     }
     sigma[i] = sqrt(sigma[i]);
     if(h > 0){
       for(j in 1:h) mu[i] += mgarch[i]*sigma[i-j];
     }
  }
}
model {
  //  prior for \mu0
  if(prior_mu0[4]==1) target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4]==5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma
  if(prior_sigma0[4] == 1) target += normal_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==4) target += student_t_lpdf(sigma0|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==5) target += cauchy_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==6) target += inv_gamma_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4]==7) target += inv_chi_square_lpdf(sigma0|prior_sigma0[3]);

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
    if(prior_mgarch[i,4]==1) target += normal_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==4) target += student_t_lpdf(mgarch[i]|prior_mgarch[i,3],prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==5) target += cauchy_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==2) target += beta_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
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
