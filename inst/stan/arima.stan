functions{
  vector dif(vector x,int d);
  vector dif(vector x, int d){
    int n = num_elements(x);
    vector[n-1] y;
    for(i in 1:(n-1)){
      y[i] = x[i+1]-x[i];
    }
    if( (d-1) == 0)
       return y;
    else
       return dif(y,d-1);
  }
  vector idif(vector x, real x0 ){
    int n = num_elements(x);
    vector [n+1] y;
    y[1] = x0;
    for(i in 2:(n+1)){
      y[i] = y[i-1] +x[i-1];
    }
    return y;
  }
  vector complete(vector x, int d, real x0){
    int n = num_elements(x);
    vector [d] y;
    if(n < d){
      for(i in 1:d){
        if(i <= n) y[i] = x[i];
        else y[i] = x0;
      }
    }
    else{
      for(i in 1:d)
        y[i] = x[i];
    }
    return y;
  }
  vector extract_vector_left(vector x, int d){
    int n =  num_elements(x);
    if(d < n ){
       vector[d] y;
       for (i in 1:d ) y[i] = x[i];
       return y;
    }
    else
      return x;
  }
  vector extract_inits_dif(vector x, int d){
    vector [d+1] x1 = extract_vector_left(x,d+1);
    int n = d+1;
    vector[d] y;
    y[1] = x1[1];
    for (i in 1:(d-1)){
     vector[d+1-i] z = dif(x1,i);
     y[i+1] = z[1];
    }
    return y;
  }
  vector inv_dif(vector x, int d,vector x0);
  vector inv_dif(vector x, int d,vector x0){
    int n = num_elements(x);
    vector[d] x1 = complete(x0,d,0.0);
    vector[n+1] y = idif(x,x1[d]);
    if((d-1) == 0 )
      return y;
    else
      return inv_dif(y,d-1,x1);
  }
}
data {
  // Model data
  int<lower=0> n;     // number of data items
  int<lower=0> p;     // number of predictors  ar
  int<lower=0> d;     // number of diferentiations
  int<lower=0> q;     // number of predictions ma
  vector[n] y;        // outcome time series
  // prior data
  vector[4] prior_mu0;    // prior location parameter
  vector[4] prior_sigma0; // prior scale parameter
  matrix[p,4] prior_ar;   // ar location hyper parameters
  matrix[q,4] prior_ma;   // ma location hyper parameters
  int<lower=0> s;         // number of predictors  arch
  int<lower=0> k;         // number of predictions garch
  int<lower=0> h;         // number of predictions mgarch
  matrix[s,4] prior_arch;    // prior arch hyper parameters
  matrix[k,4] prior_garch;   // prior ma hyper parameters
  matrix[h,4] prior_mgarch;   // prior ma hyper parameters
}
transformed data{
  vector [d] inits;
  int m = n - d;
  int tot_param = p+q;
  vector[m] y_dif;
  if(d == 0){
    y_dif = y;
    inits = rep_vector(0,d);
  }
  else{
    y_dif = dif(y,d);
    inits = extract_inits_dif(y,d);
    }
}
parameters{
  vector[p] phi0;          // ar parameters
  vector[q] theta0;        // ma parameters
  real<lower=0> sigma0;    // Variance parameter
  real mu0;                // location parameter
  vector<lower=0,upper=1>[s] alpha; // arch parameters
  vector<lower=0,upper=1>[k] beta;  // garch parameters
  vector [h] mgarch;            // mean garch parameters
}
transformed parameters{
  //***********************************************
  //             Model Parameters
  //***********************************************
   // coefficient parameters
  vector[p] phi;          // ar parameters
  vector[q] theta;        // ma parameters

  // Temporal mean and residuals
  vector[m] mu;                     // Mean Parameter
  vector[m] epsilon;
  vector<lower=0>[m] sigma;         // Variance parameter

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

  for(i in 1:m){
     mu[i] = mu0;
     sigma[i] = sigma0;
    //  ar Estimation
    if(p > 0) for(j in 1:p) if(i > j) mu[i] +=  y_dif[i-j]*phi[j];
    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += epsilon[i-j]*theta[j];
    epsilon[i] = y_dif[i] - mu[i];
    // Garch Iteration
    if(i > k){
       // arch estimation
      if(s > 0) for(j in 1:s) sigma[i] += alpha[j]*pow(epsilon[i-j],2);
       // garch estimation
      if(k > 0) for(j in 1:k) sigma[i] += beta[j]*pow(sigma[i-j], 2);
    }
    sigma[i] = sqrt(sigma[i]);
     // mgarch estimation
     if(h > 0) for(j in 1:h) mu[i] += mgarch[i]*sigma[i-j];
  }
}
model {

  //  prior for \mu0
  if(prior_mu0[4] == 1) target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4] == 4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4] == 5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma
  if(prior_sigma0[4] == 1) target += normal_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 4) target += student_t_lpdf(sigma0|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 5) target += cauchy_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 6) target += inv_gamma_lpdf(sigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 7) target += inv_chi_square_lpdf(sigma0|prior_sigma0[3]);

  // prior ar
  if(p > 0){
    for(i in 1:p){
     if(prior_ar[i,4]==1) target += normal_lpdf(phi0[i]|prior_ar[i,1],prior_ar[i,2]);
     else  target += beta_lpdf(phi0[i]|prior_ar[i,1],prior_ar[i,1]);
    }
  }
  // prior ma
  if(q > 0){
    for(i in 1:q){
      if(prior_ma[i,4]==1) target += normal_lpdf(theta0[i]|prior_ma[i,1],prior_ma[i,2]);
     else  target += beta_lpdf(theta0[i]|prior_ma[i,1],prior_ma[i,1]);
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
    if(prior_mgarch[i,4]==1) target += normal_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==4) target += student_t_lpdf(mgarch[i]|prior_mgarch[i,3],prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==5) target += cauchy_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==2) target += beta_lpdf(mgarch[i]|prior_mgarch[i,1],prior_mgarch[i,2]);
    }
  }
  // Likelihood
  target += normal_lpdf(epsilon|0,sigma);
}
generated quantities{
  real loglik = 0;
  vector[m] log_lik;
  vector[n] fit;
  vector[n] residual;

  for(i in 1:n){
    if(i<=d) residual[i] = 0;
    else residual[i] = epsilon[i-d];
    fit[i] = y[i]-residual[i];
    if(i <=m){
      log_lik[i] = normal_lpdf(y_dif[i]|mu[i],sigma[i]);
      loglik += log_lik[i];
    }
  }
}
