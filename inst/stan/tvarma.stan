functions{
  //     returns the y power of an x vector
  row_vector vecpow(real y,row_vector x, int d){
   row_vector[d] y1;
   for(i in 1:d) y1[i] = pow(x[i],y);
   return y1;
 }
 //     returns a vector with the vech transform
 row_vector vech(int d,int m,matrix y){
  row_vector[m] yk;
  int pos = 1;
  for(j in 1:d){
    for(i in j:d){
      yk[pos] = y[i,j];
      pos =pos +1;
    }
  }
  return yk;
 }
real Jpv(real v){
    real y;
    y =trigamma(v/2) -trigamma((v+1)/2) - 2*(v+3)/(v*(v+1)*(v+1));
    y = (v/(v+3))*y;
    return sqrt(y);
  }
row_vector multi(row_vector x,matrix y){
   return x*y;
 }
}
data {
  int<lower=0> n;         // number of data items
  int<lower=1> d;         // number of dimensions
  int<lower=0> p;         // number of predictors var
  int<lower=0> q;         // number of predictors var
  int<lower=1> m;         // number of dimensions vech
  matrix[n,d] y;          // outcome matrix time series
  vector[4] prior_mu0;    // prior location parameter
  vector[4] prior_sigma0; // prior scale parameter
  matrix[p,4] prior_ar;   // ar location hyper parameters
  matrix[q,4] prior_ma;   // ma location hyper parameters
  int<lower=0> s;         // number of predictors  arch
  int<lower=0> k;         // number of predictions garch
  int<lower=0> h;         // number of predictions mgarch
  matrix[s,4] prior_arch;    // prior arch hyper parameters
  matrix[k,4] prior_garch;   // prior ma hyper parameters
  matrix[h,4] prior_mgarch;  // prior ma hyper parameters
  vector[4]   prior_dfv;     // prior defree freedom genT
}
transformed data{
  //      Design Matrix VAR coefficients
  matrix[d,d] Id = diag_matrix(rep_vector(1.0,d));
  vector[d] zero = rep_vector(0.0,d);
}
parameters{
  //      Parameter VAR Bekk Model
  row_vector[d]mu0;                 // Var constant
  matrix[d,d] phi0[p];              // *temp coefficients VAR
  matrix[d,d] theta0[q];            // *temp coefficients VMA
  cholesky_factor_corr[d] Msigma0;  // *arch constant correlation
  vector<lower=0>[d] vsigma0;       // *arch constant scale
  matrix[d,d] alpha[s];             // arch coefficients
  matrix[d,d] beta[k];              // garch coefficients
  matrix[m,d] mgarch;               // MGARCH coefficients
  vector<lower=1>[d] v;             // Degree fredom
  matrix<lower=0>[n,d] lambda1;     // *freedom degrees Sigma
}
transformed parameters {
  //***********************************************
  //             Model Parameters
  //***********************************************
  // coefficient parameters
  matrix[d,d] phi[p];               // coefficients VAR
  matrix[d,d] theta[q];             // coefficients VMA
  // Temporal mean and residuals
  matrix[n,d] mu;                   // *VAR mean
  matrix[n,d] epsilon;              // *residual mean
  matrix[d,d] sigma0;               // arch constant
  matrix[d,d] sigma1;               // arch constant
  matrix[d,d] sigma[n];             // *covariance matrix sigma
  matrix[d,d] Lsigma[n];            // *Cholesky descomposition sigma
  row_vector[m] vsigma[n];          // *vech sigma
  matrix<lower=0>[n,d] lambda;      //  Generalized t-student parameter

  //***********************************************
  //         Transformation coeficients
  //***********************************************

  for( o in 1:p){
    if(prior_ar[o,4]== 1) phi[o] = phi0[o];
    else phi[o] = 2*phi0[o] - 1;
  }
  for(o in 1:q){
    if(prior_ma[o,4] == 1) theta[o] = theta0[o];
    else theta[o] = 2*theta0[o]-rep_matrix(1.0,d,d);
  }

  //***********************************************
  //      Sigma0 transformation
  //***********************************************

  sigma0 = diag_pre_multiply(vsigma0,Msigma0);
  sigma1= multiply_lower_tri_self_transpose(sigma0);

  //***********************************************
  //         VARMA estimations
  //***********************************************

  for(i in 1:n){
    //    Degrees freedom
    lambda[i]= vecpow(-1.0,lambda1[i],d);
    //  VAR Iteration
    mu[i] = mu0;
    sigma[i] = sigma1;
    for(j in 1:p) if(i > j) mu[i] += y[i-j]*phi[j];
    // ma estimation
    if(q > 0) for(j in 1:q) if(i > j) mu[i] += epsilon[i-j]*theta[j];
    epsilon[i] = y[i] - mu[i];
    //      Bekk Iteration
    if(s >= k){
       // arch estimation
      if(s > 0) for(j in 1:s)if(i>j) sigma[i] += quad_form(epsilon[i-j]'*epsilon[i-j],alpha[j]);
       // garch estimation
      if(k > 0) for(j in 1:k)if(i>j) sigma[i] += quad_form(sigma[i-j],beta[j]);
    }
    Lsigma[i]=cholesky_decompose(quad_form_diag(sigma[i],vecpow(0.5,lambda1[i],d)));
    vsigma[i] = vech(d,m,sigma[i]);
    // mgarch estimation
     if(h > 0) mu[i] = mu[i] + multi(vsigma[i],mgarch);
  }
}
model{
  //      Priors  definition
  //  prior for \mu0
  if(prior_mu0[4] == 1) target += normal_lpdf(mu0|prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4] == 4) target += student_t_lpdf(mu0|prior_mu0[3],prior_mu0[1],prior_mu0[2]);
  else if(prior_mu0[4] == 5) target += cauchy_lpdf(mu0|prior_mu0[1],prior_mu0[2]);

  // Prior sigma0 diagonal vector
  if(prior_sigma0[4] == 1) target += normal_lpdf(vsigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 4) target += student_t_lpdf(vsigma0|prior_sigma0[3],prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 5) target += cauchy_lpdf(vsigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 6) target += inv_gamma_lpdf(vsigma0|prior_sigma0[1],prior_sigma0[2]);
  else if(prior_sigma0[4] == 7) target += inv_chi_square_lpdf(vsigma0|prior_sigma0[3]);
  else if(prior_sigma0[4] == 9) target += gamma_lpdf(vsigma0|prior_sigma0[1],prior_sigma0[2]);
  //   sigma0 constant correlation Matrix
  target += lkj_corr_cholesky_lpdf(Msigma0|2.0);

  // prior ar
  if(p > 0){
    for(i in 1:p){
     if(prior_ar[i,4]==1) target += normal_lpdf(to_vector(phi[i])|prior_ar[i,1],prior_ar[i,2]);
     else  target += beta_lpdf(to_vector(phi[i])|prior_ar[i,1],prior_ar[i,1]);
    }
  }
  // prior ma
  if(q > 0){
    for(i in 1:q){
     if(prior_ma[i,4]==1) target += normal_lpdf(to_vector(theta[i])|prior_ma[i,1],prior_ma[i,2]);
     else  target += beta_lpdf(to_vector(theta[i])|prior_ma[i,1],prior_ma[i,1]);
    }
  }
  // prior arch
  if(s > 0){
    for(i in 1:s){
     if(prior_arch[i,4]==1) target += normal_lpdf(to_vector(alpha[i])|prior_arch[i,1],prior_arch[i,2]);
     else target += beta_lpdf(to_vector(alpha[i])|prior_arch[i,1],prior_arch[i,2]);
    }
  }
  // prior garch
  if(k > 0){
    for(i in 1:k){
     if(prior_garch[i,4]==1) target += normal_lpdf(to_vector(beta[i])|prior_garch[i,1],prior_garch[i,2]);
     else target += beta_lpdf(to_vector(beta[i])|prior_garch[i,1],prior_garch[i,2]);
    }
  }
   // prior mean_garch
  if(h > 0){
    for(i in 1:h){
    if(prior_mgarch[i,4]==1) target += normal_lpdf(to_vector(mgarch[i])|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==4) target += student_t_lpdf(to_vector(mgarch[i])|prior_mgarch[i,3],prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==5) target += cauchy_lpdf(to_vector(mgarch[i])|prior_mgarch[i,1],prior_mgarch[i,2]);
    else if(prior_mgarch[i,4]==2) target += beta_lpdf(to_vector(mgarch[i])|prior_mgarch[i,1],prior_mgarch[i,2]);
    }
  }
  // Prior dfv
  for(i in 1:d){
  if(prior_dfv[4] == 1) target += normal_lpdf(v[i]|prior_dfv[1],prior_dfv[2]);
  else if(prior_dfv[4] == 9) target += gamma_lpdf(v[i]|prior_dfv[1],prior_dfv[2]);
  else if(prior_dfv[4] == 8) target += log(Jpv(v[i]));
  else if(prior_dfv[4] == 6) target += inv_gamma_lpdf(v[i]|prior_dfv[1],prior_dfv[2]);
   }
  //      Likelihood
  for(i in 1:n){
   //    Generalized t-student
   for(j in 1:d) target+= inv_gamma_lpdf(lambda1[i,j]|v[j]/2,v[j]/2);
   //    Observations
   target += multi_normal_cholesky_lpdf(epsilon[i]| zero, Lsigma[i] );
  }
}
generated quantities{
  real loglik = 0;
  vector[m] log_lik;
  matrix[n,d] fit;
  matrix[n,d] residual;

  for(i in 1:n){
    fit[i] = multi_normal_cholesky_rng(mu[i],Lsigma[i])';
    residual[i] = y[i]-residual[i];
    fit[i] = y[i]-residual[i];
    if(i <=m){
      log_lik[i] = multi_normal_cholesky_lpdf(y[i]|mu[i],Lsigma[i]);
      loglik += log_lik[i];
    }
  }
}
