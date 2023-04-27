data {
  int<lower=0> n;
  int<lower=0> p;
  matrix[n, p] est;
  matrix[n, p] se;
}
parameters {
  vector[n] beta;
  real<lower=0> sigma_beta;
  real<lower=0> eta;
  vector<lower=0>[p] sigma_bias;
  corr_matrix[p] Rho;
}
model {
  matrix[p, p] Omega;
  
  sigma_beta ~ inv_gamma(1, 0.02);
  eta ~ gamma(2, 2);
  for (j in 1:p) {
    sigma_bias[j] ~ inv_gamma(1, 0.02);
  }
  Rho ~ lkj_corr(1);
  
  beta ~ normal(0, sigma_beta);
  
  for (i in 1:n) {
    Omega = eta * diag_post_multiply(diag_pre_multiply(se[i], Rho), se[i]);
    Omega = Omega + diag_matrix(square(sigma_bias));
    est[i] ~ multi_normal(to_vector(rep_array(beta[i], p)), Omega);
  }
}
