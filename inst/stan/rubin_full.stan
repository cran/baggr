functions {
#include /functions/prior_increment.stan
}

data {
  // SHARED ACROSS FULL MODELS:
  int<lower=0> N;  // total number of observations
  int<lower=0> K;  // number of sites
  int<lower=0> Nc; //number of covariates (fixed effects)
  matrix[N,Nc] X;  //covariate values (design matrix for FE)
  int pooling_type; //0 if none, 1 if partial, 2 if full
  int pooling_baseline; //pooling for proportions in control arm;
                        //0 if none, 1 if partial
  int<lower=0,upper=K> site[N];
  vector<lower=0,upper=1>[N] treatment;

  //priors for baseline parameters
  int prior_control_fam;
  int prior_control_sd_fam;
  vector[3] prior_control_val;
  vector[3] prior_control_sd_val;
  //priors for effects::
  int prior_hypermean_fam;
  int prior_hypersd_fam;
  int prior_beta_fam;
  vector[3] prior_hypermean_val;
  vector[3] prior_hypersd_val;
  vector[3] prior_beta_val;
  //priors for regression model:
  int prior_sigma_fam;
  vector[3] prior_sigma_val;

  //cross-validation variables:
  int<lower=0> N_test;
  int<lower=0> K_test;
  matrix[N_test, Nc] X_test;
  int<lower=0, upper=K> test_site[N_test];
  int<lower=0, upper=1> test_treatment[N_test];

  // NORMAL specific:
  real y[N];
  real test_y[N_test];
  real test_sigma_y_k[K_test];

}
transformed data {
  int K_pooled; // number of modelled sites if we take pooling into account
  if(pooling_type == 2)
    K_pooled = 0;
  if(pooling_type != 2)
    K_pooled = K;
}
parameters {
  // SHARED ACROSS FULL MODELS:
  real mu_baseline[pooling_baseline != 0? 1: 0];
  real mu[pooling_type != 0? 1: 0];
  real<lower=0> tau_baseline[pooling_baseline != 0? 1: 0];
  real<lower=0> tau[pooling_type == 1? 1: 0];
  vector[K_pooled] eta;
  vector[K] eta_baseline;
  vector[Nc] beta;

  // NORMAL specific:
  vector<lower=0>[K] sigma_y_k;
}
transformed parameters {
  vector[K_pooled] theta_k;
  vector[K] baseline_k;

  if(pooling_type == 0)
    theta_k = eta;
  else if(pooling_type == 1)
    theta_k = rep_vector(mu[1], K_pooled) + tau[1]*eta;

  if(pooling_baseline == 0)
    baseline_k = eta_baseline;
  else if(pooling_baseline == 1)
    baseline_k = rep_vector(mu_baseline[1], K) + tau_baseline[1]*eta_baseline;
}
model {
  // SHARED ACROSS FULL MODELS:
  vector[N] fe;
  if(N > 0){
    if(Nc == 0)
      fe = rep_vector(0.0, N);
    else
      fe = X*beta;
  }

  //controls/baselines (hyper)priors
  if(pooling_baseline == 0)
    target += prior_increment_vec(prior_control_fam, eta_baseline, prior_control_val);
  if(pooling_baseline == 1){
    eta_baseline ~ normal(0,1);
    target += prior_increment_real(prior_control_fam, mu_baseline[1], prior_control_val);
    target += prior_increment_real(prior_control_sd_fam, tau_baseline[1], prior_control_sd_val);
  }

  //hypermean priors:
  if(pooling_type > 0)
    target += prior_increment_real(prior_hypermean_fam, mu[1], prior_hypermean_val);
  else{
    for(k in 1:K)
      target += prior_increment_real(prior_hypermean_fam, eta[k], prior_hypermean_val);
  }

  //hyper-SD priors:
  if(pooling_type == 1)
    target += prior_increment_real(prior_hypersd_fam, tau[1], prior_hypersd_val);

  //fixed effect coefficient priors
  if(Nc > 0)
    target += prior_increment_vec(prior_beta_fam, beta, prior_beta_val);

  //
  if(pooling_type == 1)
    eta ~ normal(0,1);


  // NORMAL specific:
  // error term priors
  target += prior_increment_vec(prior_sigma_fam, sigma_y_k, prior_sigma_val);
  // likelihood
  for(i in 1:N){
    if(pooling_type < 2)
      y[i] ~ normal(baseline_k[site[i]] + theta_k[site[i]] * treatment[i] + fe[i], sigma_y_k[site[i]]);
    if(pooling_type == 2)
      y[i] ~ normal(baseline_k[site[i]] + mu[1] * treatment[i] + fe[i], sigma_y_k[site[i]]);
  }
}
generated quantities {
  // to do this, we must first (outside of Stan) calculate SEs in each test group,
  // i.e. test_sigma_y_k
  real logpd[K_test > 0? 1: 0];
  vector[N_test] fe_test;
  if(K_test > 0){
    if(Nc == 0)
      fe_test = rep_vector(0.0, N_test);
    else
      fe_test = X_test*beta;
    logpd[1] = 0;
    for(i in 1:N_test){
      if(pooling_type == 1)
        logpd[1] += normal_lpdf(test_y[i] | baseline_k[test_site[i]] + mu[1] * test_treatment[i] + fe_test[i],
                                sqrt(tau[1]^2 + test_sigma_y_k[test_site[i]]^2));
      if(pooling_type == 2)
        logpd[1] += normal_lpdf(test_y[i] | baseline_k[test_site[i]] + mu[1] * test_treatment[i] + fe_test[i],
                                sqrt(test_sigma_y_k[test_site[i]]^2));
    }
  }
}
