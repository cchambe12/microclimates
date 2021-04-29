// Microclimates Analysis - Prior Predictive Checks
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  int<lower=0, upper=1000> N;
  vector[N] urban;
  vector[N] method;
}

parameters{
  real mu_a_sp;
  real mu_b_urban_sp;
  real mu_b_method_sp;
  real<lower=0> sigma_y;
  
}

model{
  mu_a_sp ~ normal(400, 75);
  mu_b_urban_sp ~ normal(0, 75);
  mu_b_method_sp ~ normal(0, 75);
  sigma_y ~ normal(0, 100);
}

generated quantities{
    real y_ppc[N];
    y_ppc = normal_rng(mu_a_sp + 
		  mu_b_urban_sp * urban +
		  mu_b_method_sp * method, sigma_y);

}