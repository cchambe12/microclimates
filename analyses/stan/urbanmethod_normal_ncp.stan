// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] tx; 	// urban predictor
	vector[N] method; 	// method predictor
		
	}

parameters {
  
  real mu_a_sp;   
  real mu_b_tx_sp;     
  real mu_b_method_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_tx_sp;
  real<lower=0> sigma_b_method_sp;
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  
  vector[n_sp] b_tx_ncp; // slope of urban effect 
  vector[n_sp] b_method_ncp; // slope of method effect 
  
	}

transformed parameters {
  vector[N] yhat;

  vector[n_sp] b_tx = mu_b_tx_sp + sigma_b_tx_sp*b_tx_ncp; 
  vector[n_sp] b_method = mu_b_method_sp + sigma_b_method_sp*b_method_ncp; 
  
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_tx[sp[i]] * tx[i] + 
		          b_method[sp[i]] * method[i];
	      }
	      
}

model {
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_tx_ncp ~ normal(0, 1);
	b_method_ncp ~ normal(0, 1);
	      
        mu_a_sp ~ normal(400, 50);
        sigma_a_sp ~ normal(0, 50);

        mu_b_tx_sp ~ normal(0, 75);
        sigma_b_tx_sp ~ normal(0, 20);
        
        mu_b_method_sp ~ normal(0, 150);
        sigma_b_method_sp ~ normal(0, 20);
        
        sigma_y ~ normal(0, 50);
	      
	y ~ normal(yhat, sigma_y);

}

/*generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_tx[sp[n]] * tx[n] +
		b_method[sp[n]] * method[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}*/