// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] prov; 	// provenance predictor
	vector[N] method; 	// method predictor
		
	}

parameters {
  
  real mu_a_sp;   
  real mu_b_prov_sp;     
  real mu_b_method_sp;
  
  real<lower=0> sigma_b_prov_sp;
  real<lower=0> sigma_b_method_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  real b_prov[n_sp];
  real b_method[n_sp];
  
	}

transformed parameters {
  vector[N] yhat;
  
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_prov[sp[i]] * prov[i] + 
		          b_method[sp[i]] * method[i];
	      }
	      
}

model {
	      
    mu_a_sp ~ normal(400, 50); 
    sigma_a_sp ~ normal(0, 50); 

    mu_b_prov_sp ~ normal(0, 20);
    sigma_b_prov_sp ~ normal(0, 10);
    
    mu_b_method_sp ~ normal(0, 50);
    sigma_b_method_sp ~ normal(0, 20);
    
    sigma_y ~ normal(0, 20);
        
        a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	
	      b_prov ~ normal(mu_b_prov_sp, sigma_b_prov_sp);
        b_method ~ normal(mu_b_method_sp, sigma_b_method_sp);
	      
	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_prov[sp[n]] * prov[n] +
		b_method[sp[n]] * method[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}