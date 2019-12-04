// Microclimates analysis
// 4 June 2019 - Started by Cat
// Looking at the effects of temperature, provenance, etc on phenophase intervals
// Level: Species on INTERCEPTS and SLOPES
// This model does not have interactions 

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] mst; 	// predictor
	vector[N] lo; 	// predictor
	vector[N] lat; 	// predictor
		
	}

/*
transformed data {
  vector[N] inter_txchill1;
  vector[N] inter_txchill2;

  inter_txchill1    = tx .* chill1;
  inter_txchill2    = tx .* chill2;
}
*/

parameters {
  real mu_a_sp;   
  real mu_b_mst_sp;   
  real mu_b_lo_sp;   
  real mu_b_lat_sp;
  //real mu_b_txchill1_sp;   
  //real mu_b_txchill2_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_mst_sp; 
  real<lower=0> sigma_b_lo_sp; 
  real<lower=0> sigma_b_lat_sp;
  //real<lower=0> sigma_b_txchill1_sp;
  //real<lower=0> sigma_b_txchill2_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_mst[n_sp]; // slope of forcing effect 
  real b_lo[n_sp]; // slope of photoperiod effect
  real b_lat[n_sp]; // slope of chill effect
  //vector[n_sp] b_txchill1_ncp; // slope of lat effect
  //real b_txchill1[n_sp]; // slope of chill x force effect
  //real b_txchill2[n_sp]; // slope of chill x force effect

	}

transformed parameters {
  //vector[n_sp] b_txchill1; // slope of interaction 
  vector[N] yhat;
  
  //b_txchill1 = mu_b_txchill1_sp + sigma_b_txchill1_sp*b_txchill1_ncp;
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_mst[sp[i]] * mst[i] +
		  b_lo[sp[i]] * lo[i] +
		b_lat[sp[i]] * lat[i];
		  //b_chill2[sp[i]] * chill2[i] +
		//b_txchill2[sp[i]] * inter_txchill2[i];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_mst ~ normal(mu_b_mst_sp, sigma_b_mst_sp); 
	b_lo ~ normal(mu_b_lo_sp, sigma_b_lo_sp); 
	b_lat ~ normal(mu_b_lat_sp, sigma_b_lat_sp);
	//b_txchill1 ~ normal(mu_b_txchill1_sp, sigma_b_txchill1_sp);
	//b_txchill2 ~ normal(mu_b_txchill2_sp, sigma_b_txchill2_sp); 
	
	//b_txchill1_ncp ~ normal(0, 5);

        mu_a_sp ~ normal(0, 200);
        sigma_a_sp ~ normal(0, 50);

        mu_b_mst_sp ~ normal(0, 200);
        sigma_b_mst_sp ~ normal(0, 50);
        mu_b_lo_sp ~ normal(0, 200);
        sigma_b_lo_sp ~ normal(0, 50);
        mu_b_lat_sp ~ normal(0, 200);
        sigma_b_lat_sp ~ normal(0, 50);
        //mu_b_txchill1_sp ~ normal(0, 50);
        //sigma_b_txchill1_sp ~ normal(0, 10);

        //mu_b_txchill2_sp ~ normal(0, 50);
        //sigma_b_txchill2_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

/*
generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_tx[sp[n]] * tx[n] + 
	      	b_chill1[sp[n]] * chill1[n] +
		b_chill2[sp[n]] * chill2[n] +
		      b_txchill1[sp[n]] * inter_txchill1[n] +
		b_txchill2[sp[n]] * inter_txchill2[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}
*/

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_rng(y[n], sigma_y);
    
  }
}
