// Microclimates analysis
// 4 June 2019 - Started by Cat
// Looking at the effects of temperature, provenance, etc on phenophase intervals
// Level: Species on INTERCEPTS and SLOPES
// This model does not have interactions 

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	//int<lower=1> n_site;
	//int<lower=1, upper=n_site> site[N];
	vector[N] y; 		// response
	vector[N] dvrtemp; 	// predictor
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
  //real mu_a_site; 
  real mu_b_dvrtemp_sp;   
  real mu_b_lat_sp;    
  real<lower=0> sigma_a_sp; 
  //real<lower=0> sigma_a_site; 
  real<lower=0> sigma_b_dvrtemp_sp; 
  real<lower=0> sigma_b_lat_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  //real a_site[n_site]; // intercept for site
  real b_dvrtemp[n_sp]; // slope of forcing effect 
  real b_lat[n_sp]; // slope of photoperiod effect

	}

transformed parameters {
  vector[N] yhat;
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
            //a_site[site[i]] +
		b_dvrtemp[sp[i]] * dvrtemp[i] +
		b_lat[sp[i]] * lat[i];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	//a_site ~ normal(mu_a_site, sigma_a_site); 
	b_dvrtemp ~ normal(mu_b_dvrtemp_sp, sigma_b_dvrtemp_sp); 
	b_lat ~ normal(mu_b_lat_sp, sigma_b_lat_sp);

        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
        //mu_a_site ~ normal(0, 50);
        //sigma_a_site ~ normal(0, 10);

        mu_b_dvrtemp_sp ~ normal(0, 50);
        sigma_b_dvrtemp_sp ~ normal(0, 10);
        mu_b_lat_sp ~ normal(0, 50);
        sigma_b_lat_sp ~ normal(0, 10);

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
