// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] tx; 	// urban predictor
	vector[N] prov; 	// provenance predictor
	
	}

parameters {
  real gdd;
  vector[n_sp] burb;
  vector[n_sp] bprov;
  vector[n_sp] a;
  vector[3] mu_species;
  real<lower=0> sigma;
  vector<lower=0>[3] sigma_species;
  corr_matrix[3] Rho;
  
  /*
  real mu_a_sp;   
  real mu_b_tx_sp;     
  real mu_b_prov_sp;     
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_tx_sp;
  real<lower=0> sigma_b_prov_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  //real b_tx[n_sp]; // slope of urban effect 
  //real b_prov[n_sp]; // slope of provenance effect 
  vector[n_sp] b_tx_ncp; // NCP slope of urban effect 
  vector[n_sp] b_prov_ncp; // NCP slope of provenance effect
  */

	}

transformed parameters {
  //vector[n_sp] b_tx; 
  //vector[n_sp] b_prov;
  //vector[N] yhat;

   //b_tx = mu_b_tx_sp + sigma_b_tx_sp*b_tx_ncp;
   //b_prov = mu_b_prov_sp + sigma_b_prov_sp*b_prov_ncp;
   
  vector[3] v_aburbbprov[n_sp];
  cov_matrix[3] SRS_sigma_speciesRho;
  for ( j in 1:n_sp ) {
        v_aburbbprov[j,1] = a[j];
        v_aburbbprov[j,2] = burb[j];
        v_aburbbprov[j,3] = bprov[j];
    }
    SRS_sigma_speciesRho = quad_form_diag(Rho,sigma_species);
   

  /*
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_tx[sp[i]] * tx[i] + 
		b_prov[sp[i]] * prov[i];
	*/
  }


model {

  vector[N] mu;
  Rho ~ lkj_corr( 3 );
  
	sigma_species ~ normal( 0 , 200 );
    sigma ~ normal( 0 , 100 );
    mu_species ~ normal( 0 , 400 );
    v_aburbbprov ~ multi_normal( mu_species , SRS_sigma_speciesRho );
    for ( i in 1:N ) {
        mu[i] = a[sp[i]] + burb[sp[i]] * tx[i] + bprov[sp[i]] * prov[i];
    }
    gdd ~ normal( mu , sigma );

}

generated quantities{
    vector[N] mu;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        mu[i] = a[sp[i]] + burb[sp[i]] * tx[i] + bprov[sp[i]] * prov[i];
    }
}