data {
  // data to model
  int<lower=0> n_dates_age;         // number case dates
  int<lower=1> n_ages;          // number of age groups
  int<lower=1> n_strains;       // number of strains
  vector[n_ages] population;                                   // population in eah age group
  int<lower=0> survey_age_sample_size[n_dates_age,n_ages];     // number of samples per age group at each sample point
  int<lower=0> survey_age_positive_tests[n_dates_age,n_ages];  // number of positive tests per age group at each sample point
  int<lower=0> survey_strain[n_dates_age,n_strains];           // number of cases by strain
  matrix[n_ages,n_ages] contact_matrix; // contact matrix of number of individuals 
  int<lower=0> data_freq_age;           // freqency of data points (i.e. weekly data then data_freq_age = 7)

  // seed times
  int<lower=0> seed_start_time[n_strains]; // time when a strain is started to be seeded
  int<lower=0> seed_end_time[n_strains];   // time when a strain is stopped being seeded
  matrix[n_ages,n_strains] seed_strain_age_factor; // controls fraction of seeing events by age
  real prior_daily_seed_max[n_strains];    // maximum prior on daily seed
  
  // fixed model parameters
  real<lower=2> generation_mean[n_strains]; // mean generation time

  // priors on other parameters
  real prior_log_R0_min;            // minimum of prior of R0
  real prior_log_R0_max;            // maximum of prior of R0
  real prior_log_R0_age_min;        // minimum of prior of R0 for age
  real prior_log_R0_age_max;        // maximum of prior of R0 for age
  real prior_dR_sd_min;         // minimum of prior on sd of dR (log-normal process)    
  real prior_dR_age_sd_max;     // minimum of prior on sd of dR (log-normal process)
  real prior_dR_age_sd_min;     // minimum of prior on sd of dR for age (log-normal process)    
  real prior_dR_sd_max;         // minimum of prior on sd of dR for age (log-normal process)    
  real prior_R_age_exp_min;     // minimum of prior o R_age_exp
  real prior_R_age_exp_max;     // maximum of prior o R_age_exp
  real prior_strain_multipliers_min[n_strains-1]; // minimum prior on strain multipliers
  real prior_strain_multipliers_max[n_strains-1]; // minimum prior on strain multipliers
  real prior_dR_max;            // maximum absolute value dR and dR age can be
}

// transformed generation data
transformed data{
  // number of R and infection days modelled and corresponding to data points
  int<lower=0> n_R = ( n_dates_age - 1 ) * data_freq_age;
  int<lower=0> n_strain_multipliers;
  vector[ n_ages ] inf0_data[n_strains];
  real survey_strain_tot0;
  real<lower=0> k_trans[n_strains];
  real<lower=0> k_trans_1[n_strains];
  vector[ ( n_dates_age - 1 ) * data_freq_age ] seed_mask[n_strains];
  int t_survey_age_date[n_R];
  matrix[n_ages,n_dates_age] survey_age_positive_tests_m = to_matrix( survey_age_positive_tests )';     
  matrix[n_ages,n_dates_age] survey_age_negative_tests_m = to_matrix( survey_age_sample_size )' - to_matrix( survey_age_positive_tests )';     
  matrix[n_strains,n_dates_age] survey_strain_m = to_matrix( survey_strain )';
  
  // convert the generation time into to probability of a transition from E->I and I->R
  for( sdx in 1:n_strains ) {
    k_trans[ sdx ]   = 1 - exp( -2 / generation_mean[ sdx ] );
    k_trans_1[ sdx ] = 1 - k_trans[ sdx ];
  }

  // base strain has a multipler of 1
  n_strain_multipliers = n_strains - 1;
  
  // get the round of the survey_age to which day contributes
  for( idx in 2:n_dates_age )
    for( jdx in 1:data_freq_age)
      t_survey_age_date[ (idx-2)*data_freq_age + jdx ] = idx;
      
  // initial cases (assuming no correlation between )
  survey_strain_tot0 = sum( survey_strain_m[ ,1 ] );
  for( sdx in 1:n_strains ) {
    inf0_data[sdx] = survey_age_positive_tests_m[ ,1 ] ./ ( survey_age_positive_tests_m[ ,1 ] + survey_age_negative_tests_m[ ,1 ] );
    inf0_data[sdx] = inf0_data[sdx] * survey_strain_m[ sdx, 1 ] / survey_strain_tot0;
    inf0_data[sdx] = inf0_data[sdx] .* population / 2;
  }
  
  // calculate seed daily mask
  for( sdx in 1:n_strains ) {
    seed_mask[ sdx ] = rep_vector( 0, n_R );
    for( t in 1:n_R ) {
      if( t >= seed_start_time[ sdx ] && t <= seed_end_time[ sdx ] )
        seed_mask[ sdx ][ t ] = 1;
    }
  }
}

// fitted paramters
parameters {
  real<lower=prior_log_R0_min,upper=prior_log_R0_max> log_R0;
  real<lower=prior_log_R0_age_min,upper=prior_log_R0_age_max> log_R0_age[n_ages];
  real<lower=prior_dR_sd_min,upper=prior_dR_sd_max> dR_sd; 
  real<lower=prior_dR_age_sd_min,upper=prior_dR_age_sd_max> dR_sd_age; 
  row_vector<lower=-prior_dR_max,upper=prior_dR_max>[ n_dates_age ] dR;
  matrix<lower=-prior_dR_max,upper=prior_dR_max>[ n_ages,n_dates_age ] dR_age;
  real<lower=0,upper=1> strain_multipliers_raw[n_strain_multipliers];
  real<lower=0,upper=1> daily_seed_raw[n_strains];
  real<lower=prior_R_age_exp_min,upper=prior_R_age_exp_max> R_age_exp;
}

// transformed parameters
transformed parameters{
  row_vector[n_dates_age] R;
  matrix[ n_ages,n_dates_age ] R_age;
  matrix[ n_ages,n_dates_age ] R_age_L;
  real sum_square_R;
  real sum_square_R_age;
  vector[ n_ages ] infected;
  vector[ n_ages ] infected_contacts;
  vector[ n_ages ] exposed;
  vector[ n_ages ] new_cases;
  vector[ n_strains ] exposed_strains;
  matrix[ n_ages,n_dates_age] positivity_age_expected = rep_matrix(0, n_ages,n_dates_age );
  matrix[ n_strains,n_dates_age] positivity_strain_expected = rep_matrix(0, n_strains,n_dates_age );
  real<lower=0> strain_multipliers[n_strains];
  real<lower=0> new_infection_multiplier;
  vector[ n_R ] daily_seed;
  vector[ n_ages ] seed_age_factor;

  // inflate the raw strain multipliers base on the given priors
  strain_multipliers[1 ] = 1; 
  for( sdx in 1:n_strain_multipliers )
    strain_multipliers[sdx + 1] = prior_strain_multipliers_min[sdx] + 
      (prior_strain_multipliers_max[sdx]- prior_strain_multipliers_min[sdx]) * strain_multipliers_raw[sdx];
  
  for( sdx in 2:n_strains )
    strain_multipliers[sdx]  = strain_multipliers[sdx] * strain_multipliers[sdx-1];
  
  // calculate R as a log-process with jumps
  R     = dR;
  R[1]  = log_R0;
  
  R_age = dR_age;
  for( age in 1:n_ages ) {
    R_age[ age, 1 ] = log_R0_age[ age ];
    R_age[ age, ] = cumulative_sum( R_age[ age, ] ) + R;
  }
  R_age_L = exp( (1 - R_age_exp ) * R_age );
  R_age   = exp( R_age_exp * R_age );

  // calculate infection series from R series
  for( sdx in 1:n_strains ) {
    // calculate the seeding schedule
    daily_seed = seed_mask[ sdx ] * daily_seed_raw[ sdx ] * prior_daily_seed_max[ sdx ];
    seed_age_factor = seed_strain_age_factor[ , sdx ];
    
    infected = inf0_data[sdx];
    exposed  = infected;
    new_infection_multiplier = k_trans[ sdx ] * strain_multipliers[sdx];

    for( tdx in 1:n_R)
    {
        // calculate the new infections using the contact matrix
        infected_contacts = infected .* R_age[ , t_survey_age_date[tdx] ];
        infected_contacts = ( contact_matrix * infected_contacts ) .* R_age_L[ , t_survey_age_date[tdx] ];
        infected_contacts *= new_infection_multiplier ;
        infected_contacts += daily_seed[ tdx ] * seed_age_factor;

        // update the number of infected and exposed
        new_cases = exposed * k_trans[ sdx ];
        infected  = k_trans_1[ sdx ] * infected + new_cases;
        exposed   = exposed - new_cases + infected_contacts;
        
        // calculate the positivity and the new cases
        positivity_age_expected[,t_survey_age_date[tdx]] += infected + exposed;
        positivity_strain_expected[sdx,t_survey_age_date[tdx]] += sum( infected  ) + sum( exposed );
    }
  }
  
  for( t in 2:n_dates_age) {
    positivity_age_expected[,t] = ( positivity_age_expected[,t] ./ ( population * data_freq_age + positivity_age_expected[,t]) ) + 1e-6;
    positivity_strain_expected[,t] = ( positivity_strain_expected[,t] + 1e-6 ) / sum( positivity_strain_expected[,t]  + 1e-6); 
  }
  positivity_age_expected[,1]    = survey_age_positive_tests_m[,1] ./ (  survey_age_positive_tests_m[,1] + survey_age_negative_tests_m[,1] );
  positivity_strain_expected[,1] = ( survey_strain_m[ , 1] + 1e-6 ) / sum( survey_strain_m[, 1 ] + 1e-6 );      
        
  // clean up for output
  R_age = ( contact_matrix' * R_age_L) .* R_age;
}

model {
  // priors
  // dR_sd has a range prior
  // dR_sd_age has a range prior
  // R0 has a range prior
  // R0_age has a range prior
  // jumps have a range prior

  // infection process model (log normal)
  dR ~ normal( -0.5*dR_sd*dR_sd, dR_sd);
  for( age in 1:n_ages ) {
    dR_age[ age,] ~ normal( -0.5*dR_sd_age *dR_sd_age, dR_sd_age);
  }
  
  target += sum( survey_strain_m .* log( positivity_strain_expected ) );
  target += sum( survey_age_positive_tests_m .* log( positivity_age_expected ) );
  target += sum( survey_age_negative_tests_m .* log1m( positivity_age_expected ) );
}

