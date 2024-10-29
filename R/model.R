###################################################################################/
# model.survey.fit
###################################################################################/
model.survey.fit = function( 
    # the data the model will used for the fit
  data_survey_age     = data.infection_survey(),
  data_contact_matrix = data.contact_matrix(),
  data_population     = data.population(),
  data_variant_survey = data.variants(),
  
  # the fixed parameters in the model
  param_generation_time  = c( 5.5, 5.5),
  param_seed_start_dates = c( as.Date( "2021-01-31"), as.Date( "2021-03-20") ),
  param_seed_end_dates   = c( as.Date( "2021-01-31"), as.Date( "2021-04-20") ),
  
  # priors on the fitted parameters
  prior_log_R0_min     = log( 0.8 ),
  prior_log_R0_max     = log( 1.2 ),
  prior_log_R0_age_min = log( 0.4 ),
  prior_log_R0_age_max = log( 2.5 ),
  prior_dR_sd_min      = 0.001,
  prior_dR_sd_max      = 0.5,
  prior_dR_age_sd_min  = 0.001,
  prior_dR_age_sd_max  = 0.4,
  prior_R_age_exp_min  = 0.01,
  prior_R_age_exp_max  = 0.99,
  prior_dR_max         = 0.7,
  prior_daily_seed_max = c( 1, 50 ),
  prior_strain_multipliers_min = 1,
  prior_strain_multipliers_max = 3,
  
  # parameters used by Stan for the fit
  stan_n_iter        = 5e2,
  stan_n_chains      = 3L,
  stan_max_treedepth = 12L ,
  stan_adapt_delta   = 0.90
) {
  # the contact matrix is normalised by princinple eigenvalue so R is meaningful
  data_contact_matrix <- data_contact_matrix / max( eigen( data_contact_matrix)$values )
  
  # get the basic sizes of the data 
  data_params <- list(
    start_date    = as.Date( data_survey_age$sample_size[ , min( date ) ] ),
    end_date      = as.Date( data_survey_age$sample_size[ , max( date ) ] ),  
    dates         = as.Date( data_survey_age$sample_size[ , date ] ),  
    n_dates_age   = as.integer( data_survey_age$sample_size[ ,.N ] ),
    age_cols      = rownames( data_contact_matrix ),
    n_ages        = nrow( data_contact_matrix ),
    variants      = names( data_variant_survey )[ -1 ],
    n_strains     = length( names( data_variant_survey )[ -1 ] ),
    data_freq_age = as.integer( data_survey_age$sample_size[ 2, date ] - data_survey_age$sample_size[ 1, date ] )
  )
  
  # seed equally 
  seed_strain_age_factor <- matrix( data_population[,population] / data_population[,sum(population)], 
                                    nrow = data_params$n_ages, ncol = data_params$n_strains  )
  
  # build the data object for stan
  stan_data = c( data_params, list(
    # the survey data to fit
    contact_matrix = data_contact_matrix,
    population     = data_population[ , population ],
    survey_age_sample_size    = round( as.matrix( data_survey_age$sample_size[ , -1 ] ) ),
    survey_age_positive_tests = round( as.matrix( data_survey_age$positive_tests[ , -1 ] ) ),
    survey_strain  = as.matrix( data_variant_survey[ , -1 ] ),
    
    # seeding windows (convert dates to days from start_date)
    seed_start_time = as.integer( as.Date( param_seed_start_dates ) - data_params$start_date ),
    seed_end_time   = as.integer( as.Date( param_seed_end_dates )   - data_params$start_date ),
    seed_strain_age_factor = seed_strain_age_factor,
    
    # fixed parameter - mean generation time
    generation_mean = param_generation_time,
    
    # priors
    prior_log_R0_min     = prior_log_R0_min,
    prior_log_R0_max     = prior_log_R0_max,
    prior_log_R0_age_min = prior_log_R0_age_min,
    prior_log_R0_age_max = prior_log_R0_age_max,
    prior_dR_sd_min      = prior_dR_sd_min,
    prior_dR_sd_max      = prior_dR_sd_max,
    prior_dR_age_sd_min  = prior_dR_age_sd_min,
    prior_dR_age_sd_max  = prior_dR_age_sd_max,
    prior_R_age_exp_min  = prior_R_age_exp_min,
    prior_R_age_exp_max  = prior_R_age_exp_max,
    prior_dR_max         = prior_dR_max ,
    prior_daily_seed_max = prior_daily_seed_max,
    prior_strain_multipliers_min = array( prior_strain_multipliers_min, dim = data_params$n_strains - 1 ),
    prior_strain_multipliers_max = array( prior_strain_multipliers_max, dim = data_params$n_strains - 1 )
  ) )
  
  # Stan parameters
  stan_params <- list(
    n_iter        = stan_n_iter,  
    n_chains.     = stan_n_chains,
    max_treedepth = stan_max_treedepth, 
    adapt_delta   = stan_adapt_delta
  )
  
  # start the R(t) and A(t) curves flat
  stan_params$init <- lapply( 1:stan_params$n_chains, function(x) list( 
    dR     = rep( 0, stan_data$n_dates_age ),
    dR_age = matrix(0, nrow = stan_data$n_ages, ncol = stan_data$n_dates_age )
  ) )
  
  # get the stan model
  model <- model.survey()
  
  raw <- sampling( 
    model, 
    data    = stan_data, 
    chains  = stan_params$n_chains, 
    iter    = stan_params$n_iter, 
    pars    = c( "strain_multipliers", "R_age_exp", "dR_sd", "dR_sd_age", "log_R0", "log_R0_age", "R_age", "positivity_age_expected", "positivity_strain_expected"), 
    cores   = min( stan_params$n_chains, 3 ),
    control = list( max_treedepth = stan_params$max_treedepth, adapt_delta = stan_params$adapt_delta ),
    init    = stan_params$init
  )
  
  return( list(
    stan_raw    = raw,
    stan_data   = stan_data,
    stan_model  = model,
    stan_params = stan_params
  ) )
}

###################################################################################/
# model.survey
###################################################################################/
model.survey = function() {
  return( stanmodels$age_strain_seir_survey )
}
  