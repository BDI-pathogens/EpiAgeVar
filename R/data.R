.start_date <- as.Date( "2021-01-30")
.end_date   <- as.Date( "2021-12-04") 

###################################################################################/
# data.raw_data_avalaible
#
# Public package cannot include the underlying raw data sources. It contains the
# processed data fed in to the model and the code to do the conversions, but
# this can only be run if the underlying raw data is downloaded from source
#
###################################################################################/
data.raw_data_avalaible <- function() {
  if( system.file( "data_raw", "ons_regions", "ONS-population_2021-08-05.csv", package = "EpiAgeVar") == "" )
    stop( "Raw data not availble in public package" )
  return()
}

###################################################################################/
# data.population
###################################################################################/
data.population <- function( from_raw = FALSE ) {
  if( from_raw ) {
    data.raw_data_avalaible()
    population <- data.ons.population.england( data.ageBand.comix )
  } else {
    file <- system.file( "data", "population.csv", package = "EpiAgeVar" )
    population <- fread( file )
  }
  
  population$age <- factor( population$age, levels = data.ageBand.comix.ages )
  population     <- population[ order( age ) ]
  return( copy( population ) )
}

###################################################################################/
# data.contact_matrix
###################################################################################/
data.contact_matrix <- function( from_raw = FALSE ) {
  if( from_raw ) {
    data.raw_data_avalaible()
    cm <- data.comix.contactmatrix()
  } else {
    file <- system.file( "data", "contact_matrix.csv", package = "EpiAgeVar" )
    cm   <- fread( file )
    cm   <- `rownames<-`(  as.matrix( cm ), names( cm ) )
  }
  return( cm )
}

###################################################################################/
# data.infection_survey
###################################################################################/
data.infection_survey <- function( from_raw = FALSE ) {
  if( from_raw ) {
    data.raw_data_avalaible()
    positive_tests <- data.ons.infections( field = data.onsInfection.positiveTests, ageBands = data.ageBand.comix)
    positive_tests <- positive_tests[ date >= .start_date & date <= .end_date ]
   
    sample_size <- data.ons.infections( field = data.onsInfection.sampleSize, ageBands = data.ageBand.comix)
    sample_size <- sample_size[ date >= .start_date & date <= .end_date ]  
  } else {
    file <- system.file( "data", "infection_survey_positive.csv", package = "EpiAgeVar" )
    positive_tests <- fread( file )
    
    file <- system.file( "data", "infection_survey_size.csv", package = "EpiAgeVar" )
    sample_size <- fread( file )
  }
  setcolorder( sample_size, c( "date", data.ageBand.comix.ages ) )
  setcolorder( positive_tests, c( "date", data.ageBand.comix.ages ) )
  return( list( sample_size = copy( sample_size ), positive_tests = copy( positive_tests ) ) ) 
}

###################################################################################/
# data.variants
###################################################################################/
data.variants <- function( from_raw = FALSE ) {
  if( from_raw ) {
    cog <- data.cog.lineage()
    cog[ , lineage := ifelse( 
      lineage == "B.1.1.7",                                              "Alpha", 
      ifelse( lineage == "B.1.617.2" | substr( lineage, 1, 3 ) == "AY.", "Delta",
                                                                         "Other" ) ) ]
    cog[ ,  date := as.Date(date) + ( as.integer( as.Date( date ) - .start_date ) %% 14 ) ]
    cog <- dcast.data.table( cog, date ~ lineage, value.var = "count", fun.aggregate = function(x) sum(x) )
    cog <- cog[ date >= .start_date & date <= .end_date, .( date, Alpha, Delta ) ] 
  } else {
    file <- system.file( "data", "variants.csv", package = "EpiAgeVar" )
    cog <- fread( file )
  }
  return( copy( cog ) )
}
