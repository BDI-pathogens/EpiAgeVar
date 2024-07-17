###################################################################################/
# data.englandCasesByAge
###################################################################################/
data.englandCasesByAge <- function( 
  ageBands = data.ageBand.ukDashboard 
)
{
  file <- system.file( "data", "uk_dashboard", "englandCasesByAge.csv", package = "EpiAgeVar")
  dt   <- fread( file )
  
  if( ageBands != data.ageBand.ukDashboard ) {
    if( !( ageBand %in% data.ageBand.all ) )
        stop( sprintf( "ageBand type not supported, supported: %s", paste( data.ageBand.all, collapse = ", ") ) )
    
    adjuster <- data.adjuster.ageBand( data.ageBand.ukDashboard, ageBands, "cases", "date" ) 
    dt       <- adjuster$process( dt )
  }
  return( copy( dt ) )
}

###################################################################################/
# data.ons.englandPopulation
#
# ons population
###################################################################################/
data.ons.englandPopulation <- function( 
  ageBands = data.ageBand.decades
)
{
  filePop <- system.file( "data", "ons_regions", "ONS-population_2021-08-05.csv", package = "EpiAgeVar")  
  pop     <- fread( filePop )
  
  # ons england code
  englandCode <- "E92000001"
  pop <- pop[ areaCode == englandCode & gender == "ALL" & category == "AGE_ONLY"] 
  pop <- pop[ , .( age, population, category )]
  
  # now sort out age buckets are required
  pop <- pop[ age %in% data.ageBand.ukDashboard.ages ]
  
  if( ageBands != data.ageBand.ukDashboard ) {
    if( !( ageBand %in% data.ageBand.all ) )
      stop( sprintf( "ageBand type not supported, supported: %s", paste( data.ageBand.all, collapse = ", ") ) )
    
    adjuster <- data.adjuster.ageBand( data.ageBand.ukDashboard, ageBands, "population", c() ) 
    pop       <- adjuster$process(pop )
  }
  return( pop )
}

