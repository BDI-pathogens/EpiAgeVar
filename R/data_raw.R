###################################################################################/
# data.ukdashboard.casesByAge.england
###################################################################################/
data.ukdashboard.casesByAge.england <- function( 
  ageBands = data.ageBand.ukDashboard 
)
{
  file <- system.file( "data_raw", "uk_dashboard", "englandCasesByAge.csv", package = "EpiAgeVar")
  dt   <- fread( file )
  
  if( ageBands != data.ageBand.ukDashboard ) {
    if( !( ageBands %in% data.ageBand.all ) )
        stop( sprintf( "ageBands type not supported, supported: %s", paste( data.ageBand.all, collapse = ", ") ) )
    
    adjuster <- data.adjuster.ageBand( data.ageBand.ukDashboard, ageBands, "cases", "date" ) 
    dt       <- adjuster$process( dt )
  }
  return( copy( dt ) )
}

###################################################################################/
# data.ons.population.england
#
# ons population
###################################################################################/
data.ons.population.england <- function( 
  ageBands = data.ageBand.decades
)
{
  filePop <- system.file( "data_raw", "ons_regions", "ONS-population_2021-08-05.csv", package = "EpiAgeVar")  
  pop     <- fread( filePop )
  
  # ons england code
  englandCode <- "E92000001"
  pop <- pop[ areaCode == englandCode & gender == "ALL" & category == "AGE_ONLY"] 
  pop <- pop[ , .( age, population, category )]
  
  # now sort out age buckets are required
  pop <- pop[ age %in% data.ageBand.ukDashboard.ages ]
  
  if( ageBands != data.ageBand.ukDashboard ) {
    if( !( ageBands %in% data.ageBand.all ) )
      stop( sprintf( "ageBands type not supported, supported: %s", paste( data.ageBand.all, collapse = ", ") ) )
    
    adjuster <- data.adjuster.ageBand( data.ageBand.ukDashboard, ageBands, "population", c() ) 
    pop       <- adjuster$process(pop )
  }
  return( copy( pop ) )
}

data.comix.1_lockdown_1           = "1. Lockdown 1"           
data.comix.2_lockdown_1_easing    = "2. Lockdown 1 easing"    
data.comix.3_relaxed_restrictions = "3. Relaxed restrictions" 
data.comix.4_school_reopening     = "4. School reopening"    
data.comix.5_lockdown_2           = "5. Lockdown 2"           
data.comix.6_lockdown_2_easing    = "6. Lockdown 2 easing"    
data.comix.7_christmas            = "7. Christmas"            
data.comix.8_lockdown_3           = "8. Lockdown 3"          
data.comix.9_lockdown_3_schools   = "9. Lockdown 3 + schools"
data.comix.periods = c( 
  data.comix.1_lockdown_1, data.comix.2_lockdown_1_easing, data.comix.3_relaxed_restrictions,
  data.comix.4_school_reopening, data.comix.5_lockdown_2, data.comix.6_lockdown_2_easing,
  data.comix.7_christmas, data.comix.8_lockdown_3, data.comix.9_lockdown_3_schools
)

lockBinding( "data.comix.1_lockdown_1", environment() )                     
lockBinding( "data.comix.2_lockdown_1_easing", environment() )     
lockBinding( "data.comix.3_relaxed_restrictions", environment() )     
lockBinding( "data.comix.4_school_reopening", environment() )     
lockBinding( "data.comix.5_lockdown_2", environment() )     
lockBinding( "data.comix.6_lockdown_2_easing", environment() )        
lockBinding( "data.comix.7_christmas", environment() )                
lockBinding( "data.comix.8_lockdown_3", environment() )               
lockBinding( "data.comix.9_lockdown_3_schools", environment() )        
lockBinding( "data.comix.periods", environment() )     

###################################################################################/
# data.comix.contactmatrix
#
# Returns a Comix contact matrix:
#   column = participant age group
#   row    = contact age group
#
# Note the matrix is asymmetric due to different population size in each group
###################################################################################/
data.comix.contactmatrix <- function( 
  period   = data.comix.4_school_reopening, 
  ageBands = data.ageBand.comix
)
{
  file <- system.file( "data_raw", "comix", "contact_matrices_9_periods.csv", package = "EpiAgeVar" )  
  
  dt_contacts <- fread( file )
  setnames( dt_contacts, 1:5,c("idx", "source", "contact", "contacts", "Period"))
  dt_contacts <- dt_contacts[ Period == period ]
  
  if( ageBands != data.ageBand.comix ) {
    # get population for splitting contacts 
    pop <- data.ons.population.england( ageBands = data.ageBand.single )
    
    # first split in to a single year contract matrix
    adjusterParticipant <- data.adjuster.ageBand( 
      data.ageBand.comix, 
      data.ageBand.single, 
      sumColumns   = "contacts", 
      byColumns    = "contact", 
      bucketColumn = "source" 
    )
    oldMap <- copy( adjusterParticipant$map )
    
    adjusterContact <- data.adjuster.ageBand( 
      ifelse( ageBands != data.ageBand.single, ageBands, data.ageBand.comix ),
      data.ageBand.single, 
      sumColumns   = "contacts", 
      byColumns    = "source",
      bucketColumn = "contact"
    )  
    newMap <- copy( adjusterContact$map )
    
    oldAgeUnityMap     <- oldMap[ , .( old, new, frac = 1)]
    oldAgePopWeightMap <- pop[ , .(new = age, population ) ][ oldMap, on = "new" ]
    totPop             <- oldAgePopWeightMap[ , .(tot = sum(population)), by = "old"]
    oldAgePopWeightMap <- totPop[ oldAgePopWeightMap, on = "old"][ ,.( old, new, frac = population / tot)]
    
    # each participant in band has same contacts regardless of age 
    adjusterParticipant$map <- oldAgeUnityMap
    dt_contacts             <-adjusterParticipant$process(dt_contacts)
    
    # contacts with in a contact band are population weighted by age in band 
    # (i.e. equally likely to have contact with anyone in band)
    adjusterContact$map <- oldAgePopWeightMap
    dt_contacts         <- adjusterContact$process(dt_contacts )
    
    # then aggregate single years into new age bands
    newAgeUnityMap     <- newMap[ , .( old = new, new = old, frac = 1)]
    newAgePopWeightMap <- pop[ , .(new = age, population ) ][ newMap, on = "new" ]
    totPop             <- newAgePopWeightMap[ , .(tot = sum(population)), by = "old"]
    newAgePopWeightMap <- totPop[ newAgePopWeightMap, on = "old"][ ,.( old = new, new = old, frac = population / tot)]
    
    # special handling for ONS which does not have 0-1y ages
    if( ageBands == data.ageBand.onsInfection ){
      dt_contacts <- dt_contacts[ !( source %in% c( "0", "1") )]
      dt_contacts <- dt_contacts[ ,.( source, contact = ifelse( contact %in% c( "0", "1"), "2",contact), contacts)]
      dt_contacts <- dt_contacts[ , .(contacts = sum(contacts)), by = c("source", "contact")]
    }
    
    if( ageBands != data.ageBand.single ) {
      # sum contacts across all contact in new age band
      adjusterContact$map <- newAgeUnityMap
      dt_contacts         <- adjusterContact$process(dt_contacts )
      
      # take population weighted across all partiapnts in new age band 
      adjusterParticipant$map <- newAgePopWeightMap 
      dt_contacts             <- adjusterParticipant$process(dt_contacts )
    }
  }
  
  ages           <- data.ageBand.ages[[ ageBands]]
  contact_matrix <- dcast.data.table( dt_contacts, source ~ contact, value.var = "contacts", fun.aggregate = mean )
  setcolorder( contact_matrix, c( "source", ages ) )
  contact_matrix$source <- factor( contact_matrix$source, ages )
  contact_matrix <- contact_matrix[ order( source  )]
  contact_matrix <- t(as.matrix( contact_matrix[,-1]) )
  contact_matrix <- `colnames<-`(contact_matrix, ages)
  
  return( contact_matrix )
}

###################################################################################/
# data.cog.lineage
#
# Lineage data from cog
###################################################################################/
data.cog.lineage = function(
  remap = FALSE,
  map = list( 
    "alpha" = "B.1.1.7",
    "beta" = c("B.1.351", "B.1.351.1"),
    "gamma" = c( "P.1"),
    "delta" = c( "B.1.617","B.1.617.1","B.1.617.2","B.1.617.3" )
  ),
  removeNA    = TRUE,
  NA_Lineages = c( "Lineage data suppressed", "None" ),
  byLTLA = FALSE
)
{
  file = "lineages_by_ltla_and_week.tsv"
  full_file = system.file( "data_raw", "cog_uk", file, package = "EpiAgeVar")
  
  dt = fread( full_file )
  setnames( dt, c( "WeekEndDate", "Count" ), c( "date", "count" ) )
  
  if( removeNA )
    dt = dt[ !( Lineage %in% NA_Lineages  ) ]
  
  if( remap ){
    dt_map = data.table(
      lineage = rep( names( map), unlist( lapply( map, length ))),
      Lineage  = unlist( map )
    )
    dt = dt_map[ dt, on = "Lineage" ]
    dt[ , c( "lineage", "Lineage" ) := list( ifelse( is.na( lineage), "Other", lineage ), NULL ) ]
    
  } else {
    setnames( dt, "Lineage", "lineage")
  }
  
  if( !byLTLA ){
    dt     = dt[ , .( count = sum( count )), by = c("lineage", "date")] 
    totals = dt[ , .( total = sum( count )), by = c("date")]
    dt = totals[ dt, on = "date" ]
    dt[ , c( "frac", "total" ) := list( count / total, NULL )]
    dt = dt[ order( date, lineage)]
  }
  return( copy( dt ) )
}  

data.onsInfection.positivity    = "positivity"
data.onsInfection.sampleSize    = "sampleSize"
data.onsInfection.positiveTests = "positiveTests"
data.onsInfection.all = c(data.onsInfection.positivity, data.onsInfection.sampleSize, data.onsInfection.positiveTests )
lockBinding( "data.onsInfection.positivity", environment() )
lockBinding( "data.onsInfection.sampleSize", environment() )
lockBinding( "data.onsInfection.positiveTests", environment() )
lockBinding( "data.onsInfection.all", environment() )

###################################################################################/
# .ons.infection.file_template 
###################################################################################/
.ons.infection.file_template = list(
  "20211126" = list(
    file   = "covid19infectionsurveydatasets20211126england.xlsx",
    byAge = list(
      sheet_used = 14,
      skip = 6,
      colsPerAge = 5,
      colOffset  = setNames( as.list(
        c( 2,                            5,                               6                            )),
        c( data.onsInfection.positivity, data.onsInfection.positiveTests, data.onsInfection.sampleSize )
      )
    )
  ),
  "20220204" = list(
    file   = "20220204covid19infectionsurveydatasetsengland.xlsx",
    byAge = list(
      sheet_used = 14,
      skip = 6,
      colsPerAge = 5,
      colOffset  = setNames( as.list(
        c( 2,                            5,                               6                            )),
        c( data.onsInfection.positivity, data.onsInfection.positiveTests, data.onsInfection.sampleSize )
      )
      
    )
  ),
  "20220318" = list(
    file = "20220318covid19infectionsurveydatasetsengland1.xlsx",
    byAge = list(
      sheet_used = 15,
      skip = 5,
      colsPerAge = 5,
      colOffset  = setNames( as.list(
        c( 2,                            5,                               6                            )),
        c( data.onsInfection.positivity, data.onsInfection.positiveTests, data.onsInfection.sampleSize )
      )
    )
  )
)


###################################################################################/
# data.ons.infections
#
# Data from the ONS infection survey
###################################################################################/
data.ons.infections = function(
  field = data.onsInfection.positivity,
  release = "20220318",
  byAge = TRUE,
  ageBands = data.ageBand.onsInfection
)
{
  if( is.null( .ons.infection.file_template[[ release ]] ) )
    stop( sprintf( "don't have data for this release, available releases are: %s", paste( names( .ons.infection.file_template), collapse = ", " ) ) )
  
  if( !( field %in% data.onsInfection.all ) )
    stop( sprtinf( "only available fields from the ONS infection survey are:", paste( data.onsInfection.all, collapse = ", ") ) )
  
  template =.ons.infection.file_template[[ release ]]
  file = template$file
  full_file = system.file( "data_raw", "ons_inf_survey", file, package = "EpiAgeVar")
  
  if( byAge == TRUE ) {
    sheet_used  = template$byAge$sheet_used
    skip        = template$byAge$skip
    colsPerAge  = template$byAge$colsPerAge
    colOffset   = template$byAge$colOffset[[ field ]]
    
    raw_xl = readxl::read_xlsx( full_file, sheet = sheet_used, skip = skip )
    raw_dt = as.data.table( raw_xl )
    
    age_groups = data.ageBand.onsInfection.ages
    
    cols    = c( "date", age_groups)
    cols_idx = c( 1, colOffset + (0:(length( age_groups)-1) * colsPerAge ) )
    setnames( raw_dt, cols_idx, cols )
    dt = raw_dt[ ,.SD, .SDcols = cols]
    
    dt = dt[ , c( list( date = date ), 
                  lapply( .SD, function (x ) as.numeric( ifelse( x == "*", "-1", x) ) ) ), 
             .SDcols = age_groups]
    
    dates = dt[ , stri_split_fixed( date, " ", simplify = TRUE )]
    dates = as.Date( sprintf("%s-%s-%s", dates[,7], dates[,6], dates[,5] ), "%Y-%B-%d")
    dt[, date := dates ]
    dt = dt[ !is.na( date )]
    
    if( ageBands != data.ageBand.onsInfection ) {
      if( field == data.onsInfection.positivity ) {
        # convert to total number of cases
        pop = data.ons.population.england( ageBands = data.ageBand.onsInfection ) 
        dt  = melt.data.table( dt, "date", variable.name = "age", value.name = "value" )
        dt = pop[ dt, on = "age"][ ,.( date, age, value = value * population ) ]
        
        # adjust the total number of cases to the new age_bands
        adjuster <- data.adjuster.ageBand( data.ageBand.onsInfection, data.ageBand.single, "value", "date" ) 
        dt = adjuster$process( dt )        
        adjuster <- data.adjuster.ageBand(  data.ageBand.single, ageBands, "value", "date" ) 
        dt = adjuster$process( dt )   
        
        # convert back to a positivity
        pop = data.ons.population.england( ageBands = ageBands ) 
        dt = pop[ dt, on = "age"][ ,.( date, age, value = value / population ) ]
        
        # flip to required output form
        dt = dcast.data.table( dt, date ~ age, value.var = "value", fun.aggregate = sum )
        setcolorder( dt, c( "date", data.ageBand.ages[[ ageBands ]] ))
      } else {
        dt  = melt.data.table( dt, "date", variable.name = "age", value.name = "value" )
        
        # adjust the total number of cases to the new age_bands
        adjuster <- data.adjuster.ageBand( data.ageBand.onsInfection, data.ageBand.single, "value", "date" ) 
        dt = adjuster$process( dt )        
        adjuster <- data.adjuster.ageBand(  data.ageBand.single, ageBands, "value", "date" ) 
        dt = adjuster$process( dt )   
        dt[ , value := round( value ) ]
        
        dt = dcast.data.table( dt, date ~ age, value.var = "value", fun.aggregate = sum )
        setcolorder( dt, c( "date", data.ageBand.ages[[ ageBands ]] ))
      }
    }
    
  } else {
    if( field != data.onsInfection.positivity )
      throw( "positivity only included for aggregated data by age group" )
    
    sheet_used = 3
    skip = 5
    
    raw_xl = readxl::read_xlsx( full_file, sheet = sheet_used, skip = skip )
    raw_dt = as.data.table( raw_xl )
    setnames( raw_dt, c( 1,2), c( "date", "positivity"))
    dt = raw_dt[ , .(date, positivity )] 
    dt = dt[ !is.na( positivity)]  
    dates = dt[ , stri_split_fixed( date, " ", simplify = TRUE )]
    dates = as.Date( sprintf("%s-%s-%s", dates[,7], dates[,6], dates[,5] ), "%Y-%B-%d")
    dt[, date := dates ]
    dt = dt[ !is.na( date )]
  }
  
  return( copy( dt ) )
}
  

