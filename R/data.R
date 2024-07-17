###################################################################################/
# data.ukdashboard.casesByAge.england
###################################################################################/
data.ukdashboard.casesByAge.england <- function( 
  ageBands = data.ageBand.ukDashboard 
)
{
  file <- system.file( "data", "uk_dashboard", "englandCasesByAge.csv", package = "EpiAgeVar")
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
  filePop <- system.file( "data", "ons_regions", "ONS-population_2021-08-05.csv", package = "EpiAgeVar")  
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
  return( pop )
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
  file <- system.file( "data", "comix", "contact_matrices_9_periods.csv", package = "EpiAgeVar" )  
  
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

