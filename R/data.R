###################################################################################/
# data.englandCasesByAge
###################################################################################/
data.englandCasesByAge <- function( ageBand = data.ageBand.ukDashboard )
{
  file <- system.file( "data", "uk_dashboard", "englandCasesByAge.csv", package = "EpiAgeVar")
  dt   <- fread( file )
  
  if( ageBand != data.ageBand.ukDashboard ) {
    if( !( ageBand %in% data.ageBand.all ) )
        stop( sprintf( "ageBand type not supported, supported: %s", paste( data.ageBand.all, collapse = ", ") ) )
    
    adjuster <- data.adjuster.ageBand( data.ageBand.ukDashboard, ageBand, "cases", "date" ) 
    dt       <- adjuster$process( dt )
  }
  return( copy( dt ) )
}