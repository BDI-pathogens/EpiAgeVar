data.ageBand.ukDashboard  = "ageband_ukdashboard"
data.ageBand.decades      = "ageband_decades"
data.ageBand.onsInfection = "ageband_onsinfection"
data.ageBand.comix        = "ageband_comix"
data.ageBand.single       = "ageband_single"
data.ageBand.all          = c( data.ageBand.ukDashboard, data.ageBand.decades, data.ageBand.onsInfection, data.ageBand.comix, data.ageBand.single ) 

data.ageBand.ukDashboard.ages = c( "00_04", "05_09", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84", "85_89", "90+")
data.ageBand.decades.ages = c( "0_9", "10_19","20_29","30_39","40_49","50_59","60_69","70_79","80" )
data.ageBand.comix.ages   = c( "0-4", "5-11","12-17", "18-29", "30-39", "40-49","50-59","60-69","70+" )
data.ageBand.onsInfection.ages = c( "2-11", "12-16", "17-24", "25-34", "35-49", "50-69",  "70+")
data.ageBand.single.ages  = as.character( 0:94 )
data.ageBand.ages = list()
data.ageBand.ages[[ data.ageBand.comix ]] = data.ageBand.comix.ages
data.ageBand.ages[[ data.ageBand.decades ]] = data.ageBand.decades.ages
data.ageBand.ages[[ data.ageBand.ukDashboard ]] = data.ageBand.ukDashboard.ages
data.ageBand.ages[[ data.ageBand.single ]] = data.ageBand.single.ages
data.ageBand.ages[[ data.ageBand.onsInfection ]] = data.ageBand.onsInfection.ages

lockBinding( "data.ageBand.ukDashboard", environment() )
lockBinding( "data.ageBand.decades", environment() )
lockBinding( "data.ageBand.onsInfection", environment() )
lockBinding( "data.ageBand.comix", environment() )
lockBinding( "data.ageBand.single", environment() )
lockBinding( "data.ageBand.all", environment() )
lockBinding( "data.ageBand.ukDashboard.ages", environment() )
lockBinding( "data.ageBand.decades.ages", environment() )
lockBinding( "data.ageBand.onsInfection.ages", environment() )
lockBinding( "data.ageBand.comix.ages", environment() )
lockBinding( "data.ageBand.single.ages", environment() )
lockBinding( "data.ageBand.ages", environment() )

.ageband.maps <- list()
.ageband.maps[[ data.ageBand.comix ]]        <- list()
.ageband.maps[[ data.ageBand.decades]]       <- list()
.ageband.maps[[ data.ageBand.onsInfection ]] <- list()
.ageband.maps[[ data.ageBand.ukDashboard ]]  <- list()

.ageband.maps[[ data.ageBand.ukDashboard ]][[ data.ageBand.comix ]] <- list( 
  c( "00_04", "0-4",   1 ),
  c( "05_09", "5-11",   1 ),
  c( "00_59", "0-4",   0 ),
  c( "10_14", "5-11",  0.4 ),
  c( "10_14", "12-17",  0.6 ),
  c( "15_19", "12-17", 0.6 ),
  c( "15_19", "18-29", 0.4 ),
  c( "20_24", "18-29", 1 ),
  c( "25_29", "18-29", 1 ),
  c( "30_34", "30-39", 1 ),
  c( "35_39", "30-39", 1 ),
  c( "40_44", "40-49", 1 ),
  c( "45_49", "40-49", 1 ),
  c( "50_54", "50-59", 1 ),
  c( "55_59", "50-59", 1 ),
  c( "60_64", "60-69", 1 ),
  c( "65_69", "60-69", 1 ),
  c( "70_74", "70+", 1 ),
  c( "75_79", "70+", 1 ),
  c( "80_84", "70+",    1 ),
  c( "85_89", "70+",    1 ),
  c( "90+",   "70+",    1 ),
  c( "60+",   "70+",    0 ),
  c( "unassigned", "70+",    0 )
) 

.ageband.maps[[ data.ageBand.ukDashboard ]][[ data.ageBand.decades ]] <- list( 
  c( "00_04", "0_9",   1 ),
  c( "05_09", "0_9",   1 ),
  c( "00_59", "0_9",   0 ),
  c( "10_14", "10_19", 1 ),
  c( "15_19", "10_19", 1 ),
  c( "20_24", "20_29", 1 ),
  c( "25_29", "20_29", 1 ),
  c( "30_34", "30_39", 1 ),
  c( "35_39", "30_39", 1 ),
  c( "40_44", "40_49", 1 ),
  c( "45_49", "40_49", 1 ),
  c( "50_54", "50_59", 1 ),
  c( "55_59", "50_59", 1 ),
  c( "60_64", "60_69", 1 ),
  c( "65_69", "60_69", 1 ),
  c( "70_74", "70_79", 1 ),
  c( "75_79", "70_79", 1 ),
  c( "80_84", "80",    1 ),
  c( "85_89", "80",    1 ),
  c( "90+",   "80",    1 ),
  c( "60+",   "80",    0 ),
  c( "unassigned", "80",    0 )
) 

.ageband.maps[[ data.ageBand.ukDashboard ]][[ data.ageBand.onsInfection ]] <- list( 
  c( "00_04", "2-11",   3/5 ),
  c( "05_09", "2-11",   1 ),
  c( "00_59", "2-11",   0 ),
  c( "10_14", "2-11",   2/5 ),
  c( "10_14", "12-16",  3/5 ),
  c( "15_19", "12-16", 2/5 ),
  c( "15_19", "17-24", 3/5 ),
  c( "20_24", "17-24", 1 ),
  c( "25_29", "25-34", 1 ),
  c( "30_34", "25-34", 1 ),
  c( "35_39", "35-49", 1 ),
  c( "40_44", "35-49", 1 ),
  c( "45_49", "35-49", 1 ),
  c( "50_54", "50-69", 1 ),
  c( "55_59", "50-69", 1 ),
  c( "60_64", "50-69", 1 ),
  c( "65_69", "50-69", 1 ),
  c( "70_74", "70+", 1 ),
  c( "75_79", "70+", 1 ),
  c( "80_84", "70+",    1 ),
  c( "85_89", "70+",    1 ),
  c( "90+",   "70+",    1 ),
  c( "60+",   "70+",    0 ),
  c( "unassigned", "70+",    0 )
) 

.ageband.maps[[ data.ageBand.ukDashboard ]][[ data.ageBand.single ]] <- unlist( 
  lapply( data.ageBand.ukDashboard.ages, function( age ) { list( 
    c( age, as.character( as.integer( substr( age, 1,2 ) ) + 0 ), 0.2 ),
    c( age, as.character( as.integer( substr( age, 1,2 ) ) + 1 ), 0.2 ),
    c( age, as.character( as.integer( substr( age, 1,2 ) ) + 2 ), 0.2 ),
    c( age, as.character( as.integer( substr( age, 1,2 ) ) + 3 ), 0.2 ),
    c( age, as.character( as.integer( substr( age, 1,2 ) ) + 4 ), 0.2 )
  )
  }), recursive = FALSE )

.ageband.maps[[ data.ageBand.comix ]][[ data.ageBand.single ]] <- unlist( 
  lapply( data.ageBand.comix.ages, function( age ) { 
    if( age != "70+") {
      minAge = as.integer( strsplit( age, "-")[[1]][1] )
      maxAge = as.integer( strsplit( age, "-")[[1]][2] )
    } else {
      minAge = 70
      maxAge = 94
    }
    lapply( minAge:maxAge, function( age2) c( age, as.character(age2), 1 / ( maxAge - minAge + 1 )))
  }), recursive = FALSE )

.ageband.maps[[ data.ageBand.onsInfection ]][[ data.ageBand.single ]] <- unlist( 
  lapply( data.ageBand.onsInfection.ages, function( age ) { 
    if( age != "70+") {
      minAge = as.integer( strsplit( age, "-")[[1]][1] )
      maxAge = as.integer( strsplit( age, "-")[[1]][2] )
    } else {
      minAge = 70
      maxAge = 94
    }
    lapply( minAge:maxAge, function( age2) c( age, as.character(age2), 1 / ( maxAge - minAge + 1 )))
  }), recursive = FALSE )

.ageband.maps[[ data.ageBand.decades ]][[ data.ageBand.single ]] <- unlist( 
  lapply( data.ageBand.decades.ages, function( age ) { 
    if( age != "80") {
      minAge = as.integer( strsplit( age, "_")[[1]][1] )
      maxAge = as.integer( strsplit( age, "_")[[1]][2] )
    } else {
      minAge = 80
      maxAge = 94
    }
    lapply( minAge:maxAge, function( age2) c( age, as.character(age2), 1 / ( maxAge - minAge + 1 )))
  }), recursive = FALSE )

###################################################################################/
# data.adjuster.ageBand
#
# Maps 
###################################################################################/
data.adjuster.ageBand <- function(
  from,
  to,
  sumColumns   = c(),
  byColumns    = c(),
  bucketColumn = "age"
)
{
  if( !( from %in% data.ageBand.all ) || !( to %in% data.ageBand.all )  )
    throw( "age band is not recognised (see data.ageBand.all for known age bands)")
  
  if( is.null( .ageband.maps[[ from ]] ) )
    throw( "age band map not yet implemented for this combination - add to .ageband.maps in adjuster.R")
  if( is.null( .ageband.maps[[ from ]][[ to ]] ) )
    throw( "age band map not yet implemented for this combination - add to .ageband.maps in adjuster.R")
  
  map <- .ageband.maps[[ from ]][[ to ]] 
  map <- as.data.table( transpose( map ) )
  setnames( map, 1:3, c( "old", "new", "frac" ) )
  map[ , frac := as.numeric( frac )]
  
  adjuster <- data.class.adjuster.bucket$new( bucketColumn = bucketColumn, 
                                              map = map, 
                                              sumColumns = sumColumns, 
                                              by = byColumns )
  
  return( adjuster )
}
###################################################################################/
# data.class.adjuster
###################################################################################/
data.class.adjuster = R6Class(
  classname = "data.class.adjuster",
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( )
    {
      stop( "initialize must be implemented")
    },
    ###################################################################################/
    # process
    #
    # transforms the data set
    ###################################################################################/
    process = function( data )
    {
      if( !is.data.table( data ) )
        stop( "data must be a data.table" )
    }
  )
)

###################################################################################/
# data.class.adjuster.composite
###################################################################################/
data.class.adjuster.composite = R6Class(
  classname = "data.class.adjuster.composite",
  inherit   =  data.class.adjuster,
  private = list( 
    .adjusters = NULL 
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      adjusters  
    )
    {
      private$.adjusters <- adjusters
    },
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      adjusters <- private$.adjusters  
      if( !is.null( adjusters ) )
      {
        for( adj in adjusters )
          data <- adj$process( data )
      }
      return( data )
    }
  )
)

###################################################################################/
# data.class.adjuster.project
###################################################################################/
data.class.adjuster.project = R6Class(
  classname = "data.class.adjuster.project",
  inherit   =  data.class.adjuster,
  private = list(
    .columns   = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      columns  
    )
    {
      if( !is.character( columns ) )
        stop( "columns must be a vector of column names")
      private$.columns <- columns
    },
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      columns <- private$.columns
      if( !is.null( columns ) )
        data <- data[ , .SD, .SDcols = columns ]
      return( data )
    }
  )
)

###################################################################################/
# data.class.adjuster.rename
###################################################################################/
data.class.adjuster.rename = R6Class(
  classname = "data.class.adjuster.rename",
  inherit   =  data.class.adjuster,
  private = list(
    .columns   = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      columns  
    )
    {
      if( !is.list( columns ) )
        stop( "columns must be a named list of old to new column names")
      private$.columns <- columns
    },
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      columns <- private$.columns
      setnames( data, names( columns ), unlist( columns ) )
      return( data )
    }
  )
)

###################################################################################/
# data.class.adjuster.ratio
###################################################################################/
data.class.adjuster.ratio <- R6Class(
  classname = "data.class.adjuster.ratio",
  inherit   =  data.class.adjuster,
  private = list(
    .name        = NULL,
    .numerator   = NULL,
    .denominator = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      name,
      numerator,
      denominator
    )
    {
      private$.name        <- name
      private$.numerator   <- numerator
      private$.denominator <- denominator
    },
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      name        <- private$.name       
      numerator   <- private$.numerator   
      denominator <- private$.denominator 
      
      oldCols = c( numerator, denominator )
      tempCols = c( "RATIO_PROCESS_NUMERATOR", "RATIO_PROCESS_DENOMINATOR" )
      setnames( data, oldCols, tempCols )
      
      data[ , c( name ) := RATIO_PROCESS_NUMERATOR / RATIO_PROCESS_DENOMINATOR ]
      setnames( data, tempCols, oldCols )
      
    }
  )
)

###################################################################################/
# data.class.adjuster.groupby
###################################################################################/
data.class.adjuster.groupby = R6Class(
  classname = "data.class.adjuster.groupby",
  inherit   =  data.class.adjuster,
  private = list(
    .by         = NULL,
    .sumColumns = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      by,
      sumColumns
    )
    {
      private$.by <- by
      private$.sumColumns <- sumColumns
    },
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      sumColumns <- private$.sumColumns
      by         <- private$.by
      data <- data[ , lapply( .SD, sum ), .SDcols = sumColumns, by = by ]
      return( data )
    }
  )
)

###################################################################################/
# data.class.adjuster.replace
#
# replaces the values in a given column by values in a map which is a named list
# where the names are the old values and the values are the replacement values
# any values not in the map are left unchanged
###################################################################################/
data.class.adjuster.replace = R6Class(
  classname = "data.class.adjuster.replace",
  inherit   =  data.class.adjuster,
  private = list(
    .column = NULL,
    .map    = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      column,
      map
    )
    {
      private$.map    <- map
      private$.column <- column
      
      self$checkMap()
    },
    ###################################################################################/
    # checkMap
    ###################################################################################/
    checkMap = function()
    {
    },
    
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      
      dt_map <- data.table( OLD_VALUES = names( private$.map ),
                            NEW_VALUES = unlist( private$.map ) )
      
      col  = private$.column
      cols = names( data )
      if( !( col %in% cols ) )
        stop( "the columnin the adjuster is not in the data set")
      
      setnames( data, col, "OLD_VALUES" )
      data = dt_map[ data, on = "OLD_VALUES" ]
      data[ , NEW_VALUES := ifelse( is.na( NEW_VALUES), OLD_VALUES, NEW_VALUES ) ]
      setnames( data, "NEW_VALUES", col )
      data[ , OLD_VALUES := NULL ]
      
      return( data )
    }
  )
)

###################################################################################/
# data.class.adjuster.bucket
###################################################################################/
data.class.adjuster.bucket = R6Class(
  classname = "data.class.adjuster.bucket",
  inherit   =  data.class.adjuster,
  private = list(
    .bucketColumn = NULL,
    .map          = NULL,
    .by           = NULL,
    .sumColumns   = NULL
  ),
  public  = list(
    ###################################################################################/
    # initialize
    ###################################################################################/
    initialize = function( 
      bucketColumn,
      map,
      by = c(),
      sumColumns
    )
    {
      if( !is.data.table( map ) )
        stop( "map must be a data.table with columns old, new and frac" )
      
      if( !( bucketColumn %in% by ) )
        by = c( bucketColumn, by)
      
      private$.bucketColumn <- bucketColumn
      private$.map <- map
      private$.by <- by
      private$.sumColumns <- sumColumns
    },
    
    ###################################################################################/
    # checkMap
    ###################################################################################/
    checkMap = function( data )
    {
      map       <- private$.map
      values    <- data[ , unique( .SD ), .SDcols = private$.bucketColumn ][[ private$.bucketColumn ]]
      mapValues <- map[ , unique( .SD ), .SDcols = "old" ][[ "old" ]]
      
      if( length( setdiff( values, mapValues) ) > 0 )
        stop( "map is missing values in the data columns" )
    },
    
    ###################################################################################/
    # process
    ###################################################################################/
    process = function( data )
    {
      super$process( data )
      self$checkMap( data )
      
      bucketColumn <- private$.bucketColumn 
      map          <- copy( private$.map )
      sumColumns   <- private$.sumColumns
      by           <- private$.by
      
      bucketColumnNew = sprintf( "%s.DATA.ADJUSTER.PROCESS.REBUCKET", bucketColumn )
      fracColumn      = "frac.DATA.ADJUSTER.PROCESS.REBUCKET"
      setnames( map, c( "old", "new", "frac"), c( bucketColumn, bucketColumnNew, fracColumn ) )
      
      data <- map[ data, on = bucketColumn, allow.cartesian = TRUE ]
      for( col_zzz in sumColumns )
        data[ , c( col_zzz ) := .SD * frac.DATA.ADJUSTER.PROCESS.REBUCKET, .SDcols = col_zzz ]
      setnames( data, c( bucketColumn, bucketColumnNew ), c( bucketColumnNew, bucketColumn ) )
      
      data <- data[ , lapply( .SD, sum ), .SDcols = sumColumns, by = by ]
      return( data )
    }
  )
)

