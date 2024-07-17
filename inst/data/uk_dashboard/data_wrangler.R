#   
#  data_wrangler.R - reads the raw data from the UK Goverment website, applies
#                    simple aggregations and stores in small files for use in the 
#                    package
#
#  The data in this directory was downloaded from the UK Government webiste from
#  the url https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download
#  on 17th July 2024 at 13:00 BST. 
library( data.table )

# the location of the unzipped data directly downloaded from the UK Government site
archive_dir = "~/data/covid-19-archive/"
data_dir    = "inst/data/uk_dashboard/"

# get the timeseries of cases by age for England by aggregating data over all UTLA
file_cases_age_utla = "Cases/2022/newCasesBySpecimenDateAgeDemographics_utla_2022.csv"
file_cases_age      = "englandCasesByAge.csv"
dt_case_age_utla = fread( sprintf( "%s%s", archive_dir, file_cases_age_utla ))

# get the total for England only by age_group
dt_case_age = dt_case_age_utla[ substr( area_code,1,1) == "E", .(cases = sum(cases)), by = c( "date", "age")]
dt_case_age = dt_case_age[ !( age %in% c( "unassigned", "00_59", "60+"))]
fwrite( dt_case_age, file = sprintf( "%s%s", data_dir, file_cases_age), sep = "," )
