# Run this script to generate all the country PDF reports for Investment Climate (FCV) only
# List of countries is based on intersection of TCdata360 country list and Harmonized FCV 2017 list (from WBG IC-FCS team)
##################################
# setwd() to handle images and other files

setwd('/Users/mrpso/Documents/GitHub/reportGenerator360/')
source('global_utils.R') # data and functions needed
source('helper_functions.R') # charts and table functions needed
source('templates/FCV_charts.R') # run preprocessing code in FCV_charts.R

# Create the data reports --------------------------------------
fcv_coulist <- read.csv('templates/FCV_iso3_countrylist.csv', header=FALSE)
include <- fcv_coulist$V1
for (couName in filter(countries, (iso3 %in% include))$name) {
  .reportGenerator(couName, "FCV")
}
