# load global packages ----------------------------------------------
library(plyr) # manipulate data 
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(gridExtra) # ggplot charts side by side
library(data.table) # fast operations
library(tidyr) # transform data
library(xtable) # LaTeX tables
library(stringr) # work with character strings
library(fmsb) # radar charts
require(treemap) # treemap charts
library(DT) # customize dataTable javascript library
library(reshape2) # manipulate data
library(devtools) # allow install packages from source
#install_github('htmlwidgets/sparkline') # install sparklines
library(sparkline) # sparklines
library(knitr) # generate LaTeX PDF report
#install.packages("extrafont")

## load extra fonts for ggplot2 charts
## I don't need it if I'm using Times New Roman
#library(extrafont) 
# look at the loaded fonts
#fonts()
#fonttable()
#font_import(pattern="[T/t]imes New Roman")
#loadfonts(device="postscript")
library(Cairo)
cairo_ps("test.eps", family = "Times")
dev.off()
# avoid scientific notation
options(scipen=999)
thisYear <- substr(Sys.Date(),1,4)

##################
# # global input Report template: Entrepreneurship, Tourism, Investment Climate, etc
# input_reportID <- "Tourism"
# ##################
# # Read template report configuration
# reportConfig <- read.csv(paste0("templates/",input_reportID, "_ReportConfiguration.csv"), stringsAsFactors = FALSE)
# 
# # Read and process data from TCdata360 API ----------------
# source('datapull_TCdata360.R', local = TRUE)
# 
# # Add source links to reportConfig ------------------------
# reportConfig <- select(dataDesc, Source_Name, Source_Link) %>% 
#   distinct(Source_Name, Source_Link) %>%
#   right_join(reportConfig, by = c("Source_Name" = "Section_Description")) %>%
#   select(everything(), Section_Description = Source_Name) %>%
#   arrange(Section_Level, Order)
#        

# Initiate values on UI -----------------------------------
library(jsonlite)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
# Query country metadata:
countries <- tryCatch(fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                               flatten = TRUE), 
                      error = function(e) {print("Warning: API call to countries returns an error")}, 
                      finally = {countries = read.csv("data/countries.csv", stringsAsFactors = FALSE)})
# Query indicators:
indicators <- tryCatch(fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators/?fields=id%2Cname%2CvalueType%2Crank",
                                flatten=TRUE), 
                       error = function(e) {print("Warning: API call to indicators returns an error")}, 
                       finally = {indicators = read.csv("data/indicators.csv", stringsAsFactors = FALSE)})
# List topics
topics <- c("Entrepreneurship","Tourism")

# Read and process data from TCdata360 API ----------------
source('datapull_TCdata360.R', local = TRUE)
# Load functions: charts, tables and report generator
source("helper_functions.R", local = TRUE)

# Auxiliary functions -------------------------------------

.getISO2 <- function(couName){
  
  countryISO2 <- tolower(as.character(filter(countries,name==couName)$iso2))
}

.getRegion <- function(couName){
  
  cou <- .getCountryCode(couName)
  region <- as.character(countries[countries$iso3==cou,]$region) 
}

.getCountryCode <- function(couName){
  
  countryCode <- filter(countries, name==couName)$iso3
  if (length(countryCode)==1){
    return(countryCode)
  } else{
    return(0)
  }
}

# country flags -----------------------------------
.outFlag <- function(couName){
  
  iso <- .getISO2(couName)  
  if (paste0(iso,".png")==".png"){
    
    tags$img(src="world.png", width="40%")  
    
  } else{
    
    tags$img(src=paste0(iso,".png"), width="40%")  
  } 
  
}

.generatePDFReports <- function(couNameList){
  
  for (c in couNameList) {
    print(paste("Report generated successfully for",c))
  }
  
}
