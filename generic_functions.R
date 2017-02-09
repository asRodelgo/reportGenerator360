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

# avoid scientific notation
options(scipen=999)
thisYear <- substr(Sys.Date(),1,4)

# Read and process data from TCdata360 API ----------------
source('datapull_TCdata360.R')

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

####################################################
# OLD CODE TO RETRIEVE AND PROCESS DATA
####################################################
# # PDF Offline Report generator --------------------------
# # Read data
# # New data source added Dec17_2016. Not yet implemented
# load("/Users/asanchez3/Desktop/Data Analysis/Entrepreneurship-Ind/Testapp/all datasets.rda")
#   # Configuration file
#   # config_file <- read.csv("/Users/asanchez3/Desktop/Data Analysis/Entrepreneurship-Ind/Testapp/config/vars and names.csv")
# #load("Entrepr_DataByCategory.rda")
# #load("datasets by dimension_new.rda")
# dataDesc <- read.csv("Entrepr_DataDescription.csv", stringsAsFactors = FALSE)
# # country table ----------------------------
# countries <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/CountryClassification.csv", stringsAsFactors = FALSE)
# countries[countries$CountryCodeISO3=="NAM",]$CountryCodeISO2 <- "NA"
# # list of only countries (useful for selectors and others)
# countryNames <- filter(countries, !(CountryCodeISO2==""))
# countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2
# # transform data for easier processing
# Entrepr_data <- data.frame()
# for (i in 1:length(all.datasets)){
#   #for (j in 1:length(all.datasets[[i]])){
#     thisDataFrame <- as.data.frame(sapply(all.datasets[[i]], as.character), stringsAsFactors = FALSE)
#     thisGather <- gather(thisDataFrame, Key, Observation, -one_of("iso2c","country","Country", "year", "iso3c", "Year"))
#     thisGather$Observation <- as.numeric(thisGather$Observation)
#     thisGather <- mutate(thisGather, Category = names(all.datasets)[i])
#     if (nrow(Entrepr_data)>0) {
#       Entrepr_data <- bind_rows(Entrepr_data, thisGather)
#     } else {
#       Entrepr_data <- thisGather
#     }
#   #}
# }
# # Add descriptors and source fields
# Entrepr_data <- merge(Entrepr_data, dataDesc[,c("var","varname","Source_Link","Unit.of.Measure","Section","Subsection","Subsection2")], by.x="Key", by.y = "var", all.x = TRUE)
# Entrepr_data <- merge(Entrepr_data, countries[,c("CountryCodeISO3","CountryCodeISO2")],by.x="iso2c",by.y="CountryCodeISO2",all.x = TRUE)
# # clean up: remove duplicate columns
# Entrepr_data <- Entrepr_data %>%
#   mutate(Year = ifelse(is.na(Year),year,Year),
#                        Country = ifelse(is.na(Country),country,Country)) %>%
#   select(Key, Category, Observation, CountryCode = CountryCodeISO3, iso2c, Period = Year, 
#          IndicatorShort = varname, Source = Source_Link, Unit = Unit.of.Measure, Section, Subsection, Subsection2)
# ## Hardcode some periods missing in the source data
# # Investment Across Borders
# Entrepr_data$Period <- ifelse(grepl("iab_",Entrepr_data$Category),"2012",Entrepr_data$Period)
# 
# # might need TCMN data for some charts/tables
# TCMN_data <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))
# indicators <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/IndicatorClassification.csv", stringsAsFactors = FALSE)
# TCMN_sources <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_sources.csv", stringsAsFactors = FALSE)
# TCMN_indic <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_Indicators.csv", stringsAsFactors = FALSE)


