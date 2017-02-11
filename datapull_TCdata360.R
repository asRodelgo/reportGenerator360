# ------------------------
# Data pull from TCdata360 API
# ------------------------
library(jsonlite)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
# Query country metadata:
countries <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                      flatten = TRUE)
# Query indicators:
indicators <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators?fields=id%2Cname%2Cdataset%2CvalueType%2CdatasetId%2Cnotes%2Cproperties%2Crank%2Cdefinition",
                       flatten=TRUE)
# read data extracted from API. 
## ---- Run Writer_Report_data.R to update data from TCdata360 API
Report_data <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/Entrepreneurship_data/Report_data.csv",stringsAsFactors = FALSE)

# Read data description file (what goes in the PDF report)
dataDesc <- read.csv("DataDescription.csv", stringsAsFactors = FALSE)

# Add descriptors and source fields
Report_data <- merge(Report_data,dataDesc, by.x = "id", by.y = "tcdata360_id")
Report_data <- merge(Report_data, countries[,c("iso3","iso2","name","region")],by="iso3",all.x = TRUE)
# clean up: remove duplicate columns
Report_data <- Report_data %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = id, Country = name, Period, Observation, CountryCode = iso3, iso2,  
         IndicatorShort = varname, Source = Source_Link, Unit = Unit.of.Measure, 
         Section, Subsection, Subsection2, region)

# ---------------------------------
# Extra data: When indicators are not available in the API there will be an extraData file
# that processes the missing data
# ---------------------------------
if (file.exists(paste0("/templates/",input_reportID,"extraData.R"))) 
  source(paste0("/templates/",input_reportID,"extraData.R"))


