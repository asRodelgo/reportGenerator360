# ------------------------
# Data pull from TCdata360 API
# ------------------------
library(jsonlite)
library(tidyverse)
library(data360r)
#install.packages('bit64')
library(bit64)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
# Query country metadata:
countries <- tryCatch(fromJSON("https://tcdata360-backend.worldbank.org/api/v1/countries/",
                               flatten = TRUE), 
                      error = function(e) {print("Warning: API call to countries returns an error");
                        countries = read.csv("data/countries.csv", stringsAsFactors = FALSE)}, 
                      finally = {countries = read.csv("data/countries.csv", stringsAsFactors = FALSE)})
# Query indicators:
indicators <- tryCatch(fromJSON("https://tcdata360-backend.worldbank.org/api/v1/indicators/?fields=id%2Cname%2CvalueType%2Crank",
                                flatten=TRUE), 
                       error = function(e) {print("Warning: API call to indicators returns an error");
                         indicators = read.csv("data/indicators.csv", stringsAsFactors = FALSE)}, 
                       finally = {indicators = read.csv("data/indicators.csv", stringsAsFactors = FALSE)})

write.csv(countries, "data/countries.csv", row.names = FALSE)
write.csv(indicators, "data/indicators.csv", row.names = FALSE)

# Read data description file (what goes in the PDF report)
dataDesc <- read.csv(paste0("templates/",input_reportID,"_DataDescription.csv"), stringsAsFactors = FALSE)

indicatorsID <- filter(dataDesc, !is.na(tcdata360_id))$tcdata360_id

indicators_selected <- indicators %>%
  filter(id %in% indicatorsID) %>% 
  distinct(id,.keep_all=TRUE) %>%
  arrange(name)


Report_data <- data.frame()
specialchars <- paste(c("[-]","[.]"),collapse = "|")

for (cou in filter(countries,(iso3 %in% unique(countries$iso3)))$iso3){
  #for (cou in countries$id){
  for (ind in indicators_selected$id){
    print(paste0("Processing...",cou," ",ind))
    thisQuery <- tryCatch(get_data360(indicator_id=ind, country_iso3=cou),
                          error = function(e) {print(paste0("Warning: API data call returns an error for country ",cou," and indicator ",ind));
                            thisQuery = data.frame()}, 
                          finally = {thisQuery = data.frame()})
    
    if (nrow(thisQuery)>0){
      names(thisQuery) <- ifelse(grepl(specialchars,names(thisQuery)),substr(names(thisQuery),1,4),names(thisQuery))
      # consolidate quarterly data by the 4th quarter
      names(thisQuery) <- gsub("Q4","",names(thisQuery))
      thisQuery <- mutate(thisQuery, id = ind) %>%
        select(id,iso3 = `Country ISO3`, everything(),-dplyr::contains("Q"))
      
      if (nrow(Report_data)==0) {
        Report_data <- thisQuery
        
      } else {
        cols <- grep("\\d{4}", names(thisQuery))
        # # catch Yes/No values and remap to 1/0 to avoid bind_rows errors for conversion from numeric to factor
        if(sum(thisQuery[cols] == 'No' | thisQuery[cols] == 'Yes',na.rm=TRUE) > 0){
          thisQuery[cols] <- sapply(thisQuery[cols], as.character)
          thisQuery[cols][(thisQuery[cols] == 'No')] <- 0.0
          thisQuery[cols][(thisQuery[cols] == 'Yes')] <- 1.0
          thisQuery[cols] <- sapply(thisQuery[cols], as.numeric)
        }
        Report_data <- bind_rows(Report_data,thisQuery)
      }
    }
  }
}

# create Period variable
Report_data <- gather(Report_data, Period, Observation, -matches("[A-Z]"))
Report_data <- filter(Report_data, !is.na(Observation))
  
# iso3 id
# -----------------------------------------------------------
# This file is normally too big to include in the github project so pick some place in 
# your file system. In the future I might think about slicing it in a way that fits
#write.csv(Report_data,paste0("/Users/asanchez3/Desktop/Work/TCMN/reportGenerator360_data/",input_reportID,"_data.csv"),row.names = FALSE)

write.csv(Report_data,paste0(getwd(),"_data/",input_reportID,"_data.csv"),row.names = FALSE)

# -----------------------------------------------------------
#
# oldReport_data <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/reportGenerator360_data/Tourism_data.csv")
# oldReport_data <- mutate(oldReport_data, iso3 = as.character(iso3), Period=as.character(Period))
# Report_data <- bind_rows(Report_data,oldReport_data)
# Report_data <- distinct(Report_data, id,iso3,Period,.keep_all = TRUE)
