# ------------------------
# Data pull from TCdata360 API
# ------------------------
library(jsonlite)
library(tidyverse)
library(dplyr)
library(tidyr)
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
#for (cou in c("IND","ARM")){
for (cou in filter(countries,(iso3 %in% unique(countries$iso3)))$iso3){
#for (cou in countries$id){
  for (ind in indicators_selected$id){
  print(paste0("Processing...",cou," ",ind))
  thisQuery <- tryCatch(fromJSON(paste0("https://tcdata360-backend.worldbank.org/api/v1/data?countries=",cou,
                               "&indicators=",ind),
                        flatten = TRUE),
                        error = function(e) {print(paste0("Warning: API data call returns an error for country ",cou," and indicator ",ind));
                          thisQuery = data.frame()}, 
                        finally = {thisQuery = data.frame()})
  if (length(thisQuery$data)>0){
    thisQuery <- flatten(thisQuery$data$indicators[[1]])
    if (!is.null(thisQuery$estimated)){
      thisQuery$estimated <- NULL
      thisQuery <- as.data.frame(thisQuery)
    }  
    thisQuery <- thisQuery %>%
      mutate(iso3 = cou)
    names(thisQuery) <- gsub("values.","",names(thisQuery),fixed=TRUE)
    names(thisQuery) <- ifelse(grepl(specialchars,names(thisQuery)),substr(names(thisQuery),1,4),names(thisQuery))
    # consolidate quarterly data by the 4th quarter
    names(thisQuery) <- gsub("Q4","",names(thisQuery))
    thisQuery <- select(thisQuery, -dplyr::contains("Q"))
    
      if (nrow(Report_data)==0) {
        Report_data <- thisQuery
      } else {
        cols <- grep("\\d{4}", names(thisQuery))
        # # catch Yes/No values and remap to 1/0 to avoid bind_rows errors for conversion from numeric to factor
        if(sum(thisQuery[cols] == 'No' | thisQuery[cols] == 'Yes') > 0){
          thisQuery[cols] <- sapply(thisQuery[cols], as.character)
          thisQuery[cols][(thisQuery[cols] == 'No')] <- 0.0
          thisQuery[cols][(thisQuery[cols] == 'Yes')] <- 1.0
          thisQuery[cols] <- sapply(thisQuery[cols], as.numeric)
        }
        Report_data <- bind_rows(Report_data,thisQuery)
        # catch bind_rows errors for Column `2014` can't be converted from numeric to factor
        # Report_data <- tryCatch(bind_rows(Report_data,thisQuery),
        #                         error = function(e) {print(paste0("Warning: bind_rows error for country ",cou," and indicator ",ind));
        #                           cols <- grep("\\d{4}", names(thisQuery));
        #                           thisQuery[cols] <- lapply(thisQuery[cols], factor);
        #                           Report_data <- bind_rows(Report_data,thisQuery)},
        #                         error2 = function(e2) {print(paste0("Warning: persistent bind_rows error for country ",cou," and indicator ",ind));
        #                           Report_data <- bind_rows(Report_data,thisQuery)})
      }
    }
  }
}

# create Period variable
Report_data <- gather(Report_data, Period, Observation, -iso3,-id)

# 
# -----------------------------------------------------------
# This file is normally too big to include in the github project so pick some place in 
# your file system. In the future I might think about slicing it in a way that fits
#write.csv(Report_data,paste0("/Users/asanchez3/Desktop/Work/TCMN/reportGenerator360_data/",input_reportID,"_data.csv"),row.names = FALSE)

write.csv(Report_data,paste0("/Users/mrpso/Documents/GitHub/reportGenerator360_data/",input_reportID,"_data.csv"),row.names = FALSE)


# -----------------------------------------------------------
#
# oldReport_data <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/reportGenerator360_data/Tourism_data.csv")
# oldReport_data <- mutate(oldReport_data, iso3 = as.character(iso3), Period=as.character(Period))
# Report_data <- bind_rows(Report_data,oldReport_data)
# Report_data <- distinct(Report_data, id,iso3,Period,.keep_all = TRUE)
