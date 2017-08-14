### Generate WBG API data

extraData_wbgapi <- data.frame()
dataDesc <- read.csv(paste0("templates/",input_reportID,"_DataDescription.csv"), stringsAsFactors = FALSE)
dataDesc_extra <- subset(dataDesc,dataDesc$Source_ID!="") 
wbg_ind_list <- setdiff(dataDesc_extra$Source_ID, dataDesc_extra$Source_ID[grep("temp_gggr_", dataDesc_extra$Source_ID)])

for(ind in wbg_ind_list){
  print(paste0("Downloading WBG API data for ",ind))
  dl_url <- paste0('http://api.worldbank.org/countries/all/indicators/',ind, '?format=json&per_page=30000' ,sep = "")
  df_ind <- tryCatch(jsonlite::fromJSON(dl_url,flatten = TRUE),
                     error = function(e) {print(paste0("Warning: WBG API data call returns an error for indicator ",ind));
                       df_ind = data.frame()}, 
                     finally = {df_ind = data.frame()})
  df_ind <- as.data.frame(df_ind)
  missInd <- select(df_ind, iso2 = country.id, Period = date, Observation = value) %>%
    mutate(Source_ID = ind) %>%
    join(dataDesc, by = "Source_ID") %>%
    join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
    filter(!is.na(iso3)) %>%
    mutate(Period = as.character(Period)) %>%
    select(Key = Source_ID, Country = name, Period, Observation, CountryCode = iso3, iso2,
           IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
           Section, Subsection, Subsection2, region, Source_ID)
  
  # Append to master data file
  extraData_wbgapi <- bind_rows(extraData_wbgapi, missInd)
}

save(extraData_wbgapi, file="data/Gender_Extra_2.rda")
