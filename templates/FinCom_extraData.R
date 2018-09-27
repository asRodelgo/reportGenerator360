# -----------------------------------------------------------------------
# In case there are indicators not available from the API
# -----------------------------------------------------------------------
# Missing indicators from TCdata360
missInd <- read.csv(paste0(file_root,topic,"_extraData.csv"), stringsAsFactors = FALSE) %>%
  join(ThisDataDesc, by = "Source_ID") %>%
  join(countries[,c("iso3","iso2","name","region","adminRegion")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,
         IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
         Section, Subsection, Subsection2, region, adminRegion, Source_ID)

# # Append to master data file
ThisReport_data <- bind_rows(ThisReport_data, missInd)
#