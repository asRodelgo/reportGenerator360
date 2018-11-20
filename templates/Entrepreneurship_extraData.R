# -----------------------------------------------------------------------
# In case there are indicators not available from the API
# -----------------------------------------------------------------------
# Missing indicators from TCdata360
#"/Users/asanchez3/Desktop/Data Analysis/Entrepreneurship-Ind/Testapp/all datasets.rda")
if (file.exists(paste0(file_extra_root,"/Entrepreneurship_Extra_1.rda"))) {
  load(paste0(file_extra_root,"/Entrepreneurship_Extra_1.rda"))
} else {
  load(paste0(getwd(),"/data/Entrepreneurship_Extra_1.rda"))
}

missInd <- select(all.datasets$WB.data, iso2 = iso2c, Period = year, Observation = one_of("SL.SRV.EMPL.ZS")) %>%
  mutate(Source_ID = "SL.SRV.EMPL.ZS") %>%
  join(ThisDataDesc, by = "Source_ID") %>%
  join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,
         IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
         Section, Subsection, Subsection2, region, Source_ID)

# # Append to master data file
ThisReport_data <- bind_rows(ThisReport_data, missInd)
#
# Ratio online?in store purchases
#load("/Users/asanchez3/Desktop/Work/TCMN/reportGenerator360_data/all datasets.rda")
if (file.exists(paste0(file_extra_root,"/Entrepreneurship_Extra_1.rda"))) {
  load(paste0(file_extra_root,"/Entrepreneurship_Extra_2.rda"))
} else {
  load(paste0(getwd(),"/data/Entrepreneurship_Extra_2.rda"))
}

missInd <- select(all.datasets$consumer.barometer.data, iso2 = iso2c, Observation = one_of("online.ratio")) %>%
  mutate(Source_ID = "online.ratio", Period = 2016, Observation = Observation*100) %>%
  join(ThisDataDesc, by = "Source_ID") %>%
  join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,
         IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
         Section, Subsection, Subsection2, region, Source_ID)

# Append to master data file
ThisReport_data <- bind_rows(ThisReport_data, missInd)

