# -----------------------------------------------------------------------
# In case there are indicators not available from the API
# -----------------------------------------------------------------------
# Missing indicators from TCdata360
load("/Users/asanchez3/Desktop/Data Analysis/Entrepreneurship-Ind/Testapp/all datasets.rda")
missInd <- select(all.datasets$WB.data, iso2 = iso2c, Period = year, Observation = one_of("SL.SRV.EMPL.ZS")) %>%
  mutate(Source_ID = "SL.SRV.EMPL.ZS") %>%
  join(dataDesc, by = "Source_ID") %>%
  join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,
         IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
         Section, Subsection, Subsection2, region, Source_ID)

# # Append to master data file
Report_data <- bind_rows(Report_data, missInd)
#
# Ratio online?in store purchases
load("/Users/asanchez3/Desktop/Work/TCMN/Entrepreneurship_data/all datasets.rda")
missInd <- select(all.datasets$consumer.barometer.data, iso2 = iso2c, Observation = one_of("online.ratio")) %>%
  mutate(var = "online.ratio", Period = 2016, Observation = Observation*100) %>%
  join(dataDesc, by = "Source_ID") %>%
  join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,
         IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
         Section, Subsection, Subsection2, region, Source_ID)

# Append to master data file
Report_data <- bind_rows(Report_data, missInd)

