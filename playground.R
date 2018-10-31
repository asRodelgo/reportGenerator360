library(tidyverse)
library(ggplot2)

####### 3-bar plot
mydata <- mutate(mtcars, car = rownames(mtcars)) %>%
  filter(grepl("Merc 2",car)) %>%
  gather(variable, value, -car) %>%
  filter(variable %in% c("wt","gear","drat")) %>%
  mutate(value = ifelse(variable == "gear", -value, value))

ggplot(data = mydata, aes(car,value,fill=variable)) +
  geom_bar(data = filter(mydata, variable == "drat"),stat = 'identity', position = 'dodge') +
  geom_bar(data = filter(mydata, variable == "gear"), stat = 'identity', alpha = .5) +
  geom_bin2d(data = filter(mydata, variable == "wt"), position = 'dodge')

####### faceted bar-line chart
mydata2 <- mutate(mydata, chart_type = ifelse(variable == "wt","line","bar"))

secondFacet <- FALSE # see below
ggplot(data = mydata2, aes(car,value,fill=variable)) +
  facet_grid(chart_type~., scale = "free") +
  #geom_bar(data = filter(mydata, variable == "drat"),stat = 'identity', position = 'dodge') +
  #geom_bar(data = filter(mydata, variable == "gear"), stat = 'identity', alpha = .5) +
  geom_bar(data = filter(mydata, variable %in% c("drat","gear")), aes(fill = variable),stat = 'identity') +
  geom_bin2d(data = filter(mydata, variable == "wt")) +
  scale_y_continuous(name = NULL, labels = function(b) {
    if(!secondFacet) {
      secondFacet <<- TRUE # this is a little cray (and relies on dtF seq = facet seq; works though)
      return(paste0(round(b * 100, 0), "%"))
    }else{
      return(b)
    }
  })

##### faceted double axis
# define dummy dataset
mydata3 <- data.frame(Period=c(2014,2015, 2016, 2017), 
                 Country = c("Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia","Ethiopia"),
                 Indicator=c("Current Account","Current Account","Current Account","Current Account",
                             "Current Account","Current Account","Current Account","Current Account",
                             "Trade Balance","Trade Balance","Trade Balance","Trade Balance",
                             "Trade Balance","Trade Balance","Trade Balance","Trade Balance"), 
                 Unit = c("USD","USD","USD","USD","%GDP","%GDP","%GDP","%GDP",
                          "USD","USD","USD","USD","%GDP","%GDP","%GDP","%GDP"),
                 Observation=c(-100, -110, -125, -96, -2.45, -3.6, -5.1, -4.3, 
                               -120, -137, -145, -161, -8.45, -6.6, -7.1, -10.3))

secondFacet <- FALSE # see below
ggplot(data = mydata3, mapping = aes(x = Period, y = Observation)) +
  facet_grid(Unit~., scale = "free") +
  geom_bar(data = filter(mydata3, Unit == "USD"), aes(fill = Indicator),stat = 'identity') +
  geom_line(data = filter(mydata3, Unit == "%GDP"), aes(colour = Indicator)) +
  scale_y_continuous(name = NULL, labels = function(b) {
    if(!secondFacet) {
      secondFacet <<- TRUE # this is a little cray (and relies on dtF seq = facet seq; works though)
      return(paste0(round(b * 100, 0), "%"))
    }else{
      return(b)
    }
  })

####### double y-axis
# scale the data
mydata4 <- group_by(mydata3, Unit) %>%
  mutate(maxObs = max(Observation), minObs = min(Observation)) %>%
  #mutate(Scaled_Observation = sign(Observation)*(Observation-min(Observation))/(max(Observation)-min(Observation))) %>%
  #ungroup() %>%
  mutate(Scaled_Observation = ifelse(maxObs < 0,-.1-(Observation-maxObs)/(minObs-maxObs),
                                     .1+(Observation-minObs)/(maxObs-minObs)))

maxObs <- max(filter(mydata4, Unit == "USD")$Observation)
minObs <- min(filter(mydata4, Unit == "USD")$Observation)
maxObs2 <- max(filter(mydata4, !(Unit == "USD"))$Observation)
minObs2 <- min(filter(mydata4, !(Unit == "USD"))$Observation)

ggplot(data = mydata4, mapping = aes(x = Period, y = Scaled_Observation)) +
  geom_bar(data = filter(mydata4, Unit == "USD"), aes(fill = Indicator), alpha = .5,stat = 'identity') +
  geom_line(data = filter(mydata4, Unit == "%GDP"), aes(colour = Indicator), size = 1) +
  geom_point(data = filter(mydata4, Unit == "%GDP"), aes(colour = Indicator), size = 4) +
  scale_y_continuous(name = "USD", labels = function(a) { paste0(round((a + .1)* (maxObs-minObs) + maxObs, 0), "$")},
                     sec.axis = sec_axis(~., name = "%GDP", 
                                         labels = function(b) { paste0(round((b+.1) * (maxObs2-minObs2) + maxObs2, 1), "%")}))

#############################
# Top 5 exports (% of total value of exports)
library(data360r)
# search indicator
exportShare <- search_360("Chemicals", search_type="indicator", limit_results = 5)
# Export Product Share ID: 2758
# Export Product Share ID: 2759
dataExportShare <- get_data360(indicator_id=2758, country_iso3="USA") %>%
  gather(Period, Observation, -matches("[A-Z]")) %>%
  filter(Period == max(Period)) %>%
  arrange(desc(Observation)) %>%
  top_n(5, Observation)

dataImportShare <- get_data360(indicator_id=2759, country_iso3="USA") %>%
  gather(Period, Observation, -matches("[A-Z]")) %>%
  filter(Period == max(Period)) %>%
  arrange(desc(Observation)) %>%
  top_n(5, Observation)

dataImportShare <- get_data360(indicator_id=786, country_iso3="BRA")

################################################
# Suggested Peers methodology
data <- Report_data %>%
  filter(Subsection2 == "peers", !is.na(Observation)) %>%
  mutate(Period = ifelse(is.na(Period),as.character(as.numeric(thisYear)-1),Period),
         Observation = Observation/ifelse(is.na(Scale),1,Scale)) %>%
  group_by(Key) %>%
  filter(Period == max(Period)) %>%
  mutate(rank = percent_rank(Observation))

myCountry <- "Ethiopia"
perRank_myCountry <- filter(data, Country == myCountry) %>%
  ungroup() %>%
  mutate(Key = if_else(grepl("Population", IndicatorShort),"MPOP","MGDP")) %>%
  select(Key,Country,rank) %>%
  spread(Key,rank)
  
suggestedPeers <- filter(data, !(Country == myCountry)) %>%
  ungroup() %>%
  mutate(Key = if_else(grepl("Population", IndicatorShort),"POP","GDP")) %>%
  select(Key,Country,rank) %>%
  spread(Key, rank) %>%
  mutate(score = (abs(GDP - perRank_myCountry$MGDP)+abs(POP - perRank_myCountry$MPOP))/2) %>%
  arrange(score) %>%
  top_n(5,desc(score))
  
#####################################################
# Pull indicators for MTI poverty dataset EFI exercise: 10/29/2018
library(tidyverse)

## TCdata360 indicators -------------------------------------------

library(data360r)
# Indicators MTI
data <- read.csv("C:/Users/wb493327/OneDrive - WBG/CEM_20/EFI_Poverty_Indicators_MTI.csv", stringsAsFactors = FALSE)
indicator_ids <- data$IndicatorID
# All Countries
countries <- tryCatch(fromJSON("https://tcdata360-backend.worldbank.org/api/v1/countries/",
                               flatten = TRUE), 
                      error = function(e) {print("Warning: API call to countries returns an error");
                        countries = read.csv("data/countries.csv", stringsAsFactors = FALSE)}, 
                      finally = {countries = read.csv("data/countries.csv", stringsAsFactors = FALSE)})
# Countries EFI
countries_efi <- read.csv("C:/Users/wb493327/OneDrive - WBG/CEM_20/EFI_Poverty_Indicators_countryList.csv", stringsAsFactors = FALSE)
countryCodes <- sapply(countries_efi$Country, .getCountryCode)
#
specialchars <- paste(c("[-]","[.]"),collapse = "|")
#
Report_data <- data.frame()
for (cou in countryCodes){
  for (ind in indicator_ids[which(indicator_ids != 360)]){
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

Report_data <- gather(Report_data, Period, Value, -c(id,iso3,`Country Name`,`Subindicator Type`,Product))
tc360_data <- select(Report_data, `Indicator Code` = id, `Indicator Name` = Indicator, Unit = `Subindicator Type`,
                     `Country ISO3` = iso3, Year = Period, Product, Value)

## WEO indicators from source --------------------------------------------

weo_data <- read.csv("C:/Users/wb493327/OneDrive - WBG/CEM_20/WEO_data.csv", stringsAsFactors = FALSE)

weo_data <- select(weo_data, `Country ISO3` = ISO, `Country Name` = Country, `Indicator Name` = Subject.Descriptor,
                   Unit = Units, starts_with("X"), contains("Estimate")) %>%
  rename_at(vars(starts_with("X")),funs(gsub("X","",.))) %>%
  gather(Year, Value, -c(`Country ISO3`,`Country Name`, `Indicator Name`, Unit)) %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(`Country ISO3` %in% countryCodes)


  










