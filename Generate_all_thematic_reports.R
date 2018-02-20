#!/usr/bin/env Rscript

packages <- c("jsonlite", "plyr","dplyr", "ggplot2", "gridExtra" ,"data.table",
              "tidyr", "xtable", "stringr", "fmsb", "treemap", "DT", "reshape2",
              "devtools", "sparkline", "Cairo", "shiny", "shinythemes",
              "shinyBS", "shinyjs", "V8", "tidyverse", "scales", "tinytex", "rmarkdown", "knitr")

install.packages(packages, repos = "https://cran.stat.upd.edu.ph/")
# install.packages(packages)

# library(devtools)
# devtools::install_github(c('yihui/tinytex', 'rstudio/rmarkdown', "yihui/knitr"), force=TRUE)
library(rmarkdown)
library(tinytex)
# Sys.getenv("RSTUDIO_PANDOC")
# Sys.setenv(RSTUDIO_PANDOC = "C:/Users/mrpso/AppData/Local/Pandoc")
# rmarkdown:::find_pandoc()
# as.list(rmarkdown:::.pandoc)

#### ----- run FCV
input_reportID <- "FCV"
source('global_utils.R')
source('datapull_TCdata360.R')

#run preprocessing code
Report_data <- ReportDataList[[input_reportID]]
reportConfig <- ReportConfigList[[input_reportID]]
dataDesc <- dataDescList[[input_reportID]]

# Add latest resource rich data
resource_rich <- filter(Report_data, Key %in% 28157) %>%
  subset(Period == max(Period,na.rm = TRUE)) %>%
  mutate(ResourceRich = ifelse(Observation %in% 1,"Yes","No"))
resource_rich <- resource_rich[c('iso2','ResourceRich')]
countries <- merge(countries, resource_rich, by="iso2", all.x=TRUE)

# Add FCV class
fcv_coutyp <- filter(Report_data, Key %in% c(28150, 28151, 28152)) %>%
  subset(Period == max(Period,na.rm = TRUE)) %>%
  subset(Observation == 1)
# fcv_coutyp$FCVclass <- sub("^Fragility Class: ", "", fcv_coutyp$IndicatorShort)
fcv_coutyp$FCVclass <- fcv_coutyp$IndicatorShort
fcv_coutyp <- fcv_coutyp[c("iso2", "FCVclass")]
countries <- unique(merge(countries, fcv_coutyp, by="iso2"))

# Map longer names to existing country typologies
countries$incomeLevel_long <- mapvalues(countries$incomeLevel,
                                        from=c("LIC", "HIC", "UMC", "LMC", "INX"),
                                        to=c("Low Income", "High Income", "Upper Middle Income",
                                             "Lower Middle Income", "Upper Middle Income"))
countries <- mutate(countries, sids_long = ifelse(sids,"Yes","No"), landlocked_long= ifelse(landlocked,"Yes","No"))

# Add latest nominal GDP for non-FCV comparators
nominal_gdp <- filter(Report_data, Key %in% 28107) %>%
  subset(Period == as.numeric(max(Period,na.rm = TRUE)) -2.0) %>%
  mutate(latestNominalGDP = Observation)
nominal_gdp <- nominal_gdp[c('iso2','latestNominalGDP')]
countries <- merge(countries, nominal_gdp, by="iso2", all.x=TRUE)

text_color <- "#404040"

library(scales)
source('Report_Generator_FCVonly.R')

#preprocessing code for Entrep, Tourism
text_color <- "#818181"
region_longname <- read.csv("templates/region_longname.csv")

#### ----- run Entrep, Tourism
for(input_reportID in c("Entrepreneurship", "Tourism")){
  source('global_utils.R')
  source('Report_Generator.R')
}

#### ----- run Gender
input_reportID <- "Gender"
source('global_utils.R')

# run Gender preprocessing
Report_data <- ReportDataList[[input_reportID]]
reportConfig <- ReportConfigList[[input_reportID]]
dataDesc <- dataDescList[[input_reportID]]
text_color <- "#818181"

source('Report_Generator.R')