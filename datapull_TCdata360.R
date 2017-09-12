# ------------------------
# Data pull from TCdata360 API
# ------------------------
require(jsonlite)
# my local mac vs shiny-server
if (getwd() == "C:/Users/mrpso/Documents/GitHub/reportGenerator360"){
  file_root <- "C:/Users/mrpso/Documents/GitHub/reportGenerator360_data/"
  file_extra_root <- file_root
}
#else {
#  file_root <- "/srv/shiny-server/reportGenerator360_data/"
#  file_extra_root <- "data/"
#}
# read data extracted from API.
## ---- Run Writer_Report_data.R to update data from TCdata360 API
ReportDataList <- list()
for (topic in topics){

  ThisReport_data <- read.csv(paste0(file_root,topic,"_data.csv"),stringsAsFactors = FALSE)
    # Read data description file (what goes in the PDF report)
  ThisDataDesc <- read.csv(paste0("templates/",topic,"_DataDescription.csv"), stringsAsFactors = FALSE)

  # Add descriptors and source fields
  ThisReport_data <- merge(ThisReport_data,ThisDataDesc, by.x = "id", by.y = "tcdata360_id")
  
  ThisReport_data <- merge(ThisReport_data, countries[,c("iso3","iso2","name","region","adminRegion","incomeLevel")],by="iso3",all.x = TRUE)
  # clean up: remove duplicate columns
  ThisReport_data <- ThisReport_data %>%
    filter(Period <= thisYear) %>%
    mutate(Period = as.character(Period), Scale = ifelse(is.na(Scale),1,Scale)) %>%
    select(Key = id, Country = name, Period, Observation, Scale, CountryCode = iso3, iso2,
           IndicatorShort = Indicator_Short, Source_Name, Source_Link, Unit = Unit_Short,
           Section, Subsection, Subsection2, region, adminRegion, incomeLevel, Source_ID)

  # ---------------------------------
  # Extra data: When indicators are not available in the API there will be an extraData file
  # that processes the missing data
  # ---------------------------------
  if (file.exists(paste0("templates/",topic,"_extraData.R")))
    source(paste0("templates/",topic,"_extraData.R"), local = TRUE)

  ReportDataList[[topic]] <- ThisReport_data

}

ReportConfigList <- list()
dataDescList <- list()
for (topic in topics){
  # Read template report configuration
  ThisReportConfig <- read.csv(paste0("templates/",topic,"_ReportConfiguration.csv"), stringsAsFactors = FALSE)
  
  # Read data description file (what goes in the PDF report)
  ThisDataDesc <- read.csv(paste0("templates/",topic,"_DataDescription.csv"), stringsAsFactors = FALSE)
  dataDescList[[topic]] <- ThisDataDesc
  # Add source links to reportConfig ------------------------
  ThisReportConfig <- left_join(ThisReportConfig,ThisDataDesc[,c("Source_Name", "Source_Link")], by = c("Section_Description" = "Source_Name")) %>%
    #distinct(Section_Description, Source_Link, .keep_all = TRUE) %>%
    arrange(Section_Level, Order)
  
  ReportConfigList[[topic]] <- ThisReportConfig
  
}

