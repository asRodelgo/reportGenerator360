#######################################################
# R functions to generate charts and tables in Generic report
#
# asanchezrodelgo@ifc.org - Jun 2016
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- parameters ----

# Load data
Report_data <- ReportDataList[[input_reportID]]
reportConfig <- ReportConfigList[[input_reportID]]
dataDesc <- dataDescList[[input_reportID]]

# Map longer names to existing country typologies
countries$incomeLevel_long <- mapvalues(countries$incomeLevel,
                                        from=c("LIC", "HIC", "UMC", "LMC", "INX"),
                                        to=c("Low Income", "High Income", "Upper Middle Income",
                                             "Lower Middle Income", "Upper Middle Income"))
countries <- mutate(countries, sids_long = ifelse(sids,"Yes","No"), landlocked_long= ifelse(landlocked,"Yes","No"))

text_color <- "#404040"

couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)
########## Header ##########

## ---- fcvtext ----
# fcvtext(countries, couName)

########## OVERVIEW ##########