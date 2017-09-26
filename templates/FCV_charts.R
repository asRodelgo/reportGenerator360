#######################################################
# R functions to generate charts and tables in Generic report
#
# asanchezrodelgo@ifc.org - Jun 2016
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- parameters ----
couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)
# Load data
Report_data <- ReportDataList[[input_reportID]]
reportConfig <- ReportConfigList[[input_reportID]]
dataDesc <- dataDescList[[input_reportID]]

#### Add new columns to countries dataframe to include FCS typology
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
countries <- merge(countries, fcv_coutyp, by="iso2", all.x=TRUE)

# Map longer names to existing country typologies
countries$incomeLevel_long <- mapvalues(countries$incomeLevel,
                                        from=c("LIC", "HIC", "UMC", "LMC", "INX"),
                                        to=c("Low Income", "High Income", "Upper Middle Income",
                                             "Lower Middle Income", "Upper Middle Income"))
countries <- mutate(countries, sids_long = ifelse(sids,"Yes","No"), landlocked_long= ifelse(landlocked,"Yes","No"))

# Add latest nominal GDP for non-FCV comparators
nominal_gdp <- filter(Report_data, Key %in% 28107) %>%
  subset(Period == as.numeric(max(Period,na.rm = TRUE)) -1.0) %>%
  mutate(latestNominalGDP = Observation)
nominal_gdp <- nominal_gdp[c('iso2','latestNominalGDP')]
countries <- merge(countries, nominal_gdp, by="iso2", all.x=TRUE)


text_color <- "#404040"
########## Header ##########

## ---- fcvtext ----
fcvtext(countries, couName)

########## OVERVIEW ##########

## ---- fcvtable ----
fcvtable(Report_data,reportConfig,couName,reportConfig$Section[1],"fcvtable")

########## Macroeconomic Trend ##########
## ---- table_time_avg_1.1 ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.1", fcv=TRUE, arrange_by_datadesc=TRUE)

## ---- sparklines1.1 ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.1", num_period=10, arrange_by_datadesc=TRUE)


########## Investment Trend ##########
## ---- table_time_avg_1.2 ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.2", fcv=TRUE, arrange_by_datadesc=TRUE)

## ---- sparklines1.2 ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.2", num_period=10, arrange_by_datadesc=TRUE)

########## Crossborder Flow Trend ##########
## ---- table_time_avg_1.3 ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.3", fcv=TRUE, arrange_by_datadesc=TRUE)

## ---- sparklines1.3 ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[1],"table1.3", num_period=10, arrange_by_datadesc=TRUE)

########## DOING BUSINESS ##########

## ---- slope2.1dtf ----
line_chart_avg(Report_data,reportConfig,couName, reportConfig$Section[2], "slope2.1dtf", minTime="2015",neighbor="fcv",max_neighbors=1)

## ---- slope2.1rank ----
line_chart_avg(Report_data,reportConfig,couName, reportConfig$Section[2], "slope2.1rank", minTime="2015",neighbor="fcv",max_neighbors=1)

## ---- radar2.1 ----
radar_chart_fcv(Report_data,reportConfig,couName,reportConfig$Section[2],"radar2.1",max_num_comparators = 4, shortlist_tcdata360_id=416)

########## WEF GLOBAL COMPETITIVENESS INDEX ##########

## ---- radar2.2 ----
radar_chart_fcv(Report_data,reportConfig,couName,reportConfig$Section[2],"radar2.2",max_num_comparators = 4, shortlist_tcdata360_id=631, radar_fontsize=0.75)

########## WORLDWIDE GOVERNANCE INDICATORS ##########

## ---- bar_facewrap3.1 ----
bar_facewrap_chart_fcv(Report_data,reportConfig,couName, reportConfig$Section[3],"bar_facewrap3.1",
                       append_unit = FALSE, max_num_comparators = 4, dataset="wgi", str_wrap_size = 20, range_vals=c(-2.5, -1, 0, 1, 2.5))

########## COUNTRY POLICY AND INSTITUTIONAL ASSESSMENTS ##########

## ---- fcvtext_cpia ----
fcvtext_cpia(countries, couName, dataset="cpia")

## ---- bar_facewrap3.2.1 ----
bar_facewrap_chart_fcv(Report_data,reportConfig,couName, reportConfig$Section[3],"bar_facewrap3.2.1",
                       append_unit = FALSE, max_num_comparators = 4, dataset="cpia", str_wrap_size = 20, range_vals=c(0,1,2,3,4,5,6))

## ---- bar_facewrap3.2.2 ----
bar_facewrap_chart_fcv(Report_data,reportConfig,couName, reportConfig$Section[3],"bar_facewrap3.2.2",
                       append_unit = FALSE, max_num_comparators = 4, dataset="cpia", str_wrap_size = 20, range_vals=c(0,1,2,3,4,5,6))

########## TOP 5 OBSTACLES ##########

## ---- bar3.3 ----
bar_chart_fcv(Report_data,reportConfig,couName,reportConfig$Section[3],"bar3.3")

## ---- barclass3.3 ----
bar_chart_fcv_class(Report_data,reportConfig,couName,reportConfig$Section[3],"bar3.3")
