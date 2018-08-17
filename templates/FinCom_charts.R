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

text_color <- "#818181"

couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)

########## OVERVIEW ##########

# Uses reportConfig$Section[1] directly inside the .Rnw

########## Trade & Investment ##########

## ---- bar_facewrap_chart_Trade ----
bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"bar1")

## ---- table_time_Investment ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[2],c("table1"),str_wrap_size=30)
#table_region(Report_data,reportConfig,couName, reportConfig$Section[2],"table1")
#table_time(Report_data,reportConfig,couName,reportConfig$Section[2],"table1")

## ---- sparklines_Investment ----
#sparklines(Report_data,reportConfig,couName,reportConfig$Section[2],"table1")

## ---- table_time_avg_FinInclus ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[3],c("table2"),str_wrap_size=45)
#table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[3],"table2")

## ---- sparklines_FinInclus ----
#sparklines(Report_data,reportConfig,couName,reportConfig$Section[3],"table2")

########## Competitiveness ##########

## ---- doing_business_table ----
doing_business_table(Report_data,reportConfig,couName)

## ---- bar_top5constraints_chart ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[4],"top5constraints",paste_unit=FALSE,percentBar=TRUE,top5=TRUE)

## ---- competIndic_radar ----
radar_chart(Report_data,reportConfig,couName,reportConfig$Section[4],"competIndic")
#radar_chart_fcv(Report_data,reportConfig,couName,reportConfig$Section[4],"competIndic",max_num_comparators = 4, shortlist_tcdata360_id=631, radar_fontsize=0.75)

## ---- bar_es_chart ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[4],"es",paste_unit=FALSE,percentBar=TRUE)

########## Logistics ##########
## ---- lpi_table_countries ----
table_countries(Report_data,reportConfig,couName, reportConfig$Section[5],"lpi")

########## Sectoral Composition ##########
## ---- sectoral_stackedbar ----
barchart_stacked_FinCom(Report_data,reportConfig,couName, reportConfig$Section[6],"sectoral")

########## Entrepreneurship and Innovation ##########
## ---- table_time_avg_EntrepInnov ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[7],"entrep_table")

## ---- sparklines_EntrepInnov ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[7],"entrep_table")






