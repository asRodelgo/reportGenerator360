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

########## TRADE ##########

## ---- double_yaxis_bar_peers_trade ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[2], "bar1", country_peers = NULL, 
                       double_yaxis = TRUE, timeline = FALSE)

## ---- double_yaxis_bar_time_trade ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[2], "bar1", country_peers = NULL, 
                       double_yaxis = TRUE, timeline = TRUE)

## ---- bar_chart_exports ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"top5exports",paste_unit=FALSE,percentBar=TRUE,top5=TRUE,products = TRUE)

## ---- bar_chart_imports ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"top5imports",paste_unit=FALSE,percentBar=TRUE,top5=TRUE,products = TRUE)


########## INVESTMENT ##########

## ---- double_yaxis_bar_peers_inv ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[3], "inv_bar1", country_peers = NULL, 
                       double_yaxis = TRUE, timeline = FALSE, computeTotals = TRUE)

## ---- double_yaxis_bar_time ----
line_chart(Report_data,reportConfig,couName, reportConfig$Section[3], "inv_line2", minTime="1900",
           neighbor="region",max_neighbors=4, show_last_year=FALSE, show_data_labels=NULL, plot_spacing=0.0)

## ---- sparkline1 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark1",rankBig=FALSE)

## ---- sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark2",rankBig=FALSE)

## ---- sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark3",rankBig=FALSE)

## ---- sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark4",rankBig=FALSE)

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






