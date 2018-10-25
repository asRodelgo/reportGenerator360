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

#countryPeers <- c("TZA","KEN","UGA","RWA","ETH")
# countryCode <- .getCountryCode(couName)
# if (countryCode %in% countryPeers){
#   countryPeers <- countryPeers[-which(countryPeers == countryCode)]
# } else {
#   couRegion <- countries[countries$iso3==countryCode,]$region  # obtain the region for the selected country
#   neighbors <- countries[countries$region==couRegion,]$iso3 # retrieve all countries in that region
#   neighbors <- as.character(neighbors[!(neighbors==countryCode)])
#   set.seed(123)
#   countryPeers <- sample(neighbors,4)
# }

########## OVERVIEW ##########

# Uses reportConfig$Section[1] directly inside the .Rnw

########## TRADE ##########

## ---- double_yaxis_bar_peers_trade ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[2], "bar1", country_peers = countryPeers, 
                       double_yaxis = TRUE, timeline = FALSE)

## ---- double_yaxis_bar_time_trade ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[2], "bar1", country_peers = countryPeers, 
                       double_yaxis = TRUE, timeline = TRUE)

## ---- bar_chart_exports ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"top5exports",paste_unit=FALSE,percentBar=TRUE,top5=TRUE,products = TRUE)

## ---- bar_chart_imports ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"top5imports",paste_unit=FALSE,percentBar=TRUE,top5=TRUE,products = TRUE)


########## INVESTMENT ##########

## ---- double_yaxis_bar_peers_inv ----
barchart_double_y_axis(Report_data,reportConfig,couName, reportConfig$Section[3], "inv_bar1", country_peers = countryPeers, 
                       double_yaxis = TRUE, timeline = FALSE, computeTotals = 345)

## ---- double_yaxis_bar_time ----
line_chart(Report_data,reportConfig,couName, section = reportConfig$Section[3], table = "inv_line2", minTime=as.character(as.numeric(thisYear)-10),
           neighbor=countryPeers,max_neighbors=4, show_last_year=FALSE, show_data_labels=NULL, plot_spacing=0.0)

## ---- sparkline1 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark1",rankBig=FALSE,includeRank = FALSE)

## ---- sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark2",rankBig=FALSE,includeRank = FALSE)

## ---- sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark3",rankBig=FALSE,includeRank = FALSE)

## ---- sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName,"inv_spark4",rankBig=FALSE,includeRank = FALSE)

########## SECTORAL OVERVIEW ##########

## ---- sectoral_stackedbar ----
barchart_stacked_FinCom(Report_data,reportConfig,couName, section = reportConfig$Section[4], table = "sectoral", country_peers = countryPeers)

## ---- sectoral_piechart ----
pie_chart(Report_data,reportConfig,couName, section = reportConfig$Section[4], table = "pie1")

## ---- sectoral_valueAdded ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[4],"sec_valueAdd",str_wrap_size=45,rankBig=FALSE,includeUnit=FALSE, round_off=0, compareRegion = "SSA",includePeriod = TRUE)

## ---- sector_sparkline1 ----
figure_sparkline(Report_data,reportConfig,couName,"sec_spark1",rankBig=FALSE,includeRank = FALSE)

## ---- sector_sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName,"sec_spark2",rankBig=FALSE,includeRank = FALSE)

## ---- sector_sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName,"sec_spark3",rankBig=FALSE,includeRank = FALSE)

## ---- sector_sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName,"sec_spark4",rankBig=FALSE,includeRank = FALSE)

########## Finance ##########
## ---- fin_table_access ----
number_chart(Report_data,reportConfig,couName,section = reportConfig$Section[5],table = "access",str_wrap_size=45,rankBig=FALSE,includeUnit=TRUE, round_off=0, compareRegion = "SSA",includePeriod = TRUE)
## ---- fin_table_stability ----
number_chart(Report_data,reportConfig,couName,section = reportConfig$Section[5],table = "access2",str_wrap_size=45,rankBig=FALSE,includeUnit=TRUE, round_off=0, compareRegion = "SSA",includePeriod = TRUE)
## ---- fin_table_longTerm ----
number_chart(Report_data,reportConfig,couName,section = reportConfig$Section[5],table = "stability",str_wrap_size=45,rankBig=FALSE,includeUnit=TRUE, round_off=0, compareRegion = "SSA",includePeriod = TRUE)

########## Competitiveness ##########

## ---- doing_business_table ----
doing_business_table(Report_data,reportConfig,couName)

## ---- bar_top5constraints_chart ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[6],"top5constraints",paste_unit=FALSE,percentBar=TRUE,top5=TRUE)

## ---- competIndic_radar ----
radar_chart(Report_data,reportConfig,couName,reportConfig$Section[6],"competIndic")
#radar_chart_fcv(Report_data,reportConfig,couName,reportConfig$Section[4],"competIndic",max_num_comparators = 4, shortlist_tcdata360_id=631, radar_fontsize=0.75)

## ---- bar_es_chart ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[6],"es",paste_unit=FALSE,percentBar=TRUE)

########## Logistics ##########
## ---- lpi_table_countries ----
#table_countries(Report_data,reportConfig,couName, reportConfig$Section[7],"lpi")
number_chart(Report_data,reportConfig,couName,reportConfig$Section[7],"lpi",str_wrap_size=25,rankBig=FALSE,includeUnit=FALSE, round_off=0, compareRegion = "SSA",includePeriod = FALSE)

## ---- entrepr_table_countries ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[8],"entrep_table",str_wrap_size=35,rankBig=FALSE,includeUnit=TRUE, round_off=0, compareRegion = "SSA",includePeriod = FALSE)

########## Entrepreneurship and Innovation ##########
## ---- table_time_avg_EntrepInnov ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[8],"entrep_table")

## ---- sparklines_EntrepInnov ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[8],"entrep_table")

########## EBA ##########
## ---- table_agricultural_trade_index ----
barchart_benchmark(Report_data,reportConfig,couName, section = reportConfig$Section[9], table = "table1", country_peers = countryPeers, benchmark = TRUE)

## ---- table_agricultural_cost_export ----
barchart_benchmark(Report_data,reportConfig,couName, section = reportConfig$Section[9], table = "table2", country_peers = countryPeers, benchmark = TRUE)

## ---- table_eba_indicators ----
number_chart(Report_data,reportConfig,couName,section = reportConfig$Section[9], table = "table3",str_wrap_size=35,rankBig=FALSE,includeUnit=FALSE, round_off=0, compareRegion = "SSA",includePeriod = FALSE)




