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
########## Header ##########

## ---- figure_sparkline1 ----
figure_sparkline(Report_data,reportConfig,couName,"figure1")

## ---- figure_sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName, "figure2")

## ---- figure_sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName, "figure3")

## ---- figure_sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName, "figure4")

## ---- figure_sparkline5 ----
figure_sparkline(Report_data,reportConfig,couName, "figure5")

## ---- figure_sparkline6 ----
figure_sparkline(Report_data,reportConfig,couName, "figure6")


########## Policy ##########

## ---- bar_facewrap_chart_Policy ----
bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"table1")

## ---- bar_chart_Policy ----
bar_chart(Report_data,reportConfig,couName,reportConfig$Section[1],c("combo1","combo2","combo3"))

## ---- number_chart_Policy ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],c("combo1","combo2","combo3"),str_wrap_size=36)

## ---- combo_percent_Policy1 ----
#combo_percent(Report_data,reportConfig,couName, "Policy","combo1")

## ---- combo_percent_Policy2 ----
#combo_percent(Report_data,reportConfig,couName, "Policy","combo2")

## ---- combo_percent_Policy3 ----
#combo_percent(Report_data,reportConfig,couName, "Policy","combo3")

## ---- sparklines_Policy ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[1],"table1")

## ---- line_chart_Policy ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"figure5")

## ---- table_time_Policy ----
table_time(Report_data,reportConfig,couName,reportConfig$Section[1],"table1")

## ---- doing_business_table ----
doing_business_table(Report_data,reportConfig,couName)


########## Human capital ##########

## ---- bar_facewrap_chart_Human ----
bar_facewrap_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"bar1")

## ---- pie_chart_double_Human ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[2],"figure6")

########## Finance ##########

## ---- figure_sparkline_Fin1 ----
figure_sparkline(Report_data,reportConfig,couName,"figureFin1")

## ---- figure_sparkline_Fin2 ----
figure_sparkline(Report_data,reportConfig,couName,"figureFin2")

## ---- figure_sparkline_Fin3 ----
figure_sparkline(Report_data,reportConfig,couName,"figureFin3")

## ---- table_time_Finance ----
table_time(Report_data,reportConfig,couName, reportConfig$Section[3],"table1")

## ---- sparklines_Finance ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[3],"table1")

## ---- line_chart_Finance1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[3],"figure2")

## ---- line_chart_Finance2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[3],"line2")
#figure_sparkline(Report_data,reportConfig,couName,"figure2")

########## Markets ##########

## ---- radar_chart_Markets ----
radar_chart(Report_data,reportConfig,couName, reportConfig$Section[4],"radar1")

## ---- bar_chart_Markets ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[4],"radar1",paste_unit=FALSE,percentBar=TRUE)

## ---- table_region_Markets ----
table_region(Report_data,reportConfig,couName, reportConfig$Section[4],"table1")

# ## ---- pie_chart_double_Markets ----
#pie_chart_regular(Report_data,reportConfig,couName, "Markets",c("combo1","combo2"))

## ---- number_chart_Markets ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[4],c("table1","combo1","combo2"),str_wrap_size=36)

# ## ---- combo_percent_Markets1 ----
# combo_percent(Report_data,reportConfig,couName, "Markets","combo1")
# #pie_chart_double(Report_data,reportConfig,couName, "Markets","combo1")
# 
# ## ---- combo_percent_Markets2 ----
# combo_percent(Report_data,reportConfig,couName, "Markets","combo2")
#pie_chart_double(Report_data,reportConfig,couName, "Markets","combo2")

########## Culture ##########
## ---- table_time_avg_Culture ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[5],"table1")

## ---- sparklines_Culture ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[5],"table1")

########## Supports ##########
## ---- table_time_avg_Supports ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[6],"table1")

## ---- sparklines_Supports ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[6],"table1")

