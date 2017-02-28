#######################################################
# R functions to generate charts and tables in Generic report
#
# asanchezrodelgo@ifc.org - Feb 2017
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- parameters ----
couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)
# Load data. This doesn't work, I'm afraid
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


########## Tourism Demand and Supply ##########

## ---- line1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line1")

## ---- bar_wrap1 ----
bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line2")

## ---- number1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"radar1",str_wrap_size=30)

## ---- number2 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"radar2",str_wrap_size=30)

########## Tourism Economic Indicators ##########

## ---- spark1 ----
figure_sparkline(Report_data,reportConfig,couName,"figure2")

## ---- spark2 ----
figure_sparkline(Report_data,reportConfig,couName,"figure7")

## ---- bar1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"line4",paste_unit=FALSE)

## ---- bar2 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"line3",paste_unit = FALSE)

## ---- number3 ----
number_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"line2",str_wrap_size=30)

## ---- number4 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"line5",str_wrap_size=30)

## ---- bar_wrap2 ----
bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"line2", vertical_bars = FALSE)
# 
# ## ---- bar_wrap3 ----
# bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"line4", vertical_bars = FALSE)
# 
# ## ---- bar_wrap4 ----
# bar_facewrap_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"line3",vertical_bars = FALSE, str_wrap_size = 40)
# 

########## Tourism competitiveness ##########

## ---- pie1 ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[3],"table1")

## ---- numberBig ----
numberBig(Report_data,reportConfig,couName, reportConfig$Section[3],"table1")

## ---- table_countries ----
table_countries(Report_data,reportConfig,couName,reportConfig$Section[3],"radar1")

## ---- table_time ----
table_time(Report_data,reportConfig,couName,reportConfig$Section[3],"radar1")

## ---- table_time_avg1 ----
table_time_avg(Report_data,reportConfig,couName,reportConfig$Section[3],"radar1")

## ---- sparklines1 ----
sparklines(Report_data,reportConfig,couName,reportConfig$Section[3],"radar1")

########## Access & Transport ##########

## ---- bar_wrap5 ----
bar_facewrap_chart(Report_data,reportConfig,couName,reportConfig$Section[4],"radar1")
