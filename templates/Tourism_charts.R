#######################################################
# R functions to generate charts and tables in Generic report
#
# asanchezrodelgo@ifc.org - Feb 2017
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- parameters ----
couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)

########## Header ##########

## ---- figure_sparkline1 ----
figure_sparkline(couName,"figure1")

## ---- figure_sparkline2 ----
figure_sparkline(couName, "figure2")

## ---- figure_sparkline3 ----
figure_sparkline(couName, "figure3")

## ---- figure_sparkline4 ----
figure_sparkline(couName, "figure4")

## ---- figure_sparkline5 ----
figure_sparkline(couName, "figure5")

## ---- figure_sparkline6 ----
figure_sparkline(couName, "figure6")


########## Tourism Demand and Supply ##########

## ---- line1 ----
line_chart(couName,reportConfig$Section[1],"line1")

## ---- bar_wrap1 ----
bar_facewrap_chart(couName,reportConfig$Section[1],"line2")

## ---- number1 ----
number_chart(couName,reportConfig$Section[1],"radar1",str_wrap_size=30)

## ---- number2 ----
number_chart(couName,reportConfig$Section[1],"radar2",str_wrap_size=30)

########## Tourism Economic Indicators ##########

## ---- spark1 ----
figure_sparkline(couName,"figure2")

## ---- spark2 ----
figure_sparkline(couName,"figure7")

## ---- bar_wrap2 ----
bar_facewrap_chart(couName,reportConfig$Section[2],"line2", vertical_bars = FALSE)

## ---- bar_wrap3 ----
bar_facewrap_chart(couName,reportConfig$Section[2],"line4", vertical_bars = FALSE)

## ---- bar1 ----
bar_chart(couName, reportConfig$Section[2],"line3",paste_unit=TRUE)

## ---- number3 ----
number_chart(couName,reportConfig$Section[2],"line5",str_wrap_size=36)

########## Tourism competitiveness ##########

## ---- pie1 ----
pie_chart_region(couName, reportConfig$Section[3],"table1")

## ---- table_time_avg1 ----
table_time_avg(couName,reportConfig$Section[3],"radar1")

## ---- sparklines1 ----
sparklines(couName,reportConfig$Section[3],"radar1")

########## Access & Transport ##########

## ---- bar_wrap3 ----
bar_facewrap_chart(couName,reportConfig$Section[4],"radar1")
