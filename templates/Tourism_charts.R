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
figure_sparkline(Report_data,reportConfig,couName,"figure1",rankBig=TRUE)

## ---- figure_sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName, "figure2",rankBig=TRUE)

## ---- figure_sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName, "figure3",rankBig=TRUE)

## ---- figure_sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName, "figure4",rankBig=TRUE)

## ---- figure_sparkline5 ----
figure_sparkline(Report_data,reportConfig,couName, "figure5",rankBig=TRUE)

## ---- figure_sparkline6 ----
figure_sparkline(Report_data,reportConfig,couName, "figure6",rankBig=TRUE)

########## Tourism Demand and Supply ##########

## ---- line1.1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line1")

## ---- line1.2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line2")

## ---- number1.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"radar1",str_wrap_size=30,rankBig=TRUE)

## ---- number1.2 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"radar2",str_wrap_size=30,rankBig=TRUE)

########## Tourism Economic Indicators ##########

## ---- pie2.1 ----
pie_chart_regular(Report_data,reportConfig,couName, reportConfig$Section[2],"line4")

## ---- numberBig2.1 ----
numberBig(Report_data,reportConfig,couName, reportConfig$Section[2],"number2",rankBig = TRUE)

## ---- bar2.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],c("bar1","bar2","bar3"),paste_unit = FALSE, percentBar = TRUE)

## ---- number2.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"line5",str_wrap_size=30,rankBig=TRUE)

## ---- bar2.2 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"line3",paste_unit = FALSE)

## ---- text2.1 ----
text_box(title=c("Receipt for travel items:","Receipts for passenger transport items:"),
         body=c("These expenditures by international inbound visitors, or in their behalf,  to purchase goods and services in the reporting economy",
                "These are expenditures by international inbound visitors for all services provided in the international transportation by resident carriers and passenger services performed within an economy by nonresident carriers."),
         str_wrap_size=50)

########## Tourism competitiveness ##########

## ---- radar_chart ----
radar_chart(Report_data,reportConfig,couName,reportConfig$Section[3],"radar1")

########## Access & Transport ##########

## ---- number4.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[4],"radar1",str_wrap_size=30,rankBig=TRUE)
