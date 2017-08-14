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
figure_sparkline(Report_data,reportConfig,couName, "figure1",rankBig=FALSE)

## ---- figure_sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName, "figure2",rankBig=FALSE)

# ## ---- figure_sparkline3 ----
# figure_sparkline(Report_data,reportConfig,couName, "figure3",rankBig=FALSE)

## ---- figure_sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName, "figure4",rankBig=FALSE)

## ---- figure_sparkline5 ----
figure_sparkline(Report_data,reportConfig,couName, "figure5",rankBig=TRUE)

## ---- figure_sparkline6 ----
figure_sparkline(Report_data,reportConfig,couName, "figure6",rankBig=FALSE)

########## Economic Participation ##########

## ---- line1.1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"bar11",minTime="2012",neighbor="region",max_neighbors = 1)

## ---- line1.2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line12",minTime="2012",neighbor="region",max_neighbors = 1)

## ---- line1.3.1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line13.1",minTime="2012", neighbor="region",max_neighbors=0)

## ---- line1.3.2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line13.2",minTime="2012", neighbor="region",max_neighbors=0)

## ---- number1.4 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],c("line14"),str_wrap_size=36)

########## Wage Employment ###############

## ---- bar2.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],"bar21",paste_unit=FALSE,percentBar=FALSE)

## ---- line2.2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[2],"line22",minTime="2012", neighbor="region",max_neighbors=0)

########## Entrepreneurship ##########

## ---- number3.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[3],c("number31"),str_wrap_size=30,rankBig=TRUE)

## ---- number3.2 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[3],c("number32"),str_wrap_size=30,rankBig=TRUE)

########## Business Environment for Women ##########

## ---- bar4.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[4],"bar41",paste_unit=FALSE,percentBar=FALSE)

## ---- number4.2 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number42"),str_wrap_size=30,rankBig=TRUE)

## ---- number4.3 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number43"),str_wrap_size=30,rankBig=TRUE)

## ---- number4.4 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number44"),str_wrap_size=30,rankBig=TRUE)

########## Asset Ownership, Control, and Time Allocation ##########

## ---- bar5.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[5],"bar51",paste_unit=FALSE,percentBar=FALSE)

## ---- bar5.2.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[5],"bar52.1",paste_unit=FALSE,percentBar=FALSE)

## ---- number5.2.2 ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[5],"bar52.2",neighbor = "region",region=TRUE)


########## Social and Political Norms ##########

## ---- bar6.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[6],"bar61",paste_unit=FALSE,percentBar=FALSE)

## ---- number6.2 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[6],c("number62"),str_wrap_size=30,rankBig=TRUE)

