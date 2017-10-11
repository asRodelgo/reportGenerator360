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

text_color <- "#818181"
region_longname <- read.csv("templates/region_longname.csv") 
########## Header ##########

## ---- figure_sparkline1 ----
figure_sparkline(Report_data,reportConfig,couName,"figure1",rankBig=FALSE)

## ---- figure_sparkline2 ----
figure_sparkline(Report_data,reportConfig,couName, "figure2",rankBig=TRUE)

## ---- figure_sparkline3 ----
figure_sparkline(Report_data,reportConfig,couName, "figure3",rankBig=FALSE)

## ---- figure_sparkline4 ----
figure_sparkline(Report_data,reportConfig,couName, "figure4",rankBig=FALSE)

########## Tourism Demand and Expenditure ##########

## ---- line1.1 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line1",minTime="2005",neighbor="region",max_neighbors = 0,
           show_last_year=TRUE, show_data_labels = c(2005, 2010, 2015), plot_spacing=0.4)

## ---- line1.2 ----
line_chart(Report_data,reportConfig,couName,reportConfig$Section[1],"line2",minTime="2005",neighbor="region",max_neighbors = 0,
           show_last_year=TRUE, show_data_labels = c(2005, 2010, 2017), plot_spacing=0.4)

########## Tourism Economic Indicators ##########

## ---- bar2.1 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[2],c("bar1","bar2","bar3"),paste_unit = FALSE, percentBar = TRUE)

########## Competitiveness ##########

## ---- radar3.1 ----
radar_chart(Report_data,reportConfig,couName,reportConfig$Section[1:7],c("radar1","radar2","radar3","radar4","radar5"),neighbor = "region",
            spell_out_region=TRUE)

########## Reasons to Travel ###############

## ---- number1.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[4],c("radar6","radar10", "radar1"),str_wrap_size=35,rankBig=TRUE)

########## Travel Facilitators ###############

## ---- number1.2 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[5],c("radar7","radar2"),str_wrap_size=35,rankBig=TRUE)

########## Conditions of the Country ###############

## ---- number4.2 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[6],c("radar9","radar3"),str_wrap_size=40,rankBig=TRUE)

## ---- number4.3 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[6],"radar4",str_wrap_size=28,rankBig=TRUE)

## ---- number4.4 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[6],"radar8",str_wrap_size=28,rankBig=FALSE)


########## Other Indicators ###############

## ---- bar2.2 ----
bar_chart(Report_data,reportConfig,couName, reportConfig$Section[7],"line3",paste_unit = FALSE)

## ---- pie2.1 ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[7],"pie2",neighbor = "region", region=TRUE, spell_out_region = TRUE)

## ---- pie2.2 ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[7],"number2",neighbor = "region",region=TRUE, spell_out_region = TRUE)

## ---- number2.1 ----
number_chart(Report_data,reportConfig,couName,reportConfig$Section[1:7],"line5",str_wrap_size=30,rankBig=FALSE,includeUnit = TRUE)
