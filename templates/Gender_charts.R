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

text_color <- "#818181"
## ---- figure_sparkline1 ----
# figure_sparkline(Report_data,reportConfig,couName, "figure1",rankBig=FALSE)
figure_number_rank_only(Report_data,reportConfig,couName, "number63", str_wrap_size=30, useRank=FALSE)

## ---- figure_sparkline2 ----
figure_number_rank_only(Report_data,reportConfig,couName, "figure2", str_wrap_size=30,useRank=TRUE)

## ---- figure_sparkline3 ----
figure_number_rank_only(Report_data,reportConfig,couName, "figure3", str_wrap_size=30,useRank=TRUE)

## ---- figure_sparkline4 ----
figure_number_rank_only(Report_data,reportConfig,couName, "figure4",str_wrap_size=30, useRank = TRUE)

########## Economic Participation ##########

## ---- line1.1 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"bar11",minTime="2012",neighbor="region",max_neighbors = 1, world=TRUE)

## ---- line1.2 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"line12",minTime="2012",neighbor="region",max_neighbors = 1, world=TRUE)

## ---- line1.3.1 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"line13.1",minTime="2012", neighbor="region",max_neighbors=0, gender=TRUE)

## ---- line1.3.2 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[1],"line13.2",minTime="2009", neighbor="region",max_neighbors=0, gender=TRUE)

## ---- number1.4 ----
# number_chart(Report_data,reportConfig,couName,reportConfig$Section[1],c("line14"),str_wrap_size=36)
figure_number_rank_only(Report_data,reportConfig,couName, "line14", str_wrap_size=5,useRank=TRUE)

########## Wage Employment ###############

## ---- bar2.1 ----
bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[2],"bar21",paste_unit=FALSE,kind="stackedbar")

## ---- line2.2 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[2],"line22",minTime="2009", neighbor="region",max_neighbors=1, world=TRUE)

########## Entrepreneurship ##########

## ---- figure_sparkline_Ent1 ----
figure_number_rank_only(Report_data,reportConfig,couName, "number31", str_wrap_size=30,useRank = FALSE, paste_unit=FALSE)
# number_chart(Report_data,reportConfig,couName,reportConfig$Section[3],c("number31"),str_wrap_size=30,rankBig=TRUE)

## ---- figure_sparkline_Ent2 ----
figure_number_rank_only(Report_data,reportConfig,couName, "number32", str_wrap_size=30,useRank = FALSE, paste_unit=FALSE)
# number_chart(Report_data,reportConfig,couName,reportConfig$Section[3],c("number32"),str_wrap_size=30,rankBig=TRUE)

## ---- figure_sparkline_Ent3 ----
figure_number_rank_only(Report_data,reportConfig,couName, "number33", str_wrap_size=30,useRank = FALSE, paste_unit=FALSE)

########## Business Environment for Women ##########

## ---- bar4.1 ----
bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[4],"bar41",paste_unit=FALSE, kind="normal")

## ---- number4.2 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number42"),str_wrap_size=40)

## ---- number4.3 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number43"),str_wrap_size=40)

## ---- number4.4 ----
number_chart_yesno(Report_data,reportConfig,couName,reportConfig$Section[4],c("number44"),str_wrap_size=40)

########## Asset Ownership, Control, and Time Allocation ##########

## ---- bar5.1 ----
bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[5],"bar51",paste_unit=FALSE,kind="normal", plot_spacing=0.3)

## ---- bar5.2.1 ----
bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[5],"bar52.1",paste_unit=FALSE,kind="single_indicator")
# bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[5],"bar52.1",paste_unit=FALSE, kind="pie")

## ---- number5.2.2 ----
pie_chart_region(Report_data,reportConfig,couName, reportConfig$Section[5],"bar52.2",neighbor = "region",region=TRUE)


########## Social and Political Norms ##########

## ---- bar6.1 ----
bar_chart_gender(Report_data,reportConfig,couName, reportConfig$Section[6],"bar61",paste_unit=FALSE, plot_spacing=0.3)

## ---- line6.2 ----
line_chart_avg(Report_data,reportConfig,couName,reportConfig$Section[6],"line62",minTime="2012",neighbor="region",max_neighbors = 1, world=TRUE)

## ---- number6.3 ----
figure_number_rank_only(Report_data,reportConfig,couName, "number63", str_wrap_size=15, useRank=FALSE)


