#######################################################
# R functions to generate charts and tables in Entrepreneurship report
#
# asanchezrodelgo@ifc.org - Jun 2016
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


########## Policy ##########

## ---- bar_facewrap_chart_Policy ----
bar_facewrap_chart(couName,"Policy","table1")

## ---- bar_chart_Policy ----
bar_chart(couName,"Policy",c("combo1","combo2","combo3"))

## ---- number_chart_Policy ----
number_chart(couName,"Policy",c("combo1","combo2","combo3"),str_wrap_size=36)

## ---- combo_percent_Policy1 ----
#combo_percent(couName, "Policy","combo1")

## ---- combo_percent_Policy2 ----
#combo_percent(couName, "Policy","combo2")

## ---- combo_percent_Policy3 ----
#combo_percent(couName, "Policy","combo3")

## ---- sparklines_Policy ----
sparklines(couName,"Policy","table1")

## ---- line_chart_Policy ----
line_chart(couName,"Policy","figure5")

## ---- table_time_Policy ----
table_time(couName,"Policy","table1")

## ---- doing_business_table ----
doing_business_table(couName)


########## Human capital ##########

## ---- bar_facewrap_chart_Human ----
bar_facewrap_chart(couName, "Human capital","bar1")

## ---- pie_chart_double_Human ----
pie_chart_region(couName, "Human capital","figure6")

########## Finance ##########

## ---- figure_sparkline_Fin1 ----
figure_sparkline(couName,"figureFin1")

## ---- figure_sparkline_Fin2 ----
figure_sparkline(couName,"figureFin2")

## ---- figure_sparkline_Fin3 ----
figure_sparkline(couName,"figureFin3")

## ---- table_time_Finance ----
table_time(couName, "Finance","table1")

## ---- sparklines_Finance ----
sparklines(couName,"Finance","table1")

## ---- line_chart_Finance1 ----
line_chart(couName,"Finance","figure2")

## ---- line_chart_Finance2 ----
line_chart(couName,"Finance","line2")
#figure_sparkline(couName,"figure2")

########## Markets ##########

## ---- radar_chart_Markets ----
radar_chart(couName, "Markets","radar1")

## ---- bar_chart_Markets ----
bar_chart(couName, "Markets","radar1",paste_unit=TRUE)

## ---- table_region_Markets ----
table_region(couName, "Markets","table1")

# ## ---- pie_chart_double_Markets ----
#pie_chart_regular(couName, "Markets",c("combo1","combo2"))

## ---- number_chart_Markets ----
number_chart(couName,"Markets",c("table1","combo1","combo2"),str_wrap_size=36)

# ## ---- combo_percent_Markets1 ----
# combo_percent(couName, "Markets","combo1")
# #pie_chart_double(couName, "Markets","combo1")
# 
# ## ---- combo_percent_Markets2 ----
# combo_percent(couName, "Markets","combo2")
#pie_chart_double(couName, "Markets","combo2")

########## Culture ##########
## ---- table_time_avg_Culture ----
table_time_avg(couName,"Culture","table1")

## ---- sparklines_Culture ----
sparklines(couName,"Culture","table1")

########## Supports ##########
## ---- table_time_avg_Supports ----
table_time_avg(couName,"Supports","table1")

## ---- sparklines_Supports ----
sparklines(couName,"Supports","table1")

