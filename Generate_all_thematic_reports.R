#!/usr/bin/env Rscript

packages <- c("jsonlite", "plyr","dplyr", "ggplot2", "gridExtra" ,"data.table",
              "tidyr", "xtable", "stringr", "fmsb", "treemap", "DT", "reshape2",
              "devtools", "sparkline", "knitr", "Cairo", "shiny", "shinythemes",
              "shinyBS", "shinyjs", "V8", "tidyverse", "scales")

install.packages(packages, repos = "https://cran.stat.upd.edu.ph/")

#preprocessing code
text_color <- "#818181"
region_longname <- read.csv("templates/region_longname.csv") 

# run Entrep, Tourism
for(input_reportID in c("Entrepreneurship", "Tourism")){
  source('global_utils.R')
  source('Report_Generator.R')
}

# run Gender
input_reportID <- "Gender"
source('global_utils.R')

# run Gender preprocessing
Report_data <- ReportDataList[[input_reportID]]
reportConfig <- ReportConfigList[[input_reportID]]
dataDesc <- dataDescList[[input_reportID]]
text_color <- "#818181"

source('Report_Generator.R')