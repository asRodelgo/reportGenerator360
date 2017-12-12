# run Entrep, Tourism
for(input_reportID in c("Entrepreneurship", "Tourism")){
  source('global_utils.R')
  source('Report_Generator.R')
}

# run Gender
input_reportID <- "Gender"
source('global_utils.R')
source('templates/Gender_charts.R') #run preprocessing code here
source('Report_Generator.R')

# run FCV
input_reportID <- "FCV"
source('global_utils.R')
source('templates/FCV_charts.R') #run preprocessing code here
source('Report_Generator_FCVonly.R')