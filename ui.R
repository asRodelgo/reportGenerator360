library(shiny)
library(shinythemes)
library(shinyBS)
# use javascript
library(shinyjs)
library(V8)

#object <- get(".shinystan_temp_object", envir = shinystan:::.sso_env)
source("global_utils.R", local = TRUE)

# tagList(
#   tags$noscript(style = "color: orange; font-size: 30px; text-align: center;", 
#                 "Please enable JavaScript to use Trade and Competitiveness Monitoring note and Operations."),
#   shinyjs::useShinyjs(),
  #includeCSS("css/tcdata360.css"),
  fluidPage(
    
    fluidRow(
            column(3,
                   selectInput('inCountry', "Select country:", choices=c("Select a country",countries$name), selectize=FALSE),
                   selectInput('inTopic', "Select topic:", choices=c("Select a topic","Entrepreneurship","Tourism"), selectize=FALSE),
                   downloadButton('downloadReport', 'Download PDF')
        )
      )
    )
  
  
  