library(tidyverse)
library(shiny)
#library(shinythemes)
options(scipen = 99)

backgroundImageCSS <- "height:100vh;background-size: cover;background-image: url('%s');"
addResourcePath(prefix = 'www', directoryPath = 'www')

fluidPage(
  tags$head(
    tags$style(HTML("
                #past_year 
                  {background-color: #d6ffa1;color:black}
                #to_year 
                  {background-color: #95d447;color:black}
                #convert_value 
                  {background-color: #5b9b07;color:black}
                #submit_button
                  {background-color: #f5c116;width: 50%;display:inline-block;margin:0px 20%;font-family:Helvetica;font-weight:bold;border-radius: 20px}
                #subtitle_1
                  {background-color: #95d447;font-weight:bold}
                #subtitle_2
                  {background-color: #95d447;font-weight:bold}
                #subtitle_3
                  {background-color: #95d447;font-weight:bold;height:40%}
                  
                "
    )
    )),
  titlePanel(div(paste("Inflation Calculator ",year(today()), sep =""),style="font-family:Helvetica;font-weight:bold;color:white;padding-bottom:20px")),
  fluidRow(
    column(width=3,
           div(selectInput(inputId = "past_year",label = "From year:",
                           choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                                       "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                                       "2004","2003","2002","2001","2000"),selectize = FALSE),style="color:white"),
           div(selectInput("to_year",label = "To year:",
                           choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                                       "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                                       "2004","2003","2002","2001","2000"),selectize = FALSE),style="color:white"),
           div(numericInput("convert_value",label = "Money to convert to present value:",
                            value=0),style="color:white"),
           actionButton("submit_button","SUBMIT")
    ),
    column(width=9,
           plotOutput("line_plot",hover=hoverOpts("hover_money")),
           uiOutput("hover_info"))
  ),
  fluidRow(
    column(width = 3),
    column(width = 3,
           textOutput("subtitle_1")
    ),
    column(width = 3,
           textOutput("subtitle_2")
    ),
    column(width = 3,
           textOutput("subtitle_3")
    )
  ),
  style="height:100vh;background-size: cover;background-image: url(www/backgr_calc.png);padding-right:5%"
  
  #sprintf(backgroundImageCSS, "www/backgr_calc.png")
  
)
