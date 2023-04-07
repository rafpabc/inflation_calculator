#library(tidyverse)
#library(shiny)

ui <- fluidPage(
  titlePanel(paste("Inflation calculator ",year(today()), sep ="")),
  fluidRow(
    column(width=4,
           div(style = "width:100%;",
    selectInput("past_year",label = "From year:",
                choices = c("2022","2021","2020","2019","2018","2017","2016","2015","2014",
                            "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                            "2004","2003","2002","2001","2000")),
    selectInput("to_year",label = "To year:",
                choices = c("2022","2021","2020","2019","2018","2017","2016","2015","2014",
                            "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                            "2004","2003","2002","2001","2000")),
    numericInput("present_value",label = "Money to convert to present value:",
                 value=0),
    actionButton("submit_button","Submit")
    )
  ),
  column(width = 8,
         div(style = "width:100%;",
             textOutput("lost_value")
             
         )
         )
)
)

server <- function(input, output) {
  observeEvent(input$submit_button,{
    output$lost_value <- renderText({"This is a sample text"})
    output$lost_value <- renderText({"This is a sample text"})
    
  }
  )
  
}

shinyApp(ui, server)
