#library(tidyverse)
#library(shiny)

ui <- fluidPage(
  titlePanel(paste("Inflation calculator ",year(today()), sep ="")),
  fluidRow(
    column(width=3,
           div(style = "width:100%;",
    selectInput("past_year",label = "From year:",
                choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                            "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                            "2004","2003","2002","2001","2000")),
    selectInput("to_year",label = "To year:",
                choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                            "2013","2012","2011","2010","2009","2008","2007","2006","2005",
                            "2004","2003","2002","2001","2000")),
    numericInput("convert_value",label = "Money to convert to present value:",
                 value=0),
    actionButton("submit_button","Submit")
    )
  ),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("present_value"))
         ),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("equivalent_value"))
         ),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("perc_lost"))
         )
)
)

server <- function(input, output) {
  year_inflation <- as.data.frame(cbind(c(2023,2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,
                            2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,
                            2000),
                          c(1,1.131,1.056,1.016,1.038,1.031,1.040,1.057,1.067,1.036,
                            1.019,1.024,1.037,1.031,1.020,1.076,1.056,1.044,1.048,
                            1.055,1.064,1.069,1.076,1.087))) %>%
    rename(year=V1,inflation=V2)
  
  text_1 <- reactiveValues(converted_value = "")
  
  
  observeEvent(input$submit_button,{
    input_values <- data.frame(matrix(nrow = 0,ncol = 3))
    colnames(input_values) <- c("value_to_convert","from_year","to_year")
    input_values[1,] <- c(input$convert_value,as.numeric(input$past_year),as.numeric(input$to_year))
    year_inflation_sub <- year_inflation %>%
      filter(year >= input_values$from_year & year < input_values$to_year) %>%
      summarise(prod(inflation))
    text_1$converted_value <- input$convert_value/year_inflation_sub[[1]]
    output$present_value <- renderText({text_1$converted_value})
    output$equivalent_value <- renderText({"This is a sample text"})
    output$perc_lost <- renderText({"This is a sample text"})
    
  }
  )
  
}

shinyApp(ui, server)



