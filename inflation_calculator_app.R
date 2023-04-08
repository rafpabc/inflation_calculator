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
    )),
    column(width=9,
           plotOutput("line_plot",width="100%"))
  ),
  fluidRow(
    column(width = 3),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("subtitle_1")),
         div(style = "width:100%;",
             textOutput("present_value"))
         ),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("subtitle_2")),
         div(style = "width:100%;",
             textOutput("equivalent_value"))
         ),
  column(width = 3,
         div(style = "width:100%;",
             textOutput("subtitle_3")),
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
  
  value_reactive <- reactiveValues(subtitle1 = "")
  past_reactive <- reactiveValues(subtitle2 = "")
  future_reactive <- reactiveValues(subtitle3 = "")
  text_1 <- reactiveValues(converted_value = "")
  text_2 <- reactiveValues(converted_value = "")
  text_3 <- reactiveValues(converted_value = "")
  prod_inf <- c()
  observeEvent(input$submit_button,{
    input_values <- data.frame(matrix(nrow = 0,ncol = 3))
    colnames(input_values) <- c("value_to_convert","from_year","to_year")
    input_values[1,] <- c(input$convert_value,as.numeric(input$past_year),as.numeric(input$to_year))
    value_reactive$subtitle1 <- input$convert_value
    past_reactive$subtitle2 <- input$past_year
    future_reactive$subtitle3 <- input$to_year
    
    year_inflation_sub <- year_inflation %>%
      filter(year >= input_values$from_year & year < input_values$to_year) %>%
      mutate(year = year+1)
    
    for (i in 1:nrow(year_inflation_sub)) {
      prod_inf <- c(prod_inf,prod(year_inflation_sub$inflation[i:nrow(year_inflation_sub)]))
    }
    
    year_inflation_sub <- year_inflation_sub %>% select(-inflation) %>% bind_cols(prod_inf=prod_inf)
    year_inflation_sub <- rbind(year_inflation_sub,c(year_inflation_sub[nrow(year_inflation_sub),"year"]-1,1))
    year_inflation_sub <- year_inflation_sub %>% 
      mutate(value_for_graph = value_reactive$subtitle1 / prod_inf)
    
    
    prod_inflation <- year_inflation %>%
      filter(year >= input_values$from_year & year < input_values$to_year) %>%
      summarise(prod(inflation))
    
    output$line_plot <- renderPlot(
      ggplot(year_inflation_sub,aes(x=year,y=value_for_graph))+
        geom_line()
                                   )
    
    text_1$converted_value <- format(input$convert_value/prod_inflation[[1]],digits=1,nsmall=0,scientific=FALSE,big.mark=",")
    text_2$converted_value <- format(input$convert_value*prod_inflation[[1]],digits=1,nsmall=0,scientific=FALSE,big.mark=",")
    text_3$converted_value <- format(((input$convert_value-(input$convert_value/prod_inflation[[1]]))/input$convert_value)*100,digits=1,nsmall=2,scientific=FALSE,big.mark=",")

    output$subtitle_1 <- renderText(paste("$",format(value_reactive$subtitle1,digits=1,nsmall=0,big.mark=",")," in ",future_reactive$subtitle3," would equal this in ",past_reactive$subtitle2,sep=""))
    output$present_value <- renderText(paste("$",{text_1$converted_value},sep=""))
    output$subtitle_2 <- renderText(paste("$",format(value_reactive$subtitle1,digits=1,nsmall=0,big.mark=",")," in ",past_reactive$subtitle2," would equal this in ",future_reactive$subtitle3,sep=""))
    output$equivalent_value <- renderText(paste("$",{text_2$converted_value},sep=""))
    output$subtitle_3 <- renderText(paste("The amount of lost value from ",past_reactive$subtitle2," to ",future_reactive$subtitle3," was",sep=""))
    output$perc_lost <- renderText(paste({text_3$converted_value},"%",sep=""))
    
  }
  )
  
}

shinyApp(ui, server)



