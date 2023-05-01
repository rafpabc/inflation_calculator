library(tidyverse)
library(shiny)
#library(shinythemes)
options(scipen = 99)

function(input, output) {
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
    
    if(input$past_year >= input$to_year){
      output$warning_years <- renderText("'From year' must be smaller than 'To year'")
    } else {
    
    
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
        geom_line(color="#a18202",linewidth=1.6) +
        geom_point(shape=24,color="#b5ae43",fill="#a18202",size=3) +
        #geom_text_repel(aes(label=format(value_for_graph),hjust=0,vjust=0))+
        scale_x_continuous(limits = c(min(year_inflation_sub$year),max(year_inflation_sub$year)),
                           breaks = seq(min(year_inflation_sub$year),max(year_inflation_sub$year)))+
        xlab("Year") + ylab("Money value through each year") +
        #scale_y_continuous(labels = scales::comma) +
        theme(panel.background = element_rect(fill = "#d6ffa1", colour="#d6ffa1"),
              panel.grid.major = element_line(colour = "#d6ffa1"),
              panel.grid.minor = element_line(colour = "#d6ffa1"),
              panel.border = element_blank(),
              plot.background = element_rect(fill = "#d6ffa1"),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black"))
    )
    
    output$hover_info <- renderUI({
      hover <- input[["hover_money"]]
      if(is.null(hover)) return(NULL)
      punto <- nearPoints(year_inflation_sub, hover,threshold = 5, maxpoints = 1)
      if(nrow(punto) == 0) return(NULL)
      left_px <- hover$coords_css$x
      top_px  <- hover$coords_css$y
      style <- paste0(
        "position:absolute; z-index:100; pointer-events:none; ",
        "width:12%;height:20%;font-size:0.8em;",
        #"background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px, 
        "px; top:", top_px, "px;"
      )
      tooltip <- paste0(
        "<b> Money: </b>",     format(punto[["value_for_graph"]],digits=1,nsmall=0,scientific=FALSE,big.mark=","),     "<br/>",
        "<b> Year: </b>", punto[["year"]], "<br/>"
      )
      wellPanel(
        style = style, p(HTML(tooltip))
      )
    })
    
    text_1$converted_value <- format(input$convert_value/prod_inflation[[1]],digits=1,nsmall=0,scientific=FALSE,big.mark=",")
    text_2$converted_value <- format(input$convert_value*prod_inflation[[1]],digits=1,nsmall=0,scientific=FALSE,big.mark=",")
    text_3$converted_value <- format(((input$convert_value-(input$convert_value/prod_inflation[[1]]))/input$convert_value)*100,digits=1,nsmall=2,scientific=FALSE,big.mark=",")
    
    output$subtitle_1 <- renderText(paste("$",format(value_reactive$subtitle1,digits=1,nsmall=0,big.mark=",")," in ",future_reactive$subtitle3," would had equal $",text_1$converted_value," in ",past_reactive$subtitle2,sep=""))
    output$subtitle_2 <- renderText(paste("$",format(value_reactive$subtitle1,digits=1,nsmall=0,big.mark=",")," in ",past_reactive$subtitle2," would equal $",text_2$converted_value," in ", future_reactive$subtitle3,sep=""))
    output$subtitle_3 <- renderText(paste("Money lost ", text_3$converted_value,"% of its value from ",past_reactive$subtitle2," to ",future_reactive$subtitle3,sep=""))
    
    }
  }
  )
  
}