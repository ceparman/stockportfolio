
library(ggplot2)
library(plotrix)
library(rhandsontable)
library(data.table)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output) {
  
  returns = readRDS("returns.rds")
  Names <- unique(returns$Name)[order(unique(returns$Name))]
  Models <- data.frame("Model_Name" = Names, 
             Minimum = c(100000,120000,140000,159000,300000,25000,120000,140000),
             Fee = c(.004,.0036,.007,.008,.01,.004,.0036,.007),
             selected <-data.frame(selected =  rep(FALSE,length(names))),
             leverage = data.frame(leverage =  rep(1,length(names))),
             weights = data.frame( Weight = rep(.1,length(names))),
             stringsAsFactors = FALSE)
  

  
  values = reactiveValues(stocks = Models)
    
  reset <- FALSE
  
   observeEvent(input$reset,{
     
     
    values[["stocks"]]$selected <- rep(FALSE,length( values[["stocks"]]$selected))
       
  
     
   reset <<- TRUE
  })
  

  
  output$stocks <- renderRHandsontable({

  
    
    DT = NULL
    if (!is.null(input$stocks) & !reset ) {
      DT = setDT(hot_to_r(input$stocks))
      
      values[["stocks"]] = DT
    } else if (!is.null(values[["stocks"]])) {
      DT = values[["stocks"]]
      reset <<- FALSE
    }
    
    if (!is.null(DT))
    

   
      rhandsontable(DT, rowHeaders = NULL) %>% 
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
      hot_col(col = "Minimum", format="$,0",readOnly = TRUE) %>%  
      hot_col(col = "Fee", format="%0.00",readOnly = TRUE)

    
  })   %>% debounce(1000)
  
  output$stocksselected <- renderRHandsontable({


    ST <-values[["stocks"]]

    if (any(ST$selected == TRUE)) {

      ST <- ST[which(ST$selected == TRUE),-(c(2,3,4))]

      rhandsontable(ST, rowHeaders = NULL) %>%
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
        hot_col(col = "leverage", format="0.00",readOnly = TRUE) %>%  
        hot_col(col = "Weight", format="%0.00",readOnly = TRUE)


  } else{  #deal with an empty table
         print("non selected")
      NULL

  }
  })



   
  output$performance <- DT::renderDataTable(
    # generate bins based on input$bins from ui.R
    mtcars[1:10,1:8] 
    ,options = list(lengthChange = FALSE,dom = 't')
  )
  
  output$correlation <- DT::renderDataTable(
    # generate bins based on input$bins from ui.R
    mtcars[1:10,1:8] 
    ,options = list(lengthChange = FALSE,dom = 't')
  )
  
  
  output$comparison <- DT::renderDataTable(
    # generate bins based on input$bins from ui.R
    mtcars[1:10,1:8] 
    ,options = list(lengthChange = FALSE,dom = 't')
  )
  
  
  
  output$pie <- renderPlot({
    
    
      DT <-   values[["stocks"]]
      if (any(DT$selected == TRUE)){
      
      DT <- DT[which(DT$selected == TRUE),]
 
      group = DT$Model_Name
      value = DT$Weight
  
    
  
    pie3D(value,labels=group,theta = pi/3)
      } else NULL
    
  
    
  })
  
  
  output$plot <- renderPlot({
    
    
    df<-data.frame(x=runif(100, min = 0, max = 1),
         y = 1:100
    )

    
     plot<- ggplot(df, aes(x=x, y=y)) + geom_line()
    
    
    plot
    
    
  })
  
  
  
}

# Run the application 


