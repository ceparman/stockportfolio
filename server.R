
library(ggplot2)
library(plotrix)
library(rhandsontable)
library(data.table)
library(dplyr)
library(PerformanceAnalytics)
library(tidyquant)

# Define server logic required to draw a histogram
function(input, output) {
  
  returns <-readRDS("returns.rds")
  
  Names <- attr(returns,"dimnames")[[2]]

   spx  <- readRDS("spx_returns.rds")
  
 
  
  Models <- data.frame("Model_Name" = Names, 
            # Minimum = c(100000,120000,140000,159000,100000,120000),
            # Fee = c(.004,.0036,.007,.008,.01,.004),
             selected <-data.frame(selected =  rep(FALSE,length(Names))),
             leverage = data.frame(leverage =  rep(1,length(Names))),
             weights = data.frame( Weight = rep(.1,length(Names))),
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
   #   hot_col(col = "Minimum", format="$,0",readOnly = TRUE) %>%  
   #   hot_col(col = "Fee", format="%0.00",readOnly = TRUE) %>%  
      hot_col(col = "Weight", format="%0.00")

    
  })   %>% debounce(1000)
  
  output$stocksselected <- renderRHandsontable({


    ST <-values[["stocks"]]

    if (any(ST$selected == TRUE)) {

      ST <- ST[which(ST$selected == TRUE),-(c(2))]

      rhandsontable(ST, rowHeaders = NULL) %>%
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
        hot_col(col = "leverage", format="0.00",readOnly = TRUE) %>%  
        hot_col(col = "Weight", format="%0.00",readOnly = TRUE)


  } else{  #deal with an empty table
         print("non selected")
      NULL

  }
  })



  output$weight_sum<- renderText({
    
    ST <-values[["stocks"]]
    if (any(ST$selected == TRUE)) {
    
    w <- ST[which(ST$selected == TRUE),"Weight"]
    paste0("Total Weight ", sum(w)*100, " %")
  } else NULL
  })
 
  
  
  output$pie <- renderPlot({
    
    
    DT <-   values[["stocks"]]
    if (any(DT$selected == TRUE)){
      
      DT <- DT[which(DT$selected == TRUE),]
      
      group = DT$Model_Name
      value = DT$Weight
      
      
      
      pie3D(value,labels=group,theta = pi/3,main ="Allocations")
    } else NULL
    
    
    
  })
  
  ########## performance results ##################
  
  
  observeEvent(input$run,{
    
    ## get names, remove columns or set weights to zero
    DT <- values[["stocks"]]
    weights <- (DT$Weight[which(DT$selected == TRUE)])
    
    if (sum(weights) == 1){ 
    
 
    
    ss <- (DT$Model_Name[which(DT$selected == TRUE)])
    
    lr<- returns[,ss]
    
    weights <- (DT$Weight[which(DT$selected == TRUE)])
   
    period <- input$period
    
    
    
    pf <- Return.portfolio(lr, weights = weights,rebalance_on = period ,verbose = TRUE )
   
  
     spx <-  Return.portfolio(spx,weights = 1,rebalance_on = period,verbose = TRUE )
   
    
    
    names(pf$returns) <- "Portfolio"
    
    output$plot <- renderPlot({
      
      
      chart.CumReturns(pf$returns)
      
      
    })
   
    output$performance <-  renderRHandsontable({
      cal<- table.CalendarReturns(pf$returns)
      colnames(cal)[13] <- "YTD"  
      
      yspx <- table.CalendarReturns(spx$returns)
      
      cal$"S&P 500" <- yspx$portfolio.returns
      cal <- cal/100
      rhandsontable(
        cal
      ) %>% hot_col(col = 1:14, format="%0.00",readOnly = TRUE)
    })
    
    output$correlation <- renderRHandsontable({

      
     c<- round(cor(cbind(pf$returns,returns)),3)
     cols <- ncol(c) 
     
      rhandsontable(c) %>%
      hot_col(col = 1:cols, format="%0.00",readOnly = TRUE)
    
    })
  
    output$comparison <- DT::renderDataTable(
      # generate bins based on input$bins from ui.R
      mtcars[1:10,1:8] 
      ,options = list(lengthChange = FALSE,dom = 't')
    )
    
    output$drawdowns <- DT::renderDataTable(
      # generate bins based on input$bins from ui.R
      table.Drawdowns(pf$returns)
    )
   
     } else showModal(modalDialog(
       title = "Portfolio Error",
       "Weights must add to 100%"
     ))
    
  }) ## end performance
  

  
  
  
}

# Run the application 


