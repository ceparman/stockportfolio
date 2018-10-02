
library(ggplot2)
library(plotrix)
library(rhandsontable)
library(data.table)
library(dplyr)
library(PerformanceAnalytics)
library(plotly)



source("comparion_stats.R")

# Define server logic required to draw a histogram
function(input, output) {
  
  returns <-readRDS("returns.rds")
  
  Names <- attr(returns,"dimnames")[[2]]

   spx  <- readRDS("spx_returns.rds")
  
 
  
  Models <- data.frame("Model_Name" = Names, 
            # Minimum = c(100000,120000,140000,159000,100000,120000),
            # Fee = c(.004,.0036,.007,.008,.01,.004),
             selected <-data.frame(selected =  rep(FALSE,length(Names))),
          #   leverage = data.frame(leverage =  rep(1,length(Names))),
             weights = data.frame( Weight = rep(.1,length(Names))),
             stringsAsFactors = FALSE)
  
 
  values = reactiveValues(stocks = Models)
    
  reset <- FALSE
  
   observeEvent(input$reset,{
     
     
    values[["stocks"]]$selected <- rep(FALSE,length( values[["stocks"]]$selected))
    values[["stocks"]]$Weight <- rep(.1,length( values[["stocks"]]$selected)) 
      
    DT = values[["stocks"]]
    
    
    output$stocks <- renderRHandsontable({
    rhandsontable(DT, rowHeaders = NULL) %>% 
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
      #   hot_col(col = "Minimum", format="$,0",readOnly = TRUE) %>%  
      #   hot_col(col = "Fee", format="%0.00",readOnly = TRUE) %>%  
      hot_col(col = "Weight", format="%0.00")
    })
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

    
  })   %>% debounce(100)
  
  output$stocksselected <- renderRHandsontable({


    ST <-values[["stocks"]]

    if (any(ST$selected == TRUE)) {

      ST <- ST[which(ST$selected == TRUE),-(c(2))]

      rhandsontable(ST, rowHeaders = NULL) %>%
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
        hot_col(col = "leverage", format="0.00",readOnly = TRUE) %>%  
        hot_col(col = "Weight", format="%0.00",readOnly = TRUE)


  } else{  #deal with an empty table
      #   print("non selected")
      NULL

  }
  })



  output$weight_sum<- renderText({
    
    ST <-values[["stocks"]]
    if (any(ST$selected == TRUE)) {
    
    w <- ST[which(ST$selected == TRUE),"Weight"]
    paste0("Total Weight ", sum(w)*100, " %")
  } else   paste0("Total Weight ", 0, " %")
  })
 
  
  
  output$pie <-  renderPlotly({
    
    
    DT <-   values[["stocks"]]
    if (any(DT$selected == TRUE)){
      
      DT <- DT[which(DT$selected == TRUE),]
      
      group = DT$Model_Name
      value = DT$Weight
      

      plot_ly(labels = ~group, values = ~value, type = 'pie',
              textinfo = 'label', hoverinfo = "none", showlegend = FALSE) %>%
        layout(title = 'Portfolio Allocations')
      
      
      
    } else plotly_empty()
    
    
    
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
   
    
    
   # names(pf$returns) <- "Portfolio"
    
    
    output$plot <- renderPlotly({
      
  
      
      spx_cum <-  cumprod(1+spx$returns)-1
      pf_cum  <-  cumprod(1+pf$returns)-1
      
      
      df<- data.frame( date = index(pf_cum), pf = pf_cum$portfolio.returns,spx_cum$portfolio.returns)
      
      names(df) <- c("date","pf","sp")
      
      plot_ly(df,x=~date) %>%
        add_trace(y = ~pf, name = 'Custom Porfolio',mode = 'lines',type="scatter")   %>%
        add_trace(y = ~sp, name = 'S&P 500',mode = 'lines',type="scatter") %>%
        layout(title = "Portfolio Returns",
               xaxis = list(title = ""),
               yaxis = list(title = "Return")) %>%
        layout(legend = list(orientation = 'h')) %>%
        layout(yaxis = list(tickformat = "%"))
      
      
      
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
     
     c[upper.tri(c)] <- NA
     
     dimnames(c)[[1]][1] <- "Custom Portfolio"
     dimnames(c)[[2]][1] <- "Custom Portfolio"
     
      rhandsontable(c,rowHeaderWidth = 150) %>%
      hot_col(col = 1:cols, format="%0.00",readOnly = TRUE) 
      
    
    })
  
    output$comparison <- renderRHandsontable({
     
    psc <- comp_stats(pf,spx)  
    #print(psc)
    colnames(psc) <- c("Custom Portfolio","S&P500")
    
    rhandsontable(psc,rowHeaderWidth = 200) %>%
      hot_col(col = 1:2, format="%0.00",readOnly = TRUE)
    
    
    
    })
    
    output$drawdowns <-  renderRHandsontable({
      # generate bins based on input$bins from ui.R
      tdd<-table.Drawdowns(pf$returns)
      tdd<-tdd[ ,c(4,5,6,1,2,3)]
      colnames(tdd) <- c("Depth","Length of Drawdown","Months to Recover",
                         "Start","Peak","End")
      rhandsontable(tdd,contextMenu = FALSE) %>% hot_cols(readOnly = TRUE) %>%
        hot_col(1,format="%0.000") %>%
        hot_col(2:3, format = "00")
      
      })
   
     } else showModal(modalDialog(
       title = "Portfolio Error",
       "Weights must add to 100%"
     ))
    
  }) ## end performance
  

  
  
  
}

# Run the application 


