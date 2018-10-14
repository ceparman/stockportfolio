
library(ggplot2)
#library(plotrix)
library(rhandsontable)
library(data.table)
library(dplyr)
library(PerformanceAnalytics)
library(plotly)
library(tidyquant)



source("comparion_stats.R")


function(input, output) {
  
  returns <-readRDS("returns.rds")
  
  names <- unique(returns$symbol)
  
  spx  <- readRDS("spx_returns.rds")
  
  weights <- c(.2,.1,.1,.1,.1,.1,.1,.1,.1)
  period <- "months"

   spx  <- readRDS("spx_returns.rds")
  
 
  
  Models <- data.frame("Model_Name" = names, 
           
             selected <-data.frame(selected =  rep(TRUE,length(names))),
         
              weights = data.frame( Weight = c(.2,.1,.1,.1,.1,.1,.1,.1,.1)),
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
    

   
      rhandsontable(DT, rowHeaders = NULL,contextMenu = FALSE) %>% 
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
   #   hot_col(col = "Minimum", format="$,0",readOnly = TRUE) %>%  
   #   hot_col(col = "Fee", format="%0.00",readOnly = TRUE) %>%  
      hot_col(col = "Weight", format="%0.00")

    
  })   %>% debounce(100)
  
  output$stocksselected <- renderRHandsontable({


    ST <-values[["stocks"]]

    if (any(ST$selected == TRUE)) {

      ST <- ST[which(ST$selected == TRUE),-(c(2))]

      rhandsontable(ST, rowHeaders = NULL,contextMenu = FALSE) %>%
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
      
      m <- list(
        l = 10,
        r = 50,
        b = 10,
        t = 0,
        pad = 0
      )
      

      plot_ly(labels = ~group, values = ~value, type = 'pie',
              textinfo = 'label', hoverinfo = "none", showlegend = FALSE)%>%
       layout(autosize = F, width = 300, height = 300,margin = m )
      
      
      
    } else plotly_empty()
    
    
    
  })
  
  ########## performance results ##################
  
  
  observeEvent(input$run,{
    
   

    ## get names, remove columns or set weights to zero
    DT <- values[["stocks"]]
    weights <- (DT$Weight[which(DT$selected == TRUE)])
    
    if (sum(weights) == 1){ 
    
 
    
    ss <- (DT$Model_Name[which(DT$selected == TRUE)])
    
    #lr<- returns #[,ss]
    
    weights <- (DT$Weight[which(DT$selected == TRUE)])
   
    period <- input$period
    
    #print(lr)
    
    
    pf <- returns %>%filter(symbol %in% ss) %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = monthly.returns, 
                   weights     = weights,
                   rebalance_on = input$period,
                   col_rename  = "Custom Portfolio")
    
    
 #   pf <- Return.portfolio(lr, weights = weights,rebalance_on = period ,verbose = TRUE )
   
   
    
    pf_spx <- spx %>% mutate(symbol = "SPX") %>%
      tq_portfolio( assets_col  = symbol, 
                   returns_col = SPX, 
                   weights     = 1,
                   rebalance_on = input$period,
                   col_rename  = "SPX")
    
    
 #  spx <-  Return.portfolio(spx,weights = 1,rebalance_on = period,verbose = TRUE )
   
    
    
   # names(pf$returns) <- "Portfolio"
    
    
    output$plot <- renderPlotly({
      
  
      
      spx_cum <-  cumprod(1+pf_spx$SPX)-1
      pf_cum  <-  cumprod(1+pf$`Custom Portfolio`)-1
      
      
        df<- data.frame( date = spx$date, pf = pf_cum,spx_cum)
      
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
      cal<- tq_CalendarReturns(pf,digits=4)
      
    
      rhandsontable(cal,contextMenu = FALSE) %>% 
      
        hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (value < 0) {
              td.style.background = 'pink';
             }
           }") %>%
      hot_col(col = 1:12, format="%0.00",readOnly = TRUE) 
      
      
      
      
      
    })
    
    output$correlation <- renderRHandsontable({
    
      d<- as.data.frame(returns) %>% spread(symbol,monthly.returns)
    
   
     c<- round(cor(cbind(pf$`Custom Portfolio`,d[,-1])),3)
     cols <- ncol(c) 
     
     c[upper.tri(c)] <- NA
     
     dimnames(c)[[1]][1] <- "Custom Portfolio"
     dimnames(c)[[2]][1] <- "Custom Portfolio"
     
      rhandsontable(c,contextMenu = FALSE,rowHeaderWidth = 150) %>%
      hot_col(col = 1:cols, format="%0.00",readOnly = TRUE) %>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row == col) {
              td.style.background = 'lightgrey';
             } else if (col > row) {
              td.style.background = 'grey';
              td.style.color = 'grey';
             }
           }")
      
    
    })
  
    output$comparison <- renderRHandsontable({
     
    psc <- comp_stats(pf,spx)  
    #print(psc)
    colnames(psc) <- c("Custom Portfolio","S&P500")
    
    rhandsontable(psc,rowHeaderWidth = 200,contextMenu = FALSE) %>%
      hot_col(col = 1:2, format="%0.00",readOnly = TRUE)
    
    
    
    })
    
    output$drawdowns <-  renderRHandsontable({
      # generate bins based on input$bins from ui.R
      
      ts<- as.xts(pf$`Custom Portfolio`,order.by = pf$date)
      tdd<-table.Drawdowns(ts)
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


