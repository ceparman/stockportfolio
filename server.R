
library(ggplot2)
library(plotrix)
library(rhandsontable)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output) {
  
  returns = readRDS("returns.rds")
  Names <- unique(returns$Name)[order(unique(returns$Name))]
  Models <- data.frame("Model_Name" = Names, 
             Minimum = c(100000,120000,140000,159000,300000,25000,120000,140000),
             Fee = c(.004,.0036,.007,.008,.01,.004,.0036,.007),
            stringsAsFactors = FALSE)
  
  weights <- data.table( Weight = rep(0,nrow(Models)))
  
  selected <-data.frame(selected =  rep(FALSE,nrow(Models)))
  
  leverage <-data.frame(leverage =  rep(1,nrow(Models)))
  
  models = reactiveValues(stocks = cbind(Models,selected),
    
                          port = cbind(leverage,weights))
  
  
  output$stocks <- renderRHandsontable({

  #  rhandsontable(  Models  #, options = list(lengthChange = FALSE,dom = 't')
  #  )
    
    ST = NULL
    if (!is.null(input$stocks)) {
      ST = setDT(hot_to_r(input$stocks))
     
      models[["stocks"]] = ST
    } else if (!is.null(models[["stocks"]])) {
      ST = models[["stocks"]]
    }
    
    if (!is.null(ST))
   
      rhandsontable(ST) %>% 
      hot_col(col = "Model_Name", readOnly = TRUE) %>% 
      hot_col(col = "Minimum", format="$,0",readOnly = TRUE) %>%  
      hot_col(col = "Fee", format="%0.00",readOnly = TRUE)

    
  }) 
  
  output$stocks_selected <- DT::renderDataTable({
    # Get names and add 
 #  MT <- models[["post"]]
   selected <- models[["stocks"]][,c("Model_Name","selected")]
   MT <- cbind(selected,models[["port"]])
   MT <- MT %>% filter(selected == TRUE)
   
   DT::datatable(MT)
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
    
    
 
      group = c("Stock 1", "Stock 2", "Stock3")
      value = c(25, 25, 50)
  
    
   # bp<- ggplot(df, aes(x="", y=value, fill=group))+
    #  geom_bar(width = 1, stat = "identity")
     # pie <- bp + coord_polar("y", start=0)
    pie3D(value,labels=group,theta = pi/3)
    
    
  
    
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


