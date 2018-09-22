
library(ggplot2)
library(plotrix)

# Define server logic required to draw a histogram
function(input, output) {
  
  output$stocks <- DT::renderDataTable(

    mtcars[1:10,1:3], options = list(lengthChange = FALSE,dom = 't')
    
  )
  
  output$stocks_selected <- DT::renderDataTable(
    # generate bins based on input$bins from ui.R
    faithful[1:10,] 
    ,options = list(lengthChange = FALSE,dom = 't')
  )
  
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


