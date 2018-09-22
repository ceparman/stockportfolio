library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
fixedPage(

  # Application title
  titlePanel("Portfolio Returns"),
  fixedRow(
  column(4,
         actionButton("reset","Reset Portfolio")
         
  )
  
  ),
  hr(),
  fixedRow(
  
    column(5,
  
  
           rHandsontableOutput("stocks",height = 200,width=350)
  ),
  
  column(4,offset = 7,
         
        # rHandsontableOutput("stocks_selected",height = 200,width=300)
         DT::dataTableOutput("stocks_selected")
  ) 
  
  
   ), #table row
  
  hr()  ,
  fixedRow(
 
  column(6,offset=3,
         plotOutput("pie"))
    
    
  ), #pierow
  
  hr(),
  fixedRow(
    
    column(width=12,
           plotOutput("plot",width = "100%"))
    
    
  ),
  
  fixedRow(
    
    column(10,
           
           
           DT::dataTableOutput("performance")
    )
  ) , #performance table
    
    
    fixedRow(
      
      column(10,
             
             
             DT::dataTableOutput("correlation")
      )
      ), #correlation table
      
     
      
       fixedRow(
        
        column(10,
               
               
               DT::dataTableOutput("comparison")
        ) #comparison table
        
  
  )
  
  
  
)
      #reset button
      #pdf button
    

    # Show a plot of the generated distribution

     #table for picking stocks
      #pie chart
      #graph of results
      # monthly performace table
      # Comaprision stats
      #corelation matrix
      #top draw downs




