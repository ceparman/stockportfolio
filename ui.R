library(shiny)
library(shinydashboard)
library(rhandsontable)

# Define UI for application that draws a histogram
fixedPage(

  # Application title
  titlePanel("Portfolio Returns"),
  fixedRow(
  column(4,
         actionButton("reset","Reset Portfolio")
         
  ),
  column(4,
        selectInput("period","Rebalancing Frequency",
                    choices = c("years","quarters", "months"))
         
  )
  
  
  ),
  hr(),
  fluidRow(
  
    column(8,rHandsontableOutput("stocks",height = 200,width=400))
  
   
  
   ), #table row
  
  hr(),
  
  fluidRow(
    column(6,
           
           verticalLayout(
           rHandsontableOutput("stocksselected",width=250),
           textOutput("weight_sum")
            )) ,
    column(5, plotOutput("pie"),height = 200,height = 200)
    
    
    
  ), #pierow
  
  hr(),
  fluidRow(
  column(width=2,offset=4,actionButton("run", "Calculate Porfolio Performance")
  )
  ),
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
        
  
  ),
  fixedRow(
    
    column(10,
           
           
           DT::dataTableOutput("drawdowns")
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





