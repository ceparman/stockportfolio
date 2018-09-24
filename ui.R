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
  h2("Porfolio Chart"),
  fluidRow(
    
    column(width=12,
           plotOutput("plot",width = "100%"))
    
    
  ),
  hr(),
  h2("Porfolio Performance"),
  fluidRow(
    
    column(11,

           rHandsontableOutput("performance")
    )
  ) , #performance table
    
  hr(),
  h2("Performance Statistics Comparison"), 
  
  fixedRow(
    
    column(10,
           
           
           rHandsontableOutput("comparison")
    ) #comparison table
    
    
  ),
  
  hr(),
  h2("Correlation Matrix"), 
    fluidRow(
      
      column(10,
             
             
             rHandsontableOutput("correlation")
      )
      ), #correlation table
      
     
  hr(),
  h2("Top Drawdowns"),      
      
  fixedRow(
    
    column(10,
           
           
           rHandsontableOutput("drawdowns")
    ),#Drawdowns table
    
    hr(),
    hr()
    
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





