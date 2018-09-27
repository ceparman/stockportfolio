library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plotly)
library(shinycssloaders)

# Define UI for application that draws a histogram
fixedPage(

  # Application title
  titlePanel("Portfolio Returns"),
  hr(),
  fixedRow(
 
  column(4,
        selectInput("period","Rebalancing Frequency",
                    choices = c("years","quarters", "months"))
         
  )
  
  
  ),
 
  fluidRow(
    
    column(6,
           verticalLayout(
             rHandsontableOutput("stocks",height = 200,width=400),
             textOutput("weight_sum"),
             tags$p(),
             actionButton("reset","Reset Portfolio")

             )
    
    ), 
    column(5,plotlyOutput("pie",width = "100%") ),

  
 
  

  fluidRow(
  column(width=2,offset=4,actionButton("run", "Calculate Porfolio Performance")
  )
  ),

 
hr(),
 
  h2("Portfolio Chart"),
  fluidRow(
    
    column(width=12,
           plotlyOutput("plot",width = "100%"))
    
    
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
    ) #Drawdowns table
    
  ),
  hr(),
  hr()
  
  ) 
  
)
      



