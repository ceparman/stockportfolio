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
  fluidRow(
 
  column(5,
         
         
         verticalLayout(
           
           wellPanel(
             selectInput("period","Rebalancing Frequency",
                         choices = c("years","quarters", "months")),
             
              rHandsontableOutput("stocks"),#,height = 200,width=400),
             textOutput("weight_sum"),
             actionButton("reset","Reset Portfolio")
           ),  
               
           actionButton("run", "Calculate Porfolio Performance"),
           
           plotlyOutput("pie",width = "100%")
           

        )
  ) ,
 

  column(width=7,
           
           plotlyOutput("plot",width = "100%"),
           
           hr(),
          
          h2("Porfolio Performance"),
       
          rHandsontableOutput("performance"),

          hr(),
  
         h2("Performance Statistics Comparison"), 
           
          rHandsontableOutput("comparison"),

         hr(),
         h2("Correlation Matrix"), 

             rHandsontableOutput("correlation"),

     
         hr(),
         h2("Top Drawdowns"),      

           
           rHandsontableOutput("drawdowns")

  ) ,
  hr(),
  hr()
  
  ) 
  
)
      



