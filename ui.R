
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)

shinyUI(fixedPage(

  # Application title
  titlePanel("Greek Renaissance"),


  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 4,
      
                 
      fixedRow(      
               
               column(width=5, numericInput('faceInput', 'Face value ', 1000000, 1000000, Inf, 100000)),
               column(width=5, dateInput('expdateInput', value ='2015-12-15', 'Expiration date', weekstart=1))
                      ),
    
      fixedRow(
        
        column(width=5, sliderInput('defInput', "Capital protection, %", 90, 100, 100, 0.5)),
        column(width=5, numericInput('targetInput', 'Target', min=0, value=17))
        
        ),

      htmlOutput('paramsTable')

    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      htmlOutput('baseassetChart'),
      htmlOutput('profileChart'),
      htmlOutput("descr")
      
    ), fluid=F
  )
))
