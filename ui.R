
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)

shinyUI(fixedPage(

  # Application title
  titlePanel("Греческий ренессанс: волатильность и отвага!"),


  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 4,
      
                 h3("Входные параметры"),
                 p("От срока инвестирования и риска зависит коэффициент участия, а от целевого уровня базового актива - ожидаемая доходность.
                   Изменить параметры и посмотрите, что получится."),
                 
      fixedRow(      
               
               column(width=5, numericInput('faceInput', 'Объём сделки, руб.', 1000000, 1000000, Inf, 100000)),
               column(width=5, dateInput('expdateInput', value ='2015-12-15', 'Срок инвестирования', weekstart=1))
                      ),
    
      fixedRow(
        
        column(width=5, sliderInput('defInput', "Защита капитала, %", 90, 100, 100, 0.5)),
        column(width=5, numericInput('targetInput', 'Целевой уровень', min=0, value=17))
        
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
