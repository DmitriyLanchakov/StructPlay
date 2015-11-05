
library(shiny)
require(markdown)

shinyUI(fluidPage(

  # Application title

  # Layout name
     h1("RTS Index capital protected investment"),
      
     p("This app demonstrates capital protected structured products performance. ", 
code('Money market rate'), 
"and", code('Implied volatility') ,"reflect market condition, other options - investor's trade exposure 
       and risk appetite."), 
     p("Expected return on equity (ROE in the Summary table) is reached, when the base asset market price equals Target 
       at the expration date. Change the Target to see the expected payoff. The relation between base asset price and 
       structured pcoduct payoff is on the corresponding chart."),
     p("The projection from the current price on Base asset chart visualises 
        Investment period duration. The very right point on the time scale is the expiration date."),
     
      fluidRow(      
        
        column(width=4, 
               h3('Parameters'),
               numericInput('marketrate', 'Money market rate, % ', 10, 0.25, Inf, 0.25),
               sliderInput('iv', 'Implied volatility, %', 5, 100, 20, 1),
               selectInput('longshort', 'Trade', c('Long'='c', 'Short'='p')),
               sliderInput('investperiod', 'Investment period, months', 1, 36, 3, 1),
               numericInput('targetInput', 'Target price', min=0, value=1000),
               sliderInput('defInput', "Capital protection, %", 90, 100, 100, 0.5),
               actionButton('refresh', 'Refresh')
               ),
               

        column(width=5, 
               h3('Summary'),
               htmlOutput('paramsTable'),
               br(),
               h3('Base asset'),
               htmlOutput('baseassetChart'),
               br(),
               h3('Payoff'),
               htmlOutput('profileChart'),
               htmlOutput("descr")
               )
  


)))
