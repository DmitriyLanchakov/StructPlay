
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


source(file = 'structplay.R', echo=F)

library(shiny)
library(googleVis)
library(scales)
library(dplyr)

shinyServer(function(input, output) {
  
  revals = reactiveValues()
  
  #
  # Structured prams reactive list calculation
  #  
  
  observe({
    
    input$faceInput
    endDate = input$expdateInput
    def = input$defInput/100
    target = input$targetInput
    assetPrice = as.numeric(lastpoint[[2]][drop=T])
    startDate =  as.Date(lastpoint[[1]][drop=T])
    
    #print(paste('AssetPrice=', assetPrice, ))
    
    params = StructParams(rdepo=0.12, 
                          def=def, 
                          assetPrice=assetPrice, 
                          startDate=startDate, 
                          endDate=endDate, 
                          sigma=0.6,
                          r=0.0, 
                          TypeFlag='c', 
                          X=assetPrice)
    
    params$startDate = startDate
    params$endDate = endDate
    params$s1 = assetPrice
    params$target = target
    params$payrate = (target/assetPrice - 1)*params$Partn
    params$payratey = params$payrate/as.numeric(endDate - startDate)*365
    
    params$Partn_perc = percent(params$Partn)
    params$payrate_perc = percent(params$payrate)
    params$payratey_perc = percent(params$payratey)
    
   revals$params = params
    
    
  })
  
  
  #
  # Render params table
  #
  output$paramsTable = renderGvis({
    
    result = as.data.frame((revals$params))
    result = result %>% dplyr::select(Days, startDate, endDate, s1, target, Partn_perc, payrate_perc, payratey_perc)
    result$startDate = as.character(result$startDate, '%d.%m.%Y')
    result$endDate   = as.character(result$endDate, '%d.%m.%Y')
    result = gather(result, 'xx', 'yy')
    result$zz = c('Investment days', 'Start date', 'End date', "Start price", "Target price", "Participation", "ROR, %", "ROR, annual %")
    result = result[, c('zz', 'yy')]
    names(result) = c("Parametre", "Value")
    
    gvisTable(result, options=list(width='300', sort='disable'))
    
  })
  
  
  #
  # Render base asset chart
  #
  output$baseassetChart = renderGvis({
    
    #browser()
    DrawAssetChart(raw.data, expdate =  input$expdateInput, ticker = 'GREK', target = input$targetInput)
    
  })
  
  #
  # Render structured product profile chart
  #
  output$profileChart = renderGvis({
    

     def = input$defInput/100
     ku = revals$params$Partn
     face = input$faceInput
     X1 = as.numeric(lastpoint[[2]][drop=T])
     target = input$targetInput
     
     xEnd = X1 + abs(target - X1) * 1.1
     xStart = X1 * (1- abs(xEnd/X1-1))
     xPoints = seq(from = xStart, to = xEnd, length.out = 100)
     
    
     prfl = StructProfile(xPoints, def=def, ku=ku, face=face, X1=X1 ) - face
    chart.data = data.frame(GREK = xPoints, Profit=prfl, Breakeven=0)
    
    gchart = gvisComboChart(data = chart.data, 
                            xvar=c('GREK'), 
                            yvar=c('Profit', 'Breakeven'), 
                            options = list(
                              chartArea = "{left:100,top:10}",
                              series = "[{color:'red', targetAxisIndex: 0, lineWidth: 2}, 
  {color: 'grey',targetAxisIndex: 0, lineWidth: 1, lineDashStyle: [4, 2]}]",
                              height=350, 
                              width=600, 
                              seriesType='line', 
                              legend= "{ position: 'in' }",   
                              hAxis = "{baselineColor: 'white', gridlines: {color: 'white'}}"
                              
                              
                            ) )
    gchart$html$footer = ''
    gchart$html$caption = ''
    
    gchart
    
  })
  
  
  
  
  
})

