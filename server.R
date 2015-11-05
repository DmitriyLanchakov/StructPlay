

source(file = 'structplay.R', echo=F)

library(shiny)
library(googleVis)
library(scales)
library(dplyr)

shinyServer(function(input, output) {
  
  revals = reactiveValues()
  
  #
  # Download base asset data
  # 
  observe({
    
    input$refresh
    
    isolate({  baseasset = AssetChartData('RTS.RS', input$investperiod/12, s1=1, target=input$targetInput) })
    
    revals$baseasset = baseasset
  })
  
  
  #
  # Render base asset chart
  #
  output$baseassetChart = renderGvis({
    
    DrawAssetChart(revals$baseasset[['chartdata']])
    
  })
  
  
  #
  # Structured params calculation - reactive list
  #  
  
  observe({
    
    input$refresh
    
    isolate({
      revals$params = NULL
      params = StructParams(input$marketrate/100, 
                   input$iv/100,
                   input$defInput/100,
                   input$investperiod/12, 
                   input$longshort )
      
    })
    
    revals$params = params
    
  })
  
  
  #
  # Render params table
  #
  output$paramsTable = renderGvis({
    
    revals$params
    
    isolate({
      result = c(revals$params, revals$baseasset[2:4])
      result$roe = StructProfile(input$targetInput, result$curprice, input$longshort, input$defInput/100, result$Partn, input$investperiod/12)
      result$roeann = result$roe / (input$investperiod/12)
      result[c('roe', 'roeann', 'Partn')] = lapply(result[c('roe', 'roeann', 'Partn')], percent)
      result$target = input$targetInput
      result = result[c('today', 'expdate', 'curprice', 'target', 'Partn', 'roe', 'roeann')]
      result = result %>% data.frame %>% t %>% data.frame
      names(result) = 'Value'
      result$Params = c('Start date', 'End date', "Start price", "Target price", "Participation", "ROE, %", "ROE, annual %")
      
      
      
    })

    gvisTable(result[, c('Params', 'Value')], options=list(width=600, sort='disable'))
  })
  
  

  
  #
  # Render structured product profile chart
  #
  output$profileChart = renderGvis({
    
    
    revals$params
    
    isolate({
      
      def = input$defInput/100
      ku = revals$params$Partn
      X1 = revals$baseasset$curprice
      target = input$targetInput
      
      xEnd = X1 + abs(target - X1) * 1.1
      xStart = X1 * (1- abs(xEnd/X1-1))
      xPoints = seq(from = xStart, to = xEnd, length.out = 100)
      
      
      prfl = StructProfile(xPoints, X1,  input$longshort, def, ku, input$investperiod/12)
      
      chart.data = data.frame(AssetPrice = round(xPoints, 2), Profit=round(prfl*100, 2), Breakeven=0)
      
      gchart = gvisComboChart(data = chart.data, 
                              xvar=c('AssetPrice'), 
                              yvar=c('Profit', 'Breakeven'), 
                              options = list(
                                chartArea = "{left:50,top:10,right:0,bottom:50}",
                                series = "[{color:'red', targetAxisIndex: 0, lineWidth: 2}, 
                                {color: 'grey',targetAxisIndex: 0, lineWidth: 1, lineDashStyle: [4, 2]}]",
                                height=350, 
                                width=600, 
                                seriesType='line', 
                                legend= "{ position: 'in' }",   
                                hAxis = "{baselineColor: 'white', gridlines: {color: 'white'}, title: 'Base asset price'}",
                                vAxis="{title:'Profit, %'}"
                                
                                
                              ) )
      gchart$html$footer = ''
      gchart$html$caption = ''
      
      gchart
      
    })
    
  })
  
  
 })

