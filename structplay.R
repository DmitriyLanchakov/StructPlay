

#load('chartdata.Rdata')

#+------------------------------------+
#  Structured product parameters
#+------------------------------------+

StructParams = function(marketrate, iv, def, period, trade){
  
  require(fOptions)

  optprice = GBSOption(trade, 1, 1, period, 0, marketrate, iv)@price
  ku = (marketrate*period + 1-def) / optprice 
  
  return(list(Days=period*365, Partn=ku))
}

# StructParams(0.1, 0.2, 1, 1/4, 'c')



#+------------------------------------+
#  Structured product profile
#+------------------------------------+

StructProfile = function(S.vector, strike, trade='c', def=1, ku=1, period=1){
  
  if(trade=='c') trade=1 else trade=-1
  
  result = sapply(S.vector, function(x){
    (def + max(0, (x/strike-1)*trade*ku)) - 1
  })
  
  return(result)
}

# StructProfile(seq(from=0.8, to=1.2, by=0.01), def=0.9) %>% plot



#+------------------------------------+
#  Structured product profile
#+------------------------------------+

AssetChartData <- function(ticker, period, s1=1, target, projection=3, outputformat = 'dataframe') {
  
  require(quantmod)
  require(dplyr)
  require(tidyr)
  require(googleVis)
  
  # temp vars
  # ticker = 'RTS.RS'; period = 1/4; target = 1000; outputformat = c('dataframe', 'xts')
  suppressWarnings({
  tickerdata = getSymbols(ticker, auto.assign = F, warnings = F)[, paste0(ticker, '.Close')]
  })
  
  today   = index(tickerdata) %>% max
  expdate = today + period*365
  frstday =  today - (expdate - today)*projection
  tickerdata = tickerdata[paste0(frstday, '/', expdate),]
  
  curprice = as.numeric(tickerdata[today, 1, drop=T])
  s1 = curprice * s1
  
  targetdata = data.frame(dates = seq(from=today, to=expdate, by=1), target=target)
  targetdata = try.xts(x = targetdata[,2, drop=F], order.by = targetdata$dates)
  
  chartdata = cbind(tickerdata, targetdata) 
  
  names(chartdata) = c(ticker, 'Target')
  
  if(outputformat=='dataframe'){
    chartdata = as.data.frame(chartdata)
    chartdata$Dates = row.names(chartdata)
  }
  
  res = list(chartdata=chartdata,
             today=today,
             expdate=expdate,
             curprice=curprice)
  
  #res = chartdata
  return(res)
}

# assetdata = AssetChartData('RTS.RS', 1, target=1000, outputformat = 'dataframe')[2:4] %>% View



#+------------------------------------+
#  Base asset chart with target
#+------------------------------------+

DrawAssetChart = function(chartdata){
  
  yvars = colnames(chartdata) %>% .[.!='Dates']
  
  gchart = gvisComboChart(data = chartdata, 
                          xvar='Dates', 
                          yvar=yvars, 
                          options = list(
                            chartArea = "{left:50,top:10,right:0}",
                            series = "[{color:'red', targetAxisIndex: 0, lineWidth: 2},
  {color: 'grey',targetAxisIndex: 0, lineWidth: 1, lineDashStyle: [4, 2]}]",
                            height=300, 
                            width=600, 
                            seriesType='line', 
                            legend= "{ position: 'bottom' }",   
                            hAxis = "{baselineColor: 'white', gridlines: {color: 'white'}}"
                            
                          ) )
  gchart$html$footer = ''
  gchart$html$caption = ''
  
  
  return(gchart)
}

#   drawdata = assetdata[['chartdata']]
# DrawAssetChart((drawdata)) %>% plot


