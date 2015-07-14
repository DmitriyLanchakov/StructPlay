


require(dplyr)
library(googleVis)
library(tidyr)


#+------------------------------------+
#  Option profile
#+------------------------------------+
OptionProfile = function(S.vector, startDate=NULL, endDate=NULL, price=0, face=1, ...){
  
  library(fOptions)
  
  params = list(...)
  TypeFlag = substr(tolower(params$TypeFlag), 1, 1)
  X = as.numeric(params$X)
  
  Time = params$Time
  
  if(is.null(params$sigma)){
    params$sigma = 0.6
    Time = 0
  }
  
  if(is.null(Time))
    Time = as.numeric(endDate - startDate)
  
  if(Time == 0)
    Time = 0.0000001
  
  Time = as.numeric(Time)/252
  
  r = as.numeric(params$r)
  b = r
  sigma = as.numeric(params$sigma)
  
  result = sapply(S.vector, function(x){
    
    S = x
    GBSOption(TypeFlag, S, X, Time, r, b, sigma)@price
  }) - price
  result = result * face
  
  return(result)
}


#+------------------------------------+
#  Structured product
#+------------------------------------+

StructProfile = function(S.vector, def=1, ku=1, face=1, startDate=NULL, endDate=NULL, Time=NULL, ...){
  
  params = list(...)
  X1 = params$X1
  
  result = sapply(S.vector, function(x){
    
    face * (def + max(0, x/X1-1) * ku)
  })
  
  return(result)
}


StructParams = function(rdepo=0.1, def=1, assetPrice, ...){
  
  params = list(...)
  
  
  optprice = OptionProfile(S.vector=assetPrice, startDate=params$startDate, endDate=params$endDate, Time=params$Time,
                           sigma=params$sigma, r=params$r, TypeFlag=params$TypeFlag, X=params$X)
  
  if(is.null(params$Time)) 
    Time = as.numeric(as.Date(params$endDate) - as.Date(params$startDate))
  else
    Time = params$Time
  
  ku = (rdepo + 1-def) / (optprice / assetPrice / Time*365)
  
  return(list(Days=Time, Partn=ku))
}


load('chartdata.Rdata')
lastpoint = raw.data[which.max(raw.data$Date),]

target = 65
# s1 = 1
# s2 = 0.85

# +-----------------------+
# | Prepare data for plot |
# +-----------------------+


# LoadNSaveData = function(){
#   
#   require(dplyr)
#   
#   ticker = 'GREK'
#   xfile.path = 'c:\\1\\grek.csv'
#   col.date = 1
#   col.close = 5
#   xdate.format = '%Y-%m-%d'
#   
#   raw.data = read.csv(file = xfile.path)
#   raw.data = raw.data %>% dplyr::select(col.date, col.close)
#   names(raw.data) = c('Date', ticker)
#   raw.data$Date = as.Date(strptime(as.character(raw.data$Date), xdate.format))
#   save(raw.data, file = 'chartdata.Rdata')
#   
# }
# 
# load(file = 'chartdata.Rdata')

#### Base asset data prepared ###

DrawAssetChart <- function (raw.data, expdate, ticker, s1=NULL, s2=NULL, target) {
  
  today   = max(raw.data$Date)
  expdate = as.Date(expdate)
  frstday =  today - (expdate - today)*2.5
  
  curprice =subset(raw.data, Date==today, get('ticker'), drop=T)
  s1 = curprice * s1
  s2 = curprice * s2 
  
  add.data = data.frame(Date=as.Date(seq(from=today, to=expdate, by=1)), Ticker = NA, Target = target)
  names(add.data) = c('Date', ticker, 'Target')
  
  chart.data = raw.data %>% dplyr::filter(Date > frstday) %>% dplyr::bind_rows(add.data)
  
  # View(raw.data)
 
  
  gchart = gvisComboChart(data = chart.data, 
                          xvar=c('Date'), 
                          yvar=c(ticker, 'Target'), 
                          options = list(
                            chartArea = "{left:100,top:10}",
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

#plot(DrawAssetChart(raw.data, as.Date('2015-09-15'), ticker, s1=NULL, s2=NULL, target))

