#Cut the data and put together the data into dataholidays data set
holidaysFunction <- function(year)
{
  library(readr)
  if(year==2006)XYear <- read_csv("~/TFG/CSV/2006.csv")
  if(year==2007)XYear <- read_csv("~/TFG/CSV/2007.csv")
  if(year==2008)XYear <- read_csv("~/TFG/CSV/2008.csv")
  christmas<-subset(XYear, Month%in%12 & DayofMonth%in%20:31 | Month%in%1 & DayofMonth%in%1:8)
  thanksgiving<- subset(XYear, Month%in%11 & DayofMonth%in%20:30)
  independenceDay<-subset(XYear, Month%in%7 & DayofMonth%in%2:6)
  veteransDay<-subset(XYear, Month%in%11 & DayofMonth%in%9:14)
  total<- rbind(christmas, thanksgiving,independenceDay,veteransDay)
  return (total)
}
#Add the states to the dataholidays data set. 
addStatesintoDataHolidays <- function()
{
    for(i in 1:nrow(airports)) {
      print(i)
      row= airports[i,]
      dataHolidays$OriginState[which(dataHolidays$Origin%in%row$iata)]= as.character(row$state)
      dataHolidays$DestinationState[which(dataHolidays$Dest%in%row$iata)]= as.character(row$state)
    }
  return (dataHolidays)
}
#write a csv with the name dataholidays
writeCSVHolidays <- function()
{
  write.csv(file="~/TFG/CSV/dataHolidays.csv",x = dataHolidays)
}
#This function returns a map with the airports with the total delays and the mean of each delay.
printMap <- function()
{
  library(leaflet)
  m <- leaflet()
  m <- addTiles(m)
  validAirports = unique(dataHolidays$Origin) #Non repeated values
  validAirports <- unique(dataHolidays$Dest,incomparables = validAirports) 
  #For each airport
  for(i in 1:nrow(airports)) {
    row= airports[i,]
    #Check if the iata is from any of our valid airports
    if(row$iata%in%validAirports | row$iata%in%validAirports){
      #we sum all the departure and arribal delays and calculate the mean of each one.
      departureDelay = sum(dataHolidays$DepDelay[which(dataHolidays$Origin%in%row$iata)])
      meandeparturedelay = mean(dataHolidays$DepDelay[which(dataHolidays$Origin%in%row$iata)])
      arribalDelay = sum(dataHolidays$ArrDelay[which(dataHolidays$Dest%in%row$iata)])
      meanarribaldelay = mean(dataHolidays$ArrDelay[which(dataHolidays$Dest%in%row$iata)])
      #String contruction for the map
      contentairport <- paste(sep= " ", "<b>", row$airport, "</b>")
      contentdeparturedelay <- paste(sep= " ", "Total departure delay: " ,departureDelay)
      contentmeandeparturedelay <- paste(sep= " ", "Mean departure delay: " ,meandeparturedelay)
      contentarribaldelay <- paste(sep= " ", "Total arribal delay: " ,arribalDelay)
      contentmeanarribaldelay <- paste(sep= " ", "Mean arribal delay: " ,meanarribaldelay)
      #merge of all the strings
      content <- paste(sep = "<br/>",contentairport,contentdeparturedelay,contentmeandeparturedelay,contentarribaldelay,contentmeanarribaldelay)
      #Add a marker to the map
      icon = makeAwesomeIcon(icon = 'plane', markerColor="green")
      if(departureDelay > arribalDelay)
      {
      icon = makeAwesomeIcon(icon = 'plane', markerColor="orange")
        if(meandeparturedelay >= 15)
        {
          icon = makeAwesomeIcon(icon = 'plane', markerColor="red")
        }
      }
      m <-addAwesomeMarkers(m, lng=row$long, lat=row$lat, popup=content, icon = icon)
    }
  }
  return (m) 
}
