str(dataHolidays)

summary(dataHolidays$ArrDelay)

sum(table(dataHolidays$ArrDelay))
length(dataHolidays$ArrDelay)

require(ggplot2)
go1 <- ggplot(dataHolidays,aes(x=Dest,y=ArrDelay))+geom_boxplot()

summary(dataHolidays$Dest)
dataHolidays$Dest<-as.factor(dataHolidays$Dest)
dataHolidays$Origin<-as.factor(dataHolidays$Origin)
dataHolidays$UniqueCarrier<-as.factor(dataHolidays$UniqueCarrier)
dataHolidays$TailNum<-as.factor(dataHolidays$TailNum)
dataHolidays$CancellationCode<-as.factor(dataHolidays$CancellationCode)
res <- table(dataHolidays$Dest)

levels(dataHolidays$Dest)

install.packages(plotly)
# Add a variable for Dest with STATE (Factor) and Census REGION (Factor) : WEST, MIDWEST, NORTHEST, SOUTH and PACIFIC 

library(plotly)

pl <- ggplotly(go1)

save.image("/Users/ricardgardellagarcia/sessionMarkdown.RData")

dataHolidays <- dataHolidays[!(dataHolidays$DepDelay == ""), ]
dataHolidays <- dataHolidays[!is.na(dataHolidays$DepDelay), ]
dataHolidays <- dataHolidays[!(dataHolidays$ArrDelay == ""), ]
dataHolidays <- dataHolidays[!is.na(dataHolidays$ArrDelay), ]
