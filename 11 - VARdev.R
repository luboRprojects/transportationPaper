#------ VAR -------
library(vars)
library(lubridate)
library(ggplot2)
library(forecast)
setwd("C:/Users/Lubor/Desktop/MyPaper")

dataIn <- read.table("C:/Users/Lubor/Desktop/MyPaper/Data/dataR/masterPhase1.txt", header=TRUE)
data <- dataIn
data$Date <- ymd(data$Date)

#------------------
tsRaw <- data[which(data$Country=="Germany"), ]
tsRawTrain <- tsRaw[which(tsRaw$Date < ymd("2017-7-1")), ]
tsRawTest <- tsRaw[which(tsRaw$Date >= ymd("2017-7-1")), ]

fitTSreg  <- ts(tsRawTrain[ ,"Reg"], start=c(2010,1), freq=12)
fitTSinfl <- stl(ts(tsRawTrain[ ,"Infl"], start=c(2010,1), freq=12), s.window="periodic")
fitTSfuel <- stl(ts(tsRawTrain[ ,"Fuel"], start=c(2010,1), freq=12), s.window="periodic")
fitTSsent <- stl(ts(tsRawTrain[ ,"Sent"], start=c(2010,1), freq=12), s.window="periodic")

#------------------------------------------------
fitTSreg 
trendcycle(fitTSinfl)

seasTab <- data.frame(
 month  = 1:12,
 effect = seasPart[1:12])

plot(seasPart[1:12], type="l")
#------- SEASON DEV ----
seasPart <- seasonal(fitTSinfl)

seasPart[1:12]

#-----------------------

dataVar <- na.exclude(ts.union(
 reg  = diff(fitTSreg),
 infl = diff(trendcycle(fitTSinfl)),
 fuel = diff(trendcycle(fitTSfuel)),
 sent = diff(trendcycle(fitTSsent))
) )

fitTSreg[length(fitTSreg)]

fit1 <- VAR(dataVar, season=12)

pred <- predict(fit1)$fcst$reg
pred[1, ] <- pred[1, ] + fitTSreg[length(fitTSreg)]

ggData 

predDF <- data.frame(
 apply(pred, 2, cumsum) )

#
plot(predict(fit1), names="reg" )
fanchart(predict(fit1), names="reg")
plot(irf(fit1))

#------------------------------------------------

par(mfrow=c(3,3))
plot(trendcycle(fitTSinfl) )
 plot(diff(trendcycle(fitTSinfl)))
  plot(diff(diff(trendcycle(fitTSinfl))))
plot(trendcycle(fitTSfuel) )
 plot(diff(trendcycle(fitTSfuel)))
  plot(diff(diff(trendcycle(fitTSfuel))))
plot(trendcycle(fitTSsent) )
 plot(diff(trendcycle(fitTSsent)))
  plot(diff(diff(trendcycle(fitTSsent))))

dev.off()

nAhead = 6
trendFCinfl <- forecast(diff(trendcycle(fitTSinfl)), h=nAhead)
 plot(trendFCinfl)

# trendFCinfl <- forecast(trendcycle(fitTSinfl), h=nAhead)
#  plot(trendFCinfl)

addBase <- data.frame(trendFCinfl)
 nObsTrain <- length(trendcycle(fitTSinfl))
addBase[1, ] <- addBase[1, ] + trendcycle(fitTSinfl)[nObsTrain]

predVals <- data.frame(
 apply(addBase, 2, cumsum)
 )

#---------- First Part -------
ggData1 <- data.frame(
 Date  = tsRawTrain$Date,
 Trend = c(trendcycle(fitTSinfl) )
)

ggData2 <- data.frame(
 Date = max(ggData1$Date) + months(0:(nAhead)),
 Trend =  c(ggData1$Trend[nObsTrain], predVals$Point.Forecast),
 Low = c(ggData1$Trend[nObsTrain], predVals$Lo.95),
 Upp = c(ggData1$Trend[nObsTrain], predVals$Hi.95)
)

ggDataReal <- tsRawTest[,c("Date", "Infl")]

ggplot(ggData1, aes(x=Date, y=Trend) ) + 
 geom_line(size=1.15) + 
 geom_hline(yintercept=0, col="red") + 
 geom_line(aes(), colour="blue") + 
 geom_ribbon(data=ggData2,
  aes(x=Date, y=Trend, ymin = Low, ymax = Upp), fill = "grey90") +
 geom_line(data=ggData2, aes(y=Trend), colour="green", size=1.15) + 
 geom_line(data=ggDataReal, aes(x=Date, y=Infl), col="blue" ) + 
 theme_bw()


plot(tsRaw$Infl, type="l")






autoplot(trendFCinfl)
autoplot(fitTSinfl)
autoplot(fitTSfuel)
autoplot(fitTSsent)

head(tsRaw)
 ts1CleanModel <- reactive({
  fitTS1 <- stl(tsData()[ ,1], s.window="periodic")
  fitTS1
 })
  
 ts2CleanModel <- reactive({
  fitTS2 <- stl(tsData()[ ,2], s.window="periodic")
  fitTS2
 })