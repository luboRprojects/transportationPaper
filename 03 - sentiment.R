#-----------  Data Sentiment -----------
file <- "C:/Users/Lubor/Desktop/MyPaper/Data/dataR/sentiment.txt"
dataSentIn <- read.table(file, sep="\t", header=TRUE)

sentData <- dataSentIn
head(sentData)

totalSent <- sentData %>%
 select(Date, contains("TOT.COF") ) %>% #TOT.COF
 mutate(
  Date = floor_date(dmy(Date), "month")
 ) %>% 
 filter(Date > dmy("31/12/2009") )

colnames(totalSent)[-1] <- colnames(totalSent)[-1] %>% substr(6,7)

#======= STOP HERE FOR IF MASTER DATA CREATION ==========

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

str(totalSent)
head(totalSent)

#---------- Plotting ------------
ggData <- totalSent %>% gather(Country, Sent, -Date)

ggplot(ggData, aes(x=Date, y=Sent) ) + 
 geom_line() + 
 facet_wrap(~Country, ncol=5) + 
 theme_bw()



 facet_grid(Country~.)

