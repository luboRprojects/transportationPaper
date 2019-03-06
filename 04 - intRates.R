library(dplyr)
library(tidyr)
library(lubridate)
library(imputeTS)

fileRates <- "Data/dataR/intRates.txt"
ratesdataIn <- read.table(fileRates, sep="\t", header=TRUE)
ratesData <- ratesdataIn

ratesData <- ratesData %>% 
 mutate(TIME = ymd(paste0(TIME, "-1"))
 ) %>%
 select(
  Country, Date=TIME, ST, LT)

allDates <- data.frame(
 Date = seq(from=min(ratesData$Date), to=max(ratesData$Date), by="months")
)

cleanRates <- merge(allDates, ratesData, by="Date") %>% 
 arrange(Date, Country)

#======= STOP HERE FOR IF MASTER DATA CREATION ==========

#---------------------------------
unique(cleanRates$Country)
cleanRates %>% filter(Country=="Germany") %>% data.frame()
cleanRates %>% filter(Country=="EST") %>% data.frame()











#----------------------------------
ratesData %>% 
 gather(Rate, Value, -Date, -Country) %>%
 group_by(Country, year(Date) ) %>%
 summarise(
  n= na.omit(n())
 ) %>% data.frame


ratesData %>% 
 gather(Rate, Value, -Date, -Country) %>%
 group_by(Country, year(Date) ) %>%
 summarise(
  n= n()
 ) %>% data.frame

