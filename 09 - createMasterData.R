#============================================
#---------- CREATE MASTER DATA --------------
library(tidyr)

#---------- Run 00 - registrations ----------
registrations <- dataAll %>% 
 gather(Country, Reg, -Date) 

#--------- Run 01 - Inflation_Average -------
inflation <- HICP %>% 
 gather(Country, Infl, -Date) 

#--------- Run 02 - fuelPrices --------------
fuel <- dataFuelMonthly %>% 
 mutate(dateCode = ymd(paste0(dateCode,"-1")) ) %>%
 select(Country=country, Date=dateCode, Fuel=meanSuper)

#------------- Run 03 - Sentiment -----------

colnames(totalSent) <- c("Date", "EU", "EA", "Belgium", "Bulgary", "Czech.Republic", "Denmark",
 "Germany", "Estonia", "Ireland", "Greece", "Spain", "France", "Croatia",
 "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", 
 "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania",
 "Slovenia", "Slovakia", "Finland", "Sweden", "United.Kingdom"
)

sentiment <- totalSent %>%
 gather(Country, Sent, -Date)

#------------- Run 04 - intRates -----------

intRates <- cleanRates

#--------- Run 07 - unemployment -----------

unemploymentData <- unemData 

#-------------------------------------------

masterDataMerge00 <- merge(registrations, inflation, by=c("Date","Country"))
masterDataMerge01 <- merge(masterDataMerge00, fuel, by=c("Date","Country"))
masterDataMerge02 <- merge(masterDataMerge01, sentiment, by=c("Date","Country"))
masterDataMerge03 <- merge(masterDataMerge02, intRates, by=c("Date","Country"))
masterDataMerge04 <- merge(masterDataMerge03, unemploymentData, by=c("Date","Country"))

setwd("C:/Users/Lubor/Desktop/MyPaper/Data/dataR")
write.table(masterDataMerge04, file="masterPhase1.txt",
 row.names=FALSE, sep="\t")

#------- Check Data Availability ---------
#-----------------------------------------
 masterDataMerge04 %>% 
 group_by(Country, year(Date)) %>%
 summarise( n=n() ) %>%
 data.frame()