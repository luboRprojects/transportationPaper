#------- Fuel Prices Analysis --------
fileFuel <- "Data/dataR/fuelPrices.txt"
dataFuelIn <- read.table(fileFuel, sep="\t", header=TRUE)
dataFuel <- dataFuelIn

dataFuelMonthly <- dataFuel %>% 
 mutate(
  date     = dmy(date),
  dateCode = paste0(year(date),"-",month(date)) ) %>%
 group_by(country, dateCode) %>%
 summarise(
  minSuper  = min(super),
  meanSuper = mean(super),
  maxSuper  = max(super),
  minDiesel  = min(diesel),
  meanDiesel = mean(diesel),
  maxDiesel  = max(diesel),
  nValues = n()
 )

#======= STOP HERE FOR IF MASTER DATA CREATION ==========
head(dataFuelMonthly)
head(dataFuel)

ggData <- dataFuelMonthly %>% 
 mutate(
  Date = ymd(paste0(dateCode,"-1"))
 ) %>% 
 filter(country=="Austria") %>%
 select(Country=country, Date, Super=meanSuper, Diesel=meanDiesel) %>% 
 gather(Fuel, Price, -Country, -Date)


ggplot(ggData, aes(x=Date, y=Price, colour=Fuel) ) + 
 geom_line() + 
 theme_bw() + 
 scale_y_continuous(label=comma, limit=c(0, NA) ) + 
 ggtitle(ggData$Country[1])
 
#----------------------
# Merge datasets

fuelDataMerge <- dataFuelMonthly %>% 
 mutate(
  Date = ymd(paste0(dateCode,"-1"))
 ) %>% 
 select(Country=country, Date, Super=meanSuper, Diesel=meanDiesel) %>% 
 gather(Fuel, Price, -Country, -Date)
 
dataAllMerge <- dataAll %>% 
 gather(Country, Reg, -Date)

fuelReg <- merge(fuelDataMerge, dataAllMerge, by=c("Country","Date"))


# write.table(x=fuelReg, file="Data/dataR/fuelReg.txt", sep="\t", row.names=FALSE)



#-------------- Shiny DEV -------------
head(fuelReg0)
fuelReg <- fuelReg0 %>% # filter(Country=="Austria")%>% 
 spread(Fuel, Price) %>% 
 gather(Type, Value, -Country, -Date) %>% 
 mutate(class = ifelse(Type=="Reg", "Reg", "Fuel") )


ggplot(fuelReg, aes(x=Date, y=Value, colour=Type) ) + 
 geom_line() + 
 scale_y_continuous(limits=c(0, NA), "", label=comma) + 
 facet_grid(class~., scales="free") + 
 theme_bw() + 
 theme(
  plot.title = element_text(hjust=0, size=8),
  axis.text = element_text(colour = "black", size=10)
 ) 

