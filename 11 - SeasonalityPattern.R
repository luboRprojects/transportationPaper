#-------- Seasonality Pattern -----------
library(dplyr)
library(purrr)
library(forecast)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

dataIn <- read.table("C:/Users/Lubor/Desktop/MyPaper/Data/dataR/masterPhase1.txt", header=TRUE)
data <- dataIn # %>% select(-ST)
data$Date <- ymd(data$Date)

#===================================
# Manual Entry

data[data$Date=="2015-07-01" & data$Country=="Greece", "LT"] <- (11.43+10.26)/2

#===================================
data %>% filter(Country=="Greece")
data %>% filter(Country=="Estonia")
data %>% filter(Country=="Hungary")

data <- data %>% 
 filter(Country != "Hungary") 

# %>% filter(Country != "Estonia")

data %>% group_by(Country ) %>%  summarise(n = n()) %>% data.frame

#-----------------------------------------

makeTs <- function(x){
 trendcycle(stl(ts(x, start=c(2010,1), freq=12), s.window="periodic"))
}

cleanData <- data %>% 
 select(-LT) %>%
# filter(Country != "Hungary") %>%
# filter(Country != "Luxembourg") %>%
# filter(complete.cases(.)) %>%
 group_by(Country) %>%
 mutate_if(is.numeric, makeTs)

cleanData %>% group_by(Country ) %>%  summarise(n = n()) %>% data.frame

# write.table(x=cleanData, "C:/Users/Lubor/Desktop/MyPaper/Data/dataR/trendCycleData.txt", sep="\t", row.names=FALSE)
#------------------------------------------
# 	PROCEED WITH STATIONARITY TESTING   #
#------------------------------------------

cleanData %>% filter(Country == "Czech.Republic") %>%
 ungroup() %>%
 select(-Country) %>%
 gather(Indicator, Value, -Date) %>% 
  ggplot(., aes(x=Date, y=Value, group=Indicator) ) + 
  geom_line() + 
  facet_grid(Indicator~., scales="free") + 
  theme_bw()


#------------------------------------------

data %>%
 filter(complete.cases(.) ) %>%
 group_by(Country) %>%
 summarise(nobs = n() ) %>% data.frame

data %>%
 filter(complete.cases(.) ) %>%
 filter(Country=="Hungary")

data %>%
 filter(Country=="Hungary")

data %>%
 filter(Country=="Estonia")

apply(rak, 2, tcIdent)

stl(rak$Reg)
trendcycle(stl(ts(rak$Reg, start=c(2010,1), freq=12), s.window="periodic"))

funTS <- function(a){
 trendcycle(stl(ts(a, start=c(2010,1), freq=12), s.window="periodic"))
}

#===================================================

makeTs2 <- function(x){
 seasonal(stl(ts(x, start=c(2010,1), freq=12), s.window="periodic"))
}

nested <- data %>% 
# select(Date) %>% 
 filter(Country != "Hungary") %>%
 group_by(Country, Reg, Infl, Fuel, Sent, ST, LT, Unem) 


cumsumData <- nested %>% 
 mutate(model = map(data, cumsum))

makeCS <- function(x){cumsum(x)}

nested %>% 
 mutate(model = map(data, makeCS))

makeTs <- function(x){
 trendcycle(stl(ts(x, start=c(2010,1), freq=12), s.window="periodic"))
}

#---------------------------------------------------
#		---- Strength of Seasonlity ---
#---------------------------------------------------
seasStrong <- function(x){
 stl_features(ts(x, start=c(2010,1), freq=12), s.window="periodic")
}

seasRes <- data %>% 
 group_by(Country) %>%
 arrange(Country, Date) %>%
 summarise(seasIndex = seasStrong(Reg)["seasonal_strength"]) %>% 
 arrange(desc(seasIndex) )

seasRes %>% data.frame

seasGGdata <- data %>% 
 filter(Country %in% c("Latvia", "Czech.Republic", "Slovakia", "Germany", "Ireland", "United.Kingdom")) %>% 
 select(Country, Date, Registrations=Reg) %>% 
 transform(Country = factor(Country, 
   levels=c("Latvia", "Slovakia", "Czech.Republic", "Ireland",
            "Germany", "United.Kingdom")) )

ggplot(seasGGdata, aes(x=Date, y=Registrations, group=Country) ) + 
 geom_line() + 
 scale_x_date(date_labels = "%y", "Year") + 
 scale_y_continuous(label=comma, limits=c(0, NA)) + 
 facet_wrap(.~Country, ncol=6, scales="free") + 
 theme_bw() + 
 theme(
  panel.grid.major.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )


# ggsave("seasStrength.pdf", width=10, height=2)
#====================================================================

library(fable)
library(tsibble)

tsData <- data %>% filter(Country != "Hungary") %>%
 as_tsibble(., key = id(Country), index = Date) %>% 

nestedData <- data %>% 
 select(-Date) %>%
 group_by(Country) %>% nest

aa <- data %>% select(-Date) %>% filter(Country != "Hungary") %>% group_by(Country) %>% nest

aa %>% 
 map(data, function(x){apply(x, 2, cumsum)})

lapply(aa, apply(2, cumsum) )