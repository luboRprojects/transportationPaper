library(dplyr)
library(lubridate)

#-----------------------------------------------
setwd("C:/Users/Lubor/Desktop/MyPaper")
reg03 <- read.table("Data/dataR/reg03.txt", sep="\t", header=TRUE)
countriesSelect <- colnames(reg03)[seq(30)][-c(17,22)]

reg17 <- read.table("Data/dataR/reg17.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg16 <- read.table("Data/dataR/reg16.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg15 <- read.table("Data/dataR/reg15.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg14 <- read.table("Data/dataR/reg14.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg13 <- read.table("Data/dataR/reg13.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg12 <- read.table("Data/dataR/reg12.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg11 <- read.table("Data/dataR/reg11.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg10 <- read.table("Data/dataR/reg10.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg09 <- read.table("Data/dataR/reg09.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg08 <- read.table("Data/dataR/reg08.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg07 <- read.table("Data/dataR/reg07.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg06 <- read.table("Data/dataR/reg06.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg05 <- read.table("Data/dataR/reg05.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg04 <- read.table("Data/dataR/reg04.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
reg03 <- reg03 %>% select(countriesSelect) 
# reg02 <- read.table("Data/dataR/reg02.txt", sep="\t", header=TRUE) %>% select(countriesSelect)
# reg01 <- reg01 %>% select(countriesSelect)

dataAllBind <- rbind(reg17,reg16,reg15,reg14,reg13,reg12,reg11, reg10,reg09, reg08,reg07,
 reg06,reg05,reg04, reg03)# , reg02,reg01)

dataAll <- dataAllBind %>% 
 mutate(Country = dmy(Country)) %>% 
 dplyr::rename(Date = Country)


#----------- STOP HERE FOR OTHER ANALYSIS -----------

library(scales)
# library(plotly)
library(tidyr)
library(forecast)
library(ggplot2)

dataAllIndex <- dataAll
dataAllIndex[-1] <- dataAllIndex[, -1] %>% mutate_all(funs(. / last(.)))

ggDataIndex <- dataAllIndex %>% 
 gather(Country, Reg, -Date) %>% filter(Country !="Ireland")

ggplot(ggDataIndex, aes(x=Date, y=Reg) ) + 
 geom_line() +  
 geom_hline(yintercept=0, col="red") + 
 facet_wrap(~ Country, ncol=4 ) +
 theme_bw()


#=========== FIT MODELS =============
belgiumTS  <- ts(dataAll$Belgium, freq = 12)
fit <- decompose(belgiumTS) 
 fit %>% plot

seasadj(fit)

belgiumSTL <- stl(belgiumTS, s.window = "periodic", robust = TRUE)$time.series
 plot(belgiumSTL)
fcst <- forecast(belgiumSTL)
plot(fcst)











bgETSbelg <- baggedETS(belgiumTS)
summary(bgETSbelg)
summary(bgETSbelg)
fcast <- forecast(bgETSbelg)
plot(fcast)


 
