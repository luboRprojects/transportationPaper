library(dplyr)
library(tidyr)
library(lubridate)

fileUnem <- "C:/Users/Lubor/Desktop/MyPaper/Data/dataR/unem.txt"
unemDataIn <- read.table(fileUnem, sep="\t", header=TRUE)
unemData <- unemDataIn %>% 
 mutate(Date = dmy(Date) ) %>%
 gather(Country, Unem, -Date)



#======= STOP HERE FOR IF MASTER DATA CREATION ==========

#------------------
library(ggplot2)
library(urca)
library(lmtest)

ggData <- unemData %>% 
 gather(Country, Unemployment, -Date) %>%
 mutate(Date = dmy(Date) )

ggplot(ggData, aes(x=Date, y=Unemployment, group=Country) ) + 
 geom_line() + 
 facet_wrap(Country~., ncol=4)

ggDataDiff <- ggData %>%
 group_by(Country) %>%
 mutate(Udiff = Unemployment - lag(Unemployment)) %>%
 na.omit

ggplot(ggDataDiff, aes(x=Date, y=Udiff , group=Country) ) + 
 geom_line() + 
 facet_wrap(Country~., ncol=4) +
 scale_x_date(date_labels = "%y", "Year") + 
 theme_bw()

ggsave("unemployment.pdf")

ggDataDiffDiff <- ggDataDiff %>%
 group_by(Country) %>%
 mutate(UDiffDiff = Udiff - lag(Udiff)) %>%
 na.omit

#----------------- Stationarity Tests -----------------#
# ADF, H0: nonstationary = unit root
a <- ur.df(ggData[which(ggData$Country=="Austria"), "Unemployment"])
summary(ur.df(ggData[, "Unemployment"]) )


#---------------- AIC Selection ---------------
ggData %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(Unemployment, selectlags = "AIC")@teststat), 2),
  critValADF  = as.numeric(ur.df(Unemployment, selectlags = "AIC")@cval)[2],
  lagADF      = as.numeric(ur.df(Unemployment, selectlags = "AIC")@lags),
  KPSS = round(as.numeric(ur.kpss(Unemployment)@teststat, test="tau"), 2),
  critValKPSS = as.numeric(ur.kpss(Unemployment)@cval, test="tau")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame

#---------------- Manual Lag Selection ---------------
fixLags <- 12

ggData %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(Unemployment, lags=fixLags)@teststat), 2),
  critValADF  = as.numeric(ur.df(Unemployment, lags=fixLags)@cval)[2],
  lagADF      = as.numeric(ur.df(Unemployment, lags=fixLags)@lags),
  DWadf       = round(dwtest(ur.df(Unemployment, lags=fixLags)@testreg$residuals~1)$p.value,2),
  KPSS = round(as.numeric(ur.kpss(Unemployment)@teststat, test="tau"), 2),
  critValKPSS = as.numeric(ur.kpss(Unemployment)@cval, test="tau")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame

#---------------- Diff Data, AIC ---------------

ggDataDiff %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(Udiff)@teststat), 2),
  critValADF  = as.numeric(ur.df(Udiff)@cval)[2],
  lagADF      = as.numeric(ur.df(Udiff)@lags),
  DWadf       = round(dwtest(ur.df(Udiff)@testreg$residuals~1)$p.value,2),
  KPSS = round(as.numeric(ur.kpss(Udiff)@teststat, lags="short"), 2),
  critValKPSS = as.numeric(ur.kpss(Udiff)@cval, lags="short")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame

#---------------- Diff, Manual Lag Selection ---------------
fixLags <- 10

ggDataDiff %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(Udiff, lags=fixLags)@teststat), 2),
  critValADF  = as.numeric(ur.df(Udiff, lags=fixLags)@cval)[2],
  lagADF      = as.numeric(ur.df(Udiff, lags=fixLags)@lags),
  DWadf       = round(dwtest(ur.df(Udiff, lags=fixLags)@testreg$residuals~1)$p.value,2),
  KPSS = round(as.numeric(ur.kpss(Udiff)@teststat, test="tau"), 2),
  critValKPSS = as.numeric(ur.kpss(Udiff)@cval, test="tau")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame

#---------------- Diff Diff Data, AIC ---------------

ggDataDiffDiff %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(UDiffDiff)@teststat), 2),
  critValADF  = as.numeric(ur.df(UDiffDiff)@cval)[2],
  lagADF      = as.numeric(ur.df(UDiffDiff)@lags),
  DWadf       = round(dwtest(ur.df(UDiffDiff)@testreg$residuals~1)$p.value,2),
  KPSS = round(as.numeric(ur.kpss(UDiffDiff)@teststat, lags="short"), 2),
  critValKPSS = as.numeric(ur.kpss(UDiffDiff)@cval, lags="short")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame

#---------------- Diff Diff, Manual Lag Selection ---------------
fixLags <- 10

ggDataDiffDiff %>%
 group_by(Country) %>%
 summarise(
  ADF  = round(as.numeric(ur.df(UDiffDiff, lags=fixLags)@teststat), 2),
  critValADF  = as.numeric(ur.df(UDiffDiff, lags=fixLags)@cval)[2],
  lagADF      = as.numeric(ur.df(UDiffDiff, lags=fixLags)@lags),
  DWadf       = round(dwtest(ur.df(UDiffDiff, lags=fixLags)@testreg$residuals~1)$p.value,2),
  KPSS = round(as.numeric(ur.kpss(UDiffDiff)@teststat, test="tau"), 2),
  critValKPSS = as.numeric(ur.kpss(UDiffDiff)@cval, test="tau")[2],
  statusADF   = ifelse(ADF>critValADF, "UR", "Stat"),
  statusKPSS  = ifelse(KPSS > critValKPSS, "Trend", "Stat"),
 ) %>% data.frame


#------------- 
Some variables needs to be I(2) to become stationary.
#------------- 




















