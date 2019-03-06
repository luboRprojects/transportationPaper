#--------- Analyse Selected Countries ----------#
# Czech.Republic, Slovakia, Germany, Ireland

library(scales)
dataIn <- read.table("C:/Users/Lubor/Desktop/MyPaper/Data/dataR/masterPhase1.txt", header=TRUE)
data <- dataIn 
data$Date <- ymd(data$Date)

dataPredAll <- data %>% select(Date, Country, Registrations=Reg) %>% 
 filter(Country %in% c("Czech.Republic", "Slovakia", "Germany", "Ireland") )


source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\prepDataIrf.R")
# diffData[diffData$Series=="Reg", ]

#-------------------------------------------------
#------------ CZECH REPUBLIC ---------------------
country <- "Czech.Republic"
model   <- czBest

#------------------ SLOVAKIA ---------------------
country <- "Slovakia"
model   <- svkBest

#------------------- GERMANY ---------------------
country <- "Germany"
model   <- gerBest

#------------------- IRELAND ---------------------
country <- "Ireland"
model   <- irlBest

#------------------------------------------------
# After choosing the country run the code below
#------------------------------------------------

ggData <- dataPredAll %>%
 filter(Country == country) 

ggForecastInit <- predict(model, n.ahead=12)$fcst$reg %>% data.frame

#- Check the I(?)
condition <- diffData[diffData$Series=="Reg" & diffData$Country==country, "ADF"] == 0
ggForecastInit[1,1] <- if (condition)  ggForecastInit[1,1]  else  ggForecastInit[1,1] + ggData[85,"Registrations"]

ggForecast <- ggForecastInit %>% 
 mutate(
  Date  = ggData[85:96,"Date"],
  fcst = if(condition) ggForecastInit$fcst else cumsum(ggForecastInit$fcst) ) %>%
 mutate(
  lower = if(condition) lower else fcst + lower,
  upper = if(condition) upper else fcst + upper
 )

ggplot() + 
 geom_ribbon(data=ggForecast, aes(x=Date, ymin=lower, ymax=upper), colour="grey", alpha=0.1) + 
 geom_line(data=ggData, aes(x=Date, y=Registrations) ) +  
 geom_line(data=ggForecast, aes(x=Date, y=fcst), size=1.05 ) +
 scale_x_date(date_labels = "%Y", "Year", breaks="year") + 
 scale_y_continuous(label=comma, limits=c(0, NA))  + 
 theme_bw() + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) ) + ggtitle(country)

ggsave("CZfanplot.pdf",  width=4.7, height=3.5)
ggsave("SKfanplot.pdf",  width=4.7, height=3.5)
ggsave("GERfanplot.pdf", width=4.7, height=3.5)
ggsave("IRLfanplot.pdf", width=4.7, height=3.5)

#--------------------------------------------
#======== Impuls Response Functions ========= 

dataCZ  <- prepDataIrf("Czech.Republic")
dataSK  <- prepDataIrf("Slovakia")
dataGER <- prepDataIrf("Germany")
dataIR  <- prepDataIrf("Ireland")

irfData <- VAR(dataCZ, type="const", p=czBest$p, season=12) %>% 
 irf(., response = "reg", n.ahead = 12,
 ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.8,
 runs = 100, seed = 123) 

irfData <- VAR(dataSK, type="const", p=svkBest$p, season=12) %>% 
 irf(., response = "reg", n.ahead = 12,
 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.8,
 runs = 100, seed = 123) 

irfData <- VAR(dataGER, type="const", p=gerBest$p, season=12) %>% 
 irf(., response = "reg", n.ahead = 12,
 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.8,
 runs = 100, seed = 123)

irfData <- VAR(dataIR, type="const", p=irlBest$p, season=12) %>% 
 irf(., response = "reg", n.ahead = 12,
 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.8,
 runs = 100, seed = 123) 

#------------------

p1 <- plotIrf(irfData, name=c("infl") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=9) )

p2 <- plotIrf(irfData, name=c("fuel") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )

p3 <- plotIrf(irfData, name=c("unem") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )

p4 <- plotIrf(irfData, name=c("st") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )

p5 <- plotIrf(irfData, name=c("sent") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )

p6 <- plotIrf(irfData, name=c("reg2") ) +
  scale_x_continuous(breaks=seq(13), "Months ahead", labels=0:12 ) + 
 theme(
  panel.grid.minor.x = element_blank(),
  axis.text = element_text(colour = "black", size=10) )

GGgrob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=3, top="Czech Republic")
  ggsave("irfCZ.pdf", GGgrob,  width = 7, height = 4)
GGgrob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=3, top="Slovakia")
  ggsave("irfSK.pdf", GGgrob,  width = 7, height = 4)
GGgrob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=3, top="Germany")
  ggsave("irfGER.pdf", GGgrob,  width = 7, height = 4)
GGgrob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=3, top="Ireland")
  ggsave("irfIRL.pdf", GGgrob,  width = 7, height = 4)

ggsave("irfCZ.pdf", GGgrob,  width = 7, height = 4)

