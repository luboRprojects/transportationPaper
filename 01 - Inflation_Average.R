library(ggplot2)
library(scales)
# Cross-Correlations

fileHICP <- "Data/dataR/hicp_transportation12mAVG.txt"
dataHICPin <- read.table(fileHICP, sep="\t", header=TRUE)
dataHICP <- dataHICPin



colnames(dataHICP)[which(grepl(x=colnames(dataHICP), pattern="Germ"))] <- "Germany"

# We don't have HIPC-transportation for:
colnames(dataAll)[which(!(colnames(dataAll) %in% colnames(dataHICP)))]

dataHICP$Date <- dmy(dataHICP$Date)

HICP <- dataHICP %>% select(one_of(colnames(dataAll)))

#======= STOP HERE FOR IF MASTER DATA CREATION ==========

allCZ  <- dataAll %>%
 select(Date, Reg = Czech.Republic) %>%
 arrange(desc(Date) )

HICPCZ <- HICP %>% 
 select(Date, HICP = Czech.Republic)

CZanalyse <- merge(allCZ, HICPCZ, by="Date")

ggCZ <- CZanalyse %>% gather(Ind, Value, -Date)
ggCZ$Ind <- factor(ggCZ$Ind, label=c("HICP", "New Registrations") )

horLine1 <- data.frame(
 yint = 0,
 Ind  = levels(ggCZ$Ind)[1]
)

horLine2 <- data.frame(
 yint = 0,
 Ind  = levels(ggCZ$Ind)[2]
)

ggplot(ggCZ, aes(x=Date, y=Value) ) + 
 geom_line() + 
 facet_grid(Ind~., scales="free") + 
 geom_hline(data=horLine1, aes(yintercept=yint, group=Ind), col="red" ) + 
 geom_hline(data=horLine2, aes(yintercept=yint, group=Ind), col="grey80" ) + 
 scale_y_continuous(label=comma) + 
 theme_bw() + 
 theme(
  plot.title = element_text(hjust=0, size=8),
  axis.text = element_text(colour = "black", size=10)
 ) 


# ========================
stlData01 <- allCZ %>%
 filter(complete.cases(.)) %>%
 arrange(Date) 

startTS <- first(year(stlData01$Date))
endTS   <- first(month(stlData01$Date))

stlFit <- ts(stlData01$Reg, freq=12, start=c(startTS, endTS) ) %>%
 stl("per")

plot(stlFit)

# ========================

trendTS <- data.frame(
 Date  = stlData01$Date,
 Trend = data.frame(stlFit$time.series)$trend
) %>% merge(HICPCZ, by="Date")


ccf(diff(trendTS$Trend), diff(trendTS$HICP) )

head(HICP)
str(HICP) 