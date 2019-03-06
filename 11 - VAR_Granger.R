library(vars)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)

source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\estVarfun.R")
source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\granger.R")
# source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\grangerBest.R")
source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\getSignif.R")
source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\getIrf.R")
source("C:\\Users\\Lubor\\Desktop\\MyPaper\\Scripts\\estVarfunBest.R")

#--------------------------------------------------------------------------
#-------------------------------
dataIn <- read.table("C:/Users/Lubor/Desktop/MyPaper/Data/dataR/masterPhase1.txt", header=TRUE)
cleanData <- dataIn
cleanData$Date <- ymd(cleanData$Date)

cleanData %>% group_by(Country) %>% summarise(count = n()) %>% data.frame()

#--------- Diff needed ---------
fileDiff <- "C:/Users/Lubor/Desktop/MyPaper/Data/dataR/diffNeeded.txt"
diffData <- read.table(fileDiff , sep="\t", header=TRUE)


#----------------------------------
#diffData$ADF <- 1

#-----------------------------------
#--------------------------------------------------------------------------

at  <- estVar("Austria")
be  <- estVar("Belgium")
cz  <- estVar("Czech.Republic")
d   <- estVar("Denmark")
est <- estVar("Estonia")
fin <- estVar("Finland")
fr  <- estVar("France")
ger <- estVar("Germany")
gre <- estVar("Greece")
# hun	<- estVar("Hungary")
irl <- estVar("Ireland")
it  <- estVar("Italy")
lat <- estVar("Latvia")
lit <- estVar("Lithuania")
lux <- estVar("Luxembourg")
net <- estVar("Netherlands")
pol <- estVar("Poland")
svk <- estVar("Slovakia")
slo <- estVar("Slovenia")
esp <- estVar("Spain")
swe <- estVar("Sweden")
uk  <- estVar("United.Kingdom")
# %>% summary(equation="reg")

atSIG <- getSignif(at)
beSIG <- getSignif(be)
czSIG <- getSignif(cz)
dSIG <- getSignif(d)
estSIG <- getSignif(est)
finSIG <- getSignif(fin)
frSIG <- getSignif(fr)
gerSIG <- getSignif(ger)
greSIG <- getSignif(gre)
# hunSIG <- getSignif(hun)
irlSIG <- getSignif(irl)
itSIG <- getSignif(it)
latSIG <- getSignif(lat)
litSIG <- getSignif(lit)
luxSIG <- getSignif(lux)
netSIG <- getSignif(net)
polSIG <- getSignif(pol)
svkSIG <- getSignif(svk)
sloSIG <- getSignif(slo)
espSIG <- getSignif(esp)
sweSIG <- getSignif(swe)
ukSIG <- getSignif(uk)

allSignif <- data.frame(rbind(
 atSIG, beSIG, czSIG, dSIG, estSIG, finSIG,
 frSIG, gerSIG, greSIG, irlSIG, itSIG,
 latSIG, litSIG, luxSIG, netSIG, polSIG,
 svkSIG, sloSIG, espSIG, sweSIG, ukSIG) )

allSignif %>% 
 dplyr::select(Country, Variable, pVal=pval) %>%
  spread(Variable, pVal)

allSignif %>% 
 dplyr::select(Country, Variable, estimate) %>%
  spread(Variable, estimate)

atG  <- estGranger("Austria")
beG  <- estGranger("Belgium")
czG  <- estGranger("Czech.Republic")
dG   <- estGranger("Denmark")
estG <- estGranger("Estonia")
finG <- estGranger("Finland")
frG  <- estGranger("France")
gerG <- estGranger("Germany")
greG  <- estGranger("Greece")
# hunG <- estGranger("Hungary")
irlG <- estGranger("Ireland")
itG  <- estGranger("Italy")
latG <- estGranger("Latvia")
litG <- estGranger("Lithuania")
luxG <- estGranger("Luxembourg")
netG <- estGranger("Netherlands")
polG <- estGranger("Poland")
svkG <- estGranger("Slovakia")
sloG <- estGranger("Slovenia")
espG <- estGranger("Spain")
sweG <- estGranger("Sweden")
ukG  <- estGranger("United.Kingdom")

allGranger <- data.frame(rbind(
 atG, beG, czG, dG, estG, finG,
 frG, gerG, greG, irlG, itG,
 latG, litG, luxG, netG, polG,
 svkG, sloG, espG, sweG, ukG) ) #, hunG


allGranger %>% dplyr::select(Country, Shock, pValMan) %>% spread(Shock, pValMan)

allGranger %>% filter(pValAIC <=0.05)

#----------------------------
atBest <- estVarBest("Austria")
beBest <- estVarBest("Belgium")
czBest <- estVarBest("Czech.Republic")
dBest <- estVarBest("Denmark")
estBest <- estVarBest("Estonia")
finBest <- estVarBest("Finland")
frBest <- estVarBest("France")
gerBest <- estVarBest("Germany")
greBest <- estVarBest("Greece")
irlBest <- estVarBest("Ireland")
itBest <- estVarBest("Italy")
latBest <- estVarBest("Latvia")
litBest <- estVarBest("Lithuania")
luxBest <- estVarBest("Luxembourg")
netBest <- estVarBest("Netherlands")
polBest <- estVarBest("Poland")
svkBest <- estVarBest("Slovakia")
sloBest <- estVarBest("Slovenia")
espBest <- estVarBest("Spain")
sweBest <- estVarBest("Sweden")
ukBest <- estVarBest("United.Kingdom")

atSIGbest <- getSignif(atBest)
beSIGbest <- getSignif(beBest)
czSIGbest <- getSignif(czBest)
dSIGbest <- getSignif(dBest)
estSIGbest <- getSignif(estBest)
finSIGbest <- getSignif(finBest)
frSIGbest <- getSignif(frBest)
gerSIGbest <- getSignif(gerBest)
greSIGbest <- getSignif(greBest)
irlSIGbest <- getSignif(irlBest)
itSIGbest <- getSignif(itBest)
latSIGbest <- getSignif(latBest)
litSIGbest <- getSignif(litBest)
luxSIGbest <- getSignif(luxBest)
netSIGbest <- getSignif(netBest)
polSIGbest <- getSignif(polBest)
svkSIGbest <- getSignif(svkBest)
sloSIGbest <- getSignif(sloBest)
espSIGbest <- getSignif(espBest)
sweSIGbest <- getSignif(sweBest)
ukSIGbest <- getSignif(ukBest)

allSignifBest <- data.frame(rbind(
 atSIGbest, beSIGbest, czSIGbest, dSIGbest, estSIGbest,
 finSIGbest, frSIGbest, gerSIGbest, greSIGbest,
 irlSIGbest, itSIGbest, latSIGbest, litSIGbest,
 luxSIGbest, netSIGbest, polSIGbest, svkSIGbest,
 sloSIGbest, espSIGbest, sweSIGbest, ukSIGbest))

allSignifBest %>% 
 dplyr::select(Country, Variable, pVal=pval) %>%
  spread(Variable, pVal)

allSignifBest %>% 
 dplyr::select(Country, Variable, estimate) %>%
  spread(Variable, estimate)
