library(plyr)
#library(dplyr)
library(tidyr)
library(knitr)
library(survival)
library(ggplot2)
library(rms)

setwd("C:/Users/Daniela/Box Sync/SRQ (daniela.digiuseppe@ki.se)/QRDF/Otezla/05-10-2016")
Otezla <- read.csv2("Otezla_5-10-2016.csv",header = TRUE, sep = ",")
#  save(Otezla, file="Otezla_08-09-2016.RData")
#  
Otezla_visits <- read.csv2("Otezla_visits_5-10-2016.csv",header = TRUE, sep = ",")
#  save(Otezla_visits, file="Otezla_visits_06-08-2016.RData")
# 
# load("C:/Users/Daniela/Box Sync/SRQ (daniela.digiuseppe@ki.se)/QRDF/Otezla/Otezla_06-08-2016.RData")
# load("C:/Users/Daniela/Box Sync/SRQ (daniela.digiuseppe@ki.se)/QRDF/Otezla/Otezla_visits_06-08-2016.RData")

#Number of started treatment by diagnosis

Otezla$start_month<-substr(Otezla$ordinerat, 1, 7)
diagnos<-ddply(Otezla,c("diagnos_1","start_month"), "nrow")

diagnos2 <- spread(diagnos, start_month, nrow)
diagnos2



ddply(Otezla,c("start_month"), "nrow")

ddply(Otezla,c(), "nrow")

#Number of ongoing treatment by diagnosis
leve<-levels(as.factor(Otezla$start_month))

for (i in 1:nlevels(factor(Otezla$start_month))){
  Otezla$end_month<-substr(Otezla$utsatt, 1, 7)
  Otezla[,ncol(Otezla)+1]<-ifelse((Otezla$pagaende==1 & Otezla$start_month<(leve[i])) | 
                                    (Otezla$pagaende==0 & Otezla$end_month>=(leve[i]) & Otezla$start_month<(leve[i])),1,0)
  colnames(Otezla)[ncol(Otezla)] <- leve[i] 
}

l <- reshape(Otezla, 
             varying = c(leve), 
             v.names = "pag",
             timevar = "months", 
             times = c(leve), 
             direction = "long")

diagnos3 <-ddply(l,.(diagnos_1,months), summarise,
                 N=sum(pag))
diagnos3 <- spread(diagnos3, months, N)
diagnos3



ddply(l,.(months), summarise,
      N=sum(pag))

