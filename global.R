library(tidyverse)
library(survival)

load("www/QRDF_shiny.Rdata")

## category definition for diagnoskod_1
basdata$dxcat[basdata$diagnoskod_1 %in% c("M05.3", "M05.8L", "M05.8M", "M05.8N",
                                          "M05.9", "M05.9L", "M05.9M", "M05.9N",
                                          "M06.0", "M06.0L", "M06.0M", "M06.0N",
                                          "M06.8L", "M06.8M", "M06.8N", "M06.9",
                                          "M12.3" )] <- "RA"
basdata$dxcat[basdata$diagnoskod_1 %in% c("M45.9")] <- "AS"
basdata$dxcat[basdata$diagnoskod_1 %in% c("M46.8","M46.9","M08.1")] <- "SPA"
basdata$dxcat[basdata$diagnoskod_1 %in% c("M07.0", "M07.1", "M07.2", "M07.3x+L40.5", 
                                          "M09.0", "M07.0+L40.5", "M07.1+L40.5", 
                                          "M07.2+L40.5")] <- "PSA"
## create categories for age ??
# age = as.double((basdata$inkluderad - as.Date(basdata$fodelsedag))/365)
# age[age <= 0] <- NA
# summary(age[age < 43])
# summary(basdata$debutalder)

## merge between terapi and basdata
terapi_basdata <- merge(basdata, terapi, by = "patientkod")

## merge between besokdata and basdata
besok_basdata <- merge(basdata, besoksdata, by = "patientkod")


##Alessio's version
# creating variable for KM plot: 1) status; 2) time
# terapi_basdata$status <- as.numeric(!is.na(terapi_basdata$utsatt) & terapi_basdata$avslutad != terapi_basdata$utsatt)
# terapi_basdata$utsatt2 <- terapi_basdata$utsatt
# terapi_basdata$utsatt2[is.na(terapi_basdata$utsatt) & is.na(terapi_basdata$avslutad)] <- Sys.Date()
# terapi_basdata$utsatt2[which(is.na(terapi_basdata$utsatt) & !is.na(terapi_basdata$avslutad) &
#                          terapi_basdata$avslutsorsak == levels(terapi_basdata$avslutsorsak)[1])] <-
#   terapi_basdata$avslutad[which(is.na(terapi_basdata$utsatt) & !is.na(terapi_basdata$avslutad) &
#                            terapi_basdata$avslutsorsak == levels(terapi_basdata$avslutsorsak)[1])]
# terapi_basdata$time <- as.numeric(terapi_basdata$utsatt2 - terapi_basdata$ordinerat)

## checking events
## event = replace(event, avslutad == levels(avslutsorsak)[1], 0)
## error: avslutad (death) date is repeated also for previous obs (by merge)
# events_check <- filter(terapi_basdata, avslutsorsak == levels(avslutsorsak)[1]) %>%
#   select(patientkod, avslutsorsak, status, utsatt, utsatt2, avslutad, time)
# filter(events_check, is.na(utsatt))

# checktime <- filter(terapi_basdata, time <= 0) %>%
#   select(ordinerat, utsatt, utsatt2, avslutad, avslutsorsak, time)


##Daniela's version
terapi_basdata$death<-as.Date(ifelse(terapi_basdata$avslutsorsak == levels(terapi_basdata$avslutsorsak)[1],terapi_basdata$avslutad,NA))
terapi_basdata$status <-ifelse(!is.na(terapi_basdata$utsatt),1,0)
#table(terapi_basdata$status)
terapi_basdata$status[which(terapi_basdata$death<terapi_basdata$utsatt)] <-0
#table(terapi_basdata$status)
terapi_basdata$utsatt2<-as.Date(ifelse(terapi_basdata$status==1,terapi_basdata$utsatt,pmin(terapi_basdata$death,Sys.Date(),na.rm=T)))
terapi_basdata$time <- as.numeric(terapi_basdata$utsatt2 - terapi_basdata$ordinerat)