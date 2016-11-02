library(tidyverse)
library(survival)
library(lubridate)

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

## date of death
basdata$deathdate <- basdata$avslutad
basdata$deathdate[which(basdata$avslutsorsak != levels(basdata$avslutsorsak)[1] |
                          is.na(basdata$avslutsorsak))] <- NA

## create categories for age ??
# age = as.double((basdata$inkluderad - as.Date(basdata$fodelsedag))/365)
# age[age <= 0] <- NA
# summary(age[age < 43])
# summary(basdata$debutalder)

## merge between terapi and basdata
terapi_basdata <- merge(basdata, terapi, by = "patientkod")
## merge between besokdata and basdata
besok_basdata <- merge(basdata, besoksdata, by = "patientkod")


# creating variable for KM plot: 1) status; 2) time
terapi_basdata$status <- as.numeric(!is.na(terapi_basdata$utsatt))
terapi_basdata$status[which(terapi_basdata$deathdate <= terapi_basdata$utsatt)] <- 0  
terapi_basdata$utsatt2 <- terapi_basdata$utsatt
terapi_basdata$utsatt2[terapi_basdata$status == 0] <- pmin(terapi_basdata$deathdate[terapi_basdata$status == 0], Sys.Date(), na.rm=T)
terapi_basdata$time <- as.numeric(terapi_basdata$utsatt2 - terapi_basdata$ordinerat)

## Fix 
# bio2015<-subset(bio, (pagaende==1 & ordinerat<"2016-01-01") | (pagaende==0 & utsatt>="2016-01-01" & utsatt<="2016-01-01"))
# 
# 
# prova <- terapi_basdata %>%
#   mutate(year = floor_date(ordinerat, "year"),
#          month = floor_date(ordinerat, "month")) 
# 
# x = split(prova, prova$year)[[7]]
# #x$pagaende==1 & x$ordinerat < "2016-01-01") | (pagaende==0 & utsatt>="2016-01-01" & utsatt<="2016-01-01")
