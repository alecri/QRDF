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
basdata$dxcat <- as.factor(basdata$dxcat)
basdata$age_inclusion <- as.numeric(basdata$inkluderad - as.Date(basdata$fodelsedag))/365.25
#summary(basdata$age_inclusion)
basdata$age_inclusion_cat <- cut(basdata$age_inclusion, breaks =  c(18, 65, 100), right = FALSE)


## age and categories for age
basdata$deathdate <- basdata$avslutad
basdata$deathdate[which(basdata$avslutsorsak != levels(basdata$avslutsorsak)[1] |
                          is.na(basdata$avslutsorsak))] <- NA

basdata$age_inclusion <- as.numeric(basdata$inkluderad - as.Date(basdata$fodelsedag))/365.25
#summary(basdata$age_inclusion)
basdata$age_inclusion_cat <- cut(basdata$age_inclusion, breaks =  c(18, 65, 100), right = FALSE)
#table(basdata$age_inclusion_cat)

## from factor to double
besoksdata$smarta <- as.numeric(sub(",", ".", as.character(besoksdata$smarta)))
besoksdata$patientens_globala <- as.numeric(sub(",", ".", as.character(besoksdata$patientens_globala)))

## terapi line_trt categories
terapi$line_trt_cat <- cut(terapi$line_trt, breaks = c(0, 1, 2, max(terapi$line_trt, na.rm = T)),
                           labels = c("1", "2", "3+"))

## merge between besokdata and basdata
besok_basdata <- merge(basdata, besoksdata, by = "patientkod")
besok_basdata$age_visit <- as.numeric(besok_basdata$datum - as.Date(besok_basdata$fodelsedag))/365.25
#summary(besok_basdata$age_visit)
besok_basdata$age_visit_cat <- cut(besok_basdata$age_visit, breaks =  c(18, 65, 100), right = FALSE)
#table(besok_basdata$age_visit_cat)

##Data cleaning of therapy
terapi2<-subset(terapi, preparat_kod != "PRV" & ordinerat >="1995-01-01")

## merge between terapi and basdata
terapi_basdata <- merge(basdata, terapi2, by = "patientkod")
terapi_basdata$age_ordinerat <- as.numeric(terapi_basdata$ordinerat - as.Date(terapi_basdata$fodelsedag))/365.25
#summary(basdata$age_inclusion)
terapi_basdata$age_ordinerat_cat <- cut(terapi_basdata$age_ordinerat, breaks =  c(18, 65, 100), right = FALSE)


# creating variable for KM plot: 1) status; 2) time
terapi_basdata$status <- as.numeric(!is.na(terapi_basdata$utsatt))
terapi_basdata$status[which(terapi_basdata$deathdate <= terapi_basdata$utsatt)] <- 0  
terapi_basdata$utsatt2 <- terapi_basdata$utsatt
terapi_basdata$utsatt2[terapi_basdata$status == 0] <- pmin(terapi_basdata$deathdate[terapi_basdata$status == 0], Sys.Date(), na.rm=T)
terapi_basdata$time <- as.numeric(terapi_basdata$utsatt2 - terapi_basdata$ordinerat)