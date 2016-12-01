library(tidyverse)
library(lubridate)
library(survival)

load("www/QRDF_shiny.Rdata")

## new categorie for age
basdata$age_inclusion_cat <- cut(basdata$age_inclusion, breaks =  c(18, 30, 45, 55, 65, 110), right = FALSE)
besok_basdata$age_visit_cat <- cut(besok_basdata$age_visit, breaks =  c(18, 30, 45, 55, 65, 110), right = FALSE)
terapi_basdata$age_ordinerat_cat <- cut(terapi_basdata$age_ordinerat, breaks =  c(18, 30, 45, 55, 65, 110), right = FALSE)