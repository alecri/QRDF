library(tidyverse)

load("www/QRDF_shiny.Rdata")

## create categories for age ??
# age = as.double((basdata$inkluderad - as.Date(basdata$fodelsedag))/365)
# age[age <= 0] <- NA
# summary(age[age < 43])
# summary(basdata$debutalder)

## merge between terapi and basdata
terapi_basdata <- merge(basdata, terapi, by = "patientkod")

## merge between besokdata and basdata
besok_basdata <- merge(basdata, besoksdata, by = "patientkod")


## format for dates in x-lab dygraph
getYear <- 'function(d, gran) {
              var d = new Date(d.getTime() + 7200*1000);
return d.strftime("%Y-%m-%d");}'