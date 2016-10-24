library(tidyverse)

load("www/QRDF_shiny.Rdata")

## merge between terapi and basdata
terapi_basdata <- merge(basdata, terapi, by = "patientkod")