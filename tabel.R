library(lazyeval)
rm(list = ls())

input <- list(biologic_charcs = "Cimzia",
              drange_charcs = c(as.Date("2015-01-01"), as.Date("2015-12-31")),
              diagnos_charcs = "All",
              sex_charcs = "All",
              age_cat_charcs = "All",
              region_char_cs = "All",
              median_charcs = "iqr",
              time_anal = "0")

## apply filter to terapi_basdata


n_charcs <- terapi_basdata %>%
  filter(
    (preparat == input$biologic_charcs),
    (ordinerat >= input$drange_charcs[1] & ordinerat <= input$drange_charcs[2]),
    (input$diagnos_charcs == "All" | (dxcat %in% input$diagnos_charcs)),
    (input$sex_charcs == "All" | kon.x == input$biologic_charcs),
    (input$age_cat_charcs == "All" | age_ordinerat_cat == input$age_cat_charcs),
    (input$region_char_cs == "All" | region == input$region_char_cs)
  )


n_charcs_besok <- merge(n_charcs, besoksdata, by = "patientkod")
n_charcs_besok <- n_charcs_besok %>%
  filter(datum >= (ordinerat - 7) & datum <= utsatt) %>%
  mutate(
    diff = as.double(datum - ordinerat),
    time_anal = cut(diff, breaks = c(-7, 30, 120, 365, 730, 1095, max(diff, na.rm = T)),
                    #labels = c(0, 75, 240, 547, 912, 1460),
                    include.lowest = T, right = T))


baseline <- n_charcs_besok %>% filter(time_anal == "[-7,30]")


baseline$smarta <- as.numeric(as.character(baseline$smarta))


library(reshape2)

medians <- lapply(list_charcs, function(chr){
  n_charcs %>%
    filter_(paste0("!is.na(", chr, ")")) %>%
    arrange(patientkod, abs(diff - as.numeric(input$time_anal))) %>%
    filter(!duplicated(patientkod)) %>%
    group_by(line_trt_cat) %>%
    select_(chr, "line_trt_cat") %>%
    summarise_(median = interp(~ median(var), var = as.name(chr)),
               n_complete = interp(~ sum(!is.na(var)), var = as.name(chr))) %>%
    mutate(var_char = chr) %>% 
    gather(key, val, median, n_complete) %>%
    unite(var_char, line_trt_cat, key, sep = "") %>% 
    spread(var_char, val) %>%
    mutate(var_char = chr) %>%
    select(var_char, contains("median"), contains("n_complete"))
})

prova = do.call("rbind", medians)

library(xtable)
xtable(prova)
