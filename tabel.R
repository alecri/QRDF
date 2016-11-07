library(lazyeval)

input <- list(biologic_charcs = "Cimzia",
              drange_charcs = c(as.Date("2015-01-01"), as.Date("2015-12-31")),
              diagnos_charcs = "All",
              sex_charcs = "All",
              age_cat_charcs = "All",
              region_char_cs = "All",
              median_charcs = "iqr")

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
    time_anal = cut(diff, breaks = c(-7, 30, 120, 365, 730, 1095), 
                    include.lowest = T, right = T))


baseline <- n_charcs_besok %>% filter(time_anal == "[-7,30]")


list_charcs <- c("age_ordinerat", "eq5d", "smarta", "patients_globala", "lakarbedoming",
                 "sr", "crp", "haq")
# "patients_globala", "smarta"
additional_charcs <- c(list_charcs, "svullna_leder", "omma_leder", "das28", "das28CRP")
additional_charcs2 <- c(list_charcs, "svullna_leder66", "omma_leder68", "basdai",
                        "asdas_sr", "asdas_crp")

baseline$smarta <- as.numeric(as.character(baseline$smarta))

prova <- lapply(list_charcs, function(chr){
  baseline %>%
    filter_(paste0("!is.na(", chr, ")")) %>%
    arrange(patientkod, abs(diff-0)) %>%
    filter(!duplicated(patientkod)) %>%
    group_by(line_trt) %>%
    select_(chr, "line_trt") %>%
    summarise_(median = interp(~ median(var), var = as.name(chr)))
})
names(prova) <- list_charcs