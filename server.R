library(shiny)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(plotly)
library(survival)
library(lazyeval)
library(htmlTable)
library(xtable)

shinyServer(function(input, output){
  
  ## interactive user interfaces
  ## -----------------------------------------------------------------------------------------------------------------------
  
  ## diagnosis
  output$diagnos_1 <- renderUI({
    if (is.null(input$diagnos)){
      return(NULL)
    } else {
      if (input$diagnos == "All") return(NULL)
      choices = as.character(unique(basdata$diagnos_1[basdata$diagnos_kategori1 == input$diagnos]))
      selectizeInput("sub_diag", label = "Sub diagnosis",
                     choices = choices, multiple = TRUE, selected = choices)
    }
  })
  output$diagnos_1_besok <- renderUI({
    if (is.null(input$diagnos_besok)){
      return(NULL)
    } else {
      if (input$diagnos_besok == "All") return(NULL)
      choices = as.character(unique(besok_basdata$diagnos_1[besok_basdata$diagnos_kategori1 == input$diagnos_besok]))
      selectizeInput("sub_diag_besok", label = "Sub diagnosis",
                     choices = choices, multiple = TRUE, selected = choices)
    }
  })
  output$diagnos_1_bio <- renderUI({
    if (is.null(input$diagnos_bio)){
      return(NULL)
    } else {
      if (input$diagnos_bio == "All") return(NULL)
      choices = as.character(unique(terapi_basdata$diagnos_1.y[terapi_basdata$diagnos_kategori1 == input$diagnos_bio]))
      selectizeInput("sub_diag_bio", label = "Sub diagnosis",
                     choices = choices, multiple = TRUE, selected = choices)
    }
  })
  
  ## for sliders
  output$slideDate <- renderUI({
    minValue <- ifelse(input$time_unit == "month", Sys.Date() - 365.25*3, "1999-01-01")
    sliderInput("drange", "Range limit", min = as.Date("1999-01-01"), max = (Sys.Date() - 1),
                value = c(as.Date(minValue), (Sys.Date() - 1)))
  })
  output$slideDate_besok <- renderUI({
    minValue <- ifelse(input$time_unit_besok == "month", Sys.Date() - 365.25*3, "1999-01-01")
    sliderInput("drange_besok", "Range limit", min = as.Date("1999-01-01"), max = (Sys.Date() - 1),
                value = c(as.Date(minValue), (Sys.Date() - 1)))
  })
  output$slideDate_bio <- renderUI({
    minValue <- ifelse(input$time_unit_bio == "month", Sys.Date() - 365.25*3, "1999-01-01")
    sliderInput("drange_bio", "Range limit", min = as.Date("1999-01-01"), max = (Sys.Date() - 1),
                value = c(as.Date(minValue), (Sys.Date() - 1)))
  })
  output$slideDate_km <- renderUI({
    minValue <- ifelse(input$biologic_km != "All", min(terapi_basdata$ordinerat[terapi_basdata$preparat == input$biologic_km]), "1999-01-01")
    dateRangeInput("drange_km",
                   label = "Range limit",
                   start = as.Date(minValue), end = Sys.Date())
  })
  output$slideDate_charcs <- renderUI({
    minValue <- ifelse(input$biologic_charcs != "", min(terapi_basdata$ordinerat[terapi_basdata$preparat == input$biologic_charcs]), "1999-01-01")
    dateRangeInput("drange_charcs",
                   label = "Range limit",
                   start = as.Date(minValue), end = Sys.Date())
  })
  
  
  
  ## reactive computations
  ## -----------------------------------------------------------------------------------------------------------------------
  
  n_ts <- reactive({
    basdata %>%
      ## exclude years before 1999 + interactive filtering
      filter(#year(inkluderad) >= 1999,
        (input$diagnos == "All" | (diagnos_kategori1 %in% input$diagnos &
                                     diagnos_1 %in% input$sub_diag)),
        (input$diagnos != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
           !as.logical(input$tidig_ra*(tidig_ra == 0))),
        (input$sex == "All" | kon == input$sex),
        (age_inclusion <= input$age[2] & age_inclusion >= input$age[1]),
        (input$region == "All" | region == input$region)
      ) %>%
      ## it's maybe better to create this variable in global.R
      mutate(year = floor_date(inkluderad, "year"),
             month = floor_date(inkluderad, "month")) %>%
      group_by_(input$time_unit) %>%
      summarize(number = n()) %>% 
      filter_(paste(input$time_unit, c(">= \'", "<= \'"), floor_date(input$drange, input$time_unit), "\'")) %>%
      na.omit() %>%
      # for privacy reasons
      filter(input$region == "All" | number > 5)
  })
  
  n_ts_besok <- reactive({
    besok_basdata %>%
      ## exclude years before 1999 + interactive filtering
      filter(#year(inkluderad) >= 1999,
        datum <= Sys.Date(),
        (input$diagnos_besok == "All" | (diagnos_kategori1 %in% input$diagnos_besok &
                                           diagnos_1 %in% input$sub_diag_besok)),
        (input$diagnos_besok != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
           !as.logical(input$tidig_ra_besok*(tidig_ra == 0))),
        (input$sex_besok == "All" | kon == input$sex_besok),
        (age_visit <= input$age_besok[2] & age_visit >= input$age_besok[1]),
        (input$region_besok == "All" | region == input$region_besok)
      ) %>%
      ## it's maybe better to create this variable in global.R
      mutate(year = floor_date(datum, "year"),
             month = floor_date(datum, "month")) %>%
      group_by_(input$time_unit_besok) %>%
      summarize(number = n()) %>% 
      filter_(paste(input$time_unit_besok, c(">= \'", "<= \'"), floor_date(input$drange_besok, input$time_unit_besok), "\'")) %>%
      na.omit() %>%
      # for privacy reasons
      filter(input$region_besok == "All" | number > 5)
  })
  
  n_ts_bio <- reactive({
    n_ts_bio <- terapi_basdata %>%
      ## exclude years before 1999 + interactive filtering
      filter(#ar >= 1999,
        (input$diagnos_bio == "All" | (diagnos_kategori1 %in% input$diagnos_bio &
                                         diagnos_1.y %in% input$sub_diag_bio)),
        (input$diagnos_bio != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
           !as.logical(input$tidig_ra_bio*(tidig_ra == 0))),
        (input$line_bio == 0 | line_trt == input$line_bio | (input$line_bio == 3 & line_trt >= 3)),
        (input$sex_bio == "All" | kon.x == input$sex_bio),
        (age_ordinerat <= input$age_bio[2] & age_ordinerat >= input$age_bio[1]),
        (input$region_bio == "All" | region == input$region_bio)
      ) %>%
      mutate(year = floor_date(ordinerat, "year"),
             month = floor_date(ordinerat, "month")) %>%
      group_by_(input$time_unit_bio, "preparat") %>%
      summarize(number = n()) %>%
      group_by_(input$time_unit_bio) %>% mutate(total = sum(number)) %>%
      filter(input$biologic == "All" | preparat == input$biologic)
    if (input$biologic == "All"){
      n_ts_bio <- n_ts_bio %>% group_by_(input$time_unit_bio) %>%
        summarise(total = sum(number))
    }
    n_ts_bio %>% 
      filter_(
        paste(input$time_unit_bio, c(">= \'", "<= \'"), floor_date(input$drange_bio, input$time_unit_bio), "\'")
      )
  })
  
  pagaende <- reactive({
    drange <- floor_date(input$drange_bio, input$time_unit_bio)
    dates <- seq(drange[1], drange[2], by = input$time_unit_bio)
    pagaende <- do.call("rbind", lapply(dates, function(x)
      terapi_basdata %>%
        mutate(time_level = x) %>%
        filter(
          (input$diagnos_bio == "All" | (diagnos_kategori1 %in% input$diagnos_bio &
                                           diagnos_1.y %in% input$sub_diag_bio)),
          (input$diagnos_bio != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
             !as.logical(input$tidig_ra_bio*(tidig_ra == 0))),
          (input$line_bio == 0 | line_trt == input$line_bio | (input$line_bio == 3 & line_trt >= 3)),
          (input$sex_bio == "All" | kon.x == input$sex_bio),
          (age_ordinerat <= input$age_bio[2] & age_ordinerat >= input$age_bio[1]),
          (input$region_bio == "All" | region == input$region_bio),
          (ordinerat <= x) & (utsatt > x | pagaende == 1)
        ) %>%
        group_by_("time_level", "preparat") %>%
        summarize(number = n()) %>%
        group_by_("time_level") %>% mutate(total = sum(number)) %>%
        filter(
          input$biologic == "All" | preparat == input$biologic
        )
    ))
    if (input$biologic == "All"){
      pagaende <- pagaende %>% group_by_("time_level") %>%
        summarise(total = sum(number))
    }
    pagaende %>%
      rename_(.dots=setNames("time_level", input$time_unit_bio))
  })
  
  
  # for big table
  n_big_bio <- reactive({
    bigTab <- if (input$ongoing == FALSE){
      n_ts_bio <- terapi_basdata %>%
        filter(
          input$biologic == "All" | preparat == input$biologic,
          ordinerat <= input$drange_bio[2] &  ordinerat >= input$drange_bio[1]
        ) %>%
        group_by(diagnos_1.x, line_trt_cat, kon.x, age_ordinerat_cat) %>%
        summarize(number = n())
    } else {
      drange <- floor_date(input$drange_bio, input$time_unit_bio)
      dates <- seq(drange[1], drange[2], by = input$time_unit_bio)
      pagaende <- do.call("rbind", lapply(dates, function(x)
        terapi_basdata %>%
          mutate(time_level = x) %>%
          filter(
            input$biologic == "All" | preparat == input$biologic,
            ordinerat <= input$drange_bio[2] &  ordinerat >= input$drange_bio[1],
            (ordinerat <= x) & (utsatt > x | pagaende == 1)
          ) %>%
          group_by(diagnos_1.x, line_trt_cat, kon.x, age_ordinerat_cat) %>%
          summarize(number = n())))
    }
    bigTab
  })
  
  surv.data <- reactive({
    bio_km <- terapi_basdata %>%
      filter(
        time > 0,
        utsatt2 <= Sys.Date(),
        ordinerat >= input$drange_km[1] & ordinerat <= input$drange_km[2],
        (input$line_km == 0 | line_trt == input$line_km | (input$line_km == 3 & line_trt >= 3)),
        (input$region_km == "All" | region == input$region_km),
        (input$diagnos_km == "All" | dxcat == input$diagnos_km),
        (input$sex_km == "All" | kon.x == input$sex_km),
        (age_ordinerat <= input$age_km[2] & age_ordinerat >= input$age_km[1])
      )
    
    if (input$biologic_km != "All"){
      datemin_drug <- min(bio_km$ordinerat[bio_km$preparat == input$biologic_km])
      bio_km <- bio_km %>% filter(ordinerat >= datemin_drug)
    }
    
    surv.data <- data.frame()
    surv.data <- rbind(
      with(summary(survfit(Surv(time, status) ~ 1, data = bio_km)), data.frame(preparat = "All", time, surv, n.risk, n.event))
    )
    if (input$biologic_km != "All"){
      surv.data <- rbind(surv.data,
                         with(summary(survfit(Surv(time, status) ~ 1,
                                              data = subset(bio_km, preparat == input$biologic_km))),
                              data.frame(preparat = input$biologic_km, time, surv, n.risk, n.event)))
    }
    surv.data
  })
  
  ## variable to summarize in tab disease characteristics
  list_charcs <- reactive({
    list_charcs <- list("eq5d", "smarta", "patientens_globala", "lakarbedomning",
                        "sr", "crp", "haq")
    if (input$diagnos_charcs %in% c("RA", "PSA")){
      list_charcs <- c(list_charcs, "svullna_leder", "omma_leder", "das28", "das28CRP")
    }
    if (input$diagnos_charcs %in% c("SPA", "AS")){
      list_charcs <- c(list_charcs, "svullna_leder66", "omma_leder68", "basdai",
                       "asdas_sr", "asdas_crp")
    }
    list_charcs
  })
  
  n_charcs <- reactive({
    if (input$biologic_charcs == "") return(NULL)
    terapi_basdata %>%
      filter(
        (preparat == input$biologic_charcs),
        (ordinerat >= input$drange_charcs[1] & ordinerat <= input$drange_charcs[2]),
        (input$diagnos_charcs == "All" | (dxcat %in% input$diagnos_charcs)),
        (input$sex_charcs == "All" | kon.x == input$sex_charcs),
        (age_ordinerat <= input$age_charcs[2] & age_ordinerat >= input$age_charcs[1]),
        (input$region_char_cs == "All" | region == input$region_char_cs)
      ) %>% 
      merge(besoksdata, by = "patientkod") %>%
      filter(datum >= (ordinerat - 7) & datum <= utsatt) %>%
      mutate(
        diff = as.double(datum - ordinerat),
        time_anal = cut(diff, breaks = c(-7, 30, 90, 240, 480, 730, 1095, max(diff, na.rm = T)),
                        labels = c(0, 75, 150, 365, 600, 912, 1460),
                        include.lowest = T, right = T)) %>%
      filter(time_anal == as.character(input$time_anal))
  })
  
  medians <- reactive({
    if (input$biologic_charcs == "") return(NULL)
    if (nrow(n_charcs()) == 0L) return(NULL)
    do.call("rbind", lapply(list_charcs(), function(chr){
      n_charcs() %>%
        filter_(paste0("!is.na(", chr, ")")) %>%
        arrange(patientkod, abs(diff - as.numeric(input$time_anal))) %>%
        filter(!duplicated(patientkod)) %>%
        group_by(line_trt_cat) %>%
        select_(chr, "line_trt_cat") %>%
        summarise_(median = interp(~ median(var), var = as.name(chr)),
                   n_complete = interp(~ sum(!is.na(var)), var = as.name(chr)),
                   iqr_low = interp(~ quantile(var, probs = .25, na.rm = T), var = as.name(chr)),
                   iqr_upp = interp(~ quantile(var, probs = .75, na.rm = T), var = as.name(chr))
        ) %>%
        mutate(var_char = chr) %>% 
        gather(key, val, median, n_complete, iqr_low, iqr_upp) %>%
        unite(var_char, line_trt_cat, key, sep = "") %>% 
        spread(var_char, val) %>%
        mutate(var_char = chr) %>%
        select(var_char, contains("median"), contains("n_complete"), contains("iqr"))
    }))
  })
  
  medians_tab <- reactive({
    if (input$biologic_charcs == "") return(NULL)
    if (is.null(medians())) return(NULL)
    medians <- cbind(variable = medians()$var_char, as.data.frame(
      do.call("cbind", lapply(1:3, function(line){
        index <- medians()[, grep(line, names(medians()))]
        if (length(index) == 0) median <- NA
        if (input$median_charcs == "iqr"){
          index <- index[-grep("n_", names(index))]
          median <- apply(index, 1, function(x){
            paste0(x[1], " (", x[2], "-", x[3], ")")
          })
        } else {
          index <- index[-grep("iqr", names(index))]
          median <- apply(index, 1, function(x){
            paste0(x[1], " (", x[2], ")")
          })
        }
      }))
    ))
    ## adding additional info
    summaries <- n_charcs() %>%
      arrange(patientkod, abs(diff - as.numeric(input$time_anal))) %>%
      filter(!duplicated(patientkod)) %>%
      rowwise() %>%
      mutate(
        dmard_conc = 1 - all(is.na(cbind(dmard1, dmard2, dmard3, dmard4)) |
                               data.frame(dmard1, dmard2, dmard3, dmard4) == "Otezla"),
        mtx = 1*any(data.frame(dmard1, dmard2, dmard3, dmard4) == "Methotrexate", na.rm = T),
        nsaid_conc = 1 - is.na(nsaid),
        kortison_conc = 1 - is.na(kortison)
      ) %>%
      group_by(line_trt_cat) %>%
      summarize(n = n(),
                median_age = round(median(age_ordinerat, na.rm = T), 2),
                female = round(100*mean(kon.x == "Kvinna"), 2),
                percent_dmard_conc = round(100*mean(dmard_conc)),
                percent_mtx = round(100*mean(mtx)),
                percent_nsaid = round(100*mean(nsaid_conc)),
                percent_kortison = round(100*mean(kortison_conc))
      )
    addinfo <- cbind(variable = colnames(summaries[-1]),
                     do.call("cbind", lapply(1:3, function(line){
                       index <- summaries[grep(line, summaries$line_trt_cat), ]
                       if (nrow(index) == 0){
                         rep(NA, 7)
                       } else {
                         t(index[-1])
                       }
                     })))
    colnames(medians) <- colnames(addinfo) <- c("variable", "1st line", "2nd line", "3+line")
    medians <- rbind(addinfo, medians)
    medians
  })
  
  
  ## rendering output
  ## -----------------------------------------------------------------------------------------------------------------------
  
  ## rendering tables
  output$table <- renderDataTable({
    n_ts()
  })
  output$table_besok <- renderDataTable({
    n_ts_besok()
  })
  output$table_bio <- renderDataTable({
    table_bio <- if (input$ongoing == FALSE){
      n_ts_bio()
    } else {
      pagaende()
    }
    if (input$region_bio != "All"){
      if (input$biologic != "All"){
        table_bio <- table_bio %>% filter(number > 5)
      } else {
        table_bio <- table_bio %>% filter(total > 5)
      }
    }
    table_bio 
  })
  output$tableBig_bio <- renderDataTable({ 
    n_big_bio()
    })
  output$table_KM <- renderDataTable({
    surv.data()
  })
  
  output$table_charcs_all <- renderDataTable({
    if (input$biologic_charcs == "") return(NULL)
    n_charcs()
  })
  output$table_median <- renderDataTable({
    if (input$biologic_charcs == "") return(NULL)
    medians()
  })
  
  #includeCSS("www/table1.css")
  #output$table_charcs <- renderText({
  output$table_charcs <- renderDataTable({   
    if (input$biologic_charcs == "") return(NULL)
    #htmlTable(medians_charcs())
    #           header =  c("", rep(c("1", "2", "3+"), 2)),
    #           n.cgroup = c(1, 3, 3),
    #           cgroup = c("Variables", "Median", "N complete")
    # )
    # print(xtable(medians_charcs()), type = "html",
    #       include.rownames = FALSE)
    medians_tab()
  }
  )
  
  ## rendering plots
  output$tsplot <- renderDygraph({
    dataxts <- as.xts(n_ts(), order.by = n_ts()[[input$time_unit]])
    dygraph(dataxts) %>% dyLegend(width = 700) %>%
      dyRangeSelector(dateWindow = input$drange)
  })
  
  output$tsplot_besok <- renderDygraph({
    dataxts <- as.xts(n_ts_besok(), order.by = n_ts_besok()[[input$time_unit_besok]])
    dygraph(dataxts) %>% dyLegend(width = 700) %>%
      dyRangeSelector(dateWindow = input$drange_besok)
  })
  
  output$tsplot_bio <- renderDygraph({
    data_bio <- if (input$ongoing == FALSE){
      n_ts_bio()
    } else {
      pagaende()
    }
    if (input$region_bio != "All"){
      if (input$biologic != "All"){
        data_bio <- data_bio %>% filter(number > 5)
      } else {
        data_bio <- data_bio %>% filter(total > 5)
      }
    }
    dataxts <- if (input$biologic == "All"){
      as.xts(data_bio[, "total"], order.by = data_bio[[input$time_unit_bio]])
    } else {
      varlist <- if (input$biologic != "All"){
        if (input$showall == TRUE){
          c("number", "total")
        } else {
          c("number")
        }
      }
      as.xts(data_bio[, varlist], order.by = data_bio[[input$time_unit_bio]])
    }
    dygraph(dataxts) %>% dyLegend(width = 700) %>%
      dyRangeSelector(dateWindow = input$drange_bio)
  })
  
  output$KM <- renderPlotly({
    ggplotly(
      ggplot(surv.data(), aes(x = time/(365/12), y = surv, col = preparat)) + 
        geom_step() + xlab("Months")
    )
  })
  
  
  ## download tables
  ## -----------------------------------------------------------------------------------------------------------------------
  
  output$downloadTab <- downloadHandler(
    filename = function() {
      paste('tab-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(n_ts(), con)
    }
  )
  
  output$downloadTab_besok <- downloadHandler(
    filename = function() {
      paste('tab_besok-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(n_ts_besok(), con)
    }
  )
  
  output$downloadTab_bio <- downloadHandler(
    filename = function() {
      paste('tab_bio-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(if (input$ongoing == FALSE){
        n_ts_bio()
      } else {
        pagaende()
      }, con)
    }
  )
  
  output$downloadTab_km <- downloadHandler(
    filename = function() {
      paste('tab_km-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(surv.data(), con)
    }
  )
  
  output$downloadTab_charcs <- downloadHandler(
    filename = function() {
      paste('tab_charcs-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(medians_tab(), con)
    }
  )
  
})