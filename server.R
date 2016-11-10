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
   
   
   
   ## reactive computations
   ## -----------------------------------------------------------------------------------------------------------------------
   
   n_ts <- reactive({
      group_bylist <- if (input$compare != "none"){
         c(input$time_unit, input$compare)
      } else {
         c(input$time_unit)
      }

      basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#year(inkluderad) >= 1999,
                (input$diagnos == "All" | (diagnos_kategori1 %in% input$diagnos &
                                           diagnos_1 %in% input$sub_diag)),
            (input$diagnos != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
               !as.logical(input$tidig_ra*(tidig_ra == 0)))
            ) %>%
         ## it's maybe better to create this variable in global.R
         mutate(year = floor_date(inkluderad, "year"),
                month = floor_date(inkluderad, "month")) %>%
         group_by_(.dots = group_bylist) %>%
         summarize(number = n()) %>% 
        filter_(paste(input$time_unit, c(">= \'", "<= \'"), floor_date(input$drange, input$time_unit), "\'")) %>%
        na.omit()
   })
   
   n_ts_besok <- reactive({
     group_bylist <- if (input$compare_besok != "none"){
       c(input$time_unit_besok, input$compare_besok)
     } else {
       c(input$time_unit_besok)
     }
     
      besok_basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#year(inkluderad) >= 1999,
           datum <= Sys.Date(),
                (input$diagnos_besok == "All" | (diagnos_kategori1 %in% input$diagnos_besok &
                                              diagnos_1 %in% input$sub_diag_besok)),
           (input$diagnos_besok != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
              !as.logical(input$tidig_ra_besok*(tidig_ra == 0)))
           ) %>%
         ## it's maybe better to create this variable in global.R
         mutate(year = floor_date(datum, "year"),
                month = floor_date(datum, "month")) %>%
        group_by_(.dots = group_bylist) %>%
         summarize(number = n()) %>% 
       filter_(paste(input$time_unit_besok, c(">= \'", "<= \'"), floor_date(input$drange_besok, input$time_unit_besok), "\'")) %>%
        na.omit()
   })
   
   n_ts_bio <- reactive({
      n_ts_bio <- terapi_basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#ar >= 1999,
                (input$diagnos_bio == "All" | (diagnos_kategori1 %in% input$diagnos_bio &
                                                  diagnos_1.y %in% input$sub_diag_bio)),
                (input$diagnos_bio != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
                   !as.logical(input$tidig_ra_bio*(tidig_ra == 0)))
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
        filter_(paste(input$time_unit_bio, c(">= \'", "<= \'"), floor_date(input$drange_bio, input$time_unit_bio), "\'"))
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
         (ordinerat <= x) & (utsatt > x | pagaende == 1)
       ) %>%
       group_by_("time_level", "preparat") %>%
       summarize(number = n()) %>%
       group_by_("time_level") %>% mutate(total = sum(number)) %>%
       filter(input$biologic == "All" | preparat == input$biologic)
     )
     )

     if (input$biologic == "All"){
       pagaende <- pagaende %>% group_by_("time_level") %>%
         summarise(total = sum(number))
     }
     pagaende %>%
       rename_(.dots=setNames("time_level", input$time_unit_bio))
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
         (input$age_cat_km == "All" | age_ordinerat_cat == input$age_cat_km)
       )
     
     if (input$biologic_km != "All"){
       datemin_drug <- min(bio_km$ordinerat[bio_km$preparat == input$biologic_km])
       bio_km <- bio_km %>% filter(ordinerat >= datemin_drug)
     }
     
     surv.data <- data.frame()
     surv.data <- rbind(
       with(summary(survfit(Surv(time, status) ~ 1, data = bio_km)), data.frame(preparat = "All", time, surv, upper, lower))
     )
     if (input$biologic_km != "All"){
       surv.data <- rbind(surv.data,
                          with(summary(survfit(Surv(time, status) ~ 1,
                                               data = subset(bio_km, preparat == input$biologic_km))),
                               data.frame(preparat = input$biologic_km, time, surv, upper, lower)))
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
   
   medians_charcs <- reactive({
     if (input$biologic_charcs == "") return(NULL)
     n_charcs <- terapi_basdata %>%
       filter(
         (preparat == input$biologic_charcs),
         (ordinerat >= input$drange_charcs[1] & ordinerat <= input$drange_charcs[2]),
         (input$diagnos_charcs == "All" | (dxcat %in% input$diagnos_charcs)),
         (input$sex_charcs == "All" | kon.x == input$sex_charcs),
         (input$age_cat_charcs == "All" | age_ordinerat_cat == input$age_cat_charcs),
         (input$region_char_cs == "All" | region == input$region_char_cs)
       ) %>% 
       merge(besoksdata, by = "patientkod") %>%
       filter(datum >= (ordinerat - 7) & datum <= utsatt) %>%
       mutate(
         diff = as.double(datum - ordinerat),
         time_anal = cut(diff, breaks = c(-7, 30, 120, 365, 730, 1095, max(diff, na.rm = T)),
                         labels = c(0, 75, 240, 547, 912, 1460),
                         include.lowest = T, right = T)) %>%
       filter(time_anal == as.character(input$time_anal))
     
     medians <- do.call("rbind", lapply(list_charcs(), function(chr){
       n_charcs %>%
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
     
     medians <- as.data.frame(
       do.call("cbind", lapply(1:3, function(line){
         index <- medians[, grep(line, names(medians))]
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
     )
     colnames(medians) <- c("1st line", "2nd line", "3+line")
     medians
   })
   

   ## rendering output
   ## -----------------------------------------------------------------------------------------------------------------------
   
   ## rendering table
   output$table <- renderDataTable({
      n_ts()
   })
   output$table_besok <- renderDataTable({
      n_ts_besok()
   })
   output$table_bio <- renderDataTable({
     if (input$ongoing == FALSE){
       n_ts_bio()
     } else {
       pagaende()
     }
   })
   
   ## rendering plot
   output$tsplot <- renderDygraph({
      
    dataxts <- if (input$compare != "none"){
       nts <- n_ts() %>% spread_(input$compare, "number")
       as.xts(nts, order.by = nts[[input$time_unit]])
     } else {
       as.xts(n_ts(), order.by = n_ts()[[input$time_unit]])
     }
     dygraph(dataxts) %>% dyLegend(width = 700) %>%
       dyRangeSelector(dateWindow = input$drange)
   })

   output$tsplot_besok <- renderDygraph({
     
     dataxts <- if (input$compare_besok != "none"){
       nts <- n_ts_besok() %>% spread_(input$compare_besok, "number")
       as.xts(nts, order.by = nts[[input$time_unit_besok]])
     } else {
       as.xts(n_ts_besok(), order.by = n_ts_besok()[[input$time_unit_besok]])
     }
     
     dygraph(dataxts) %>% dyLegend(width = 700) %>%
        dyRangeSelector(dateWindow = input$drange_besok)
   })
      
   output$tsplot_bio <- renderDygraph({
     data_bio <- if (input$ongoing == FALSE){
       n_ts_bio()
     } else {
       pagaende()
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
   
   
   output$table_KM <- renderDataTable({
     surv.data()
   })
   
   output$KM <- renderPlotly({
     ggplotly(
       ggplot(surv.data(), aes(x = time, y = surv, col = preparat)) + 
         geom_step() + xlab("Days")
     )
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
     medians_charcs()
   }
   )

})