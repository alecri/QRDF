library(shiny)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(plotly)
library(survival)

shinyServer(function(input, output){
   
   ## for subdiagnosis
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
   
   n_ts <- reactive({
      group_bylist <- if (input$compare != "none"){
         c(input$time_unit, input$compare)
      } else {
         c(input$time_unit)
      }

      basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#year(inkluderad) >= 1999,
            inkluderad >= input$drange[1] & inkluderad <= input$drange[2],
                (input$diagnos == "All" | (diagnos_kategori1 %in% input$diagnos &
                                           diagnos_1 %in% input$sub_diag))
            ,
            (input$diagnos != "Reumatoid artrit och reumatoid artrit med underdiagnoser" |
               !as.logical(input$tidig_ra*(tidig_ra == 0)))
            ) %>%
         ## it's maybe better to create this variable in global.R
         mutate(year = floor_date(inkluderad, "year"),
                month = floor_date(inkluderad, "month")) %>%
         group_by_(.dots = group_bylist) %>%
         summarize(number = n())
   })
   
   n_ts_besok <- reactive({
      besok_basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#year(inkluderad) >= 1999,
                datum >= input$drange_besok[1] & datum <= input$drange_besok[2],
                (input$diagnos_besok == "All" | (diagnos_kategori1 %in% input$diagnos_besok &
                                              diagnos_1 %in% input$sub_diag_besok))) %>%
         ## it's maybe better to create this variable in global.R
         mutate(year = floor_date(datum, "year"),
                month = floor_date(datum, "month")) %>%
         group_by_(input$time_unit_besok) %>%
         summarize(number = n())
   })
   
   n_ts_bio <- reactive({
      n_ts_bio <- terapi_basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(#ar >= 1999,
           ordinerat >= input$drange_bio[1] & ordinerat <= input$drange_bio[2],
                (input$diagnos_bio == "All" | (diagnos_kategori1 %in% input$diagnos_bio &
                                                  diagnos_1.y %in% input$sub_diag_bio)),
                (input$ongoing == FALSE | pagaende == 1)) %>%
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
      n_ts_bio
   })
   
   ## rendering table
   output$table <- renderDataTable({
      n_ts()
   })
   output$table_besok <- renderDataTable({
      n_ts_besok()
   })
   output$table_bio <- renderDataTable({
      n_ts_bio()
   })
   
   ## rendering plot
   output$tsplot <- renderDygraph({
      
      # color <- if (input$comp_region){
      #    "region"
      # } else {
      #    NULL
      # }
      # n_ts() %>%
      #    ggplot(aes_string(x = input$time_unit, y = "number", color = color)) +
      #    geom_line() + theme_bw() + ylab("Number of patients")
     
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
      dataxts <- as.xts(n_ts_besok(), order.by = n_ts_besok()[[input$time_unit_besok]])
      dygraph(dataxts) %>% dyLegend(width = 700) %>%
        dyRangeSelector(dateWindow = input$drange_besok)
   })
      
   output$tsplot_bio <- renderDygraph({
      dataxts <- if (input$biologic == "All"){
         as.xts(n_ts_bio()[, "total"], order.by = n_ts_bio()[[input$time_unit_bio]])
      } else {
         varlist <- if (input$biologic != "All"){
            if (input$showall == TRUE){
               c("number", "total")
            } else {
               c("number")
            }
         }
         as.xts(n_ts_bio()[, varlist], order.by = n_ts_bio()[[input$time_unit_bio]])
      }
      dygraph(dataxts) %>% dyLegend(width = 700) %>%
        dyRangeSelector(dateWindow = input$drange_bio)
   })
   
   surv.data <- reactive({
     bio_km <- terapi_basdata %>%
         filter(
           time > 0,
           ordinerat >= input$drange_km[1] & ordinerat <= input$drange_km[2],
           (input$line_km == 0 | line_trt == input$line_km | (input$line_km == 3 & line_trt >= 3)),
           (input$region_km == "All" | region == input$region_km),
           (input$diagnos_km == "All" | dxcat == input$diagnos_km)
           )
     
     surv.data <- data.frame()
     surv.data <-rbind(
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
   
   output$table_KM <- renderDataTable({
     surv.data()
   })
   
   output$KM <- renderPlotly({
     ggplotly(
       ggplot(surv.data(), aes(x = time, y = surv, col = preparat)) + 
         geom_step() 
       #+geom_step(aes(y = lower), linetype = "dotted") +
       #geom_step(aes(y = upper), linetype = "dotted")
     )
   })

      
   
   

})