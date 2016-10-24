library(shiny)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(plotly)


load("www/QRDF_shiny.Rdata")

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
   output$diagnos_1_bio <- renderUI({
      if (is.null(input$diagnos)){
         return(NULL)
      } else {
         if (input$diagnos_bio == "All") return(NULL)
         choices = as.character(unique(terapi_basdata$diagnos_1.y[terapi_basdata$diagnos_kategori1 == input$diagnos_bio]))
         selectizeInput("sub_diag_bio", label = "Sub diagnosis",
                        choices = choices, multiple = TRUE, selected = choices)
      }
   })
   
   n_ts <- reactive({
      group_bylist <- if (input$comp_region){
         c(input$time_unit, "region")
      } else {
         c(input$time_unit)
      }

      basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(year(inkluderad) >= 1999,
                (input$diagnos == "All" | (diagnos_kategori1 %in% input$diagnos &
                                           diagnos_1 %in% input$sub_diag))) %>%
         ## it's maybe better to create this variable in global.R
         mutate(year = floor_date(inkluderad, "year"),
                month = floor_date(inkluderad, "month")) %>%
         group_by_(.dots = group_bylist) %>%
         summarize(number = n())
   })
   
   n_ts_bio <- reactive({
      n_ts_bio <- terapi_basdata %>%
         ## exclude years before 1999 + interactive filtering
         filter(ar >= 1999,
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
     
     dataxts <- if (input$comp_region){
       nts <- spread(n_ts(), region, number)
       as.xts(nts, order.by = nts[[input$time_unit]])
     } else {
       as.xts(n_ts(), order.by = n_ts()[[input$time_unit]])
     }
     dygraph(dataxts) %>% dyLegend(width = 700)
     
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
      dygraph(dataxts) %>% dyLegend(width = 700)
   })
   

})