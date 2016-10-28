library(shiny)
library(plotly)
library(dygraphs)
library(lubridate)

shinyUI(
  navbarPage(
    "QRDF",
    
    tabPanel(
      "Patients",
      
      sidebarLayout(
         sidebarPanel(
            selectInput("time_unit", label = "Time unit", 
                        choices = list("month", "year"), 
                        selected = "month"),
            h2("Subset"), 
            sliderInput("drange", "Range limit", min = as.Date("1999-01-01") , max = max(basdata$inkluderad, na.rm = T),
                        value = c(as.Date("1999-01-01"), max(basdata$inkluderad, na.rm = T))),
            selectInput("diagnos", label = "Diagnos",
                           choices = c("All", levels(basdata$diagnos_kategori1)), multiple = FALSE,
                           selected = NULL),
            uiOutput("diagnos_1"),
            selectInput("compare", label = "Comparison by",
                        choices = list("none", "region", "kon", "tidig_ra"),
                        selected = "none")
         ),
         
         mainPanel(
            ## Hide errors. Uncomment in the final version
            # tags$style(type="text/css",
            #            ".shiny-output-error { visibility: hidden; }",
            #            ".shiny-output-error:before { visibility: hidden; }"),
            tabsetPanel(type = "tabs", 
                        tabPanel("Plot", dygraphOutput("tsplot")), 
                        tabPanel("Table", dataTableOutput("table")),
                        tabPanel("Summary", 
                                 p("Some text"))
            )
         )
      )
   ),
  
   
   tabPanel(
     "Visits",
     
     sidebarLayout(
        sidebarPanel(
           selectInput("time_unit_besok", label = "Time unit",
                       choices = list("month", "year"),
                       selected = "month"),
           h2("Subset"), 
           sliderInput("drange_besok", "Range limit", min = as.Date("1999-01-01") , max = max(basdata$inkluderad, na.rm = T),
                       value = c(as.Date("1999-01-01"), max(basdata$inkluderad, na.rm = T))),
           selectInput("diagnos_besok", label = "Diagnos",
                       choices = c("All", levels(besok_basdata$diagnos_kategori1)), multiple = FALSE,
                       selected = NULL),
           uiOutput("diagnos_1_besok")
        ),
        
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", dygraphOutput("tsplot_besok")),
                       tabPanel("Table", dataTableOutput("table_besok")),
                       tabPanel("Summary",
                                p("Some text"))
           )
        )
     )),
   
   tabPanel(
     "Bio Compounds",

     sidebarLayout(
        sidebarPanel(
           selectInput("biologic", label = "Biologic",
                       choices = c("All", levels(terapi$preparat)), selected = "All"),
           selectInput("time_unit_bio", label = "Time unit",
                       choices = list("month", "year"),
                       selected = "month"),
           h2("Subset"), 
           sliderInput("drange_bio", "Range limit", min = as.Date("1999-01-01") , max = max(basdata$inkluderad, na.rm = T),
                       value = c(as.Date("1999-01-01"), max(basdata$inkluderad, na.rm = T))),
           radioButtons("ongoing", "On going",
                        c("All"= FALSE, "Only on going"= TRUE)
           ),
           selectInput("diagnos_bio", label = "Diagnos",
                       choices = c("All", levels(terapi_basdata$diagnos_kategori1)), multiple = FALSE,
                       selected = NULL),
           uiOutput("diagnos_1_bio"),
           conditionalPanel("input.biologic != 'All'", checkboxInput("showall", "Show Total", TRUE))
        ),

        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", dygraphOutput("tsplot_bio")),
                       tabPanel("Table", dataTableOutput("table_bio")),
                       tabPanel("Summary",
                                p("Some text"))
           )
           )
   )),
   
   tabPanel(
     "Summary"
   ),
   
   tabPanel(
     "KM",
     sidebarLayout(
       sidebarPanel(
         selectInput("biologic_km", label = "Biologic",
                     choices = c("All", levels(terapi$preparat)), selected = "All"),
         selectInput("line_km", label = "Line treatment",
                     choices = c("All" = 0, "1" = 1, "2" = 2, "3+" = 3), selected = 0),
         ## Age,
         dateRangeInput("drange_km",
                        label = "Range limit",
                        start = "1999-01-01", end = Sys.Date()),
         selectInput("region_km", label = "Region",
                     choices = c("All", levels(basdata$region)), selected = "All"),
         selectInput("diagnos_km", label = "Diagnos",
                     choices = c("All", unique(basdata$dxcat)), selected = "All")
       ),
       
       
       mainPanel(
         plotlyOutput("KM")
       )
     ))
   )
)