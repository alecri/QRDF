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
            uiOutput("slideDate"),
            selectInput("diagnos", label = "Diagnos",
                           choices = c("All", levels(basdata$diagnos_kategori1)), multiple = FALSE,
                           selected = NULL),
            conditionalPanel("input.diagnos == 'Reumatoid artrit och reumatoid artrit med underdiagnoser'",
                             checkboxInput("tidig_ra", "Only early RA", FALSE)),
            uiOutput("diagnos_1"),
            selectInput("compare", label = "Comparison by",
                        choices = list("none", "region", "kon", "age_inclusion_cat"),
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
           uiOutput("slideDate_besok"),
           selectInput("diagnos_besok", label = "Diagnos",
                       choices = c("All", levels(besok_basdata$diagnos_kategori1)), multiple = FALSE,
                       selected = NULL),
           conditionalPanel("input.diagnos_besok == 'Reumatoid artrit och reumatoid artrit med underdiagnoser'",
                            checkboxInput("tidig_ra_besok", "Only early RA", FALSE)),
           uiOutput("diagnos_1_besok"),
           selectInput("compare_besok", label = "Comparison by",
                       choices = list("none", "region", "kon", "age_visit_cat"),
                       selected = "none")
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
           uiOutput("slideDate_bio"),
           radioButtons("ongoing", "On going",
                        c("All"= FALSE, "Only on going"= TRUE)
           ),
           selectInput("diagnos_bio", label = "Diagnos",
                       choices = c("All", levels(terapi_basdata$diagnos_kategori1)), multiple = FALSE,
                       selected = NULL),
           conditionalPanel("input.diagnos_bio == 'Reumatoid artrit och reumatoid artrit med underdiagnoser'",
                            checkboxInput("tidig_ra_bio", "Only early RA", FALSE)),
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
         dateRangeInput("drange_km",
                        label = "Range limit",
                        start = "1999-01-01", end = Sys.Date()),
         selectInput("region_km", label = "Region",
                     choices = c("All", levels(basdata$region)), selected = "All"),
         selectInput("diagnos_km", label = "Diagnos",
                     choices = c("All", as.character(unique(basdata$dxcat))), selected = "All")
       ),
       
       
       mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Plot", plotlyOutput("KM")),
                     tabPanel("Table", dataTableOutput("table_KM"))
                     )
       )
     )),
   
   tabPanel(
     "Disease characteristics",
     sidebarLayout(
       sidebarPanel(
         selectInput("biologic_charcs", label = "Biologic",
                     choices = c("", levels(terapi$preparat)), selected = ""),
         dateRangeInput("drange_charcs",
                        label = "Range limit",
                        start = "1999-01-01", end = Sys.Date()),
         selectInput("diagnos_charcs", label = "Diagnos",
                     choices = c("All", as.character(unique(basdata$dxcat))), selected = "All"),
         ## Time of analysis,
         selectInput("sex_charcs", label = "Sex",
                     choices = c("All", levels(terapi$kon)), selected = "All"),
         selectInput("age_cat_charcs", label = "Age",
                     choices = c("All", "[18, 65)", "[65+"), selected = "All"),
         selectInput("region_char_cs", label = "Region",
                     choices = c("All", levels(basdata$region)), selected = "All"),
         ## Duration of disease,
         radioButtons("median_charcs", "Show median together with:",
                      choices = c("Interquartile range" = "iqr", "No. of non missing"))
       ),
       
       
       mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Table", dataTableOutput("table_charcs")),
                     tabPanel("Summary",
                              p("Some text"))
         )
       )
     ))
   )
)