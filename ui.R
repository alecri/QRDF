library(shiny)
library(plotly)
library(dygraphs)


load("www/QRDF_shiny.Rdata")

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
            selectInput("diagnos", label = "Diagnos",
                           choices = c("All", levels(basdata$diagnos_kategori1)), multiple = FALSE,
                           selected = NULL),
            uiOutput("diagnos_1"),
            checkboxInput("comp_region", "Compare regions", FALSE)
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
     "Visits"
   ),
   
   tabPanel(
     "Bio Compounds",

     sidebarLayout(
        sidebarPanel(
           selectInput("biologic", label = "Biologic",
                       choices = c("All", levels(terapi$preparat)), selected = "All"),
           selectInput("time_unit_bio", label = "Time unit",
                       choices = list("month", "year"),
                       selected = "month"),
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
   )
   )
)