library(shiny)
library(plotly)
library(dygraphs)
library(lubridate)

shinyUI(
  navbarPage(
    "QRDF",
    
    tabPanel(
      "Home",
      h2("Welcome to the Quality Register Drug Follow up of the Swedish Rheumatology Quality Registers"),
      br(),
      p("Swedish Rheumatology Quality Registers (SRQ) comprise a cooperation 
          between clinical registers in Swedish rheumatology participating in a 
          national program for continuous follow up of patients for drug surveillance, clinical trials, health economics and clinical research and for health care quality improvement. SRQ works together with research driven pharmaceutical companies to improve patient health. To enable planning and design of specific research projects the character of the patient cohorts to be found in the SRQ database is displayed here. Data in SRQ is owned by the participating registers that perform the analyses in agreed studies aiming for scientific publication of the results.
        
        The data content overview displayed here comprises encoded data such as the register coverage in the population, total numbers of patients and visits, and numbers and type of patients treated with the class of immuno-modulating drugs in question and for each company such data on their own product(s)")
      ),
      br(),
      p("The QRDF contains now both bDMARD (biologics) and tsDMARD (small molecules). 
        Therefore, every time that the total number of biologics are described, also Otezla (apremilast) is included."),

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
          selectInput("sex", label = "Sex",
                      choices = c("All", levels(basdata$kon)), selected = "All"),
          sliderInput("age", "Age:", 
                      min = 0, max = 110, value = c(18, 80), step= 1),
          selectInput("region", label = "Region",
                      choices = c("All", levels(basdata$region)), selected = "All")
        ),
        
        mainPanel(
          # Hide errors. Uncomment in the final version
          # tags$style(type="text/css",
          #            ".shiny-output-error { visibility: hidden; }",
          #            ".shiny-output-error:before { visibility: hidden; }"),
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", dygraphOutput("tsplot")), 
                      tabPanel("Table", 
                               p(" "),
                               downloadButton('downloadTab', 'Download table'),
                               hr(),
                               dataTableOutput("table")),
                      tabPanel("Summary", 
                               p(""),
                               p("Number of patients registered in the Swedish Rheumatology Register by month. Possibility to have the numbers by year."),
                               p("Patients can be classified according to their diagnosis: the groups and the single diagnoses in the list are the same that the doctor can chose when registering a patient"),
                               p("In the graph there is the possibility of comparing patients by gender, region of treatment and age (18-65 vs. 65+)")
                               
                      )
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
          selectInput("sex_besok", label = "Sex",
                      choices = c("All", levels(besok_basdata$kon)), selected = "All"),
          sliderInput("age_besok", "Age:", 
                      min = 0, max = 110, value = c(18, 80), step= 1),
          selectInput("region_besok", label = "Region",
                      choices = c("All", levels(besok_basdata$region)), selected = "All")
        ),
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", dygraphOutput("tsplot_besok")),
                      tabPanel("Table", 
                               p(" "),
                               downloadButton('downloadTab_besok', 'Download table'),
                               hr(),
                               dataTableOutput("table_besok")),
                      tabPanel("Summary",
                               p("Some text"))
          )
        )
      )),
    
    tabPanel(
      "Biologics",
      
      sidebarLayout(
        sidebarPanel(
          selectInput("biologic", label = "Biologic",
                      choices = c("All", levels(terapi$preparat)), selected = "All"),
          selectInput("time_unit_bio", label = "Time unit",
                      choices = list("month", "year"),
                      selected = "month"),
          h2("Subset"), 
          uiOutput("slideDate_bio"),
          radioButtons("ongoing", "Number of",
                       c("started treatments"= FALSE, "ongoing treatments"= TRUE)
          ),
          br(),
          selectInput("diagnos_bio", label = "Diagnos",
                      choices = c("All", levels(terapi_basdata$diagnos_kategori1)), multiple = FALSE,
                      selected = NULL),
          conditionalPanel("input.diagnos_bio == 'Reumatoid artrit och reumatoid artrit med underdiagnoser'",
                           checkboxInput("tidig_ra_bio", "Only early RA", FALSE)),
          uiOutput("diagnos_1_bio"),
          conditionalPanel("input.biologic != 'All'", checkboxInput("showall", "Show Total", FALSE)),
          selectInput("line_bio", label = "Line treatment",
                      choices = c("All" = 0, "1" = 1, "2" = 2, "3+" = 3), selected = 0),
          selectInput("sex_bio", label = "Sex",
                      choices = c("All", levels(terapi$kon)), selected = "All"),
          sliderInput("age_bio", "Age:", 
                      min = 0, max = 110, value = c(18, 80), step= 1),
          selectInput("region_bio", label = "Region",
                      choices = c("All", levels(terapi_basdata$region)), selected = "All")
        ),
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", dygraphOutput("tsplot_bio")),
                      tabPanel("Table", 
                               p(" "),
                               downloadButton('downloadTab_bio', 'Download table'),
                               hr(),
                               dataTableOutput("table_bio")),
                      tabPanel("Big table", 
                               p(" "),
                               #downloadButton('downloadTab_bio', 'Download table'),
                               hr(),
                               dataTableOutput("tableBig_bio")),
                      tabPanel("Summary",
                               p("Some text"))
          )
        )
      )),
    
    
    tabPanel(
      "Disease characteristics",
      sidebarLayout(
        sidebarPanel(
          selectInput("biologic_charcs", label = "Biologic",
                      choices = c("", levels(terapi$preparat)), selected = ""),
          uiOutput("slideDate_charcs"),
          # dateRangeInput("drange_charcs",
          #                label = "Range limit",
          #                start = "1999-01-01", end = Sys.Date()),
          selectInput("diagnos_charcs", label = "Diagnos",
                      choices = c("All", levels(basdata$dxcat)), selected = "All"),
          selectInput("sex_charcs", label = "Sex",
                      choices = c("All", levels(terapi$kon)), selected = "All"),
          sliderInput("age_charcs", "Age:", 
                      min = 0, max = 110, value = c(18, 80), step= 1),
          selectInput("region_char_cs", label = "Region",
                      choices = c("All", levels(basdata$region)), selected = "All"),
          ## Duration of disease,
          selectInput("time_anal", "Time of analysis",
                      choices = c("Treatment start" = 0, "Follow-up 3-8 months" = 150, 
                                  "Follow-up 9-16 months" = 365, "Follow-up 17-24 months" = 600, 
                                  "Follow-up 25-36 months" = 912, "Follow-up >36 months" = 1460), selected = 0),
          radioButtons("median_charcs", "Show median together with:",
                       choices = c("Interquartile range" = "iqr", "No. of non missing"))
        ),
        
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Table", #htmlOutput("table_charcs")),
                               p(" "),
                               downloadButton('downloadTab_charcs', 'Download table'),
                               hr(),
                               dataTableOutput("table_charcs")),
                      tabPanel("Summary",
                               p("Some text"))
                      #                      ,
                      #                      tabPanel("Check1",
                      #                               dataTableOutput("table_charcs_all")
                      #                      ),
                      #                      tabPanel("Check2",
                      #                               dataTableOutput("table_median")
                      #                      )
          )
        )
      )),
    
    tabPanel(
      "KM",
      sidebarLayout(
        sidebarPanel(
          selectInput("biologic_km", label = "Biologic",
                      choices = c("All", levels(terapi$preparat)), selected = "All"),
          selectInput("line_km", label = "Line treatment",
                      choices = c("All" = 0, "1" = 1, "2" = 2, "3+" = 3), selected = 0),
          uiOutput("slideDate_km"),
          selectInput("region_km", label = "Region",
                      choices = c("All", levels(basdata$region)), selected = "All"),
          selectInput("diagnos_km", label = "Diagnos",
                      choices = c("All", levels(basdata$dxcat)), selected = "All"),
          selectInput("sex_km", label = "Sex",
                      choices = c("All", levels(basdata$kon)), selected = "All"),
          sliderInput("age_km", "Age:", 
                      min = 0, max = 110, value = c(18, 80), step= 1)
        ),
        
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", plotlyOutput("KM")),
                      tabPanel("Table",  
                               p(" "),
                               downloadButton('downloadTab_km', 'Download table'),
                               hr(),
                               dataTableOutput("table_KM"))
          )
        )
      ))
    
      )
  )