library(markdown)
library(leaflet)
library(dplyr)
library(echarts4r)  # ( need this for gauges)
library(shinyWidgets)
library(DT)
library(bslib)

staff_rating <- read.csv("staff_rating.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")

navbarPage("Hospital Ratings",
           theme = bslib::bs_theme(bootswatch = "lumen"),
           
           tabPanel("Directory Map",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l"))
                      ),
                      mainPanel(
                        plotOutput("plot")
                      )
                    )
           ),
           
           tabPanel("Star Ratings",
                    verbatimTextOutput("summary")
           ),
           
           tabPanel("Staff & Communication",
                    fluidRow(
                      column(3, selectInput("state_staff", "Filter by State:",
                                            choices = c("All", sort(unique(staff_rating$State))),
                                            selected = "All")),
                      column(3, selectInput("facility_staff", "Choose Facility:",
                                            choices = c("All", sort(unique(staff_rating$`Facility Name`))),
                                            selected = "All")),
                      column(6, checkboxGroupInput("measure", "Select Measures:",
                                                   choices = c(
                                                     "Staff Care Quality" = "staff",
                                                     "Communication" = "communication",
                                                     "Facility Rating" = "rating",
                                                     "Recommendation" = "recommend"
                                                   ),
                                                   selected = c("staff", "communication", "rating", "recommend"),
                                                   inline = TRUE))
                    ),
                    hr(),
                    fluidRow(
                      column(12, plotOutput("staff_chart", height = "500px"))
                    ),
                    hr(),
                    fluidRow(
                      column(12, tableOutput("staff_table"))
                    )
           ),
           
           navbarMenu("Specialty Care",
                      
                      # --- BIRTHING FRIENDLY HOSPITALS ---
                      tabPanel("Birthing Friendly Hospitals",
                               
                               # Summary stat bar
                               fluidRow(
                                 column(12,
                                        div(
                                          style = "background:#f8f9fa; border-radius:8px; padding:12px 20px;
                                                   margin-bottom:15px; border-left:4px solid #e91e8c;",
                                          uiOutput("birthing_summary_bar")
                                        )
                                 )
                               ),
                               
                               # Info box
                               fluidRow(
                                 column(12,
                                        div(
                                          style = "background:#fff3f8; border:1px solid #f8bbd0; border-radius:8px;
                                                   padding:12px 20px; margin-bottom:15px;",
                                          tags$b("\u2139\ufe0f What is a Birthing Friendly Hospital?"),
                                          tags$p(
                                            "The 'Birthing Friendly' designation is awarded by the Centers for Medicare &
                                            Medicaid Services (CMS) to hospitals that have adopted safe and recommended
                                            maternity care practices, support breastfeeding, and provide high-quality
                                            care for mothers and newborns.",
                                            style = "margin:6px 0 0 0; color:#555; font-size:13px;"
                                          )
                                        )
                                 )
                               ),
                               
                               # Map
                               fluidRow(column(12, leafletOutput("worldMap", height = "500px"))),
                               
                               hr(),
                               
                               # State filter
                               fluidRow(
                                 column(3,
                                        selectInput("birthing_state", "Filter by State:",
                                                    choices = c("All", sort(unique(birthing$state))),
                                                    selected = "All")
                                 )
                               ),
                               
                               # Hospital cards
                               fluidRow(
                                 column(12, uiOutput("birthing_cards"))
                               )
                      ), # end Birthing Friendly Hospitals
                      
                      # --- VETERAN INPATIENT PSYCHIATRIC FACILITIES ---
                      tabPanel("Veteran Inpatient Psychiatric Facilities",
                               fluidRow(column(12, leafletOutput("vaMap", height = "600px"))),
                               hr(),
                               fluidRow(
                                 column(12,
                                        h3("VA Inpatient Psychiatric Facility Hotlines"),
                                        selectInput("hotline_state", "Filter by State:",
                                                    choices = c("All", sort(unique(VA_IPF_geocoded$State))),
                                                    selected = "All"),
                                        uiOutput("hotlineCards")
                                 )
                               )
                      ), # end VA tab
                      
                      # --- SURGERY CENTERS ---
                      tabPanel("Surgery Centers",
                               fluidRow(column(12, leafletOutput("SurgMap", height = "600px"))),
                               hr()
                      ) # end Surgery Centers
                      
           ), # end Specialty Care
           
           # --- RISK FACTORS ---
           tabPanel("Risk Factors",
                    tabsetPanel(
                      id = "risk_tabs",
                      
                      tabPanel("Find My Hospital",
                               br(),
                               fluidRow(
                                 column(4, selectInput("state_hai", "What state are you in?",
                                                       choices = c("All", sort(unique(hai_cleaned$State))),
                                                       selected = "All")),
                                 column(4, selectInput("facility_hai", "Select a Hospital:",
                                                       choices = c("Select a hospital..." = ""),
                                                       selected = "")),
                                 column(4, selectInput("care_type", "What type of care are you seeking?",
                                                       choices = c("General", "Surgery", "Maternity", "Emergency")))
                               ),
                               hr(),
                               uiOutput("hospital_card"),
                               uiOutput("compare_nudge")
                      ), # end Find My Hospital
                      
                      tabPanel("Compare Hospitals",
                               br(),
                               fluidRow(
                                 column(3, selectInput("state_hai_compare", "Filter by State:",
                                                       choices = c("All", sort(unique(hai_cleaned$State))),
                                                       selected = "All")),
                                 column(9, shinyWidgets::checkboxGroupButtons(
                                   inputId = "infection_type_compare",
                                   label = "Select Infection Types:",
                                   choices = c(
                                     "Central Line Infection",
                                     "Urinary Tract Infection",
                                     "Surgical Site - Colon",
                                     "Surgical Site - Hysterectomy",
                                     "MRSA Blood Infection",
                                     "C. Difficile Infection"
                                   ),
                                   selected = c(
                                     "Central Line Infection",
                                     "Urinary Tract Infection",
                                     "MRSA Blood Infection",
                                     "C. Difficile Infection"
                                   ),
                                   justified = TRUE,
                                   checkIcon = list(yes = icon("check"))
                                 )),
                               hr(),
                               fluidRow(column(12, uiOutput("gauge_grid")))
                      ) # end Compare Hospitals
                      
                    ) # end tabsetPanel
           ), # end Risk Factors
           
           navbarMenu("More",
                      tabPanel("Table", DT::dataTableOutput("table")),
                      tabPanel("About")
                    )
           )
) # end navbarPage
