library(markdown)
library(leaflet)

staff_rating <- read.csv("staff_rating.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")

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
                      tabPanel("Birthing Friendly Hospitals",
                               fluidRow(column(12, leafletOutput("worldMap", height = "600px")))
                      ),
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
                            ),
                      tabPanel("Surgery Centers", 
                               fluidRow(column(12, leafletOutput("SurgMap", height = "600px"))),
                               hr()
                               )
           ),
          #RISK FACTORS
            tabPanel("Risk Factors",
                    tabsetPanel(
                      tabPanel("Risk Factors",
                      # Step 1: Guided inputs
                      fluidRow(
                        column(4, selectInput("state_hai", "What state are you in?",
                                              choices = c("All", sort(unique(hai_cleaned$State))),
                                              selected = "All")),
                        column(4, selectInput("facility_hai", "Select a Hospital:",
                                              choices = c("Select a hospital..." = ""),
                                              selected = "")),
                        column(4, selectInput("care_type", "What type of care are you seeking?",
                                              choices = c("General",
                                                          "Surgery",
                                                          "Maternity",
                                                          "Emergency")))
                      ),
                      
                      hr(),
                      
                      # Step 2: Hospital card (renders from server once facility is chosen)
                      uiOutput("hospital_card"),
                      
                      # Step 3: Compare nudge (renders from server once facility is chosen)
                      uiOutput("compare_nudge")
                    ),
                      tabPanel("Compare Hospitals",
                               br(),
                               fluidRow(
                                 column(3, selectInput("state_hai_compare", "Filter by State:",
                                                       choices = c("All", sort(unique(hai_cleaned$State))),
                                                       selected = "All")),
                                 column(9, checkboxGroupInput("infection_type_compare", "Select Infection Types:",
                                                              choices = c("Central Line Infection",
                                                                          "Urinary Tract Infection",
                                                                          "Surgical Site - Colon",
                                                                          "Surgical Site - Hysterectomy",
                                                                          "MRSA Blood Infection",
                                                                          "C. Difficile Infection"),
                                                              selected = c("Central Line Infection",
                                                                           "Urinary Tract Infection",
                                                                           "MRSA Blood Infection",
                                                                           "C. Difficile Infection"),
                                                              inline = TRUE))
                               ),
                               hr(),
                               fluidRow(column(12, plotOutput("hai_compare_chart", height = "500px")))
                      )
                    )
           ),
           navbarMenu("More",
                      tabPanel("Table", DT::dataTableOutput("table")),
                      tabPanel("About")
           )
)