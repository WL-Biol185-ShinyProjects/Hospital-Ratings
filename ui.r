library(markdown)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(DT)
library(bslib)

directory <- read.csv("directory.csv", check.names = FALSE)
staff_rating <- read.csv("staff_rating.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
SurgCenters <- read.csv("SurgCenters.csv",check.names = FALSE)
readmission <- read.csv("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv", check.names = FALSE)
readmission_clean <- readmission %>%
  select(-`Facility ID`, -`Footnote`, -`Measure Name`, 
         -`Excess Readmission Ratio`, -`Expected Readmission Rate`,
         -`Start Date`, -`End Date`) %>%
  mutate(
    `Number of Readmissions` = as.numeric(`Number of Readmissions`),
    `Number of Discharges`   = as.numeric(`Number of Discharges`),
    `Predicted Readmission Rate` = as.numeric(`Predicted Readmission Rate`)
  )

df_combined <- readmission_clean %>%
  group_by(`Facility Name`, State) %>%
  summarise(
    `Total Readmissions` = sum(`Number of Readmissions`, na.rm = TRUE),
    `Total Discharges`   = sum(`Number of Discharges`, na.rm = TRUE),
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE)
  ) %>%
  ungroup() 


navbarPage("Hospital Ratings",
           theme = bslib::bs_theme(bootswatch = "lumen"),
           
           # DIRECTORY MAP---------------
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
           
           tabPanel("Hospital Directory",
                    fluidRow(
                      column(12,
                             div(
                               style = "background:#e8f0fe; border:1px solid #8ab4f8; border-radius:8px;
                 padding:12px 20px; margin-bottom:15px;",
                               tags$b("ℹ️ About This Directory"),
                               tags$p(
                                 "This directory provides a comprehensive list of hospitals and healthcare facilities 
           across the United States. Use the map to explore facilities by location, or filter 
           by state below to find facilities near you.",
                                 style = "margin:6px 0 0 0; color:#555; font-size:13px;"
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(12,
                             div(
                               style = "height: 600px; width: 100%;",
                               leafletOutput("directoryMap", height = "600px", width = "100%")
                             )
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(12,
                             h3("Hospitals by State"),
                             selectInput("state_dir", "Filter by State:",
                                         choices = c("All", sort(unique(directory$State))),
                                         selected = "All"),
                             uiOutput("directory_cards")
                      )
                    )
           ),
           
#-----------end directory page-------------------------
           
           tabPanel("Star Ratings",
                    verbatimTextOutput("summary")
           ),
           
           tabPanel("Staff & Communication",
                    tabsetPanel(
                      tabPanel("Patient Experience",
                               fluidRow(
                                 column(3, selectInput("state_staff", "Filter by State:",
                                                       choices = c("All", sort(unique(staff_rating$State))),
                                                       selected = "All")),
                                 column(9, checkboxGroupInput("measure", "Select Measures:",
                                                              choices = c(
                                                                "Staff Care Quality" = "staff",
                                                                "Communication"      = "communication",
                                                                "Facility Rating"    = "rating",
                                                                "Recommendation"     = "recommend"
                                                              ),
                                                              selected = c("staff", "communication", "rating", "recommend"),
                                                              inline = TRUE))
                               ),
                               hr(),
                               fluidRow(column(12, plotOutput("staff_chart", height = "400px"))),
                               hr(),
                               fluidRow(column(12, tableOutput("staff_table")))
                      ),
                      
                      tabPanel("Engagement Scores",
                               fluidRow(
                                 column(3, selectInput("state_hvbp", "Filter by State:",
                                                       choices = NULL,
                                                       selected = "All")),
                                 column(3, selectInput("facility_hvbp", "Look Up a Hospital:",
                                                       choices = c("Select a hospital..." = ""),
                                                       selected = ""))
                               )
                      )
                      ),
                      hr(),
                      uiOutput("hvbp_hospital_card")
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
                               fluidRow(
                                 column(12,
                                        div(
                                          style = "background:#e8f0fe; border:1px solid #8ab4f8; border-radius:8px;
                      padding:12px 20px; margin-bottom:15px;",
                                          tags$b("\u2139\ufe0f What is a VA Hospital?"),
                                          tags$p(
                                            "VA Hospitals are medical facilities operated by the United States Department of Veterans Affairs.
               They provide comprehensive healthcare services to eligible veterans, including primary care,
               specialty care, mental health services, and rehabilitation programs.",
                                            style = "margin:6px 0 0 0; color:#555; font-size:13px;"
                                          )
                                        )
                                 )
                               ),
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
                               fluidRow(
                                 column(12,
                                        div(
                                          style = "background:#e8f0fe; border:1px solid #8ab4f8; border-radius:8px;
                                                  padding:12px 20px; margin-bottom:15px;",
                                          tags$b("ℹ️ What is a Surgery Center?"),
                                          tags$p(
                                            "Surgery centers are outpatient facilities where patients receive surgical care 
                                             and are discharged the same day. They specialize in a wide range of procedures 
                                             including orthopedic, ophthalmologic, gastroenterological, and other surgeries.",
                                            style = "margin:6px 0 0 0; color:#555; font-size:13px;"
                                          )
                                        )
                                 )
                               ),
                               fluidRow(column(12, leafletOutput("SurgMap", height = "600px"))),
                               hr(),
                               # ⬇️ CHANGED - filter and cards now together inside one column(12), with h3 header added
                               fluidRow(
                                 column(12,
                                        h3("Surgery Centers by State"),        # ⬅️ ADDED
                                        selectInput("state_surg", "Filter by State:",
                                                    choices = c("All", sort(unique(SurgCenters$State))),
                                                    selected = "All"),
                                        uiOutput("surg_cards")                 # ⬅️ moved inside column(12)
                                 )
                               )
                      )
                               # end Surgery Centers
                      
           ),
     
           # end Specialty Care
           
           # --- RISK FACTORS ---
           tabPanel("Risk Factors",
                    tabsetPanel(
                      id = "risk_tabs",
                      
                      tabPanel("Find My Hospital",
                               br(),
                               fluidRow(
                                 column(6, selectInput("state_hai", "What state are you in?",
                                                       choices = c("Select a state..." = "", sort(unique(hai_cleaned$State))),
                                                       selected = "")),
                                 column(6, uiOutput("facility_dropdown")),

                               ),
                               hr(),
                               uiOutput("hospital_card"),
                               uiOutput("compare_nudge")
                      ),# end Find My Hospital

                      
                      tabPanel("Compare Hospitals",
                               br(),
                               fluidRow(
                                 column(3, selectInput("state_hai_compare",
                                                       "Filter by State:",
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
                                 ))
                               ),
                               hr(),
                               # Gauge grid: 3 gauges side by side
                               # Dynamic gauges grid
                               uiOutput("gauge_grid")
                      
                      ), # end Compare Hospitals
                      
      # ---- New Readmission Risks tab ----
      tabPanel("Readmission Risks",
               br(),
               fluidRow(
                 column(6, selectInput("state_readmission", "Filter by State:", 
                                       choices = c("All", sort(unique(df_combined$State))),
                                       selected = "All")),
                 column(6, uiOutput("facility_readmission_dropdown"))
               ),
               hr(),
               DT::dataTableOutput("readmission_table")
      )
                     
                    ), # end tabsetPanel
                    ),
       
navbarMenu("More",
           tabPanel("Table", DT::dataTableOutput("table")),
           tabPanel("About")
  )
 ) # end navbarPage

