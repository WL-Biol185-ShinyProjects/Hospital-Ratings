library(markdown)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(DT)
library(bslib)
library(tidyverse)

hospitalgen <- read.csv("hosp.general.info.csv", check.names = FALSE)
directory <- read.csv("directory.csv", check.names = FALSE)
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
  ) %>%
  filter(
    !is.na(`Number of Readmissions`),
    !is.na(`Number of Discharges`),
    !is.na(`Predicted Readmission Rate`)
  )
df_combined <- readmission_clean %>%
  group_by(`Facility Name`, State) %>%
  summarise(
    `Total Readmissions` = sum(`Number of Readmissions`, na.rm = TRUE),
    `Total Discharges`   = sum(`Number of Discharges`, na.rm = TRUE),
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE)
  ) %>%
  ungroup() 
staff_rating <- read_csv("ASCQR_OAS_CAHPS_BY_ASC.csv") %>%
  select(-1) %>%
  rename(
    `Facility Name`  = `Facility Name`,
    `State`          = State,
    `Staff Care`     = `Patients who reported that staff definitely gave care in a professional way and the facility was clean`,
    `Communication`  = `Patients who reported that staff definitely communicated about what to expect during and after the procedure`,
    `High Rating`    = `Patients who gave the facility a rating of 9 or 10 on a scale from 0 (lowest) to 10 (highest)`,
    `Would Recommend to Others`= `Patients who reported YES they would DEFINITELY recommend the facility to family or friends`
  ) %>%
  select(`Facility Name`, State, `Staff Care`, `Communication`,
         `High Rating`, `Would Recommend to Others`) %>%
  mutate(across(c(`Staff Care`, `Communication`, `High Rating`, `Would Recommend to Others`),
                ~ suppressWarnings(as.numeric(as.character(.x)))))

navbarPage("Hospital Ratings",
           theme = bslib::bs_theme(bootswatch = "lumen"),
           
           # HOME PAGE ---------------
           tabPanel("Home",
                    fluidRow(
                      column(12,
                             div(
                               style = "text-align: center; padding: 40px 20px;",
                               h1(style = "color: #1a3a5c; font-size: 48px;", "🏥 Hospital Ratings"),
                               p(style = "color: #555; font-size: 18px; max-width: 700px; margin: auto;",
                                 "Explore hospital quality, safety, and patient experience data 
                                  across the United States. This website is designed to help patients make informed decisions on where they choose to receive care.")
                             )
                      )
                    ),
                    hr(),
                    # About section
                    fluidRow(
                      column(12,
                             div(
                               style = "background:#f8f9fa; border-radius:10px; padding:25px; margin:10px;",
                               h3(style = "color:#1a3a5c;", "ℹ️ About This App"),
                               p(style = "color:#555; font-size:14px;",
                                 "This application uses publicly available data from the Centers for Medicare & 
                                  Medicaid Services (CMS) to help patients, researchers, and healthcare professionals 
                                  explore hospital quality metrics across the United States."),
                               p(style = "color:#555; font-size:14px;",
                                 "Data includes star ratings, patient experience scores, infection rates, 
                                  readmission rates, and more.")
                             )
                      )
                    ),
                    # About Us section
                    fluidRow(
                      column(12,
                             div(
                               style = "padding: 30px 10px;",
                               h2(style = "color: #1a3a5c; text-align: center; margin-bottom: 5px;", "About Us"),
                               hr(),
                               
                               # School logo
                               fluidRow(
                                 column(12,
                                        div(style = "text-align: center; margin-bottom: 25px;",
                                            tags$img(src = "logo.png", height = "80px",
                                                     style = "object-fit: contain;")
                                        )
                                 )
                               ),
                               
                               # Project description paragraph
                               fluidRow(
                                 column(8, offset = 2,
                                        div(style = "text-align: center; margin-bottom: 30px;",
                                            p(style = "color: #555; font-size: 14px; line-height: 1.8;",
                                              "Hi! We are undergraduate Neuroscience Students at Washington and Lee University planning to pursue healthcare professions. This project came from our collective interest in public health and was developed as part of a data science course. 
               Our goal was to make hospital quality data more accessible and understandable 
               for the public, to help prospective patients better understand their options for healthcare across the country.")
                                        )
                                 )
                               ),
                               
                               # Team member cards
                               fluidRow(
                                 # Person 1
                                 column(6,
                                        div(style = "background:#fff; border:1px solid #b3d1f7; border-radius:12px; 
                       padding:25px; margin:10px; text-align:center;
                       box-shadow: 2px 2px 8px rgba(0,0,0,0.08);",
                                            tags$img(src = "maren.png", height = "150px", width = "150px",
                                                     style = "border-radius: 50%; object-fit: cover; 
                              border: 3px solid #8ab4f8; margin-bottom: 15px;"),
                                            h4(style = "color: #1a3a5c; margin: 10px 0 5px 0;", "Maren Barclay"),
                                            p(style = "color: #888; font-size: 13px; margin-bottom: 15px;", "Co-Creator / Neuroscience Major, Africana Studies Minor"),
                                            p(style = "color: #555; font-size: 13px; text-align: left; line-height: 1.7;",
                                              "My ultimate career goal is to become a Physician's Assistant or Nurse Practitioner, but I am interested in all types of healthcare. Beyond academics, I love hiking, painting, and spending time with friends!")
                                        )
                                 ),
                                 # Person 2
                                 column(6,
                                        div(style = "background:#fff; border:1px solid #b3d1f7; border-radius:12px; 
                       padding:25px; margin:10px; text-align:center;
                       box-shadow: 2px 2px 8px rgba(0,0,0,0.08);",
                                            tags$img(src = "jojo.jpg", height = "150px", width = "150px",
                                                     style = "border-radius: 50%; object-fit: cover; 
                              border: 3px solid #8ab4f8; margin-bottom: 15px;"),
                                            h4(style = "color: #1a3a5c; margin: 10px 0 5px 0;", "Joanne Lee"),
                                            p(style = "color: #888; font-size: 13px; margin-bottom: 15px;", "Co-Creator / Neuroscience Major, Philosophy Minor"),
                                            p(style = "color: #555; font-size: 13px; text-align: left; line-height: 1.7;",
                                              "In the future I hope to become either a physician or physical therapist! When not studying you can find me running for the W&L track team, spending time doing outdoorsy things, or pranking people.")
                                        )
                                 )
                               )
                             )
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

#----------STAR RATINGS PAGE-----------------           
           tabPanel("Star Ratings",
                    fluidRow(
                      column(
                        3,
                        selectInput("sr_state", "State", choices = c("All", sort(unique(hospitalgen$State)))),
                        selectInput("sr_type", "Hospital Type", choices = c("All", sort(unique(hospitalgen$`Hospital Type`)))),
                        selectInput("sr_ownership", "Hospital Ownership", choices = c("All", sort(unique(hospitalgen$`Hospital Ownership`)))),
                        textInput("sr_search", "Search by Name", value = "")
                      ),
                      column(
                        9,
                        uiOutput("hospital_cards")
                      )
                    )
           ),
           
           tabPanel("Staff & Communication",
                    tabsetPanel(
                      tabPanel("Patient Experience",
                               br(),
                               fluidRow(
                                 column(
                                   4,
                                   selectizeInput(
                                     "state_staff", "Filter by State:",
                                     choices = sort(unique(staff_rating$State)),
                                     selected = NULL,
                                     multiple = TRUE,
                                     options = list(placeholder = "All states")
                                   )
                                 ),
                                 column(
                                   4,
                                   selectizeInput(
                                     "facility_staff", "Filter by Facility:",
                                     choices = NULL,
                                     selected = NULL,
                                     multiple = TRUE,
                                     options = list(placeholder = "All facilities in selected states")
                                   )
                                 ),
                                 column(
                                   4,
                                   actionButton("apply_staff", "Apply Filters", class = "btn-primary")
                                 )
                               ),
                               hr(),
                               tabsetPanel(
                                 tabPanel(
                                   "Chart",
                                   conditionalPanel(
                                     condition = "input.apply_staff > 0",
                                     div(style = "padding-bottom: 20px;",
                                         plotOutput("staff_chart", height = "500px")
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.apply_staff == 0",
                                     div(class = "well", "Choose filters and click Apply Filters to load the chart.")
                                   )
                                 ),
                                 tabPanel(
                                   "Table",
                                   conditionalPanel(
                                     condition = "input.apply_staff > 0",
                                     tableOutput("staff_table")
                                   )
                                 )
                               ),
                               p(
                                 style = "color:#888; font-size:12px; margin-top:8px;",
                                 "⚠️ Scores represent the percentage of surveyed patients who responded positively. When multiple facilities or states are selected, scores are averaged across all surveyed patients in that group. Data sourced from CMS OAS CAHPS Survey (2023–2024)."
                               )
                      ),
                      
                      tabPanel("Engagement Scores",
                               br(),
                               fluidRow(
                                 column(3, selectInput("state_hvbp", "Filter by State:",
                                                       choices = NULL,
                                                       selected = "All")),
                                 column(3, selectInput("facility_hvbp", "Look Up a Hospital:",
                                                       choices = c("Select a hospital..." = ""),
                                                       selected = ""))
                               ),
                               hr(),
                               uiOutput("hvbp_hospital_card")
                      )
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
                                 column(6, uiOutput("facility_dropdown"))

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
                                     "Surgical Site - Colon",
                                     "Surgical Site - Hysterectomy",
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
                               uiOutput("gauge_grid"),
                      ), #end compare hospitals

      tabPanel("Readmission Risks",
               br(),
               fluidRow(
                 column(6, selectInput("state_readmission", "Filter by State:", 
                                       choices = c("All", sort(unique(df_combined$State))),
                                       selected = "All")),
                 column(6, uiOutput("readmission_kpi_cards"))
                 ),
                        hr(),
               tabsetPanel(
                 tabPanel("Scorecard", 
                          br(),
                          uiOutput("readmission_scorecard")),
                 tabPanel("Full Data Table", 
                          br(),
                          DT::dataTableOutput("readmission_table"))
               )
      ) #end readmission
                     
                    ) #end tabset
                    ), #end tab 
                 
       
navbarMenu("More",
           tabPanel("About",
                             includeMarkdown("about.md")
                    )
)
 ) # end navbarPage

