library(markdown)

staff_rating <- read.csv("staff_rating.csv")
navbarPage("Hospital Ratings",
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
           tabPanel("Risk Factors"),
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About")
           )
)


