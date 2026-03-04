library(markdown)

navbarPage("Hospital Ratings",
           tabPanel("Directory Map",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                        )
                      ),
                      mainPanel(
                        plotOutput("plot")
                      )
                    )
           ),
           tabPanel("Star Ratings",
                    verbatimTextOutput("summary")
           ),
           tabPanel("Staff and Communication"),
           tabPanel("Risk Factors"),
           
             navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               
                                        )
                                 )
                               )


