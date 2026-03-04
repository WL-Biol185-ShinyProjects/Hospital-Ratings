# Load libraries
library(tidyverse)
library(shiny)

# Load data
df <- FY_2025_Hospital_Readmissions_Reduction_Program_Hospital

# Clean data
df <- df %>% select(-`Facility ID`, -`Footnote`, -`Measure Name`, 
                    -`Excess Readmission Ratio`, -`Expected Readmission Rate`,
                    -`Start Date`, -`End Date`)

df$`Number of Readmissions` <- as.numeric(df$`Number of Readmissions`)
df$`Number of Discharges` <- as.numeric(df$`Number of Discharges`)
df$`Predicted Readmission Rate` <- as.numeric(df$`Predicted Readmission Rate`)

# Combine by facility
df_combined <- df %>%
  group_by(`Facility Name`, State) %>%
  summarise(
    `Total Readmissions` = sum(`Number of Readmissions`, na.rm = TRUE),
    `Total Discharges` = sum(`Number of Discharges`, na.rm = TRUE),
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE)
  ) %>%
  ungroup()

# Shiny App
ui <- fluidPage(
  titlePanel("Hospital Readmissions Dashboard"),
  
  # Search bar
  fluidRow(
    column(4, textInput("search", "Search Facility:", placeholder = "Type facility name...")),
    column(4, selectInput("state", "Filter by State:", 
                          choices = c("All", sort(unique(df_combined$State))),
                          selected = "All"))
  ),
  
  hr(),
  
  # Bar chart
  fluidRow(
    column(12, plotOutput("barchart", height = "300px"))
  ),
  
  hr(),
  
  # Full wide table
  fluidRow(
    column(12, tableOutput("table"))
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    data <- df_combined
    if (input$search != "") {
      data <- data %>% filter(grepl(input$search, `Facility Name`, ignore.case = TRUE))
    }
    if (input$state != "All") {
      data <- data %>% filter(State == input$state)
    }
    data
  })
  
  output$barchart <- renderPlot({
    filtered() %>%
      slice_max(`Total Readmissions`, n = 10) %>%
      ggplot(aes(x = reorder(`Facility Name`, `Total Readmissions`), y = `Total Readmissions`)) +
      geom_bar(stat = "identity", fill = "#2c7fb8") +
      coord_flip() +
      labs(title = "Top 10 Facilities by Total Readmissions", x = "", y = "Total Readmissions") +
      theme_minimal()
  })
  
  output$table <- renderTable({
    filtered()
  })
}

shinyApp(ui, server)