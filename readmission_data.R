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
  titlePanel("Hospital Readmissions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("facility", 
                  "Select Facility:", 
                  choices = unique(df_combined$`Facility Name`),
                  selected = unique(df_combined$`Facility Name`)[1]),
      sliderInput("readmissions",
                  "Number of Readmissions:",
                  min = min(df_combined$`Total Readmissions`, na.rm = TRUE),
                  max = max(df_combined$`Total Readmissions`, na.rm = TRUE),
                  value = c(min(df_combined$`Total Readmissions`, na.rm = TRUE),
                            max(df_combined$`Total Readmissions`, na.rm = TRUE)))
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    df_combined %>%
      filter(`Facility Name` == input$facility,
             `Total Readmissions` >= input$readmissions[1],
             `Total Readmissions` <= input$readmissions[2])
  })
}

shinyApp(ui, server)






