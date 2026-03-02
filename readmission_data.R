# Load libraries
library(tidyverse)
library(shiny)

# Load data
df <- FY_2025_Hospital_Readmissions_Reduction_Program_Hospital

# Clean data
df <- df %>% select(-`Facility ID`)
df$`Number of Readmissions` <- as.numeric(df$`Number of Readmissions`)

# Shiny App
ui <- fluidPage(
  titlePanel("Hospital Readmissions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("facility", 
                  "Select Facility:", 
                  choices = unique(df$`Facility Name`),
                  selected = unique(df$`Facility Name`)[1]),
      sliderInput("readmissions",
                  "Number of Readmissions:",
                  min = min(df$`Number of Readmissions`, na.rm = TRUE),
                  max = max(df$`Number of Readmissions`, na.rm = TRUE),
                  value = c(min(df$`Number of Readmissions`, na.rm = TRUE),
                            max(df$`Number of Readmissions`, na.rm = TRUE)))
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    df %>%
      filter(`Facility Name` == input$facility,
             `Number of Readmissions` >= input$readmissions[1],
             `Number of Readmissions` <= input$readmissions[2])
  })
}

shinyApp(ui, server)

#editing data
df <- df %>% select(-`Footnote`)
df <- df %>% select(-`Measure Name`)
df <- df %>% select(-`Excess Readmission Ratio`)
df <- df %>% select(-`Expected Readmission Rate`)
df <- df %>% select(-`Start Date`)
df <- df %>% select(-`End Date`)

shinyApp(ui, server)


#combining facility names 
df_combined <- df %>%
  group_by(`Facility Name`, State) %>%
  summarise(
    `Total Readmissions` = sum(`Number of Readmissions`, na.rm = TRUE),
    `Total Discharges` = sum(`Number of Discharges`, na.rm = TRUE),
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE)
  ) %>%
  ungroup()
choices = unique(df_combined$`Facility Name`)
df_combined %>%
  filter(`Facility Name` == input$facility,
         `Total Readmissions` >= input$readmissions[1],
         `Total Readmissions` <= input$readmissions[2]) 