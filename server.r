library(shiny)
library(leaflet)
staff_rating <- read.csv("staff_rating.csv", row.names = 1, check.names = FALSE)
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
function(input, output, session) {
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
  # Update facility dropdown when state changes
  observeEvent(input$state_staff, {
    if (input$state_staff == "All") {
      facilities <- c("All", sort(unique(staff_rating$`Facility Name`)))
    } else {
      facilities <- c("All", sort(unique(staff_rating$`Facility Name`[staff_rating$State == input$state_staff])))
    }
    updateSelectInput(session, "facility_staff", choices = facilities, selected = "All")
  })
  
  # Filtered data
  staff_filtered <- reactive({
    data <- staff_rating
    if (input$state_staff != "All") {
      data <- data %>% filter(State == input$state_staff)
    }
    if (input$facility_staff != "All") {
      data <- data %>% filter(`Facility Name` == input$facility_staff)
    }
    data
  })
  
  # Selected columns based on chosen measures
  selected_cols <- reactive({
    cols <- c()
    if ("staff" %in% input$measure) {
      cols <- c(cols,
                "Patients who reported that staff definitely gave care in a professional way and the facility was clean",
                "Patients who reported that staff somewhat gave care in a professional way or the facility was somewhat clean",
                "Patients who reported that staff did not give care in a professional way or the facility was not clean")
    }
    if ("communication" %in% input$measure) {
      cols <- c(cols,
                "Patients who reported that staff definitely communicated about what to expect during and after the procedure",
                "Patients who reported that staff somewhat communicated about what to expect during and after the procedure",
                "Patients who reported that staff did not communicate about what to expect during and after the procedure")
    }
    if ("rating" %in% input$measure) {
      cols <- c(cols,
                "Patients who gave the facility a rating of 9 or 10 on a scale from 0 (lowest) to 10 (highest)",
                "Patients who gave the facility a rating of 7 or 8 on a scale from 0 (lowest) to 10 (highest)",
                "Patients who gave the facility a rating of 0 to 6 on a scale from 0 (lowest) to 10 (highest)")
    }
    if ("recommend" %in% input$measure) {
      cols <- c(cols,
                "Patients who reported YES they would DEFINITELY recommend the facility to family or friends",
                "Patients who reported PROBABLY YES they would recommend the facility to family or friends",
                "Patients who reported NO, they would not recommend the facility to family or friends")
    }
    cols
  })
  
  # Chart
  output$staff_chart <- renderPlot({
    req(input$measure)
    staff_filtered() %>%
      select(`Facility Name`, all_of(selected_cols())) %>%
      pivot_longer(-`Facility Name`, names_to = "Category", values_to = "Percent") %>%
      ggplot(aes(x = `Facility Name`, y = Percent, fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Staff & Communication Ratings", x = "", y = "Percent of Patients") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 7))

})
  # Table
  output$staff_table <- renderTable({
    req(input$measure)
    staff_filtered() %>%
      select(`Facility Name`, State, all_of(selected_cols()))
  })
  # Birthing Friendly Hospitals Map
  output$worldMap <- renderLeaflet({
    
    btn <- input$newButton
    
    leaflet(Birthing_Friendly_Hospitals_Geocoded) %>%
      setView(
        lng = mean(Birthing_Friendly_Hospitals_Geocoded$lon, na.rm = TRUE),
        lat = mean(Birthing_Friendly_Hospitals_Geocoded$lat, na.rm = TRUE),
        zoom = 5
      ) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", name, "</b><br>",
          addr, "<br>",
          "<a href='https://www.google.com/maps/dir/?api=1&destination=",
          lat, ",", lon,
          "' target='_blank'>Get Directions</a>"
        )
      )
  })
    
}

