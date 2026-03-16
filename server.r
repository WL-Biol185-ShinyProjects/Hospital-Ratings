library(shiny)
library(leaflet)
library(tidyverse)

staff_rating <- read.csv("staff_rating.csv", row.names = 1, check.names = FALSE)
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
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
  
  #Hospital Associated Infections
  hai_filtered <- reactive({
    data <- hai_cleaned
    if (input$state_hai != "All") {
      data <- data %>% filter(State == input$state_hai)
    }
    if (input$facility_hai != "All") {
      data <- data %>% filter(Facility.Name == input$facility_hai)
    }
    data <- data %>% filter(Infection.Type %in% input$infection_type)
    data
  })
  # Update facility dropdown when state changes
  observeEvent(input$state_hai, {
    if (input$state_hai == "All") {
      facilities <- c("All", sort(unique(hai_cleaned$Facility.Name)))
    } else {
      facilities <- c("All", sort(unique(hai_cleaned$Facility.Name[hai_cleaned$State == input$state_hai])))
    }
    updateSelectInput(session, "facility_hai", choices = facilities, selected = "All")
  })
  # HAI Chart Find My Hospital
  output$hai_chart <- renderPlot({
    req(input$infection_type)
    data <- hai_filtered()
    
    # Shared plot elements
    shared_layers <- list(
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1),
      annotate("text", x = 0.6, y = 1.05,
               label = "National Average (1.0) — Lower is Better",
               color = "black", size = 3.5, hjust = 0),
      coord_flip(),
      theme_minimal(),
      theme(legend.position = "bottom",
            plot.subtitle = element_text(size = 9, color = "gray50"),
            axis.text.y = element_text(size = 11))
    )
    
    if (input$facility_hai == "All") {
      data %>%
        group_by(Infection.Type) %>%
        summarise(Avg_SIR = mean(Score, na.rm = TRUE), .groups = "drop") %>%
        mutate(Performance = case_when(
          Avg_SIR < 1 ~ "Better than Average",
          Avg_SIR > 1 ~ "Worse than Average",
          TRUE ~ "About Average"
        )) %>%
        ggplot(aes(x = reorder(Infection.Type, Avg_SIR), y = Avg_SIR, fill = Performance)) +
        geom_col() +
        scale_fill_manual(values = c(
          "Better than Average" = "#2ecc71",
          "About Average"       = "#f39c12",
          "Worse than Average"  = "#e74c3c"
        ), drop = FALSE) +
        labs(title = "Select a facility to see its infection rates",
             subtitle = "Currently showing state average — filter by facility above",
             x = "", y = "Average SIR Score", fill = "Performance") +
        shared_layers
      
    } else {
      data %>%
        ggplot(aes(x = reorder(Infection.Type, Score), y = Score, fill = Compared.to.National)) +
        geom_col() +
        scale_fill_manual(values = c(
          "Better than the National Benchmark"   = "#2ecc71",
          "No Different than National Benchmark" = "#f39c12",
          "Worse than the National Benchmark"    = "#e74c3c"
        ), drop = FALSE) +
        labs(title = paste("Infection Rates:", input$facility_hai),
             subtitle = "Scores below 1.0 mean fewer infections than the national average",
             x = "", y = "Standardized Infection Ratio (SIR)", fill = "Performance") +
        shared_layers
    }
  })
  # HAI Table
  output$hai_table <- renderTable({
    req(input$infection_type)
    hai_filtered() %>%
      select(
        "Facility" = Facility.Name,
        "State" = State,
        "Infection Type" = Infection.Type,
        "SIR Score" = Score,
        "Compared to National Average" = Compared.to.National
      ) %>%
      arrange(`SIR Score`)
  })
  # Compare tab filtered data
  hai_compare_filtered <- reactive({
    data <- hai_cleaned
    if (input$state_hai_compare != "All") {
      data <- data %>% filter(State == input$state_hai_compare)
    }
    data <- data %>% filter(Infection.Type %in% input$infection_type_compare)
    data
  })
  
  # Compare chart
  output$hai_compare_chart <- renderPlot({
    req(input$infection_type_compare)
    hai_compare_filtered() %>%
      ggplot(aes(x = Infection.Type, y = Score, fill = Infection.Type)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
      annotate("text", x = 0.6, y = 1.05,
               label = "National Average (1.0) — Lower is Better",
               color = "black", size = 3.5, hjust = 0) +
      coord_flip() +
      labs(
        title = if (input$state_hai_compare == "All") {
          "Infection Rate Distribution — All Hospitals Nationwide"
        } else {
          paste("Infection Rate Distribution —", input$state_hai_compare, "Hospitals")
        },
        subtitle = "Each box shows the spread of scores across all hospitals in the selected area",
        x = "", y = "Standardized Infection Ratio (SIR)"
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.subtitle = element_text(size = 9, color = "gray50"),
            axis.text.y = element_text(size = 11))
  })
  
  # Birthing Friendly Hospitals Map and Directions
  babyIcon <- makeIcon(
    iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ctext y='.9em' font-size='90'%3E%F0%9F%91%B6%3C/text%3E%3C/svg%3E",
    iconWidth = 35, iconHeight = 35
  
  )
  
  babyCluster <- markerClusterOptions(
    iconCreateFunction = JS("
    function(cluster) {
      return L.divIcon({
        html: '<div style=\"background-color: #a8d8ea; border-radius: 50%; width: 50px; height: 50px; display: flex; align-items: center; justify-content: center; border: 2px solid #7ab3c8; position: relative;\"><span style=\"font-size:26px;\">🍼</span><span style=\"position:absolute; bottom:-5px; right:-5px; background:#7ab3c8; border-radius:50%; width:20px; height:20px; display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:bold; color:white;\">' + cluster.getChildCount() + '</span></div>',
        className: 'baby-cluster',
        iconSize: L.point(50, 50)
      });
    }
  ")
  )
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
        icon = babyIcon,
        clusterOptions = babyCluster,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          addr, "<br>",
          "<a href='https://www.google.com/maps/dir/?api=1&destination=",
          lat, ",", lon,
          "' target='_blank'>Get Directions</a>"
        )
      )
  })
    
    # Veteran Inpatient Psychiatric Facilities Map
    brainIcon <- makeIcon(
      iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ccircle cx='50' cy='50' r='50' fill='%23c8d8e8'/%3E%3Ctext y='.85em' x='.1em' font-size='80'%3E%F0%9F%A7%A0%3C/text%3E%3C/svg%3E",
      iconWidth = 45, iconHeight = 45
    )
      output$vaMap <- renderLeaflet({
      leaflet(VA_IPF_geocoded) %>%
        setView(
          lng = mean(VA_IPF_geocoded$lon, na.rm = TRUE),
          lat = mean(VA_IPF_geocoded$lat, na.rm = TRUE),
          zoom = 4
        ) %>%
        addTiles() %>%
        addMarkers(
          lng = ~lon,
          lat = ~lat,
          icon = brainIcon,
          popup = ~paste0(
            "<b>", Facility.Name, "</b><br>",
            full_address, "<br>",
            "<a href='https://www.google.com/maps/dir/?api=1&destination=",
            lat, ",", lon,
            "' target='_blank'>Get Directions</a>"
          )
        )
    })
    # Vet IPF Hotline section
    output$hotlineCards <- renderUI ({
      data <- VA_IPF_geocoded
      if (input$hotline_state != "All") {
        data <- data %>% filter(State == input$hotline_state)
      }
      
      cards <- lapply(1:nrow(data), function(i) {
        div(style = "background-color: #f5f5f5; border-radius: 10px; padding: 15px; margin: 10px; display: inline-block; width: 280px; vertical-align: top; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
            h4(style = "color: #1a3a5c; margin-top: 0;",
               "\U1F3E5 ", data$Facility.Name[i]),
            p(style = "color: #555;",
              "\U1F4CD ", data$full_address[i]),
            p(style = "font-size: 16px; font-weight: bold;",
              "\U260E\UFE0F ", data$Telephone.Number[i]),
            tags$a(
              href = paste0("tel:", gsub("[^0-9]", "", data$Telephone.Number[i])),
              class = "btn btn-primary btn-sm",
              style = "background-color: #1a3a5c; border: none;",
              "Call Now"
            )
        )
      })
      do.call(tagList, cards)
})
}
