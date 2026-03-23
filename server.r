library(shiny)
library(leaflet)
library(tidyverse)
library(DT)

staff_rating <- read.csv("staff_rating.csv", row.names = 1, check.names = FALSE)
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
SurgCenters <- read.csv("SurgCenters.csv")
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
  # RISK FACTORS — FIND MY HOSPITAL
  observeEvent(input$state_hai, {
    filtered <- if (input$state_hai == "All") {
      hai_cleaned
    } else {
      hai_cleaned %>% filter(State == input$state_hai)
    }
    updateSelectInput(session, "facility_hai",
                      choices = c("Select a hospital..." = "",
                                  sort(unique(filtered$Facility.Name))))
  })
 
   care_type_infections <- reactive({
    switch(input$care_type,
           "General"   = c("MRSA Blood Infection",
                           "C. Difficile Infection",
                           "Central Line Infection",
                           "Urinary Tract Infection"),
           "Surgery"   = c("Surgical Site - Colon",
                           "Surgical Site - Hysterectomy",
                           "Central Line Infection",
                           "MRSA Blood Infection"),
           "Maternity" = c("Surgical Site - Hysterectomy",
                           "Urinary Tract Infection",
                           "C. Difficile Infection"),
           "Emergency" = c("MRSA Blood Infection",
                           "C. Difficile Infection",
                           "Central Line Infection",
                           "Urinary Tract Infection")
    )
  })
  #  Render the hospital card
   output$hospital_card <- renderUI({
     
     no_hospital_selected <- is.null(input$facility_hai) ||
       input$facility_hai == "" ||
       input$facility_hai == "Select a hospital..."
     
      
      if (no_hospital_selected) {
        div(
          style = "margin-top: 10px;",
          h4(
            if (input$state_hai == "All") "All Hospitals — Select a hospital above for details"
            else paste("Hospitals in", input$state_hai, "— Select one above for details"),
            style = "color: #333;"
          ),
          p("Sorted by best infection safety first.", style = "color:#777; font-size:13px;"),
          DT::dataTableOutput("summary_dt")
        )
        
      } else {
        
        hosp_data <- hai_cleaned %>%
          filter(Facility.Name == input$facility_hai,
                 Infection.Type %in% care_type_infections())
        
        req(nrow(hosp_data) > 0)
        
        hosp_data <- hosp_data %>%
          mutate(
            dot_color = case_when(
              Score < 1.0  ~ "#2ecc71",
              Score == 1.0 ~ "#f39c12",
              Score > 1.0  ~ "#e74c3c",
              TRUE         ~ "#aaaaaa"
            ),
            performance_label = case_when(
              Score < 1.0  ~ "Better than national average",
              Score == 1.0 ~ "At national average",
              Score > 1.0  ~ "Worse than national average",
              TRUE         ~ "No data available"
            )
          )
        avg_sir      <- mean(hosp_data$Score, na.rm = TRUE)
        better_count <- sum(hosp_data$Score < 1.0, na.rm = TRUE)
        total_count  <- nrow(hosp_data)
        
        overall_label <- case_when(
          avg_sir < 0.5 ~ "Excellent",
          avg_sir < 1.0 ~ "Good",
          avg_sir < 1.5 ~ "Fair",
          TRUE          ~ "Poor"
        )
        
        overall_color <- case_when(
          avg_sir < 0.5 ~ "#2ecc71",
          avg_sir < 1.0 ~ "#f39c12",
          avg_sir < 1.5 ~ "#e67e22",
          TRUE          ~ "#e74c3c"
        )
        
        div(
          style = "background:#fff; border:1px solid #ddd; border-radius:10px; padding:20px; margin-top:10px;",
          
          h3(input$facility_hai, style = "margin-top:0;"),
          
          p(paste0("Showing infection types relevant to: ", input$care_type),
            style = "color:#555; font-size:13px; margin-bottom:10px;"),
          
          div(
            style = paste0("background:", overall_color, "; color:white; padding:8px 14px;",
                           "border-radius:6px; display:inline-block; font-size:15px;"),
            strong(overall_label), " — Overall Infection Safety"
          ),
          
          hr(),
          
          lapply(1:nrow(hosp_data), function(i) {
            div(style = "display:flex; align-items:center; margin:10px 0;",
                div(style = paste0("width:16px; height:16px; border-radius:50%; background:",
                                   hosp_data$dot_color[i], "; margin-right:12px; flex-shrink:0;")),
                div(
                  strong(hosp_data$Infection.Type[i]),
                  br(),
                  span(hosp_data$performance_label[i], style = "color:#555; font-size:13px;")
                )
            )
          }),
          
          hr(),
          
          p(
            paste0("This hospital performs better than the national average in ",
                   better_count, " out of ", total_count, " tracked infection categories."),
            style = "font-size:15px; color:#333;"
          )
        )
      }
   })
   
        # Show summary table for the selected state
        output$summary_dt <- DT::renderDataTable({
        state_data <- if (input$state_hai == "All") {
          hai_cleaned
        } else {
          hai_cleaned %>% filter(State == input$state_hai)
        }
    state_data %>%
      group_by(Facility.Name, State) %>%
      summarise(
        Avg_SIR = round(mean(Score, na.rm = TRUE), 2),
        Better  = sum(Score < 1.0, na.rm = TRUE),
        Total   = n(),
        .groups = "drop"
      ) %>%
      mutate(
        Safety_Rating = case_when(
          Avg_SIR < 0.5 ~ "Excellent",
          Avg_SIR < 1.0 ~ "Good",
          Avg_SIR < 1.5 ~ "Fair",
          TRUE          ~ "Poor"
        ),
        Summary = paste0(Better, " of ", Total, " categories better than average")
      ) %>%
      arrange(Avg_SIR) %>%
      select(
        "Hospital"      = Facility.Name,
        "State"         = State,
        "Safety Rating" = Safety_Rating,
        "Summary"       = Summary
      )
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        # Compare nudge button
        output$compare_nudge <- renderUI({
          req(input$facility_hai != "")
          
          div(
            style = "margin-top:15px; padding:12px; background:#f0f4ff;
               border-radius:8px; border-left:4px solid #3498db;",
            span("👉 Want to see how this hospital compares to others nearby?  "),
            actionButton("go_compare", "Compare Hospitals →",
                         style = "background:#3498db; color:white; border:none;
                            border-radius:4px; padding:6px 12px; margin-left:8px;")
          )
        })
        
        # Wire compare button to switch tabs
        observeEvent(input$go_compare, {
          updateTabsetPanel(session, inputId = "risk_tabs", selected = "Compare Hospitals")
        }) 
        hai_compare_filtered <- reactive({
          data <- hai_cleaned
          if (input$state_hai_compare != "All") {
            data <- data %>% filter(State == input$state_hai_compare)
          }
          data <- data %>% filter(Infection.Type %in% input$infection_type_compare)
          data
        })
        
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
        output$birthing_summary_bar <- renderUI({
          total    <- nrow(birthing)
          n_states <- length(unique(birthing$state))
          
          span(
            style = "font-size:15px; color:#333;",
            tags$b(paste0("🏥 ", total)),
            " Birthing Friendly Hospitals across ",
            tags$b(paste0(n_states, " states"))
          )
        })
        # Birthing hospital cards
        output$birthing_cards <- renderUI({
          data <- birthing
          if (input$birthing_state != "All") {
            data <- data %>% filter(state == input$birthing_state)
          }
          
          if (nrow(data) == 0) {
            return(p("No hospitals found for the selected state."))
          }
          
          cards <- lapply(1:nrow(data), function(i) {
            div(
              style = "background:#fff; border:1px solid #f8bbd0; border-radius:10px;
               padding:15px; margin:10px; display:inline-block; width:280px;
               vertical-align:top; box-shadow:2px 2px 5px rgba(0,0,0,0.08);",
              
              h4(style = "color:#c2185b; margin-top:0; font-size:15px;",
                 "🏥 ", data$name[i]),
              
              p(style = "color:#555; font-size:13px; margin:4px 0;",
                "📍 ", data$addr[i], ", ", data$city[i], ", ", 
                data$state[i], " ", data$zip[i]),
              
              tags$a(
                href = paste0("https://www.google.com/maps/dir/?api=1&destination=",
                              data$lat[i], ",", data$lon[i]),
                target = "_blank",
                class = "btn btn-sm",
                style = "background:#e91e8c; color:white; border:none;
                 margin-top:6px; margin-right:4px;",
                "🗺️ Directions"
              ),
              
              if (!is.na(data$phone[i]) && data$phone[i] != "") {
                tags$a(
                  href = paste0("tel:", gsub("[^0-9]", "", data$phone[i])),
                  class = "btn btn-sm",
                  style = "background:#1a3a5c; color:white; border:none; margin-top:6px;",
                  "📞 Call"
                )
              }
            )
          })
          do.call(tagList, cards)
        })
        
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
    #surgery map
    output$SurgMap <- renderLeaflet({
      leaflet(SurgCenters) %>%
        addTiles() %>%
        addMarkers(
          lng   = ~longitude,
          lat   = ~latitude,
          popup = ~paste0(
            "<b>", SurgCenters[["Facility Name"]], "</b><br>",
            full_address
          )
        )
    })
}
