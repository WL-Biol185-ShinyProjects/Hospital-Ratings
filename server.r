library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(bslib) 

staff_rating <- read.csv("staff_rating.csv", row.names = 1, check.names = FALSE)
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
SurgCenters <- read.csv("SurgCenters.csv", check.names = FALSE)
readmission <- read.csv("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv", check.names = FALSE)  
hvbp_raw <- read.csv("hvbp_person_and_community_engagement.csv", check.names = FALSE)

hvbp_clean <- hvbp_raw %>%
  select(
    `Facility Name`, State,
    nurse_comm  = `Communication With Nurses Performance Rate`,
    doctor_comm = `Communication With Doctors Performance Rate`,
    staff_resp  = `Responsiveness Of Hospital Staff Performance Rate`,
    care_trans  = `Care Transition Performance Rate`,
    med_comm    = `Communication About Medicines Performance Rate`,
    cleanliness = `Cleanliness And Quietness Of Hospital Environment Performance Rate`,
    discharge   = `Discharge Information Performance Rate`,
    overall     = `Overall Rating Of Hospital Performance Rate`,
    base_score  = `Hcahps Base Score`,
    consistency = `Hcahps Consistency Score`
  ) %>%
  mutate(
    across(c(nurse_comm, doctor_comm, staff_resp, care_trans,
             med_comm, cleanliness, discharge, overall),
           ~ as.numeric(gsub("%", "", .))),
    base_score  = as.numeric(base_score),
    consistency = as.numeric(consistency)
  )

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
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

function(input, output, session) {
  
  # Populate HVBP state dropdown on startup
  observe({
    updateSelectInput(session, "state_hvbp",
                      choices = c("All", sort(unique(hvbp_clean$State))),
                      selected = "All")
  })
  observeEvent(input$state_hvbp, {
    filtered <- if (input$state_hvbp == "All") {
      hvbp_clean
    } else {
      hvbp_clean %>% filter(State == input$state_hvbp)
    }
    updateSelectInput(session, "facility_hvbp",
                      choices = c("Select a hospital..." = "",
                                  sort(unique(filtered$`Facility Name`))),
                      selected = "")
  })
  hvbp_labels <- c(
    nurse_comm  = "Nurse Communication",
    doctor_comm = "Doctor Communication",
    staff_resp  = "Staff Responsiveness",
    care_trans  = "Care Transition",
    med_comm    = "Medicine Communication",
    cleanliness = "Cleanliness & Quiet",
    discharge   = "Discharge Info",
    overall     = "Overall Rating"
  )
  

  
  output$hvbp_hospital_card <- renderUI({
    req(input$facility_hvbp != "" && input$facility_hvbp != "Select a hospital...")
    
    hosp <- hvbp_clean %>% filter(`Facility Name` == input$facility_hvbp)
    req(nrow(hosp) > 0)
    
    scores <- hosp %>%
      select(all_of(names(hvbp_labels))) %>%
      pivot_longer(everything(), names_to = "measure", values_to = "score") %>%
      mutate(
        label = recode(measure, !!!hvbp_labels),
        color = case_when(
          score >= 80 ~ "#2ecc71",
          score >= 65 ~ "#f39c12",
          score >= 50 ~ "#e67e22",
          TRUE        ~ "#e74c3c"
        ),
        grade = case_when(
          score >= 80 ~ "Excellent",
          score >= 65 ~ "Good",
          score >= 50 ~ "Fair",
          TRUE        ~ "Poor"
        )
      )
    
    overall_avg <- mean(scores$score, na.rm = TRUE)
    overall_color <- case_when(
      overall_avg >= 80 ~ "#2ecc71",
      overall_avg >= 65 ~ "#f39c12",
      overall_avg >= 50 ~ "#e67e22",
      TRUE              ~ "#e74c3c"
    )
    
    div(
      style = "background:#fff; border:1px solid #ddd; border-radius:10px; padding:20px; margin-top:10px;",
      h3(input$facility_hvbp, style = "margin-top:0;"),
      div(style = paste0("display:inline-block; background:", overall_color,
                         "; color:white; border-radius:6px; padding:6px 14px;",
                         "font-size:14px; margin-bottom:15px;"),
          strong(paste0("Overall Avg: ", round(overall_avg, 1), "%"))
      ),
      hr(),
      fluidRow(
        lapply(1:nrow(scores), function(i) {
          column(3,
                 div(style = paste0("background:#f9f9f9; border-radius:8px; padding:12px;",
                                    "margin:6px 0; border-top: 4px solid ", scores$color[i], ";",
                                    "text-align:center;"),
                     p(scores$label[i],
                       style = "font-size:12px; color:#555; margin:0 0 6px 0; font-weight:bold;"),
                     div(style = paste0("font-size:26px; font-weight:bold; color:", scores$color[i], ";"),
                         paste0(round(scores$score[i], 1), "%")),
                     div(style = paste0("font-size:11px; color:", scores$color[i], ";"),
                         scores$grade[i])
                 )
          )
        })
      ),
      hr(),
      p(paste0("HCAHPS Base Score: ", hosp$base_score,
               " | Consistency Score: ", hosp$consistency),
        style = "font-size:12px; color:#888; margin:0;")
    )
  })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
 
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
# STAFF RATINGS 

  observeEvent(input$state_staff, {
    if (input$state_staff == "All") {
      facilities <- c("All", sort(unique(staff_rating$`Facility Name`)))
    } else {
      facilities <- c("All", sort(unique(staff_rating$`Facility Name`[staff_rating$State == input$state_staff])))
    }
    updateSelectInput(session, "facility_staff", choices = facilities, selected = "All")
  })
  
  staff_filtered <- reactive({
    data <- staff_rating
    if (input$state_staff != "All") data <- data %>% filter(State == input$state_staff)
    data
  })
  
  measure_labels <- c(
    "Patients who reported that staff definitely gave care in a professional way and the facility was clean"       = "Staff Care",
    "Patients who reported that staff definitely communicated about what to expect during and after the procedure" = "Communication",
    "Patients who gave the facility a rating of 9 or 10 on a scale from 0 (lowest) to 10 (highest)"              = "High Rating",
    "Patients who reported YES they would DEFINITELY recommend the facility to family or friends"                  = "Would Recommend"
  )
  
  pos_cols <- c(
    "Patients who reported that staff definitely gave care in a professional way and the facility was clean",
    "Patients who reported that staff definitely communicated about what to expect during and after the procedure",
    "Patients who gave the facility a rating of 9 or 10 on a scale from 0 (lowest) to 10 (highest)",
    "Patients who reported YES they would DEFINITELY recommend the facility to family or friends"
  )
  
  output$staff_chart <- renderPlot({
    req(input$measure)
    
    active_cols <- pos_cols[
      c("staff", "communication", "rating", "recommend") %in% input$measure
    ]
    req(length(active_cols) > 0)
    
    data <- staff_filtered() %>%
      select(`Facility Name`, all_of(active_cols)) %>%
      rowwise() %>%
      mutate(avg_score = mean(c_across(all_of(active_cols)), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Tier = case_when(
        avg_score >= 80 ~ "Excellent",
        avg_score >= 65 ~ "Good",
        avg_score >= 50 ~ "Fair",
        TRUE            ~ "Poor"
      ))
    
    heatmap_data <- data %>%
      group_by(Tier) %>%
      summarise(across(all_of(active_cols), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(-Tier, names_to = "Measure", values_to = "Score") %>%
      mutate(
        Measure = recode(Measure, !!!measure_labels),
        Tier    = factor(Tier, levels = c("Excellent", "Good", "Fair", "Poor"))
      ) %>%
      filter(!is.na(Score))
    
    ggplot(heatmap_data, aes(x = Measure, y = Tier, fill = Score)) +
      geom_tile(color = "white", linewidth = 1.5) +
      geom_text(aes(label = paste0(round(Score, 1), "%")),
                size = 5, fontface = "bold", color = "white") +
      scale_fill_gradient(low = "#e74c3c", high = "#2ecc71",
                          limits = c(0, 100), name = "% Positive") +
      labs(
        title = "Patient Experience by Performance Tier",
        subtitle = "Average positive response rate per measure",
        x = "", y = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position  = "right",
        axis.text.x      = element_text(face = "bold", size = 11),
        axis.text.y      = element_text(face = "bold", size = 11),
        panel.grid       = element_blank(),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(color = "#555", size = 11)
      )
  })
  
  output$staff_table <- renderTable({
    req(input$measure)
    active_cols <- pos_cols[
      c("staff", "communication", "rating", "recommend") %in% input$measure
    ]
    staff_filtered() %>%
      select(`Facility Name`, State, all_of(active_cols)) %>%
      rename_with(~ recode(., !!!measure_labels), all_of(active_cols))
  })
  
  # RISK FACTORS — FIND MY HOSPITAL
  output$facility_dropdown <- renderUI({
    req(input$state_hai)
    
    hospitals <- hai_cleaned %>%
      filter(State == input$state_hai) %>%
      pull(Facility.Name) %>%
      sort()
    
    selectInput("facility_hai", "Select a Hospital:",
                choices = c("Select a hospital..." = "", hospitals),
                selected = "")
  })
  
  care_type_infections <- reactive({ 
    switch(input$care_type, "General" = c
           ("MRSA Blood Infection", "C. Difficile Infection", 
             "Central Line Infection", 
             "Urinary Tract Infection"), 
           "Surgery" = c("Surgical Site - Colon", "Surgical Site - Hysterectomy", "Central Line Infection", "MRSA Blood Infection"), 
           "Maternity" = c("Surgical Site - Hysterectomy", "Urinary Tract Infection", "C. Difficile Infection"), 
           "Emergency" = c("MRSA Blood Infection", "C. Difficile Infection", "Central Line Infection", "Urinary Tract Infection") ) })
  
  # --- RISK FACTORS — Readmission Risks ---
  # Reactive filtered data based on search and state
  readmit_filtered <- reactive({
    req(df_combined)  # ensure df_combined exists
    data <- df_combined
    
    # Filter by search text
    if (!is.null(input$search_readmission) && input$search_readmission != "") {
      data <- data %>% filter(grepl(input$search_readmission, `Facility Name`, ignore.case = TRUE))
    }
    
    # Filter by state
    if (!is.null(input$state_readmission) && input$state_readmission != "All") {
      data <- data %>% filter(State == input$state_readmission)
    }
    
    data
  })
  
  # Bar chart: Top 10 facilities by total readmissions
  output$readmission_barchart <- renderPlot({
    req(readmit_filtered())
    
    readmit_filtered() %>%
      slice_max(`Total Readmissions`, n = 10) %>%
      ggplot(aes(x = reorder(`Facility Name`, `Total Readmissions`), y = `Total Readmissions`)) +
      geom_bar(stat = "identity", fill = "#2c7fb8") +
      coord_flip() +
      labs(title = "Top 10 Facilities by Total Readmissions",
           x = "", y = "Total Readmissions") +
      theme_minimal(base_size = 13)
  })
  
  # Full table of filtered readmission data
  output$readmission_table <- renderTable({
    req(readmit_filtered())
    readmit_filtered()
  })
  #  Render the hospital card
   output$hospital_card <- renderUI({
     req(input$state_hai)
     if (!isTruthy(input$facility_hai) || input$facility_hai == "Select a hospital...") {
       div(
         style = "margin-top:10px;",
         h4(paste("Hospitals in", input$state_hai, "— Select one above for details"),
            style = "color:#333;"),
         p("Sorted by best infection safety first.", style = "color:#777; font-size:13px;"),
         DT::dataTableOutput("summary_dt")
       )
     } else {
       hosp_data <- filter(hai_cleaned, Facility.Name == input$facility_hai,
                           Infection.Type %in% care_type_infections())
       req(nrow(hosp_data) > 0)
       
       hosp_data <- hosp_data %>%
         mutate(dot_color = case_when(
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
         ))
       
       avg_sir <- mean(hosp_data$Score, na.rm = TRUE)
       better_count <- sum(hosp_data$Score < 1.0, na.rm = TRUE)
       total_count <- nrow(hosp_data)
       
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
         div(style = paste0("background:", overall_color, "; color:white; padding:8px 14px;",
                            "border-radius:6px; display:inline-block; font-size:15px;"),
             strong(overall_label), " — Overall Infection Safety"),
         hr(),
         lapply(1:nrow(hosp_data), function(i) {
           div(style = "display:flex; align-items:center; margin:10px 0;",
               div(style = paste0("width:16px; height:16px; border-radius:50%; background:",
                                  hosp_data$dot_color[i], "; margin-right:12px; flex-shrink:0;")),
               div(strong(hosp_data$Infection.Type[i]),
                   br(),
                   span(hosp_data$performance_label[i], style = "color:#555; font-size:13px;"))
           )
         }),
         hr(),
         p(paste0("This hospital performs better than the national average in ",
                  better_count, " out of ", total_count, " tracked infection categories."),
           style = "font-size:15px; color:#333;")
       )
     }
   })
   
   output$summary_dt <- DT::renderDataTable({
     state_data <- if (input$state_hai == "All") hai_cleaned else filter(hai_cleaned, State == input$state_hai)
     state_data %>%
       group_by(Facility.Name, State) %>%
       summarise(Avg_SIR = round(mean(Score, na.rm = TRUE), 2),
                 Better  = sum(Score < 1.0, na.rm = TRUE),
                 Total   = n(), .groups = "drop") %>%
       mutate(Safety_Rating = case_when(
         Avg_SIR < 0.5 ~ "Excellent",
         Avg_SIR < 1.0 ~ "Good",
         Avg_SIR < 1.5 ~ "Fair",
         TRUE          ~ "Poor"
       ),
       Summary = paste0(Better, " of ", Total, " categories better than average")) %>%
       arrange(Avg_SIR) %>%
       select("Hospital" = Facility.Name,
              "State" = State,
              "Safety Rating" = Safety_Rating,
              "Summary" = Summary)
   }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
   
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
   observeEvent(input$go_compare, {
     updateTabsetPanel(session, "risk_tabs", selected = "Compare Hospitals")
   })
   output$gauge_grid <- renderUI({
     infection_types <- input$infection_type_compare
     req(length(infection_types) > 0)
     
     state_data <- if (input$state_hai_compare == "All") {
       hai_cleaned
     } else {
       hai_cleaned %>% filter(State == input$state_hai_compare)
     }
     
     gauge_boxes <- lapply(infection_types, function(inf) {
       inf_data <- state_data %>% filter(Infection.Type == inf)
       avg_sir   <- round(mean(inf_data$Score, na.rm = TRUE), 2)
       
       label <- case_when(
         avg_sir < 0.5 ~ "Excellent",
         avg_sir < 1.0 ~ "Good",
         avg_sir < 1.5 ~ "Fair",
         TRUE          ~ "Poor"
       )
       color <- case_when(
         avg_sir < 0.5 ~ "#2ecc71",
         avg_sir < 1.0 ~ "#f39c12",
         avg_sir < 1.5 ~ "#e67e22",
         TRUE          ~ "#e74c3c"
       )
       
       column(4,
              div(
                style = paste0(
                  "background:#fff; border:1px solid #ddd; border-radius:10px;",
                  "padding:20px; margin:10px 0; text-align:center;",
                  "border-top: 5px solid ", color, ";"
                ),
                h5(inf, style = "font-size:13px; color:#555; margin-bottom:10px;"),
                div(
                  style = paste0(
                    "font-size:36px; font-weight:bold; color:", color, ";"
                  ),
                  avg_sir
                ),
                div(
                  style = paste0(
                    "display:inline-block; background:", color,
                    "; color:white; border-radius:12px;",
                    "padding:3px 12px; font-size:12px; margin-top:6px;"
                  ),
                  label
                ),
                p("Compared to the national average",
                  style = "font-size:11px; color:#aaa; margin-top:8px;")
              )
       )
     })
     
     # Wrap every 3 gauges into a fluidRow
     rows <- split(gauge_boxes, ceiling(seq_along(gauge_boxes) / 3))
     do.call(tagList, lapply(rows, function(r) fluidRow(r)))
   })
   
   # ---------------- Specialty Care: Birthing Hospitals ----------------
   output$birthing_summary_bar <- renderUI({
     total <- nrow(birthing)
     n_states <- length(unique(birthing$state))
     span(style = "font-size:15px; color:#333;",
          tags$b(paste0("🏥 ", total)), " Birthing Friendly Hospitals across ",
          tags$b(paste0(n_states, " states")))
   })
   
   output$birthing_cards <- renderUI({
     data <- birthing
     if (input$birthing_state != "All") data <- filter(data, state == input$birthing_state)
     if (nrow(data) == 0) return(p("No hospitals found for the selected state."))
     cards <- lapply(1:nrow(data), function(i) {
       div(style = "background:#fff; border:1px solid #f8bbd0; border-radius:10px; padding:15px; margin:10px; display:inline-block; width:280px; vertical-align:top; box-shadow:2px 2px 5px rgba(0,0,0,0.08);",
           h4(style = "color:#c2185b; margin-top:0; font-size:15px;", "🏥 ", data$name[i]),
           p(style = "color:#555; font-size:13px; margin:4px 0;",
             "📍 ", data$addr[i], ", ", data$city[i], ", ", data$state[i], " ", data$zip[i]),
           tags$a(
             href = paste0("https://www.google.com/maps/dir/?api=1&destination=", data$lat[i], ",", data$lon[i]),
             target = "_blank", class = "btn btn-sm",
             style = "background:#e91e8c; color:white; border:none; margin-top:6px; margin-right:4px;",
             "🗺️ Directions"
           ),
           if (!is.na(data$phone[i]) && data$phone[i] != "") {
             tags$a(
               href = paste0("tel:", gsub("[^0-9]", "", data$phone[i])),
               class = "btn btn-sm", style = "background:#1a3a5c; color:white; border:none; margin-top:6px;",
               "📞 Call"
             )
           }
       )
     })
     do.call(tagList, cards)
   })
   
   output$worldMap <- renderLeaflet({
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
        }"))
     leaflet(birthing) %>%
       setView(lng = mean(birthing$lon, na.rm = TRUE),
               lat = mean(birthing$lat, na.rm = TRUE), zoom = 5) %>%
       addTiles() %>%
       addMarkers(lng = ~lon, lat = ~lat, icon = babyIcon, clusterOptions = babyCluster,
                  popup = ~paste0("<b>", name, "</b><br>", addr, "<br>",
                                  "<a href='https://www.google.com/maps/dir/?api=1&destination=",
                                  lat, ",", lon, "' target='_blank'>Get Directions</a>"))
   })
   
   # ---------------- VA Facilities ----------------
   output$vaMap <- renderLeaflet({
     brainIcon <- makeIcon(
       iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ccircle cx='50' cy='50' r='50' fill='%23c8d8e8'/%3E%3Ctext y='.85em' x='.1em' font-size='80'%3E%F0%9F%A7%A0%3C/text%3E%3C/svg%3E",
       iconWidth = 45, iconHeight = 45
     )
     leaflet(VA_IPF_geocoded) %>%
       setView(lng = mean(VA_IPF_geocoded$lon, na.rm = TRUE),
               lat = mean(VA_IPF_geocoded$lat, na.rm = TRUE), zoom = 4) %>%
       addTiles() %>%
       addMarkers(lng = ~lon, lat = ~lat, icon = brainIcon,
                  popup = ~paste0("<b>", Facility.Name, "</b><br>", full_address,
                                  "<br><a href='https://www.google.com/maps/dir/?api=1&destination=",
                                  lat, ",", lon, "' target='_blank'>Get Directions</a>"))
   })
   
   output$hotlineCards <- renderUI({
     data <- VA_IPF_geocoded
     if (input$hotline_state != "All") data <- filter(data, State == input$hotline_state)
     cards <- lapply(1:nrow(data), function(i) {
       div(style = "background-color: #f5f5f5; border-radius: 10px; padding: 15px; margin: 10px; display: inline-block; width: 280px; vertical-align: top; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
           h4(style = "color: #1a3a5c; margin-top: 0;", "\U1F3E5 ", data$Facility.Name[i]),
           p(style = "color: #555;", "\U1F4CD ", data$full_address[i]),
           p(style = "font-size: 16px; font-weight: bold;", "\U260E\UFE0F ", data$Telephone.Number[i]),
           tags$a(href = paste0("tel:", gsub("[^0-9]", "", data$Telephone.Number[i])),
                  class = "btn btn-primary btn-sm", style = "background-color: #1a3a5c; border: none;", "Call Now")
       )
     })
     do.call(tagList, cards)
   })
   
    #---------------- Surgery Centers ----------------
   #map 
   output$SurgMap <- renderLeaflet({
      leaflet(SurgCenters) %>%
        addTiles() %>%
        addAwesomeMarkers(
          lng   = ~longitude,
          lat   = ~latitude,
          icon  = awesomeIcons(
            icon = "hospital"
          ),
          popup = ~paste0(
            "<b>", SurgCenters[["Facility Name"]], "</b><br>",
            full_address
          )
        )
    })
   #cards
   output$surg_cards <- renderUI({
     data <- SurgCenters
     if (input$state_surg != "All") {
       data <- data %>% filter(State == input$state_surg)
     }
     
     if (nrow(data) == 0) {
       return(p("No surgery centers found for the selected state."))
     }
     
     cards <- lapply(1:nrow(data), function(i) {
       div(
         style = "background:#fff; border:1px solid #b3d1f7; border-radius:10px;
          padding:15px; margin:10px; display:inline-block; width:280px;
          vertical-align:top; box-shadow:2px 2px 5px rgba(0,0,0,0.08);",
         
         h4(style = "color:#1a3a5c; margin-top:0; font-size:15px;",
            "🏥 ", data$"Facility Name"[i]),  
         
         p(style = "color:#555; font-size:13px; margin:4px 0;",
           "📍 ", data$full_address[i]),
         
         if (isTRUE(!is.na(data[["Telephone Number"]][i]) && data[["Telephone Number"]][i] != "")) {
           p(style = "color:#555; font-size:13px; margin:4px 0;",
             "📞 ", data$"Telephone Number"[i])  
         },
         
         tags$a(
           href = paste0("https://www.google.com/maps/dir/?api=1&destination=",
                         data$latitude[i], ",", data$longitude[i]),
           target = "_blank",
           class = "btn btn-sm",
           style = "background:#1a3a5c; color:white; border:none;
            margin-top:6px; margin-right:4px;",
           "🗺️ Directions"
         ),
         
         if (isTRUE(!is.na(data[["Telephone Number"]][i]) && data[["Telephone Number"]][i] != "")) {
           tags$a(
             href = paste0("tel:", gsub("[^0-9]", "", data[["Telephone Number"]][i])),
             class = "btn btn-sm",
             style = "background:#e63946; color:white; border:none; margin-top:6px;",
             "📞 Call"
           )
         }
       )
     })
     do.call(tagList, cards)
   })
}
