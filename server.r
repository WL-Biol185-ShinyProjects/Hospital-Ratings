library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(bslib) 
library(purrr)

hospitalgen <- read.csv("hosp.general.info.csv", check.names = FALSE)
directory <- read.csv("directory.csv", check.names = FALSE)
birthing <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")
VA_IPF_geocoded <- read.csv("VA_IPF_geocoded.csv")
hai_cleaned <- read.csv("hai_cleaned.csv")
SurgCenters <- read.csv("SurgCenters.csv", check.names = FALSE)
readmission <- read.csv("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv", check.names = FALSE)  
hvbp_raw <- read.csv("hvbp_person_and_community_engagement.csv", check.names = FALSE)

staff_rating <- read_csv("ASCQR_OAS_CAHPS_BY_ASC.csv", show_col_types = FALSE) %>%
  select(-1) %>%
   rename(
    `Facility Name`  = `Facility Name`,
    `State`          = State,
    `Staff Care`     = `Patients who reported that staff definitely gave care in a professional way and the facility was clean`,
    `Communication`  = `Patients who reported that staff definitely communicated about what to expect during and after the procedure`,
    `High Rating`    = `Patients who gave the facility a rating of 9 or 10 on a scale from 0 (lowest) to 10 (highest)`,
    `Would Recommend`= `Patients who reported YES they would DEFINITELY recommend the facility to family or friends`
  ) %>%
  select(`Facility Name`, State, `Staff Care`, `Communication`, 
         `High Rating`, `Would Recommend`) %>%
  mutate(across(c(`Staff Care`, `Communication`, `High Rating`, `Would Recommend`),
                ~ as.numeric(as.character(.x))))

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
    `Avg Predicted Readmission Rate` = mean(`Predicted Readmission Rate`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()
names(df_combined)

#stars
make_stars <- function(n) {
  n <- suppressWarnings(as.numeric(n))
  if (is.na(n)) return(NA_character_)
  paste0(strrep("★", n), strrep("☆", 5 - n))
}                      
hospitalgen <- hospitalgen %>%                                       
  mutate(                                                            
    overall_star_display = map_chr(`Hospital overall rating`, make_stars),
    emergency_badge = ifelse(`Emergency Services` == "Yes", "🚨 Emergency", ""),
    birthing_badge = ifelse(`Meets criteria for birthing friendly designation` == "Y", "🤱 Birthing Friendly", "")  
  ) 

#server function
function(input, output, session) {

  # --- STAR RATINGS PAGE ---
  
  # filtered data
  sr_filtered <- reactive({
    df <- hospitalgen
    if (input$sr_state != "All") df <- df %>% filter(State == input$sr_state)
    if (input$sr_type != "All")  df <- df %>% filter(`Hospital Type` == input$sr_type)
    if (input$sr_ownership != "All") df <- df %>% filter(`Hospital Ownership` == input$sr_ownership)
    if (nchar(input$sr_search) > 0)
      df <- df %>% filter(grepl(input$sr_search, `Facility Name`, ignore.case = TRUE))
    df
  })
  
  # render cards
  output$hospital_cards <- renderUI({
    rows <- sr_filtered()
    if (nrow(rows) == 0) return(p("No hospitals match your filters."))
    
    cards <- lapply(1:nrow(rows), function(i) {
      h <- rows[i, ]
      stars <- ifelse(is.na(h$overall_star_display), "Not Rated", h$overall_star_display)
      rating <- suppressWarnings(as.numeric(h$`Hospital overall rating`))
      star_color <- case_when(
        is.na(rating) ~ "#888",
        rating >= 4   ~ "#2e7d32",
        rating == 3   ~ "#f9a825",
        TRUE          ~ "#c62828"
      )
      
      div(
        style = "background:#fff; border:1px solid #b3d1f7; border-radius:12px;
                 padding:20px; margin-bottom:15px;
                 box-shadow: 2px 2px 8px rgba(0,0,0,0.08);",
        
        # hospital name + location
        h4(style = "color:#1a3a5c; margin:0 0 4px 0;", h$`Facility Name`),
        p(style = "color:#888; font-size:13px; margin:0 0 10px 0;",
          paste0(h$`City/Town`, ", ", h$State, " — ", h$`Hospital Type`)),
        
        # star rating
        div(style = paste0("font-size:22px; color:", star_color, "; margin-bottom:8px;"), stars),
        
        # badges
        div(style = "margin-bottom:10px;",
            if (h$emergency_badge != "") span(style = "background:#e8f0fe; border-radius:12px;
                padding:3px 10px; font-size:12px; margin-right:6px;", h$emergency_badge),
            if (h$birthing_badge != "")  span(style = "background:#fce4ec; border-radius:12px;
                padding:3px 10px; font-size:12px;", h$birthing_badge)
        ),
        
        # measure counts
        fluidRow(
          column(4,
                 div(style = "background:#f8f9fa; border-radius:8px; padding:10px; text-align:center;",
                     tags$b(style = "color:#1a3a5c; font-size:12px;", "Mortality"),
                     p(style = "margin:4px 0 0 0; font-size:12px;",
                       paste0("✅ ", h$`Count of MORT Measures Better`, " better")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("➖ ", h$`Count of MORT Measures No Different`, " average")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("❌ ", h$`Count of MORT Measures Worse`, " worse"))
                 )
          ),
          column(4,
                 div(style = "background:#f8f9fa; border-radius:8px; padding:10px; text-align:center;",
                     tags$b(style = "color:#1a3a5c; font-size:12px;", "Safety"),
                     p(style = "margin:4px 0 0 0; font-size:12px;",
                       paste0("✅ ", h$`Count of Safety Measures Better`, " better")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("➖ ", h$`Count of Safety Measures No Different`, " average")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("❌ ", h$`Count of Safety Measures Worse`, " worse"))
                 )
          ),
          column(4,
                 div(style = "background:#f8f9fa; border-radius:8px; padding:10px; text-align:center;",
                     tags$b(style = "color:#1a3a5c; font-size:12px;", "Readmissions"),
                     p(style = "margin:4px 0 0 0; font-size:12px;",
                       paste0("✅ ", h$`Count of READM Measures Better`, " better")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("➖ ", h$`Count of READM Measures No Different`, " average")),
                     p(style = "margin:2px 0; font-size:12px;",
                       paste0("❌ ", h$`Count of READM Measures Worse`, " worse"))
                 )
          )
        )
      )
    })
    do.call(tagList, cards)
  })
  
  # compare checkboxes — shows names of filtered hospitals
  output$compare_checkboxes <- renderUI({
    nms <- sr_filtered()$`Facility Name`
    if (length(nms) == 0) return(p("No hospitals to compare.", style = "font-size:12px;"))
    checkboxGroupInput("compare_selected", NULL, choices = nms)
  })
  
  # compare panel
  output$compare_panel <- renderUI({
    req(input$compare_btn)
    isolate({
      selected <- input$compare_selected
      if (is.null(selected) || length(selected) == 0)
        return(div(style="color:#c62828;", "Please select hospitals to compare."))
      if (length(selected) > 3)
        return(div(style="color:#c62828;", "Please select 3 or fewer hospitals."))
      
      df <- hospitalgen %>% filter(`Facility Name` %in% selected)
      
      cols <- c("Facility Name", "State", "Hospital overall rating",
                "Count of MORT Measures Better", "Count of MORT Measures Worse",
                "Count of Safety Measures Better", "Count of Safety Measures Worse",
                "Count of READM Measures Better", "Count of READM Measures Worse",
                "Emergency Services", "Meets criteria for birthing friendly designation")
      
      div(
        style = "background:#e8f0fe; border-radius:10px; padding:15px; margin-bottom:20px;",
        h4(style = "color:#1a3a5c;", "⚖️ Side-by-Side Comparison"),
        renderTable(df %>% select(all_of(cols)))
      )
    })
  })
  
  # clear compare
  observeEvent(input$clear_compare, {
    updateCheckboxGroupInput(session, "compare_selected", selected = character(0))
  })
  
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
  
#DIRECTORY
  #map
  output$directoryMap <- renderLeaflet({
    map_data <- directory %>% filter(!is.na(latitude) & !is.na(longitude))
    
    leaflet(map_data) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste0("<b>", Facility.Name, "</b><br>", full_address))
  })
  #cards:
  output$directory_cards <- renderUI({
    data <- directory
    
    # ⬅️ ADDED - remove rows with no address
    data <- data %>% filter(!is.na(full_address) & full_address != "")
    
    if (input$state_dir != "All") data <- filter(data, State == input$state_dir)
    
    if (nrow(data) == 0) {
      return(p("No facilities found for the selected state."))
    }
    
    cards <- lapply(1:nrow(data), function(i) {
      div(style = "background-color: #f5f5f5; border-radius: 10px; padding: 15px; margin: 10px; display: inline-block; width: 280px; vertical-align: top; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
          
          h4(style = "color: #1a3a5c; margin-top: 0;", "🏥 ", data$Facility.Name[i]),
          
          p(style = "color: #555;", "📍 ", data$full_address[i]),
          
          if (isTRUE(!is.na(data$Telephone.Number[i]) && data$Telephone.Number[i] != "")) {
            p(style = "font-size: 16px; font-weight: bold;", "☎️ ", data$Telephone.Number[i])
          },
          
          tags$a(href = paste0("https://www.google.com/maps/dir/?api=1&destination=",
                               data$latitude[i], ",", data$longitude[i]),
                 target = "_blank",
                 class = "btn btn-sm",
                 style = "background-color: #1a3a5c; color: white; border: none; margin-top: 6px; margin-right: 4px;",
                 "🗺️ Directions"),
          
          if (isTRUE(!is.na(data$Telephone.Number[i]) && data$Telephone.Number[i] != "")) {
            tags$a(href = paste0("tel:", gsub("[^0-9]", "", data$Telephone.Number[i])),
                   class = "btn btn-sm",
                   style = "background-color: #e63946; color: white; border: none; margin-top: 6px;",
                   "📞 Call")
          }
      )
    })
    do.call(tagList, cards)
  })
  #end directory
  
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
  
# --------STAFF RATINGS -------------

  measure_map <- c(
    staff         = "Staff Care",
    communication = "Communication",
    rating        = "High Rating",
    recommend     = "Would Recommend"
  )
  
  short_labels <- c(
    "Staff Care"      = "Staff Care Quality",
    "Communication"   = "Communication",
    "High Rating"     = "Facility Rating",
    "Would Recommend" = "Would Recommend"
  )
  
  observeEvent(input$state_staff, {
    if (is.null(input$state_staff) || length(input$state_staff) == 0) {
      choices <- sort(unique(staff_rating$`Facility Name`))
    } else {
      choices <- sort(unique(
        staff_rating$`Facility Name`[staff_rating$State %in% input$state_staff]
      ))
    }
    updateSelectizeInput(session, "facility_staff", choices = choices, selected = NULL)
  })
  
  
  staff_filtered <- reactive({
    data <- staff_rating 
    
    if (!is.null(input$state_staff) && length(input$state_staff) > 0)
      data <- data %>% filter(State %in% input$state_staff)
    if (!is.null(input$facility_staff) && length(input$facility_staff) > 0)
      
      data <- data %>% filter(`Facility Name` %in% input$facility_staff)
    
    # Drop rows where any of the four measure columns are NA
    data <- data %>%
      filter(
        !is.na(`Staff Care`),
        !is.na(`Communication`),
        !is.na(`High Rating`),
        !is.na(`Would Recommend`)
      )
    data
  })
  
  output$staff_chart <- renderPlot({
    req(input$apply_staff > 0)
    data <- staff_filtered()
    req(nrow(data) > 0)
    
    group_var <- if (!is.null(input$facility_staff) && length(input$facility_staff) > 0) {
      "Facility Name"
    } else {
      "State"
    }
    
    plot_data <- data %>%
      group_by(.data[[group_var]]) %>%
      summarise(across(all_of(unname(measure_map)), ~ mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(-all_of(group_var), names_to = "Measure", values_to = "Score") %>%
      rename(Group = all_of(group_var)) %>%
      mutate(
        Measure = factor(unname(short_labels[Measure]), levels = unname(short_labels)),
        Score = round(Score, 1)
      )
    
    dodge <- position_dodge(width = 0.8)
    
    ggplot(plot_data, aes(x = Measure, y = Score, fill = Group, group = Group)) +
      geom_col(position = dodge, width = 0.7) +
      geom_text(
        aes(label = paste0(Score, "%")),
        position = dodge,
        hjust = -0.15,
        size = 4
      ) +
      coord_flip(clip = "off") +
      scale_y_continuous(
        limits = c(0, 110),
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(
        x = NULL, y = NULL, fill = group_var,
        title = "Patient Experience Scores",
        subtitle = "% of surveyed patients who responded positively to each measure"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"
      )
  }, height = function() {
    data <- staff_filtered()
    n_groups <- if (!is.null(input$facility_staff) && length(input$facility_staff) > 0) {
      length(unique(data$`Facility Name`))
    } else {
      length(unique(data$State))
    }
    max(400, n_groups * 45)
  })
  output$staff_table <- renderTable({
    req(input$apply_staff > 0)
    data <- staff_filtered()
    req(nrow(data) > 0)
    
    if (!is.null(input$facility_staff) && length(input$facility_staff) > 0) {
      group_var <- "Facility Name"
    } else {
      group_var <- "State"
    }
    
    data %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        `Staff Care Quality` = round(mean(as.numeric(`Staff Care`), na.rm = TRUE), 1),
        `Communication` = round(mean(as.numeric(`Communication`), na.rm = TRUE), 1),
        `Facility Rating` = round(mean(as.numeric(`High Rating`), na.rm = TRUE), 1),
        `Would Recommend` = round(mean(as.numeric(`Would Recommend`), na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
  
  # RISK FACTORS — FIND MY HOSPITAL
  output$facility_readmission_dropdown <- renderUI({
    if (input$state_readmission == "All") {
      facilities <- sort(unique(df_combined$`Facility Name`))
    } else {
      facilities <- df_combined %>%
        filter(State == input$state_readmission) %>%
        pull(`Facility Name`) %>%
        sort()
    }
    
    selectInput("facility_readmission", "Select a Facility:",
                choices = c("All" = "All", facilities),
                selected = "All")
  })
  
  # Table reacts to both dropdowns
  output$readmission_table <- DT::renderDataTable({
    req(input$state_readmission)
    
    df <- df_combined
    
    if (input$state_readmission != "All") {
      df <- df %>% filter(State == input$state_readmission)
    }
    
    if (isTruthy(input$facility_readmission) && input$facility_readmission != "All") {
      df <- df %>% filter(`Facility Name` == input$facility_readmission)
    }
    
    df %>%
      select(`Facility Name`, State, `Total Discharges`,
             `Total Readmissions`, `Avg Predicted Readmission Rate`) %>%
      arrange(`Avg Predicted Readmission Rate`) %>%
      DT::datatable(options = list(pageLength = 15), rownames = FALSE)
  })
  
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
  
  # SUmmary cards
  output$readmission_kpi_cards <- renderUI({
    df <- df_combined
    if (input$state_readmission != "All")
      df <- df %>% filter(State == input$state_readmission)
    
    avg_rate   <- round(mean(df$`Avg Predicted Readmission Rate`, na.rm = TRUE), 2)
    total_read <- scales::comma(sum(df$`Total Readmissions`, na.rm = TRUE))
    total_disc <- scales::comma(sum(df$`Total Discharges`, na.rm = TRUE))
    n_hosp     <- nrow(df)
    make_card <- function(icon, label, value, color) {
      div(style = paste0(
        "background:#fff; border-radius:10px; padding:18px 20px;",
        "border-top:5px solid ", color, ";",
        "box-shadow:1px 2px 6px rgba(0,0,0,0.08);",
        "text-align:center; margin:6px;"),
        div(style = "font-size:26px;", icon),
        div(style = paste0("font-size:24px; font-weight:bold; color:", color, ";"), value),
        div(style = "font-size:12px; color:#777; margin-top:4px;", label)
      )
    } 
    fluidRow(
      column(3, make_card("🏥", "Hospitals", n_hosp, "#3498db")),
      column(3, make_card("📤", "Total Discharges", total_disc, "#9b59b6")),
      column(3, make_card("🔄", "Total Readmissions", total_read, "#e67e22")),
      column(3, make_card("📊", "Avg Predicted Rate", paste0(avg_rate, "%"), "#e74c3c"))
    )
  })
  output$readmission_scorecard <- renderUI({
    df <- df_combined
    
    if (input$state_readmission != "All")
      df <- df %>% filter(State == input$state_readmission)
    
    if (!is.null(input$search_readmission) && input$search_readmission != "")
      df <- df %>% filter(grepl(input$search_readmission, `Facility Name`, ignore.case = TRUE))
    
    df <- df %>%
    mutate(
      Risk_Tier = case_when(
        `Avg Predicted Readmission Rate` >= 20 ~ "High Risk",
        `Avg Predicted Readmission Rate` >= 17 ~ "Moderate Risk",
        `Avg Predicted Readmission Rate` >= 14 ~ "Low Risk",
        TRUE                                   ~ "Very Low Risk"
      ),
      tier_color = case_when(
        Risk_Tier == "High Risk"     ~ "#e74c3c",
        Risk_Tier == "Moderate Risk" ~ "#e67e22",
        Risk_Tier == "Low Risk"      ~ "#f1c40f",
        TRUE                         ~ "#2ecc71"
      ),
      tier_icon = case_when(
        Risk_Tier == "High Risk"     ~ "🔴",
        Risk_Tier == "Moderate Risk" ~ "🟠",
        Risk_Tier == "Low Risk"      ~ "🟡",
        TRUE                         ~ "🟢"
      )
    ) %>%
    arrange(desc(`Avg Predicted Readmission Rate`))
  
  if (nrow(df) == 0) return(p("No hospitals match your filters."))
  
  tiers <- c("High Risk", "Moderate Risk", "Low Risk", "Very Low Risk")
  tier_sections <- lapply(tiers, function(tier) {
    tier_df <- df %>% filter(Risk_Tier == tier)
    if (nrow(tier_df) == 0) return(NULL)
    
    color <- tier_df$tier_color[1]
    icon  <- tier_df$tier_icon[1]
    
    cards <- lapply(1:nrow(tier_df), function(i) {
      rate  <- round(tier_df$`Avg Predicted Readmission Rate`[i], 1)
      readm <- scales::comma(tier_df$`Total Readmissions`[i])
      disc  <- scales::comma(tier_df$`Total Discharges`[i])
      column(4,
             div(style = paste0(
               "background:#fff; border-radius:10px; padding:15px;",
               "border-left:5px solid ", color, ";",
               "box-shadow:1px 2px 5px rgba(0,0,0,0.07); margin:6px 0;"),
               div(style = "font-size:13px; font-weight:bold; color:#333; margin-bottom:8px;",
                   tier_df$`Facility Name`[i]),
               div(style = "font-size:11px; color:#888; margin-bottom:10px;",
                   tier_df$State[i]),
               div(style = paste0("font-size:28px; font-weight:bold; color:", color, ";"),
                   paste0(rate, "%")),
               div(style = "font-size:11px; color:#aaa;", "Predicted Readmission Rate"),
               hr(style = "margin:8px 0;"),
               div(style = "display:flex; justify-content:space-between; font-size:12px; color:#555;",
                   div(tags$b(readm), br(), "Readmissions"),
                   div(style = "text-align:right;", tags$b(disc), br(), "Discharges")
               )
             )
      )
    })
    tagList(
      div(style = paste0(
        "background:", color, "22; border-left:4px solid ", color, ";",
        "padding:10px 16px; border-radius:6px; margin:20px 0 10px 0;",
        "font-weight:bold; font-size:15px; color:", color, ";"),
        paste0(icon, " ", tier, " — ", nrow(tier_df), " hospital(s)")
      ),
      fluidRow(cards)
    )
  })
  
  do.call(tagList, tier_sections)
})
# 3. Detailed table with color-coded risk tier
output$readmission_table <- DT::renderDataTable({
  df <- df_combined
  
  if (input$state_readmission != "All")
    df <- df %>% filter(State == input$state_readmission)
  if (isTruthy(input$facility_readmission) && input$facility_readmission != "All")
    df <- df %>% filter(`Facility Name` == input$facility_readmission) 
  df %>%
    mutate(Risk_Tier = case_when(
      `Avg Predicted Readmission Rate` >= 20 ~ "🔴 High Risk",
      `Avg Predicted Readmission Rate` >= 17 ~ "🟠 Moderate Risk",
      `Avg Predicted Readmission Rate` >= 14 ~ "🟡 Low Risk",
      TRUE                                   ~ "🟢 Very Low Risk"
    )) %>%
    select(`Facility Name`, State, `Total Discharges`,
           `Total Readmissions`, `Avg Predicted Readmission Rate`, Risk_Tier) %>%
    arrange(`Avg Predicted Readmission Rate`) %>%
    DT::datatable(options = list(pageLength = 15), rownames = FALSE) %>%
    DT::formatStyle("Avg Predicted Readmission Rate",
                    color = DT::styleInterval(c(14, 17, 20),
                                              c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c")),
                    fontWeight = "bold"
    )
})
# Render the hospital card
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
    hosp_data <- filter(hai_cleaned, Facility.Name == input$facility_hai)
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
output$facility_dropdown <- renderUI({
  req(input$state_hai != "")
  
  facilities <- hai_cleaned %>%
    filter(State == input$state_hai) %>%
    pull(Facility.Name) %>%
    unique() %>%
    sort()
  
  selectInput("facility_hai", "Select a Hospital:",
              choices = c("Select a hospital..." = "", facilities),
              selected = "")
})
output$summary_dt <- DT::renderDataTable({
  state_data <- if (input$state_hai == "All") hai_cleaned else filter(hai_cleaned, State == input$state_hai)
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
      "Hospital"       = Facility.Name,
      "State"          = State,
      "Safety Rating"  = Safety_Rating,
      "Summary"        = Summary
    )
}, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
# Nudge button — fixed req() check
output$compare_nudge <- renderUI({
  req(
    isTruthy(input$facility_hai),
    input$facility_hai != "",
    input$facility_hai != "Select a hospital..."
  )
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
    avg_sir  <- round(mean(inf_data$Score, na.rm = TRUE), 2)
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
             div(style = paste0("font-size:36px; font-weight:bold; color:", color, ";"), avg_sir),
             div(style = paste0(
               "display:inline-block; background:", color,
               "; color:white; border-radius:12px;",
               "padding:3px 12px; font-size:12px; margin-top:6px;"
             ), label),
             p("Compared to the national average",
               style = "font-size:11px; color:#aaa; margin-top:8px;")
           )
    )
  })
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
     hospitalIcon <- makeIcon(
       iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ccircle cx='50' cy='50' r='50' fill='%23c8d8e8'/%3E%3Ctext y='.85em' x='.1em' font-size='80'%3E%F0%9F%8F%A5%3C/text%3E%3C/svg%3E",
       iconWidth = 45, iconHeight = 45
     )
     
     leaflet(SurgCenters) %>%
       # ⬅️ ADDED - setView to center and zoom the map on load
       setView(lng = mean(SurgCenters$longitude, na.rm = TRUE),
               lat = mean(SurgCenters$latitude, na.rm = TRUE), zoom = 4) %>%
       addTiles() %>%
       # ⬅️ CHANGED - addAwesomeMarkers replaced with addMarkers using custom icon
       addMarkers(lng = ~longitude, lat = ~latitude, icon = hospitalIcon,
                  popup = ~paste0("<b>", `Facility Name`, "</b><br>", full_address,
                                  # ⬅️ ADDED - Get Directions link in popup
                                  "<br><a href='https://www.google.com/maps/dir/?api=1&destination=",
                                  latitude, ",", longitude, "' target='_blank'>Get Directions</a>"))
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
