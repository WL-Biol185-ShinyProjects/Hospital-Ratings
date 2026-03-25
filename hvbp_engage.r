
library(tidyverse)
library(shiny)
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
  mutate(across(c(nurse_comm, doctor_comm, staff_resp, care_trans,
                  med_comm, cleanliness, discharge, overall),
                ~ as.numeric(gsub("%", "", .))),
         base_score  = as.numeric(base_score),
         consistency = as.numeric(consistency))

