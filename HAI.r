library(tidyverse)

hai <- read.csv("Healthcare_Associated_Infections-Hospital.csv")

hai_cleaned <- hai %>%
  # Keep only the main SIR rows, drop confidence limits, counts, etc.
  filter(Measure.ID %in% c("HAI_1_SIR", "HAI_2_SIR", "HAI_3_SIR", 
                           "HAI_4_SIR", "HAI_5_SIR", "HAI_6_SIR")) %>%
  # Give them readable names
  mutate(Infection.Type = recode(Measure.ID,
                                 "HAI_1_SIR" = "Central Line Infection",
                                 "HAI_2_SIR" = "Urinary Tract Infection",
                                 "HAI_3_SIR" = "Surgical Site - Colon",
                                 "HAI_4_SIR" = "Surgical Site - Hysterectomy",
                                 "HAI_5_SIR" = "MRSA Blood Infection",
                                 "HAI_6_SIR" = "C. Difficile Infection"
  )) %>%
  mutate(Score = as.numeric(Score)) %>%
  select(Facility.ID, Facility.Name, Address, City.Town, State,
         ZIP.Code, Telephone.Number, Infection.Type,
         Score, Compared.to.National) %>%
  filter(!is.na(Score))

write.csv(hai_cleaned, "hai_cleaned.csv", row.names = FALSE)
message("Done!")

