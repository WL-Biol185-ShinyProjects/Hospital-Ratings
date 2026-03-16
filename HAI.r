library(tidyverse)

hai <- read.csv("Healthcare_Associated_Infections-Hospital.csv")

hai_cleaned <- hai %>%
  # Keep only the main SIR rows, drop confidence limits, counts, etc.
  filter(Measure.ID %in% c("HAI_1_SIR", "HAI_2_SIR", "HAI_3_SIR", 
                           "HAI_4_SIR", "HAI_5_SIR", "HAI_6_SIR")) %>%
  # Give them readable names
  mutate(Infection.Type = recode(Measure.ID,
                                 "HAI_1_SIR" = "CLABSI",           # Central Line
                                 "HAI_2_SIR" = "CAUTI",            # Urinary Tract
                                 "HAI_3_SIR" = "SSI Colon",        # Colon Surgery
                                 "HAI_4_SIR" = "SSI Hysterectomy", # Hysterectomy
                                 "HAI_5_SIR" = "MRSA",             # MRSA
                                 "HAI_6_SIR" = "C.Diff"            # C. Difficile
  )) %>%
  # Convert Score to numeric (some may be "Not Available")
  mutate(Score = as.numeric(Score)) %>%
  # Drop columns you won't need
  select(Facility.ID, Facility.Name, Address, City.Town, State, 
         ZIP.Code, Telephone.Number, Infection.Type, 
         Score, Compared.to.National) %>%
  # Drop rows with no score
  filter(!is.na(Score))

write.csv(hai_cleaned, "hai_cleaned.csv", row.names = FALSE)
message("Done! Rows: ", nrow(hai_cleaned))