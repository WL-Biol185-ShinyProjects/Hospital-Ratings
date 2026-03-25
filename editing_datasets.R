library(tidyverse)
environmentName(environment())

#tidying va ipf
VA_IPF[["Facility.ID"]] <-NULL
VA_IPF[["Start.Date"]] <-NULL
VA_IPF[["End.Date"]] <-NULL
VA_IPF[["Sample"]] <- NULL
VA_IPF[["Footnote"]] <- NULL

#what's this??
VA_IPF <- read.csv("VA_IPF.csv")
#saving file as new
write.csv(VA_IPF, "VA_IPF_cleaned.csv", row.names = FALSE)
VA_IPF <- read.csv("VA_IPF_cleaned.csv")

#fixing errors:
names(VA_IPF)

getwd()
ls()
write.csv(VA_IPF, "VA_IPF_cleaned.csv", row.names = FALSE)


#Tidying Timely and Effect...
library(readr)
TimelyEffectiveCare <- read_csv("Timely_and_Effective_Care_Hospital.csv")
#read_csv keeps spaces in titles
View(Timely_and_Effective_Care_Hospital)
View(TimelyEffectiveCare)
TimelyEffectiveCare[["Facility ID"]] <-NULL
#able to use space because used _ in read code
TimelyEffectiveCare[["Start Date"]] <-NULL
TimelyEffectiveCare[["End Date"]] <-NULL
TimelyEffectiveCare[["Sample"]] <-NULL
TimelyEffectiveCare[["Footnote"]] <-NULL
#saving file as new
write.csv(TimelyEffectiveCare, "Timely_Effective_Care_cleaned.csv", row.names = FALSE)

#tidying locations (surgery centers)
library(tidyverse)
library(readr)
locations[["Start Date"]] <- NULL
locations[["Facility ID"]] <- NULL
locations[["End Date"]] <- NULL 
locations[["Address"]] <- NULL
locations[["City/Town"]] <- NULL
locations[["State"]] <- NULL
locations[["Zip Code"]] <- NULL
locations[["County/Parish"]] <- NULL

#saving tidied
write.csv(locations, "SurgCenters.csv", row.names = FALSE)

#minimalizing all table data for directory (HAVE NOT STARTED)
library(dplyr)
VA_IPF_geocoded           <- read.csv("VA_IPF_geocoded.csv")
Timely_Effective_Care     <- read.csv("Timely_Effective_Care_cleaned.csv")
SurgCenters               <- read.csv("SurgCenters.csv")
staff_rating              <- read.csv("staff_rating.csv")
person_community_engage <- read.csv("hvbp_person_and_community_engagement.csv")
hosp_info               <- read.csv("hosp.general.info.csv")
HAI                     <- read.csv("hai_cleaned.csv")
readmissions            <- read.csv("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital.csv")
birthing                <- read.csv("Birthing_Friendly_Hospitals_Geocoded.csv")


combined <- bind_rows(
  VA_IPF_geocoded         %>% select(-any_of(c("Facility.ID", "Score"))),
  Timely_Effective_Care   %>% select(-any_of(c("Facility.ID", "Score"))),
  SurgCenters             %>% select(-any_of(c("Facility.ID", "Score"))),
  staff_rating            %>% select(-any_of(c("Facility.ID", "Score"))),
  person_community_engage %>% select(-any_of(c("Facility.ID", "Score"))),
  hosp_info               %>% select(-any_of(c("Facility.ID", "Score"))),
  HAI                     %>% select(-any_of(c("Facility.ID", "Score"))),
  readmissions            %>% select(-any_of(c("Facility.ID", "Score"))),
  birthing                %>% select(-any_of(c("Facility.ID", "Score")))
) %>%
  mutate(
    latitude  = coalesce(latitude, lat),
    longitude = coalesce(longitude, lon)
  ) %>%
  select(-any_of(c("lat", "lon"))) %>%
  mutate(
    Facility.Name     = coalesce(Facility.Name, name),
    full_address      = coalesce(full_address, addr),
    City.Town         = coalesce(City.Town, city),
    State             = coalesce(State, state),
    ZIP.Code          = coalesce(ZIP.Code, zip),
    Telephone.Number  = coalesce(Telephone.Number, phone)
  ) %>%
  select(-any_of(c("name", "addr", "city", "state", "zip", "phone"))) %>%
  distinct(Facility.Name, .keep_all = TRUE)
  
view(combined)
colnames(combined)

  distinct(Facility.Name, .keep_all = TRUE)
#combining lat and long columns
combined <- combined %>%
  mutate(
    latitude  = coalesce(latitude, lat),
    longitude = coalesce(longitude, lon)
  ) %>%
  select(-lat, -lon)
write.csv(combined, "combined.csv", row.names = FALSE)

# FOR DIRECTORY DATA: need to rerun
install.packages("tidygeocoder")
library(tidygeocoder)
> 
  > # Geocode only the missing rows
  > missing_coords <- directory %>% 
  +     filter(is.na(latitude) | is.na(longitude)) %>%
  +     geocode(address = full_address, method = "osm", lat = lat_new, long = lon_new)
# Join back into the main table
directory <- directory %>%
  left_join(missing_coords %>% select(Facility.Name, lat_new, lon_new), 
            by = "Facility.Name") %>%
  mutate(
    latitude  = ifelse(is.na(latitude), lat_new, latitude),
    longitude = ifelse(is.na(longitude), lon_new, longitude)
  ) %>%
  select(-lat_new, -lon_new)