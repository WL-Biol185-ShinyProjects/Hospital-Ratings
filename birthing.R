library(tidyverse)
library(tidygeocoder)
hosp_general <- read.csv("hosp.general.info.csv", check.names = TRUE)
birthing <- hosp_general %>%
  select(
    name  = Facility.Name,
    addr  = Address,
    city  = City.Town,
    state = State,
    zip   = ZIP.Code,
    phone = Telephone.Number,
    meets_criteria = Meets.criteria.for.birthing.friendly.designation
  ) %>%
  filter(meets_criteria == "Y")


nrow(birthing)


birthing_geocoded <- birthing %>%
  geocode(
    street     = addr,
    city       = city,
    state      = state,
    postalcode = zip,
    method     = "census"
  ) %>%
  rename(lon = long)

sum(!is.na(birthing_geocoded$lat))

birthing_geocoded <- birthing_geocoded %>%
  filter(!is.na(lat) & !is.na(lon))

# -------------------------------------------------------
write.csv(birthing_geocoded, "Birthing_Friendly_Hospitals_Geocoded.csv", row.names = FALSE)

nrow(birthing_geocoded)
colnames(birthing_geocoded)