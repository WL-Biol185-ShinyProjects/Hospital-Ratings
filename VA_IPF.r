library(tidyverse)
library(tidygeocoder)

VA_IPF_geocoded <- read.csv("VA_IPF_cleaned.csv") %>%
  select(-Measure.ID, -Measure.Name, -Condition, -Score) %>%
  distinct() %>%
  unite("full_address", Address, City.Town, State, ZIP.Code, sep = ", ", remove = FALSE) %>%
  geocode(full_address, method = "osm", lat = lat, long = lon)

write.csv(VA_IPF_geocoded, "VA_IPF_geocoded.csv", row.names = FALSE)
message("Done!")
