
library(tidyverse)

VA_IPF_cleaned <- read.csv("VA_IPF_cleaned.csv")

# Check what it looks like first
head(VA_IPF_cleaned)
colnames(VA_IPF_cleaned)

VA_IPF_cleaned <- VA_IPF_cleaned %>%
  select(-Measure.ID, -Measure.Name,-Condition, -Score) %>%
  distinct()

library(tidygeocoder)

VA_IPF_cleaned <- read.csv("VA_IPF_cleaned.csv") %>%
  select(-Measure.ID, -Measure.Name, -Score) %>%
  distinct()

VA_IPF_geocoded <- VA_IPF_cleaned %>%
  unite("full_address", Address, City.Town, State, ZIP.Code, sep = ", ", remove = FALSE) %>%
  geocode(full_address, method = "osm", lat = lat, long = lon)
