library(leaflet)
library(tidygeocoder)
library(dplyr)
library(tidyr)

locations <- Surgery_centers %>%
  unite("full_address", `Address`, `City/Town`, `State`, `ZIP Code`,
        sep = ", ", remove = FALSE, na.rm = TRUE) %>%
  geocode(full_address, method = "osm", lat = latitude, long = longitude)

# SurgCenters = final edited version of locations
# naming map code
SurgMap <- 
  leaflet(SurgCenters) %>%
    addTiles() %>%
    addMarkers(
      lng   = ~longitude,
      lat   = ~latitude,
      popup = ~paste0(
        "<b>", `Facility Name`, "</b><br>",
        full_address
      )
    )
#to view:
SurgMap

#opening and saving new data set- needto finish
View(locations)
write.csv(locations, "locations.csv", row.names = FALSE)
file.rename("locations.csv", "surg_centers")