library(leaflet)
library(tidygeocoder)
library(dplyr)
library(tidyr)

#coding surg ctrs
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
      icon  = Icon
    )
      popup = ~paste0(
        "<b>", `Facility Name`, "</b><br>",
        full_address
      )


#to view:
SurgMap

#icon:
emoji <- "🏥"

Icon <- makeIcon(
  iconUrl = paste0(
    "data:image/svg+xml,",
    URLencode(paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>",
      "<text y='.9em' font-size='90'>", emoji, "</text>",
      "</svg>"
    ))
  ),
  iconWidth  = 35,
  iconHeight = 35
)

#opening and saving new data set- needto finish
View(locations)
write.csv(locations, "locations.csv", row.names = FALSE)
file.rename("locations.csv", "surg_centers")


#CODING DIRECTORY
#obtaining coordinates(didn't work cause it took 2 hours? Figure something else out)
Hospital_locations <- Hospital_General_Information_1_ %>%
  unite("full_address", `Address`, `City/Town`, `State`, `ZIP Code`,
        sep = ", ", remove = FALSE, na.rm = TRUE) %>%
  geocode(full_address, method = "osm", lat = latitude, long = longitude)