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

#minimalizing all table data for directory
library(dplyr)
combined <- VA_IPF %>%
  full_join(Timely_Effective_Care_cleaned,by = "Facility.Name")