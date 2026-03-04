# Load libraries
library(tidyverse)
library(shiny)

# Load data
staff_rating <-ASCQR_OAS_CAHPS_BY_ASC

#cleaning columns 
staff_rating <- staff_rating %>% select( - `Facility ID`)
staff_rating <- staff_rating %>% select( - `County/Parish`)
staff_rating <- staff_rating %>% select( - `Facilities and staff linear mean score`)
staff_rating <- staff_rating %>% select( - `Patients'rating of the facility linear mean score`)
staff_rating <- staff_rating %>% select( - `Patients reccomending the facility linear mean score`)
staff_rating <- staff_rating %>% select( - `Footnote`)
staff_rating <- staff_rating %>% select( - `Number of Completed Surveys`)
staff_rating <- staff_rating %>% select( - `Survey Response Percent Rate`)
staff_rating <- staff_rating %>% select( - `Start Date`)
staff_rating <- staff_rating %>% select( - `End Date`)

staff_rating <- staff_rating[, -c(13, 17, 21, 22, 26, 27)]

