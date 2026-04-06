# Load libraries
library(tidyverse)
library(shiny)

# Load data
staff_rating <-ASCQR_OAS_CAHPS_BY_ASC

staff_rating <- staff_rating %>%
  rename(
    `Facility Name`  = Facility.Name,
    `State`          = State,
    `Staff Care`     = Patients.who.reported.that.staff.definitely.gave.care.in.a.professional.way.and.the.facility.was.clean,
    `Communication`  = Patients.who.reported.that.staff.definitely.communicated.about.what.to.expect.during.and.after.the.procedure,
    `High Rating`    = Patients.who.gave.the.facility.a.rating.of.9.or.10.on.a.scale.from.0..lowest..to.10..highest.,
    `Would Recommend`= Patients.who.reported.YES.they.would.DEFINITELY.recommend.the.facility.to.family.or.friends
  ) %>%
  select(`Facility Name`, State, `Staff Care`, `Communication`, 
         `High Rating`, `Would Recommend`) %>%
  mutate(across(c(`Staff Care`, `Communication`, `High Rating`, `Would Recommend`),
                ~ as.numeric(as.character(.x))))

write.csv(staff_rating, "staff_rating.csv", row.names = FALSE)