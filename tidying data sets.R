# Tidying VA IPF
library(tidyverse)
VA_IPF[["Facility ID"]] <-NULL
VA_IPF[["Start Date"]] <-NULL
VA_IPF[["End Date"]] <-NULL
VA_IPF <- read.csv("VA_IPF.csv")
VA_IPF[["Facility ID"]] <- NULL
VA_IPF[["Start Date"]] <- NULL
VA_IPF[["End Date"]] <- NULL
write.csv(VA_IPF, "VA_IPF.csv", row.names = FALSE)
VA_IPF <- read.csv("VA_IPF_cleaned.csv")

getwd()
ls()
write.csv(VA_IPF, "VA_IPF_cleaned.csv", row.names = FALSE)
#Tidying Timely and Effect...
  #none of this is in the shared file pls figure this out next time!!
TimelyEffectiveCare <- Timely_and_Effective_Care_Hospital
TimelyEffectiveCare[["Facility ID"]] <- NULL
colnames(TimelyEffectiveCare)
TimelyEffectiveCare[["Facility ID"]] <- NULL

