
library(tidyverse)

VA_IPF_cleaned <- read.csv("VA_IPF_cleaned.csv")

# Check what it looks like first
head(VA_IPF_cleaned)
colnames(VA_IPF_cleaned)

VA_IPF_cleaned <- VA_IPF_cleaned %>%
  select(-Measure.ID, -Measure.Name,-Condition, -Score) %>%
  distinct()

