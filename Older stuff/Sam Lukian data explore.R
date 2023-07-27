# for Sam, Lukian

library(tidyverse)

mydata <- read_csv("clean boat electro catch with waterbody.csv")

data_sum <- mydata %>% group_by(SiteID, WaterbodyName, coords.x1, coords.x2) %>%
  summarise(N_surveys = n_distinct(SamplingRecordID),
            earliest = min(SampleDate),
            latest = max(SampleDate))
sum(data_sum$N_surveys)

write_csv(data_sum, "Sam Lukian data summary.csv")
