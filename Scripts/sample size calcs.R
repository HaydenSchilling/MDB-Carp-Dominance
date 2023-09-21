# Sample size calculation
library(tidyverse)

mydata <- read_csv("Data/2023 formatted wide catch data with backpack.csv")
rivers = read_csv("Data/Rivers.csv")
rivers <- unique(rivers$Rivers)

dat_sum <- mydata %>% filter(WaterbodyName %in% rivers) %>%
  group_by(WaterbodyName, Method) %>% summarise(n=n_distinct(OperationID)) %>%
  pivot_wider(names_from = Method, values_from = n) %>%
  mutate(Total = BTE + BPE,
         Perc_BPE = BPE/Total*100) %>%
  select(-BTE, -BPE)
dat_sum

dat_sum2 <- mydata %>% filter(WaterbodyName %in% rivers) %>% 
  mutate(Event = paste(SampleDate, SiteID)) %>%
  group_by(WaterbodyName) %>% summarise(events = n_distinct(Event),
                                        sites=n_distinct(SiteID))
dat_sum2

