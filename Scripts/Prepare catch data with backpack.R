# make catch data contain waterbody name
library(tidyverse)

catch_all <- read_csv("C:/Users/schilh01/OneDrive - DPIE/All e-catch_30_11_2022_WRPA_fixed.csv")

site_dets <- read_csv("Sites and waterbodyID dictionary.csv")

waterbods <- read_csv("Waterbody dictionary.csv") %>% rename(WaterbodyID = WaterBodyID)

catch <- catch_all %>% filter(Method == "BTE" | Method == "BPE") %>% left_join(site_dets) %>% left_join(waterbods)

missing_dets <- read_csv("Sites with no previous waterbody name.csv") %>% rename(WaterbodynameFixed = WaterbodyName)

catch <- catch %>% left_join(missing_dets)

catch_F <- catch %>% mutate(WaterbodyName = case_when(!is.na(WaterbodyName) ~ WaterbodyName,
                                                      T ~ WaterbodynameFixed))

sum(is.na(catch_F$WaterbodyName))

missing <- catch_F %>% filter(is.na(WaterbodyName)) %>% distinct(SiteName, SiteID)
write_csv(missing, "backpack boat sites with no waterbody.csv") # all test sites 

write_csv(catch_F, "clean boat and backpack electro catch with waterbody.csv")

# Test 
# namoi <- catch_F %>% filter(WaterbodyName == "Namoi River")
