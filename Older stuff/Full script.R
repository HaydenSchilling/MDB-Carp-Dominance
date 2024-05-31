# Larger analyses
library(tidyverse)
library(lubridate)

catch <- read_csv("clean waterbody_catch_11_08_2023_with_fixed_WRPA.csv") %>% # filter(Method == "Boat Electrofishing")
mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == "Murray River")

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing

#all_data
catch <- catch %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>%
  filter(Method == "BTE"| Method == "BPE") %>% select(-1,-2,-3,-project_segment) %>%
  distinct()

ab <- read_csv("Data/a b values.csv")

bio <- read_csv("../../Murray cod recruitment/All bio_11_08_2023.csv")  %>% left_join(ab) %>%
  mutate(CalcWeight = 10^(a)*Length_mm^b)

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(OperationID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

catch2 <- catch2 %>% left_join(catch_total_biomass)

catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID, CommonName, OperationID, Method, Biomass_proportion)

# yy <- catch2 %>%
#   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# hist(yy$SampleDate, breaks = "years")
# xx <- catch %>% filter(OperationID %in% yy$OperationID)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(5:last_col()), na.rm=T))

table(catch2_wide$Total_percent)

catch2_wide$`Common carp` <- replace_na(catch2_wide$`Common carp`,0)

catch2_wide$Date <- as.Date(catch2_wide$SampleDate)
catch2_wide$Year <- year(catch2_wide$Date)
catch2_wide$Month <- month(catch2_wide$Date)


table(catch2_wide$Method)

hist(catch2_wide$Year)
hist(catch2_wide$Month)

catch2_wide <- catch2_wide %>% filter(Total_percent == 1) %>% 
  mutate(Year_ending_June = case_when(Month < 7 ~ Year,
                                      T ~ Year + 1),
         fYear = as.factor(as.character(Year))) #%>%
  #mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
  #                                `Common carp` == 0 ~ 0.000000001,
  #                                T ~ `Common carp`),
       


#### Now group by SampleID not OperationID
# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Data/Rivers.csv")

i = 9#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
# i
# paste("This is job number",i)

full_data <- data.frame()

 for(i in 1:nrow(rivers)){

catch <- read_csv("clean waterbody_catch_11_08_2023_with_fixed_WRPA.csv") %>% # filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == rivers$Rivers[i])

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing

#all_data
catch <- catch %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>%
  filter(Method == "BTE"| Method == "BPE") %>% select(-1,-2,-3,-project_segment) %>%
  distinct()

ab <- read_csv("Data/a b values.csv")

bio <- read_csv("../../Murray cod recruitment/All bio_11_08_2023.csv")  %>% left_join(ab) %>%
  mutate(CalcWeight = 10^(a)*Length_mm^b)
bio_summary <- bio %>% group_by(CommonName, SamplingRecordID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(SamplingRecordID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

catch2 <- catch2 %>% left_join(catch_total_biomass)

catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID, CommonName, SamplingRecordID, Biomass_proportion)

# yy <- catch2 %>%
#   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# hist(yy$SampleDate, breaks = "years")
# xx <- catch %>% filter(OperationID %in% yy$OperationID)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(4:last_col()), na.rm=T))

# table(catch2_wide$Total_percent)

catch2_wide$`Common carp` <- replace_na(catch2_wide$`Common carp`,0)

catch2_wide$Date <- as.Date(catch2_wide$SampleDate)
catch2_wide$Year <- year(catch2_wide$Date)
catch2_wide$Month <- month(catch2_wide$Date)


# table(catch2_wide$Method)
#
# hist(catch2_wide$Year)
# hist(catch2_wide$Month)

catch2_wide <- catch2_wide %>% filter(Total_percent == 1) %>%
  mutate(Year_ending_June = case_when(Month < 7 ~ Year,
                                      T ~ Year + 1),
         fYear = as.factor(as.character(Year_ending_June))) %>%
  mutate(WaterbodyName = rivers$Rivers[i])
#mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`),

full_data <- bind_rows(full_data, catch2_wide)
}
write_csv(full_data, "Biomass proportions by SampleRecordID river level.csv")


#### Now group by SampleID not OperationID (whole basin)
# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Data/Rivers.csv")

i = 9#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
# i
# paste("This is job number",i)

full_data <- data.frame()

#for(i in 1:nrow(rivers)){
  
catch <- read.csv("clean waterbody_catch_11_08_2023_with_fixed_WRPA.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")


bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) 
bad3 <- segments %>% filter(grepl("*Selective*",project_segment))# all extra and selective fishing

catch <- catch %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>%
  filter(Method == "BTE" |Method == "BPE") %>%  # 
  select(-1,-2,-3,-project_segment) %>%
  distinct()


basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

catch <- catch %>% #filter(CommonName == "Golden perch") %>%
  filter(SWWRPANAME_NEW %in% basins)

elevation_dat <- read_csv("Data/Site elevations.csv") %>% rename(SampleLongitude = coords.x1, SampleLatitude = coords.x2)
catch <- catch %>% left_join(elevation_dat) %>% filter(elevation <= 700) %>% select(-elevation, -elev_units)
  
ab <- read_csv("Data/a b values.csv")

bio <- read_csv("../../Murray cod recruitment/All bio_11_08_2023.csv")  %>% left_join(ab) %>%
  mutate(CalcWeight = 10^(a)*Length_mm^b)
#


  
  bio_summary <- bio %>% group_by(CommonName, SamplingRecordID) %>%
    summarise(biomass_mean = mean(CalcWeight, na.rm=T))
  
  catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)
  
  catch_total_biomass <- catch2  %>%
    ungroup() %>% group_by(SamplingRecordID) %>%
    summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))
  
  catch2 <- catch2 %>% left_join(catch_total_biomass)
  
  catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
    select(SampleDate, SiteID, CommonName, SamplingRecordID, Biomass_proportion)
  
  
  # yy <- catch2 %>%
  #   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L)
  # hist(yy$SampleDate, breaks = "years")
  # xx <- catch %>% filter(OperationID %in% yy$OperationID)
  
  catch2_wide <- catch2 %>% ungroup() %>%
    pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
    rowwise() %>% mutate(Total_percent = sum(c_across(4:last_col()), na.rm=T))
  
  # table(catch2_wide$Total_percent)
  
  catch2_wide$`Common carp` <- replace_na(catch2_wide$`Common carp`,0)
  
  catch2_wide$Date <- as.Date(catch2_wide$SampleDate)
  catch2_wide$Year <- year(catch2_wide$Date)
  catch2_wide$Month <- month(catch2_wide$Date)
  
  
  # table(catch2_wide$Method)
  #
  # hist(catch2_wide$Year)
  # hist(catch2_wide$Month)
  
  catch2_wide <- catch2_wide %>% filter(Total_percent == 1) %>%
    mutate(Year_ending_June = case_when(Month < 7 ~ Year,
                                        T ~ Year + 1),
           fYear = as.factor(as.character(Year_ending_June))) #%>%
  #mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
  #                                `Common carp` == 0 ~ 0.000000001,
  #                                T ~ `Common carp`),
  
  full_data <- bind_rows(full_data, catch2_wide)
#}
write_csv(full_data, "Biomass proportions by SampleRecordID basin level.csv")
