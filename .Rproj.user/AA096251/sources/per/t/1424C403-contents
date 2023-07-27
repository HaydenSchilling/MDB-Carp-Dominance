#### Overall NSW Basin
library(tidyverse)
library(lubridate)

#all_data <- read.csv("../Long term abundance/All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))
all_data <- read.csv("All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))


segments <- all_data %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")
#all_data
all_data <- all_data %>% filter(!project_segment %in% bad_list) %>% select(-1,-2,-3,-project_segment) %>%
  distinct() %>%
  filter(Method == "BTE"| Method == "BPE")

# back <- all_data %>% filter(!project_segment %in% bad_list) %>% select(-1,-2,-3,-project_segment) %>%
#   distinct() %>%
#   filter(Method == "BPE")
# 
# boat <- all_data %>% filter(!project_segment %in% bad_list) %>% select(-1,-2,-3,-project_segment) %>%
#   distinct() %>%
#   filter(Method == "BTE")

basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

all_data <- all_data %>% #filter(CommonName == "Golden perch") %>%
  filter(SWWRPANAME_NEW %in% basins)


elevation_dat <- read_csv("Site elevations.csv")
all_data <- all_data %>% left_join(elevation_dat) %>% filter(elevation <= 700) %>% select(-elevation, -elev_units)

# 
#### Just checking this dataset is the same (it is but above has SWRPANAME)

# catch <- read_csv("clean boat electro catch with waterbody.csv") %>% # filter(Method == "Boat Electrofishing")
#   mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()# %>% filter(WaterbodyName == "Murray River")
# 
# segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)
# 
# bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
#               "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
#               "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
#               "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
#               "Murray Cod Slot Limit Assessment:2020/Extra")
# 
# catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(Method == "BTE") %>% select(-1,-2,-3,-project_segment) %>%
#   distinct()



bio <- read_csv("All bio_27_01_2022.csv") %>% mutate(CalcWeight = 10^(a)*Length_mm^b)

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- all_data %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(OperationID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

catch2 <- catch2 %>% left_join(catch_total_biomass)

catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, OperationID, Method, Biomass_proportion)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(6:last_col()), na.rm=T))

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
          fYear = as.factor(as.character(Year_ending_June)))%>%
  filter(fYear != "1992") %>% filter(fYear != "1993") %>% filter(fYear != "1994") %>% filter(fYear!=2023)
# mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`))

table(catch2_wide$Method)

#write_csv(catch2_wide, "Formatted catch wide efish with backpack.csv")

# n_distinct(catch2_wide$SiteID)
# tempt <- catch2_wide %>% mutate(events = paste(SampleDate, SiteID))
# n_distinct(tempt$events)

# 
# library(glmmTMB)
# 
# f1 <- glmmTMB(`Common carp` ~ 1 + fYear + (1|Date) + (1|SiteID),
#               data = catch2_wide, family = beta_family())
# 
# summary(f1)
# 
# f2 <- glmmTMB(`Common carp` ~ 1 + fYear + (1|Date) + (1|SiteID) +(1|SWWRPANAME_NEW),
#               data = catch2_wide, family = beta_family())
# 
# summary(f2)
# 
# library(DHARMa)
# resids <- simulateResiduals(f2)
# plot(resids)



library(brms)
catch2_wide$Response <- catch2_wide$`Common carp`
catch2_wide$Response2 <- catch2_wide$`Murray cod`
catch2_wide$FDate <- as.factor(as.character(catch2_wide$Date))
catch2_wide$SiteID <- as.factor(as.character(catch2_wide$SiteID))
table(catch2_wide$Response)
priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))

f2 <- brm(Response ~ fYear + Method + (1|FDate) + (1|SiteID) + (1|SWWRPANAME_NEW), #  (1|MethodType)
          data = catch2_wide, family = zero_one_inflated_beta(),
          
          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=10000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = "Carp full MDB WRPA day site random with backpack.rds")
