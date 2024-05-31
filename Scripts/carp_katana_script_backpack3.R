# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Rivers.csv")

i = as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
i
paste("This is job number",i)
# for(i in 1:nrow(rivers)){

catch <- read.csv("clean waterbody_catch_11_08_2023_with_fixed_WRPA.csv") %>% # filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == rivers$Rivers[i])

site_dets <- all_data %>% select(SiteName, SiteID) %>% distinct()

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
  filter(Method == "BTE" |Method == "BPE") %>% 
  select(-1,-2,-3,-project_segment) %>%
  distinct()

ab <- read_csv("Data/a b values.csv")

bio <- read_csv("../../Murray cod recruitment/All bio_11_08_2023.csv")  %>% left_join(ab) %>%
  mutate(CalcWeight = 10^(a)*Length_mm^b)

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(SamplingRecordID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

# method change
meths <- catch2 %>% select(SamplingRecordID, Method) %>% distinct() %>%
  group_by(SamplingRecordID) %>% summarise(method_N =n())
table(meths$method_N)

catch2 <- catch2 %>% left_join(meths) %>% mutate(Method = case_when(method_N == 2 ~ "Hybrid",
                                                                    T ~ Method)) %>%
  select(-method_N)

catch2 <- catch2 %>% select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method,Taxa_biomass) %>% # ,Total_biomass, Biomass_proportion
  group_by(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method) %>%
  summarise(Taxa_biomass = sum(Taxa_biomass, na.rm=T)) %>% ungroup() %>% left_join(catch_total_biomass) %>%
  filter(Total_biomass >0) %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method,Taxa_biomass,Total_biomass, Biomass_proportion)


# yy <- catch2 %>%
#   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# hist(yy$SampleDate, breaks = "years")
# xx <- catch %>% filter(OperationID %in% yy$OperationID)

catch2 <- catch2 %>%# mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method, Biomass_proportion)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(6:last_col()), na.rm=T))

# table(catch2_wide$Total_percent)

catch2_wide$`Common carp` <- replace_na(catch2_wide$`Common carp`,0)

catch2_wide$Date <- as.Date(catch2_wide$SampleDate)
catch2_wide$Year <- lubridate::year(catch2_wide$Date)
catch2_wide$Month <- lubridate::month(catch2_wide$Date)


# table(catch2_wide$Method)
#
# hist(catch2_wide$Year)
# hist(catch2_wide$Month)

catch2_wide <- catch2_wide %>% filter(Total_percent == 1) %>%
  mutate(Year_ending_June = case_when(Month < 7 ~ Year,
                                      T ~ Year + 1),
         fYear = as.factor(as.character(Year_ending_June))) %>%
  filter(fYear != "1992") %>% filter(fYear != "1993") %>% filter(fYear != "1994")
#mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`),

#write_csv(catch2_wide, "Data/2023 formatted wide catch data with backpack.csv")

n_distinct(catch2_wide$SiteID)
tempt <- catch2_wide %>% mutate(events = paste(SampleDate, SiteID))
n_distinct(tempt$events)

# sss <- catch2_wide %>% group_by(Method) %>% summarise(mean_carp_prop = mean(`Common carp`, na.rm=T),
#                                                       sd_carp_prop = sd(`Common carp`, na.rm=T))
# 
# sss

# library(glmmTMB)
#
# f1 <- glmmTMB(`Common carp` ~ 1 + fYear + (1|Date) + (1|SiteID),
#               data = catch2_wide, family = beta_family())
#
# summary(f1)


#
# library(DHARMa)
# resids <- simulateResiduals(f1)
# plot(resids)
#



library(brms)
catch2_wide$Response <- catch2_wide$`Common carp`
catch2_wide$Response2 <- catch2_wide$`Murray cod`
catch2_wide$FDate <- as.factor(as.character(catch2_wide$Date))
catch2_wide$SiteID <- as.factor(as.character(catch2_wide$SiteID))
table(catch2_wide$Response)
priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))

f2 <- brm(Response ~ fYear + (1|Method) + (1|FDate) + (1|SiteID), #  (1|MethodType)
          data = catch2_wide, family = zero_one_inflated_beta(),

          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=10000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = paste0("2024 site level backpack ",rivers$Rivers[i]," day site random.rds"))

# }