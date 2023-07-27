# Larger analyses
library(tidyverse)
library(lubridate)

catch <- read_csv("clean boat electro catch with waterbody.csv") %>% # filter(Method == "Boat Electrofishing")
mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == "Murray River")

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(Method == "BTE") %>% select(-1,-2,-3,-project_segment) %>%
  distinct()

bio <- read_csv("All bio_27_01_2022.csv") %>% mutate(CalcWeight = 10^(a)*Length_mm^b)

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
       




library(glmmTMB)

f1 <- glmmTMB(`Common carp` ~ 1 + fYear + (1|Date) + (1|SiteID),
              data = catch2_wide, family = beta_family())

summary(f1)



library(DHARMa)
resids <- simulateResiduals(f1)
plot(resids)




library(brms)
catch2_wide$Response <- catch2_wide$`Common carp`
catch2_wide$Response2 <- catch2_wide$`Murray cod`
catch2_wide$FDate <- as.factor(as.character(catch2_wide$Date))
catch2_wide$SiteID <- as.factor(as.character(catch2_wide$SiteID))
table(catch2_wide$Response)
priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))

f2 <- brm(Response ~ fYear + (1|FDate) + (1|SiteID), #  (1|MethodType)
          data = catch2_wide, family = zero_one_inflated_beta(),

          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=1000,
          seed = 1234,
          cores=4,
          file_refit = "always",
file = "test Murray day site random_small.rds")


f2 <- readRDS("test Namoi day site random.rds")
summary(f2)
plot(f2)

library(DHARMa)
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(f2)),
  observedResponse = catch2_wide$Response,
  fittedPredictedResponse = apply(t(posterior_epred(f2)), 1, mean),
  integerResponse = TRUE)

plot(model.check)

library(tidybayes)
#yy <- new_data %>% add_epred_draws(f2)

### important to drop levels below
#mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
yy <- f2 %>% epred_draws(newdata = expand_grid(fYear = levels (droplevels(catch2_wide$fYear))),
                         #                        #Method = "Boat Electrofishing",
                         #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                         #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                         #Sampling_duration = 90,
                         re_formula = NA,
                         ndraws=500) %>%
  mutate(Species = "Common carp")

write_csv(yy, "Namoi carp annual prediction biomass_small.csv")


# f3 <- readRDS("test Namoi day site random MC.rds")
# summary(f3)
# 
# yy2 <- f3 %>% epred_draws(newdata = expand_grid(fYear = levels (droplevels(catch2_wide$fYear))),
#                          #                        #Method = "Boat Electrofishing",
#                          #days_since = seq(0, max(mydata_wide$days_since), by = 50),
#                          #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
#                          #Sampling_duration = 90,
#                          re_formula = NA) %>%
#   mutate(Species = "Murray cod")
# 
# write_csv(yy2, "Namoi MC annual prediction biomass.csv")
# 
# yy3 <- bind_rows(yy, yy2)

#zz <- yy %>% filter(SWWRPANAME_NEW == "Lachlan" | SWWRPANAME_NEW =="Murrumbidgee") #filter(days_since > 3000)

ggplot(yy, aes(x=as.numeric(fYear))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))+
  geom_smooth(aes(y= .epred*100))+
  ylab("Predicted Biomass Carp (%)") + xlab("Year")



#### Now group by SampleID not OperationID
# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Rivers.csv")

i = 9#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
# i
# paste("This is job number",i)

full_data <- data.frame()

 for(i in 1:nrow(rivers)){

catch <- read_csv("clean boat and backpack electro catch with waterbody.csv") %>% # filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == rivers$Rivers[i])

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(Method == "BTE"|Method == "BPE") %>% select(-1,-2,-3,-project_segment) %>%
  distinct()

bio <- read_csv("All bio_27_01_2022.csv") %>% mutate(CalcWeight = 10^(a)*Length_mm^b)

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


#### Now group by SampleID not OperationID (whoel basin)
# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Rivers.csv")

i = 9#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
# i
# paste("This is job number",i)

full_data <- data.frame()

#for(i in 1:nrow(rivers)){
  
catch <- read.csv("../Long term abundance/All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")
#all_data
catch <- catch %>% filter(!project_segment %in% bad_list) %>% select(-1,-2,-3,-project_segment) %>%
  distinct() %>%
  filter(Method == "BTE"| Method == "BPE")


basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

catch <- catch %>% #filter(CommonName == "Golden perch") %>%
  filter(SWWRPANAME_NEW %in% basins)


elevation_dat <- read_csv("Site elevations.csv")
catch <- catch %>% left_join(elevation_dat) %>% filter(elevation <= 700) %>% select(-elevation, -elev_units)
  
  bio <- read_csv("All bio_27_01_2022.csv") %>% 
    # mutate(a = case_when(CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Murray short-necked turtle" ~ 0,
    #                                                                  CommonName == "NA" ~ NA_integer_,
    #                                                                  CommonName == "Zero catch at DRY site" ~ NA_integer_,
    #                                                                  CommonName == "No catch" ~ NA,
    #                                                                  CommonName == "Freshwater prawn" ~ 0,
    #                                                                  CommonName == "Common carp - Goldfish hybrid" ~ -4.5918,
    #                                                                  CommonName == "Unidentified Maccullochella cod" ~ -5.113,
    #                                                                  CommonName == "Alpine Spiny crayfish" ~ 0,
    #                                                                  CommonName == "Flat-headed galaxias" ~ -5.387,
    #                                                                  CommonName == "Trout Cod / Murray Cod Hybrid" ~ -5.1123,
    #                                                                  CommonName == "Galaxia spp" ~ -5.387,
    #                                                                  CommonName == "Unidentified Euastacus" ~ 0,
    #                                                                  CommonName == "Unidentified Tadpole" ~ 0,
    #                                                                  CommonName == "Shrimp" ~ 0,
    #                                                                  CommonName == "Platypus" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Broad-shelled turtle" ~ 0,
    #                                                                  T ~ a),
    #                                                    b = case_when(CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Murray short-necked turtle" ~ 0,
    #                                                                  CommonName == "NA" ~ NA,
    #                                                                  CommonName == "Zero catch at DRY site" ~ NA,
    #                                                                  CommonName == "No catch" ~ NA,
    #                                                                  CommonName == "Freshwater prawn" ~ 0,
    #                                                                  CommonName == "Common carp - Goldfish hybrid" ~ 2.9476,
    #                                                                  CommonName == "Unidentified Maccullochella cod" ~ 3.0825,
    #                                                                  CommonName == "Alpine Spiny crayfish" ~ 0,
    #                                                                  CommonName == "Flat-headed galaxias" ~ 3.1513,
    #                                                                  CommonName == "Trout Cod / Murray Cod Hybrid" ~ 3.0825,
    #                                                                  CommonName == "Galaxia spp" ~ 3.1513,
    #                                                                  CommonName == "Unidentified Euastacus" ~ 0,
    #                                                                  CommonName == "Unidentified Tadpole" ~ 0,
    #                                                                  CommonName == "Shrimp" ~ 0,
    #                                                                  CommonName == "Platypus" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Eastern long-necked tortoise" ~ 0,
    #                                                                  CommonName == "Broad-shelled turtle" ~ 0,
    #                                                                  T ~ b)) %>%
    mutate(CalcWeight = 10^(a)*Length_mm^b)
  
#a_b <- read_csv("a b missing nums.csv")


  
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
