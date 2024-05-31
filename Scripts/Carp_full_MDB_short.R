#### Overall NSW Basin
library(tidyverse)
library(lubridate)

#all_data <- read.csv("../../Murray cod recruitment/All_catch_11_08_2023_with_fixed_WRPA.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))
all_data <- read.csv("clean waterbody_catch_11_08_2023_with_fixed_WRPA.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))

site_dets <- all_data %>% select(SiteName, SiteID) %>% distinct()

segments <- all_data %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing

#all_data
all_data <- all_data %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>%
  select(-1,-2,-3,-project_segment) %>%
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


elevation_dat <- read_csv("Data/Site elevations.csv") %>% rename(SampleLongitude = coords.x1, SampleLatitude = coords.x2)
all_data <- all_data %>% left_join(elevation_dat) %>%  filter(elevation <= 700) %>% select(-elevation, -elev_units)

write_csv(all_data, "Data/2024 full with lats lons.csv")

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
# 
# 
# b2 <- read_csv("data/All bio_27_01_2022.csv") %>% select(CommonName, a, b) %>% distinct()
# write_csv(b2, "Data/a b values.csv")

ab <- read_csv("Data/a b values.csv")

bio <- read_csv("../../Murray cod recruitment/All bio_11_08_2023.csv")  %>% left_join(ab) %>%
  mutate(CalcWeight = 10^(a)*Length_mm^b)

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- all_data %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

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

table(catch2$Method)

catch2 <- catch2 %>% select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method,Taxa_biomass) %>% # ,Total_biomass, Biomass_proportion
  group_by(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method) %>%
  summarise(Taxa_biomass = sum(Taxa_biomass, na.rm=T)) %>% ungroup() %>% left_join(catch_total_biomass) %>%
  filter(Total_biomass >0) %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method,Taxa_biomass,Total_biomass, Biomass_proportion)

  
# catch2 <- catch2 %>% left_join(catch_total_biomass)
# 
# catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
#   select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, OperationID, Method,Taxa_biomass,Total_biomass, Biomass_proportion)


### to do Gilligan comparison, run above but don't filter for elevation or bad sites etc

# murrum <- catch2 %>% left_join(site_dets) %>% # filter(SWWRPANAME_NEW == "Murrumbidgee") %>%
#   mutate(Date = lubridate::ymd_hms(SampleDate),
#          Year = lubridate::year(Date)) %>% filter(Year == 2004 | Year == 2004) %>% left_join(site_dets)
# 
# site_list <- c("Willow Isles", "Glen Avon - Redgum Mill", "Wyreema" , "Webbs Road",
#                "Cookoothama", "Columbo 66", "Hillas Creek", "Wahroonga", "Brungle Bridge", "Readymix",
#                "Glendale", "Kabarogong", "Woolgarlo", "Coodravale", "Cooma", "Willow Tree Waterhole",
#                "Coppins Crossing", "Lobbs Hole", "Bywong", "Cappawidgee", "Foxlow", "Benbullen",
#                "Bolaro", "Cotter Flats", "Pethers Hut")
# 
# murrum2 <-murrum %>% filter(SiteName %in% site_list)
# table(murrum2$SiteName)
# 
# site_list2 <- unique(murrum2$SiteName)
# (site_list2)
# 
# setdiff(site_list, site_list2)
# 
# tt <- murrum2 %>% group_by(CommonName) %>% summarise(mean_prop = mean(Biomass_proportion, na.rm=T),
#                                                     biomass_tot = sum(Taxa_biomass, na.rm=T))
# 3.275546e+05/sum(tt$biomass_tot)*100
# 
# xxx <- murrum %>% select(-Taxa_biomass,-Total_biomass, -SiteID) %>%
#   pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum, values_fill = 0)
# 
# xxx2 <- murrum %>% select(-Biomass_proportion,-Total_biomass, -SiteID) %>%
#   pivot_wider(names_from = CommonName, values_from = Taxa_biomass, values_fn = sum, values_fill = 0)
# 
# 
# median(xxx$`Common carp`, na.rm=T)
# 
# hist(xxx$`Common carp`, main = "Histogram of proportion carp across samples")
# hist(xxx2$`Common carp`, main = "Histogram of carp biomass across samples")
# 
catch2 <- catch2 %>%# mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
   select(SampleDate, SiteID,SWWRPANAME_NEW, CommonName, SamplingRecordID, Method, Biomass_proportion)

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
  filter(fYear != "1992") %>% filter(fYear != "1993") %>% filter(fYear != "1994") %>% filter(fYear!=2024)
# mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`))

table(catch2_wide$Method)

#write_csv(catch2_wide, "Formatted catch wide efish with backpack 2023.csv")

# n_distinct(catch2_wide$SiteID)
# tempt <- catch2_wide %>% mutate(events = paste(SampleDate, SiteID))
# n_distinct(tempt$events)
# 
# dd <- all_data %>% dplyr::select(OperationID, ProjectName, SamplingRecordID) %>% distinct()
# proj_dat <- catch2_wide %>% distinct(.keep_all = T) %>% left_join(dd) %>% group_by(ProjectName) %>%
#   summarise(n_distinct(SamplingRecordID))
# write_csv(proj_dat, "Projects in carp analysis.csv")

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


dat_sum <- catch2_wide %>% group_by(Method) %>% summarise(n=n_distinct(SamplingRecordID))
dat_sum

dat_sum2 <- catch2_wide %>% ungroup() %>% summarise(sites=n_distinct(SiteID), events = n_distinct(SamplingRecordID))
dat_sum2

write_csv(catch2_wide, "Data/2024 Formatted by survey.csv")


library(brms)
catch2_wide$Response <- catch2_wide$`Common carp`
catch2_wide$Response2 <- catch2_wide$`Murray cod`
catch2_wide$FDate <- as.factor(as.character(catch2_wide$Date))
catch2_wide$SiteID <- as.factor(as.character(catch2_wide$SiteID))
table(catch2_wide$Response)
priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))

f2 <- brm(Response ~ fYear + (1|Method) + (1|FDate) + (1|SiteID) + (1|SWWRPANAME_NEW), #  (1|MethodType)
          data = catch2_wide, family = zero_one_inflated_beta(),
          
          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=10000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = "Testing_Sampling_level.rds")


f2 <- readRDS("Testing_Sampling_level.rds")

pp_check(f2, nsamples = 100)

library(DHARMa)
check_brms <- function(model,             # brms model
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       plot = TRUE,       # make plot?
                       ...                # further arguments for DHARMa::plotResiduals 
) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(model, ndraws = 1000)),
    observedResponse = mdata$Y, 
    fittedPredictedResponse = apply(
      t(brms::posterior_epred(model, ndraws = 1000, re.form = NA)),
      1,
      mean),
    integerResponse = integer)
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  
  invisible(dharma.obj)
  
}

model2.check <- check_brms(f2, integer = F)



### Gilligan check
xxx$Response <- xxx$`Common carp`

fg <- brm(Response ~ Method + (1|SiteName), #  (1|MethodType)
          data = xxx, family = zero_one_inflated_beta(),
          
          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=2000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = "gilligan analysis test.rds")

summary(fg)

library(brms)
library(tidybayes)

f2 <- readRDS("Testing_Sampling_level.rds")

f2 <- readRDS("Katana results/2024 site level backpack Namoi River day site random.rds")
summary(f2)


library(tidybayes)
yy <- f2 %>% epred_draws(newdata = expand_grid(fYear = unique(droplevels(catch2_wide$fYear))),
                             #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                             #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                             #Sampling_duration = 90,
                             re_formula = NA,
                             ndraws=2000) %>%
  mutate(Species = "Common carp")

hist(yy$.epred)
median(yy$.epred)
quantile(yy$.epred, probs = c(0.025,0.975))




ggplot(yy, aes(x=as.numeric(as.character(fYear)))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))+
  #geom_smooth(aes(y= .epred*100))+
  ylab("Predicted Biomass Carp (%)") + xlab("Year ending June") 

ggsave("Revised overall biomass estimates.png", dpi=300, units="cm", width=21, height=14.8)
ggsave("Revised overall biomass estimates.pdf", dpi=300, units="cm", width=21, height=14.8)
