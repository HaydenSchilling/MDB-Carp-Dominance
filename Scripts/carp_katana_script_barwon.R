# Larger analyses
library(tidyverse)
library(lubridate)

rivers = read_csv("Rivers.csv")

i = 9#as.numeric(Sys.getenv('PBS_ARRAY_INDEX'))
i
paste("This is job number",i)
# for(i in 1:nrow(rivers)){

catch <- read_csv("clean boat electro catch with waterbody.csv") %>% # filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName == rivers$Rivers[i])

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

f2 <- brm(Response ~ fYear + (1|FDate) + (1|SiteID), #  (1|MethodType)
          data = catch2_wide, family = zero_one_inflated_beta(),

          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=20000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = paste0(rivers$Rivers[i]," day site random.rds"))

# }