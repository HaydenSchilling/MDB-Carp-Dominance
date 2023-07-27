# Namoi test script

library(tidyverse)
library(lubridate)

catch <- read_csv("Namoi River Catch.csv")
bio <- read_csv("Namoi River Bio.csv")

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * Caught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(OperationID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

catch2 <- catch2 %>% left_join(catch_total_biomass)

catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(Date, SiteID, CommonName, OperationID, MethodType, Biomass_proportion)

catch2_wide <- catch2 %>% ungroup() %>% pivot_wider(names_from = CommonName, values_from = Biomass_proportion) %>%
 rowwise() %>% mutate(Total_percent = sum(c_across(5:26), na.rm=T))

catch2_wide$`Common carp` <- replace_na(catch2_wide$`Common carp`,0)
  
catch2_wide$Date <- dmy(catch2_wide$Date)
catch2_wide$Year <- year(catch2_wide$Date)
catch2_wide$Month <- month(catch2_wide$Date)
catch2_wide$fYear <- as.factor(as.character(catch2_wide$Year))

table(catch2_wide$MethodType)

hist(catch2_wide$Year)
hist(catch2_wide$Month)

catch2_wide <- catch2_wide %>% filter(Total_percent == 1)

library(glmmTMB)

f1 <- glmmTMB(`Common carp` ~ fYear + (1|Date) + (1|MethodType),
              data = catch2_wide, family = beta_family())

summary(f1)



library(DHARMa)
resids <- simulateResiduals(f1)
plot(resids)

catch2_wide$Response <- catch2_wide$`Common carp`
table(catch2_wide$Response)

library(brms)
f2 <- brm(Response ~ fYear , #  (1|MethodType) + (1|Date)
              data = catch2_wide, family = zero_one_inflated_beta(
  link = "logit",
  link_phi = "log",
  link_zoi = "logit",
  link_coi = "logit"
))

saveRDS(f2, "test no random.rds")

library(DHARMa)
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(f2)),
  observedResponse = catch2_wide$Response,
  fittedPredictedResponse = apply(t(posterior_epred(f2)), 1, mean),
  integerResponse = TRUE)

plot(model.check)
