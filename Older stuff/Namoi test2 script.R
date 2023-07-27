# Namoi test script

library(tidyverse)
library(lubridate)

catch <- read_csv("Namoi River Catch.csv")# %>% filter(Method == "Boat Electrofishing")
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

catch2_wide <- catch2_wide %>% filter(Total_percent == 1) %>% filter(MethodType == "BTE") %>%
  ##mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
  #                                 `Common carp` == 0 ~ 0.000000001,
  #                                 T ~ `Common carp`)) %>%
  filter(Month <=5)

# library(glmmTMB)
# 
# f1 <- glmmTMB(`Common carp` ~ 1 + fYear + (1|Date) + (1|SiteID),
#               data = catch2_wide, family = beta_family())
# 
# summary(f1)
# 
# 
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

# f2 <- brm(Response ~ fYear + (1|FDate) + (1|SiteID), #  (1|MethodType)
#           data = catch2_wide, family = zero_one_inflated_beta(),
# 
#           #prior = priors,
#           control = list(adapt_delta = 0.95),
#           iter=8000,
#           seed = 1234,
#           cores=4,
#           file_refit = "always",
# file = "test Namoi day site random.rds")


f2 <- readRDS("test Namoi day site random.rds")
summary(f2)

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
ndraws=5000) %>%
   mutate(Species = "Common carp")

write_csv(yy, "Namoi carp annual prediction biomass.csv")


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



### No model
plot_dat <- catch2_wide %>% group_by(Year) %>% summarise(meanperc = mean(`Common carp`),
                                                         sd_perc = sd(`Common carp`),
                                                         n = n(),
                                                         se_perc = sd_perc/sqrt(n))

ggplot(plot_dat, aes(Year, meanperc)) + geom_point()+
  geom_errorbar(aes(ymin=meanperc - se_perc, ymax = meanperc+se_perc))




