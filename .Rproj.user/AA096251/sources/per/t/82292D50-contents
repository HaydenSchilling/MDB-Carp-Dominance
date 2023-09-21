# Model evaluation
library(tidyverse)
library(brms)
library(scales)

rivers <- read_csv("Data/Rivers.csv")
#SRA_data <- read_csv("Data/SRA alien biomass estimates.csv")
full_dat <- read_csv("Data/2023 formatted wide catch data with backpack.csv") %>%
  mutate(fYear = as.factor(as.character(fYear)) ) %>% filter(fYear != "1992") %>%
  filter(fYear != "1993") %>% filter(fYear != "1994") %>% filter(fYear != "2024")
full_dat$Response <- full_dat$`Common carp`
full_dat$Response2 <- full_dat$`Murray cod`
full_dat$FDate <- as.factor(as.character(full_dat$Date))
full_dat$SiteID <- as.factor(as.character(full_dat$SiteID))

big_data <- data.frame()
#SRA_final_df <- data.frame()
ss <- full_dat %>% group_by(WaterbodyName) %>% summarise(mean_val = mean(Response),
                                                         median_val = median(Response))

### Barwon River = 9 (good), Castlereagh=5 (good), Macquarie = 4 (good), Darling = 2 (good), gwydir = 8 (good), Lachlan = 6 (good),
### Murrumbidgee = 7 (good fit), Namoi = 3 (good), Murray = 1 (good)
for(i in 1:nrow(rivers)){
  
  #i=1
  
  barwon <- read_rds(paste0("Katana results/Backpack/2023 fixed backpack ",rivers$Rivers[i]," day site random.rds"))
  summary(barwon) # Well mixed - good model
  #plot(barwon)
pp_check(barwon, ndraws = 500) + theme(panel.background = element_rect(fill="white"),
                                       plot.background = element_rect(fill="white"))
ggsave(paste0("Katana results/Backpack/posterior predictive check fixed backpack ",rivers$Rivers[i]," .png"))
  b_dat <- full_dat %>% filter(WaterbodyName == rivers$Rivers[i]) 
  #if(i == 7){
   # b_dat <- b_dat %>% filter(fYear != 1996) %>% filter(fYear !=2002)}
  table(b_dat$fYear)
  # table(b_dat$Method)
  # n_distinct(b_dat$SiteID)
  # tempt <- b_dat %>% mutate(events = paste(SampleDate, SiteID))
  # n_distinct(tempt$events)
  # if(b_dat$WaterbodyName[1] == "Murrumbidgee River"){
  #   b_dat <- b_dat %>%
  #     filter(fYear != "1992") %>% filter(fYear != "1993") %>% filter(fYear != "1994") %>%
  #     filter(fYear != "1996") %>% filter(fYear != "2002")
  # }
  
  ggplot(b_dat, aes(Response)) + geom_histogram(aes(y = stat(density) * 10) ,binwidth=0.1)+ 
    facet_wrap(~fYear)+ ylab("Percentage of Shots") + xlab("Proportion Biomass Carp")+
    ggtitle(rivers$Rivers[i])#+ scale_y_continuous(labels = percent_format())
  #ggsave(paste0("Katana results/ raw histograms",rivers$Rivers[i],".png"), dpi=600, width = 21, height=14.8, units="cm")
  
  # make predictions
  library(tidybayes)
  #yy <- new_data %>% add_epred_draws(f2)
  ### important to drop levels below
  #mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
  yy <- barwon %>% epred_draws(newdata = expand_grid(fYear = levels(droplevels(b_dat$fYear)),
                                                       Method = "BTE"),
                               #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                               #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                               #Sampling_duration = 90,
                               re_formula = NA,
                               ndraws=2000) %>%
    mutate(Species = "Common carp", River = rivers$Rivers[i])
  
  write_csv(yy, paste0("Katana results/Backpack/2023 Fixed Backpack ",rivers$Rivers[i]," carp annual prediction biomass.csv"))
  big_data <- big_data %>% bind_rows(yy)
  #SRA_final_df <- SRA_final_df %>% bind_rows(SRA_est)
  
  ggplot(yy, aes(x=as.numeric(fYear))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
        stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
    #geom_rug(data=mydata_wide)+
    theme(axis.title = element_text(face="bold", size = 14),
          axis.text = element_text(size=12, colour="black"),
          axis.ticks = element_line(colour="black"))+
    #geom_smooth(aes(y= .epred*100))+
    ylab("Predicted Biomass Carp (%)") + xlab("Year ending June") +
    ggtitle(rivers$Rivers[i])
  ggsave(paste0("Katana results/Backpack/Fixed Backpack ",rivers$Rivers[i]," time series.png"),
         dpi=600, width=21, height=14.8, units="cm")
  
}

big_data <- big_data %>% mutate(River = case_when(River  == "Darling River" ~ "Darling-Baaka River",
                                                  T ~ River))
# SRA_final_df <- SRA_final_df %>% mutate(River = case_when(Valley  == "Darling River" ~ "Darling-Baaka River",
#                                                           T ~ Valley))

ggplot(big_data, aes(x=as.numeric(fYear))) + facet_wrap(~River, ncol=2) +
  #geom_line(data=SRA_final_df, aes(x=year, y = mean*100), col = "darkgreen")+
  #geom_ribbon(data=SRA_final_df, aes(x=year, ymin = lower95*100, ymax=upper95*100), fill = "green", alpha=0.2)+
  stat_pointinterval(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        strip.text = element_text(size=12, colour="black", face="bold"))+
  #geom_smooth(aes(y= .epred*100))+
  ylab("Predicted Biomass Carp (%)") + xlab("Year ending June")# +
#ggtitle(rivers$Rivers[i])
ggsave(paste0("Katana results/Backpack/Fixed backpack Supplementary all river time series BTE.png"),
       dpi=600, width=21, height=14.8*2, units="cm")
ggsave(paste0("Katana results/Backpack/Fixed backpack Supplementary all river time series BTE.pdf"),
       dpi=600, width=21, height=14.8*2, units="cm")


### Overall MDB
library(tidyverse)
library(brms)

barwon <- read_rds(paste0("Katana results/Backpack/Carp full MDB WRPA day site random with backpack.rds"))
summary(barwon) # Well mixed - good model
#plot(barwon)



# make predictions
library(tidybayes)
#yy <- new_data %>% add_epred_draws(f2)

### important to drop levels below
#mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
yy <- barwon %>% epred_draws(newdata = expand_grid(fYear = (as.character(c(1995, 1996, seq(1998,2023,1)))),
                                                     Method = "BTE"),
                             #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                             #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                             #Sampling_duration = 90,
                             #ndraws=5000,
                             re_formula =NA) %>%
  mutate(Species = "Common carp")

write_csv(yy, paste0("Katana results/Backpack/Overall MDB carp annual prediction biomass.csv"))


ggplot(yy, aes(x=as.numeric(fYear))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  ylab("Predicted Biomass Carp (%)") + xlab("Year ending June") +
  #ggtitle("Overall NSW MDB")+
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))
#geom_smooth(aes(y= .epred*100))+

ggsave(paste0("Katana results/Overall MDB carp biomass time series.png"),
       dpi=600, width=21, height=14.8, units="cm")

ggsave(paste0("Katana results/Overall MDB carp biomass time series.pdf"),
       dpi=600, width=21, height=14.8, units="cm")


yy <- read_csv("Katana results/Backpack/Overall MDB carp annual prediction biomass.csv")
dat_sum <- yy %>% group_by(fYear) %>% summarise(quantile = scales::percent(c(0.025, 0.5, 0.975)),
                                                estimate = quantile(.epred, c(0.025, 0.5, 0.975))) %>%
  pivot_wider(names_from = quantile, values_from = estimate)

# summarise(median = median(.epred),
#           lower_95 = quantile(probs=0.025))
dat_sum

quantile(yy$.epred, probs = c(0.025,0.5, 0.975))
mean(dat_sum$median)

### exploring other predictions

b_dat <- full_dat %>% filter(fYear != 2024)

# make predictions
library(tidybayes)
#yy <- new_data %>% add_epred_draws(f2)
library(scales)
### important to drop levels below

basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling")

#mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
yy <- barwon %>% epred_draws(newdata = expand_grid(fYear = (as.character(c(1995, 1996, seq(1998,2023,1)))),
                                                   Method = "BTE",
                                                   SWWRPANAME_NEW = basins),
                             #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                             #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                             #Sampling_duration = 90,
                             #ndraws=5000,
                             re_formula = ~(1|SWWRPANAME_NEW)) %>%
  mutate(Species = "Common carp")

write_csv(yy, paste0("Katana results/Backpack/Overall MDB carp annual prediction biomass WRPAs.csv"))


ggplot(yy, aes(x=SWWRPANAME_NEW)) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  ylab("Predicted Biomass Carp (%)") + xlab("Water Resource Planning Area") +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  #ggtitle("Overall NSW MDB")+
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.text.x = element_text(angle=60, hjust=1, vjust=1),
        axis.ticks = element_line(colour="black"))
#geom_smooth(aes(y= .epred*100))+

ggsave("Katana results/WRPA carp biomass.png",
       dpi=600, width=21, height=14.8, units="cm")

ggsave("Katana results/WRPA carp biomass.pdf",
       dpi=600, width=21, height=14.8, units="cm")

### Annual by WRPA

b_dat <- full_dat %>% filter(fYear != 2023)

# make predictions
library(tidybayes)
#yy <- new_data %>% add_epred_draws(f2)
library(scales)
### important to drop levels below
#mydata_wide$SWWRPANAME_NEW <- droplevels(mydata_wide$SWWRPANAME_NEW)
yy <- barwon %>% epred_draws(newdata = expand_grid(fYear = levels(droplevels(b_dat$fYear)),
                                                   Method = "BTE",
                                                   SWWRPANAME_NEW = unique(b_dat$SWWRPANAME_NEW)),
                             #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                             #SWWRPANAME_NEW = levels(droplevels(mydata_wide$SWWRPANAME_NEW)),
                             #Sampling_duration = 90,
                             #ndraws=5000,
                             re_formula = ~(1|SWWRPANAME_NEW)) %>%
  mutate(Species = "Common carp")

#write_csv(yy, paste0("Katana results/Backpack/Overall MDB carp annual prediction biomass WRPAs.csv"))


ggplot(yy, aes(x=as.numeric(fYear))) + facet_wrap(~SWWRPANAME_NEW, ncol=2) +
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  ylab("Predicted Biomass Carp (%)") + xlab("Year") +
  #scale_x_continuous(breaks=c(2000,2010,2020))+
  #scale_x_discrete(labels = function(x) 
  #  stringr::str_wrap(x, width = 15))+
  #ggtitle("Overall NSW MDB")+
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        #axis.text.x = element_text(angle=60, hjust=1, vjust=1),
        axis.ticks = element_line(colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        strip.text = element_text(size=12, colour="black", face="bold"))
#geom_smooth(aes(y= .epred*100))+

ggsave("Katana results/WRPA carp biomass Annual.png",
       dpi=600, width=21, height=14.8*2, units="cm")

wrpa_sum <- yy %>% group_by(SWWRPANAME_NEW, fYear) %>% summarise(median_est = median (.epred))
wrpa_sum

#### Histograms at samplerecordID level (not shot/Operation)
library(tidyverse)
library(brms)
library(scales)

rivers <- read_csv("Data/Rivers.csv")

full_dat <- read_csv("Biomass proportions by SampleRecordID river level.csv") %>%
  mutate(fYear = as.factor(as.character(fYear)) )%>% filter(fYear != "1992") %>%
  filter(fYear != "1993") %>% filter(fYear != "1994")
full_dat$Response <- full_dat$`Common carp`
full_dat$Response2 <- full_dat$`Murray cod`
full_dat$FDate <- as.factor(as.character(full_dat$Date))
full_dat$SiteID <- as.factor(as.character(full_dat$SiteID))
b_dat <- full_dat

### Barwon River = 9, Castlereagh=5, Macquarie = 4, Darling = 2, gwydir = 8, Lachlan = 6,
### Murrumbidgee = 7 (bad fit), Namoi = 3, Murray = 1 (bad fit)
for(i in 1:nrow(rivers)){
  
  b_dat <- full_dat %>% filter(WaterbodyName == rivers$Rivers[i]) 
  
  
  ggplot(b_dat, aes(Response)) + geom_histogram(aes(y = stat(density) * 10) ,binwidth=0.1)+ 
    facet_wrap(~fYear)+ ylab("Percentage of Surveys") + xlab("Proportion Biomass Carp")+
    ggtitle(rivers$Rivers[i])#+ scale_y_continuous(labels = percent_format())
  ggsave(paste0("Katana results/ survey histograms",rivers$Rivers[i],".png"), dpi=600, width = 21, height=14.8, units="cm")
  
}

sample_size2 <- full_dat %>% group_by(WaterbodyName) %>% summarise(n = n_distinct(SamplingRecordID),
                                                                   mean_carp = mean(Response, na.rm=T),
                                                                   median_carp = median(Response, na.rm=T)) %>% 
  mutate(WaterbodyName = case_when(WaterbodyName  == "Darling River" ~ "Darling-Baaka River",
                                                        T ~ WaterbodyName))

full_dat <- full_dat %>% mutate(WaterbodyName = case_when(WaterbodyName  == "Darling River" ~ "Darling-Baaka River",
                                                          T ~ WaterbodyName))


ggplot(full_dat, aes(Response)) + geom_histogram(aes(y = stat(density) * 10) ,
                                                 breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.01),
                                                 closed="left")+ 
  facet_wrap(~WaterbodyName, ncol=2)+ theme_bw()+
  #geom_vline(data = sample_size2, aes(xintercept=mean_carp), col="blue")+
  #geom_vline(data = sample_size2, aes(xintercept=median_carp), col="red")+
  geom_text(data=sample_size2, aes(0.25,20, label=paste0("n = ",n)))+
  ylab("Percentage of Surveys") + xlab("Proportion Biomass Carp")+
  theme(axis.text = element_text(size=9, colour="black"),
        #axis.text.x = element_text(angle=60, vjust=0.5),
        axis.title = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"))
#ggtitle(rivers$Rivers[i])#+ scale_y_continuous(labels = percent_format())
ggsave(paste0("Katana results/survey histograms all rivers.png"), dpi=600, width = 21, height=14.8*2, units="cm")
ggsave(paste0("Katana results/survey histograms all rivers.pdf"), dpi=600, width = 21, height=14.8*2, units="cm")



### Basin scale histogram

rivers <- read_csv("Data/Rivers.csv")

full_dat <- read_csv("Biomass proportions by SampleRecordID basin level.csv") %>%
  mutate(fYear = as.factor(as.character(fYear)) )%>% filter(fYear != "1992") %>%
  filter(fYear != "1993") %>% filter(fYear != "1994") %>% filter(fYear != "2024")


#full_dat <- full_dat %>% left_join(elevation_dat) %>% filter(elevation <= 700) %>% select(-elevation, -elev_units)


full_dat$Response <- full_dat$`Common carp`
full_dat$Response2 <- full_dat$`Murray cod`
full_dat$FDate <- as.factor(as.character(full_dat$Date))
full_dat$SiteID <- as.factor(as.character(full_dat$SiteID))

sample_size <- full_dat %>% group_by(fYear) %>%
  summarise(sample_size = n_distinct(SamplingRecordID))

### Barwon River = 9, Castlereagh=5, Macquarie = 4, Darling = 2, gwydir = 8, Lachlan = 6,
### Murrumbidgee = 7 (bad fit), Namoi = 3, Murray = 1 (bad fit)

b_dat <- full_dat #%>% filter(WaterbodyName == rivers$Rivers[i]) 

ggplot(b_dat, aes(Response)) + geom_histogram(aes(y = stat(density) * 10) ,
                                              breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.01),
                                              closed="left")+ 
  facet_wrap(~fYear)+ ylab("Percentage of Surveys") + xlab("Proportion Biomass Carp")+
  geom_text(data=sample_size, aes(x=0.25, y = 75, label = paste0("n = ",sample_size)), size=3)+
  ylim(0,100)+ scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1),
                                  labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"))+
  theme_bw()+ # ggtitle("NSW MDB")+
  theme(axis.text = element_text(size=9, colour="black"),
        #axis.text.x = element_text(angle=60, vjust=0.5),
        axis.title = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"))#+
#geom_vline(aes(xintercept = median(Response)),col='blue',size=2)
ggsave(paste0("Katana results/survey histograms overall2.png"), dpi=600, width = 21, height=14.8, units="cm")
ggsave(paste0("Katana results/survey histograms overall2.pdf"), dpi=600, width = 21, height=14.8, units="cm")




#### Plot showing change over time
# with top and bottom 10%

dat_sum <- b_dat %>% group_by(fYear) %>% summarise(total_surveys = n(),
                                                   `Less than 10%` = sum(Response < 0.1),
                                                   `Greater than 90%` = sum(Response >0.9)) %>%
  pivot_longer(cols = c(`Less than 10%`,`Greater than 90%`), values_to = "Proportion", names_to = "value") %>%
  mutate(Year = as.numeric(as.character(fYear)),
         Proportion = Proportion/total_surveys) %>% filter(Year != 1997)


# Done better below
# ggplot(dat_sum, aes(Year, Proportion, shape=value, col=value, size=total_surveys)) + geom_point()  + geom_smooth(method="lm") +
#   theme_classic() +  theme(axis.text = element_text(size=12, colour="black"),
#                            #axis.text.x = element_text(angle=60, vjust=0.5),
#                            axis.title = element_text(size=14, face="bold"),
#                            legend.title = element_text(size=10, face="bold"),
#                            legend.text = element_text(size=10),
#                            legend.position = "bottom")+
#   scale_size_binned_area(name = "Number of Surveys")+
#   scale_color_manual(name = "Carp Percentage", values = c("blue", "orange"))+
#   scale_shape_discrete(name = "Carp Percentage") + ylab("Proportion of Surveys") + ylim(c(0,.75))
# 
# ggsave("Proportion carp top bottom over time.png", dpi=600, width = 20, height=15, units="cm")  
# ggsave("Proportion carp top bottom over time.pdf", dpi=600, width = 20, height=15, units="cm")  

## Try a glm
datdat <- dat_sum %>% pivot_wider(values_from = Proportion, names_from = value) %>%
  rename(small = `Less than 10%`,
         large = `Greater than 90%`)
f2 <- brm(large ~ Year, data = datdat, family = "beta")
summary(f2)



#plot(ggeffects::ggpredict(f2))
library(tidybayes)
zz <- f2 %>% epred_draws(newdata = expand_grid(Year = seq(min(datdat$Year),max(datdat$Year),0.05)),
                             re_formula = NA) %>% mutate(value = "Greater than 90%")

f3 <- brm(small ~ Year, data = datdat, family = "beta")
summary(f3)
#plot(ggeffects::ggpredict(f3))
zzz <- f3 %>% epred_draws(newdata = expand_grid(Year = seq(min(datdat$Year),max(datdat$Year),0.05)),
                         re_formula = NA) %>% mutate(value = "Less than 10%")

zzzz <- bind_rows(zz,zzz)

ggplot(zzzz, aes(Year, col=value, fill=value)) +  stat_lineribbon(aes(y = .epred), .width=0.95, alpha=0.4)

ggplot(dat_sum, aes(Year, Proportion, col=value,fill=value, size=total_surveys), shape=21) + geom_point(col="black", shape=21)  +# geom_smooth(method="lm") +
  stat_lineribbon(data=zzzz, aes(x=Year, y = .epred, col=value, fill=value), .width=0.95, alpha=0.4, inherit.aes = F)+
  theme_classic() +  theme(axis.text = element_text(size=12, colour="black"),
                           #axis.text.x = element_text(angle=60, vjust=0.5),
                           axis.title = element_text(size=14, face="bold"),
                           legend.title = element_text(size=10, face="bold"),
                           legend.text = element_text(size=10),
                           legend.position = "bottom")+
  scale_size_binned_area(name = "Number of Surveys")+
  #scale_color_manual(name = "Carp Percentage", values = c("blue", "orange"))+
  #scale_fill_manual(name = "Carp Percentage", values = c("blue", "orange"))+
  scale_fill_manual(name = "Carp Percentage", values = c("skyblue", "darksalmon"))+
  scale_color_manual(name = "Carp Percentage", values =c("skyblue4", "indianred"))+
  scale_shape_discrete(name = "Carp Percentage") + ylab("Proportion of Surveys") + ylim(c(0,.6))

ggsave("Proportion carp top bottom over time.png", dpi=600, width = 20, height=15, units="cm")  
ggsave("Proportion carp top bottom over time.pdf", dpi=600, width = 20, height=15, units="cm")  

data_extract <- zzzz %>% group_by(Year, value) %>% summarise(quantile = scales::percent(c(0.025, 0.5, 0.975)),
                                                       estimate = quantile(.epred, c(0.025, 0.5, 0.975))) %>%
  pivot_wider(names_from = quantile, values_from = estimate)
