# Simulation testing of method
library(tidyverse)

# make overall population
# combination of beta and bernoulli distributions

beta_pop <- rbeta(7000, 1.5,3)
hist(beta_pop)

bern_pop <- as.numeric(rbernoulli(3000, 0.6))
hist(bern_pop)

pop <- c(beta_pop, bern_pop)
hist(pop)

df_pop <- data.frame(Prop = pop, site = "test")
sites <- as.character(seq(1:40))
df_pop$site <- sample(sites, nrow(df_pop), replace=T)
hist(df_pop$Prop)

sites_df <- data.frame(site = sites, variation = rnorm(40, 0,0.15))
hist(sites_df$variation)

df_pop1 <- df_pop %>% left_join(sites_df) %>% 
  mutate(Prop2 = Prop+variation,
         Prop2 = case_when(Prop2>1 ~1,
                           Prop2 <0 ~0,
                           T ~ Prop2),
         Year = 1)
hist(df_pop1$Prop2)

### Year 2

beta_pop <- rbeta(7000,4.5,2)
hist(beta_pop)

bern_pop <- as.numeric(rbernoulli(3000, 0.4))
hist(bern_pop)

pop <- c(beta_pop, bern_pop)
hist(pop)

df_pop <- data.frame(Prop = pop, site = "test")
sites <- as.character(seq(1:40))
df_pop$site <- sample(sites, nrow(df_pop), replace=T)
hist(df_pop$Prop)

# sites_df <- data.frame(site = sites, variation = rnorm(40, 0,0.1))
# hist(sites_df$variation)

df_pop2 <- df_pop %>% left_join(sites_df) %>% 
  mutate(Prop2 = Prop+variation,
         Prop2 = case_when(Prop2>1 ~1,
                           Prop2 <0 ~0,
                           T ~ Prop2),
         Year = 2)
hist(df_pop2$Prop2)

### year 3
beta_pop <- rbeta(7000, 1.2,3)
hist(beta_pop)

bern_pop <- as.numeric(rbernoulli(3000, 0.6))
hist(bern_pop)

pop <- c(beta_pop, bern_pop)
hist(pop)

df_pop <- data.frame(Prop = pop, site = "test")
sites <- as.character(seq(1:40))
df_pop$site <- sample(sites, nrow(df_pop), replace=T)
hist(df_pop$Prop)

# sites_df <- data.frame(site = sites, variation = rnorm(40, 0,0.1))
# hist(sites_df$variation)

df_pop3 <- df_pop %>% left_join(sites_df) %>% 
  mutate(Prop2 = Prop+variation,
         Prop2 = case_when(Prop2>1 ~1,
                           Prop2 <0 ~0,
                           T ~ Prop2),
         Year = 3)
hist(df_pop3$Prop2)

### combine years 
df_final_pop <- bind_rows(df_pop1, df_pop2, df_pop3)

### True values
true_values <- df_final_pop %>% group_by(Year) %>% 
  summarise(true_prop = mean(Prop, na.rm=T),
            true_prop_var = mean(Prop2, na.rm=T))

true_site_values <- df_final_pop %>% group_by(site) %>%
  summarise(true_prop = mean(Prop, na.rm=T),
            true_prop_var = mean(Prop2, na.rm=T))

### Sample true pop

sample_df <- df_final_pop %>% group_by(Year) %>%
  sample_n(100) %>% mutate(Year = as.factor(as.character(Year)),
                           site = as.factor(site))

library(brms)
f2 <- brm(Prop2 ~ Year + (1|site), #  (1|MethodType)
          data = sample_df, family = zero_one_inflated_beta(),
          
          #prior = priors,
          control = list(adapt_delta = 0.95),
          iter=2000,
          seed = 1234,
          cores=4,
          file_refit = "always",
          file = "method testing.rds")

summary(f2)
### What are our answers
library(tidybayes)
yy <- f2 %>% epred_draws(newdata = expand_grid(Year = levels(droplevels(sample_df$Year))),
                             #site = levels(droplevels(sample_df$site))),
                             re_formula = NULL,
                             ndraws=2000) 

ggplot(yy, aes(x=as.numeric(Year))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  #geom_line(data=SRA_est, aes(x=year, y = mean*100), col = "darkgreen")+
  #geom_ribbon(data=SRA_est, aes(x=year, ymin = lower95*100, ymax=upper95*100), fill = "green", alpha=0.2)+
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  geom_point(data=true_values, aes(y=true_prop*100),col="red")+
  geom_point(data=true_values, aes(y=true_prop_var*100),col="blue")+
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))+
  #geom_smooth(aes(y= .epred*100))+
  ylab("Predicted Biomass Carp (%)") + xlab("Year ending June") #+
ggsave("Method testing 100 samples per year.png", dpi=600,
       width=21, height=14.8, units="cm")

### what about random effect groups
yy <- f2 %>% epred_draws(newdata = expand_grid(Year = levels(droplevels(sample_df$Year)),
                         #days_since = seq(0, max(mydata_wide$days_since), by = 50),
                         site = levels(droplevels(sample_df$site))),
                         #Sampling_duration = 90,
                         re_formula = NULL,
                         ndraws=2000) 

ggplot(yy, aes(x=as.numeric(site))) +# facet_wrap(~SWWRPANAME_NEW, scales = "free") +
  #geom_line(data=SRA_est, aes(x=year, y = mean*100), col = "darkgreen")+
  #geom_ribbon(data=SRA_est, aes(x=year, ymin = lower95*100, ymax=upper95*100), fill = "green", alpha=0.2)+
  stat_halfeye(aes(y= .epred*100), alpha=0.25) + theme_classic() +
  geom_point(data=true_site_values, aes(y=true_prop*100),col="red")+
  geom_point(data=true_site_values, aes(y=true_prop_var*100),col="green")+
  #geom_rug(data=mydata_wide)+
  theme(axis.title = element_text(face="bold", size = 14),
        axis.text = element_text(size=12, colour="black"),
        axis.ticks = element_line(colour="black"))+
  #geom_smooth(aes(y= .epred*100))+
  ylab("Predicted Biomass Carp (%)") + xlab("Site") #+

f2$ranef
