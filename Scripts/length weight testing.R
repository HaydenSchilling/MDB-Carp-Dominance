# testing changes in LW relationships for carp

library(tidyverse)
library(lubridate)
bio <- read_csv("All bio_27_01_2022.csv") %>% mutate(CalcWeight = 10^(a)*Length_mm^b) %>% 
  filter(CommonName == "Common carp") %>%
  drop_na(FishWeight) %>% drop_na(SampleDate) %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate),
    Year_ending_June = case_when(Month < 7 ~ Year,
                                 T ~ Year + 1),
    fYear = as.factor(as.character(Year_ending_June))) %>%
  filter(FishWeight < 10000) %>% filter(Length_mm < 1000) %>%
  filter(fYear != "2004")

bad <- bio %>% filter(fYear == "2020" & Length_mm > 750)
bad2 <- bio %>% filter(fYear == "2010" & Length_mm >500 & FishWeight <1000)
bad3 <- bio %>% filter(fYear == "2014" & Length_mm> 500 & FishWeight < 1000)
bad4 <- bio %>% filter(fYear == "2014" & Length_mm < 550 & FishWeight > 4000)
bad5 <- bio %>% filter(fYear == "2017" & Length_mm < 350 & FishWeight > 2000)
bad5 <- bio %>% filter(fYear == "2019" & Length_mm > 400 & FishWeight < 300)
bad6 <- bio %>% filter(fYear == "2017" & Length_mm > 400 & FishWeight < 300)
bad7 <- bio %>% filter(Length_mm <100 & FishWeight>100)
bad8 <- bio %>% filter(fYear == "2022" & Length_mm > 600 & FishWeight < 1000)
bad9 <- bio %>% filter(fYear == "2021" & Length_mm > 450 & FishWeight < 800)
bad10 <- bio %>% filter(fYear == "2017" & Length_mm > 100 & Length_mm < 370 & FishWeight > 1400)
bad11 <- bio %>% filter(fYear == "2016" & Length_mm > 100 & Length_mm < 380 & FishWeight > 1500)


all_bad <- bind_rows(bad, bad2, bad3, bad4, bad5,bad6,bad7,bad8,bad9,bad10,bad11)
bio <- bio %>% anti_join(all_bad)  %>% filter(fYear != "2023")

ggplot(bio, aes(Length_mm, FishWeight)) + geom_point() +facet_wrap(~fYear, scales = "free") + theme_bw()
ggsave("length weight annual.png", dpi=600, width=21, height=21, units = "cm")

ggplot(bio, aes(Length_mm, FishWeight, col=fYear)) + geom_smooth() +
  scale_x_log10() + scale_y_log10() + theme_bw()+
  scale_colour_discrete(name = "Year")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(size=12, face="bold"))+
  labs(y="Weight (g)", x = "Length (mm)")# +facet_wrap(~fYear, scales = "free")
ggsave("length weight logged coloured season.png", dpi=600, width=21, height=15, units = "cm")
