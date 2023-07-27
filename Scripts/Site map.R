# Carp biomass map

library(tidyverse)
library(sf)
library(ggspatial)
library(lubridate)

rivers = read_csv("Rivers.csv")

WRPA_all <-st_read("../database exploration/WRA Shapefile/Surface Water Water Resource Plan Areas.shp") %>%
  dplyr::select(SWWRPANAME) %>% st_make_valid()

#WRPA <- WRPA_all %>% filter(SWWRPANAME == Focus_Area)

stream<-st_read("../Jerom/RMaps/mdb_Stream_layer_GDA1994.shp") 
states<-st_read("../Jerom/RMaps/Australian_states_GDA1994.shp")



catch <- read_csv("clean boat and backpack electro catch with waterbody.csv") %>% # filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(WaterbodyName %in% rivers$Rivers)

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
  select(WaterbodyName, coords.x1, coords.x2, SampleDate, SiteID, CommonName, OperationID, Method, Biomass_proportion)

# yy <- catch2 %>%
#   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# hist(yy$SampleDate, breaks = "years")
# xx <- catch %>% filter(OperationID %in% yy$OperationID)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(9:last_col()), na.rm=T))

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
         fYear = as.factor(as.character(Year_ending_June))) 

#write_csv(catch2_wide, "formatted wide catch data.csv")
#%>%
#mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`),

data_summary <- catch2_wide %>% group_by(WaterbodyName, coords.x1, coords.x2) %>%
  summarise(min_prop = min(`Common carp`),
            max_prop = max(`Common carp`),
            n = n())

data_summary2 <- catch2_wide %>% group_by(WaterbodyName) %>%
  summarise(min_prop = min(`Common carp`),
            max_prop = max(`Common carp`),
            n = n())
data_summary2

data_summary3 <- catch2_wide %>% group_by(WaterbodyName, Year_ending_June) %>%
  summarise(min_prop = min(`Common carp`),
            max_prop = max(`Common carp`),
            n = n())
data_summary3

## now other sites


rivers = read_csv("Rivers.csv")

WRPA_all <-st_read("../database exploration/WRA Shapefile/Surface Water Water Resource Plan Areas.shp") %>%
  dplyr::select(SWWRPANAME) %>% st_make_valid()

#WRPA <- WRPA_all %>% filter(SWWRPANAME == Focus_Area)

stream<-st_read("../Jerom/RMaps/mdb_Stream_layer_GDA1994.shp") 
states<-st_read("../Jerom/RMaps/Australian_states_GDA1994.shp")



elevation_dat <- read_csv("Site elevations.csv")
#all_data <- all_data 



catch <- read_csv("clean boat and backpack electro catch with waterbody.csv") %>% 
  left_join(elevation_dat) %>% filter(elevation <= 700) %>% select(-elevation, -elev_units) %>%# filter(Method == "Boat Electrofishing")
  mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct() %>% filter(!WaterbodyName %in% rivers$Rivers)

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #


catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(Method == "BTE" | Method == "BPE") %>% select(-1,-2,-3,-project_segment) %>%
  distinct() %>% filter(SWWRPANAME_NEW %in% basins)

bio <- read_csv("All bio_27_01_2022.csv") %>% mutate(CalcWeight = 10^(a)*Length_mm^b)

bio_summary <- bio %>% group_by(CommonName, OperationID) %>%
  summarise(biomass_mean = mean(CalcWeight, na.rm=T))

catch2 <- catch %>% left_join(bio_summary) %>% mutate(Taxa_biomass = biomass_mean * NumberCaught)

catch_total_biomass <- catch2  %>%
  ungroup() %>% group_by(OperationID) %>%
  summarise(Total_biomass = sum(Taxa_biomass, na.rm=T))

catch2 <- catch2 %>% left_join(catch_total_biomass)

catch2 <- catch2 %>% mutate(Biomass_proportion = Taxa_biomass/Total_biomass) %>%
  select(WaterbodyName, coords.x1, coords.x2, SampleDate, SiteID, CommonName, OperationID, Method, Biomass_proportion)

# yy <- catch2 %>%
#   dplyr::group_by(SampleDate, SiteID, OperationID, Method, CommonName) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# hist(yy$SampleDate, breaks = "years")
# xx <- catch %>% filter(OperationID %in% yy$OperationID)

catch2_wide <- catch2 %>% ungroup() %>%
  pivot_wider(names_from = CommonName, values_from = Biomass_proportion, values_fn = sum) %>%
  rowwise() %>% mutate(Total_percent = sum(c_across(9:last_col()), na.rm=T))

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
         fYear = as.factor(as.character(Year_ending_June))) 

#write_csv(catch2_wide, "formatted wide catch data.csv")
#%>%
#mutate(`Common carp` = case_when(`Common carp` == 1 ~ 0.999999999,
#                                `Common carp` == 0 ~ 0.000000001,
#                                T ~ `Common carp`),

data_summaryX <- catch2_wide %>% group_by(WaterbodyName, coords.x1, coords.x2) %>%
  summarise(min_prop = min(`Common carp`),
            max_prop = max(`Common carp`),
            n = n())


### Site Map
p1 <- ggplot(data_summary, aes(coords.x1, coords.x2, col = WaterbodyName, size=n)) +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="grey70")+
  layer_spatial(WRPA_all, aes(),fill="grey90", col="grey90", 
                alpha=0.9, size=1)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  layer_spatial(data=stream,colour="grey30")+
  #scale_size_continuous(label="Samples")+
  #layer_spatial(data=rivers,colour="red")+
  geom_point(data=data_summaryX, aes(coords.x1, coords.x2, size=n), inherit.aes = F, col="black", fill="white", shape=21)+
  geom_point(shape=21, fill="white")+
  # scale_fill_manual(name="Water Resource Planning Area",
  #                   values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                  "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                  "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
 # scale_colour_manual(values=c("hotpink"), name=NULL)+
  coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
           ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
  scale_x_continuous(breaks=c(138,142,146,150))+
  theme_bw()+
  ylab("Latitude") + xlab("Longitude")+
  scale_size_continuous(name = "Number\nof shots")+
  scale_color_discrete(name="River")+
  theme(legend.position = "right",
        axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=11),
        legend.text = element_text(colour="black", size=10))
p1


### Inset map

p2 <- ggplot() +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="khaki3")+
  layer_spatial(WRPA_all,col="black", fill = "grey90" ,
                alpha=0.9, size=0.5)+
  geom_rect(col="black", aes(xmin=st_bbox(WRPA_all)[1],xmax= st_bbox(WRPA_all)[3], 
                             ymin= st_bbox(WRPA_all)[2], ymax = st_bbox(WRPA_all)[4]),
            fill=NA)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  #layer_spatial(data=stream,colour="grey30")+
  #layer_spatial(data=rivers,colour="red")+
  #geom_point(shape=4,size=2)+
  #scale_fill_manual(name="Water Resource Planning Area",
  #                  values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                 "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                 "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  #scale_colour_manual(values=c("hotpink"), name=NULL)+
  #coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
#         ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
#scale_x_continuous(breaks=c(138,142,146,150))+
theme_classic()+
  ylab("Latitude") + xlab("Longitude")+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(colour="black", size=10),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(fill=NA, colour="black"),
        plot.background = element_blank())

p2

library(patchwork)

#p2 + p1 + plot_layout(guides = 'collect', widths = c(0.5,1))# & theme(legend.position = "bottom")
p1 + inset_element(p2, left = 0.01, bottom = 0.65, right = .35, top = 1.03, align_to = "panel")

ggsave("Site map.png", dpi = 600, width=21, height=21, units="cm")
ggsave("Site map.pdf", dpi = 600, width=21, height=21, units="cm")


### histogram of data (excludes empty catches)
p2 <- ggplot(data_summary3, aes(Year_ending_June, n)) + geom_col() +
  facet_wrap(~WaterbodyName)+
  ylab("Number of shots (excludes 'No Catch')")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=10),
        legend.text = element_text(colour="black", size=8),
        strip.text = element_text(face="bold", size=10))
p2
ggsave("Effort over time.png", dpi = 600, width=21, height=14.8, units="cm")


##### NOW OVERALL NOT JUST MAJOR RIVERS

all_data <- read.csv("../Long term abundance/All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))


segments <- all_data %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")
#all_data
all_data <- all_data %>% filter(!project_segment %in% bad_list) %>% select(-1,-2,-3,-project_segment) %>%
  distinct() %>%
  filter(Method == "BTE")

basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

all_data <- all_data %>% #filter(CommonName == "Golden perch") %>%
  filter(SWWRPANAME_NEW %in% basins)

all_data <- all_data %>% group_by(coords.x1, coords.x2) %>%
  summarise(n = n_distinct(OperationID))


p1b <- ggplot(all_data, aes(coords.x1, coords.x2, size=n)) +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="grey70")+
  #layer_spatial(WRPA_all, aes(fill=SWWRPANAME), col="black", 
  #             alpha=0.9, size=1)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  layer_spatial(data=stream,colour="grey30")+
  #scale_size_continuous(label="Samples")+
  #layer_spatial(data=rivers,colour="red")+
  geom_point(shape=21, fill="white")+
  # scale_fill_manual(name="Water Resource Planning Area",
  #                   values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                  "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                  "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  # scale_colour_manual(values=c("hotpink"), name=NULL)+
  coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
           ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
  scale_x_continuous(breaks=c(138,142,146,150))+
  theme_bw()+
  ylab("Latitude") + xlab("Longitude")+
  theme(legend.position = "right",
        axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=10),
        legend.text = element_text(colour="black", size=8))
p1b
ggsave("Site map overall.png", dpi = 600, width=21, height=21, units="cm")

