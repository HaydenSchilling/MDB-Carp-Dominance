# altitude investigation

library(tidyverse)

alt_dat <- read_csv("All electro site data_altitude.csv") %>% select(SampleLongitude, SampleLatitude) %>% distinct() %>%
  rename(x = 1, y = 2) %>% filter(!is.na(x)) %>% filter(x < 180)

#summary(alt_dat$Altitude)

#install.packages("elevatr")
library(elevatr)


range(alt_dat$x)

ll_prj <- "EPSG:4326"
mts_sp <- sp::SpatialPoints(sp::coordinates(alt_dat), 
                            proj4string = sp::CRS(SRS_string = ll_prj))
                            

tt <- get_elev_point(locations = mts_sp, src="aws", z=8)

tty <- as.data.frame(tt)

write_csv(tty, "Site elevations.csv")

# full_dat <- alt_dat %>% rename(coords.x1 = x, coords.x2 =y) %>% left_join(tty)
# 
# original <- alt_dat <- read_csv("Boat electro site data_altitude.csv") %>%
#   select(SampleLongitude, SampleLatitude, Altitude) %>%
#   rename(coords.x1 = SampleLongitude, coords.x2 =SampleLatitude) %>% left_join(tty) %>%filter(elevation > -4000) %>%
#   mutate(difference = elevation - Altitude)
# 
# plot(original$Altitude, original$elevation)
