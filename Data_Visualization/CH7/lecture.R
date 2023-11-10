# working directory & data load
setwd("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Data_Visualization/CH7")
library(readxl)
pm10 <- read_excel("data/pm_data.xlsx")
head(pm10)

# data wrangling
library(ggplot2)
library(tidyverse)
pm10.long <- pm10 %>%
  pivot_longer(cols = -date, names_to = "city", values_to = "pm10")

# graph
ggplot(data = pm10.long, aes(x = date, y = pm10, color = city)) +
  geom_line() + scale_color_discrete(limits=c("Seoul", "Busan", "Daegu", "Incheon"))
  ggtitle("202135-157844")

# loading the map
library(sf)
NE.countries<-read_sf("ne_110m_admin_0_countries.shp")
# NE.countries
# st_crs(NE.countries)

# draw a map
# ggplot() + geom_sf(data=NE.countries) + coord_sf(expand = FALSE)
# ggplot() + geom_sf(data=NE.countries) + coord_sf(xlim=c(76, 150), ylim=c(20, 55))
# ggplot() + geom_sf(data=NE.countries) + coord_sf(crs=st_crs(2163))

### except for the part of the vector data
ggplot() +
  geom_sf(data=NE.countries) +
  coord_sf(xlim=c(76, 150), ylim=c(20, 55)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggtitle("202135-157844")


# load the map with geodata
library(geodata)
godm.Korea <- getData('GADM', country='kor', level=1)