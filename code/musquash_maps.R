## map of potential sample locations within the Musquash Estuary. 

#load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the Musquash Estuary polygon
musquash <- read_sf("data/Shapefiles/Musquash_MPA_region.shp")%>%st_transform(latlong)

#coastline <- read_sf("r:/Science/CESD/HES_MPAGroup/Data/Shapefiles/Coastline/Land_AtlCanada_ESeaboardUS.shp")

#create a basemap
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong))


#make a map for the readme
p1 <- ggplot()+
  geom_sf(data=musquash%>%filter(!is.na(ZONE)),aes(fill=ZONE),col="black")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position=c(0.1,0.2))+
  labs(title="Musquash Marine Protected Area",fill="")+
  scale_fill_viridis(discrete=T,direction = -1);p1

ggsave("inst/Musquash_MPA.png",p1,height=6,width=6,units="in",dpi=600)
  