#Load libraries -----
library(tidyverse)
library(sf)
library(gdistance)
library(ggplot2)
library(ggspatial)
library(viridis)
library(rnaturalearth)

sf_use_s2(FALSE)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
dem_proj <- "+proj=sterea +lat_0=46.5 +lon_0=-66.5 +k=0.999912 +x_0=2500000 +y_0=7500000 +datum=NAD83 +units=m +no_defs"

#load the Musquash Estuary polygon
musquash <- read_sf("data/Shapefiles/Musquash_MPA_region.shp")%>%st_transform(latlong)%>%filter(!is.na(ZONE)) #something wonky with the zone polygon

#plotting boundaries
musquash_bounds_buf <- musquash%>%
  st_transform(utm_mar)%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_as_sf()%>%
  st_buffer(1.5)%>% #1.5 km buffer
  st_transform(latlong)%>%
  st_bbox()

musquash_bounds <- musquash%>%st_bbox()

#basemap
basemap <- read_sf("data/Shapefiles/musquash_coastal_hr.shp")%>%st_transform(latlong)

#acoustic receivers

acoustic_df <- read.csv("data/Acoustic/musq_grid.csv")%>%
               st_as_sf(coords=c("long","lat"),crs=latlong)

p1 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=musquash,fill=NA)+
  geom_sf(data=acoustic_df)+
  coord_sf(xlim=musquash_bounds_buf[c(1,3)],ylim=musquash_bounds_buf[c(2,4)],expand=0)+
  theme_bw()+
  annotation_scale(location="br")

ggsave("output/musquash_acoustic_grid.png",p1,height=6,width=6,units="in",dpi=300)
