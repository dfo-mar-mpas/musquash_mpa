#eDNA (proposed) sample design 2022 - onward 

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
  musquash <- read_sf("data/Musquash_MPA_region.shp")%>%st_transform(latlong)

#load coordinates
  edna_samples <- read.csv("data/Musquash_WaterSampling_2022_formatted.csv")%>%
                  mutate(longitude=(long_deg+long_dm/60)*-1,latitude=lat_deg+lat_dm/60)%>%
                  st_as_sf(coords=c("longitude","latitude"),crs=latlong)%>%
                  mutate(target = station)%>%
                  tidyr::separate(target,c("name","index"))%>%
                  mutate(index = as.numeric(index),
                         process = ifelse(name =="Musq",ifelse(index %% 2,FALSE,TRUE),TRUE),
                         bottom_process = ifelse(station %in% c("Musq_20","Zone 3_1"),TRUE,FALSE))%>% #5 fathom hole and Zone 3 1 (deepest we sampled)
                  dplyr::select(-c("name","index","Notes"))%>%
                  suppressMessages()%>%
                  suppressWarnings() # these are just about NAs and don't impact the data
                  
  
  edna_stations <- read.csv("output/2022_eDNA_sample_stations.csv")%>%
                   st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)%>%
                   mutate(sampled = ifelse(station %in% edna_samples$station,TRUE,FALSE))
                   

  p1 <- ggplot()+
    geom_sf(data=musquash%>%filter(!is.na(ZONE)),aes(fill=ZONE),col="black")+
    geom_sf(data=edna_stations%>%filter(!sampled),col="black",size=1.5,pch=4)+
    geom_sf(data=edna_samples%>%filter(!process),size=2,fill="white",pch=21)+
    geom_sf(data=edna_samples%>%filter(process),size=2,pch=19,col="black")+
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position=c(0.1,0.2))+
    labs(title="2022 eDNA sample stations",fill="")+
    scale_fill_viridis(discrete=T,direction = -1);p1
  
  ggsave("output/eDNA_processing_2022.png",p1,
         height=6,width=6,units="in",dpi=600)

#output the stations to a csv
  write.csv(edna_samples%>%
              data.frame()%>%
              filter(process)%>%
              dplyr::select(station,bottom_process),"output/edna_processing_2022.csv",row.names=FALSE)

