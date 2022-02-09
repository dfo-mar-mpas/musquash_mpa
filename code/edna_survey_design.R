#eDNA (proposed) sample design 2022 - onward 

#load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(rgeos)
library(geosphere)
library(sp)

sf_use_s2(FALSE)

#Projections ------------
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the Musquash Estuary polygon
  musquash <- read_sf("data/Musquash_MPA_region.shp")%>%st_transform(latlong)

#load in the scaffold coordinates generated using google earth. These have the outer boundaries of 
#stations that a transect with equal spacing can be derived. 
  scaffold <- read.csv("data/edna_scaffold_coordinates.csv")%>%
               st_as_sf(coords=c("long","lat"),remove=FALSE,crs=latlong)

#North-South Transect
  axis_ns <- scaffold%>%filter(name %in% c("Musquash_2","Musquash_6"))%>%as_Spatial()
  
  axis_ns_dist <-  scaffold%>%
                    filter(name %in% c("Musquash_2","Musquash_6"))%>%
                    st_distance()%>%
                    max()%>%
                    as.numeric()%>% #2869.611 m 
                    plyr::round_any(.,100) #round to 2900
  
  axis_ns_spacing <- axis_ns_dist/4 # Musquash_2 - 6 so 4 spaces in between stations
  
  axis_ns_bearing <- bearing(axis_ns)[1]
  
  axis_ns_intermediate <- destPoint(axis_ns[1,],axis_ns_bearing,axis_ns_spacing*c(1:3))%>%
                          data.frame()%>%
                          st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)%>%
                          mutate(description="Outer to Inner intermediate stations",
                                 name=c("Musquash_3","Musquash_4","Musquash_5"))%>%
                          rename(long=lon)%>%
                          dplyr::select(names(scaffold))

#East-West Transect
  axis_ew <- scaffold%>%filter(name %in% c("Musquash_7","Musquash_10"))%>%as_Spatial()
  
  axis_ew_dist <-  scaffold%>%
                    filter(name %in% c("Musquash_7","Musquash_10"))%>%
                    st_distance()%>%
                    max()%>%
                    as.numeric()%>% #2161.757 m 
                    plyr::round_any(.,100) #round to 2200
  
  axis_ew_spacing <- axis_ew_dist/3 # Musquash_7 - 10 so 3 spaces in between stations
  
  axis_ew_bearing <- bearing(axis_ew)[1]
  
  axis_ew_intermediate <- destPoint(axis_ew[1,],axis_ew_bearing,axis_ew_spacing*c(1:2))%>%
                          data.frame()%>%
                          st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)%>%
                          mutate(description="West to East intermediate stations",
                                 name=c("Musquash_8","Musquash_9"))%>%
                          rename(long=lon)%>%
                          dplyr::select(names(scaffold))

## bring the data together and map it
  eDNA_stations <- rbind(scaffold,axis_ns_intermediate,axis_ew_intermediate)%>%
                    mutate(id=as.numeric(gsub("Musquash_","",name)))%>%
                    arrange(id)%>%
                    dplyr::select(-id)
  
  p1 <- ggplot()+
    geom_sf(data=musquash%>%filter(!is.na(ZONE)),aes(fill=ZONE),col="black")+
    geom_sf(data=eDNA_stations,size=2)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position=c(0.1,0.2))+
    labs(title="Proposed 2022 eDNA sample stations",fill="")+
    scale_fill_viridis(discrete=T,direction = -1);p1
  
  ggsave("output/eDNA_Sample_Stations_2022.png",p1,
         height=6,width=6,units="in",dpi=600)

#output the stations to a csv
  write.csv(eDNA_stations%>%data.frame()%>%dplyr::select(-geometry),
            "data/eDNA_Sample_Stations_2022.csv",row.names = FALSE)

