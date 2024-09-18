## Survey design for eDNA sampling in the Musquash Estuary 2022

#Load libraries -----
library(dplyr)
library(sf)
library(raster)
library(gdistance)
library(ggplot2)
library(ggspatial)
library(geosphere)
library(stars)
library(viridis)
library(fasterize)
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

#load the dem
musquash_dem <- raster("data/stjohn_5.tif")%>% ##only works locally
                crop(.,extent(musquash%>%st_transform(dem_proj)))%>%
                projectRaster(.,crs=latlong)

#location of the bridge to start sampling
musquash_transect <- data.frame(lon=c(-66.31992,-66.249810,-66.24532),lat=c(45.19198, 45.159978,45.14148))%>%
  st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)%>%
  mutate(name=c("start","mid","end"))

#convert the shape file into a distance 
grid <- raster(extent(musquash%>%st_transform(utm_mar)),res=0.05,crs=utm_mar) #50m resolution grid

#index of the values that overlap with the MPA polygon
ind <- grid%>%
      fasterize(musquash%>%st_transform(utm_mar),.)%>%
      values(.)%>%
      as.data.frame()%>%
      rename(val=1)%>%
      mutate(val=is.na(val))%>%
      pull(val)

#set raster value for the transition matrix
grid[] <- 1 #land - no conductance
grid[ind] <- 0 #water (note set to 1, if you set to 10 you change the costDistance calculation)

#create transition matrix
trans <- transition(grid,transitionFunction = min,directions=16)%>%
          geoCorrection(.,type="c",multpl = FALSE)

musquash_path_coords1 <- shortestPath(trans,
                              origin=musquash_transect%>%filter(name=="start")%>%st_transform(utm_mar)%>%as_Spatial(),
                              goal = musquash_transect%>%filter(name=="mid")%>%st_transform(utm_mar)%>%as_Spatial(),output="SpatialLines")%>%
                 coordinates()%>%
                 data.frame()%>%
                 st_as_sf(coords=c("x","y"),crs=utm_mar)%>%
                 st_transform(latlong)%>%
                 mutate(lon=st_coordinates(.)[,1],
                        lat=st_coordinates(.)[,2])

musquash_path_coords2 <- shortestPath(trans,
                                      origin=musquash_transect%>%filter(name=="mid")%>%st_transform(utm_mar)%>%as_Spatial(),
                                      goal = musquash_transect%>%filter(name=="end")%>%st_transform(utm_mar)%>%as_Spatial(),output="SpatialLines")%>%
  coordinates()%>%
  data.frame()%>%
  st_as_sf(coords=c("x","y"),crs=utm_mar)%>%
  st_transform(latlong)%>%
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2])

musquash_path <- rbind(musquash_path_coords1,musquash_path_coords2)%>%summarise(do_union=FALSE)%>%st_cast("LINESTRING")

#length of musquash_path in km
musquash_path_length <- st_length(musquash_path)%>%as.numeric()/1000

n=30 # number of within line spacing final sample size will be this +2

musquash_path_length 
musquash_path_length/n #~ 500m

#these are technically regularly spaced within the line at ~ 
sp <- musquash_path%>%
                  st_transform(utm_mar)%>%
                  st_line_sample(n=30,type="regular")%>%
                  st_cast("POINT")%>%
                  st_transform(latlong)%>%
                  st_as_sf()%>%
                  mutate(lon=st_coordinates(.)[,1],
                         lat=st_coordinates(.)[,2])%>%
                 mutate(station=paste0("Musq_",2:(n+1)),
                        id=2:31)%>%
                 rename(geometry=x)%>%
                 arrange(id)%>% #This is just for quick sorting
                 dplyr::select(station,lon,lat,geometry)%>%
                 rbind(musquash_transect%>% #keep this on under the bridge even though it isn't quite 500m to Musq_2 but it's close
                         filter(name=="start")%>%
                         mutate(station="Musq_1")%>%
                         dplyr::select(station,lon,lat,geometry),
                       .)

#get the last point to be ~500m from the last sample along the line
last_point <- geosphere::bearing(sample_points[nrow(sample_points),]%>%as_Spatial(),
                                 musquash_transect%>%filter(name=="end")%>%as_Spatial())%>%
  destPoint(p = sample_points[nrow(sample_points),]%>%as_Spatial(),
            b = .,
            d = musquash_path_length/30)%>%
  data.frame()%>%
  st_as_sf(coords=c("lon","lat"),crs=latlong,remove=F)%>%
  mutate(station=paste0("Musq_",n+2))%>%
  dplyr::select(station,lon,lat,geometry)

#bind the last point
sample_points <- rbind(sp,last_point)%>%
                 mutate(type="Musquash Axis Transect",
                        zone=))

#assign zones to the points
sample_zone=st_intersects(sample_points%>%st_transform(utm_mar),
                          musquash%>%dplyr::select(geometry,ZONE)%>%st_transform(utm_mar))%>%as.numeric()

sample_zone[is.na(sample_zone)] = 1 # this is because some of those points are right on the boundaries based on the resoluton of the transition layer thorugh the upper reaches of the MPA

sample_points$zone <- musquash$ZONE[sample_zone]

ggplot()+
  geom_sf(data=musquash)+
  geom_sf(data=sample_points)+
  theme_bw()


#Randomly sampled points within the other zones

#grab the big area of zone 2A
temp <- musquash%>%
        filter(ZONE == "Zone 2A")%>%
        st_bbox()

temp[4] <- 45.172

temp <- temp%>%st_as_sfc()%>%st_as_sf()

ggplot()+geom_sf(data=musquash,aes(fill=ZONE))+geom_sf(data=temp,fill=NA)

zone2a <-  musquash%>%
            filter(ZONE == "Zone 2A")%>%
            st_intersection(temp)

#view the crop
ggplot()+geom_sf(data=musquash,aes(fill=ZONE))+geom_sf(data=zone2a,fill="grey50")

zones <- rbind(musquash%>%
                 filter(ZONE %in% c("Zone 2B","Zone 3")),zone2a)%>%
         mutate(area=st_area(.)/1000/1000,#area of the zones in km2
                sample_size = as.numeric(ceiling(20*area/max(area)))) # proportional sampling size based on 20 samples in the large area

ggplot()+geom_sf(data=zones,aes(fill=ZONE))

zone_samples <- NULL
for(i in unique(zones$ZONE)){
  
  temp <- zones%>%
          filter(ZONE == i)
  
  set.seed(99) #this will permit consistent sampling and coordinate extractions
    
  zs <- st_sample(temp,size=as.numeric(temp$sample_size))%>%
        st_as_sf()%>%
        mutate(zone=i,
               lon=st_coordinates(.)[,1],
               lat=st_coordinates(.)[,2],
               station=paste(zone,1:n(),sep="_"),
               type="Zonal samples")%>%
        rename(geometry=x)%>%
        dplyr::select(zone,station,lon,lat,type,geometry)
        
  zone_samples <- rbind(zone_samples,zs)
  
}



ggplot()+geom_sf(data=musquash,aes(fill=ZONE))+geom_sf(data=zone_samples)+theme_bw()


### bring all the data together -------

musquash_eDNA_samples <- rbind(sample_points%>%dplyr::select(zone,station,lon,lat,type,geometry),
                               zone_samples)%>%
                         mutate(depth = extract(musquash_dem,as_Spatial(.)))

musquash_sample_map <- ggplot()+
                      #geom_sf(data=basemap,fill="darkolivegreen3")+
                      geom_sf(data=musquash%>%rename(zone=ZONE),aes(fill=zone))+
                      geom_sf(data=musquash_eDNA_samples%>%filter(type=="Zonal samples"),aes(fill=zone),pch=21)+
                      geom_sf(data=musquash_eDNA_samples%>%filter(type == "Musquash Axis Transect"),pch=19,col="black",size=2)+
                      geom_sf(data=musquash_path,col="black")+
                      coord_sf(xlim=musquash_bounds[c(1,3)],ylim=musquash_bounds[c(2,4)],expand=0)+
                      annotation_scale()+
                      labs(fill="")+
                      theme_bw()+
                      theme(panel.grid = element_blank(),
                            legend.position=c(0.1,0.2));musquash_sample_map

ggsave("output/musquash_sample_map_2022.png",musquash_sample_map,height=7,width=7,units="in",dpi=300)
write.csv(musquash_eDNA_samples%>%data.frame()%>%dplyr::select(-geometry),"output/2022_eDNA_sample_stations.csv",row.names=F)


