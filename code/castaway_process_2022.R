## CastAway CTD Processing for the 2022 field season. 

#Load libraries 
library(dplyr)
library(sf)
library(ggplot2)
library(sp)
library(rgeos)
library(devtools)
library(tidyr)

sf_use_s2(FALSE)

#source the CastAway CTD Processing script 
  source_url("https://raw.githubusercontent.com/rystanley/CastawayCTD/master/code/CTDProcess.R") #parsing script
  source_url("https://raw.githubusercontent.com/rystanley/CastawayCTD/master/code/CTDProcessBatchProcess.R") #batch processing script
  source_url("https://raw.githubusercontent.com/rystanley/CastawayCTD/master/code/ctd_check.R") #script for checking whether casts are valid
  
  #function for extracting the med point for sequenced labels using the 'cut' function - https://stackoverflow.com/questions/22312207/how-to-assign-cut-range-midpoints-in-r
  get_midpoint <- function(cut_label) {
    mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
  }
  
#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the Musquash Estuary polygon ----
musquash <- read_sf("data/Shapefiles/Musquash_MPA_region.shp")%>%st_transform(latlong)

#read in the sond data 
sonde_df <- read.csv("data/Sonde/Musquash_eDNA2209v2_cleaned.csv") 

#load the sampled sites for coordinates -----
edna_samples <- read.csv("data/DataSheets/Musquash_WaterSampling_2022_formatted.csv")%>%
                mutate(longitude=(long_deg+long_dm/60)*-1,latitude=lat_deg+lat_dm/60)%>%
                st_as_sf(coords=c("longitude","latitude"),crs=latlong,remove=FALSE)%>%
                mutate(target = station)%>%
                tidyr::separate(target,c("name","index"))%>%
                mutate(index = as.numeric(index),
                       process = ifelse(name =="Musq",ifelse(index %% 2,FALSE,TRUE),TRUE))%>% #this is a logical to retain only the samples in the processing plan (code/edna_sampling_2022.R)
                dplyr::select(-c("name","index","Notes"))%>%
                #filter(process)%>% #25 stations (note that 2 also have a bottom Zone3_1 and musq_20) so 27 *3 ~ 82 for a full plate
                suppressMessages()%>%
                suppressWarnings() # these are just about NAs and don't impact the data

#process the CastAway data ---- 

#get the file paths
ctdpaths <- dir("data/CTD/2022/",full.names = T)

#parse those that are invalid sets
castaway_paths <- ctdpaths[ctd_check_batch(ctdpaths)]

#batch process the data
castaway_data_all <- CTDProcess_Batch(castaway_paths)

#get the coordinates for each station
castaway_stations_df <- castaway_data_all%>%
                        distinct(FileName,.keep_all = TRUE)%>%
                        dplyr::select(FileName,CastTime_Local,
                                      Latitude_Start,Longitude_Start)%>% # Latitude_End,Longitude_End
                        st_as_sf(coords=c("Longitude_Start","Latitude_Start"),crs=latlong)%>%
                        st_join(edna_samples%>%dplyr::select(station,geometry), #this will join the coordinates from the cast away to the closest one from the samples
                                join=st_nearest_feature)%>%
                        filter(station %in% (edna_samples%>%filter(process)%>%pull(station)))%>% ## note that this has 24 rows and not 25 (target) because we didn't get a cast at musq_30
                        data.frame()%>%
                        dplyr::select(FileName,station) #this is a rubric now that can be used to match stations to the files

castaway_stations_df%>%pull(station)%>%table()%>%unique() #check to show that the join only selected one adjacent station

#filter out the castaway data we don't need and merge with the station matched dataframe
castaway_data <- castaway_data_all%>%
                 filter(FileName %in% castaway_stations_df$FileName)%>%
                 left_join(.,castaway_stations_df)

#write the processed data
write.csv(castaway_data,file = "output/castaway_processed.csv",row.names=F)

#Now get smoothed data for the casts (average by depth for up and down data)

bin_int <-  0.25 #resolution of the averages

castaway_smooth <- castaway_data%>%
                   filter(!is.na(Depth))%>%
                   mutate(depth_bin = cut(Depth,breaks = seq(0,round(max(Depth)),bin_int)))%>% #cut into pieces by the bin_int
                   rowwise()%>%
                   mutate(depth = get_midpoint(depth_bin)*-1)%>% #extract the mid point of the 'cut' interval
                   data.frame()%>%
                   group_by(station,depth)%>%
                   summarise(temperature = mean(Temperature,na.rm=T),
                             salinity = mean(Salinity,na.rm=T),
                             density = mean(Density,na.rm=T))%>% #get the average for the depth interval within each cast
                   ungroup()%>%
                   data.frame()%>%
                   arrange(station,-depth)

#write the smoothed data
write.csv(castaway_smooth,file="output/castaway_processed_smoothed.csv")
                    
#plot the data just to inspect it for vertical structure. 
castaway_plot <- castaway_smooth%>%
                 mutate(station = factor(station,levels=edna_samples%>%filter(process)%>%pull(station)),
                        plot_group = case_when(
                          grepl("Zone 2A",station) ~ "Zone 2A",
                          grepl("Zone 2B",station) ~ "Zone 2B",
                          grepl("Zone 3",station) ~ "Zone 3",
                          TRUE ~ "Axis transect"
                        ))%>%
                 gather("var","val",c("temperature","salinity","density"))
                 
#ugly plot
p1 <- ggplot(data=castaway_plot,aes(x=val,y=depth,group=station,col=station))+
  geom_point()+
  geom_path()+
  facet_wrap(var~plot_group,scales="free")+
  theme_bw()+
  labs(y="Depth (m)",col="Station",x="")+
  theme(strip.background = element_rect(fill="white"));p1

ggsave("output/castaway_profiles.png",p1,width=6,height=6,units="in",dpi=300)

#extract the upper and lower temperature values

#function to assign the top and bottom groupings for the extracts
temp_group <- function(x,bin=1){
  
  #bin is the grouping layer (default is the top and bottom 1 m)
  
  vec <- rep("Middle",length(x))
  vec[which(abs(x)<bin)] <- "Surface"
  vec[which(abs(x)>max(abs(x)-bin))] <- "Bottom"
  return(vec)
  
}

castaway_extracts <- castaway_smooth%>%
                     group_by(station)%>%
                     mutate(location = temp_group(depth))%>%
                     ungroup()%>%
                     data.frame()%>%
                     filter(location !="Middle")%>% #just want to the top and bottom m
                     group_by(station,location)%>%
                     summarise(temperature = mean(temperature),
                                salinity = mean(salinity))%>%
                     ungroup()%>%
                     mutate(location = factor(location,levels=c("Surface","Bottom")))%>% #use this to sort 
                     arrange(station,location)%>%
                     data.frame()%>%
                     mutate(platform="CastAway")%>%
                     gather("variable","value",c("temperature","salinity"))

sonde_extracts <- sonde_df%>%
                 filter(Station %in% (edna_samples%>%filter(process)%>%pull(station)))%>%
                 mutate(location = rep(c("Surface","Bottom"),25))%>%
                 dplyr::select(Station,location,Temp,Sal,Turbid.,Chl)%>%
                 rename(station=Station,temperature=Temp,salinity=Sal,turbidity=Turbid.,chl=Chl)%>%
                 mutate(platform="Sonde",
                        location = factor(location,levels=c("Surface","Bottom")))%>%
                 gather("variable","value",c("temperature","salinity","turbidity","chl"))
            
#bring it together
edna_meta <- rbind(castaway_extracts,sonde_extracts)%>%
             arrange(station,location)
                 
write.csv(edna_meta,"output/environmental_covariates.csv",row.names=FALSE)                 


