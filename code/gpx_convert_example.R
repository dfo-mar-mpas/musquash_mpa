#libraries
library(rgdal)
library(sf)
library(dplyr)

#read in the coordinates and convert them to an sf then convert that sf into an 'sp' datatype
test <- read.csv("data/eDNA_Sample_Stations_2022.csv")%>%
        st_as_sf(coords=c("long","lat"),crs=latlong)%>%
        as_Spatial()

#write the sp datatype to a gpx using the rgdal::writeOGR
writeOGR(test,
         dsn="data/musq_stations.gpx", layer="name", driver="GPX",
         dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer = T)
