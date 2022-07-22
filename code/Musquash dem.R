#load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(raster)

#load DEM for the Area
dem <- raster("R:/Science/CESD/HES_MPAGroup/Data/Bathymetry/dem35c_5c.tif")

#common projections
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- proj4string(dem)

#load Musquash polygon
canMPAs <- read_sf("R:/Science/CESD/HES_MPAGroup/Data/Shapefiles/DFO_MPA_MPO_ZPM_SHP/DFO_MPA_MPO_ZPM.shp")
musquash <- canMPAs%>%
            filter(NAME_E == "Musquash Estuary Marine Protected Area")%>%
            st_union()%>%
            st_transform(utm)

#create a crop of the DEM to the area of Musquash with 3 km buffer
ex_musquash <- extent(as_Spatial(musquash%>%st_buffer(3000)))

dem2 <- crop(dem,ex_musquash)
dem3 <- mask(dem2,as_Spatial(musquash))

#save outputs
writeRaster(dem2,filename="Musquash_area_dem.tif")
writeRaster(dem3,filename="Musquash_dem.tif")
