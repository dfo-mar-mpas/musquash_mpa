## Code for map showcasing sample locations for Lalla et al. (Microbial eDNA analysis)

#load libraries ----------
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(viridis)
library(MarConsNetData)
library(ggspatial)
library(ggnewscale)
library(cowplot)
library(gridExtra)

#source functions
source("c:/Users/stanleyr/Documents/Github/MCRG_functions/code/ddm_to_dd.R")
source("c:/Users/stanleyr/Documents/Github/MCRG_functions/code/trim_img_ws.R")

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_mar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load shapefiles --------
musquash_mpa <- read_sf("data/Shapefiles/Musquash_MPA_region.shp")%>%st_transform(latlong)
coastline <- read_sf("data/Shapefiles/musquash_coastal_hr.shp")%>%st_transform(latlong)

##Basemap for broader regional view ----
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong)%>%
                   mutate(country="Canada"),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong)%>%
                   mutate(country="USA"))

#Maritimes Conservation Network ----
mar_network <- data_draft_areas()%>%st_transform(latlong)

#load sampling coordiantes
sample_coords <- read.csv("data/edna/lalla_ms_meta.csv")%>% 
                 distinct(station,.keep_all=TRUE)%>%
                 st_as_sf(coords=c("lon","lat"),crs=latlong)

#load bathymetry
coastline <- read_sf("data/Shapefiles/musquash_coastal_hr.shp")%>%st_transform(latlong)

musq_dem <- rast("data/Bathymetry/Musquash_area_dem.tif")%>%
            project(latlong)

musq_box <- musquash_mpa%>%
             filter(ZONE%in%c("Zone 1","Zone 2A"))%>%
             st_bbox()%>%
             st_as_sfc()%>%
             st_transform(utm_mar)%>%
             st_buffer(1.15)%>%
             st_transform(latlong)%>%
             st_bbox()%>%
             st_as_sfc()

musq_dem_trim <- musq_dem%>%
                 crop(musq_box)%>%
                 clamp(.,lower= -2,upper=Inf)


land_mask <- coastline %>% 
  st_intersection(musq_box) %>%
  st_union() 

land_raster <- rasterize(vect(land_mask), 
                         musq_dem_trim, 
                         background = -2)

# Set land (value = 1) to NA
land_raster[land_raster == 1] <- NA

musq_dem_merged <- merge(musq_dem_trim, land_raster)


#set up plots
musq_plot_lim <- musquash_mpa%>%
  st_bbox()%>%
  st_as_sfc()%>%
  st_transform(utm_mar)%>%
  st_buffer(0.2)%>%
  st_transform(latlong)%>%
  st_bbox()

p1 <- ggplot()+
  geom_spatraster(data=musq_dem_merged)+
  geom_sf(data=coastline)+
  geom_sf(data=musquash_mpa,fill=NA,linewidth=0.6)+
  scale_fill_gradientn(
    colors = c("#D1FFBB", "#A8E6CF", "#56CCF2", "#2E86DE", "#1A5490", "#0D1B2A"),
    na.value = "transparent",
    name = "Depth (m)"
  )+
  new_scale_fill()+
  geom_sf(data=sample_coords,aes(fill=zone),shape=21,size=3)+
  labs(fill="Zone")+
  coord_sf(expand=0,xlim=musq_plot_lim[c(1,3)],ylim=c(musq_plot_lim[2]+0.001,musq_plot_lim[4]))+
  annotation_scale(location="bl")+
  theme_bw()+
  theme(axis.text=element_blank(),
        plot.background = element_blank())

ggsave("output/edna_lalla_musq.jpg",p1+theme(legend.position = "none"),height=6,width=6,units="in",dpi=300)
trim_img_ws("output/edna_lalla_musq.jpg")
  
#get the legends
p1_transparent <- p1 + 
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent", color = "transparent"))

legend <- get_legend(p1_transparent)

ggsave("output/edna_lalla_musq_legend.png", legend,
       height = 4, width = 2, units = "in", dpi = 300,
       bg = "transparent")


  
#Larger scale plot
fundy_bbox <- c(xmin = -66.5, ymin = 44.5, xmax = -63.5, ymax = 46.2)%>%st_bbox()

lg_plot <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=mar_network,fill="cornflowerblue")+
  geom_sf(data=musq_box,fill=NA)+
  coord_sf(expand=0,xlim=fundy_bbox[c(1,3)],ylim=fundy_bbox[c(2,4)])+
  theme_bw()+
  annotation_scale(location="bl")+
  annotation_north_arrow(location="tr")

ggsave("output/edna_lalla_musq_lg.jpg",lg_plot,height=6,width=6,units="in",dpi=300)
trim_img_ws("output/edna_lalla_musq_lg.jpg")


##make the globe
center_pt <- fundy_bbox%>%
             st_set_crs(latlong)%>%
             st_as_sfc()%>%
             st_centroid()

lon0 <- st_coordinates(center_pt)[1]
lat0 <- st_coordinates(center_pt)[2]

globe_crs <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s",lat0, lon0)

#download the world globe basemap
world_globe <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
  st_transform(globe_crs)



#define the box denoting the study region you want to highlight
global_box <- fundy_bbox%>%
              st_set_crs(latlong)%>%
              st_as_sfc()%>%
              st_transform(globe_crs)

#make a circle to wrap the globe plot

globe_circle <- st_sfc(
  st_buffer(
    st_point(c(0, 0)),   # center of orthographic projection
    dist =  6378137  # meters
  ),
  crs = globe_crs
)

#crudgy way to make it so that the oceans are white in the plot
globe_disc <- st_sfc(
  st_point(c(0, 0)),  # center in projected coords
  crs = globe_crs
) %>%
  st_buffer(dist = 6378137) %>%   # Earth radius in meters
  st_as_sf()


global_inset2 <- ggplot() +
  
  #global background
  geom_sf(data = globe_disc, fill = "white", colour = "black", linewidth = 0.4)+
  
  # Land
  geom_sf(
    data = world_globe,
    colour = "grey20",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = world_globe%>%filter(formal_en == "Canada"),
    fill = "grey60",
    colour = "grey20",
    linewidth = 0.2
  ) +
  
  # Study region box
  geom_sf(
    data = global_box,
    fill = NA,
    colour = "black",
    linewidth = 0.9
  ) +
  
  geom_sf(data = globe_circle,
          fill = NA,
          colour = "grey30",
          linewidth = 0.4)+
  
  coord_sf(crs = globe_crs) +
  
  theme_void() +
  
  theme(
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background  = element_rect(fill = NA, colour = NA)
  )

ggsave(
  "output/lalla_global_inset2.png",
  plot = global_inset2,
  width = 4,
  height = 4,
  dpi = 600,
  bg = "transparent"
)

