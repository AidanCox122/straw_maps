## MAKE MAPS OF STRAW SITES ##


# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rgdal)
library(ggrepel)

# read in data ------------------------------------------------------------

# add in full species name
species <- 
  tibble(
    Species = c('AeCa', 'CoSe', 'QuAg', 'SaSp', 'SaEx', 'UmCa', 'RoCa', 'RuUr',
                'SeSe', 'FrCa', 'FrLa', 'SyAl', 'LoIn', 'HeAr'),
    longName = c('Aesculus californica',
                 'Cornus sericea',
                 'Quercus agrifolia',
                 'Salix spp.',
                 'Salix exigua',
                 'Umbelularia californica',
                 'Rosa californica',
                 'Rubus ursinus',
                 'Sequoia sempervirens',
                 'Frangula californica',
                 'Fraxinus latifolia',
                 'Symphoricarpos albus',
                 'Lonicera involucrata',
                 'Heteromeles arbutifolia'),
    flagColor = c('green',
                  'coral1',
                  'yellow',
                  'turquoise',
                  'turquoise1',
                  'tan4',
                  'pink',
                  'darkorchid',
                  'orangered',
                  'red4',
                  'mediumblue',
                  'white',
                  'darkgoldenrod1',
                  'red')) %>% 
  arrange(longName)

# get a marin county DEM From: https://marincounty.maps.arcgis.com/apps/webappviewer/index.html?id=cb183511c2d541458c8fd529f64c3347


# True Grass --------------------------------------------------------------

points <- 
  read_csv(r"(data\clean\TrueGrass_PlantCoordinates.csv)") %>%
  left_join(species, by = 'Species')


# convert to SF object
locs_sf <- st_as_sf(points,coords = c("Longitude","Latitude"),
                    # points were colleted using WGS84
                    crs =4326)  

# read in plant zones
plnt_zones <- 
  st_read(r"(C:\Users\acox\OneDrive - Point Blue\Desktop\STRAW\Mapping\GIS\TrueGrass_shapefiles\TrueGrass Planting Shapefile\TrueGrass_Planting Areas_WCB.shp)") %>% 
  st_transform(st_crs(locs_sf))

# then project
# locs_t <- st_transform(locs_sf, proj_marin)

# extract coordinates
# plant points
locs_coords <- 
  as.data.frame(st_coordinates(locs_sf)) %>%
  mutate(
    Site = locs_sf$Site,
    Species = locs_sf$Species,
    Label = locs_sf$Name)

# shapefile points
shp_coords <- 
  as.data.frame(st_coordinates(plnt_zones))

# create a static plot
raw_map<-
  ggplot() +
  geom_sf(data = plnt_zones) +
  geom_sf(data = locs_sf,
          aes(fill = longName),
          shape = 21,
          stroke = 0.1,
          color = 'black', 
          size = 0.3) +
  # set coord system and limits
  coord_sf(
    crs = raster::crs(locs_sf),
    xlim = c(min(shp_coords$X),   max(shp_coords$X)),
    ylim = c(min(shp_coords$Y), max(shp_coords$Y)),
    expand = F,
    lims_method = "cross") +
  labs(title = paste(locs_coords$Site, Sys.Date())) +
  # geom_text(aes(x = quantile(shp_coords$X, 0.3), y = quantile(shp_coords$Y, 0.9), label = , size = 2) +
  scale_fill_discrete(name = 'Species',
                      type = species %>% 
                        filter(Species %in% locs_coords$Species) %>%
                        pull(flagColor)) +
  labs(x = 'Latitude', y = "Longitude") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme(title = element_text(size = 3),
        legend.key.size = unit(0.25, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic', size = 4),
        axis.title = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


## create geotif file ------------------------------------------------------

# for more information, please read: https://rpubs.com/Rubio-Polania/1123497

# save the map as a .tif object
ggsave(plot=raw_map,
       paste0("products/", unique(locs_coords$Site), '_', Sys.Date(), ".png"),
       device = "png",
       scale = 2,
       dpi = 600)

# Create a StackedRaster object from the saved plot
rasterMap <- raster::stack(paste0("products/", unique(locs_coords$Site), '_', Sys.Date(), ".png"))
  
# Get the GeoSpatial Components
lat_long <- ggplot_build(raw_map)$layout$panel_params[[1]][c("x_range","y_range")] 

# Supply GeoSpatial  data to the StackedRaster 
raster::extent(rasterMap) <- c(lat_long$x_range,lat_long$y_range)
raster::projection(rasterMap) <- raster::crs(locs_sf)

# Create the GeoTiff
raster::writeRaster(rasterMap,
                    paste0("products/trueGrass_geo_", Sys.Date(), ".tif"),
                    datatype = "INT1U",
                    overwrite = TRUE)

 

# SoMar -------------------------------------------------------------------
data.files <- list.files('data/clean/', full.names = T)
site.files <- data.files[grepl('SoMar', data.files)]

points <- read_csv("data/clean/SoMar_PlantCoordinates_2024-02-26.csv") %>%
  left_join(species, by = 'Species')

# read in plant zones
plnt_zones <- 
  st_read(r"(C:\Users\acox\OneDrive - Point Blue\Desktop\STRAW\Mapping\GIS\SoMar_shapefile\SoMar Plantings Shapefile\SoMar_PlantingAreas_WCB.shp)") %>% 
  st_transform(
    crs = 4326) %>% 
  mutate(ShortName = 
           str_remove(Name,
                      pattern = 'Zone ')) %>% 
  filter(ShortName %in% c('2a', '2b', '2c', '2side', '3'))

# convert to SF object
locs_sf <- st_as_sf(points,coords = c("Longitude","Latitude"),
                    # points were colleted using WGS84
                    crs =st_crs(plnt_zones)) %>% 
  st_join(
    plnt_zones %>% 
      dplyr::select(Zone = ShortName, geometry))

 

# 4326

# then project
# locs_t <- st_transform(locs_sf, proj_marin)

# create a static plot

raw_maps <- 
  vector(mode = 'list',
         length = nrow(plnt_zones)) %>% 
  set_names(plnt_zones$ShortName)

for(x in names(raw_maps)) {
  shp_coords <- 
    as.data.frame(
      st_coordinates(
        plnt_zones %>% 
          filter(ShortName == x))) %>% 
    mutate(Zone = x)
  
  locs_coords <- 
    as.data.frame(
      st_coordinates(locs_sf)) %>%
    mutate(
      Site = locs_sf$Site,
      Species = locs_sf$Species,
      Label = locs_sf$Name,
      Zone = locs_sf$Zone) 
  
  
  raw_maps[[x]]<-
    ggplot() +
    geom_sf(data = plnt_zones) +
    geom_sf(data = locs_sf,
            aes(fill = longName),
            shape = 21,
            stroke = 0.1,
            color = 'black', 
            size = 0.3) +
    # set coord system and limits
    coord_sf(
      crs = raster::crs(locs_sf),
      xlim = c(min(shp_coords$X),   max(shp_coords$X)),
      ylim = c(min(shp_coords$Y), max(shp_coords$Y)),
      expand = F,
      lims_method = "cross") +
    labs(title = paste(locs_coords$Site, x, Sys.Date())) +
    # geom_text(aes(x = quantile(shp_coords$X, 0.3), y = quantile(shp_coords$Y, 0.9), label = , size = 2) +
    scale_fill_discrete(name = 'Species',
                         type = species %>% 
                          filter(Species %in% locs_coords$Species) %>%
                          pull(flagColor)) +
    labs(x = 'Latitude', y = "Longitude") +
    theme_bw() +
    guides(fill = guide_legend(override.aes = list(size = 2))) +
    theme(title = element_text(size = 3),
          legend.key.size = unit(0.25, 'cm'),
          legend.title = element_blank(),
          legend.text = element_text(face = 'italic', size = 4),
          axis.title = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  ggsave(plot=raw_maps[[x]],
         paste0("products/", unique(locs_coords$Site), '_', unique(shp_coords$Zone), '_', Sys.Date(), ".png"),
         device = "png",
         scale = 2,
         dpi = 600)
  print(paste('Finishing', x))
}


# create geotif file ---

# for more information, please read: https://rpubs.com/Rubio-Polania/1123497

for(x in names(raw_maps)) {
  shp_coords <- 
    as.data.frame(
      st_coordinates(
        plnt_zones %>% 
          filter(ShortName == x))) %>% 
    mutate(Zone = x)
  
  locs_coords <- 
    as.data.frame(
      st_coordinates(locs_sf)) %>%
    mutate(
      Site = locs_sf$Site,
      Species = locs_sf$Species,
      Label = locs_sf$Name,
      Zone = locs_sf$Zone) 
  
  rasterMap <- raster::stack(paste0("products/", unique(locs_coords$Site), '_', unique(shp_coords$Zone), '_', Sys.Date(), ".png"))
  
  lat_long <- ggplot_build(raw_maps[[x]])$layout$panel_params[[1]][c("x_range","y_range")] 
  
  raster::extent(rasterMap) <- c(lat_long$x_range,lat_long$y_range)
  raster::projection(rasterMap) <- raster::crs(locs_sf)
  
  raster::writeRaster(rasterMap,
                      paste0("products/", unique(locs_coords$Site), '_', unique(shp_coords$Zone), '_geo_', Sys.Date(), ".tif"),
                      datatype = "INT1U",
                      overwrite = TRUE)
  print(paste('Finished with Zone', x))
}


# shollenberger -----------------------------------------------------------
data.files <- list.files('data/raw/', full.names = T)
site.files <- data.files[grepl('shollenberger', data.files)]

points <- read_csv(site.files)

plnt_zones <- 
  st_read(r"(C:\Users\acox\OneDrive - Point Blue\Desktop\STRAW\Mapping\GIS\Shollenberger_shapefile\shollenberger_trailhead.shp)") %>% 
  st_transform(crs = 4326)


# convert to SF object
locs_sf <- 
  st_as_sf(points,coords = c("Longitude","Latitude"),
                    # points were colleted using WGS84
                    crs =4326) 
  
# 4326

# then project
# locs_t <- st_transform(locs_sf, proj_marin)

# extract coordinates
locs_coords <- 
  as.data.frame(st_coordinates(locs_sf)) 

# shapefile coordinates
shp_coords <- 
  as.data.frame(st_coordinates(plnt_zones))

# create a static plot
raw_map<-
  ggplot() +
  geom_sf(data = plnt_zones) +
  geom_sf(data = locs_sf,
          color = 'red',
          size = 0.05) +
  # set coord system and limits
  coord_sf(
    crs = raster::crs(locs_sf),
    xlim = c(min(shp_coords$X),   max(shp_coords$X)),
    ylim = c(min(shp_coords$Y), max(shp_coords$Y)),
    expand = F,
    lims_method = "cross") +
  geom_text(aes(x = quantile(shp_coords$X, 0.9), y = quantile(shp_coords$Y,0.9), label = paste('Shollenberger', Sys.Date())), size = 2) +
  labs(x = 'Latitude', y = "Longitude") +
  theme_bw() +
  theme(legend.key.size = unit(0.25, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic', size = 4),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


# create geotif file ---

# for more information, please read: https://rpubs.com/Rubio-Polania/1123497

# save the map as a .tif object
ggsave(plot=raw_map,
       paste0("products/shollenberger_", Sys.Date(), ".png"),
       device = "png",
       units = 'cm',
       dpi = 600)

# Create a StackedRaster object from the saved plot
rasterMap <- raster::stack(paste0(paste0("products/shollenberger_", Sys.Date(), ".png")))

# Get the GeoSpatial Components
lat_long <- ggplot_build(raw_map)$layout$panel_params[[1]][c("x_range","y_range")] 

# Supply GeoSpatial  data to the StackedRaster 
raster::extent(rasterMap) <- c(lat_long$x_range,lat_long$y_range)
raster::projection(rasterMap) <- raster::crs(locs_sf)

# Create the GeoTiff
raster::writeRaster(rasterMap,
                    paste0("products/shollenberger_geo_", Sys.Date(), ".tif"),
                    datatype = "INT1U",
                    overwrite = TRUE)


# raster code -------------------------------------------------------------

  # load this LiDar DEM of Sonoma County from (https://gis-sonomacounty.hub.arcgis.com/datasets/sonomacounty::sonoma-veg-map-lidar-hydro-flattened-bare-earth-hs-2013-web-mercator/about) 
  
  sonoma <- raster::raster(r"(C:\Users\acox\OneDrive - Point Blue\Desktop\STRAW\Mapping\straw_maps\data\raw\Sonoma_dem.tiff)")
  
  # load this geotiff with a dem of SF bay area (30m resolution; https://earthworks.stanford.edu/catalog/stanford-nh236hj3673)
  true_grass <- raster::raster('data/raw/Digital_Elevation_Model_2.tif')
  
  true_grass_cropped <- raster::crop(x = true_grass, y = as_Spatial(locs_sf))
  
  # convert raster to dataframe
  true_grass_df <- 
    raster::rasterToPoints(true_grass_cropped) %>% 
    as.data.frame() %>% 
    rename(elevation = Digital_Elevation_Model_2)

  sonoma_dem_df <- 
    raster::rasterToPoints(sonoma) %>% 
    as.data.frame()
  
  # plot the raster with some points
  ggplot() + 
    geom_raster(data = true_grass_df,
                aes(x, y, fill = elevation)) +
    geom_sf(data = head(locs_sf), color = 'red')
  
  ggplot() +
    geom_raster(data = true_grass_df,
                aes(x, y, fill = elevation)) +
    geom_sf(data = plnt_zones, fill = NA, color = 'white') +
    geom_sf(data = locs_sf,
            aes(col = Species),
            size = 1.5) +
    # set coord system and limits
    coord_sf(
      crs = raster::crs(locs_sf),
      xlim = c(min(locs_coords$X),   max(locs_coords$X)),
      ylim = c(min(locs_coords$Y), max(locs_coords$Y)),
      expand = T,
      lims_method = "cross") +
    #facet_wrap(~AgeClass) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  