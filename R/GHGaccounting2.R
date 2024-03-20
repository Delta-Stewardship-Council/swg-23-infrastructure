## ------------------------------------------ ##
#   Infrastructure -- GHG Accounting
## ------------------------------------------ ##

# Life stage: Early Analysis

# Script author(s): Karrin Alstad
# Date: 2024/02/22

# Purpose:
## Attempting to follow the GHG accounting steps outlined by Steve Deverel
## See steps in literature\scoping_plan writeup_04252022
## Info on Terra package: https://rspatial.github.io/terra/reference/terra-package.html#spatraster
# load packages
library(terra)
library(sf)
library(ggplot2)

# set to my directory for now
folder <- "C:/Users/KAlstad/OneDrive - California Department of Fish and Wildlife/NCEAS_data"
infra <- "C:/Users/KAlstad/Documents/Github_C/swg-23-infrastructure"

# using Tidally influenced boundary and assuming this is the same as the
# "MHHWS tidal boundary from Brophy et al. 2019" that is listed in Deverel et al instructions 

# Legal delta boundary - shape file

legal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_LegalBoundary/legal_delta_SacSJ.shp")) 
legal %>% 
  ggplot() +
  geom_sf()

tidal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"))%>%
  st_transform(crs = 3310)
tidal %>% 
  ggplot() +
  geom_sf()

# try terra version of shp read:
path <- file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_LegalBoundary/legal_delta_SacSJ.shp") 
v <- vect(path)
# plot the shapefile
plot(v)

# 2.	All spatial datasets were clipped according to the MHHWS tidal boundary from Brophy et al. 2019. 

# Read in NOAA CCAP raster cover data for California
canloc <- file.path(folder,"CCAP/ca_2021_ccap_v2_hires_canopy_draft_20240108.tif")

# read in the canopy raster
canopy <- rast(canloc)


plot(canopy, reset = F)

### Crop and mask by any polygon
tidal_bf <- vect(tidal)
plot(tidal_bf)

project(canopy , tidal_bf)
raster_cp <- crop(canopy, tidal_bf, mask= T)
# Note: if mask= F, the crop will be by extent (box) ###

r2 <- crop(canopy, extent(tidal))
r3 <- mask(r2, tidal)



plot(r3, reset = F)




# Read in SFEI Bay Area Aquatic Resources Inventory BAARI data from ESRI geodatabase file (.gdb) using this suggestion:
# https://gis.stackexchange.com/questions/184013/read-a-table-from-an-esri-file-geodatabase-gdb-using-r/271043#271043

baariloc <- file.path(folder,"BAARI/BAARI_v2.1_final__SFEI_2017/BAARI_v2pt1__SFEI_2017.gdb")
sf::st_layers(dsn = baariloc)
BAARIbay <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Baylands")
BAARIbay %>% 
  ggplot() +
  geom_sf()
BAARIwet <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Wetlands")
BAARIwet %>% 
  ggplot() +
  geom_sf()
BAARIstm <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Streams")
BAARIstm %>% 
  ggplot() +
  geom_sf()


# Read in SFEI Delta Aquatic Resources Inventory (DARI)
dariloc <- file.path(folder,"DARIv1.1/DARIv1.1_SFEI_2022.gdb")
sf::st_layers(dsn = dariloc)
dariwet <- sf::st_read(dsn = dariloc, layer= "DARIv1_1_wetlands")
dariwet %>% 
  ggplot() +
  geom_sf()

daristm <- sf::st_read(dsn = dariloc, layer= "DARIv1_1_streams")
daristm %>% 
  ggplot() +
  geom_sf()






# Brophy, L.S., C.M. Greene, V.C. Hare, B. Holycross, A. Lanier, W.N. Heady, et al. 2019. 
# Insights into estuary habitat loss in the western United States using a new method for 
# mapping maximum extent of tidal wetlands. PloS one 14(8): e0218558.


# Boundary used in Brophy et al 2019
# MHHWS tidal boundary from Brophy et al. 2019. 

## Import CDFW’s VegCAMP mapping Delta 2002 and Suisun 2003 
# https://wildlife.ca.gov/Data/GIS/Vegetation-Data

vc_suis_2006 <- file.path(folder,"VegCAMP/500_Suis2006/ds500.gdb")
sf::st_layers(dsn = vc_suis_2006)
suis_2006 <- sf::st_read(dsn = vc_suis_2006, layer= "ds500")
