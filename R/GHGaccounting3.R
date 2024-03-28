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

#	Land use in early 2000s and present day (as represented by 2016) as follows. 
# Two data sets were used: the NOAA C-CAP database (https://coast.noaa.gov/digitalcoast/data/ccapregional.html) 
# for comparison with more detailed San Francisco Estuary Institute (SFEI) datasets 
# based on the Bay Area Aquatic Resources Inventory (BAARI), Delta Aquatic Resources Inventory (DARI), 
# and CDFW’s VegCAMP mapping. 

# Details:
# The 2001 C-CAP data sets were used to correspond to the 2002/2003 SFEI dataset 
#   based on Delta 2002 and Suisun 2003 VegCAMP mapping,

# The 2016 C-CAP data sets were used to correspond to the 2015/2016 SFEI dataset 
#   based on Delta 2016 and Suisun 2015 mapping from DARI and BAARI




# set to my directory for now
folder <- "C:/Users/KAlstad/OneDrive - California Department of Fish and Wildlife/NCEAS_data"
infra <- "C:/Users/KAlstad/Documents/Github_C/swg-23-infrastructure"

# using Tidally influenced boundary and assuming this is the same as the
# "MHHWS tidal boundary from Brophy et al. 2019" that is listed in Deverel et al instructions 

tidal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"))%>%
  st_transform(crs = 3310)
tidal %>% 
  ggplot() +
  geom_sf()

# try terra version of shp read (as an example)
# path <- file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_LegalBoundary/legal_delta_SacSJ.shp") 
# v <- vect(path)
# # plot the shapefile
# plot(v)

# 2.	All spatial data sets were clipped according to the MHHWS tidal boundary from Brophy et al. 2019. (same a our tidal, i think)
# Brophy, L.S., C.M. Greene, V.C. Hare, B. Holycross, A. Lanier, W.N. Heady, et al. 2019. 
# Insights into estuary habitat loss in the western United States using a new method for 
# mapping maximum extent of tidal wetlands. PloS one 14(8): e0218558.
# Note: map in this paper looks similar to our tidal boundary map

# Read in NOAA regional CCAP raster cover data for California: 2001 and 2016
#https://coastalimagery.blob.core.windows.net/ccap-landcover/CCAP_bulk_download/Regional_30meter_Land_Cover/index.html
#canloc <- file.path(folder,"CCAP/ca_2021_ccap_v2_hires_canopy_draft_20240108.tif") # Not the right file; leave for example
caps2016 <- file.path(folder,"CCAP/conus_2016_ccap_landcover_20200311.tif")
caps2001 <- file.path(folder,"CCAP/conus_2001_ccap_landcover_20200311.tif")

# read in the canopy raster
c2016r <- rast(caps2016)
c2001r <- rast(caps2001)

### Crop and mask by any polygon
# convert tidal into a terra vector object
tidal_tv <- vect(tidal)
# plot the converted shapefile
plot(tidal_tv)

# reproject tidal_tv with the same CRS as the canopy raster
#tidal_tv_reproj <- project(tidal_tv, crs(canopy))
tidal_tv_reproj <- project(tidal_tv, crs(c2016r))

# crop canopy raster and then masking with our re-projected tidally influenced boundary
# NOTE: takes a while to run
# Note: if mask= F, the crop will be by extent (box) ###
#raster_cp <- crop(canopy, tidal_bf_reproj, mask= T)
c2016rcp <- crop(c2016r, tidal_tv_reproj, mask= T)
c2001rcp <- crop(c2001r, tidal_tv_reproj, mask= T)
# plot the cropped canopy raster and check
#plot(raster_cp)
plot(c2016rcp)
plot(c2001rcp)


# Delta 2016 and Suisun 2015 mapping from DARI and BAARI
# Read in SFEI Bay Area Aquatic Resources Inventory BAARI data from ESRI geodatabase file (.gdb) using this suggestion:
# https://gis.stackexchange.com/questions/184013/read-a-table-from-an-esri-file-geodatabase-gdb-using-r/271043#271043

baariloc <- file.path(folder,"BAARI/BAARI_v2.1_final__SFEI_2017/BAARI_v2pt1__SFEI_2017.gdb")
sf::st_layers(dsn = baariloc)
BAARIbay <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Baylands")%>%
  st_transform(st_crs(tidal_tv))
crs(BAARIbay)
crs(tidal)
str(BAARIbay)

BAARIbay %>% 
  ggplot() +
  geom_sf()
BAARIwet <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Wetlands")
BAARIwet %>% 
  ggplot() +
  geom_sf()

# Error reading streams layer
# BAARIstm <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Streams")
# BAARIstm %>% 
#   ggplot() +
#   geom_sf()
# Error in `geom_sf()`:
#   ! Problem while converting geom to grob.
# i Error occurred in the 1st layer.
# Caused by error in `UseMethod()`:
#   ! no applicable method for 'st_as_grob' applied to an object of class "c('XY', 'MULTICURVE', 'sfg')"



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



## Import Delta 2002 and Suisun 2003 VegCAMP mapping
# https://wildlife.ca.gov/Data/GIS/Vegetation-Data

# Delta 2006 (cannot find 2002; can only find 2006)

# The following report describes the vegetation classification and mapping of the Legal Delta
# portion of the Sacramento-San Joaquin River Delta conducted in 2005-2006 

vc_delta_2006 <- file.path(folder,"VegCAMP/292_SacSJ/ds292.gdb")
sf::st_layers(dsn = vc_delta_2006)
delta_2006 <- sf::st_read(dsn = vc_delta_2006, layer= "ds292")
delta_2006 %>% 
  ggplot() +
  geom_sf()


# Delta 2016 (won't unzip)

# vc_delta_2016 <- file.path(folder,"VegCAMP/ds2855/ds2855.gdb")
# sf::st_layers(dsn = vc_delta_2016)
# delta_2016 <- sf::st_read(dsn = vc_delta_2016, layer= "ds2855")
# delta_2016 %>% 
#   ggplot() +
#   geom_sf()


# Suisun 2015 

vc_suis_2015 <- file.path(folder,"VegCAMP/2676_Suis2015/ds2676.gdb")
sf::st_layers(dsn = vc_suis_2015)
suis_2015 <- sf::st_read(dsn = vc_suis_2015, layer= "ds2676")
suis_2015 %>% 
  ggplot() +
  geom_sf()



# Suisun 2003 

vc_suis_2003 <- file.path(folder,"VegCAMP/162/ds162.gdb")
sf::st_layers(dsn = vc_suis_2003)
suis_2003 <- sf::st_read(dsn = vc_suis_2003, layer= "ds162")
suis_2003 %>% 
  ggplot() +
  geom_sf()



