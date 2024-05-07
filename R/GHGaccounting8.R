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
library(dplyr)
#	Land use in early 2000s and present day (as represented by 2016) as follows. 
# Two data sets were used: the NOAA C-CAP database (https://coast.noaa.gov/digitalcoast/data/ccapregional.html) 
# for comparison with more detailed San Francisco Estuary Institute (SFEI) datasets 
# based on the Bay Area Aquatic Resources Inventory (BAARI), Delta Aquatic Resources Inventory (DARI), 
# and CDFW’s VegCAMP mapping. 

#################################################################################
#################################################################################
# Details:
# The 2001 C-CAP data sets were used to correspond to the 2002/2003 SFEI dataset 
#   based on Delta 2002 and Suisun 2003 VegCAMP mapping,

# The 2016 C-CAP data sets were used to correspond to the 2015/2016 SFEI dataset 
#   based on Delta 2016 and Suisun 2015 mapping from DARI and BAARI
#################################################################################
#################################################################################


# set to my directory for now

folder <- "C:/Users/KAlstad/OneDrive - California Department of Fish and Wildlife/NCEAS_data"
infra <- "C:/Users/KAlstad/OneDrive - California Department of Fish and Wildlife/swg-23-infrastructure"

# using Tidally influenced boundary from: 
#https://data.cnra.ca.gov/dataset/san-francisco-bay-and-sacramento-san-joaquin-delta-dem-for-modeling-version-4-2/resource/bf85c268-9a49-447a-96d4-9ec2761f361b
# "MHHWS tidal boundary from Brophy et al. 2019" that is listed in Deverel et al instructions 

# dembay <- file.path(folder ,"DEM_baydelta/dem_bay_delta_10m_20201207.tif")
# # read in the canopy raster
# dembayr <- rast(dembay)
# plot(dembayr)
# 
# class(dembayr) #"SpatRaster"
# dembayr  #coord. ref. : NAD83 / UTM zone 10N (EPSG:26910) ; dimensions  : 16660, 18527, 1  (nrow, ncol, nlyr)
# dembayrp <- project(dembayr,  "EPSG:3310")
# saveRDS(dembayrp, file = "dembayrp")

dembayrp <- readRDS(file = "dembayrp")
str(dembayrp)
# do not know how to use this DEM to find tidal range (as suggested in Deveral instructions)
# default to shape file

tidal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"))%>%
  st_transform(crs = 4326)
tidal %>%
  ggplot() +
  geom_sf()

# 2.	All spatial data sets were clipped according to the MHHWS tidal boundary from Brophy et al. 2019. (same a our tidal, i think)
# Brophy, L.S., C.M. Greene, V.C. Hare, B. Holycross, A. Lanier, W.N. Heady, et al. 2019.
# Note: map in this paper looks similar to our tidal boundary map

# Read in NOAA regional CCAP raster cover data for California: 2001 and 2016
#https://coastalimagery.blob.core.windows.net/ccap-landcover/CCAP_bulk_download/Regional_30meter_Land_Cover/index.html
#canloc <- file.path(folder,"CCAP/ca_2021_ccap_v2_hires_canopy_draft_20240108.tif") # Not the right file; leave for example
# caps2016 <- file.path(folder,"CCAP/conus_2016_ccap_landcover_20200311.tif")
# caps2001 <- file.path(folder,"CCAP/conus_2001_ccap_landcover_20200311.tif")
# 
# # read in the canopy raster
# c2016r <- rast(caps2016)
# c2001r <- rast(caps2001)

# saveRDS(c2016r, file = "c2016r")
# saveRDS(c2001r, file = "c2001r")

c2016r <- readRDS(file = "c2016r")
c2001r <- readRDS(file = "c2001r")


# In order to clip the large ccap raster,
# convert tidal into a terra vector object 

tidal_tv <- terra::vect(tidal)
plot(tidal_tv)

# str(tidal_tv) #S4 class 'SpatVector' [package "terra"]
# crs(tidal_tv) #ID[\"EPSG\",4326]]
# crs(c2016r) #ID[\"EPSG\",5070]]"
# crs(c2001r) #ID[\"EPSG\",5070]]"
# str(c2016r) #S4 class 'SpatRaster' [package "terra"]

#plot(c2016r)
# see plot in github figures: plot_c2016r; 
# needs to be clipped to study region


# Angel's idea was to reproject the vector to the same projection as the CCAP raster data because 
# impossible to reproject CCAP (too large)
# However: when i run this re-projection line I get a lot of errors (maybe that doesn't matter?)

# S4 method for SpatVector
project(tidal_tv, c2016r, partial = FALSE) # There were 49 warnings (use warnings() to see them)

# reproject tidal_tv with the same CRS as the canopy raster
tidal_tv_reproj <- terra::project(tidal_tv, "EPSG:5070", partial = FALSE)  # There were 49 warnings (use warnings() to see them)
# 1: PROJ: Cannot open https://cdn.proj.org/ca_nrc_NA83SCRS.tif: schannel: CertGetCertificateChain trust error CERT_TRUST_IS_UNTRUSTED_ROOT (GDAL error 1)

crs(tidal_tv_reproj) #ID[\"EPSG\",5070]]  - This is the projection we want....

# However, the below line is now not working  :(
# crop canopy raster and then masking with our re-projected tidally influenced boundary
# NOTE: takes a while to run
# Note: if mask= F, the crop will be by extent (box) ###
c2016rcp <- crop(c2016r, tidal_tv_reproj, mask= T)  #Error: [crop] extents do not overlap
c2001rcp <- crop(c2001r, tidal_tv_reproj, mask= T)

# convert crs to "NAD83 / California Albers\" ID[\"EPSG\",3310]]"
c2016 <- project(c2016rcp,  "EPSG:3310")
c2001 <- project(c2001rcp,  "EPSG:3310")
# crs(c2016)
# crs(c2001)

# plot the cropped canopy raster and check
plot(c2016rcp)
plot(c2001rcp)

# convert the raster object into a dataframe 
# The values range from 2 to 21 and what each value represents can be found in this pdf here:
# https://coast.noaa.gov/data/digitalcoast/pdf/ccap-class-scheme-regional.pdf

c2016_df <- as.data.frame(c2016)
c2001_df <- as.data.frame(c2001)
colnames(c2016_df) <- c("ID")

library(readr)
CCAPtyps <- data.frame()
CCAPtyps <- read.csv(file.path(infra,"data-raw/csvFiles/CCAPlandcovertypes.csv"), skip = 1, header=FALSE, sep=",")
colnames(CCAPtyps) <- c("ID", "Class")


# > CCAPtyps
# ID                                  Class
# 1   0                            Background 
# 2   1                          Unclassified 
# 3   2             Developed, High Intensity 
# 4   3           Developed, Medium Intensity 
# 5   4              Developed, Low Intensity 
# 6   5                 Developed, Open Space 
# 7   6                      Cultivated Crops 
# 8   7                           Pasture/Hay 
# 9   8                  Grassland/Herbaceous 
# 10  9                      Deciduous Forest 
# 11 10                      Evergreen Forest 
# 12 11                          Mixed Forest 
# 13 12                           Scrub/Shrub 
# 14 13           Palustrine Forested Wetland 
# 15 14        Palustrine Scrub/Shrub Wetland 
# 16 15 Palustrine Emergent Wetland Persistent
# 17 16            Estuarine Forested Wetland 
# 18 17         Estuarine Scrub/Shrub Wetland 
# 19 18            Estuarine Emergent Wetland 
# 20 19                  Unconsolidated Shore 
# 21 20                           Barren Land 
# 22 21                            Open Water 
# 23 22                Palustrine Aquatic Bed 
# 24 23                 Estuarine Aquatic Bed 
# 25 24                                Tundra 
# 26 25                    Perennial Ice/Snow 



# r merge CCAP with land type names by ID
c2016_df2 <- merge(c2016_df, CCAPtyps, by = 'ID')
head(c2016_df2)  # do i need this?

unique(c2016_df2$Class)


# Delta 2016 and Suisun 2015 mapping from DARI and BAARI
# Read in SFEI Bay Area Aquatic Resources Inventory BAARI data from ESRI geodatabase file (.gdb) using this suggestion:
# https://gis.stackexchange.com/questions/184013/read-a-table-from-an-esri-file-geodatabase-gdb-using-r/271043#271043

baariloc <- file.path(folder,"BAARI/BAARI_v2.1_final__SFEI_2017/BAARI_v2pt1__SFEI_2017.gdb")
sf::st_layers(dsn = baariloc)
BAARIbay <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Baylands")
BAARIbp <- st_transform(BAARIbay,  "EPSG:3310")
#crs(BAARIbp)
BAARIbp %>%
  ggplot() +
  geom_sf()


BAARIwet <- sf::st_read(dsn = baariloc, layer= "BAARI_v2pt1_Wetlands")
BAARIwp <- st_transform(BAARIwet,  "EPSG:3310")
crs(BAARIwp)
BAARIwp %>%
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


# 2022: this is not used by Deverel but we may want
# # Read in SFEI Delta Aquatic Resources Inventory (DARI)
# dariloc <- file.path(folder,"DARIv1.1/DARIv1.1_SFEI_2022.gdb")
# sf::st_layers(dsn = dariloc)
# dariwet <- sf::st_read(dsn = dariloc, layer= "DARIv1_1_wetlands")
# #crs(dariwet) # already 3310
# # dariwet %>% 
# #   ggplot() +
# #   geom_sf()
# 
# daristm <- sf::st_read(dsn = dariloc, layer= "DARIv1_1_streams")
# #crs(daristm) # already 3310
# # daristm %>% 
# #   ggplot() +
# #   geom_sf()



## Import Delta and Suisun VegCAMP mapping data
# https://wildlife.ca.gov/Data/GIS/Vegetation-Data

# Sacramento - San Joaquin Legal Delta (CDFW) (zip) [no date in title]
# Vegcamp pdf: "The following report describes the vegetation classification and mapping of the Legal Delta
# portion of the Sacramento-San Joaquin River Delta conducted in 2005-2006" 
vc_delta_2006 <- file.path(folder,"VegCAMP/292_SacSJ/ds292.gdb")
sf::st_layers(dsn = vc_delta_2006)
delta_2006 <- sf::st_read(dsn = vc_delta_2006, layer= "ds292")
crs(delta_2006) # already 3310
delta_2006 %>%
  ggplot() +
  geom_sf()

# Sacramento - San Joaquin Delta Vegetation and Land Use Update 2016 (CDFW) (zip)
vc_delta_2016 <- file.path(folder,"VegCAMP/2855/ds2855.gdb")
sf::st_layers(dsn = vc_delta_2016)
delta_2016 <- sf::st_read(dsn = vc_delta_2016, layer= "ds2855")
# delta_2016 %>%
#   ggplot() +
#   geom_sf()


# Suisun 2003 
vc_suis_2003 <- file.path(folder,"VegCAMP/162/ds162.gdb")
sf::st_layers(dsn = vc_suis_2003)
suis_2003 <- sf::st_read(dsn = vc_suis_2003, layer= "ds162")
#crs(suis_2003) # already 3310
# suis_2003 %>%
#   ggplot() +
#   geom_sf()


# Suisun 2006 
vc_suis_2006 <- file.path(folder,"VegCAMP/500_Suis2006/ds500.gdb")
sf::st_layers(dsn = vc_suis_2006)
suis_2006 <- sf::st_read(dsn = vc_suis_2006, layer= "ds500")
#crs(suis_2006) # already 3310
# suis_2006 %>%
#   ggplot() +
#   geom_sf()


# Suisun 2015 
vc_suis_2015 <- file.path(folder,"VegCAMP/2676_Suis2015/ds2676.gdb")
sf::st_layers(dsn = vc_suis_2015)
suis_2015 <- sf::st_read(dsn = vc_suis_2015, layer= "ds2676")
#crs(suis_2015) # already 3310
# suis_2015 %>%
#   ggplot() +
#   geom_sf()






#################################################
# From Deveral instructions
# Step 3.	We delineated the brackish from freshwater areas as the midpoint 
# between Browns Island and Sherman Lake. We then created two corresponding 
# polygons and clipped all spatial datasets within the polygons.

# 3.1 Find midpoint:
# Find shape file with Browns Island and Sherman Lake identified.  

islands <- sf::read_sf(file.path(infra,"data-raw/shapefiles/RevisedIslands_160912_AllIslands/RevisedIslands_160912_AllIslands.shp"))

# get just the Sherman Island
sherman <- islands %>%
  filter(NAME=="SHERMAN ISLAND")

# get just the Browns Island
browns <- islands %>%
  filter(NAME=="BROWNS ISLAND")

# draw a line to represent the least amount of distance between the two islands
line_betw_islands <- sf::st_nearest_points(sherman, browns)

# find the midpoint on that line
midpoint <- sf::st_centroid(line_betw_islands)

# check the plot
ggplot() +
  geom_sf(data = sherman) +
  geom_sf(data = browns) +
  geom_sf(data = line_betw_islands) + 
  geom_sf(data = midpoint)

# reproject the midpoint to use WGS 84 to get the exact lat/lon
midpoint_lat_lon <- sf::st_transform(midpoint, crs = 4326) # or 3310??  crs = 4326
midpoint_lat_lon

# 3.2: Create a vertical line at the longitude of the point
# Find the longitude of the point
longitude <- st_coordinates(midpoint_lat_lon)[1, "X"]

#  Create a vertical line to extend of polygon bounding box
vertical_line_coords <- matrix(c(longitude, st_bbox(tidal)$ymin, longitude, st_bbox(tidal)$ymax), ncol = 2, byrow = TRUE)
vertical_line <- st_linestring(vertical_line_coords)
vertical_line_sf <- st_sfc(vertical_line, crs = st_crs(4326))  # Define the CRS

# 3.3: Split the polygon along the line
# vv <- vect(vertical_line_sf)
# p <- c(tidal_tv , vv)
# plot(p, col=c("blue", "red"))

# Plotting
plot(tidal_tv, main = "tidal with Point and Line")
plot(midpoint_lat_lon, add = TRUE, pch = 19, col = "red")
plot(vertical_line_sf, add = TRUE, col = "blue")


# 3.3: clip all spatial datasets within each of the polygons



