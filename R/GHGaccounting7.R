## ------------------------------------------ ##
#   Infrastructure -- GHG Accounting
## ------------------------------------------ ##

# Life stage: Revising Analysis after discussions with method authors

# Script author(s): Karrin Alstad
# Date: 2024/05/03

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

dembay <- file.path(folder ,"DEM_baydelta/dem_bay_delta_10m_20201207.tif")
# read in the canopy raster
dembayr <- rast(dembay)
plot(dembayr)

class(dembayr)
dembayr
#crs(your_raster) <- "your_crs"
dembayrp <- project(dembayr,  "EPSG:3310")

# convert the raster object into a dataframe 
dembayrp_df <- as.data.frame(dembayr)

colnames(c2016_df) <- c("ID")

#Historical, Modern, and Reference scenarios were based on historical land cover mapping from
#Whipple et al. (2012) 

#Historical Land-Use and Land-cover data sets: USGS
#https://water.usgs.gov/GIS/dsdl/ds240/index.html
#https://water.usgs.gov/GIS/dsdl/ds240/index.html#raster

library(readr)
histcodes <- data.frame()
histcodes <- read.csv(file.path(infra,"EnhancedHistori/CoverCodes.csv"), skip = 0, header=FALSE, sep=",")
colnames(covtyps) <- c("ID", "Class")


#LUCODE are included in these files

# lower bay
histshp2 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g37122/g37122.shp"))%>%
  st_transform(crs = 4326)
histshp2 %>% 
  ggplot() +
  geom_sf()

# upper bay
histshp2 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g38122/g38122.shp"))%>%
  st_transform(crs = 4326)
histshp2 %>% 
  ggplot() +
  geom_sf()

# north of the bay
histshp1 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g39112/g39112.shp"))%>%
  st_transform(crs = 4326)
histshp1 %>% 
  ggplot() +
  geom_sf()

# Delta
# monterray bay to Fresno
histshp3 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g36120/g36120.shp"))%>%
  st_transform(crs = 4326)
histshp3 %>% 
  ggplot() +
  geom_sf()

# lat: 37 - 38 N; lat 122 to 120 W
histshp3 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g37120/g37120.shp"))%>%
  st_transform(crs = 4326)
histshp3 %>% 
  ggplot() +
  geom_sf()

# lat: 38 - 39 N; lat 122 to 120 W
histshp3 <- sf::read_sf(file.path(folder ,"EnhancedHistori/g38120/g38120.shp"))%>%
  st_transform(crs = 4326)
histshp3 %>% 
  ggplot() +
  geom_sf()


# RASTER files
# Bay-Delta should be covered by NW map:
hist2 <- file.path(folder ,"EnhancedHistori/giras1/giras1.tif")
# read in the canopy raster
hist2r <- rast(hist2)
plot(hist2r)

# SW map includes very south of san juaquin
hist3 <- file.path(folder ,"EnhancedHistori/giras3/giras3.tif")
# read in the canopy raster
hist3r <- rast(hist3)
plot(hist3r)
class(hist3r)

#Vaughn et al: modern land cover mapping developed for the Landscape Scenario
#Planning Tool (LSPT; https://www.sfei.org/projects/landscape-scenario-planning-tool) from 2016
#fine-scale vegetation mapping from CDFW VegCAMP (CDFW 2019), and 2016 crop mapping
#from LandIQ (CDWR and LandIQ 2020). 


#####################
#####################
##  Current problem: this shape file is transforming correctly!
# Error generated: "GDAL Message 1: Sub-geometry 0 has coordinate dimension 2, but container has 3"
cropmap2022 <- sf::read_sf(file.path(folder ,"LandIQ/i15_Crop_Mapping_2022_Provisional_shp/i15_Crop_Mapping_2022_Provisional.shp"))
str(cropmap2022)
st_crs(data$areacolumn) <- 4326
cropmap2022t <-cropmap2022%>%
  st_transform(crs = 4326)
cropmap2022t %>% 
  ggplot() +
  geom_sf()

# Chris says LandIQ data is part of the data that Angel put together to KNB


crops <- file.path(folder,"NatHabMgeWetland/PM4.16.gdb")
sf::st_layers(dsn = crops)
mgwetlnd <- sf::st_read(dsn = crops, layer= "ManagedWetlands_2016")
mgwetlnd %>%
  ggplot() +
  geom_sf()
nathab <- sf::st_read(dsn = crops, layer= "Natural_Habitat")
nathab %>%
  ggplot() +
  geom_sf()
names(nathab)


# Sacramento - San Joaquin Delta Vegetation and Land Use Update 2016 (CDFW) (zip)
vc_delta_2016 <- file.path(folder,"VegCAMP/2855/ds2855.gdb")
sf::st_layers(dsn = vc_delta_2016)
delta_2016 <- sf::st_read(dsn = vc_delta_2016, layer= "ds2855")
delta_2016 %>%
  ggplot() +
  geom_sf()






#To develop the Maximum potential scenario, we first
#erased areas that were classified as urban or barren. Of the remaining area, everywhere
#currently within intertidal elevations was assigned to tidal wetland. 


# Subsided areas, defined as below mean lower low water (MLLW) according to a 2017 tidally referenced DEM (DSC 2022b;
# SFEI 2022), were assigned to be nontidal peat-building wetland managed for subsidence reversal (Fig. A2).
# Vaughn et al. preview

#To develop the GHG-habitat scenario, we first excluded areas that were
#classified as developed, existing wetland, or cultivated for rice by the LandIQ 2018 dataset of
#agricultural parcels (CDWR and LandIQ 2021).  
#We selected LandIQ parcels that are currently within the intertidal zone for conversion to tidal marsh.
#This area included only 12,756 ha, short
#of the 13,200 ha (32,500 acres) of tidal marsh called for in the Delta Plan Performance Measure
#4.16 from the amended chapter 4, “Protect, Restore, and Enhance the Delta Ecosystem” (DSC 2022a).
                                                                                      
#In addition to this 12,756 ha of tidal wetland,
#the GHG-habitat scenario included 12,165 ha of managed wetland for subsidence reversal and
#carbon sequestration (Delta Plan Performance Measure 5.2; DSC 2013).....


#To develop landscape configurations for GHG 1 and GHG 2, we first excluded areas that were
#classified as developed, existing wetland, or cultivated for rice and assigned LandIQ parcels to
#be nontidal peat-building wetland or rice field *****on the basis of highest rank for potential GHG
#benefit**** [not clear] for a total of 30,972 ha (76,500 acres) for GHG 1 and 15,425 ha (38,100 acres) for GHG
#2. For each scenario, the first 45% of parcel acreage was assigned to rice and the remaining
#parcels were assigned to nontidal peat-building wetland.









# EXTRA STUFF below here ___________________

tidal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"))%>%
  st_transform(crs = 4326)
tidal %>% 
  ggplot() +
  geom_sf()

# try terra version of shp read (as an example)
# path <- file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_LegalBoundary/legal_delta_SacSJ.shp") 
# v <- vect(path)
# # plot the shapefile
# plot(v)

# From previous instructions:
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


# convert tidal into a terra vector object
tidal_tv <- vect(tidal)
# plot the converted shapefile
plot(tidal_tv)

# reproject tidal_tv with the same CRS as the canopy raster
tidal_tv_reproj <- project(tidal_tv, crs(c2016r))

# crop canopy raster and then masking with our re-projected tidally influenced boundary
# NOTE: takes a while to run
# Note: if mask= F, the crop will be by extent (box) ###
c2016rcp <- crop(c2016r, tidal_tv_reproj, mask= T)
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
vv <- vect(vertical_line_sf)
p <- Ctidal_tv , vv)
plot(p, col=c("blue", "red"))

# Plotting
plot(tidal_tv, main = "tidal with Point and Line")
plot(midpoint_lat_lon, add = TRUE, pch = 19, col = "red")
plot(vertical_line_sf, add = TRUE, col = "blue")


# 3.3: clip all spatial datasets within each of the polygons



