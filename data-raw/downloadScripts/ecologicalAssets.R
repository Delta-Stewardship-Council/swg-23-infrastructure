# This script pulls the datasources for the ecological assets categories

# Libraries ---------------------------------------------------------------

library(rvest)
library(dplyr)
library(sf)

# Remove scientific notation
options(scipen = 9999)

# Helper functions
source(file.path("R", "downloadCNRA.R"))

# Data lists --------------------------------------------------------------

# Contains shape files read into R
shapefiles <- list()
# Contains areas of polygons derived from the shapefiles
area <- list()

# Conservation easements --------------------------------------------------

downloadCNRA(url = "https://data.cnra.ca.gov/dataset/california-conservation-easement-database", 
             fileLink = "https://data.cnra.ca.gov/dataset/31b65732-941d-4af0-9d8c-279fac441fd6/resource/94b36840-0540-445b-8caf-19657fcc5fda/download/cced_2023a.zip") %>% 
  unzipShapefile(outPath = file.path("data-raw", "shapefiles", "conservationEasements"))

# Getting metadata from the files
shapefiles$conservationEasements <- st_read(file.path("data-raw", "shapefiles", "conservationEasements", "CCED_2023a_Release.shp"))

# st_area can be used to calculate the area of a polygon. Here, it's already calculated
area$conservationEasements <- shapefiles$conservationEasements %>% 
  data.frame() %>% 
  transmute(siteName = sitename, Shape_Area,
            area = as.numeric(st_area(shapefiles$conservationEasements)))

all(all.equal(area$conservationEasements$Shape_Area, area$conservationEasements$area))

# Protected areas ---------------------------------------------------------
downloadCNRA("https://data.cnra.ca.gov/dataset/california-protected-areas-database",
             "https://data.cnra.ca.gov/dataset/0ae3cd9f-0612-4572-8862-9e9a1c41e659/resource/27323846-4000-42a2-85b3-93ae40edeff9/download/cpad_2023a.zip") %>% 
  unzipShapefile(file.path("data-raw", "shapefiles", "protectedAreas"))

shapefiles$protectedAreas <- st_read(file.path("data-raw", "shapefiles", "protectedAreas", "CPAD_2023a_Holdings.shp"))

area$protectedAreas <- shapefiles$protectedAreas %>% 
  data.frame() %>% 
  transmute(siteName = SITE_NAME,
            shapeArea = as.numeric(st_area(shapefiles$protectedAreas)))

# i07 Habitat Delta 1977 --------------------------------------------------
downloadCNRA("https://data.cnra.ca.gov/dataset/i07-habitat-delta-1977",
             "https://gis.data.cnra.ca.gov/datasets/d790064980d04fad8fb55270b0aea3e3_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
             method = "curl") %>% 
  unzipShapefile(file.path("data-raw", "shapefiles", "habitatDelta1977"))

shapefiles$habitatDelta1997 <- st_read(file.path("data-raw", "shapefiles", "habitatDelta1977", "i07_Habitat_Delta_1977.shp"))

area$habitatDelta1997 <- shapefiles$habitatDelta1997 %>% 
  data.frame() %>% 
  transmute(siteType = TYPE,
            dateData = Date_Data_,
            lastModified = Last_Mod_1,
            Shape__Are,
            area = as.numeric(st_area(shapefiles$habitatDelta1997)),
            length = Shape__Len)

all(all.equal(area$habitatDelta1997$Shape__Are, area$habitatDelta1997$area))

# Water storage -----------------------------------------------------------

# Not downloaded. Shows how full the reservoirs are, but I can't think of why 
# this would be needed. Also, the data seems to be not updated, e.g., today
# is 8-23-23 but data is shown for May 4 (of which year I cannot tell.)

# subsidence data ---------------------------------------------------------

# This might require a bit more work. I could not find a shape file for this
downloadCNRA("https://data.cnra.ca.gov/dataset/tre-altamira-insar-subsidence",
             "https://data.cnra.ca.gov/dataset/5e2d49e1-9ed0-425e-9f3e-2cda4a213c26/resource/8b5aec90-61bc-4ffa-abeb-c9f8261989ac/download/vertical_displacement_point_data_locations_2023q2.gdb.zip", 
             path = file.path("data-raw", "shapefiles", "vertical_displacement_point_data_locations_2023q2.gdb.zip")) %>% 
  unzip(file.path("data-raw", "shapefiles", "subsidenceGDB",
                  "vertical_displacement_point_data_locations_2023q2.gdb.zip"),
        exdir = file.path("data-raw", "shapefiles", "subsidenceGDB"))

# # This can be read into R via:
# layersAvailable <- st_layers(file.path("data-raw", "shapefiles", "subsidenceGDB",
#                                        "Vertical_Displacement_Point_Data_Locations_2023Q2.gdb"))
# subsidence <- st_read(file.path("data-raw", "shapefiles", "subsidenceGDB",
#                                 "Vertical_Displacement_Point_Data_Locations_2023Q2.gdb"))
# # However, this is a very large file...takes about 5 min to read into R. 
# # Don't really know how to grab the subsidence data from this database...

# Groundwater sustainability plan -----------------------------------------

# There are many csv files here. One example that we can pull is the total water use
# downloadCNRA("https://data.cnra.ca.gov/dataset/gspar")

downloadCNRA("https://data.cnra.ca.gov/dataset/gspar",
             "https://data.cnra.ca.gov/dataset/05041838-896f-4d1a-beff-0fd43b5a3e0f/resource/0860c5f6-90e5-426f-b274-58c4bc72c2af/download/total_water_use.csv") %>% 
  read.csv()

# Native Species Richness -------------------------------------------------

# Active website...Will need RSelenium...Can't get this to work on my state laptop
# since my organization manages my browser.

# Saving the RData file ---------------------------------------------------

# # As of 8.24.23, this file is 192 mb. File too large to download. Likely due to
# # the size of the subsidence data. Removing subsidence data, still 86.6 mb. Easier
# # to simply read the file instead.
# save.image(file = file.path("data-raw", "RData", "ecologicalAssets.RData"))

