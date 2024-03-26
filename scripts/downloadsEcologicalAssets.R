# This script pulls the datasources for the ecological assets categories

# Libraries ---------------------------------------------------------------

library(rvest)
library(dplyr)
library(sf)
library(readr)
library(patchwork)
library(ggplot2)

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
             file = "California Conservation Easement Database 2023aSHP") %>% 
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
             file = "California Protected Areas Database 2023a releaseSHP") %>% 
  unzipShapefile(file.path("data-raw", "shapefiles", "protectedAreas"))

shapefiles$protectedAreas <- st_read(file.path("data-raw", "shapefiles", "protectedAreas", "CPAD_2023a_Holdings.shp"))

area$protectedAreas <- shapefiles$protectedAreas %>% 
  data.frame() %>% 
  transmute(siteName = SITE_NAME,
            shapeArea = as.numeric(st_area(shapefiles$protectedAreas)))

# i07 Habitat Delta 1977 --------------------------------------------------
downloadCNRA("https://data.cnra.ca.gov/dataset/i07-habitat-delta-1977",
             "ShapefileZIP",
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
# For now, download the csv file at:
# https://gis.data.ca.gov/datasets/CDFW::statewide-terrestrial-native-species-richness-summary-ace-ds1332/about

shapefiles$speciesRichness <- st_read("data-raw/shapefiles/nativeSpeciesRichness/Statewide_Terrestrial_Native_Species_Richness_Summary_-_ACE_[ds1332].shp") %>% 
  rename(
    ecoRegionNativeRank = NtvRankEco,
    ecoRegionNativeWeight = NtvSumEco,
    stateNativeRank = NtvRankSW,
    stateNativeWeight = NtvSumSW,
    nativeCount = NativeCoun,
    nativeAmphibianCount = NtvAmph,
    nativeReptailCount = NtvRept,
    nativeBirdCount = NtvBird,
    nativeMammalCount = NtvMamm,
    nativePlantCount = NtvPlnt,
    gameCount = GameCount,
    climateVulnerableCount = ClimVulCou,
    ecoRegionCode = Eco_Sect,
    ecoRegionName = Eco_Name,
    ecoRegionCodeJepson = Jepson_Eco,
    county = County,
    area = Shape__Are,
    length = Shape__Len
  )

# area$nativeRichness <- read_csv(
#   file.path("data-raw", "csvFiles", "Statewide_Terrestrial_Native_Species_Richness_Summary_-_ACE_[ds1332].csv"), 
#   col_types = cols(
#     OBJECTID = col_double(),
#     Hex_ID = col_double(),
#     NtvRankEco = col_double(),
#     NtvSumEco = col_double(),
#     NtvRankSW = col_double(),
#     NtvSumSW = col_double(),
#     NativeCount = col_double(),
#     NtvAmph = col_double(),
#     NtvRept = col_double(),
#     NtvBird = col_double(),
#     NtvMamm = col_double(),
#     NtvPlnt = col_double(),
#     GameCount = col_double(),
#     ClimVulCount = col_double(),
#     Eco_Sect = col_character(),
#     Eco_Name = col_character(),
#     Jepson_Eco = col_character(),
#     County = col_character(),
#     Shape__Area = col_double(),
#     Shape__Length = col_double()
#   ),
# ) %>% 
#   rename(
#     ecoRegionNativeRank = NtvRankEco,
#     ecoRegionNativeWeight = NtvSumEco,
#     stateNativeRank = NtvRankSW,
#     stateNativeWeight = NtvSumSW,
#     nativeCount = NativeCount,
#     nativeAmphibianCount = NtvAmph,
#     nativeReptailCount = NtvRept,
#     nativeBirdCount = NtvBird,
#     nativeMammalCount = NtvMamm,
#     nativePlantCount = NtvPlnt,
#     gameCount = GameCount,
#     climateVulnerableCount = ClimVulCount,
#     ecoRegionCode = Eco_Sect,
#     ecoRegionName = Eco_Name,
#     ecoRegionCodeJepson = Jepson_Eco,
#     county = County,
#     area = Shape__Area,
#     length = Shape__Length
#   )

# delta levees ------------------------------------------------------------

# Have to download delta levee shape files
# "https://gis.data.ca.gov/datasets/7995cfe94bcc4e15afe92f40efb66cc6_0/explore"

# shapefiles$levees <- st_read(file.path("data-raw", "shapefiles", "deltaLeveeAreas", 
#                                        "i17_Delta_Levees_Centerlines_2017.shp")) %>%
#   st_transform(crs = 3310)
# 
# area$levees <- lapply(na.omit(unique(shapefiles$levees$LMA)),
#                       function(x) {
#                         polygon <- shapefiles$levees %>% 
#                           filter(LMA == x) %>% 
#                           st_union() %>% 
#                           st_polygonize()
#                         
#                         area <- polygon %>% 
#                           st_area() %>% 
#                           units::set_units(acre) %>% 
#                           as.numeric()
#                         
#                         if (area == 0) {
#                           p = NULL
#                         } else {
#                           p <- ggplot() +
#                             geom_sf(data = polygon, fill = "lightblue") +
#                             labs(title = x, subtitle = paste0("Acres: ", area)) +
#                             theme_minimal()
#                         }
#                         
#                         df <- tibble(LMA = x,
#                                      acres = area)
#                         
#                         list(df = df,
#                              polygon = polygon,
#                              p = p)
#                       }) %>% 
#   setNames(na.omit(unique(shapefiles$levees$LMA)))
# 
# area$leveesDF <- lapply(area$levees, "[[", 1) %>% 
#   bind_rows() %>% 
#   arrange(-acres) %>% 
#   mutate(LMA = tolower(LMA)) %>% 
#   full_join(read.csv(file.path("data-raw", "csvFiles", "DLIS.IslandBasics.csv"), check.names = F) %>% 
#               transmute(LMA = tolower(`Field Name`),
#                         acresDLIS = `Area (acres)`,
#                         priorityIsland = `Priority Island`,
#                         leveed = Leveed,
#                         active = `Active for DLIS`),
#             by = "LMA") %>% 
#   mutate(difference = acres - acresDLIS)
# 
# # Loading in the DLIS islands shape file to compare areas highlighted
# shapefiles$DLISLevees <- st_read(file.path("data-raw", "shapefiles", "RevisedIslands_160912_AllIslands", 
#                                            "RevisedIslands_160912_AllIslands.shp"))

# i03 Suisun Marsh --------------------------------------------------------

downloadCNRA("https://data.cnra.ca.gov/dataset/i03-suisunmarshboundary", file = "ShapefileZIP",
             method = "curl") %>% 
  unzipShapefile(outPath = file.path("data-raw", "shapefiles", "suisunMarsh"))

# Saving the RData file ---------------------------------------------------

# # As of 8.24.23, this file is 192 mb. File too large to download. Likely due to
# # the size of the subsidence data. Removing subsidence data, still 86.6 mb. Easier
# # to simply read the file instead.
# save.image(file = file.path("data-raw", "RData", "ecologicalAssets.RData"))

