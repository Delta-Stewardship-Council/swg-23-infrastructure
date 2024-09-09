# Install and load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(dplyr)
library(rvest)
library(httr)
library(terra)
library(sf)

# Step 1: Scrape the download site for the newest dataset
downloadUrl <- "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/"
webpage <- read_html(downloadUrl)
links <- webpage %>% 
  html_nodes("a") %>% 
  html_attr("href")
zipLinks <- links[grep("\\.zip$", links)]
# Assuming the first link is the newest
newestZip <- zipLinks[1]  
cat("The newest file appears to be", shQuote(newestZip))
# Check this assumpetion

# Step 2: Download the file
tempFile <- tempfile(fileext = ".zip")
download.file(paste0(downloadUrl, newestZip), tempFile, mode = "wb")

# Step 3: Unzip the file
unzipDir <- "data-raw/shapefiles/croplandDataLayer"
dir.create(unzipDir, showWarnings = FALSE)
unzip(tempFile, exdir = unzipDir)

# Step 4: Identify the .tif file
tifFiles <- list.files(unzipDir, pattern = "\\.tif$", full.names = TRUE)

# Step 5: Read in the .tif file
raster <- rast(tifFiles)
# Plotting it
plot(raster)

# Step 6: Filter for only the tidal Delta
# First, we need to get California's boundary
deltaPath <- "data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"
deltaBoundary <- read_sf(deltaPath) %>% 
  st_transform(crs(raster))

# Crop the raster to California's extent
deltaRaster <- crop(raster, deltaBoundary)
deltaRaster <- mask(deltaRaster, deltaBoundary)

# Step 7: Save the file in a format that can be used by the 'sf' package
# Convert raster to polygons
deltaCropPolygons <- as.polygons(deltaRaster)

# Convert to sf object
deltaCropSF <- st_as_sf(deltaCropPolygons) %>% 
  st_transform(3310)

# Save as shapefile
cleanedDataDirectory <- "data-clean/shapefiles/deltaCropland"
dir.create(cleanedDataDirectory, showWarnings = FALSE)
st_write(deltaCropSF, file.path(cleanedDataDirectory, "deltaCropland.shp"), delete_layer = TRUE)

print("Pipeline completed successfully. Output saved as 'deltaCropland.shp'")