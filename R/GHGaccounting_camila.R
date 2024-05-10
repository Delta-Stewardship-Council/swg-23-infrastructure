
## Script with general file path that make reading in the GHG data more straight forward (R project is set up accordingly)

## In this case all file paths to files ar relative to directories within the Rproj or directly from a url

## Note that the dambay file is ignored by git given the size of the file. You can access this file by downloading from the SharePoint folder: Infrastructure Group > Data Sets and Products > GHG > dem_bay_delta_10m_20201207.zip

## Safe the dem_bay files inside thedata-raw folder and use the code below to read in the data.


dembayr <- rast("data-raw/dem_bay_delta_10m_20201207.tif")

dembayp <- project(dembay, "EPSG:3310")

tidal <- read_sf("data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp")


conus_2001_url <- "https://coastalimagery.blob.core.windows.net/ccap-landcover/CCAP_bulk_download/Regional_30meter_Land_Cover/conus_2001_ccap_landcover_20200311.tif"

conus_2016_url <- "https://coastalimagery.blob.core.windows.net/ccap-landcover/CCAP_bulk_download/Regional_30meter_Land_Cover/conus_2016_ccap_landcover_20200311.tif"

c2016r <- rast(conus_2016_url)
c2001r <- rast(conus_2001_url)
