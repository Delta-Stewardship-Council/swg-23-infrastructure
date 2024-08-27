## This script looks into Structure data from the National Survay Infrastructure and crops the CS data into the bounding box of our area of interest (levee polygons). It then creates a DF with all the type of structrs in the area of interest. The idea is to use the list af all the structure types to give value to each of them. Then, be able to calculate the value of structures in each levee polygon.

library(sf)
library(dplyr)
library(htmltools)
library(rvest)
library(janitor)
library(readr)

## National Survay Infrastructure data for CA - downloaded from [here](https://www.hec.usace.army.mil/confluence/nsi/userguides/download-tool), selecting CA on the map. Data downloaded 1014-08-22


## Note, to run this line you need to download the data in the link above and added to your local project under data-raw/gpkg folders.
nsi <- read_sf("data-raw/gpkg/nsi_2022_06.gpkg")

## Levee areas
levee_areas <- read_sf("data-clean/shapefiles/fixedLevees/leveedAreas.shp")
plot(levee_areas$geometry)

## Exploring data
st_bbox(levee_areas)
st_bbox(nsi)

st_crs(levee_areas)
st_crs(nsi)

## transform to nsi CRS
levee_4326 <- st_transform(levee_areas,
                           crs = 4326)

delta_bbox <- st_bbox(levee_4326)

nsi_delta <- st_crop(nsi, delta_bbox)

write_sf(nsi_delta, "data-clean/nsi_2022_delta_area.shp")


## reading saved shapefile
delta_nsi_shp <- read_sf("data-clean/nsi_2022_delta_area.shp")


## Creating table with structure type and its full name provided by NSI documentation online.
## URL from where to extract the table
nsi_url <- "https://www.hec.usace.army.mil/confluence/nsi/userguides/survey-tool/attribute-guide"

## list with all the tables in that url
df <- nsi_url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

## table of interest
occ_type <- df[[2]] %>% 
  clean_names()

## delta structure code + key

delta_occtype <- delta_nsi_shp %>% 
  select(occtype) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  rename(occupancy_type = occtype) %>% 
  left_join(occ_type, by = "occupancy_type") %>% 
  arrange(occupancy_type)

write_csv(delta_occtype, "data-clean/nsi_2022_06_occupancy_type.csv")
