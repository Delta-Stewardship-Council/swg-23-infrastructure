## Exploring raster data: Invest output

## Set up ----
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)


## read data ----
## raster
run3_dollars <- rast("data-raw/rasters/InVEST_run3/tot_c_cur.tif")

plot(run3_dollars)

## shapefile with levee polygons

levee_areas <- vect("data-clean/shapefiles/fixedLevees/leveedAreas.shp")

levee_areas_sf <- read_sf("data-clean/shapefiles/fixedLevees/leveedAreas.shp") %>% 
  mutate(area_id = paste0("island_", 1:130))

## Check projection ----
crs(run3_dollars,
    describe = TRUE)

crs(levee_areas,
    describe = TRUE)

## Note: both files are in same CRS NAD83

plot(run3_dollars)
lines(levee_areas)

## raster values ----
rast_freq <- freq(run3_dollars)

## values per area

levee_areas_total_value <- zonal(run3_dollars, levee_areas, fun = "sum") %>% 
  mutate(area_id = paste0("island_", 1:130))

## joining total value to shapefile
area_total_value_sf <- levee_areas_sf %>% 
  left_join(levee_areas_total_value, by = "area_id")


## Plot total currency
ggplot(area_total_value_sf)+
  geom_sf(aes(fill = tot_c_cur))+
  labs(fill = "Total $ Value") +
  scale_fill_continuous(low = "khaki",
                        high =  "firebrick",
                        labels = comma)


