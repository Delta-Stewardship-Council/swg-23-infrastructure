## code for sketch plots for each layer

## Set up
library(tidyverse)
library(leaflet)
library(sf)

## Read data

prob_fail <- st_read("data-clean/shapefiles/probFailure/prob_fail_levee_area.shp")
st_crs(prob_fail)
prob_fail_w84 <- prob_fail %>% 
  st_transform('+proj=longlat +datum=WGS84')

structure_value <- st_read("data-clean/shapefiles/nationalStructureInventory/nsi_2022_total_value_levee_area.shp")

managed_wetlands <- st_read("data-clean/shapefiles/managedWetlands/managedWetlands.shp")

croplands <- st_read("data-clean/shapefiles/deltaCropland/deltaCropland.shp")

## all layers are projected in California Albers

## Plots

## Probability of Failure

## color pallet
prob_fail_pal <- colorNumeric(
  palette = "viridis", 
  domain = prob_fail_w84$lev_flr)
  # na.color = "gray")

leaflet::leaflet(prob_fail_w84) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = prob_fail_w84,
              fillColor = ~prob_fail_pal(lev_flr),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = prob_fail_pal,
            values = ~lev_flr,
            opacity = 0.6,
            title = "Delta Levee Probability of Failure",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)





