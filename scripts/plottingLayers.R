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

structure_value <- st_read("data-clean/shapefiles/nationalStructureInventory/nsi_2022_total_value_levee_area.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')

managed_wetlands <- st_read("data-clean/shapefiles/managedWetlands/managedWetlands.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

croplands <- st_read("data-clean/shapefiles/deltaCropland/deltaCropland.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

soc_vul <- st_read("data-clean/shapefiles/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>% 
  select(LMA, area, leveeAr, RPL_THEMES) %>% 
  st_transform('+proj=longlat +datum=WGS84')

## Even though all layers where initially in CA Albers, Leaflet was giving me a warning about the need to re project into '+proj=longlat +datum=WGS84'. Now all data layers have been converted to the projection Leaflet likes.

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
  # addPolygons(data = structure_value,
  #             fillColor = "gray",
  #             fillOpacity = 0.4) %>%  ## adds structure value polygon on top
  addLegend(pal = prob_fail_pal,
            values = ~lev_flr,
            opacity = 0.6,
            title = "Delta Levee Probability of Failure",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)



## Structure values

## color pallet
structure_value_pal <- colorNumeric(
  palette = "viridis", 
  domain = structure_value$strct__)
# na.color = "gray")

leaflet::leaflet(structure_value) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = structure_value,
              fillColor = ~structure_value_pal(strct__),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = structure_value_pal,
            values = ~strct__,
            opacity = 0.6,
            title = "Value ($) of all structures in a levee area",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)


## Managed Wetlands
rainbow <- c("#ff0000", "#ff5300", "#ffa500", "#ffd200", "#ffff00", "#80c000", "#008000", "#004080", "#0000ff", "#2600c1", "#4b0082")

## color pallet
managed_wetlands_pal <- colorFactor(
  palette = rainbow,
  domain  = managed_wetlands$PM_LndT)
# na.color = "gray")

leaflet::leaflet(managed_wetlands) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = managed_wetlands,
              fillColor = ~managed_wetlands_pal(PM_LndT),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = managed_wetlands_pal,
            values = ~PM_LndT,
            opacity = 0.6,
            title = "Test",
            position = "bottomleft") %>%
  setView(lat=38.2, lng=-121.7, zoom=9)


## Croplands
unique(croplands$Clss_Nm)

## there are 74 types of croplands! To hard to plot them all on a map. Consulting the group about grouping them.

## Social Vulnerability

## color pallet
soc_vul_pal <- colorNumeric(
  palette = "viridis", 
  domain = soc_vul$RPL_THEMES)
# na.color = "gray")

leaflet::leaflet(soc_vul) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = soc_vul,
              fillColor = ~soc_vul_pal(RPL_THEMES),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addLegend(pal = soc_vul_pal,
            values = ~RPL_THEMES,
            opacity = 0.6,
            title = "Social Vulnerability Index",
            position = "bottomleft") %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)
