## code for sketch plots for each layer

## Set up
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)

## Read data

prob_fail <- st_read("data-clean/shapefiles/probFailure/prob_fail_levee_area.shp")
st_crs(prob_fail)
prob_fail_w84 <- prob_fail %>% 
  st_transform('+proj=longlat +datum=WGS84')

structure_value <- st_read("data-clean/shapefiles/nationalStructureInventory/nsi_2024_total_value_levee_area.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')

managed_wetlands <- st_read("data-clean/shapefiles/managedWetlands/managedWetlands.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

croplands <- st_read("data-clean/shapefiles/deltaCropland/deltaCropland.shp") %>% 
  st_transform('+proj=longlat +datum=WGS84')

soc_vul <- st_read("data-clean/shapefiles/leveeLSDayPopWeightSVI/leveeLSDayPopWeightSVI.shp") %>% 
  select(LMA, area, leveeAr, RPL_THEMES) %>% 
  st_transform('+proj=longlat +datum=WGS84')

## Even though all layers where initially in CA Albers, Leaflet was giving me a warning about the need to re project into '+proj=longlat +datum=WGS84'. Now all data layers have been converted to the projection Leaflet likes.

## color pallet ----

## Probability of Failure

prob_fail_pal <- colorNumeric(
  palette = "viridis", 
  domain = prob_fail_w84$lev_flr)
  # na.color = "gray")

## Managed Wetlands
rainbow <- c("#ff0000", "#ff5300", "#ffa500", "#ffd200", "#ffff00", "#80c000", "#008000", "#004080", "#0000ff", "#2600c1", "#4b0082")


managed_wetlands_pal <- colorFactor(
  palette = rainbow,
  domain  = managed_wetlands$PM_LndT)
# na.color = "gray")

## Croplands
unique(croplands$Clss_Nm)

crops_pal <- colorFactor(
  palette = rainbow,
  domain  = croplands_complete$type)

## Social Vulnerability

soc_vul_pal <- colorNumeric(
  palette = "viridis", 
  domain = soc_vul$RPL_THEMES)
# na.color = "gray")


## Plot ----
## One map with all layers ----
leaflet() %>%
  ## add tiles
  addProviderTiles(providers$CartoDB.Positron, 
                   group = "Grey background") %>%
  addProviderTiles("Esri.WorldImagery", 
                   group = "Imagery") %>%
  ## Add all polygon layers
  addPolygons(data = soc_vul,
              group = "Social Vulnerability",
              fillColor = ~soc_vul_pal(RPL_THEMES),
              label = ~htmlEscape(
                paste("SOVI Index:", RPL_THEMES)),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addPolygons(data = prob_fail_w84,
              group = "Probability of Failure",
              fillColor = ~prob_fail_pal(lev_flr),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addPolygons(data = managed_wetlands,
              group = "Habitat Type",
              fillColor = ~managed_wetlands_pal(PM_LndT),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  addPolygons(data = croplands_complete,
              group = "Croplands",
              fillColor = ~crops_pal(type),
              # label = soc_vul_labs,
              stroke = TRUE,
              fillOpacity = 0.6, 
              color="black", # polygon border color
              weight=0.8, ) %>% # polygon border weight
  ## Legend layers
  addLegend(group = "Social Vulnerability",
            pal = soc_vul_pal,
            values = soc_vul$RPL_THEMES,
            opacity = 0.6,
            title = "Index",
            position = "bottomleft") %>%
  addLegend(group = "Probability of Failure",
            pal = prob_fail_pal,
            values = prob_fail_w84$lev_flr,
            opacity = 0.6,
            title = "Probability of Failure",
            position = "bottomleft") %>%
  addLegend(group = "Habitat Type",
            pal = managed_wetlands_pal,
            values = managed_wetlands$PM_LndT,
            opacity = 0.6,
            title = "Habitat Type",
            position = "bottomleft") %>% 
  addLayersControl(
    baseGroups = c("Grey background", "Imagery"),
    overlayGroups = c("Social Vulnerability",
                      "Probability of Failure",
                      "Managed Wetlands"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # hide these groups by default
  hideGroup(c("Social Vulnerability",
              "Probability of Failure",
              "Habitat Type",
              "Croplands")) %>% 
  setView(lat=38.2, lng=-121.7, zoom=9)
  




