# To be used after running everything from leveeFix.R

library(leaflet)

polygonMapData <- bind_rows(
  shapefiles$deltaLegal %>% 
    mutate(dataset = "delta"),
  polygonDF %>% 
    filter(!st_is_empty(.)) %>% 
    mutate(dataset = "levees")
) %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326) %>% 
  mutate(LMA = ifelse(is.na(LMA), "Legal Delta", LMA))

pal <- colorFactor("inferno", domain = polygonMapData$LMA)

linesMapData <- shapefiles$levees %>% 
  filter(LMA %in% {polygonDF %>% 
      filter(st_is_empty(.)) %>% 
      pull(LMA)}) %>% 
  mutate(dataset = "leftover") %>% 
  st_transform(4326)

# Plotting

leaflet() %>% 
  addPolygons(
    data = polygonMapData,
    stroke = F,
    fillColor = ~pal(LMA),
    fillOpacity = 0.55, smoothFactor = 0.5,
    popup = paste0(polygonMapData$LMA)
  ) %>% 
  addPolylines(
    data = linesMapData,
    weight = 4, color = "green"
  ) %>% 
  addProviderTiles("Esri.WorldImagery")


