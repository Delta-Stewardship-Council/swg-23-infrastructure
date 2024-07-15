# This explores the levee centerlines
source(file.path("R", "createLeveePolygons.R"))

library(mapview)
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)

# Ruleset:

# 1. 2017 levee centerline shapefile, polygonize these leveed areas
# 1b. shared levee lines
# 2. Integrate the suisun marsh dataset
# 3. NLD centerline shapefile, use these additional centerlines to complete uncompleted areas
# 4. Integrate the Delta border, tidally influenced version for now, and find
# shortest distance of bordering incomplete polygons to the border

#### IMPORTANT ####
# Make sure you run through at least these two sections in "ecologicalAssets.R":
# Section "Data lists"
# Section "delta levees"

shapefiles <- list()
polygons <- list()
areas <- list()

# Rule 1: 2017 levee centerline -------------------------------------------
# Use of centerline to make into polygons
# Available here: https://hub.arcgis.com/datasets/7995cfe94bcc4e15afe92f40efb66cc6_0/about

leveeURL <- "https://gis.water.ca.gov/arcgis/rest/services/Structure/i17_Delta_Levees_Centerlines_2017/FeatureServer/0/query"

queryParameters <- list(
  outFields = "*",
  where = "1=1",
  f = "geojson"
)

shapefiles$levees <- readFromAPI(leveeURL, queryParameters) %>% 
  st_transform(crs = 3310)

polygons$leveesAll <- lapply(na.omit(unique(shapefiles$levees$LMA)),
                      function(x) {
                        polygon <- shapefiles$levees %>% 
                          filter(LMA == x) %>% 
                          st_union() %>% 
                          st_polygonize()
                        
                        area <- polygon %>% 
                          st_area() %>% 
                          units::set_units(acre) %>% 
                          as.numeric()
                        
                        if (area == 0) {
                          p = NULL
                        } else {
                          p <- ggplot() +
                            geom_sf(data = polygon, fill = "lightblue") +
                            labs(title = x, subtitle = paste0("Acres: ", area)) +
                            theme_minimal()
                        }
                        
                        df <- tibble(LMA = x,
                                     acres = area)
                        
                        list(df = df,
                             polygon = polygon,
                             p = p)
                      }) %>% 
  setNames(na.omit(unique(shapefiles$levees$LMA)))

# areas$RANDModel <- lapply(polygons$levees, "[[", 1) %>% 
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

# Loading in the DLIS islands shape file to compare areas highlighted
shapefiles$DLISLevees <- st_read(file.path("data-raw", "shapefiles", "RevisedIslands_160912_AllIslands", 
                                           "RevisedIslands_160912_AllIslands.shp"))

# Rule 1b: shared levee lines ---------------------------------------------
sharedLevee <- list()

# Tyler Island
sharedLevee$`Tyler Island` <- data.frame(LMA = "Tyler Island",
                                         sharedLevees(
  "Tyler Island", c(414, 415, 417)
))

# Jones Tract
sharedLevee$`Jones Tract` <- data.frame(LMA = "Jones Tract",
                                        st_intersection(
  sharedLevees("lower jones tract",
               c(filter(shapefiles$levees, LMA == "Upper Jones Tract") %>%
                   pull(OBJECTID),
                 255)),
  st_as_sfc(st_bbox(snapCenterlines(filter(shapefiles$levees,
                                           LMA %in% c("Lower Jones Tract", "Upper Jones Tract")),
                                    10), crs = 3310))
))

# Roberts Island
# Don't really know why both islands are classified as together; DLIS generally
# breaks up these areas, especially if there's a valid levee separating them.
sharedLevee$`Roberts Island` <- shapefiles$levees %>% 
  filter(LMA %in% c("Middle Roberts Island", "Upper Roberts Island") |
           OBJECTID %in% c(256, 255)) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_collection_extract() %>%
  .[c(1:2)] %>% 
  st_union() %>% 
  data.frame(LMA = "Middle & Upper Roberts Islands",
             .)

# Wright-Elmwood Tract
sharedLevee$`Wright-Elmwood Tract` <- data.frame(LMA = "Wright-Elmwood Tract",
                                                 sharedLevees("wright-elmwood tract", c(341, 342)))

# Little Egbert Island
sharedLevee$`Little Egbert Island` <- data.frame(LMA = "Little Egbert Island",
                                                 sharedLevees("little egbert island", 193))

# Prospect Island
sharedLevee$`Prospect Island` <- data.frame(LMA = "Prospect Island",
                                            sharedLevees("prospect island",
                                              c(473, 474, 283, 58)))

# Libby McNeil
sharedLevee$`Libby McNeil` <- data.frame(LMA = "Libby McNeil",
                                         sharedLevees("Libby McNeil", c(300, 421, 419, 420)))

# Drexler Tract
# Going to combine drexler and drexler pocket; cannot see how they are separated
sharedLevee$`Drexler` <- data.frame(LMA = "Drexler",
                                    sharedLevees(c("drexler", "drexler pocket"), c(277, 278)))

# Union Island
# RAND separates this into east and west, but I don't see a levee in the DWR's or USACE's datasets
# For now, will not split this
sharedLevee$`Union Island` <- shapefiles$levees %>%
  filter(LMA %in% c("Union Island West", "Union Island East") |
           OBJECTID %in% c(370, 40)) %>%
  st_union() %>%
  st_polygonize() %>% 
  data.frame(LMA = "Union Island",
             .)

# Stewart Tract
# RAND separates this into Stewart Tract and Mossdale Island. They appear to have
# used a union pacific railway as a levee that separates these two areas.
# I don't see this railway classified as a levee on either the DWR's or USACE's
# datasets. Will treat whole thing as Stewart Tract
sharedLevee$`Stewart Tract` <- data.frame(LMA = "Stewart Tract",
                                          sharedLevees(c("Stewart Tract", "Mossdale"), NA))

# Walnut Grove
# This polygonized fine, but because there are two polygons, only the first becomes a polygon
# if you use st_cast(). You have to st_collection_extract() if you want all the polygons

# Netherlands
# Have to draw a line across 283 and 435 to connect the polygon
sharedLevee$Netherlands <- bind_rows(
  drawLine(shapefiles$levees %>% 
             filter(OBJECTID == 283),
           shapefiles$levees %>% 
             filter(OBJECTID == 435), 4, 1),
  shapefiles$levees %>% 
    filter(LMA == "Netherlands" | OBJECTID %in% c(435, 215, 54, 55, 249))
) %>% 
  st_union() %>%
  st_polygonize() %>% 
  st_collection_extract() %>% 
  .[1:2] %>% 
  st_union() %>% 
  data.frame(LMA = "Netherlands",
             .)

# Honker Lake Tract and Holt Station
sharedLevee$`Honker Lake Tract and Holt Station` <- sharedLevees(c("Honker Lake Tract", "Holt Station"),
             c(185, 278, 255)) %>% 
  st_collection_extract() %>% 
  .[1:2] %>% 
  st_union() %>% 
  data.frame(LMA = "Honker Lake Tract and Holt Station",
             .)

# Glanville Tract
# Somehow the lines don't touch, by pixels. Going to just draw a line.
sharedLevee$`Glanville Tract` <- drawLine(shapefiles$levees %>% 
           filter(OBJECTID == 8),
         shapefiles$levees %>% 
           filter(OBJECTID == 209),
         2, 2) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(LMA == "Glanville Tract")) %>%
  st_union() %>% 
  st_polygonize() %>% 
  data.frame(LMA = "Glanville Tract",
             .)

# New Hope Tract
sharedLevee$`New Hope Tract` <- drawLine(shapefiles$levees %>% 
                                           filter(OBJECTID == 287),
                                         shapefiles$levees %>% 
                                           filter(OBJECTID == 289), 2, 1) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(LMA == "New Hope Tract")) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  data.frame(LMA = "New Hope Tract",
             .)

# Pearson District
sharedLevee$`Pearson District` <- sharedLevees("Pearson District", c(33, 196, 195)) %>% 
  st_collection_extract() %>% 
  .[1] %>% 
  data.frame(LMA = "Pearson District",
             .)

# Combining
polygons$rule1b <- bind_rows(
  lapply(polygons$leveesAll, function(x) {
    if (is.null(x$p)) {
      data.frame(LMA = x$df$LMA)
    } else {
      data.frame(LMA = x$df$LMA,
                 x$polygon)
    }
  }) %>% 
    bind_rows() %>% 
    mutate(dataset = "dwrCenterlines") %>% 
    st_sf() %>% 
    filter(!st_is_empty(geometry)),
  bind_rows(sharedLevee) %>% 
    mutate(dataset = "sharedLevee")) %>% 
  # Removing West Sacramento, Steward Tract (inner), 	Clifton Court Forebay
  # Cache Haas Area overlaps with Yolano in the NLD database,
  # Grizzly Slough as will be represented by the area in the NLD
  filter(!LMA %in% c("West Sacramento",
                     "Area adjacent to the Sacramento River Deep Water Ship Channel",
                     "Stewart Tract (inner)",
                     "Clifton Court Forebay",
                     "Area east of Shin Kee Tract",
                     "Area east of Rio Blanco Tract",
                     "Cache Haas Area",
                     "Grizzly Slough",
                     "Glanville Tract",
                     "Kasson District",
                     "Prospect Island",
                     "Holt Station",
                     "Van Sickle Island", # Van Sickle Already a part of the Suisun Marsh layer
                     "Summer Lake Subdivision", # An area within Hotchkiss Tract that's not needed
                     "Area adjacent to Sycamore Slough" # A weird polygonize that has an area of 0.000000002555919 acre
                     )
         )

# ggplot(polygons$rule1b, aes(fill = dataset)) +
#   geom_sf()

# Rule 2 Suisun Marsh integration -----------------------------------------

suisunURL <- "https://gis.water.ca.gov/arcgis/rest/services/Boundaries/i03_SuisunMarshBoundary/FeatureServer/0/query"

queryParameters <- list(
  outFields = "*",
  where = "1=1",
  f = "geojson"
)

shapefiles$`Suisun Marsh` <- readFromAPI(suisunURL, queryParameters) %>% 
  st_transform(crs = 3310) %>% 
  filter(LOCATION != "Primary (Water)") %>% 
  mutate(LMA = "Suisun Marsh")

polygons$ruleTwo <- bind_rows(
  polygons$rule1b,
  shapefiles$`Suisun Marsh` %>% 
    mutate(dataset = "suisunMarsh")
)

# ggplot(polygons$ruleTwo, aes(fill = dataset)) +
#   geom_sf()

# Rule 3: NLD centerline --------------------------------------------------

# This dataset is available online:
# https://hub.arcgis.com/maps/87acff1ba86c40098b59472292de3d11/about
# From there, you can click on layer 17, the polygon layer and use the API explorer
# Here, I am asking for all fields with STATES = 'California' in geojson format

nldURL <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/NLD2_PUBLIC_v1/FeatureServer/17/query"

queryParameters <- list(
  where = "STATES = 'California'",
  outFields = "*",
  f = "geojson"
)

shapefiles$nld <- readFromAPI(nldURL, query = queryParameters) %>% 
  st_transform(crs = 3310)

nldPolygons <- list()

# West Sacramento
# This contains area running alongside the toe drain that is not present in the RAND
nldPolygons$`West Sacramento` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "West Sacramento") %>% 
  mutate(LMA = "West Sacramento")

# Liberty Island - NLD
nldPolygons$`Liberty Island` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Solano County Levee 68") %>% 
  mutate(LMA = "Liberty Island")

# # Hasting Tract - NLD (overlaps with centerline)
# nldPolygons$`Hasting Tract` <- shapefiles$nld %>% 
#   filter(SYSTEM_NAME == "RD 2060 - Hastings Tract") %>% 
#   mutate(LMA = "Hastings Tract")

# Egbert Tract
nldPolygons$`Egbert Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 0536 - Egbert tract") %>% 
  mutate(LMA = "Egbert Tract")

# # Grand Island, include with it "Sacramento County Levee 26" from NLD
# nldPolygons$`Grand Island` <- shapefiles$nld %>% 
#   filter(SYSTEM_NAME %in% c("RD 0003 - Grand Island", "Sacramento County Levee 26")) %>% 
#   mutate(LMA = "Grand Island")

# Brannan-Andrus, use NLD
# Not using the full polygon since there is an "upper" Brannan-Andrus
nldPolygons$`Brannan-Andrus` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Brannan-Andrus LMD - RD 0556") %>% 
  st_cast("MULTILINESTRING") %>% 
  bind_rows(shapefiles$levees %>% 
              filter(OBJECTID == 397)) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_collection_extract() %>% 
  .[1] %>% 
  data.frame(LMA = "Brannan-Andrus",
             .)

# Brack Tract, use NLD
nldPolygons$`Brack Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "San Joaquin County Levee 56") %>% 
  mutate(LMA = "Brack Tract")

# Shin Kee Tract, does not fully touch in centerlines dataset, use NLD
# San Joaquin County Levee 4
nldPolygons$`Shin Kee Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "San Joaquin County Levee 4") %>% 
  mutate(LMA = "Shin Kee Tract")

# Rio Blanco Tract, does not touch, use NLD
# San Joaquin County Levee 117
nldPolygons$`Rio Blanco Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "San Joaquin County Levee 117") %>% 
  mutate(LMA = "Rio Blanco Tract")

# Byron Tract, use NLD
# RD 0800 - Byron Tract
nldPolygons$`Byron Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 0800 - Byron Tract") %>% 
  mutate(LMA = "Byron Tract")

# Hotchkiss Tract, use NLD
# Contra Costa County Levee 22
nldPolygons$`Hotchkiss Tract` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Contra Costa County Levee 22") %>% 
  mutate(LMA = "Hotchkiss Tract")

# Dutch Slough, use NLD
# Contra Costa County Levee 6
# Contra Costa County Levee 24
nldPolygons$`Dutch Slough` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("Contra Costa County Levee 6", "Contra Costa County Levee 24")) %>% 
  mutate(LMA = "Dutch Slough")

# Veale tract + Bixler Tract, use NLD
# Contra Costa County Levee 7
# Don't really know how to split into Bixler
nldPolygons$`Veale and Bixler Tracts` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("Contra Costa County Levee 7")) %>% 
  mutate(LMA = "Veale and Bixler Tracts")

# Beach Lake - Morrison Creek
nldPolygons$`Beach Lake - Morrison Creek` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Beach Lake - Morrison Creek") %>% 
  mutate(LMA = "Beach Lake - Morrison Creek")

# San Joaquin County Levee 24
nldPolygons$`San Joaquin County Levee 24` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% "San Joaquin County Levee 24") %>% 
  mutate(LMA = "San Joaquin County Levee 24")

# Contra Costa County Levee 14
nldPolygons$`Contra Costa County Levee 14` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% "Contra Costa County Levee 14") %>% 
  mutate(LMA = "Contra Costa County Levee 14")

# San Joaquin County Levee 114
nldPolygons$`San Joaquin County Levee 114` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "San Joaquin County Levee 114") %>% 
  mutate(LMA = "San Joaquin County Levee 114")

# San Joaquin County Levee 81 West
nldPolygons$`San Joaquin County Levee 81 West` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "San Joaquin County Levee 81 West") %>% 
  mutate(LMA = "San Joaquin County Levee 81 West")

# # Bear Creek - Units 7 west and 21
# nldPolygons$`Bear Creek - Units 7 west and 21` <- shapefiles$nld %>% 
#   filter(SYSTEM_NAME == "Bear Creek - Units 7 west and 21") %>%
#   mutate(LMA = "Bear Creek - Units 7 west and 21")

# Duck Creek left bank - Stockton
nldPolygons$`Duck Creek left bank - Stockton` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Duck Creek left bank - Stockton") %>% 
  mutate(LMA = "Duck Creek left bank - Stockton")

# Contra Costa County Levee 255
nldPolygons$`Contra Costa County Levee 255` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Contra Costa County Levee 255") %>% 
  mutate(LMA = "Contra Costa County Levee 255")

# Lakeshore Dry Levee
nldPolygons$`Lakeshore Dry Levee` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Lakeshore Dry Levee") %>% 
  mutate(LMA = "Lakeshore Dry Levee")

# Contra Costa County Levee 25
nldPolygons$`Contra Costa County Levee 25` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Contra Costa County Levee 25") %>% 
  mutate(LMA = "Contra Costa County Levee 25")

# Contra Costa County Levee 39
nldPolygons$`Contra Costa County Levee 39` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Contra Costa County Levee 39") %>% 
  mutate(LMA = "Contra Costa County Levee 39")

# Combining
polygons$ruleThree <- bind_rows(
  polygons$ruleTwo,
  bind_rows(nldPolygons) %>% 
    mutate(dataset = "nld")
)

# Rule 4: Shortest distance to the functional Delta -----------------------

shapefiles$functionalDelta <- st_read(file.path("data-raw", "shapefiles", "deltaBoundary", 
                                              "SacSJ_TidallyInfluencedBoundary", "Tidally_Influenced_Delta_SacSJ.shp")) %>% 
  st_transform(crs = 3310)

# Steps: 
# 1: join the boundary file
# 2: identify the applicable lines
# 3: st_boundary to find the end points
# 4: find shortest distance to the boundary

# Create three layers, the joined polygon (to rule 3), DLIS, and NLD to be plotted as layers
combinedData <- polygons$ruleThree %>%
  bind_rows(shapefiles$DLISLevees %>% 
              mutate(dateset = "DLIS"),
            shapefiles$nld %>% 
              mutate(dataset = "NLD")) %>% 
  st_cast("POLYGON") %>% 
  st_transform(4326)

# Create the map
leaflet() %>%
  # Add base map tiles
  addTiles() %>%
  # Add polygons$ruleThree layer
  addPolygons(
    data = subset(combinedData, dataset %in% c("dwrCenterlines", "sharedLevee", "suisunMarsh", "nld")),
    group = "Rule Three", # Assign a group for toggling
    fillColor = "blue", # Customize fill color
    fillOpacity = 0.5, # Customize fill opacity
    weight = 1, # Customize border weight
    color = "black"  # Customize border color
    # popup = paste0(subset(combinedData, dataset == "dwrCenterlines" | dataset == "sharedLevee")$LMA)
  ) %>%
  # Add intersection layer
  addPolygons(
    data = subset(combinedData, is.na(dataset)),
    group = "DLIS", # Assign a group for toggling
    fillColor = "orange", # Customize fill color
    fillOpacity = 0.5, # Customize fill opacity
    weight = 1, # Customize border weight
    color = "black"  # Customize border color
  ) %>%
  # Add nld layer
  addPolygons(
    data = subset(combinedData, dataset == "NLD"),
    group = "NLD", # Assign a group for toggling
    fillColor = "khaki", # Customize fill color
    fillOpacity = 0.5, # Customize fill opacity
    weight = 1, # Customize border weight
    color = "black"  # Customize border color
  ) %>%
  # Add in the functional delta boundary
  addPolylines(
    data = shapefiles$functionalDelta %>% 
      mutate(dataset = "functionalDelta") %>% 
      st_transform(4326),
    weight = 6, color = "firebrick"
  ) %>% 
  # Add layer control to toggle layers
  addLayersControl(
    overlayGroups = c("Rule Three", "DLIS", "NLD"), # Names of groups to toggle
    options = layersControlOptions(collapsed = FALSE) # Keep the control panel open
  )

# Starting from the top, and going clockwise
# Only use already created polygons from the NLD. Will intersect with the
# perimeter when required.

perimeter <- list()

# RD 1600, 0827, 0785, and 0537 - SacYolo North
perimeter$`RD 1600, 0827, 0785, and 0537 - SacYolo North` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 1600, 0827, 0785, and 0537 - SacYolo North") %>% 
  mutate(LMA = "RD 1600, 0827, 0785, and 0537 - SacYolo North")

# MA 09 - City of Sacramento - American R left bank
# This needs a lot of intersecting
# Intersect with the perimeter
# Difference with `Beach Lake - Morrison Creek`; only doing this one as there
# is a leveed area here from DLIS and DWR
perimeter$`MA 09 - City of Sacramento - American R left bank` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "MA 09 - City of Sacramento - American R left bank") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  # This difference isn't completely clean. The overlap between these areas aren't complete
  st_difference(filter(shapefiles$nld %>% 
                         filter(SYSTEM_NAME == "Beach Lake - Morrison Creek"))) %>% 
  mutate(LMA = "MA 09 - City of Sacramento - American R left bank")

perimeter$`Sacramento County Levee 119` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Sacramento County Levee 119") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Sacramento County Levee 119")

perimeter$`Sacramento County Levee 70` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Sacramento County Levee 70") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Sacramento County Levee 70")

perimeter$`Sacramento County Levee 32` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Sacramento County Levee 32") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  st_difference(sharedLevee$`New Hope Tract` %>% 
                  st_sf()) %>% 
  mutate(LMA = "Sacramento County Levee 32")

perimeter$`Grizzly Slough Area` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("Sacramento County Levee 64", "Sacramento County Levee 22",
                            "Sacramento County Levee 57")) %>% 
  bind_rows(shapefiles$nld %>% 
              filter(SYSTEM_NAME == "Sacramento County Levee 90") %>% 
              st_intersection(shapefiles$functionalDelta),
            shapefiles$nld %>% 
              filter(SYSTEM_NAME == "San Joaquin County Levee 37") %>% 
              st_intersection(shapefiles$functionalDelta)) %>% 
  mutate(LMA = "Grizzly Slough Area")

perimeter$`Bear Creek - Units 7 west and 21` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Bear Creek - Units 7 west and 21") %>%
  mutate(LMA = "Bear Creek - Units 7 west and 21")

perimeter$`Bear Creek - Units 7, 22, and 23` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Bear Creek - Units 7, 22, and 23") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Bear Creek - Units 7, 22, and 23")

perimeter$`Bear Creek - Units 8, 25, and 27` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Bear Creek - Units 8, 25, and 27") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  st_difference(polygons$leveesAll$`Atlas Tract`$polygon %>% 
                  st_sf()) %>% 
  mutate(LMA = "Bear Creek - Units 8, 25, and 27")
  
perimeter$`Mormon Slough - Calaveras R right bank - RD 2074` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Mormon Slough - Calaveras R right bank - RD 2074") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Mormon Slough - Calaveras R right bank - RD 2074")

perimeter$`Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek")

perimeter$`Littlejohn Creek right bank - Unit 2` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Littlejohn Creek right bank - Unit 2") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Littlejohn Creek right bank - Unit 2")

perimeter$`Littlejohn Creek left bank - Unit 1` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Littlejohn Creek left bank - Unit 1") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Littlejohn Creek left bank - Unit 1")

perimeter$`RD 0017, 2094, 2096, 2075, 2064 - SJ River East` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 0017, 2094, 2096, 2075, 2064 - SJ River East") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "RD 0017, 2094, 2096, 2075, 2064 - SJ River East")

perimeter$`Blewatt District` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 2101 - Blewatt District") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "RD 2101 - Blewatt District")

# Pico-Naglee
perimeter$`Pico-Naglee` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("San Joaquin County Levee 81")) %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Pico-Naglee")

# Pescadero District + Paradise Junction
perimeter$`Pescadero District and Paradise Junction` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("RD 2058 and RD 2095 - Paradise Cut")) %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Pescadero District and Paradise Junction")

perimeter$`Deuel levee` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Deuel levee") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Deuel levee")

# Kasson District
perimeter$`Kasson District` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME %in% c("RD 2085 - Kasson District")) %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Kasson District")

# Rio Vista, use NLD; much smaller area than in the RAND though
perimeter$`Rio Vista` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Mellin Levee - Rio Vista") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Rio Vista")

# Yolano and Cache Haas Area
# Overlaps with centerline, use NLD fully and remove from centerline
perimeter$`Yolano-Cache Slough` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "RD 2068 and RD 2098 - Yolano-Cache Slough") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Yolano-Cache Slough")

perimeter$`Putah Creek right bank - Unit 2` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Putah Creek right bank - Unit 2") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Putah Creek right bank - Unit 2")

# Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2
perimeter$`Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2")

# Cache Creek - RD 2035 - Willow Bypass
perimeter$`Cache Creek - RD 2035 - Willow Bypass` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Cache Creek - RD 2035 - Willow Bypass") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Cache Creek - RD 2035 - Willow Bypass")
  
# Yolo Bypass West Levee - Cache Creek Unit 4
perimeter$`Yolo Bypass West Levee - Cache Creek Unit 4` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Yolo Bypass West Levee - Cache Creek Unit 4") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Yolo Bypass West Levee - Cache Creek Unit 4")

# Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1
perimeter$`Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1")

# Knights Landing U2 - Yolo Bypass - Service Area 6
perimeter$`Knights Landing U2 - Yolo Bypass - Service Area 6` <- shapefiles$nld %>% 
  filter(SYSTEM_NAME == "Knights Landing U2 - Yolo Bypass - Service Area 6") %>% 
  st_intersection(shapefiles$functionalDelta) %>% 
  mutate(LMA = "Knights Landing U2 - Yolo Bypass - Service Area 6")

  # source(file.path("scripts", "leveeCorrectionScripts", "boundaryPolygons.R"))

polygons$ruleFour <- bind_rows(
  polygons$ruleThree,
  bind_rows(perimeter) %>% 
    mutate(dataset = "perimeter - NLD")
)

# polygons$ruleFour %>% 
#   filter(LMA %in% c(
#     "Grand Island",
#     "Bear Creek - Units 7 west and 21",
#     "Dutch Slough",
#     "Glanville Tract",
#     "Hastings Tract",
#     "Kasson District",
#     "Libby McNeil",
#     "Lower Roberts Island",
#     "Prospect Island")) %>% 
#   ggplot() +
#   geom_sf(fill = "blue", alpha = 0.5) +
#   facet_wrap(~dataset)

ggplot() +
  geom_sf(data = polygons$ruleFour,
          aes(fill = "NCEAS"), alpha = 0.2) +
  # geom_sf(data = shapefiles$DLISLevees,
  #         aes(fill = "DLIS"), alpha = 0.5) +
  geom_sf(data = shapefiles$functionalDelta %>% 
            st_cast("MULTILINESTRING"),
          aes(color = "Tidal Perimeter")) +
  scale_fill_manual(values = c("NCEAS" = "#0099FF80",
                              "DLIS" = "#FF990080")) +
  scale_color_manual(values = c("Tidal Perimeter" = "red")) +
  labs(color = element_blank(), fill = "Dataset")

# Finding overlapping areas -----------------------------------------------

# Are there shape files that overlap fully with one another? Trying to find
# duplicated areas
overlappedAreas <- function() {
  
  # # This is a brute force method that I tried first; use st_overlap instead
  # # Get the number of features (polygons) in the shapefile
  # numPolygons <- nrow(polygons$ruleFour)
  # 
  # # Create a logical matrix to store overlap results (TRUE for overlap, FALSE otherwise)
  # overlapMatrix <- matrix(NA, nrow = numPolygons, ncol = numPolygons)
  # 
  # # Iterate through each pair of polygons
  # for (i in 1:numPolygons) {
  #   for (j in 1:numPolygons) {
  #     # Check if the polygons intersect
  #     intersection <- st_intersects(polygons$ruleFour[i, ], polygons$ruleFour[j, ], sparse = FALSE)
  #     
  #     # Update the overlap matrix if they intersect
  #     overlapMatrix[i, j] <- overlapMatrix[j, i] <- intersection
  #   }
  # }
  # 
  # # Convert the matrix to a data frame for better readability (optional)
  # overlapDataFrame <- as.data.frame(overlapMatrix)
  # colnames(overlapDataFrame) <- polygons$ruleFour$LMA 
  # overlapDataFrame <- data.frame(LMA = polygons$ruleFour$LMA,
  #                                overlapDataFrame)
  # 
  # # Finding overlaps
  # overlaps <- data.frame(LMA = polygons$ruleFour$LMA,
  #                        overlapTimes = rowSums(overlapDataFrame[, -1]))
  
  shapefile <- polygons$ruleFour

  overlapDataFrame <- st_overlaps(shapefile, sparse = F) %>% 
    data.frame(shapefile$LMA,
               .) %>% 
    setNames(c("LMA", shapefile$LMA)) 
  
  overlaps <- data.frame(LMA = shapefile$LMA,
                          overlapTimes = rowSums(overlapDataFrame[, -1]))
  
  # Overlap to what?
  overlappedLMA <- subset(overlaps, overlapTimes > 0 & LMA != "Suisun Marsh")$LMA
  
  # Plotting the overlaps with some quick calculations to gauge if the overlap is substantial
  overlapped <- lapply(overlappedLMA, function(x) {
    
    data <- data.frame(LMA = x,
                       overlappedLMA = polygons$ruleFour$LMA[which(subset(overlapDataFrame, LMA == x) == TRUE) - 1])
    
    p <- polygons$ruleFour %>% 
      filter(LMA %in% c(x, data$overlappedLMA)) %>% 
      ggplot() +
      geom_sf(aes(fill = LMA), alpha = 0.3) +
      theme_void()
    
    overlapMatrix <- matrix(NA, nrow = nrow(data) - 1, ncol = 2)
    
    pOverlapped <- st_intersection(filter(polygons$ruleFour, LMA == x),
                                   filter(polygons$ruleFour, LMA %in% data$overlappedLMA, LMA != x) %>% 
                                     st_union()) %>% 
      {
        areaInterest <- st_area(filter(polygons$ruleFour, LMA == x))
        areaOverlapped <- st_area(.)
        
        ggplot(.) +
          geom_sf() +
          labs(title = x,
               subtitle = paste0("Overlapped = ", round(areaOverlapped, 2), " m^2\n", "Total = ", 
                                 round(areaInterest, 2), " m^2\n", "Percentage: ", round(areaOverlapped/areaInterest, 2), "%")) +
          theme_bw(base_size = 18)
      }
    p * pOverlapped
  }) %>% 
    setNames(overlappedLMA)
}
overlappedAreas <- overlappedAreas()

# total area...use as a filter to remove weird polygons, e.g.,
# Area adjacent to Sycamore Slough

# All overlapping areas are OK. Was looking for eggregious overlaps and did not
# find any. There are going to be areas that overlap slightly simply due to
# the different resolutions between the DWR and USCE (and within the USCE) 
# shapefiles.
stop()
# Saving the shapefile ----------------------------------------------------

polygons$ruleFour %>% 
  mutate(ruleset = case_when(
    dataset == "dwrCenterlines" ~ "1",
    dataset == "sharedLevee" ~ "1b",
    dataset == "suisunMarsh" ~ "2",
    dataset == "nld" ~ "3",
    dataset == "perimeter - NLD" ~ "4"
  ), .after = dataset) %>% 
  {
    # Checking duplicates. Should only be Suisun Marsh, Grizzly Slough Area, and
    # Dutch Slough. Suisun Marsh: all areas are called as one; Grizzly Slough
    # Area contains 5 leveed areas from the NLD within that area; Dutch Slough
    # contains 2 leveed areas from the NLD. Any other should be checked
  duplicatedLMAs <- janitor::get_dupes(., "LMA")
  if (!all(c("Suisun Marsh", "Grizzly Slough Area", "Dutch Slough") %in% unique(duplicatedLMAs$LMA)))
    stop("Check your duplicated LMAs.", call. = F)
  # If successful, continue on
  .
} %>% 
  split(.$LMA) %>% 
  {
  st_write(
    lapply(., function(x) {
      # Cannot save "geometrycollection" as a shapefile. This object is exclusive to
      # the sf package.
      
      if (!is.null(x$geometry)) {
        if (any(st_geometry_type(x$geometry) == "GEOMETRYCOLLECTION")) {
          data.frame(LMA = x$LMA,
                     dataset = x$dataset,
                     ruleset = x$ruleset,
                     x$geometry %>% 
                       .[[1]] %>% 
                       st_multipolygon() %>% 
                       st_sfc(crs = 3310))
        } else {
          if (all(st_geometry_type(x$geometry) %in% c("MULTIPOLYGON", "POLYGON"))) {
            data.frame(LMA = x$LMA,
                       dataset = x$dataset,
                       ruleset = x$ruleset,
                       x$geometry)
          } else {
            browser()
            # # Check here, you need the polygons in either "POLYGON" or "MULTIPOLYGON"
            # # format to save as a shapefile.
            # data.frame(LMA = x$LMA,
            #            dataset = x$dataset,
            #            ruleset = x$ruleset,
            #            x$geometry %>%
            #              st_cast("MULTIPOLYGON") %>%
            #              .[[1]] %>%
            #              st_multipolygon() %>%
            #              st_sfc(crs = 3310))
          }
        }
      } else {
        data.frame(LMA = x$LMA,
                   dataset = x$dataset,
                   ruleset = x$ruleset,
                   st_sfc(st_multipolygon(list()), crs = 3310))
      }
    }) %>% 
      bind_rows() %>% 
      mutate(geometry = st_make_valid(geometry),
             # Calculate area before saving the polygons
             area = st_area(geometry) %>% 
               units::set_units(acre) %>% 
               as.numeric()), 
    file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp"), 
    append = F
  )
  }

# Saving the plot of this
overallBoundingBox <- c(
  xmin = min(st_bbox(shapefiles$DLISLevees)["xmin"], st_bbox(polygons$ruleFour)["xmin"]),
  ymin = min(st_bbox(shapefiles$DLISLevees)["ymin"], st_bbox(polygons$ruleFour)["ymin"]),
  xmax = max(st_bbox(shapefiles$DLISLevees)["xmax"], st_bbox(polygons$ruleFour)["xmax"]),
  ymax = max(st_bbox(shapefiles$DLISLevees)["ymax"], st_bbox(polygons$ruleFour)["ymax"])
)

fixedVsRand <- {
  ggplot(polygons$ruleFour) +
    geom_sf(aes(fill = LMA)) +
    geom_sf(data = shapefiles$functionalDelta %>% 
              st_cast("MULTILINESTRING"), color = "red", size = 5) +
    scale_fill_viridis_d(option = "H") +
    coord_sf(xlim = c(overallBoundingBox["xmin"], overallBoundingBox["xmax"]),
             ylim = c(overallBoundingBox["ymin"], overallBoundingBox["ymax"])) +
    theme_bw(base_size = 18) +
    theme(legend.position = "none") +
    labs(title = "NCEAS")
} *
  {
    ggplot(shapefiles$DLISLevees) +
      geom_sf(aes(fill = NAME)) +
      geom_sf(data = shapefiles$functionalDelta %>% 
                st_cast("MULTILINESTRING"), color = "red", size = 5) +
      scale_fill_viridis_d(option = "H") +
      coord_sf(xlim = c(overallBoundingBox["xmin"], overallBoundingBox["xmax"]),
               ylim = c(overallBoundingBox["ymin"], overallBoundingBox["ymax"])) +
      theme_bw(base_size = 18) +
      theme(legend.position = "none") +
      labs(title = "DLIS")
  }

library(Cairo)
CairoPNG(file = file.path("figures", "fixedVsRAND.png"), 
         width = 15, height = 10, units = "in", res = 300)
fixedVsRand
dev.off()  

