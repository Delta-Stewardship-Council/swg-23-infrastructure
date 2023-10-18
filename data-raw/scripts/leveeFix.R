# This explores the levee centerlines
source(file.path("R", "createLeveePolygons.R"))

#### IMPORTANT ####
# Make sure you run through at least these two sections in "ecologicalAssets.R":
# Section "Data lists"
# Section "delta levees"

# failedPolygons <- area$leveesDF %>% 
#   filter(acres == 0, acresDLIS > 0)

# Is the area calculation the same? ---------------------------------------

# using the DLIS file, check to see if the calculation in R is the same as
# in GIS

shapefiles$DLISLevees %>% 
  mutate(areaCheck = shapefiles$DLISLevees %>% 
           st_area() %>% 
           as.numeric(),
         areaCheckAcres = shapefiles$DLISLevees %>% 
           st_area() %>%
           units::set_units(acres) %>% 
           as.numeric(),
         areaDifference = areaCheck - Shape_Area) %>% 
  {
    all.equal(.$Shape_Area, .$areaCheck)
  }

# DLIS-08 (DISCOVERY BAY AREA) is vastly different
# When converted to acres, this area is 2633.720251 acres, which is essentially the
# same as on the Tableau website: 2632 acres. I think there seems to be some
# legacy area calculation in this shapefile.

needManualFix <- NULL
failedLevees <- NULL
inDLISnotLevees <- NULL
notNeeded <- NULL
inconsistentName <- list()
snappedFixed <- list()
sharedLeveeFixed <- list()
drawnDLIS <- list()

# ehrheardt club ----------------------------------------------------------

# compareIslandsTotal("ehrheardt club")

# Levee 195 has a break in it
needManualFix <- c(needManualFix, "ehrheardt club")

# new hope tract ----------------------------------------------------------

# compareIslandsTotal("new hope tract")

# shapefiles$levees %>% 
#   filter(LMA == "New Hope Tract") %>% 
#   st_distance()
# # Potential gap between 287 and 289

snappedFixed$`New Hope Tract` <- snapCenterlines(
  shapefiles$levees %>% 
    filter(LMA == "New Hope Tract"), tolerance = 15
)

# tyler island ------------------------------------------------------------

# compareIslandsTotal("tyler island")

# Will have to find the shared levees manually. I looked on the the geoportal page
sharedLeveeFixed$`Tyler Island` <- sharedLevees(
  "Tyler Island", c(414, 415, 417)
)

# brack tract -------------------------------------------------------------

# compareIslandsTotal("brack tract")

# No additional levee to connect to. May need to use the legal delta border
needManualFix <- c(needManualFix, "brack tract")

# honker lake tract -------------------------------------------------------

# compareIslandsTotal("honker lake tract")

# Shares levees with several other levee objects: 185, 278, 255, 26
# However, DLIS appears to have made a custom line going along S Holt Rd
needManualFix <- c(needManualFix, "honker lake tract")

# lower jones tract -------------------------------------------------------

# compareIslandsTotal("JONES TRACT")
# plotLeveeLines("lower jones tract")

# filter(shapefiles$levees,
#        LMA %in% c("Lower Jones Tract", "Upper Jones Tract")) %>% 
#   ggplot() +
#   geom_sf(aes(color = factor(OBJECTID)))

# 225 includes extra areas. Need to limit bounding box in order to get only Jones Tract
sharedLeveeFixed$`Jones Tract` <- st_intersection(
  sharedLevees("lower jones tract",
               c(filter(shapefiles$levees, LMA == "Upper Jones Tract") %>%
                   pull(OBJECTID),
                 255)),
  st_as_sfc(st_bbox(snapCenterlines(filter(shapefiles$levees,
                                     LMA %in% c("Lower Jones Tract", "Upper Jones Tract")),
                              10), crs = 3310))
) 

# pearson district --------------------------------------------------------

# compareIslandsTotal("pearson district")

# DLIS manually created this layer using River Rd as the top connecting layer

needManualFix <- c(needManualFix, "pearson district")

# randall island ----------------------------------------------------------

# compareIslandsTotal("randall island")

# DLIS manually created this layer using River Rd as the bottom connecting layer

needManualFix <- c(needManualFix, "randall island")

# libby mcneil ------------------------------------------------------------

# compareIslandsTotal("libby mcneil")

# DLIS includes the upper part of Walnut Grove as part of this. Also includes
# the marshy areas to the NE of the area. Private levee?

# {shapefiles$DLISLevees %>%
#     filter(NAME == "LIBBY MCNEIL") %>%
#     ggplot() +
#     geom_sf(fill = "firebrick")} *
#   {sharedLevees("Libby McNeil", c(300, 421, 422, 11, 418, 420)) %>%
#       ggplot() +
#       geom_sf(fill = "khaki")}

needManualFix <- c(needManualFix, "Libby McNeil")

# union island east -------------------------------------------------------

# compareIslandsTotal("union island east")

# DLIS includes only the eastern part of this island, separated by Tracy Blvd
needManualFix <- c(needManualFix, "union island east")

# union island west -------------------------------------------------------

# compareIslandsTotal("union island west")

# DLIS includes only the western part of this island, separated by Tracy Blvd
needManualFix <- c(needManualFix, "union island west")

# # Could theoretically combine the two island into one large union island
# shapefiles$levees %>% 
#   filter(LMA %in% c("Union Island East", "Union Island West") |
#            OBJECTID %in% c(370, 40)) %>% 
#   st_union() %>% 
#   st_polygonize() %>% 
#   ggplot() +
#   geom_sf(fill = "firebrick")

# byron tract -------------------------------------------------------------

# compareIslandsTotal("byron tract")

# DLIS contains parts of Discovery Bay. Large gaps in west and SW

needManualFix <- c(needManualFix, "byron tract")

# middle roberts island & upper roberts island ----------------------------

# # DLIS combines both middle and upper roberts together
# # compareIslandsTotal("middle & upper roberts island")
# plotLeveeLines(c("middle roberts island", "upper roberts island"))

# Levee 255 needed to connect the polygon. Could snap the lines, but better
# to use the actual levee when possible. Need to cut the line though.

sharedLeveeFixed$`Roberts Island` <- shapefiles$levees %>% 
  filter(LMA %in% c("Middle Roberts Island", "Upper Roberts Island") |
           OBJECTID %in% c(256)) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(OBJECTID == 255) %>% 
              st_cast("LINESTRING") %>% 
              # Plot this if you're curious if this is the right line or not
              .[10, ] %>% 
              lwgeom::st_linesubstring(0, 0.1)) %>% 
  st_union() %>% 
  st_polygonize()

# netherlands -------------------------------------------------------------

# compareIslandsTotal("netherlands")
# DLIS includes a northern section that splits into Glide District. Can't 
# reproduce that here since there is no linestring there. 

# # This does not include the northern section that DLIS has
# sharedLevees("netherlands", 
#              249) %>% 
#   ggplot() +
#   geom_sf(fill = "firebrick")

needManualFix <- c(needManualFix, "netherlands")

# glide district ----------------------------------------------------------

# compareIslandsTotal("glide district")

# # netherland line splits this area in DLIS. Don't have this polygon
# {shapefiles$DLISLevees %>% 
#     filter(NAME == "GLIDE DISTRICT") %>% 
#     ggplot() +
#     geom_sf(fill = "khaki")} *
#   {shapefiles$levees %>% 
#       filter(LMA == "Glide District" |
#                OBJECTID %in% c(282, 249)) %>% 
#       bind_rows(shapefiles$levees %>% 
#                   filter(OBJECTID == 55) %>% 
#                   st_cast("LINESTRING") %>% 
#                   lwgeom::st_linesubstring(0, 0.1)) %>% 
#       bind_rows(st_snap(shapefiles$levees %>% 
#                           filter(OBJECTID == 283),
#                         shapefiles$levees %>% 
#                           filter(OBJECTID == 435), 30)) %>% 
#       st_union() %>% 
#       st_polygonize() %>% 
#       ggplot() +
#       geom_sf(fill = "firebrick")}

needManualFix <- c(needManualFix, "glide district")

# bixler tract ------------------------------------------------------------

# compareIslandsTotal("bixler tract")

# Not an enclosed area. Did DLIS use the Delta boundary?

needManualFix <- c(needManualFix, "bixler tract")

# veale tract -------------------------------------------------------------

# compareIslandsTotal("veale tract")

# Not an enclosed area. Did DLIS use the Delta boundary?

needManualFix <- c(needManualFix, "veale tract")

# hotchkiss tract ---------------------------------------------------------

# compareIslandsTotal("hotchkiss tract")

# Break in levee 234. Cannot snap together with current approaches

needManualFix <- c(needManualFix, "hotchkiss tract")

# stewart tract -----------------------------------------------------------

# compareIslandsTotal("stewart tract")

# DLIS uses part of the inner Stewart Tract shape to connect the polygon.
# Can likely draw a line continuing Steward Rd along the Union Pacific rail

needManualFix <- c(needManualFix, "stewart tract")

# shin kee tract ----------------------------------------------------------

# compareIslandsTotal("shin kee tract")

# Shares with levees 43 and 380. There is a gap between 380 and 358 that st_snap
# can't seem to correct. Might be due to the intersection point being in the
# middle of levee 380 instead of at the endpoints

needManualFix <- c(needManualFix, "shin kee tract")

# rio blanco tract --------------------------------------------------------

# compareIslandsTotal("rio blanco tract")

# DLIS only includes the western region of this tract. The eastern portion, 
# OBJECT ID 327 is used in polygon DLIS-15

# # st_snap does not work between 73 and 323 sadly. 
# shapefiles$levees %>% 
#   filter(OBJECTID %in% c(323, 325, 326, 73)) %>% 
#   st_touches()

# Draw from the nearest points between the two linestrings, then polygonize
snappedFixed$`Rio Blanco Tract` <- st_nearest_points(
  shapefiles$levees %>% 
    filter(OBJECTID == 323) %>% 
    st_boundary() %>%
    st_cast("POINT") %>% 
    .[1, ],
  shapefiles$levees %>% 
    filter(OBJECTID == 73) %>% 
    st_cast("MULTIPOINT")
) %>% 
  st_sf() %>% 
  bind_rows(shapefiles$levees %>% 
              filter(OBJECTID %in% c(323, 325, 326, 73))) %>% 
  st_union() %>% 
  st_polygonize()

# wright-elmwood tract ----------------------------------------------------

# compareIslandsTotal("wright-elmwood tract")

sharedLeveeFixed$`Wright-Elmwood Tract` <- sharedLevees("wright-elmwood tract", c(341, 342))

# smith tract (lincoln village) -------------------------------------------

# compareIslandsTotal("smith tract (lincoln village)")

# DLIS calls this area "North Stockton" and is lumped with other levees:
# Need to decide the eastern extent of this area

# boggs tract -------------------------------------------------------------

# compareIslandsTotal("boggs tract")

# DLIS calls this area "Central Stockton" and is lumped with other levees:
# 367, 366, 365, 364, 361, 141, 142, 90, 143, 144. The middle "island" here
# as shown in DLIS do not have levees accordingly to the DWR centerlines.
# Again, need to decide the eastern extent of this area

# wetherbee lake ----------------------------------------------------------

# compareIslandsTotal("wetherbee lake")

# Made two manual connections here. The western cut is different than the one
# from DLIS. I went diagonally to the start of the levee centerline while 
# DLIS made a more horizontal draw

snappedFixed$`Wetherbee Lake` <- st_linestring(as.matrix(rbind(shapefiles$levees %>% 
                                                                 filter(OBJECTID == 319) %>% 
                                                                 st_boundary() %>% 
                                                                 st_cast("POINT") %>% 
                                                                 .[2, ] %>% 
                                                                 st_coordinates(),
                                                               shapefiles$levees %>% 
                                                                 filter(OBJECTID == 92) %>% 
                                                                 st_boundary() %>% 
                                                                 st_cast("POINT") %>% 
                                                                 .[4, ] %>% 
                                                                 st_coordinates()))) %>% 
  st_sfc(crs = 3310) %>% 
  st_union(
    st_linestring(as.matrix(rbind(shapefiles$levees %>% 
                                    filter(OBJECTID == 92) %>% 
                                    st_boundary() %>% 
                                    st_cast("POINT") %>% 
                                    .[1, ] %>% 
                                    st_coordinates(),
                                  shapefiles$levees %>% 
                                    filter(OBJECTID == 440) %>% 
                                    st_boundary() %>% 
                                    st_cast("POINT") %>% 
                                    .[1, ] %>% 
                                    st_coordinates()))) %>% 
      st_sfc(crs = 3310)
  ) %>% 
  st_sf() %>% 
  bind_rows(shapefiles$levees %>% 
              filter(OBJECTID %in% c(91, 319, 92, 440))) %>% 
  st_union() %>% 
  st_polygonize()

# walthall ----------------------------------------------------------------

# compareIslandsTotal("walthall")

# DLIS appears to cut horizontally across to connect 318 with the corner of 92
# Eastern border not defined.

needManualFix <- c(needManualFix, "walthall")

# mcmullin ranch ----------------------------------------------------------

# compareIslandsTotal("mcmullin ranch")

# DLIS does not seem to follow 270 when cutting to the east. The northern cut
# also does not seem to follow the levee centerline for 423

needManualFix <- c(needManualFix, "mcmullin ranch")

# river junction ----------------------------------------------------------

# compareIslandsTotal("river junction")

# South of McMullin Ranch, sharing the border to the north. DLIS had a custom
# border there which dictates this northern border as well

needManualFix <- c(needManualFix, "river junction")

# pico-naglee -------------------------------------------------------------

# compareIslandsTotal("pico-naglee")

# Southern border needs to be defined

needManualFix <- c(needManualFix, "pico-naglee")

# paradise junction -------------------------------------------------------

# compareIslandsTotal("paradise junction")

# Western border needs to be defined

needManualFix <- c(needManualFix, "paradise junction")

# egbert tract ------------------------------------------------------------

# compareIslandsTotal("egbert tract")

# Western border needs to be defined

needManualFix <- c(needManualFix, "egbert tract")

# little egbert tract -----------------------------------------------------

compareIslandsTotal("little egbert tract")

inconsistentName$`Little Egbert Island` <- "Little Egbbert Tract"

sharedLeveeFixed$`Little Egbert Island` <- sharedLevees("little egbert island", 193)

# liberty island ----------------------------------------------------------

# compareIslandsTotal("liberty island")

# This is now a flooded island. How are we dealing with this? Levee has already
# failed?

failedLevees <- c(failedLevees, "liberty island")

# dutch slough ------------------------------------------------------------

# compareIslandsTotal("dutch slough")

# Bottom border needs to be defined

needManualFix <- c(needManualFix, "dutch slough")

# weber tract -------------------------------------------------------------

# compareIslandsTotal("weber tract")

# Part of "Central Stockton" in DLIS

needManualFix <- c(needManualFix, "weber")

# yolano ------------------------------------------------------------------

# compareIslandsTotal("yolano")

# DLIS island is shaped dramatically different from the levee centerlines

needManualFix <- c(needManualFix, "yolano")

# drexler pocket ----------------------------------------------------------

# compareIslandsTotal("drexler pocket")

# Western boundary drawn from end point of 182 to corner of Kingston School Rd

needManualFix <- c(needManualFix, "drexler pocket")

# smith tract -------------------------------------------------------------

# compareIslandsTotal("smith tract")

# Top portion of "Central Stockton" polygon in DLIS

needManualFix <- c(needManualFix, "smith tract")

# little mandeville island ------------------------------------------------

# compareIslandsTotal("little mandeville island")

# This is a flooded island. How are we dealing with this? Levee has already
# failed?

failedLevees <- c(failedLevees, "little mandeville island")

# mildred island ----------------------------------------------------------

# compareIslandsTotal("mildred island")

# This is a flooded island. How are we dealing with this? Levee has already
# failed?

failedLevees <- c(failedLevees, "mildred island")

# winter island -----------------------------------------------------------

# compareIslandsTotal("winter island")

# Somewhat flooded island. However, since not fully flooded, will simply enclose

snappedFixed$`Winter Island` <- shapefiles$levees %>% 
  filter(LMA == "Winter Island") %>% 
  st_boundary() %>% 
  st_cast("POINT") %>% 
  {
    st_linestring(
      as.matrix(rbind(.[1, ] %>% 
                        st_coordinates(),
                      .[2, ] %>% 
                        st_coordinates()))
    ) %>% 
      st_sfc(crs = 3310)
  } %>% 
  st_sf() %>% 
  bind_rows(shapefiles$levees %>% 
              filter(LMA == "Winter Island")) %>% 
  st_union() %>% 
  st_polygonize()

# In levees but not DLIS --------------------------------------------------
# Use tableau to find the island of interest and record name in DLIS

# orwood and palm tract ---------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Orwood and Palm Tract", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Named PALM-ORWOOD in DLIS
inconsistentName$`Orwood and Palm Tract` <- "PALM-ORWOOD"

# canal ranch -------------------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Canal Ranch", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

inconsistentName$`Canal Ranch` <- "CANAL RANCH TRACT"

# van sickle island -------------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Van Sickle Island", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Part of DLIS-63 (GRIZZLY ISLAND AREA)
inDLISnotLevees$`Van Sickle Island` <- "DLIS-63 (GRIZZLY ISLAND AREA)"

# grizzly slough ----------------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Grizzly Slough", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# DLIS expands this area. Includes: 83, 84, 453. Eastern border needs to be
# drawn

needManualFix <- c(needManualFix, "grizzly slough")

# glanville tract ---------------------------------------------------------

# shapefiles$levees %>%
#   mutate(LMA = ifelse(LMA == "Glanville Tract", LMA, NA)) %>%
#   ggplot() +
#   geom_sf(aes(color = LMA))
# 
# shapefiles$levees %>% 
#   filter(LMA == "Glanville Tract") %>% 
#   st_touches()

# Need to connect 8 with 209
# Cannot snap it
snappedFixed$`Glanville Tract` <- drawLine(shapefiles$levees %>% 
           filter(OBJECTID == 8),
         shapefiles$levees %>% 
           filter(OBJECTID == 209),
         2, 2) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(LMA == "Glanville Tract")) %>%
  st_union() %>% 
  st_polygonize()

inconsistentName$`Glanville Tract` <- "GLANVILLE"

# area adjacent to the sacramento river deep water ship channel -----------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Area adjacent to the Sacramento River Deep Water Ship Channel", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Part of DLIS-20 (YOLO BYPASS)
needManualFix <- c(needManualFix, "area adjacent to the sacramento river deep water ship channel")

# stewart tract (inner) ---------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Stewart Tract (inner)", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Inner is disregarded by DLIS. Joined with entire Steward Tract
notNeeded <- c(notNeeded, "steward tract (inner)")

# summer lake subdivision -------------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Summer Lake Subdivision", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Encompassed by Hotchkiss Tract
notNeeded <- c(notNeeded, "summer lake subdivision")

# area northwest of discovery bay -----------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Area northwest of Discovery Bay", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Not used in DLIS. DLIS also creates their own Discovery Bay area:
# DLIS-08 (DISCOVERY BAY AREA)
notNeeded <- c(notNeeded, "Area northwest of Discovery Bay")

# area east of shin kee tract ---------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Area east of Shin Kee Tract", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# DLIS splits this area into DLIS-17 and DLIS-16 (LODI)
needManualFix <- c(needManualFix, "area east of shin kee tract")

# area east of rio blanco tract -------------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Area east of Rio Blanco Tract", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Part of DLIS-15
needManualFix <- c(needManualFix, "Area east of Rio Blanco Tract")

# area adjacent to sycamore slough ----------------------------------------

# shapefiles$levees %>% 
#   mutate(LMA = ifelse(LMA == "Area adjacent to Sycamore Slough", LMA, NA)) %>% 
#   ggplot() +
#   geom_sf(aes(color = LMA))

# Part of DLIS-18
needManualFix <- c(needManualFix, "Area adjacent to Sycamore Slough")

# Putting everything together ---------------------------------------------
# Will need to be updated with the manual fixes. As of 9/11, this will not 
# include this.

# Fixing polygons that were registered but incorrectly --------------------

# west sacramento ---------------------------------------------------------

# compareIslands("west sacramento")

snappedFixed$`West Sacramento` <- drawLine(shapefiles$levees %>% 
                           filter(OBJECTID == 434),
                         shapefiles$levees %>% 
                           filter(OBJECTID == 435), 
                         1, 1) %>% 
  bind_rows(
    shapefiles$levees %>% 
      filter(LMA %in% "West Sacramento" |
               OBJECTID %in% c(215, 54))
  ) %>% 
  st_union() %>% 
  st_polygonize()

# cache haas area ---------------------------------------------------------

# compareIslands("cache haas area")

# DLIS does include area south of the area, which is all water (flooded)
# It also extends north. Levees 167, 174 are a part of this northern area
# Need to manually connect from Swan Rd eastward.

needManualFix <- c(needManualFix, "Cache Haas Area")

# bethel island -----------------------------------------------------------

# compareIslands("bethel island")

# DLIS does include the Delta Cove. OK. Will be homes, so will include as well
snappedFixed$`Bethel Island` <- drawLine(shapefiles$levees %>% 
                                           filter(OBJECTID == 133),
                                         shapefiles$levees %>% 
                                           filter(OBJECTID == 137),
                                         2, 1) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(LMA == "Bethel Island")) %>% 
  st_union() %>% 
  st_polygonize()

# walnut grove ------------------------------------------------------------

compareIslands("walnut grove")

# Walnut grove in DLIS does not include the northern part of the island, separated
# by the DCC. That area is a part of Lisbon McNeil

needManualFix <- c(needManualFix, "walnut grove")

# prospect island ---------------------------------------------------------

# compareIslands("prospect island")

# Still slight difference from DLIS due to inclusion of tiny island at the bottom
# right
sharedLeveeFixed$`Prospect Island` <- sharedLevees("prospect island",
                                                   c(473, 474, 283, 58))

# clifton court forebay ---------------------------------------------------

# compareIslands("clifton court forebay")

# Need to manually do this. Includes levees 112 and 465 connected by Byron HWy to
# levee 485

needManualFix <- c(needManualFix, "clifton court forebay")

# holt station ------------------------------------------------------------

# compareIslands("holt station")

# Don't really know how DLIS did their polygon. Seems to run along S Holt Rd,
# but not 100% clean

# bind_rows(shapefiles$DLISLevees %>% 
#               filter(NAME == "HONKER LAKE TRACT") %>% 
#               mutate(dataset = "DLIS"),
#           shapefiles$levees %>% 
#             filter(LMA == "Honker Lake Tract" |
#                      OBJECTID %in% c(185, 278, 255, 26)) %>% 
#             mutate(dataset = "levees")) %>% 
#   ggplot() +
#   geom_sf(aes(fill = dataset))

needManualFix <- c(needManualFix, "holt station")

# kasson district ---------------------------------------------------------

compareIslandsTotal("kasson district")

# bind_rows(shapefiles$DLISLevees %>% 
#             filter(NAME == "KASSON DISTRICT") %>% 
#             mutate(dataset = "DLIS"),
#           shapefiles$levees %>% 
#             filter(LMA == "Kasson District") %>% 
#             mutate(dataset = "levees")) %>% 
#   st_geometry() %>% 
#   plot()

# points <- locator(type = "p")
points <- list(x = c(-112096.4, -113944.6, -114272.7, 114312.9),
               y = c(-37485.71, -35289.31, -34767.00, -34767.00))

# 96 and 240 are the ending points
pointsEnds <- drawLine(shapefiles$levees %>% 
                         filter(OBJECTID == 240),
                       shapefiles$levees %>% 
                         filter(OBJECTID == 96),
                       3, 1, returnPoints = T)


drawnDLIS$`Kasson District` <- data.frame(
  x = c(pointsEnds[1, "X"], points$x, pointsEnds[2, "X"]), 
  y = c(pointsEnds[1, "Y"], points$y, pointsEnds[2, "Y"])
) %>% 
  st_as_sf(coords = c("x", "y"), crs = 3310) %>% 
  summarise() %>% 
  st_cast("MULTILINESTRING") %>% 
  bind_rows(
    shapefiles$levees %>% 
      filter(LMA == "Kasson District") %>% 
      mutate(dataset = "levees")
  ) %>% 
  summarise() %>% 
  st_polygonize()

# Folding in the Delta Boundaries -----------------------------------------

# i03 is the delta boundary dataset from DWR
# For now, will use the tidally influenced one from Britne

shapefiles$deltaTidal <- st_read(file.path("data-raw", "shapefiles", "deltaBoundary", 
                                           "SacSJ_TidallyInfluencedBoundary", "Tidally_Influenced_Delta_SacSJ.shp")) %>% 
  st_transform(crs = 3310)

# shapefiles$deltaTidal %>% 
#   ggplot() +
#   geom_sf(fill = "firebrick")


# {bind_rows(shapefiles$deltaTidal %>%
#             mutate(dataset = "delta"),
#           polygonDF %>%
#             mutate(dataset = "levees")) %>%
#   ggplot() +
#   geom_sf(aes(color = dataset)) +
#     theme_minimal() +
#     theme(legend.position = "bottom")} *
#   {bind_rows(shapefiles$deltaTidal %>%
#                mutate(dataset = "delta"),
#              shapefiles$DLISLevees %>%
#                mutate(dataset = "levees")) %>%
#       ggplot() +
#       geom_sf(aes(color = dataset)) +
# theme_minimal() +
# theme(legend.position = "bottom")}


shapefiles$deltaLegal <- st_read(file.path("data-raw", "shapefiles", "deltaBoundary",
                                           "SacSJ_LegalBoundary", "legal_delta_SacSJ.shp")) %>% 
  st_transform(crs = 3310)

# {bind_rows(shapefiles$deltaLegal %>% 
#              mutate(dataset = "delta"),
#            polygonDF %>% 
#              mutate(dataset = "levees")) %>% 
#     ggplot() +
#     geom_sf(aes(color = dataset)) +
#     theme_minimal() +
#     theme(legend.position = "bottom")} * 
#   {bind_rows(shapefiles$DLISLevees %>% 
#                mutate(dataset = "levees"),
#              shapefiles$deltaLegal %>% 
#                mutate(dataset = "delta") %>% 
#                st_cast("MULTILINESTRING")) %>% 
#       ggplot() +
#       geom_sf(aes(color = dataset)) +
#       theme_minimal() +
#       theme(legend.position = "bottom")}

legalDeltaFix <- list()

# DLIS-19 Grizzly Slough Area ---------------------------------------------

# 209, 14, 469, 497, 83
# Bounding box approach does not work since the bounding box is SMALLER than
# the large delta boundary. This causes the lines from the bounding box to appear
# Only currently useful if the bounding box is LARGER than the area of interest

# shapefiles$levees %>% 
#   filter(OBJECTID %in% c(209, 14, 469, 497, 83)) %>% 
#   bind_rows(shapefiles$deltaLegal) %>% 
#   st_intersection(st_as_sfc(st_bbox(shapefiles$DLISLevees %>% 
#                                         filter(NAME == "DLIS-19 (GRIZZLY SLOUGH AREA)"),
#                                       crs = 3310))) %>% 
#   st_geometry() %>% 
#   plot()

# Manual draw between 497 and 469
# Manual draw between break in 83


# polygonDF %>% 
#   filter(st_is_empty(.)) %>% 
#   pull(LMA)

# # START HERE
# bind_rows(
#   shapefiles$DLISLevees %>%
#     filter(NAME == "DLIS-19 (GRIZZLY SLOUGH AREA)"),
#   shapefiles$levees %>% 
#     filter(OBJECTID %in% c(209, 14, 469, 497, 83))
# ) %>% 
#   st_geometry() %>% 
#   plot()
# 
# # points <- locator(type = "p")
# pointsA <- list(x = c(-126029.7, -125843.9, -125678.8),
#                 y = c(27632.85, 27560.59, 27457.37))
# 
# pointsB <- list(x = c(-121869.9, -121859.6, -121818.3),
#                 y = c(22967.25, 22843.38, 22678.23))
# 
# data.frame(
#   x = c(pointsA$x), 
#   y = c(pointsA$y)
# ) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 3310) %>% 
#   summarise() %>% 
#   st_cast("MULTILINESTRING") %>% 
#   bind_rows(
#     data.frame(
#       x = c(pointsB$x), 
#       y = c(pointsB$y)
#     ) %>% 
#       st_as_sf(coords = c("x", "y"), crs = 3310) %>% 
#       summarise() %>% 
#       st_cast("MULTILINESTRING"),
#     shapefiles$levees %>% 
#       filter(OBJECTID %in% c(8, 209, 14, 469, 497, 83)),
#     shapefiles$deltaLegal %>% 
#       st_cast("LINESTRING") %>% 
#       lwgeom::st_linesubstring(0.25, 0.3111)
#   ) %>% 
#   st_union() %>% 
#   st_polygonize() %>% 
#   ggplot() +
#   geom_sf(fill = "firebrick")
# # Not finished

# NORTH STOCKTON ----------------------------------------------------------

# shapefiles$levees %>% 
#   filter(OBJECTID %in% c(457, 78, 356, 353, 354, 368, 87, 369, 88, 452, 340, 
#                          341, 342, 343, 344, 345, 82, 89)) %>% 
#   bind_rows(shapefiles$deltaLegal %>% 
#               st_cast("LINESTRING"),
#             shapefiles$deltaLegal %>% 
#               st_cast("LINESTRING") %>% 
#               lwgeom::st_linesubstring(0.37, 0.4) %>% 
#               mutate(extract = T)) %>% 
#   ggplot() +
#   geom_sf(aes(color = extract))

# Need to draw line between 369 and 88 to close off
legalDeltaFix$`North Stockton` <- drawLine(shapefiles$levees %>% 
           filter(OBJECTID == 369),
         shapefiles$levees %>% 
           filter(OBJECTID == 88),
         2, 2) %>% 
  bind_rows(
    shapefiles$levees %>% 
      filter(OBJECTID %in% c(457, 78, 356, 353, 354, 368, 87, 369, 88, 452, 340, 
                             341, 342, 343, 344, 345, 82, 89)),
    shapefiles$deltaLegal %>% 
      st_cast("LINESTRING") %>% 
      lwgeom::st_linesubstring(0.37, 0.4)
  ) %>% 
  st_union() %>% 
  st_polygonize()

# RECLAMATION DISTRICT 17 -------------------------------------------------

# plotLeveeLines("Reclamation District No. 17")
# shapefiles$levees %>% 
#   filter(LMA == "Reclamation District No. 17") %>% 
#   bind_rows(shapefiles$deltaLegal %>% 
#               st_cast("LINESTRING"),
#             shapefiles$deltaLegal %>% 
#               st_cast("LINESTRING") %>% 
#               lwgeom::st_linesubstring(0.4, .47) %>% 
#               mutate(extract = T)) %>% 
#   ggplot() +
#   geom_sf(aes(color = extract))

legalDeltaFix$`Reclamation District No. 17` <- shapefiles$levees %>% 
  filter(LMA == "Reclamation District No. 17") %>% 
  bind_rows(
    shapefiles$deltaLegal %>% 
      st_cast("LINESTRING") %>% 
      lwgeom::st_linesubstring(0.4, .47)
  ) %>% 
  st_union() %>% 
  st_polygonize()

# Final product -----------------------------------------------------------

# Which polygons were successful?
polygons <- lapply(area$levees, function(x) {
  if (is.null(x$p)) {
    data.frame(LMA = x$df$LMA)
  } else {
    data.frame(LMA = x$df$LMA,
               x$polygon)
  }
})

# Of the unsuccessful polygons, update the ones that were manually fixed:
# in the order of: inconsistentName, sharedLeveeFixed, snappedFixed, legalDeltaFixed

# inconsistentName
polygons$`Orwood and Palm Tract` <- data.frame(LMA = "Orwood and Palm Tract",
                                               area$levees$`Orwood and Palm Tract`$polygon)
polygons$`Canal Ranch` <- data.frame(LMA = "Canal Ranch",
                                     area$levees$`Canal Ranch`$polygon)
polygons$`Little Egbert Island` <- data.frame(LMA = "Little Egbert Island",
                                              sharedLeveeFixed$`Little Egbert Island`)

names(sharedLeveeFixed)
polygons$`Tyler Island`$geometry <- sharedLeveeFixed$`Tyler Island`
polygons$`Jones Tract` <- data.frame(LMA = "Jones Tract",
                                     sharedLeveeFixed$`Jones Tract`)
# Removes the two smaller Jones Tracts
polygons$`Roberts Island` <- data.frame(LMA = "Roberts Island",
                                        sharedLeveeFixed$`Roberts Island`)
# Removes Middle and Upper Roberts Islands
polygons$`Wright-Elmwood Tract`$geometry <- sharedLeveeFixed$`Wright-Elmwood Tract`
polygons$`Prospect Island`$geometry <- sharedLeveeFixed$`Prospect Island`

# names(snappedFixed)
polygons$`Glanville Tract` <- data.frame(LMA = "Glanville Tract",
                                         snappedFixed$`Glanville Tract`)
polygons$`New Hope Tract`$geometry <- snappedFixed$`New Hope Tract`
polygons$`Rio Blanco Tract`$geometry <- snappedFixed$`Rio Blanco Tract`
polygons$`Wetherbee Lake`$geometry <- snappedFixed$`Wetherbee Lake`
polygons$`Winter Island`$geometry <- snappedFixed$`Winter Island`
polygons$`West Sacramento`$geometry <- snappedFixed$`West Sacramento`
polygons$`Bethel Island`$geometry <- snappedFixed$`Bethel Island`

# names(drawnDLIS)
polygons$`Kasson District`$geometry <- drawnDLIS$`Kasson District`$geometry

# names(legalDeltaFix)
polygons$`North Stockton` <- data.frame(LMA = "North Stockton",
                                        legalDeltaFix$`North Stockton`)
polygons$`Reclamation District No. 17` <- data.frame(LMA = "Reclamation District No. 17",
                                                     legalDeltaFix$`Reclamation District No. 17`)

# Folding in Suisun Marsh area --------------------------------------------

polygons$`Suisun Marsh` <- st_read(file.path("data-raw", "shapefiles", "suisunMarsh", "i03_SuisunMarshBoundary.shp")) %>% 
  st_transform(crs = 3310) %>% 
  filter(LOCATION != "Primary (Water)") %>% 
  mutate(LMA = "Suisun Marsh") %>% 
  st_union() %>% 
  data.frame(LMA = "Suisun Marsh", .)

# Plotting everything
polygonDF <- bind_rows(polygons) %>% 
  st_sf()

# Saving the shape file ---------------------------------------------------
st_write(
  lapply(polygons, function(x) {
    
    if (!is.null(x$geometry)) {
      if (any(st_geometry_type(x$geometry) == "GEOMETRYCOLLECTION")) {
        data.frame(LMA = x$LMA,
                   x$geometry %>% 
                     .[[1]] %>% 
                     st_multipolygon() %>% 
                     st_sfc(crs = 3310))
      } else {
        data.frame(LMA = x$LMA,
                   x$geometry %>% 
                     st_cast("MULTIPOLYGON") %>% 
                     .[[1]] %>% 
                     st_multipolygon() %>% 
                     st_sfc(crs = 3310))
      }
    } else {
      data.frame(LMA = x$LMA,
                 st_sfc(st_multipolygon(list()), crs = 3310))
    }
  }) %>% 
    bind_rows(), 
  file.path("data", "shapefiles", "fixedLevees", "fixedLevees.shp"), 
  append = F
)

# Saving RData
save.image(file.path("data", paste0("fixedLevees_", format(Sys.Date(), "%m%d%y"), ".RData")))

stop()

finalFigures <- list(
  # Fixed levee polygons vs RAND model
  {polygonDF %>% 
      ggplot(aes(fill = LMA)) +
      geom_sf() +
      theme_minimal() +
      labs(title = "Fixed levee centerlines") +
      theme(legend.position = "none")} * 
    {shapefiles$DLISLevees %>% 
        ggplot(aes(fill = NAME)) +
        geom_sf() +
        theme_minimal() +
        labs(title = "RAND Model") +
        theme(legend.position = "none")},
  # Fixed levee polygons vs RAND model against Legal Delta
  {bind_rows(shapefiles$levees %>% 
               filter(LMA %in% {polygonDF %>% 
                   filter(st_is_empty(.)) %>% 
                   pull(LMA)}) %>% 
               mutate(dataset = "leftover"),
             polygonDF %>% 
               st_cast("MULTIPOLYGON") %>% 
               filter(!st_is_empty(.)) %>% 
               mutate(dataset = "levees"),
             shapefiles$deltaLegal %>% 
               mutate(dataset = "delta") %>% 
               st_cast("LINESTRING")) %>% 
      ggplot() +
      geom_sf(aes(color = dataset)) +
      theme_minimal() +
      theme(legend.position = "bottom")} *
    {bind_rows(shapefiles$DLISLevees %>% 
                 mutate(dataset = "levees"),
               shapefiles$deltaLegal %>% 
                 mutate(dataset = "delta") %>% 
                 st_cast("MULTILINESTRING")) %>% 
        ggplot() +
        geom_sf(aes(color = dataset)) +
        theme_minimal() +
        theme(legend.position = "bottom")},
  # Fixed levee polygons vs RAND model against functional Delta
  {bind_rows(shapefiles$levees %>% 
               filter(LMA %in% {polygonDF %>% 
                   filter(st_is_empty(.)) %>% 
                   pull(LMA)}) %>% 
               mutate(dataset = "leftover"),
             polygonDF %>% 
               filter(!st_is_empty(.)) %>% 
               mutate(dataset = "levees"),
             shapefiles$deltaTidal %>% 
               mutate(dataset = "delta") %>% 
               st_cast("LINESTRING")) %>% 
      ggplot() +
      geom_sf(aes(color = dataset)) +
      theme_minimal() +
      theme(legend.position = "bottom")} *
    {bind_rows(shapefiles$DLISLevees %>% 
                 mutate(dataset = "levees"),
               shapefiles$deltaTidal %>% 
                 mutate(dataset = "delta") %>% 
                 st_cast("MULTILINESTRING")) %>% 
        ggplot() +
        geom_sf(aes(color = dataset)) +
        theme_minimal() +
        theme(legend.position = "bottom")},
  # Fixed levee polygons using Legal Delta and Functional Delta
  {bind_rows(shapefiles$levees %>% 
               filter(LMA %in% {polygonDF %>% 
                   filter(st_is_empty(.)) %>% 
                   pull(LMA)}) %>% 
               mutate(dataset = "leftover"),
             polygonDF %>% 
               filter(!st_is_empty(.)) %>% 
               mutate(dataset = "levees"),
             shapefiles$deltaLegal %>% 
               mutate(dataset = "delta") %>% 
               st_cast("LINESTRING")) %>% 
      ggplot() +
      geom_sf(aes(color = dataset)) +
      theme_minimal() +
      labs(title = "Fixed polygons, Legal Delta") +
      theme(legend.position = "bottom")} *
    {bind_rows(shapefiles$levees %>% 
                 filter(LMA %in% {polygonDF %>% 
                     filter(st_is_empty(.)) %>% 
                     pull(LMA)}) %>% 
                 mutate(dataset = "leftover"),
               polygonDF %>% 
                 filter(!st_is_empty(.)) %>% 
                 mutate(dataset = "levees"),
               shapefiles$deltaTidal %>% 
                 mutate(dataset = "delta") %>% 
                 st_cast("LINESTRING")) %>% 
        ggplot() +
        geom_sf(aes(color = dataset)) +
        theme_minimal() +
        labs(title = "Fixed polygons, Functional Delta") +
        theme(legend.position = "bottom")},
  {polygonDF %>% 
      ggplot(aes(fill = LMA)) +
      geom_sf() +
      theme_minimal(base_size = 19) +
      labs(title = "From RData") +
      theme(legend.position = "none")} * 
    {st_read(file.path("data", "shapefiles", "fixedLevees", "fixedLevees.shp")) %>% 
        ggplot(aes(fill = LMA)) +
        geom_sf() +
        theme_minimal(base_size = 19) +
        labs(title = "From shapefile") +
        theme(legend.position = "none")}
) %>% 
  setNames(
    c("fixedVsRAND",
      "againstLegalDelta",
      "againstFunctionalDelta",
      "fixedVsDelta",
      "RDataVsShapefile")
  )

library(Cairo)

# Loop through the list and save each plot

for (i in seq_along(finalFigures)) {
  CairoPNG(file = file.path("data", "figures", paste0(names(finalFigures)[i], ".png")), 
           width = 15, height = 10, units = "in", res = 300)
  print(finalFigures[[i]])
  dev.off()
}

