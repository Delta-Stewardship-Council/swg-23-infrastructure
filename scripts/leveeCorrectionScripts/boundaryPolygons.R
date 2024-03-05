# This script will create the polygons on the boundary, rule 4 within the ruleset.
# The `leveeFixRuleset.R` file will sourced this script directly.

nceas <- list()

# NCEAS1 ------------------------------------------------------------------

nceas$polygon1 <- bind_rows(
  shapefiles$nld %>% 
    filter(name %in% c("MA 09 - City of Sacramento - American R left bank", 
                       "Beach Lake - Morrison Creek")),
  plotSectionBoundary(0.55, 0.7, returnPlot = F) %>% 
    filter(state == "selected") %>% 
    mutate(name = "deltaBoundary"),
  closestLine(shapefiles$nld %>% 
                filter(name %in% c("Beach Lake - Morrison Creek")),
              plotSectionBoundary(0.55, 0.7, returnPlot = F) %>% 
                filter(state == "selected"), positionIndex = 1) %>% 
    st_sf()
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS1")

# NCEAS2 ------------------------------------------------------------------

nceas$polygon2 <- bind_rows(
  # NCEAS1 closest line
  closestLine(shapefiles$nld %>% 
                filter(name %in% c("Beach Lake - Morrison Creek")),
              plotSectionBoundary(0.55, 0.7, returnPlot = F) %>% 
                filter(state == "selected"), positionIndex = 1),
  # boundary delta
  plotSectionBoundary(0.65, 0.8, returnPlot = F) %>% 
    filter(state == "selected"),
  # Required levee boundaries, NLD
  shapefiles$nld %>% 
    filter(name %in% c("Beach Lake - Morrison Creek")),
  # Required levee, centerline
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(38, 79, 190, 200, 199, 198, 7, 213, 214, 8)),
  # Touching 38 with the NLD levee
  closestLine(shapefiles$nld %>% 
                filter(name %in% c("Beach Lake - Morrison Creek")),
              shapefiles$levees %>% 
                filter(OBJECTID %in% c(38)), positionIndex = 2),
  # Connecting 198 and 213
  drawLine(
    shapefiles$levees %>% 
      filter(OBJECTID == 198),
    shapefiles$levees %>% 
      filter(OBJECTID == 213),
    2, 1
  ),
  # NCEAS2 closest line
  st_nearest_points(
    shapefiles$levees %>% 
      filter(OBJECTID == 8),
    plotSectionBoundary(0.55, 0.8, returnPlot = F)
  ) %>% 
    st_sf()
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS2")

# NCEAS3 ------------------------------------------------------------------

nceas$polygon3 <- bind_rows(
  # Closest line from NCEAS2
  st_nearest_points(
    shapefiles$levees %>% 
      filter(OBJECTID == 8),
    plotSectionBoundary(0.55, 0.8, returnPlot = F)
  ) %>% 
    st_sf(),
  # Required levee centerlines
  shapefiles$levees %>%
    filter(OBJECTID %in% c(8, 14, 209, 262, 469, 497, 83, 453, 222)),
  # Joining 8 and 209
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 209),
                    shapefiles$levees %>% filter(OBJECTID == 8)) %>%
    st_sf(),
  # # Joining 209 and 14
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 209),
                    shapefiles$levees %>% filter(OBJECTID == 14)) %>%
    st_extend_line(1) %>%
    st_sf(),
  # Joining 469, 497
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 469),
                    shapefiles$levees %>% filter(OBJECTID == 497)) %>%
    st_sf(),
  # Joining 453 to 222
  closestLine(shapefiles$levees %>% filter(OBJECTID == 453),
              shapefiles$levees %>% filter(OBJECTID == 222), 2),
  # Joining 222 to boundary
  # st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 222),
  #                   plotSectionBoundary(0.55, 0.8, returnPlot = F)) %>%
  #   st_sf(),
  closestLine(shapefiles$levees %>% filter(OBJECTID == 222),
              plotSectionBoundary(0.55, 0.8, returnPlot = F), positionIndex = 2),
  plotSectionBoundary(0.7, 0.8, returnPlot = F)
) %>%
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS3")

# NCEAS4 ------------------------------------------------------------------

# NCEAS3 bottom line
nceas$polygon4 <- bind_rows(
  closestLine(shapefiles$levees %>% filter(OBJECTID == 222),
              plotSectionBoundary(0.55, 0.8, returnPlot = F), positionIndex = 2),
  # Required centerlines
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(85, 221, 84, 83)),
  # Joining 453 to 222
  closestLine(shapefiles$levees %>% filter(OBJECTID == 453),
              shapefiles$levees %>% filter(OBJECTID == 222), 2),
  # Closing levee 84
  drawLine(shapefiles$levees %>% filter(OBJECTID == 84),
           shapefiles$levees %>% filter(OBJECTID == 84), 2, 3),
  # Supplementary NLD, but don't want the full levee
  plotSectionBoundary(0, 0.97, shapefiles$nld %>% 
                        filter(name == "Sacramento County Levee 90"), returnPlot = F),
  plotSectionBoundary(0.78, 0.82, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS4")

# NCEAS5 ------------------------------------------------------------------

# NLD at the top of the polygon
nceas$polygon5 <- bind_rows(
  plotSectionBoundary(0, 0.4, shapefiles$nld %>% 
                        filter(name %in% c("Sacramento County Levee 32")), 
                      returnPlot = F),
  # 289 to Sac County Levee 32
  closestLine(shapefiles$levees %>% filter(OBJECTID == 289),
              plotSectionBoundary(0, 0.4, shapefiles$nld %>% 
                                    filter(name %in% c("Sacramento County Levee 32")), 
                                  returnPlot = F), 2),
  shapefiles$nld %>% 
    filter(name %in% c("San Joaquin County Levee 170")),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(289, 287, 118, 9, 178, 149, 150)),
  # 287 and 289
  drawLine(shapefiles$levees %>% filter(OBJECTID == 287),
           shapefiles$levees %>% filter(OBJECTID == 289), 2, 1),
  # 118 and 287
  closestLine(shapefiles$levees %>% filter(OBJECTID == 118),
              shapefiles$levees %>% filter(OBJECTID == 287), 2),
  # County Levee 170 to 150
  closestLine(shapefiles$nld %>% 
                filter(name %in% c("San Joaquin County Levee 170")),
              shapefiles$levees %>% filter(OBJECTID == 150), 2),
  # SJ County Levee 170 to 149
  drawLine(shapefiles$nld %>% 
             filter(name %in% c("San Joaquin County Levee 170")),
           shapefiles$levees %>% filter(OBJECTID == 149), 1, 6),
  # 149 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 149),
              plotSectionBoundary(0.78, 0.85, returnPlot = F), 6),
  plotSectionBoundary(0.78, 0.85, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS5")

# NCEAS6 ------------------------------------------------------------------

nceas$polygon6 <- bind_rows(
  # 149 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 149),
              plotSectionBoundary(0.78, 0.85, returnPlot = F), 6),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(149, 152, 467, 381, 380, 74, 476)),
  # 379 to 467
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(379)),
              shapefiles$levees %>% filter(OBJECTID %in% c(467)), 1),
  # 476 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 476),
              plotSectionBoundary(0.78, 0.85, returnPlot = F), 2),
  plotSectionBoundary(0.78, 0.85, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS6")


# NCEAS7 ------------------------------------------------------------------

nceas$polygon7 <- bind_rows(
  # 476 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 476),
              plotSectionBoundary(0.78, 0.85, returnPlot = F), 2),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(476, 358, 323, 325, 324, 120)),
  # 476 to 358
  closestLine(shapefiles$levees %>% filter(OBJECTID == 476),
              shapefiles$levees %>% filter(OBJECTID == 358), 1),
  # 358 to 323
  closestLine(shapefiles$levees %>% filter(OBJECTID == 358),
              shapefiles$levees %>% filter(OBJECTID == 323), 2),
  # 324 to 120
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 324),
                    shapefiles$levees %>% filter(OBJECTID == 120)) %>% 
    st_sf(),
  # 120 to boundary
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 120),
                    plotSectionBoundary(0.81, 0.85, returnPlot = F)) %>% 
    st_sf(),
  # boundary
  plotSectionBoundary(0.81, 0.85, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS7")

# NCEAS8 ------------------------------------------------------------------

nceas$polygon8 <- bind_rows(
  # 120 to boundary
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 120),
                    plotSectionBoundary(0.81, 0.85, returnPlot = F)) %>% 
    st_sf(),
  shapefiles$levees %>% filter(OBJECTID %in% c(120, 324)),
  # 324 to 120
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 324),
                    shapefiles$levees %>% filter(OBJECTID == 120)) %>% 
    st_sf(),
  shapefiles$nld %>% filter(name == "Bear Creek - Units 7 west and 21"),
  # Bear Creek to 324
  closestLine(shapefiles$levees %>% filter(OBJECTID == 324),
              shapefiles$nld %>% 
                filter(name == "Bear Creek - Units 7 west and 21"), 2),
  # boundary
  plotSectionBoundary(0.84, 0.87, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS8")

# NCEAS9 ------------------------------------------------------------------

nceas$polygon9 <- bind_rows(
  shapefiles$nld %>% 
    filter(name == "Bear Creek - Units 7, 22, and 23"),
  # boundary
  plotSectionBoundary(0.84, 0.87, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS9")

# NCEAS10 -----------------------------------------------------------------

nceas$polygon10 <- bind_rows(
  shapefiles$nld %>% 
    filter(name == "Bear Creek - Units 8, 25, and 27"),
  # boundary
  plotSectionBoundary(0.84, 0.87, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS10")

# NCEAS11 -----------------------------------------------------------------

nceas$polygon11 <- bind_rows(
  shapefiles$nld %>% 
    filter(name == "Mormon Slough - Calaveras R right bank - RD 2074"),
  # boundary
  plotSectionBoundary(0.86, 0.89, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS11")

# NCEAS12 -----------------------------------------------------------------

nceas$polygon12 <- bind_rows(
  shapefiles$nld %>% 
    filter(name == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek"),
  closestLine(
    shapefiles$nld %>% 
      filter(name == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek"),
    plotSectionBoundary(0.88, 0.91, returnPlot = F), 2
  ),
  plotSectionBoundary(0.88, 0.91, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS12")

# NCEAS13 -----------------------------------------------------------------

nceas$polygon13 <- bind_rows(
  closestLine(
    shapefiles$nld %>% 
      filter(name == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek"),
    plotSectionBoundary(0.88, 0.91, returnPlot = F), 2
  ),
  # Required NLD
  shapefiles$nld %>% 
    filter(name == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek"),
  # Required centerlines
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(317, 91, 319, 318, 320)),
  # drawn boundary for NCEAS12 to 317
  closestLine(shapefiles$nld %>% 
                filter(name == "Mormon Slough-Calaveras left bank - RD 0404 - Duck Creek"),
              shapefiles$levees %>% filter(OBJECTID == 317), 2),
  # 320 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 320),
              plotSectionBoundary(0.89, 0.96, returnPlot = F), 2),
  # boundary
  plotSectionBoundary(0.89, 0.96, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS13")

# NCEAS14 -----------------------------------------------------------------

nceas$polygon14 <- bind_rows(
  # 320 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 320),
              plotSectionBoundary(0.89, 0.96, returnPlot = F), 2),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(320, 318, 92, 93, 423)),
  # 318 to 92
  closestLine(shapefiles$levees %>% filter(OBJECTID == 318),
              shapefiles$levees %>% filter(OBJECTID == 92), 1),
  # 92 to 93
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID == 92),
                    shapefiles$levees %>% filter(OBJECTID == 93)) %>% 
    st_sf(),
  # boundary
  plotSectionBoundary(0.95, 0.97, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS14")

# NCEAS15 -----------------------------------------------------------------

nceas$polygon15 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(423, 93, 94, 270)),
  plotSectionBoundary(0.96, 1, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS15")

# NCEAS16 -----------------------------------------------------------------

nceas$polygon16 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(270, 94, 95, 330)),
  plotSectionBoundary(0.96, 1, returnPlot = F),
  plotSectionBoundary(0, 0.01, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS16")

# NCEAS17 -----------------------------------------------------------------

nceas$polygon17 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(240, 242, 96)),
  plotSectionBoundary(0, 0.03, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS17")

# NCEAS18 -----------------------------------------------------------------

nceas$polygon18 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(296, 297, 101, 298, 100, 303, 304, 305, 302)),
  # 302 to boundary
  closestLine(shapefiles$levees %>% 
                filter(OBJECTID %in% 302),
              plotSectionBoundary(0.02, 0.05, returnPlot = F), 1),
  plotSectionBoundary(0.02, 0.05, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS18")

# NCEAS19 -----------------------------------------------------------------

nceas$polygon19 <- bind_rows(
  # 302 to boundary
  closestLine(shapefiles$levees %>% 
                filter(OBJECTID %in% 302),
              plotSectionBoundary(0.02, 0.05, returnPlot = F), 1),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(307, 308, 99, 309)),
  # 302 to 307
  drawLine(shapefiles$levees %>% filter(OBJECTID %in% c(302)),
           shapefiles$levees %>% filter(OBJECTID %in% c(307)), 1, 2),
  # 309 to boundary
  st_extend_line(closestLine(shapefiles$levees %>% 
                               filter(OBJECTID %in% 309),
                             plotSectionBoundary(0.02, 0.08, returnPlot = F), 1), 
                 distance = 100, end = "TAIL") %>% 
    st_sf(),
  plotSectionBoundary(0.02, 0.08, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS19")

# NCEAS20 -----------------------------------------------------------------

nceas$polygon20 <- bind_rows(
  # 465 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(465)),
              plotSectionBoundary(0.08, 0.1, returnPlot = F), 1),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(465, 112, 466, 486, 488, 489, 97, 485)),
  # 485 to boundary
  st_extend_line(
    closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(485)),
                plotSectionBoundary(0.08, 0.1, returnPlot = F), 1),
    100, "TAIL"
  ) %>% 
    st_sf(),
  plotSectionBoundary(0.08, 0.1, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS20")

# NCEAS21 -----------------------------------------------------------------

# RAND separates this area into three different locations. I don't know their
# justifications for doing so

nceas$polygon21 <- bind_rows(
  # 485 to boundary
  st_extend_line(
    closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(485)),
                plotSectionBoundary(0.08, 0.1, returnPlot = F), 1),
    100, "TAIL"
  ) %>% 
    st_sf(),
  # Byron Tract, but don't complete it to disregard from polygon
  plotSectionBoundary(0.5, 1, shapefiles$nld %>% 
                        filter(name == "RD 0800 - Byron Tract"), returnPlot = F),
  plotSectionBoundary(0, 0.1, shapefiles$nld %>% 
                        filter(name == "RD 0800 - Byron Tract"), returnPlot = F),
  # Byron Tract to 294
  st_nearest_points(shapefiles$nld %>% 
                      filter(name == "RD 0800 - Byron Tract"),
                    shapefiles$levees %>% filter(OBJECTID %in% 294)) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  # 102 and 294
  shapefiles$levees %>% filter(OBJECTID %in% c(102, 294, 485)),
  # 102 and 294
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID %in% c(102)),
                    shapefiles$levees %>% filter(OBJECTID %in% c(294))) %>% 
    st_sf(),
  # 485 to Byron Tract
  closestLine(shapefiles$levee %>% filter(OBJECTID == 485),
              shapefiles$nld %>% 
                filter(name == "RD 0800 - Byron Tract"), 
              2) %>% 
    st_extend_line(100, end = "TAIL") %>% 
    st_sf(),
  # 102 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 102),
              plotSectionBoundary(0.08, 0.12, returnPlot = F), 1),
  plotSectionBoundary(0.08, 0.12, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS21")

# NCEAS22 -----------------------------------------------------------------

nceas$polygon22 <- bind_rows(
  # 102 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 102),
              plotSectionBoundary(0.08, 0.12, returnPlot = F), 1),
  shapefiles$levees %>% filter(OBJECTID == 103),
  # 103 to 102
  drawLine(shapefiles$levees %>% filter(OBJECTID %in% 102),
           shapefiles$levees %>% filter(OBJECTID %in% 103), 1, 1),
  # 103 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 103),
              plotSectionBoundary(0.1, 0.13, returnPlot = F), 2) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  # boundary
  plotSectionBoundary(0.11, 0.13, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS22")

# NCEAS23 -----------------------------------------------------------------

nceas$polygon23 <- bind_rows(
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 103),
              plotSectionBoundary(0.1, 0.13, returnPlot = F), 2) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  drawLine(closestLine(shapefiles$levees %>% filter(OBJECTID %in% 103),
                       plotSectionBoundary(0.1, 0.13, returnPlot = F), 2),
           shapefiles$levees %>% filter(OBJECTID %in% 61), 1, 2),
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 61),
              plotSectionBoundary(0.11, 0.13, returnPlot = F), 1),
  shapefiles$levees %>% filter(OBJECTID %in% 61),
  plotSectionBoundary(0.11, 0.13, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS23")

# NCEAS24 -----------------------------------------------------------------

nceas$polygon24 <- bind_rows(
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 61),
              plotSectionBoundary(0.11, 0.13, returnPlot = F), 1),
  drawLine(closestLine(shapefiles$levees %>% filter(OBJECTID %in% 61),
                       plotSectionBoundary(0.11, 0.13, returnPlot = F), 1),
           shapefiles$levees %>% filter(OBJECTID == 408), 1, 1),
  shapefiles$levees %>% filter(OBJECTID %in% c(408, 62)),
  closestLine(shapefiles$levees %>% filter(OBJECTID == 62),
              plotSectionBoundary(0.11, 0.14, returnPlot = F), 1),
  plotSectionBoundary(0.12, 0.14, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS24")

# NCEAS25 -----------------------------------------------------------------

nceas$polygon25 <- bind_rows(
  closestLine(shapefiles$levees %>% filter(OBJECTID == 62),
              plotSectionBoundary(0.11, 0.14, returnPlot = F), 1),
  shapefiles$levees %>% filter(OBJECTID %in% c(113, 186)),
  # 113 to connecting boundary line
  drawLine(closestLine(shapefiles$levees %>% filter(OBJECTID == 62),
                       plotSectionBoundary(0.11, 0.14, returnPlot = F), 1),
           shapefiles$levees %>% filter(OBJECTID %in% c(113)), 1, 4),
  # Connecting points within 113
  drawLine(shapefiles$levees %>% filter(OBJECTID %in% c(113)),
           shapefiles$levees %>% filter(OBJECTID %in% c(113)), 2, 3),
  # 113 to 186
  st_nearest_points(shapefiles$levees %>% filter(OBJECTID %in% c(113)),
                    shapefiles$levees %>% filter(OBJECTID %in% c(186))) %>% 
    st_sf(),
  # 186 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 186),
              plotSectionBoundary(0.13, 0.15, returnPlot = F), 4) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  plotSectionBoundary(0.13, 0.15, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS25")

# NCEAS26 -----------------------------------------------------------------

nceas$polygon26 <- bind_rows(
  # 186 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID == 186),
              plotSectionBoundary(0.13, 0.15, returnPlot = F), 4) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  shapefiles$levees %>% filter(OBJECTID %in% c(186, 187, 188, 189, 114)),
  # 114 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% 114),
              plotSectionBoundary(0.13, 0.16, returnPlot = F), 1),
  plotSectionBoundary(0.13, 0.16, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS26")

# NCEAS27 -----------------------------------------------------------------

nceas$polygon27 <- bind_rows(
  # 328 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(328)),
              plotSectionBoundary(0.25, 0.29, returnPlot = F), 1),
  shapefiles$levees %>% filter(OBJECTID %in% c(328, 251, 193)),
  # 193 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(193)),
              plotSectionBoundary(0.25, 0.29, returnPlot = F), 1) %>% 
    st_extend_line(100, end = "TAIL") %>% 
    st_sf(),
  plotSectionBoundary(0.25, 0.29, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS27")

# NCEAS28 -----------------------------------------------------------------

nceas$polygon28 <- bind_rows(
  # 193 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(193)),
              plotSectionBoundary(0.25, 0.29, returnPlot = F), 1) %>% 
    st_extend_line(100, end = "TAIL") %>% 
    st_sf(),
  shapefiles$levees %>% filter(OBJECTID %in% c(193, 104, 192, 191)),
  # 191 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(191)),
              plotSectionBoundary(0.28, 0.31, returnPlot = F), 1),
  plotSectionBoundary(0.28, 0.31, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS28")

# NCEAS29 -----------------------------------------------------------------

nceas$polygon29 <- bind_rows(
  # 191 to boundary
  closestLine(shapefiles$levees %>% filter(OBJECTID %in% c(191)),
              plotSectionBoundary(0.28, 0.30, returnPlot = F), 1),
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(191, 192, 223, 228, 226, 225, 116)),
  # 192 to 223
  closestLine(shapefiles$levees %>% filter(OBJECTID == 192),
              shapefiles$levees %>% filter(OBJECTID == 223), 2),
  # 116 to boundary
  plotSectionBoundary(0.29, 0.36, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS29")

# NCEAS30 -----------------------------------------------------------------

nceas$polygon30 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(108, 107, 109, 454, 455, 174, 167, 446, 448, 437)),
  # 109 to 107
  closestLine(shapefiles$levees %>% filter(OBJECTID == 109),
              shapefiles$levees %>% filter(OBJECTID == 107), 3),
  # 454 to 455
  drawLine(shapefiles$levees %>% filter(OBJECTID == 454),
           shapefiles$levees %>% filter(OBJECTID == 455), 2, 1),
  # 455 to 174
  drawLine(shapefiles$levees %>% filter(OBJECTID == 455),
           shapefiles$levees %>% filter(OBJECTID == 174), 1, 1),
  plotSectionBoundary(0.34, 0.39, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS30")

# NCEAS31 -----------------------------------------------------------------

nceas$polygon31 <- bind_rows(
  shapefiles$levees %>% 
    filter(OBJECTID %in% c(448, 449, 451, 121, 477, 482, 470, 67, 471, 
                           433, 168, 437)),
  shapefiles$nld %>% 
    filter(name %in% c("Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1",
                       "Yolo Bypass West Levee - Cache Creek Unit 4",
                       "Cache Creek - RD 2035 - Willow Bypass",
                       "Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2")),
  # 470 to 67
  drawLine(shapefiles$levees %>% filter(OBJECTID %in% 67),
           shapefiles$levees %>% filter(OBJECTID %in% 470), 1, 2),
  # 477 to 168
  closestLine(shapefiles$levees %>% filter(OBJECTID == 477),
              shapefiles$levees %>% filter(OBJECTID == 168), 1) %>% 
    st_extend_line(100, end = "TAIL") %>% 
    st_sf(),
  # 448 to 449
  drawLine(shapefiles$levees %>% filter(OBJECTID == 448),
           shapefiles$levees %>% filter(OBJECTID == 449), 2, 1),
  # 477 to itself, x2
  drawLine(shapefiles$levees %>% filter(OBJECTID == 477),
           shapefiles$levees %>% filter(OBJECTID == 477), 2, 3),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 477),
           shapefiles$levees %>% filter(OBJECTID == 477), 4, 5),
  # 477 to 482
  drawLine(shapefiles$levees %>% filter(OBJECTID == 477),
           shapefiles$levees %>% filter(OBJECTID == 482), 13, 14),
  # 482 to itself
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 10, 11),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 3, 5),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 4, 6),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 1, 2),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 7, 8),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 9, 12),
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 482), 13, 15),
  # 483 to 471
  drawLine(shapefiles$levees %>% filter(OBJECTID == 482),
           shapefiles$levees %>% filter(OBJECTID == 471), 16, 5),
  # Will have to piecemeal the boundary to avoid "completed"
  # polygons from the NLD
  # Connects to "Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2"
  plotSectionBoundary(0.37, 0.43, returnPlot = F),
  # Connects "Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2"
  # to Cache Creek - RD 2035 - Willow Bypass
  st_linestring(
    as.matrix(rbind(
      st_intersection(shapefiles$nld %>% filter(name == "Cache Creek - RD 2035 - Willow Bypass"),
                      plotSectionBoundary(0.4, 0.45, returnPlot = F)) %>% 
        st_coordinates(),
      st_intersection(shapefiles$nld %>% 
                        filter(name %in% c("Putah Cr Unit 1 - Yolo Bypass - Willow Slgh Unit 2")),
                      plotSectionBoundary(0.43, 0.45, returnPlot = F)) %>% 
        st_coordinates()
    ))
  ) %>% 
    st_sfc(crs = 3310) %>% 
    st_extend_line(100, "TAIL") %>% 
    st_sf(),
  # Only want a part of Knights Landing U2 - Yolo Bypass - Service Area 6
  plotSectionBoundary(0, 0.4, 
                      shapefiles$nld %>% filter(name == "Knights Landing U2 - Yolo Bypass - Service Area 6"), F),
  plotSectionBoundary(0.5, 1, 
                      shapefiles$nld %>% filter(name == "Knights Landing U2 - Yolo Bypass - Service Area 6"), F),
  # Connects Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1 to 
  # Knights Landing U2 - Yolo Bypass - Service Area 6
  st_linestring(
    as.matrix(rbind(
      st_intersection(shapefiles$nld %>% filter(name == "Cache Crk U1 - Yolo Bypass U2 - Knights Landing U1"),
                      plotSectionBoundary(0.49, 0.50, returnPlot = F)) %>% 
        st_coordinates(),
      st_intersection(shapefiles$nld %>% 
                        filter(name %in% c("Knights Landing U2 - Yolo Bypass - Service Area 6")),
                      plotSectionBoundary(0.49, 0.495, returnPlot = F)) %>% 
        st_coordinates()
    ))
  ) %>% 
    st_sfc(crs = 3310) %>% 
    st_sf(),
  # Connect RD 1600, 0827, 0785, and 0537 - SacYolo North to boundary
  closestLine(plotSectionBoundary(0.495, 0.5145, returnPlot = F),
              shapefiles$nld %>% 
                filter(name %in% c("RD 1600, 0827, 0785, and 0537 - SacYolo North")), 2),
  # RD 1600, 0827, 0785, and 0537 - SacYolo North
  plotSectionBoundary(0, 0.5, shapefiles$nld %>% filter(name == "RD 1600, 0827, 0785, and 0537 - SacYolo North"), F),
  plotSectionBoundary(0.945, 1, shapefiles$nld %>% filter(name == "RD 1600, 0827, 0785, and 0537 - SacYolo North"), F),
  # West Sac
  plotSectionBoundary(0.1, 0.24, shapefiles$nld %>% filter(name == "West Sacramento"), F),
  # Combining West Sac and SacYolo North
  drawLine(plotSectionBoundary(0.945, 1, shapefiles$nld %>% filter(name == "RD 1600, 0827, 0785, and 0537 - SacYolo North"), F),
           # West Sac
           plotSectionBoundary(0.1, 0.24, shapefiles$nld %>% filter(name == "West Sacramento"), F), 
           1, 2),
  plotSectionBoundary(0.495, 0.52, returnPlot = F)
) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_sf() %>% 
  mutate(LMA = "NCEAS31")

