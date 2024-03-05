library(sf)
library(dplyr)
library(ggplot2)

# Purpose -----------------------------------------------------------------

# This script explores how to find overlapping areas between two polygons. This 
# is important for our project as we would like to calculate assets contained
# within each leveed area.

# Starting example --------------------------------------------------------
# https://stackoverflow.com/questions/76019507/clipping-polygons-using-other-polygons-sf-objects-in-r

# one big polygon
pol1 = st_polygon(list(rbind(c(0,0),c(8,0),c(8,8),c(0,8),c(0,0))))

# your two cases: inside and outside 
pol_in = st_polygon(list(rbind(c(4,4),c(4,6),c(6,6),c(6,4),c(4,4))))
pol_out = st_polygon(list(rbind(c(10,2),c(10,3),c(11,3),c(11,2),c(10,2))))
# Changing this to partially overlap with the larger polygon
pol_touching = st_polygon(list(rbind(c(7,3),c(7,5),c(9,5),c(9,3),c(7,3))))

fire <- st_sf(data.frame(name = c("pol_in", "pol_out")),  
              geometry = st_sfc(pol_in, pol_out))

# quick plot
plot(pol1, xlim = c(0,12))
plot(fire$geometry, add = T)

# what you want
plot(fire[pol1, ], add = T)

# Overlapping one
plot(pol_touching, add = T)

# So it seems as though the "joining" in this example isn't really a join but 
# simply a bind, i.e., the metrics in pol in or pol out will not
# joined to pol1 per area of interest.

# Exploring joins ---------------------------------------------------------

# There are many different type of joins. Which one is appropriate for our effort?

# Converting the polygons above into sf data frames to simulate our polygons
shapeOverlapping <- st_sf(data.frame(name = "polOverlap"), geometry = st_sfc(pol_touching)) %>% 
  mutate(extraInfo = T)
shapeNoOverlap <- st_sf(data.frame(name = "polOut"), geometry = st_sfc(pol_out)) %>% 
  mutate(extraInfo = T)
ogShape <- st_sf(data.frame(name = "polOg"), geometry = st_sfc(pol1))

exploreJoins <- function(x, y, joinType) {
  shape1 <- st_join(x, y, join = joinType)
  shape2 <- st_join(y, x, join = joinType)
  p <- ggplot() +
    geom_sf(data = shape1) +
    geom_sf(data = shape2)

  print(shape1)
  print(shape2)
  print(st_area(shape1))
  print(st_area(shape2))
  print(p)
}

# Normal join, st_intersects
exploreJoins(ogShape, shapeOverlapping, st_intersects)
# Seems to joins the extra information but does not keep only the overlapped area

# st_contains_properly
exploreJoins(ogShape, shapeOverlapping, st_contains_properly)
# Seems to joins the extra information but does not keep only the overlapped area

# st_contains
exploreJoins(ogShape, shapeOverlapping, st_contains)
# Seems to joins the extra information but does not keep only the overlapped area

# st_covered_by
exploreJoins(ogShape, shapeOverlapping, st_covered_by)
# Seems to joins the extra information but does not keep only the overlapped area

# st_covers
exploreJoins(ogShape, shapeOverlapping, st_covers)
# Seems to joins the extra information but does not keep only the overlapped area

# st_crosses
exploreJoins(ogShape, shapeOverlapping, st_crosses)
# Seems to joins the extra information but does not keep only the overlapped area

# st_disjoint
exploreJoins(ogShape, shapeOverlapping, st_disjoint)
# Seems to joins the extra information but does not keep only the overlapped area

# st_equals_exact
# exploreJoins(ogShape, shapeOverlapping, st_equals_exact)
# Doesn't work; likely need to have specific requirements for this. Doesn't
# seem like what we want anyways

# st_equals
exploreJoins(ogShape, shapeOverlapping, st_equals)
# Seems to joins the extra information but does not keep only the overlapped area

# st_overlaps
exploreJoins(ogShape, shapeOverlapping, st_overlaps)
# Seems to joins the extra information but does not keep only the overlapped area

# Seems as though all the joins do NOT automatically chose the overlapped areas
# They seem to simply be a T/F query of if the two dataframes should be joined
# or not, and that's it. 

# Next, try: 
# 1. Find a join that correctly identifies overlapping areas. This should mean 
# the extraInfo column is retained
# 2. Watch the order of x and y. Ideally, we want to retain the geometry of
# the leveed area
# 3. Calculate the overlapping area of the two geometry

# Try st_overlap
overlapJoin <- st_join(ogShape, shapeOverlapping, join = st_overlaps)
overlapped <- overlapJoin %>% 
  mutate(overlappedArea = st_area(st_intersection(st_geometry(ogShape), st_geometry(shapeOverlapping))))
# This calculates the value. Does not combine the geometries
intersected <- st_sf(name = "overlappedRegion", 
                     geometry = st_intersection(st_geometry(ogShape), st_geometry(shapeOverlapping)))
ggplot() +
  geom_sf(data = ogShape) +
  geom_sf(data = shapeOverlapping, fill = "khaki") +
  geom_sf(data = intersected, fill = "firebrick")

combined <- bind_rows(ogShape, intersected)
ggplot(combined) +
  geom_sf(data = shapeOverlapping, fill = "khaki") +
  geom_sf(aes(fill = name)) +
  scale_fill_manual(values = c("firebrick", "grey90")) +
  guides(fill = "none")

combinedArea <- bind_rows(overlapped, intersected)
ggplot(combinedArea) +
  geom_sf()

# Does st_overlap keep those inside the polygon, i.e., fully overlapped
shapeInside <- st_sf(data.frame(name = "polIn"), geometry = st_sfc(pol_in)) %>% 
  mutate(extraInfo = T)

intersect2 <- st_join(ogShape, bind_rows(shapeInside, shapeOverlapping, shapeNoOverlap), join = st_overlaps)

# This is problematic...Only keeps the overlapping polygon. Should have kept
# shapeInside as well.

intersect3 <- st_intersection(ogShape, 
                              bind_rows(shapeInside, shapeOverlapping, shapeNoOverlap))
# This appears to be more of what we want, where shapeInside and ONLY the overlapped
# portion of shapeOverlapping are kept. However, the geometry of the original
# shape isn't kept. Need to add this back
ggplot(ogShape) +
  geom_sf() +
  geom_sf(data = intersect3, fill = "firebrick")

# Calculate the areas
intersect3 <- intersect3 %>% 
  mutate(area = units::set_units(st_area(geometry), acres))

# Last step would be to join back the geometry of ogShape, however, this will be
# more complicated for the levees since it has to be done per shape.

# Conclusion --------------------------------------------------------------

# Originally, I had thought that you could simply combine the geometries of two
# objects of interest together into a single row in the sf data frame, i.e.,
# geometry a and geometry b becomes geometry c which contains both a and b.
# I was mistaken. I guess this now makes sense since there would be no way
# to truly differentiate those combined geometries with one another if they were
# a singular geometry. I then explored several approaches to join these polygons
# together. Since we are only interested in the overlapping areas between
# our polygons, all the different join types in st_joins() were too generic to
# keep both polygons entirely within AND only the overlapped portion to our
# leveed area.

# The solution to this is multistep:
# Use st_intersection() to find polygons that intersects with the leveed polygon.
# This is not to be confused with st_intersects, which appears to only check
# to see if polygons are intersecting and does not account for only the 
# overlapped areas. After st_interesections, you can then calculate area. Be
# sure to add back to original polygon geometry.

