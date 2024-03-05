## ------------------------------------------ ##
#   Infrastructure -- `terra` Demonstration
## ------------------------------------------ ##

# Life stage: EXPLORATORY 

# Script author(s): Angel Chen

# Purpose:
## Explore functions from terra, which includes:
## rast(), vect(), mask(), crop(), and extract()

## ------------------------------------------ ##
#           Masking with a raster ----
## ------------------------------------------ ##

# load the package
library(terra)

# get the file path of the elevation raster
f <- system.file("ex/elev.tif", package="terra")
# read in the elevation raster
r <- rast(f)
# create a new raster marking the areas where elevation is over 400
# this will be our mask
msk <- ifel(r < 400, NA, 1)
# plot the original elevation raster
plot(r, reset = F)
# plot the mask on top of that
plot(msk, add = T)

# mask values in the elevation raster with our mask
m <- mask(r, msk)
# plot the result
plot(m)

## ------------------------------------------ ##
#         Masking with a shapefile ----
## ------------------------------------------ ##

# get the file path of the shapefile
f <- system.file("ex/lux.shp", package="terra")
# read in the shapefile
v <- vect(f)[1,]
# plot the original elevation raster
plot(r, reset = F)
# plot the shapefile on top of that
plot(v, add = T)

# mask values in the elevation raster with our shapefile
mv1 <- mask(r, v)
# compare with this alternative method of cropping our elevation raster to the extent of our shapefile, and then masking
mv2 <- crop(r, v, mask = T)
# plot and compare the results
plot(mv1)
plot(mv2)

# note that you can also turn raster objects into data frames for convenience
df1 <- as.data.frame(mv1)
df2 <- as.data.frame(mv2)

## ------------------------------------------ ##
#     Extracting values from a raster ----
## ------------------------------------------ ##

# extract the mean elevation in the area covered by the shapefile
mean_elev <- extract(x = r, y = v, fun = mean, na.rm = T)
mean_elev
