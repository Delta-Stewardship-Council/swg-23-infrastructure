#' Plot the polygons side-by-side
#'
#' @param name Name of the area of interest, all in lower case
#'
#' @return A side-by-side ggplot object.
#' @export
#'
#' @details
#' This is a very limited helper function, requiring two things be already
#' defined: 1) DLISLevees shape file and 2) polygonized centerlines from the levees
#' dataset. These will remain hard coded here. This function will plot the 
#' polygon area from the DLIS layer on the left and the polygon created from 
#' the centerline shape file on the right. 
#' 
#' The function is meant to compare the polygons between the two approaches to
#' compare the resulting area calculations. 
#' 
#' @note
#' Requires `library(patchwork)`
#' 
#' @examples
#' compareIslands("sherman island")
compareIslands <- function(name) {
  titleCase <- tools::toTitleCase(name)
  
  {shapefiles$DLISLevees %>% 
      filter(NAME == toupper(name)) %>% 
      {
        ggplot(data = .) +
          geom_sf(aes(fill = NAME)) +
          labs(title = paste0(titleCase, "_DLIS"), 
               subtitle = paste0("Acres: ", units::set_units(st_area(.), acre))) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    } *
    area$levees[[titleCase]]$p
}

#' Highlight DLIS polygon on overall map besides the levee centerlines
#'
#' @param name Name of the area of interest, all in lower case
#'
#' @return A side-by-side ggplot object.
#' @export
#'
#' @details
#' This is a very limited helper function, requiring two things be already
#' defined: 1) DLISLevees shape file and 2) polygonized centerlines from the levees
#' dataset. These will remain hard coded here. This function highlight the
#' specified polygon on the overall DLIS map on the left and the levee centerlines 
#' on the right. 
#' 
#' The function is meant to help visualize polygons that could not be polygonized.
#' 
#' @note
#' Requires `library(patchwork)`
#' 
#' @examples
compareIslandsTotal <- function(name) {
  {ggplot(shapefiles$DLISLevees %>% 
            mutate(NAME = ifelse(NAME == toupper(name), NAME, NA))) +
      geom_sf(aes(fill = NAME)) +
      theme_minimal() +
      theme(legend.position = "none")} *
    {ggplot(shapefiles$levees %>% 
              mutate(OBJECTID = factor(OBJECTID)) %>% 
              filter(LMA == tools::toTitleCase(name))) +
        geom_sf(aes(color = OBJECTID)) +
        theme_minimal() +
        theme(legend.position = "bottom")}
}

#' Plot the levee lines
#'
#' @param name Name of the levee of interest
#' @param x An object of class sf. NOT filtered for area of interest
#'
#' @return A ggplot object of the leveed area, colored based on OBJECTID
#' @export
#'
#' @examples
plotLeveeLines <- function(name, x = shapefiles$levees) {
  x %>% 
    mutate(OBJECTID = factor(OBJECTID)) %>% 
    filter(LMA %in% tools::toTitleCase(name)) %>% 
    ggplot() +
    geom_sf(aes(color = OBJECTID))
}

#' Combine levee centerlines together
#'
#' @param name Name of the levee of interest
#' @param objects Object ID of the levee of interest
#' @param x An object of class sf. NOT filtered for area of interest
#'
#' @return A polygonized object.
#' @export
#'
#' @details
#' Ensure that your x is not already fitltered here.
#' 
#' @examples
sharedLevees <- function(name, objects, x = shapefiles$levees) {
  x %>% 
    filter(LMA %in% tools::toTitleCase(name) |
             OBJECTID %in% objects) %>% 
    st_union() %>% 
    st_polygonize()
}

#' Fix a polygon by snapping lines close to one another together
#'
#' @param x An object of class sf
#' @param tolerance Distance value used to potentially snap two centerlines together
#'
#' @return A polygonized object.
#' @export
#'
#' @details
#' This function is meant to try and snap two centerlines together. A tolerance
#' value must be provided and it will determine if two centerlines are close enough
#' to be snapped together.
#' 
#' There is an assumption that lines that are greater than 0.1 but less than the 
#' tolerance value are candidates to be snapped together.
#' 
#' @examples {
#' snapCenterlines(shapefiles$levees %>% 
#' filter(LMA == "New Hope Tract"), 15)
#' }
snapCenterlines <- function(x, tolerance, minDistance = 0.1) {
  
  if (missing(tolerance)) stop("Define your tolerance value.", call. = F)
  
  distanceMatrix <- x %>% 
    st_distance()
  
  lapply(1:nrow(distanceMatrix), function(i) {
    
    numericDistance <- as.numeric(distanceMatrix[i, ])
    touches <- numericDistance < minDistance
    potentialTouches <- numericDistance < tolerance
    differences <- potentialTouches - touches
    
    if (sum(differences) != 0) {
      j <- which(differences != 0)
      newLine <- st_snap(x[i, ], x[j, ], tolerance)
      return(newLine)
    } else return(x[i, ])
  }) %>% 
    bind_rows() %>% 
    st_union() %>% 
    st_polygonize()
}

drawLine <- function(x, y, xPosition = NULL, yPosition = NULL, returnPoints = F) {
  xPoint <- x %>% 
    st_boundary() %>% 
    st_cast("POINT")
  
  yPoint <- y %>% 
    st_boundary() %>% 
    st_cast("POINT")
  
  if (is.null(xPosition)) {
    xPlot <- bind_rows(x,
              xPoint %>% 
                mutate(ID = factor(row_number()))) %>% 
      ggplot() +
      geom_sf(aes(color = ID))
  }
  
  if (is.null(yPosition)) {
    yPlot <- bind_rows(y,
              yPoint %>% 
                mutate(ID = factor(row_number()))) %>% 
      ggplot() +
      geom_sf(aes(color = ID))
  }
  
  if (is.null(xPosition) & is.null(yPosition)) {
    return({
      print(xPlot * yPlot)
    })
  } else {
    if (is.null(xPosition)) {
      return(print(xPlot))
    } else {
      if (is.null(yPosition)) {
        return(print(yPlot))
      }
    }
  }
  
  matrixPoints <- as.matrix(rbind(
    xPoint %>% 
      .[xPosition, ] %>% 
      st_coordinates(),
    yPoint %>% 
      .[yPosition, ] %>% 
      st_coordinates()
  ))
  
  if (returnPoints) return(matrixPoints)

  st_linestring(matrixPoints) %>% 
    st_sfc(crs = 3310) %>% 
    st_sf()
}
