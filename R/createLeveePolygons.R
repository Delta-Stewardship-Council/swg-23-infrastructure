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
  library(patchwork)
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


#' Plotting a section of a shapefile
#'
#' @param start Start of the line, 0-1 
#' @param end End of the line, 0-1
#' @param shp The shapefile, defaults to the functional Delta boundary
#' @param returnPlot T/F, if T, will return a plot while F will return the dataset
#'
#' @return
#' @export
#'
#' @examples 
plotSectionBoundary <- function(start, end, shp = shapefiles$functionalDelta, returnPlot = T) {
  linstringDelta <- shp %>% 
    st_cast("LINESTRING")
  
  df <- bind_rows(
    linstringDelta %>% 
      mutate(state = "boundary"),
    linstringDelta %>% 
      lwgeom::st_linesubstring(start, end) %>% 
      mutate(state = "selected")
  )
  
  if (returnPlot) {
    return({
      ggplot(df, aes(color = state)) +
        geom_sf()
    })
  } else {
    df %>% 
      filter(state == "selected")
  }
}


#' Draw a closest line between a point and a line
#'
#' @param startingLine Line to find the point to connect FROM
#' @param toLine Line to connect the point TO
#' @param positionIndex Which point to use from the starting line
#' @param check T/F, if a plot should be generated of the generated line.
#'
#' @return
#' @export
#'
#' @examples
closestLine <- function(startingLine, toLine, positionIndex = NULL, check = F) {
  xPoints <- st_boundary(startingLine) %>% 
    st_cast("POINT")
  
  if (is.null(positionIndex)) {
    return({
      bind_rows(startingLine,
                xPoints %>% 
                  mutate(ID = factor(row_number()))) %>% 
        ggplot() +
        geom_sf(aes(color = ID))
    })
  }

  pointInterest <- xPoints %>% 
    filter(row_number() == positionIndex)
  
  finLine <- st_nearest_points(pointInterest,
                               toLine)
  if (check) {
    return({
      bind_rows(startingLine,
                pointInterest,
                toLine,
                finLine %>% 
                  st_sf() %>% 
                  mutate(desiredLine = T)) %>% 
        ggplot(aes(color = desiredLine)) +
        geom_sf()
    })
  }
  finLine %>% 
    st_sf()
}

#' Plot a set of LINESTRING in leaflet
#'
#' @details
#' Meant to help explore where gaps might exist
#' 
#' @param x An sf object
#'
#' @return
#' @export
#'
#' @examples
plotLinesLeaflet <- function(x) {
  if (is.null(x$label)) {
    if (is.null(x$name)) {
      x <- mutate(x, label = paste0("centerline_", OBJECTID))
    } else {
      x <- mutate(x, label = ifelse(is.na(name), paste0("centerline_", OBJECTID), name))
    }
  }
  
  if (!is.null(x$ID)) {
    x <- mutate(x, label = paste0(label, "_id", ID))
  }
  
  linesMapData <- x %>% 
    st_transform(4326)
  
  pal <- colorFactor("inferno", domain = linesMapData$label)

  leaflet() %>% 
    addPolylines(
      data = linesMapData, opacity = 1,
      weight = 10, color = ~pal(label),
      popup = paste0(linesMapData$label)
    ) %>% 
    addProviderTiles("Esri.WorldImagery")
}

# These two functions are taken directly from:
# https://github.com/Jean-Romain/ALSroads
# Can download the package once I look closer at the other functions
# For now, was taken from SO question

st_ends_heading <- function(line)
{
  M <- sf::st_coordinates(line)
  i <- c(2, nrow(M) - 1)
  j <- c(1, -1)
  
  headings <- mapply(i, j, FUN = function(i, j) {
    Ax <- M[i-j,1]
    Ay <- M[i-j,2]
    Bx <- M[i,1]
    By <- M[i,2]
    unname(atan2(Ay-By, Ax-Bx))
  })
  
  return(headings)
}

st_extend_line <- function(line, distance, end = "BOTH")
{
  if (!(end %in% c("BOTH", "HEAD", "TAIL")) | length(end) != 1) stop("'end' must be 'BOTH', 'HEAD' or 'TAIL'")
  
  M <- sf::st_coordinates(line)[,1:2]
  keep <- !(end == c("TAIL", "HEAD"))
  
  ends <- c(1, nrow(M))[keep]
  headings <- st_ends_heading(line)[keep]
  distances <- if (length(distance) == 1) rep(distance, 2) else rev(distance[1:2])
  
  M[ends,] <- M[ends,] + distances[keep] * c(cos(headings), sin(headings))
  newline <- sf::st_linestring(M)
  
  # If input is sfc_LINESTRING and not sfg_LINESTRING
  if (is.list(line)) newline <- sf::st_sfc(newline, crs = sf::st_crs(line))
  
  return(newline)
}

#' Read in a spatial layer from the ESRI REST API
#' 
#' @details
#' Use the API Explorer if you are unfamiliar with building a query for ESRI's API
#' I have only tested this with a geojson file, which is supported by `st_read`.
#' 
#'
#' @param url The API url.
#'
#' @return
#' @export
#'
#' @examples {
#' readFromAPI("https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/NLD2_PUBLIC_v1/FeatureServer/17/query?outFields=*&where=1%3D1&f=geojson")
#' }
readFromAPI <- function(url, query) {

  # Does your query have at least outFields, where, and f? 
  if (!all(c("outFields", "where", "f") %in% names(query))) 
    stop("Your query should include at minimum: outFields, where, and f.", 
         call. = F)
  
  # How long is the dataset?
  lengthQuery <- httr::content(
    httr::GET(url, 
              query = c(query, 
                        returnCountOnly = "true")), 
    "text"
  )
  
  length <- as.numeric(sub('.*"count":([0-9]+).*', '\\1', lengthQuery))
  
  if (length >= 1000) {
    offset <- 0
    data <- list()
    
    repeat {
      queryParameters <- c(query,
                           resultOffset = offset,
                           resultRecordCount = 1000)
      
      response <- httr::GET(url, query = queryParameters)
      
      if (response$status_code == 200) {
        parsedData <- httr::content(response, "text")
        readData <- sf::st_read(parsedData, quiet = T)
      } else {
        stop(paste0("Check your link, failed to retrieve data. Starting offset was ", offset), call. = F)
      }
      
      lengthData <- nrow(readData)
      data <- c(data, list(readData))
      
      if (lengthData < 1000) {
        data <- do.call(rbind, data)
        break()
      }
      
      offset <- offset + lengthData
    }
  } else {
    response <- httr::GET(url, query = query)
    
    if (response$status_code == 200) {
      parsedData <- httr::content(response, "text")
      data <- sf::st_read(parsedData, quiet = T)
    } else {
      stop("Check your link, failed to retrieve data.", call. = F)
    }
  }
  data
}
