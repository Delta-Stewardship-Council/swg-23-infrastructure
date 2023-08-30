#' Show all available links on a webpage
#'
#' @param url Webpage URL
#' @param selector Optional. The specific portion of the webpage to focus on
#'
#' @return A vector of URL strings
#' @export
#'
#' @examples
#' showLinks("https://data.cnra.ca.gov/dataset/california-conservation-easement-database")
#' 
#' showLinks("https://data.cnra.ca.gov/dataset/california-conservation-easement-database", "#dataset-resources")
showLinks <- function(url, selector = NULL) {
  
  page <- read_html(url)
  
  if (!is.null(selector)) {
    if (selector == "#dataset-resources")
      page <- html_nodes(page, selector)
    namesNodes <- html_nodes(page, ".heading")
    
    namesHeading <- html_text(namesNodes)
    namesHeading <- regmatches(namesHeading, regexpr("(?<=\\n).*?(?=\\n)", namesHeading, perl = T))
    namesHeading <- gsub("\\s{2,}", "", namesHeading)
    
    links <- html_nodes(page, "a")
    
    linkURLs <- html_attr(links, "href")
    
    data.frame(
      item = namesHeading,
      url = linkURLs[grepl("https", linkURLs)]
    )
  } else {
    links <- html_nodes(page, "a")
    
    linkURLs <- html_attr(links, "href")
    linkURLs
  }
}

#' Download files from the CNRA website
#'
#' @param url URL of the data package
#' @param fileLink URL of the file of interest. If `NULL`, will return to you
#' a list of possible links to choose from. This is limited to the "dataset-resources"
#' section of the website.
#' @param path File path to save the file. Defaults to the temporary directory.
#' @param ... Additional arguments to be sent to `download.file`. Generally,
#' meant to accommodate specifying the `method` argument.
#'
#' @return A character string that will appear only if the file was successfully created.
#' @export
#'
#' @examples
downloadCNRA <- function(url, file = NULL, path = tempdir(), ...) {
  
  linkURLs <- showLinks(url, "#dataset-resources")
  
  if (is.null(file)) {
    message("Choose from one of the following `items`, specifying it in the `file` argument of this function: ")
    return(linkURLs)
  }
  
  if (!grepl("https://", file)) {
    file <- subset(linkURLs, item == file)[["url"]]
  }
  
  fileName <- tempfile()
  
  download.file(file, destfile = fileName, ...)
  fileName
}

#' Extracting a shape file from a zipped folder
#'
#' @param filePath File path to the file.
#' @param outPath File path to save the zipped files.
#'
#' @return All zipped files will be extracted. The file directory of the 
#' shape file(s) will be returned as a string.
#' @export
#'
#' @examples
unzipShapefile <- function(filePath, outPath) {
  
  fileNames <- unzip(filePath, list = T)[["Name"]]
  shapefileName <- fileNames[which(grepl("*.\\.shp$", fileNames))]
  unzip(filePath, exdir = outPath)
  cat("The extracted shape file(s) is/are:", shapefileName)
}