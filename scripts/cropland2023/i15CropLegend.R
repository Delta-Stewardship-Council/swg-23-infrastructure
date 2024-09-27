# Load required libraries
library(pdftools)
library(dplyr)

# Download and extract the zip file
url <- "https://data.cnra.ca.gov/dataset/6c3d65e3-35bb-49e1-a51e-49d5a2cf09a9/resource/44c1bde8-7ac4-4582-b8de-d09264e180fb/download/i15_crop_mapping_2020-gdb.zip"
tempFile <- tempfile(fileext = ".zip")
download.file(url, tempFile, mode = "wb")
tempDir <- tempdir()
unzip(tempFile, exdir = tempDir)

# Find the PDF file
pdfFile <- list.files(tempDir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)

# Read pages 20 to 23 of the PDF
pdfText <- pdf_text(pdfFile)[20:23]
# Parse each line to be by itself
sections <- strsplit(pdfText, "\n\n")

# Save the pdf to github for documentation
file.rename(pdfFile, file.path("doc", "i15CropDocumentation_2020.pdf"))

# --- Will simply create the data file by hand ---
sections

classSubclass <- bind_rows(
  data.frame(
    class = "G",
    description = "Grain and hay crops",
    ID = c(1, 2, 3, 6, 7),
    subclass = c(
      "Barley", "Wheat", "Oats", "Miscellaneous grain and hay", "Mixed grain and hay"
    )
  ),
  data.frame(
    class = "R",
    description = "Rice",
    ID = c(1, 2),
    subclass = c(
      "Rice", "Wild Rice"
    )
  ),
  data.frame(
    class = "F",
    description = "Field crops",
    ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    subclass = c(
      "Cotton", "Safflower", "Flax", "Hops", "Sugar beets","Corn (field & sweet)",
      "Grain sorghum", "Sudan", "Castor beans", "Beans (dry)", "Miscellaneous field", 
      "Sunflowers", "Hybrid sorghum/sudan", "Millet", "Sugar cane", 
      "Corn, Sorghum or Sudan grouped for remote sensing only"
    )
  ),
  data.frame(
    class = "P",
    description = "Pasture",
    ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    subclass = c(
      "Alfalfa & alfalfa mixtures", "Clover", "Mixed pasture", "Native pasture", 
      "Induced high water table native pasture", "Miscellaneous grasses", "Turf farms", 
      "Bermuda grass", "Rye grass", "Klein grass"
    )
  ),
  data.frame(
    class = "T",
    description = "Truck, nursery & berry crops",
    ID = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
    subclass = c(
      "Artichokes", "Asparagus", "Beans (green)", "Cole crops (mixture of 22-25)", 
      "Carrots", "Celery", "Lettuce (all descriptions)", "Melons, squash, and cucumbers (all descriptions)", 
      "Onions & garlic", "Peas", "Potatoes", "Sweet potatoes", "Spinach", 
      "Tomatoes (processing)", "Flowers, nursery & Christmas tree farms", 
      "Mixed (four or more)", "Miscellaneous truck", "Bush berries", "Strawberries", 
      "Peppers (chili, bell, etc.)", "Broccoli", "Cabbage", "Cauliflower", 
      "Brussels sprouts", "Tomatoes (market)", "Greenhouse", "Blueberries", 
      "Asian leafy vegetables", "Lettuce or Leafy Greens grouped for remote sensing only", 
      "Potato or Sweet potato grouped for remote sensing only"
    )
  ),
  data.frame(
    class = "D",
    description = "Deciduous fruits and nuts",
    ID = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    subclass = c(
      "Apples", "Apricots", "Cherries", "Peaches and nectarines", "Pears", 
      "Plums", "Prunes", "Figs", "Miscellaneous deciduous", "Mixed deciduous", 
      "Almonds","Walnuts", "Pistachios", "Pomegranates", 
      "Plums Prunes or Apricots grouped for remote sensing only"
    )
  ),
  data.frame(
    class = "C",
    description = "Citrius and subtropical",
    ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    subclass = c(
      "Grapefruit", "Lemons", "Oranges", "Dates", "Avocados", "Olives", 
      "Miscellaneous subtropical fruit", "Kiwis", "Jojoba", "Eucalyptus", 
      "Mixed subtropical fruits"
    )
  ),
  data.frame(
    class = "V",
    description = "Vineyards",
    ID = c(1, 2, 3),
    subclass = c(
      "Table grapes", "Wine grapes", "Raisin grapes"
    )
  ),
  data.frame(
    class = "YP",
    description = NA,
    ID = NA,
    subclass = c(
      "no subclass"
    )
  ),
  data.frame(
    class = "I",
    description = "Idle",
    ID = c(1, 2, 4),
    subclass = c(
      "Land not cropped the current or previous crop season, but cropped within the past three years", 
      "new lands being prepared for crop production", 
      "long term, land consistently idle for four or more years"
    )
  ),
  data.frame(
    class = "X",
    description = "Not cropped or unclassified",
    ID = NA,
    subclass = c(
      "no subclass"
    )
  ),
  data.frame(
    class = "S",
    description = "Semi-agricultural & incidental to agriculture",
    ID = c(1, 2, 3, 4, 5, 6),
    subclass = c(
      "Farmsteads (includes a farm residence)", "Livestock feed lot operations", 
      "Dairies", "Poultry farms", "Farmsteads (without a farm residence)", 
     " Miscellaneous semi-agricultural (small roads, ditches, non-planted areas of cropped fields)"
    )
  ),
  data.frame(
    class = "U",
    description = "Urban",
    ID = NA,
    subclass = c(
      "generic nomenclature with no subclass"
    )
  ),
  data.frame(
    class = "UR",
    description = "Urban residential",
    ID = c(1, 11, 12, 13, 14, 2, 21, 22, 23, 24, 3, 31, 32, 33, 34, 4, 41, 42, 43, 44),
    subclass = rep(c("Single family dwellings with lot sizes greater than 1 acre up to 5 acres (ranchettes, etc.)", 
                 "Single family dwellings with a density of 1 unit/acre up to 8+ units per acre", 
                 "Multiple family (apartments, condominiums, townhouses, barracks, bungalows, duplexes, etc.)", 
                 "Trailer courts"), each = 5),
    waterUseFactor = rep(c(NA, "0% to 25% area irrigated", "26% to 50% area irrigated", 
                           "51% to 75% area irrigated", "76% to 100% area irrigated"), 
                         times = 4)
  ),
  data.frame(
    class = "UC",
    description = "Commercial",
    ID = c(1, 2, 3, 4, 5, 6, 7, 8),
    subclass = c(
      "Offices, retailers, etc.", "Hotels", "Motels",
      "Recreation vehicle parking, camp sites", 
      "Institutions (hospitals, prisons, reformatories, asylums, etc., 
      having a reasonably constant 24-hour resident population)", 
      "Schools (yards to be mapped separately if large enough)", 
      "Municipal auditoriums, theaters, churches, buildings and stands associated with race tracks, 
      football stadiums, baseball parks, rodeo arenas, amusement parks, etc.", 
      "Miscellaneous highwater use (to be used to indicate a high water use condition not covered by the above categories.)"
    )
  ),
  data.frame(
    class = "UI",
    description = "Industrial",
    ID = c(1, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    subclass = c(
      "Manufacturing, assembling, and general processing", 
      "Extractive industries (oil fields, rock quarries, gravel pits, rock and gravel processing plants, etc.)", 
      "Storage and distribution (warehouses, substations, railroad marshalling yards, tank farms, etc.)", 
      "Saw mills", "Oil refineries" , "Paper mills", "Meat packing plants", 
      "Steel and aluminum mills", "Fruit and vegetable canneries and general food processing", 
      "Miscellaneous highwater use (to be used to indicate a high water use condition not covered by other categories.)", 
      "Sewage treatment plant including ponds", 
      "Waste accumulation sites (public dumps, sewage sludge sites, landfill and hazardous waste sites, etc.)",
      "Wind farms, solar collector farms, etc."
    )
  ),
  data.frame(
    class = "UL",
    description = NA,
    ID = c(1, 2, 3, 4, 5),
    subclass = c(
      "Lawn area – irrigated", "Golf course – irrigated", 
      "Orsubclassntal landscape (excluding lawns) – irrigated", "Cemeteries – irrigated", 
      "Cemeteries - not irrigated"
    )
  ),
  data.frame(
    class = "UV",
    description = "Vacant",
    ID = c(1, 3, 4, 6, 7),
    subclass = c(
      "Unpaved areas (vacant lots, graveled surfaces, play yards, developable open lands within urban areas, etc.)",
      "Railroad right of way", 
      "Paved areas (parking lots, paved roads, oiled surfaces, flood control channels, tennis court areas, auto sales lots, etc.)", 
      "Airport runways", 
      "Land in urban area that is not developable"
    )
  ),
  data.frame(
    class = "NC",
    description = "Native Class",
    ID = NA,
    subclass = c(
      "generic nomenclature with no subclass"
    )
  ),
  data.frame(
    class = "NV",
    description = "Native vegetation",
    ID = c(1, 2, 3, 4, 5, 6, 7),
    subclass = c(
      "Grassland", "Light brush", "Medium brush", "Heavy brush", 
      "Brush and timber", "Forest", "Oak woodland"
    )
  ),
  data.frame(
    class = "NR",
    description = "Riparian vegatation",
    ID = c(1, 2, 3, 4, 5),
    subclass = c(
      "Marsh lands, tules and sedges", "Natural highwater table meadow", 
      "Trees, shrubs or other larger stream side or watercourse vegetation",
      "Seasonal duck marsh, dry or only partially wet during summer", 
      "Permanent duck marsh, flooded during summer"
    )
  ),
  data.frame(
    class = "NW",
    description = "Water Surface",
    ID = c(1, 2, 3, 4, 5, 6, 7),
    subclass = c(
      "River or stream (natural fresh water channels)", 
      "Water channel (all sizes\n- ditches and canals - delivering water for irrigation and urban use - e.g. State Water Project, Central\nValley Project, water district canals, etc.)", 
      "Water channel (all sizes - ditches and canals - for removing\non-farm drainage water - surface runoff and subsurface drainage - e.g. Colusa drain, drainage ditches in\nImperial)", 
      "Freshwater lake, reservoir, or pond (all sizes, includes ponds for stock, recreation,\ngroundwater recharge, managed wetlands, on-farm storage, etc.)", 
      "Brackish and saline water (includes\nareas in estuaries, inland water bodies, the ocean, etc.)", 
      "Wastewater pond (dairy, sewage, cannery,\nwinery, etc.)", 
      "Paved water conveyance channels within urban areas (mainly for flood control)"
    )
  ),
  data.frame(
    class = "NB",
    description = "Barren and Wasteland",
    ID = c(1, 2, 3, 4, 5),
    subclass = c(
      "Dry stream channels", "Mine tailings", "Barren land", 
      "Salt flats", "Sand dunes"
    )
  ),
  data.frame(
    class = "E",
    description = "Entry denied",
    ID = NA,
    subclass = c(
      "no subclass"
    )
  ),
  data.frame(
    class = "Z",
    description = "Outside of study area",
    ID = NA,
    subclass = c(
      "no subclass"
    )
  )
)

# --- Write csv ---
write.csv(classSubclass, file.path("data-raw", "csvFiles", "i15cropSubclass.csv"), row.names = F)

# # Obsolete code. Tried using regex to parse the pdf manually but it got too complicated
# # Define the extractCropInfo function
# extractCropInfo <- function(text) {
#   patternMain <- "CLASS\\s*([^-]+)\\s*-\\s*([^:]+):"
#   mainMatch <- regexec(patternMain, text, perl = TRUE)
#   mainExtracted <- regmatches(text, mainMatch)[[1]]
#   
#   class <- trimws(mainExtracted[2])
#   cropType <- trimws(mainExtracted[3])
#   
#   patternCrops <- "(\\d+)\\s*-\\s*([^,.]*(?:,(?!\\s*\\d+\\s*-)[^,.]*)*)(?=[,.]|$)"
#   cropMatches <- gregexpr(patternCrops, text, perl = TRUE)
#   cropExtracted <- regmatches(text, cropMatches)[[1]]
#   
#   cropValues <- numeric()
#   cropNames <- character()
#   
#   for (match in cropExtracted) {
#     parts <- strsplit(match, "\\s*-\\s*")[[1]]
#     cropValues <- c(cropValues, as.numeric(parts[1]))
#     cropNames <- c(cropNames, trimws(parts[2]))
#   }
#   
#   df <- data.frame(
#     class = rep(class, length(cropValues)),
#     cropType = rep(cropType, length(cropValues)),
#     cropValue = cropValues,
#     cropName = cropNames,
#     stringsAsFactors = FALSE
#   )
#   
#   if (nrow(df) == 0) {
#     stop("Unparsed", call. = F)
#   }
#   df
# }
# 
# # Define extractSpecialCropInfo function
# # This function was made after-the-fact, made specifically to parse vectors that
# # could not be parsed by extractCropInfo during the first run
# extractSpecialCropInfo <- function(text) {
#   # Extract class and cropType
#   patternMain <- "CLASS\\s*([^-]+)\\s*-\\s*(.+?)(?=:\\s*|\\s*\\(')"
#   mainMatch <- regexec(patternMain, text, perl = TRUE)
#   mainExtracted <- regmatches(text, mainMatch)[[1]]
#   
#   class <- trimws(mainExtracted[2])
#   cropType <- trimws(mainExtracted[3])
#   
#   # Extract cropValues and cropNames
#   patternCrops <- "(\\d+)\\.\\s*([^,]+)(?=,|\\.|$)"
#   cropMatches <- gregexpr(patternCrops, text, perl = TRUE)
#   cropExtracted <- regmatches(text, cropMatches)[[1]]
#   
#   if (length(cropExtracted) == 0) {
#     # Handle cases with no subclass
#     return(data.frame(
#       class = class,
#       cropType = cropType,
#       cropValue = NA,
#       cropName = "No subclass",
#       stringsAsFactors = FALSE
#     ))
#   }
#   
#   cropValues <- numeric()
#   cropNames <- character()
#   
#   for (match in cropExtracted) {
#     parts <- strsplit(match, "\\. ")[[1]]
#     cropValues <- c(cropValues, as.numeric(parts[1]))
#     cropNames <- c(cropNames, trimws(parts[2]))
#   }
#   
#   data.frame(
#     class = rep(class, length(cropValues)),
#     cropType = rep(cropType, length(cropValues)),
#     cropValue = cropValues,
#     cropName = cropNames,
#     stringsAsFactors = FALSE
#   )
# }
# 
# # Process the text and extract crop information
# processPdfPages <- function(pdfPages) {
#   allCropInfo <- data.frame()
#   
#   for (page in pdfPages) {
#     sections <- strsplit(page, "\n\n")[[1]]
#     
#     for (section in sections) {
#       cleanedSection <- trimws(gsub("\n", " ", section))
#       
#       if (grepl("^CLASS", cleanedSection)) {
#         # Try extractCropInfo first
#         cropInfo <- tryCatch(
#           extractCropInfo(cleanedSection),
#           error = function(e) NULL
#         )
#         
#         # If extractCropInfo fails, use extractSpecialCropInfo
#         if (is.null(cropInfo)) {
#           cropInfo <- extractSpecialCropInfo(cleanedSection)
#         }
#         
#         allCropInfo <- rbind(allCropInfo, cropInfo)
#         
#         if (cropInfo$class[1] == "Z") {
#           return(allCropInfo)
#         }
#       }
#     }
#   }
#   
#   return(allCropInfo)
# }
# cropInfo <- processPdfPages(pdfText)