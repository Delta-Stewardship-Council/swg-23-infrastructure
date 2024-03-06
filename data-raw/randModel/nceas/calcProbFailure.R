# This script will aggregate the probability of failure for all leveed areas 
# within the RAND model that has data

# Libraries ---------------------------------------------------------------

library(dplyr)

# Load analysis and model parameters --------------------------------------

source(file.path("data-raw", "randModel", "nceas", "DeltaEstimator_TN.r"))
# Loading function to extract probability of failure
# This function is a stripped-down version from the RAND team
source(file.path("data-raw", "randModel", "nceas", "probabilityFailure.R"))

# Calculating prob of failure across all surveys --------------------------

# Remove all flooded (and unleveed areas?)
islands <- filter(analysisData$Leveed_Code, Leveed == "Leveed")

probFail <- lapply(islands$ID_Number,
                   function(island) {
                    
                     cat(island, filter(islands, ID_Number == island)["Island.or.Tract"][[1]], "\n")
                     data <- probabilityFailure(island, db)
                     
                     islandData <- data.frame(
                       islandID = island,
                       islandName = filter(analysisData$Leveed_Code, ID_Number == island)["Island.or.Tract"]
                     )
                     
                     finalData <- lapply(data, function(scenario) {
                       data.frame(t(scenario[[1]][[1]]$Scenario),
                                  hydrologicFailure = scenario[[1]][[1]]$Probability.HydrologicFailure,
                                  seismicFailure = scenario[[1]][[1]]$Probability.SeismicFailure,
                                  leveeFailure = scenario[[1]][[1]]$Probability.LeveeFailure)
                     }) %>% 
                       bind_rows() %>% 
                       {bind_cols(islandData, .)}
                   }
) %>% 
  bind_rows()

# Writing to a flat file --------------------------------------------------

write.csv(probFail, file = file.path("data", "randProbFailure", "probFailure.csv"), row.names = F)

# Close connection to the Access db
odbcClose(db)
