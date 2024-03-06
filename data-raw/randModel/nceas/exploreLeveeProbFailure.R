library(dplyr)
library(ggplot2)

# First need to source the DeltaEstimator_TN.r script. Contains all the
# scenario and model set up data. The changes I made to this version are:
# inputSRDType = "DLIS", added `dbPath` to increase ease of use, and remove
# the parallelization code.

source(file.path("data-raw", "randModel", "nceas", "DeltaEstimator_TN.r"))
# Loading function to extract probability of failure
# This function is a stripped-down version from the RAND team
source(file.path("data-raw", "randModel", "nceas", "probabilityFailure.R"))

sherman <- probabilityFailure(146, db)

shermanData <- lapply(sherman, function(scenario) {
  data.frame(t(scenario[[1]][[1]]$Scenario),
             hydrologicFailure = scenario[[1]][[1]]$Probability.HydrologicFailure,
             seismicFailure = scenario[[1]][[1]]$Probability.SeismicFailure,
             leveeFailure = scenario[[1]][[1]]$Probability.LeveeFailure)
}) %>% 
  bind_rows()

# The 30 elements is based on the 30 different scenarios found here:
analysisData[['ScenarioData']]

# Discount_Rate, LF_Curve, PGAR_Curve, SR_Curve, SLR_Condition, GF_Year, Year
# Explore

# Don't really understand the difference between year and GF Year. GF Year seems to be the different climate change scenarios
distinct(shermanData, GF_Year, Year, hydrologicFailure, SLR_Condition) %>% 
  mutate(SLR_Condition = factor(SLR_Condition, levels = c("baseline", "nominal", "high"))) %>% ggplot(aes(x = Year, hydrologicFailure)) +
  geom_col(aes(fill = GF_Year)) +
  facet_wrap(~SLR_Condition) +
  labs(title = "Higher sea level rise leads to higher probability of hydrological failure")

# SR curve based on different climate change scenarios
distinct(shermanData, GF_Year, Year, SR_Curve, SLR_Condition) %>% 
  mutate(SLR_Condition = factor(SLR_Condition, levels = c("baseline", "nominal", "high"))) %>% 
  ggplot(aes(x = Year, SR_Curve)) +
  geom_col(aes(fill = GF_Year)) +
  facet_wrap(~SLR_Condition) +
  labs(title = "Stage recurrence levels associated with different sea level rise and year scenarios")

# Hydrologic failure based on SR curve and the LF Curve
distinct(shermanData, GF_Year, hydrologicFailure, SR_Curve, SLR_Condition) %>% 
  mutate(SLR_Condition = factor(SLR_Condition, levels = c("baseline", "nominal", "high"))) %>% 
  ggplot(aes(x = GF_Year, hydrologicFailure)) +
  geom_col(aes(fill = SR_Curve)) +
  facet_wrap(~SLR_Condition) +
  labs(title = "Hydrologic failure dependent on the stage recurrence curve")

distinct(analysisData$ScenarioData, LF_Curve, Levee_Fragility_Description) %>% 
  ggplot(aes(LF_Curve, Levee_Fragility_Description)) +
  geom_col() +
  labs(title = "Lower LF_Curve associated with less ")
# LF curve, levee fragility based on investment scenarios
distinct(shermanData, GF_Year, hydrologicFailure, LF_Curve, SLR_Condition) %>% 
  mutate(SLR_Condition = factor(SLR_Condition, levels = c("baseline", "nominal", "high"))) %>% 
  ggplot(aes(x = GF_Year, hydrologicFailure)) +
  geom_col(aes(fill = LF_Curve), position = position_dodge()) +
  facet_wrap(~SLR_Condition) +
  labs(title = "Hydrologic failure dependent on levee fragility curve") +
  scale_fill_manual(values = c(scales::hue_pal()(3)), 
                    labels = c("Maintenance only", "PL84-99 investment", "Urban upgrades")) +
  labs(title = "PL84-99 investment decreases hydrological failure")
# Maintenance only = 4
# PL84-99 investment = 5
# Urban upgrades = 6
# Urban upgrade and Maintenance only are equivalent in terms of LF curve (levee fragility)?

distinct(shermanData, hydrologicFailure, LF_Curve, SLR_Condition, SR_Curve) %>% 
  mutate(SLR_Condition = factor(SLR_Condition, levels = c("baseline", "nominal", "high"))) %>% 
  ggplot(aes(SR_Curve, hydrologicFailure)) +
  geom_col(aes(fill = LF_Curve), position = position_dodge(preserve = "single")) +
  facet_wrap(~SLR_Condition) +
  scale_fill_manual(values = c(scales::hue_pal()(3)), 
                    labels = c("Maintenance only", "PL84-99 investment", "Urban upgrades")) +
  labs(title = "Hydrologic failure increases with a higher SR_Curve, less investments, and higher SLR_Condition")

# Close connection to the Access db
odbcClose(db)

# Conclusion --------------------------------------------------------------

# Prob of flooding based on hydrologic failure and seismic failure per leveed area. 
# Seismic failure is constant while hydrologic failure is based on: 
# ElevNavD88 and the prob of failing per value (SR_Curve and LF_Curve)