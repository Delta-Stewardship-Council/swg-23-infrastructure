# swg-23-infrastructure
2023 synthesis working group infrastructure project

**Title:** Evaluating the social, economic, and ecological costs and benefits of levee infrastructure within the Delta.

**Issue:** Levees are critical to life in the Delta, but management decisions on funding for upkeep and improvement are not fully informed by available data.

**Goal:** Build on the [RAND model](https://www.rand.org/pubs/tools/TL266/tool.html) and create a reproducible version to inform investment strategies aimed at repairing and reinforcing our Delta levees, by:

-   Incorporating up-to-date and additional social, economic, and ecological metrics.

-   Calculating the costs and benefits of each leveed area.

-   Weighing each valuation by the probability of structural failure of each levee.

-   This creates a holistic risk model to better inform optimal investment strategies.

## Repo Organization

```
-- swg-23-infrastructure.Rproj
-- scripts
  |__terraDemonstration.R
  |__pullsEcologicalAssets.R
  |__overlappingPolygons
        |__overlappingPolygonsExploration.R
        |__manageWetlands.R
  |__leveeCorrectionScripts
        |__leveeFixRuleset.R
        |__leveeFix.R
        |__leafletPlot.R
        |__boundaryPolygon.R
-- README.md
-- R
   |__downloadCNRA.R
   |__createLeveePolygon.R
-- figures
-- LICENSE
-- doc
  |__polygonPlots.qmd
  |__workflow.qmd
-- data-raw
  |__csvFiles
        |__DLIS.IslandBasics.csv
        |__levees.csv
        |__Statewide_Terrestrial_Native_Species_Richness_Summary_-_ACE_[ds1332].csv
  |__shapefiles
        |__suisunMarsh
        |__RevisedIslands_160912_AllIslands
        |__nativeSpeciasRichness
        |__nationalLeveeDatabase
        |__habitatDelta1977
        |__deltaLeveeAreas
        |__deltaBoundary
        |__county_levees
-- data-clean
      |__shapefiles
          |__fixedLevees

```

### Download Data
1. Make sure to have all Ecological Assets. Go to  `scripts` > `downloadsEcologicalAssets.R` and run script.
2. Correct Levees shapefiles. Go to `scripts` > `leveeCorrectionsScripts` and run:
- `leveeFixRules.R` --> explores the levee centerlines; creates `fixedLevees` > `leveedAreas.shp`
- `leveeFix.R` --> explores the levee centerlines; creates `fixedLevees` > `fixedLevees.shp`
- `boundaryPolygons.R`
- `leafletPlot.R`

### Data Correction

### 




