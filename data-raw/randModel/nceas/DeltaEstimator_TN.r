#This program is used to estimate different metrics needed as part of the Delta study
#This adapts models built by


###################################################################################
#    START:CHECK THESE THREE VARIABLES-CRUCIAL TO ENSURING ANALYSIS IS CORRECT    #
###################################################################################

#mode set; set varInitializeOnlyQ to FALSE to run the analysis (default); set to TRUE to only load analysis data variables
varInitializeOnlyQ = FALSE
#type of stage recurrence domain? set to "DLIS" for standard analysis -- set to MaxMin for other
inputSRDType = "DLIS"
#include base sheets in output? (normally set to TRUE)
includeExternalSheets = FALSE
#include date in the file name?
inclDateFileName = FALSE
#set database to connect to 
odbcChannelName = "DriverDeltaSuisun"
# File locations
dbPath <- "C:\\Users\\TXNguyen\\Documents\\GitHub\\swg-23-infrastructure\\data-raw\\randModel\\DriverDeltaSuisun.accdb"
##########################
#    END:VITAL CHECKS    #
##########################




#################################################################
#                   Begin Initialization                        #
#################################################################

#build directories
#----------------------------------------------------------------
dir.self = file.path("data-raw", "randModel")
deltald = file.path("data-raw", "randModel", "DeltaEstimator.r")

dir.bin = file.path(dir.self, 'bin')
dir.ref = file.path(dir.self, 'ref')
dir.core = file.path(dir.self, 'core')
dir.out = file.path(dir.self, 'out')

#check for out directory; if it does not exist, create
if (!dir.exists(dir.out)) {
    #create output directory
    dir.create(dir.out)
}

#check for out directory; if it does not exist, create
if (
    (!dir.exists(dir.bin)) || 
    (!dir.exists(dir.ref)) ||
    (!dir.exists(dir.core))
    ) {
    #warning
    warning(
        "One or more critical subdirectories (bin, core, ref) not found. Analysis will not proceed."
    )
    #stop
    stop()
}


#clear analysisData
if('analysisData' %in% ls()) {
    rm(analysisData)
}
#----------------------------------------------------------------


#load relevant components
#----------------------------------------------------------------

#...................optional analysis add-ons....................

#### NOT SUPPORTED; DO NOT CHANGE

inclAnalysisQ = list()
#add analysis types here
inclAnalysisTypes = c()

#include pl84 sheets?
inclPL84SheetsQ = FALSE

#initialize analysis queries here

#pga multi-island failure analyses
inclAnalysisQ[['PGA_MIF']] = FALSE
inclAnalysisQ[['SLR_MIF']] = FALSE
#................................................................



#load in bin code
lapply(
    file.path(dir.bin, dir(dir.bin)),
    function(h) {
        if (!grepl('Post_AD_Load', h)) {
            source(h)
        }
    }
)

#set options
options(stringsAsFactors = FALSE)


#check for library functions

#pacakges that are needed
needPacks = c("RODBC", "parallel")
#check for installed packages
checkPackages(needPacks)
#initiate database connection
#db = odbcConnect(odbcChannelName)
db <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", dbPath, ";"))

#----------------------------------------------------------------


###------------------------------------------------------###
#              global variable initialization              #
###------------------------------------------------------###


#.....................analysisData core list......................

#initialize
if (!("analysisData" %in% ls())) {
    analysisData = list()
}
#stage recurrence type
analysisData[["StageRecurrenceDomainType"]] = inputSRDType

#set stage recurrence table
analysisData[['Stage_Recurrence_Table']] = 'Stage_Recurrence'
#get all stage recurrence scenarios
analysisData[['Scenarios.StageRecurrence']] = unname(unlist(sqlQuery(db, paste('select distinct Scenario from ', analysisData[['Stage_Recurrence_Table']], sep = ''))))
#get all levee fragility scenarios
analysisData[['Scenarios.LeveeFragility']] = unname(unlist(sqlQuery(db, "select distinct Scenario from FragilityCurveHydro")))
#get all valid seismic fragility scenarios
analysisData[['Scenarios.SeismicFragility']] = unname(unlist(sqlQuery(db, "select distinct Scenario from FragilityCurveSeismic")))
#get all levee fragility scenarios
analysisData[['Scenarios.PGARecurrence']] = unname(unlist(sqlQuery(db, "select distinct Scenario from pga_Recurrence")))
#load in information about the scenarios
analysisData[['ScenarioData']] = readRef('ScenarioData')
#get base year
analysisData[["BaseYear"]] = min(analysisData[["ScenarioData"]][analysisData[["ScenarioData"]][ , "SLR_Condition"] == "baseline", "GF_Year"])
#build temporary matrix mapping scenarios to fields in ScenarioData; ssdfmMatrix -> scenario2ScenDatFieldMapMatrix
ssdfmMatrix = matrix(
    c(
        'StageRecurrence','SR_Curve',
        'PGARecurrence','PGAR_Curve',
        'LeveeFragility','LF_Curve',
        'SeismicFragility','SF_Curve'
    ),
    ncol = 2,
    byrow = TRUE
)

#initialize w0
w0 = which(analysisData[['ScenarioData']][[ssdfmMatrix[1,2]]] %in% analysisData[[paste('Scenarios', ssdfmMatrix[1,1], sep = '.')]])

#loop over all rows in ssdfmMatrix to check validity of entries in ScenarioData against availability in the database
for (sRow in 1:nrow(ssdfmMatrix)) {
    #get temporary index of all valid scenarios with 
    w0 = intersect(
        #valid stage recurrence curve indices
        w0,
        which(analysisData[['ScenarioData']][[ssdfmMatrix[sRow,2]]] %in% analysisData[[paste('Scenarios', ssdfmMatrix[sRow,1], sep = '.')]])
    )
}
#check to make sure each scenario included in the analysis is also defined in the read in file
analysisData[['Scenarios']] = analysisData[['ScenarioData']][['Scenario']][w0]

#find any values that may not be defined
w0 = which(!(analysisData[['ScenarioData']][['Scenario']] %in% analysisData[['Scenarios']]))
#print out errors
for (i0 in w0) {
    print(paste('Scenario ', analysisData[['ScenarioData']][['Scenario']][i0],' was excluded; scenario data (SR Curve or LF Curve) is missing or invalid. Check ScenarioData.dat in the /ref subdirectory to ensure that data is included.',sep = ''))
}

#stop execution if all data is missing
if (length(w0) == length(analysisData[['Scenarios']] )) {
    warning('Error: no valid scenarios were found. Please check to ensure scenarios are included in the ScenarioData.dat file.')
    stop()
}

#source in functions include "Post_AD_load"
#load in bin code
lapply(
    file.path(dir.bin, dir(dir.bin)),
    function(h) {
        if (grepl('Post_AD_Load', h)) {
            source(h)
        }
    }
)

#get island ids included in the analysis
#analysisData[['IslandIDs']] = unname(unlist(sqlQuery(db,'select distinct ID_Number from Island_Tract where Water_Flooded = 0 and FragilityCurve = 1')))
analysisData[['IslandIDs']] = unname(unlist(sqlQuery(db,'select distinct ID_Number from Island_Tract where Water_Flooded = 0')))# and Levees = 1
#get island ids with no fragility curve, but that aren't flooded
#analysisData[['IslandIDs_NoLF']] = unname(unlist(sqlQuery(db,'select distinct ID_Number from Island_Tract where Water_Flooded = 0 and FragilityCurve = 0')))
#check for necessary fragility curves for scenario data
necLFCurves = unique(analysisData[['ScenarioData']][["LF_Curve"]])
#get ids that have all lf curves
idsWithAllLF = analysisData[['IslandIDs']]
#check accross island ids
for (lfc in necLFCurves) {
    #get temporary ext
    tempTab = sqlQuery(db, paste("select distinct ID_Number from FragilityCurveHydro where Scenario =", lfc))
    #reduce
    idsWithAllLF = intersect(idsWithAllLF, tempTab[, "ID_Number"])
}
#set those ids that have no LF
analysisData[['IslandIDs_NoLF']] = setdiff(analysisData[['IslandIDs']], idsWithAllLF)

#get water supply islands
analysisData[['IslandIDs_WaterSupply']] = c(2, 3, 10, 13, 18, 29, 32, 97, 98, 102, 104, 115, 118, 120, 130, 146, 157, 160, 162, 166, 169, 174)
#query to remove nolf
analysisData[['RemoveNOLFQ']] = TRUE
#baseline LF curve
analysisData[['LFCurve.BaselineIndex']] = 4
#FLAG.TEMPORARY: fix to ignore island 32 while it is figured out
#analysisData[['IslandIDs']] = analysisData[['IslandIDs']][!(analysisData[['IslandIDs']] %in% c(21, 30, 64))]
#give ids that were excluded from the analysis
analysisData[['NonAnalysisIslandIDs']] = setdiff(
    unname(unlist(sqlQuery(db,"select distinct ID_Number from Island_Tract"))),
    analysisData[['IslandIDs']]
)
#binary code indicating whether or not the island is leveed
analysisData[['LeveeQ']] = unlist(
    lapply(
        analysisData[['IslandIDs']],
        function (h) {
            unname(
                unlist(
                    sqlQuery(db,paste('select Levees from Island_Tract where ID_Number =', h))
                )
            )
        }
    )
)


#first 8: c(1,2,3,5,6,7,8,9)#
analysisData[['AssetCodes']] = sqlFetch(db,'AssetCode')

#get relevant damage curve indices
analysisData[['DamageCurves']] = sort(unique(analysisData[['AssetCodes']][['AssetDamageCurve']]))
#remove non-indexed damage curve values
analysisData[['DamageCurves']] = analysisData[['DamageCurves']][which(!is.element(analysisData[['DamageCurves']],c(0,NA)))]
#get proportion warned table
analysisData[['ProportionWarnedCurve']] = readRef('ProportionWarned')
#proportion willing to help
analysisData[['HelpProportion.Willing']] = 0.95
#proportion capable of helping
analysisData[['HelpProportion.Capable']] = 0.90
#these give the domain of x values to interpolate over for each parameter desired; not that doRange = function(minRange = min(dsr[,1]), maxRange = max(dsr[,2]), interval = 0.5) has the default values shown
analysisData[['Interval.StageRecurrence']] = 0.5
analysisData[['Domain.StageRecurrence']] = unlist(unname(sqlQuery(db, paste('select distinct Stage_ft from ', analysisData[['Stage_Recurrence_Table']], sep = ''))))#doRange()
#FLAG: these are temporary fixes
analysisData[['Domain.SeismicFragility']] = unname(unlist(sqlQuery(db,'select distinct pga from FragilityCurveSeismic')))
#pga domain
analysisData[['Domain.PGARecurrence']] = unname(unlist(sqlQuery(db,'select distinct pga from pga_Recurrence where Scenario = 2')))
#give metrics that receive separate files
analysisData[['SeparateMetrics']] = c()
#set assets to get assetData for; this is a list of asset names
analysisData[['assetDataAssets']] = 'all'
#c('Gas Well Active', 'Tree or Vine Crop', 'Field Crop', 'Residential Structure', 'Commercial Structure', 'Roads All', 'Oil Pipeline', 'Nat Gas Pipeline', 'Public School')
#asset data extractions
if (analysisData[['assetDataAssets']] %in% c('all','All','ALL')) {
    tempADEVec = sapply(
        unique(analysisData[['AssetCodes']][['AssetName']]),
        as.character
    )
} else {
    tempADEVec = sort(analysisData[['assetDataAssets']])
}
#get asset data extractions
analysisData[['assetDataExtractions']] = paste(
    'AssetData',
    gsub(
        ' ',
        '_',
        tempADEVec,
        fixed = TRUE
    ),
    sep = '.'
)
#get those assets that are included in the EAD calculation
analysisData[['EADAssets']] = readRef('EAD_Asset_Names')
#which assets are included in assets at risk
analysisData[['assetsAtRiskAssets']] = 'all'
#c('Gas Well Active', 'Tree or Vine Crop', 'Field Crop', 'Residential Structure', 'Commercial Structure', 'Roads All', 'Oil Pipeline', 'Nat Gas Pipeline', 'Public School')
#asset data extractions
if (analysisData[['assetsAtRiskAssets']] %in% c('all','All','ALL')) {
    tempAVEVec = sapply(
        unique(analysisData[['AssetCodes']][['AssetName']]),
        as.character
    )
} else {
    tempAVEVec = sort(analysisData[['assetsAtRiskAssets']])
}
#get asset data extractions
analysisData[['assetsAtRiskExtractions']] = paste(
    'AssetsAtRisk',
    gsub(
        ' ',
        '_',
        tempAVEVec,
        fixed = TRUE
    ),
    sep = '.'
)
#clear temporary vectors
rm(tempADEVec)
rm(tempAVEVec)

#give metrics that will show up in the standard metric file
analysisData[['StandardMetrics']] = c(
    'Probability.HydrologicFailure',
    'Probability.SeismicFailure',
    'Probability.LeveeFailure',
    'Expected.AnnualDamage',
    paste(
        'Expected.AnnualDamage',
        gsub(' ', '',
            as.character(
                unique(
                    analysisData[['AssetCodes']][['AssetClass']]
                )
            )
        ),
        sep = ''
    ),
    #add assets at risk
    'AssetsAtRisk',
    #assets at risk aggregated to class
    paste(
        'AssetsAtRisk',
        gsub(' ', '',
            as.character(
                    unique(
                    analysisData[['AssetCodes']][['AssetClass']]
                )
            )
        ),
        sep = ''
    ),
    #assets at risk by type
    analysisData[['assetsAtRiskExtractions']],
    #add asset values used in conjunction with damage curves
    analysisData[['assetDataExtractions']],
    'PopulationAtRisk',
    'Expected.AnnualAgriculturalLandLoss',
    'Expected.AnnualFatalities',
    #set to zero if WSDRS is missing for now (7-18-16)
    'RecoveryCost',
    'Expected.AnnualDamageRecoveryCost',
    'Expected.FloodingLeveedHabitat',
    'LeveedHabitat'
)

#insert metrics
analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']],analysisData[['SeparateMetrics']]))
#insert curves to extract
analysisData[['CurvesOut']] = c('FN','FD')
#Set matrices to export
analysisData[['ExportMatrices']] = c(
    c(
        'StandardMetrics', #standard set of metrics
        'BaselineCompMetrics', #metrics including levee failure risk, EAD, and EAF for each island/scenario with LF curve = analysisData[['LFCurve.BaselineIndex']]
        'MetricsAtRisk', #static data which includes assets at risk for each island as well as the counts/raw data used to quantify assets at risk
        'MetricsAtRiskLong', #long form of MetricsAtRisk
        'MAREADLong', #Metrics At Risk aggregated for only assets contributing to EAD
        'CurvesOut' #FN/FD curves
    ),
    paste(
        'PL8499-',
        c(
            'EAD', #effects of PL84-99 levee investments on EAD
            'EAF', #effects of PL84-99 levee investments on EAF
            'Flooding', #effects of PL84-99 levee investments on flooding
            'Ag', #effects of PL84-99 levee investments on agriculture,
            'Habitat' #effects on habitat
        ),
        '_effects',
        sep = ''
    )
)
#FLAG:EXPORT Set export format
analysisData[['ExportType']] = 'csv'
#insert time steps
analysisData[['Times']] = 1:1
#damage curves that are on a per unit basis
analysisData[['DamageCurveDamagePerUnitIndices']] = c(5,6,7,8)
#asset codes that are treated differently; they represent values that are not treated probabilistically and instead are treated as constant values
analysisData[['AssetCodes.RealPropertyAcreage']] = 46:50
#set asset codes that are used to calculate Expected Annual Agricultural Land Loss (EAALL)
analysisData[['AssetCodes.EAALL']] = c(47,50)
#asset value headers for standard metrics sheet
analysisData[['AssetDataHeaders']] = unname(
    unlist(
        sapply(
            analysisData[['assetDataExtractions']],
            function(h) {
                convertAssetValueToHeader(h, analysisData[['AssetCodes']], analysisData[['DamageCurveDamagePerUnitIndices']])
            }
        )
    )
);

#set optional analysis variable names
analysisData[['OptionalMIFAnalyses']] = c('SLR_MIF', 'PGA_MIF')
#set optional MIF export name
analysisData[['MIFAnalysisExportName']] = 'MIF_Analysis_Data'
#add argument to account for Hollie's experiments in DB
analysisData[['pgaSQLArgument']] = 'Probability < 1 and pga > 0.0001'

#.................................................................




#...................PL84-99 Sheet Variables.......................

#pl84-99 island ids
#analysisData[['pl8499_island_ids']] = readRef("Islands_PL84-99")[ , "ID_Number"]
#baseline curve
analysisData[['baseline_lf_scenario']] = 4
#pl84-99 island ids
analysisData[['pl8499_lf_scenario']] = 5

#.................................................................




#.....................Curve Out Variables.........................

#gives the coefficient for number of unique ranges, so twice this number is the number of unique vertex ids
numUniqueCurveRange = 1

#.................................................................




#......................Indexing Variables.........................

#give number of overall scenarios
nS = length(analysisData[['Scenarios']])
##levee fragility scenarios are correlated with scenarios; this kluge leaves the number at 1 , which effectively eliminates the outer product. Any references (outside of indexing) are mapped to convertScenario(analysisData[['Scenarios']][jSR], typeOut = 'lftype'); later, nLF can be mapped to analysisData[['Scenarios.LeveeFragility']] to utilize the outter product
nLF = 1#length(analysisData[['Scenarios.LeveeFragility']])
nT = 1#length(analysisData[['Times']])

#.................................................................




#................Expected.AnnualDamages Variables.................

#recovery cost variables

#cost per acre foot
analysisData[['RecoveryCostPerAcreFoot']] = 420

#additional flat fee
analysisData[['RecoveryCostAddon']] = 25000000
#.................................................................




#..............Expected.AnnualFatalaties Variables................

#mortality function has a domain or inundation depth
mortalityFunction = function(id) {
    if (id <= 0) {
        0
    } else {
        #jonkman mortality mean
        jmm = 5.2
        #function output
        pnorm((log(id/3.2808) - jmm)/2)
    }
}

#population variance? variable set on EAF cell $P$2 from Hollie's workbook
analysisData[["PopVar"]] = 1
#proportion warned has a function of time
proprtionWarned = function(t) {
    1
}

#number of parameter matrix header fields
pmNHeaderFields = 6

#.................................................................




#...................ProjectedHabitat Variables....................

#include?
analysisData[["IncludeProjectedHabitatDataQ"]] = FALSE

#load in data if desired
if (analysisData[["IncludeProjectedHabitatDataQ"]]) {
    #data used for projected habitat
    analysisData[['HabitatData']] = readRef('NonDBMetricData.PH')
    #define number of columns in matrices
    allHabitatTypes = sort(unique(analysisData[['HabitatData']][,which(names(analysisData[['HabitatData']])=='DLIS.Habitat.Categories')]))
    #number of habitats
    nHabitatTypes = length(allHabitatTypes)
    #add the expected annual change in these habitat types to the list
    analysisData[['StandardMetrics']] = c(
        analysisData[['StandardMetrics']],
        paste('Expected.AnnualChangeHabitat', gsub(' ','',allHabitatTypes[allHabitatTypes != ' ']), sep = '')
    )
    #add to separate matrix
    analysisData[['SeparateMetrics']] = union(analysisData[['SeparateMetrics']], 'ProjectedHabitat')
    #re-load metrics
    analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
    #add to export matrices
    analysisData[['ExportMatrices']] = union(analysisData[['ExportMatrices']], 'ProjectedHabitat')
} else {
    nHabitatTypes = 0
}
#.................................................................




#....................Expected.WSDRS Variables.....................
#WSDRS = Water Supply Distruption Risk Score

#include in standard metrics?
analysisData[['IncludeWSDRSDataQ']] = FALSE

#load data if included
if (analysisData[['IncludeWSDRSDataQ']]) {
    #load in wsdrs table data
    analysisData[['WSDRSData']] = readRef('NonDBMetricData.WSDRS')
    #generate logit function
    wsdrsDistruption = function (x) {
        L = 1
        k = 0.0000469638044494324
        x0 = 145919.463500256
        
        L/(1+exp(-k*(x - x0)))
    }

    #get headers for printout
    analysisData[['WSDRSHeadersOut']] = unlist(
        lapply(
            names(analysisData[['WSDRSData']])[
                which(
                    grepl(analysisData[['Scenarios.StageRecurrence']][1],
                    names(analysisData[['WSDRSData']])
                )
            )],
            function(h) {
                unlist(strsplit(h, '.', fixed = TRUE))[1]
            }
        )
    )
    'Expected.WSDRS'
    
    #add in expected damage metrics
    analysisData[['StandardMetrics']] = c(analysisData[['StandardMetrics']], 'Expected.WSDRS')

    #re-load metrics
    analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
}
#.................................................................




#.........................EACH Variables..........................

#include EACH?
analysisData[['IncludeEACHDataQ']] = FALSE

#include data if desired
if (analysisData[['IncludeEACHDataQ']]) {
    #data used for projected habitat;
    analysisData[['EACHData']] = readRef('NonDBMetricData.FloodedHabitat')
    #take only relevant columns
    analysisData[['EACHData']] = analysisData[['EACHData']][
        c(
            'DLIS_ID',
            'DLIS.Habitat.Categories',
            sort(
                names(analysisData[['EACHData']])[grepl("SRSCEN.", names(analysisData[['EACHData']]), fixed = TRUE)]
            )
        )
    ]
    #define number of columns in matrices
    analysisData[['allEACHTypes']] = sort(unique(analysisData[['EACHData']][,which(names(analysisData[['EACHData']])=='DLIS.Habitat.Categories')]))
    #number of habitats
    nEACHTypes = length(analysisData[['allEACHTypes']])
    #add the expected annual change in habitat to the standard metrics list
    analysisData[['StandardMetrics']] = c(
        analysisData[['StandardMetrics']],
        paste('Expected.AnnualChangeHabitat', gsub(' ', '', analysisData[['allEACHTypes']]), sep = '')
    )
    #re-load metrics
    analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
}
#.................................................................




#.....................Expected Island Impact......................

#include?
analysisData[['IncludeHighValueHabitatDataQ']] = FALSE

#include data if desired
if (analysisData[['IncludeHighValueHabitatDataQ']]) {
    #load
    analysisData[['HighValueHabitat']] = readRef('NonDBMetricData.HighValueHabitat')

    #add in extpected damage metrics
    analysisData[['StandardMetrics']] = c(analysisData[['StandardMetrics']], 'Expected.IslandImpacts')
    
    #re-load metrics
    analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
}

#.................................................................



#..................Potential Unleveed Habitat.....................
#query on whether or not to include potential unleveed habitat; this will be read in do.Extract.r
analysisData[['InlcudePUHDataQ']] = TRUE

if(analysisData[['InlcudePUHDataQ']] ) {
    #included long form table
    analysisData[['PUHData']] = sqlQuery(db, "select * from PotentialHabitat")#readRef('NonDBMetricData.PotentialUnleveedHabitat')
    
    #add the potential unleveed habitat to the standard metrics list
    analysisData[['StandardMetrics']] = c(
        analysisData[['StandardMetrics']],
        paste('PotentialUnleveedHabitat', "By_Elevation_InPlans", sep = '.')
        #paste('PotentialUnleveedHabitat', unique(analysisData[['PUHData']][,'by_elev_type']), sep = '.')
    )
    
    #re-load metrics
    analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
}
#.................................................................




#...............Optional PGA_MIF Analyses Variable................

if (inclAnalysisQ[['PGA_MIF']]) {

    #set year events to analyze
    analysisData[['pgaAnalysis.YearEvents']] = c(10, 25, 50, 100, 200, 500, 1000)
    
    #get pgas using mean curve
    analysisData[['pgaAnalysis.YearEventPGAs']] = unname(
        unlist(
            lapply(
                analysisData[['pgaAnalysis.YearEvents']], 
                pgaOfEvent
            )
        )
    )
    
    #determine whether or not mean curve should be used for each island (if False, individual island pga recurrence curves will be used)
    analysisData[['pgaAnalysis.MeanPGARQ']] = FALSE
    
    #initialize fields list
    analysisData[['pgaAnalysis.StandardMetricFields']] = list()
    
    #loop to add extraction metrics
    for (ye in 1:length(analysisData[['pgaAnalysis.YearEvents']])) {
    
        #set fields for year event ye
        analysisData[['pgaAnalysis.StandardMetricFields']][[as.character(ye)]] = c(
            paste('ExpectedDamage.', analysisData[['pgaAnalysis.YearEvents']][ye], 'YearPGA', sep = ''),
            paste('ExpectedFatalities.', analysisData[['pgaAnalysis.YearEvents']][ye], 'YearPGA', sep = ''),
            paste('Probability.LeveeFailure.', analysisData[['pgaAnalysis.YearEvents']][ye], 'YearPGA', sep = '')
        )
    
        #add in extpected damage metrics
        analysisData[['StandardMetrics']] = c(
            analysisData[['StandardMetrics']],
            analysisData[['pgaAnalysis.StandardMetricFields']][[as.character(ye)]]
        )
        
        #re-load metrics
        analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']],analysisData[['SeparateMetrics']]))
    }
}

#.................................................................




#...............Optional SLR_MIF Analyses Variable................

if (inclAnalysisQ[['SLR_MIF']]) {
    
    #set events
    analysisData[['slrAnalysis.YearEvents']] = c(10, 25, 50, 100, 200, 500, 1000)
    
    #set event headers
    analysisData[['slrAnalysis.StandardMetricFields']] = list()
    
    #loop to add extraction metrics
    for (ye in 1:length(analysisData[['slrAnalysis.YearEvents']])) {
        
        #set fields for year event ye
        analysisData[['slrAnalysis.StandardMetricFields']][[as.character(ye)]] = c(
            paste('ExpectedDamage.', analysisData[['slrAnalysis.YearEvents']][ye], 'YearSLR', sep = ''),
            paste('ExpectedFatalities.', analysisData[['slrAnalysis.YearEvents']][ye], 'YearSLR', sep = ''),
            paste('Probability.LeveeFailure.', analysisData[['slrAnalysis.YearEvents']][ye], 'YearSLR', sep = '')
        )
        
        #add in extpected damage metrics
        analysisData[['StandardMetrics']] = c(
            analysisData[['StandardMetrics']],
            analysisData[['slrAnalysis.StandardMetricFields']][[as.character(ye)]]
        )
        
        #re-load metrics
        analysisData[['Metrics']] = sort(union(analysisData[['StandardMetrics']], analysisData[['SeparateMetrics']]))
    }
}

#.................................................................




#Generate the header for the standard metrics matrix out
#.................................................................
analysisData[['StandardMetricsHeader']] = unname(
    unlist(
        sapply(
            analysisData[['StandardMetrics']],
            function(h) {
                #convert the extraction name to the asset name
                tempStr = convertAssetExtractionDataToName(h)
                #next, convert the asset name to the header name
                tempStr = convertAssetValueToHeader(tempStr, analysisData[['AssetCodes']], analysisData[['DamageCurveDamagePerUnitIndices']])
                #final header value out
                paste('Metric', tempStr, sep = '.')
            }
        )
    )
)
#.................................................................




#Read in leveed status
#.................................................................
#get levee status
analysisData[['Leveed_Code']] = readRef("Island_Leveed_Status")
#function for returning leveed code
getLeveeCode = function(islandID, type = "integer") {
    #condition based on type
    if (type == "integer") {
        #header to extract
        extField = "Leveed_Code"
    } else if (type == "string") {
        #get string field
        extField = "Leveed"
    }
    #check for membership
    if (islandID %in% analysisData[['Leveed_Code']][ , "ID_Number"]) {
        #index
        out = analysisData[['Leveed_Code']][analysisData[['Leveed_Code']][ , "ID_Number"] == islandID, extField]
        #convert if appropriate
        if (type == "integer") {
            #integer
            out = as.integer(out)
        }
    } else {
        #otherwise, pull from db
        out = as.numeric(sqlQuery(db, paste('select Levees from Island_Tract where ID_Number =', islandID)))
        #convert if desired
        if (type == "string") {
            #convert
            if (out == 1) {
                out = "Leveed"
            } else if (out == 0) {
                out == "Unleveed"
            }
        }
    }
    #return
    return(out)
}
#.................................................................

#----------------------------------------------------------------


###------------------------------------------------------###
#               other variable initialization              #
###------------------------------------------------------###


#...............Include Metrics from Island Tract.................

#include asset data from island tracts?
analysisData[["load_it_metrics"]] = FALSE
#assets
analysisData[["load_it_assets"]] = c("Roads_All", "Roads_LC", "Roads_SFS")
#include asset data
analysisData[["load_it_AssetData"]] = paste("AssetData", analysisData[["load_it_assets"]], sep = ".")
#include asset data it name
analysisData[["load_it_AssetData_it_names"]] = paste(analysisData[["load_it_assets"]], "mile", sep = "_")
#add names
analysisData[["load_it_AssetData_names"]] = unname(
    unlist(
        lapply(
            analysisData[["load_it_assets"]],
            function (h) {
                #prepend and append
                newName = paste(
                    "Metric.AssetData",
                    h,
                    "Enumeration:miles",
                    sep = "."
                )
                #return value
                return(newName)
            }

        )
    )
)

#include risk metrics
analysisData[["load_it_AssetsAtRisk"]] = paste("AssetsAtRisk", analysisData[["load_it_assets"]], sep = ".")

#include asset at risk it name
analysisData[["load_it_AssetsAtRisk_it_names"]] = paste(analysisData[["load_it_assets"]], "value", sep = "_")
#add matrix component
analysisData[["load_it_AssetsAtRisk_names"]] = unname(
    unlist(
        lapply(
            analysisData[["load_it_assets"]],
            function (h) {
                #prepend and append
                newName = paste(
                    "Metric.AssetsAtRisk",
                    h,
                    sep = "."
                )
                #return name
                return(newName)
            }
        )
    )
)

#update standard metrics
analysisData[['StandardMetrics']] = c(
    analysisData[['StandardMetrics']],
    analysisData[["load_it_AssetData"]],
    analysisData[["load_it_AssetsAtRisk"]]
)

#update standard metrics header
analysisData[['StandardMetricsHeader']] = c(
    analysisData[['StandardMetricsHeader']],
    analysisData[["load_it_AssetData_names"]],
    analysisData[["load_it_AssetsAtRisk_names"]]
)

#re-load metrics
analysisData[['Metrics']] = sort(union(analysisData[['Metrics']], analysisData[['StandardMetrics']]))

#.................................................................



#............Set asset valuation growth information...............

#Use econFactor from db for asset growth?
analysisData[["UseDBEconFactors"]] = FALSE

#annual rate of growth
analysisData[["AnnualAssetValueGrowth"]] = 0.02
#.................................................................



#....................Generated File Variables.....................
#initialize list
filename.out = list()
#loop to add name
for (exp in analysisData[['ExportMatrices']]) {
    filename.out[[exp]] = paste('DLIS', exp, 'csv',sep = '.')
}
#get those names to keep
wKeep = intersect(
    which(!grepl("PL84", analysisData[['ExportMatrices']])),
    which(analysisData[['ExportMatrices']] != "CurvesOut")
)

#reduce export matrices
analysisData[['ExportMatrices']] = analysisData[['ExportMatrices']][wKeep]

#update file name for master scenario key
filename.out[["Master_scenario_key"]] = "DLIS.MasterScenarioKey.csv"


#.................................................................



# #.............Load Core Function to be Run on Slaves..............
# #core model calculations
# source(file.path(dir.core, 'dm0.r'))
# #slave node shell function (includes checks against UA/BA)
# source(file.path(dir.core, 'doModels.r'))
# #.................................................................
# 
# 
# 
# #part of testing; if varInitializeOnlyQ is set, then only analysis data variables will be initialized
# #----------------------------------------------------------------
# if (varInitializeOnlyQ) {
#     stop()
# }
# #----------------------------------------------------------------
# 
# # Load required libraries for parallel processing
# if (!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
# library(foreach)
# 
# if (!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
# library(doParallel)
# 
# # Initialize parallelization
# numSlaves = 2  # Set to 2 to run in a quasi-single-threaded mode
# cl = makeCluster(numSlaves)
# registerDoParallel(cl)
# 
# # Notify user of stage
# print('######  Initiating model calculations...  ######')
# 
# # Main model execution
# # Make sure exec.ParRuns.r contains the appropriate parallel processing structure
# source(file.path(dir.core, 'exec.ParRuns.r'))
# print('Done.')
# 
# # Notify user of next stage
# print('######  Starting export matrix generation...  ######')
# 
# # Export matrix generation
# # Make sure do.Extract.r is compatible with the parallel setup
# #source(file.path(dir.core, 'do.Extract.r'))
# source("C:\\Users\\abathulla\\Desktop\\Delta_Estimator_Win32\\core\\do.Extract.r")
# 
# print('Done.')

# Ensure the ODBC connection is closed
# odbcClose(db)

# # Kill the cluster
# stopCluster(cl)
