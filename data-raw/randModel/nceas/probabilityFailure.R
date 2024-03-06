#' Calculate probability of levee failure per the RAND model
#' 
#' @description
#' This is a stripped-down version of the `dm0.r` file from the RAND model that
#' calculates only the seismic and hydrologic failure probability of a leveed
#' area. Asset values calculations were removed.
#' 
#' @param islandID Numeric ID of levee of interest. Check `analysisData$Leveed_Code`
#' @param dbase RODBC connection to the `DriverDeltaSuisun.accdb`
#'
#' @return A list of 30 elements of various information including the probability
#' of failure. Each element represents a scenario, which can be checked via
#' `analysisData$ScenarioData`.
#'
#' @examples
#' shermanIsland <- probabilityFailure(146, db)
probabilityFailure <- function(islandID, dbase) {
  # Extract information specific to the island tract
  islandInfo = sqlQuery(dbase,paste('select * from Island_Tract where ID_Number = ', islandID, sep = ''))
  
  # Seismic information of the island
  seismicClass = islandInfo[which(names(islandInfo)=='SiteVulnSeismicMin')]#islandInfo[which(names(islandInfo)=='SiteVulnSeismic')]
  #numeric
  seismicClass = as.numeric(seismicClass)
  
  # Length factor
  # used to calculate the seismic fragility later
  if (islandInfo[['Length_Factor']] < 0.1 || is.na(islandInfo[['Length_Factor']])) {
    lengthFactor = 1
  } else {
    lengthFactor = islandInfo[['Length_Factor']]
  }
  
  # Begin populating the model domains, i.e., range of possible values
  modelData = list()
  modelData[['Domain.SeismicFragility']] = analysisData[['Domain.SeismicFragility']]
  modelData[['Domain.PGARecurrence']] = analysisData[['Domain.PGARecurrence']]
  
  #09/09/2016 - OVERRIDE; NOW USE 0.5 FT FOR ALL (from code)
  modelData[['Interval.StageRecurrence']] = 0.5
  
  #build stage recurrence domain
  modelData[['Domain.StageRecurrence']] = unlist(
    lapply(
      analysisData[['Domain.StageRecurrence']],
      function (h) {
        if(
          h >= roundSpecial(
            max(
              islandInfo[['AvgFloorElevation_ft']],
              islandInfo[['dblMinStage_ft']]
            ),
            modelData[['Interval.StageRecurrence']],
            1
          ) &&
          h <= (
            roundSpecial(
              islandInfo[['dblMaxStage_ft']],
              modelData[['Interval.StageRecurrence']],
              2
            ) + modelData[['Interval.StageRecurrence']]) && (
              (h %% modelData[['Interval.StageRecurrence']]) == 0
            )
        ) {
          h
        } else {
          NULL
        }
      }
    )
  )
  
  # I'm not too familiar with this part yet.
  #ADDED 5/8/17 -- ALLOWS USER TO OVERRIDE STAGE RECURRENCE DOMAIN TO USE ALL STAGES THAT HAVE NON-0 PROBABILITY ACROSS ALL SCENARIOS
  if (analysisData[["StageRecurrenceDomainType"]] != "DLIS") {
    modelData[['Domain.StageRecurrence']] = unname(
      unlist(
        sqlQuery(
          dbase, 
          paste("select distinct Stage_ft from Stage_Recurrence where ID_Number =", islandID, "and Probability > 0")
        )
      )
    )
  }
  #notify user of null domains
  if (is.null(modelData[['Domain.StageRecurrence']])) {
    print(
      paste(
        'Note: the stage recurrence domain is null (average floor elevation higher than max stage) for ',
        islandID,
        '.',
        sep = ''
      )
    )
  }
  
  #the rounding is to calibrate with Hollie's results
  modelData[['Domain.InundationDepth']] = round(modelData[['Domain.StageRecurrence']] - as.numeric(islandInfo[which(names(islandInfo) == 'AvgFloorElevation_ft')]),2)
  #remove any values less than 0
  modelData[['Domain.InundationDepth']] = binaryMap(modelData[['Domain.InundationDepth']])*modelData[['Domain.InundationDepth']]
  # #m is the number of stages in the domain
  # m = length(modelData[['Domain.StageRecurrence']])
  
  #initialize list out
  out = list()
  
  #initialize out list
  for (jS in 1:nS) {
    out[[jS]] = list()
    
    for (jLF in 1:nLF) {
      out[[jS]][[jLF]] = list()
      
      for (jT in 1:nT) {
        #initialize
        out[[jS]][[jLF]][[jT]] = list()
        
        #add island id
        out[[jS]][[jLF]][[jT]][['IslandID']] = islandID
        
        #error handling
        if (is.null(modelData[['Domain.StageRecurrence']])) {
          out[[jS]][[jLF]][[jT]][['Error.NullStageRecurrenceDomain']] = 1
        } else {
          out[[jS]][[jLF]][[jT]][['Error.NullStageRecurrenceDomain']] = 0
        }
      }
    }
  }
  
  ######################################################################
  #loop over each necessary interpolation to generate relevant matrices#
  ######################################################################

  #loop list contains information from the inner loop (unique for each jS, jLF, jT)
  curScenarioData = list()

  #stage recurrence nested loop
  # Begin calculations
  for (jS in 1:nS) {
    
    #loop over available types and store for loop
    for (sdType in setdiff(names(analysisData[['ScenarioData']]), c('Scenario'))) {
      #get component of scenario
      curScenarioData[[sdType]] = convertScenario(analysisData[['Scenarios']][jS], sdType)
      #check
      if (!(sdType %in% c("SLR_Condition", "Discount_Rate"))) {
        #convert to integer
        curScenarioData[[sdType]] = as.integer(curScenarioData[[sdType]])
      } else if (sdType == "Discount_Rate") {
        #convert to numeric
        curScenarioData[[sdType]] = as.numeric(curScenarioData[[sdType]])
      }
    }
    
    #TEMP: 07-18-2016 REPLACE MISSING SR/LF CURVES WITH BASELINE
    #distinct scenarios; conver sdType to field name
    distSLTables = list(
      "SR_Curve" = analysisData[['Stage_Recurrence_Table']],
      "LF_Curve" = "FragilityCurveHydro"
    )
    
    #loop to check and add
    for (sdType in c("SR_Curve", "LF_Curve")) {
      
      #get distinct stage recurrence scenarios
      dist = sqlQuery(dbase, paste("select distinct Scenario from ", distSLTables[[sdType]], " where ID_Number = ", islandID))
      #vector
      dist = unname(unlist(dist))
      #check to see that data is included for the scenario
      if (!(curScenarioData[[sdType]] %in% dist)) {
        #default to baseline
        curScenarioData[[sdType]] = dist[1]
      }
    }
    
    #levee fragility nested loop
    for (jLF in 1:nLF) {
      
      #time nested loop
      for (jT in 1:nT) {
        
        #indexedParamterMatrices[[jS]][[jLF]][[jT]]
        #FLAG: generate parameter matrix for each scenario and time step necessary; parameterMatrix will be subsequently adjusted with any dimensional changes added to databased
        #enter in this order: 
        #(1) Table Source Type
        #(2) matrixOut Name
        #(3) Interpolation Type
        #(4) SQL Table name
        #(5) Interpolation Domain
        #(6) Scenario ID (if applicable,'NULL' otherwise [independent of scenario])
        #(7 - 8) then paired (x,y) 
        #(9) then extra argument
        parameterMatrix = matrix(
          c(
            'SQL', 'StageRecurrence', 'PDF', analysisData[['Stage_Recurrence_Table']], 'StageRecurrence', curScenarioData[['SR_Curve']], 'Stage_ft', 'Probability',
            #levee fragility is correlated with stage recurrence, so the cross product of the two spaces is eliminated; analysisData[['Scenarios.LeveeFragility']][jLF] -> curScenarioData[['LF_Curve']],
            'SQL', 'LeveeFragility', 'CDF', 'FragilityCurveHydro', 'StageRecurrence', curScenarioData[['LF_Curve']], 'ElevNavD88', 'ProbFail',
            'SQL', 'SeismicFragility', 'CDF', 'FragilityCurveSeismic', 'SeismicFragility', curScenarioData[['SF_Curve']], 'pga','Probability',
            #'SQL', 'PGARecurrence', 'PDF', 'pga_Recurrence', 'SeismicFragility', curScenarioData[['PGAR_Curve']], 'pga', 'Probability'
            'SQL', 'ProbSeisFailGivenPGA', 'CDF', 'FragilityCurveSeismic', 'PGARecurrence', curScenarioData[['SF_Curve']], 'pga', 'Probability'
          ),
          ncol = pmNHeaderFields + 2,
          byrow = TRUE
        )
        
        # #this is a lazy solution at the moment
        # parameterMatrix = rbind(parameterMatrix)
 
        #############################
        #parameter-level nested loop#
        #############################
        # 1:nrow(parameterMatrix)
        
        for (p in 1:nrow(parameterMatrix)) {
          
          #shifts the water depth relative to island elevation; used to distinguish between maps with domains of stage feet and those with domains of inundation depth
          if (grepl('InundationDepth', parameterMatrix[p,5])) {
            domainShift = as.numeric(islandInfo[which(names(islandInfo) == 'AvgFloorElevation_ft')])
          } else {
            domainShift = 0
          }
          
          #build id field
          idstr = 'ID_Number'

          #determine necessary ID number component -- takes advantage of the fact that some tables use ID_Number for the island id, while others use if for other values; default is islandID
          if (grepl('DamageCurve',parameterMatrix[p, 2])) {
            idnum = as.numeric(unlist(strsplit(parameterMatrix[p, 2], '.', fixed=TRUE))[2])
            #update idstr to call from Asset Class
            idstr = "AssetCode"
          } else if (parameterMatrix[p,2] %in% c('SeismicFragility', "ProbSeisFailGivenPGA")) {
            idnum = seismicClass
            #exception
            idstr = 'Site_Class'
          } else {
            #default to islandID
            idnum = islandID
          }
          
          #kluge to account for Hollie's experimental inclusion of probabilities == 1
          if (parameterMatrix[p,4] == 'pga_Recurrence') {
            #add arguments restricting probability
            addArgs = analysisData[['pgaSQLArgument']]
          } else {
            #otherwise, leave blank
            addArgs = ''
          }
          
          #right now, additional columns for the desired sql table will be assumed to be located in the sql table and ordered according
          currentTable = getTable(
            parameterMatrix[p,4],
            parameterMatrix[p,(pmNHeaderFields+1)],
            parameterMatrix[p,(pmNHeaderFields+2)],
            idnum,
            dbase,
            type = parameterMatrix[p,1],
            ScenID.SR = parameterMatrix[p,6],
            idTag = idstr,
            additionalQArgs = addArgs
          )
    
          #set values to interpolate
          interpVals = modelData[[paste('Domain', parameterMatrix[p,5], sep = '.')]]
          
          #ADJ PER HOLLIE: Incorporate increased probability (this needs some kind of ironing out; possible to have consecutive probabilities go the incorrect direction)
          if (parameterMatrix[p, 4] == 'FragilityCurveHydro') {
            #build temporary replacement
            tempProbRepl = unname(
              unlist(
                lapply(
                  #apply to each row
                  1:nrow(currentTable),
                  #function to be applied
                  function (h) {
                    #apply lf only if elevation is below crest elevation
                    if(currentTable[h, parameterMatrix[p, (pmNHeaderFields + 1)]] < islandInfo[['AvgCrestElevation']]) {
                      #multiply probability by length factor
                      prob = currentTable[h, parameterMatrix[p, (pmNHeaderFields + 2)]] * lengthFactor
                    } else {
                      #otherwise, leave interpolation image unchanged TEMP
                      prob = currentTable[h, parameterMatrix[p, (pmNHeaderFields + 2)]] * lengthFactor
                    }
                    #max at 1
                    min(prob, 1)
                  }
                )
              )
            )

            #replace currentTable probability column
            currentTable[, parameterMatrix[p, (pmNHeaderFields + 2)]] = tempProbRepl
          }
          
          
          #FROM 7-14-16; ADD LENGTH FACTOR TO SEISMIC PROBABILITIES WHEN GENERATING PGA RECURRENCE
          if (parameterMatrix[p , 2] == "ProbSeisFailGivenPGA") {
            #get table to use for interpolate bounds
            currentTable[ , "Probability"] = unname(
              unlist(
                lapply(
                  currentTable[ , "Probability"],
                  function(h) {
                    min(h*lengthFactor, 1)
                  }
                )
              )
            )
           
            #get table of information on pga recurrence
            pgaTable = getTable(
              "pga_Recurrence",
              "pga",
              "Probability",
              islandID,
              dbase,
              type = "SQL",
              ScenID.SR = curScenarioData[['PGAR_Curve']],
              idTag = "ID_Number",
              additionalQArgs = ""
            )
            #order
            ord = order(pgaTable[ , "pga"])
            #set
            pgaTable = pgaTable[ord, ]
            #replace top row
            pgaTable[1, ] = c(0.00001, 1)
            
            #adjust; build new matrix 
            pt = c()
            #loop to add
            for (i0 in 1:(nrow(pgaTable) - 1)) {
              pt = c(
                pt,
                #mean pga
                (pgaTable[i0, "pga"] + pgaTable[i0 + 1, "pga"])/2,
                #estimated likelihood
                pgaTable[i0, "Probability"] - pgaTable[i0 + 1, "Probability"]
              )
            }
            #convert to matrix
            pt = matrix(pt, ncol = 2, byrow = TRUE)
            #data frame
            pt = data.frame(pt)
            #names
            names(pt) = names(pgaTable)
            #override interpolation values to generate IVO.ProbSeisFailGivenPGA
            interpVals = pt[ , "pga"]
            #add in pga matrix
            out[[jS]][[jLF]][[jT]][["EstimatedPGARecurrenceMatrix"]] = pt
            
          }
          
          #perform interpolation; use a simple stand in variable for clarity
          outvec = c()
          
          for (k in interpVals) {
            outvec = c(
              outvec,
              pwlInterpolation(
                k,
                parameterMatrix[p, (pmNHeaderFields+1)],
                parameterMatrix[p, (pmNHeaderFields+2)],
                currentTable,
                interpolationType = parameterMatrix[p, 3]
              )
            )
          }
          
          #ensure that results are numeric
          outvec = as.numeric(outvec)
          
          # Determines `IVO.`: Stage_Recurrence, LeveeFragility, SeismicFragility, ProbSeisFailGivenPGA
          out[[jS]][[jLF]][[jT]][[paste('IVO', parameterMatrix[p,2], sep = '.')]] = outvec
          
        }
        
        #adjust stage recurrence curves according to Hollie's curves; NOTE: THE CURVES ARE ALREADY LINEARLY INTERPOLATED, AND ALL PROBABILITIES HAVE ALREADY BEEN NORMALIZED; THEREFORE, THE MULTIPLICATION BY 1 AND 2 IS WARRANTED; THESE ASSUMPTIONS WERE NOT HIGHLIGHTED EARLY ON
        out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']] = (2*modelData[['Interval.StageRecurrence']])*out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']]
        
        #######################
        #Some global variables#
        #######################
        
        #TEMP: Levee Sesmic Fragility; add lengthFactor (maximum ensures that there cannot be probability greater than 1)
        seismicFragility = as.numeric(out[[jS]][[jLF]][[jT]][['IVO.ProbSeisFailGivenPGA']] %*% out[[jS]][[jLF]][[jT]][["EstimatedPGARecurrenceMatrix"]][ , "Probability"])
        #ensure seismicFragility is not greater than 1
        seismicFragility = min(seismicFragility, 1)
        
        #find net probability of a levee failure
        out[[jS]][[jLF]][[jT]][['VectorOut.NetProbabilityLeveeFailure']] = binaryMap(modelData[['Domain.InundationDepth']])*out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']]*(out[[jS]][[jLF]][[jT]][['IVO.LeveeFragility']] + seismicFragility - out[[jS]][[jLF]][[jT]][['IVO.LeveeFragility']]*seismicFragility)
        
        #calculate the hydrologic levee failure fate
        out[[jS]][[jLF]][[jT]][['Probability.HydrologicFailure']] = sum(
          binaryMap(modelData[['Domain.InundationDepth']])*out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']]*out[[jS]][[jLF]][[jT]][['IVO.LeveeFragility']]
        )
        
        #calculate the seismic levee failure fate
        out[[jS]][[jLF]][[jT]][['Probability.SeismicFailure']] = seismicFragility #sum(binaryMap(modelData[['Domain.InundationDepth']])*out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']]*seismicFragility)
        
        #initialize the net levee failure fate
        plf = sum(binaryMap(modelData[['Domain.InundationDepth']])*out[[jS]][[jLF]][[jT]][['IVO.StageRecurrence']]*out[[jS]][[jLF]][[jT]][['IVO.LeveeFragility']])
        
        #add in seismic information
        plf = plf + seismicFragility - plf * seismicFragility 
        #add calculation
        out[[jS]][[jLF]][[jT]][['Probability.LeveeFailure']] = plf
        # Scenario data
        out[[jS]][[jLF]][[jT]][['Scenario']] <- unlist(curScenarioData)
      }
    }
  }
  out
}
