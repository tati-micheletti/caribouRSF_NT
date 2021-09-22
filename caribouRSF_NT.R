defineModule(sim, list(
  name = "caribouRSF_NT",
  description = paste0("This is a module that implements the Boreal Caribou",
                       " Resource Selection Function model developed by ",
                       "DeMars et al., 2019 for the ",
                       "Northwest Territories in Canada"),
  keywords = c("Caribou", "RSF"),
  authors = structure(list(list(given = "Tati", 
                                family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.1", caribouRSF_NT = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "caribouRSF_NT.Rmd")),
  reqdPkgs = list("data.table", "ggplot2", "PredictiveEcology/pemisc", 
                  "tati-micheletti/usefulFuns", "magrittr", "raster",
                  "PredictiveEcology/fireSenseUtils@V.2.0_NWT", "tictoc"),
  parameters = rbind(
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, 
                    paste0("If last year of simulation is not multiple of",
                           " predictionInterval, should it predict for the last year too?")),
    defineParameter("rowOfFixedLayers", "numeric", 8, NA, NA, 
                    paste0("This is the number of rows from the classTable which contain",
                           " fixed covariates (i.e. water, shrub, herbs, etc.)",
                           " When providing classTable yourself, you need to inform this parameter")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("nBootstrap", "numeric", 100, NA, NA, "How many bootstrap replicates do we want for the coefficients?"),
    defineParameter("makeAssertions", "logical", TRUE, NA, NA, "Should layers be tested for correcteness? This increases simulation time"),
    defineParameter("plotTime", "numeric", end(sim), NA, NA, "plot time"),
    defineParameter("simulationProcess", "character", "dynamic", NA, NA, 
                    paste0("Should the simulation use LandR (dynamic) or land cover map (static)?",
                           "defaults to dynamic")),
    defineParameter(".plotTimeInterval", "numeric", 10, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set"),
    defineParameter("recoveryTime", "numeric", 60, NA, NA, "Time to recover the forest enough for caribou"),
    defineParameter("predictionInterval", "numeric", 20, NA, NA, "Time between predictions"),
    defineParameter(name = "baseLayer", class = "numeric", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?"),
    defineParameter(name = "decidousSp", class = "character", 
                    default = c("Betu_Pap", "Popu_Tre", "Popu_Bal"), 
                    min = NA, max = NA, desc = "Deciduous species to be considered for caribou"),
    defineParameter(".useCache", "character", ".inputObjects", NA, NA,
                    desc = paste0("Internal. Can be names of events or the whole module ",
                                  "name; these will be cached by SpaDES")),
    defineParameter("cropRSFToShp", "logical", "FALSE", NA, NA,
                    desc = paste0("Should the RSF be cropped to a smaller extent? ",
                                  "If TRUE, shp in RSFmModel HAS to be provided")),
    defineParameter("yearsToSaveCaribouLayers", "numeric", NA, NA, NA,
                    desc = paste0("In which years should the simulation save the layers used ",
                                  " to generate the caribou RSF predictions? Defaults to NA, no saving")),
    defineParameter("leadingSpThreshold", "numeric", 0.75, NA, NA,
                    desc = paste0("This is the threshold to define if a stand (pixel) is pure or ",
                                  "mixedwood. If either conifer or broadleaf if above this value ",
                                  "it is considered a pure stand")),
    defineParameter("deciduousCoverDiscount", "numeric", 0.8418911, NA, NA,
                    paste0("This was estimated with data from NWT on March ",
                           "18, 2020 and may or may not be universal."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "classTable", objectClass = "data.table",
                 desc = paste0("Two-column classification table for covariate/class. When providing this ",
                               "object, the user needs to follow the format:",
                               "First row: naming (i.e. layers and classCode), ",
                               "Second row to nth row ('rowOfFixedLayers'): ",
                               "fixed covariates with naming following the ",
                               " same covariates used in the RSF model",
                               "nth row + 1 to end of table: simulated covariates ",
                               "with naming following the same covariates used ",
                               "in the RSF model"),
                 sourceURL = "https://drive.google.com/file/d/1S4WoDlvCy_XKlSEQe8VLtSMr5mvDbCkQ/"),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = paste0("Map of groups of pixels that share the same info from cohortData (sp, age, biomass, etc).",
                               "Here is mainly used to determine old and recent burns based on tree age,",
                               " and if deciduous by species")),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc")),
    expectsInput(objectName = "caribouCoefTable", objectClass = "data.table", 
                 desc = "Caribou coefficients from the NT1 caribou model (DeMars et al., 2019)", 
                 sourceURL = "https://drive.google.com/file/d/1Q_OCXre7ksVwMauFvp-80LSb7xtOVNQ7"),
    expectsInput(objectName = "rstLCC", objectClass = "RasterLayer", 
                 desc = paste0("This will give is all 'fixedLayers' except for",
                               " water and anthropogenic layers (see next 2 inputs)"), 
                 sourceURL = ""),
    expectsInput(objectName = "caribouLCC", objectClass = "RasterLayer", 
                 desc = paste0("If a specific landcover class layer is to be used with 
                               the caribou module, you pass it here. 
                               This will give is all 'fixedLayers' except for",
                               " water and anthropogenic layers (see next 2 inputs)"), 
                 sourceURL = ""),
    expectsInput(objectName = "anthropogenicLayers", objectClass = "RasterLayer", 
                 desc = "These are: Linear feature density (1-km radius), 
                         Distance to major road, 
                         Distance to polygonal disturbance, 
                         Distance to settlement with the corresponding exponential equations (see
                 DeMars, Hodson, et al 2019 for details)", 
                 sourceURL = "https://drive.google.com/file/d/1A4BBYFDAzllpPFyj29cM0MC6nF9A6-kv/"),
    expectsInput(objectName = "fixedLayers", objectClass = "character", 
                 desc = "Fixed layers for the Caribou RSF model, currently: 
                         water,
                         Bryoids,
                         Tall shrub,
                         Short shrub,
                         Treed wetland,
                         Shrub wetland,
                         Herb wetland,
                         Herb",
                 sourceURL = ""),
    expectsInput(objectName = "simulLayers", objectClass = "character", 
                 desc = paste0("Possibly simulated layers for the Caribou RSF model, ",
                               "currently: 
                               Proportion of broadleaf (1-km radius),
                               Proportion of conifer sparse (1-km radius)
                               Conifer open,
                               Conifer sparse,
                               Broadleaf dense,
                               Broadleaf open,
                               Mixedwood open,
                               Mixedwood dense,
                               Non-vegetated"), 
                 sourceURL = ""),
    expectsInput(objectName = "fireLayers", objectClass = "character", 
                 desc = paste0("Possibly simulated layers for the Caribou RSF model, ",
                               "currently: 
                               4 (lowlands, uplandsNontreed, uplandsConifer, 
                                  uplandsBroadleaf) * 6 classes of 
                                  burns (0-10y,10-20y,20-30y,30-40y,40-60y)"), 
                 sourceURL = ""),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Study area for the prediction. Currently only available for NWT", 
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it", 
                 sourceURL = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df"),
    expectsInput(objectName = "historicalFires", objectClass = "list", 
                 desc = "List for fire by year. This layer was built by James Hodson (GNWT)",
                 sourceURL = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7"),
    expectsInput(objectName = "rstCurrentBurnList", objectClass = "list", 
                 desc = "List of fires by year (raster format). These layers are produced by simulation",
                 sourceURL = ""),
    expectsInput(objectName = "binningTable", objectClass = "data.table", 
                 desc = "Original binning table from DeMars et al., 2019 (Updated in JAN2021 after bugfix)",
                 sourceURL = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc"),
    expectsInput(objectName = "runName", objectClass = "character",
                 desc = "runName relates to the area ran for the simulation. Defaults is to stop without it",
                 sourceURL = NA),
    expectsInput(objectName = "sppEquiv", objectClass = "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.", 
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "coeffTablAndValues", objectClass = "list", 
                  desc = "List with model equation. Default is DeMars et al., 2019."),
    createsOutput(objectName = "predictedPresenceProbability", objectClass = "list", 
                  desc = "List of rasters per year, indicating the probability of presence of Caribous"),
    createsOutput(objectName = "caribouLayers", objectClass = "list", 
                  desc = "Stack of all layers for a given year: burns, simulLayers, fixedLayers")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.caribouRSF_NT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, start(sim), "caribouRSF_NT", "makingModel")
      sim <- scheduleEvent(sim, start(sim), "caribouRSF_NT", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "caribouRSF_NT", "preparingLayers")
      sim <- scheduleEvent(sim, start(sim), "caribouRSF_NT", "calculatingRSF")
      sim <- scheduleEvent(sim, P(sim)$plotTime, "caribouRSF_NT", "plot", eventPriority = .last()) 
      #P(sim)$.plotInitialTime
    },
    makingModel = {
      # Prepare the Equation
      sim$coeffTablAndValues <- buildCoefficientsTable(caribouCoefTable = sim$caribouCoefTable,
                                                       nBootstrap = P(sim)$nBootstrap)
    },
    gettingData = {
      mod$cohortData <- usefulFuns::createModObject(data = "cohortData", sim = sim, 
                                                    pathInput = inputPath(sim), currentTime = time(sim))
      mod$pixelGroupMap <- usefulFuns::createModObject(data = "pixelGroupMap", sim = sim, 
                                                       pathInput = inputPath(sim), currentTime = time(sim))
      
      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData))) {
        warning(paste0("Vegetation layers not found for year ", time(sim),
                       ". Simulations will NOT use simulated vegetation"))
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF_NT", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF_NT", "gettingData")
      }
    },
    preparingLayers = {
      if (P(sim)$simulationProcess == "dynamic"){
        currRstLCC <- makeLCCfromCohortData(cohortData = mod$cohortData,
                                            pixelGroupMap = mod$pixelGroupMap,
                                            lccClassTable = data.table(
                                              standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                                                               "pureCon_sparse",
                                                               "pureBroad_dense", "pureBroad_open", "pureBroad_sparse",
                                                               "mixed_dense", "mixed_open", "mixed_sparse"), 
                                              LCCclass = c(1,6,8,32,
                                                           2,11,11, 
                                                           3,13,13)), # HARDCODED TO MATCH LCC05
                                            deciduousCoverDiscount = P(sim)$deciduousCoverDiscount,
                                            species = sim[["sppEquiv"]][[sim[["runName"]]]],
                                            leadingSpThreshold = P(sim)$leadingSpThreshold,
                                            decidousSp = P(sim)$decidousSp,
                                            rstLCC = sim$rstLCC) # This raster needs to be already the land cover raster
                                 # after landscape changes (i.e. burns and coming from cohort data)
      } else {
        currRstLCC <- sim$caribouLCC
      }
        sim$fireLayers <- composeFireLayers(currentTime = time(sim),
                                            historicalFires = sim$historicalFires,
                                            pathData = dataPath(sim),
                                            species = sim[["sppEquiv"]][[sim[["runName"]]]],
                                            fireLayers = sim$fireLayers,
                                            cohortData = mod$cohortData,
                                            pixelGroupMap = mod$pixelGroupMap,
                                            decidousSp = P(sim)$decidousSp,
                                            landClasses = c("Lowlands", "UplandsNonTreed",
                                                            "UplandConifer", "UplandBroadleaf"),
                                            yearClasses = c(10, 20, 30, 40, 60),
                                            simulationProcess = P(sim)$simulationProcess,
                                            # For correspondingClassesValues see: https://drive.google.com/drive/u/0/folders/1911W_RGwcC36HovCHtpvf806w2GHDiuO
                                            # This is how the model was built by GNWT
                                            correspondingClassesValues = list(
                                              "dynamic" = list( # Converted EOSD to LCC05 values
                                                "Lowlands" = c(8, 17, 19, 31:32),
                                                "UplandsNonTreed" = c(23, 16, 18),
                                                "UplandConifer" = c(1, 6),
                                                "UplandBroadleaf" = c(2, 3, 11, 13) # 15 shouldn't exist (broadleaf sparse)
                                              ),
                                              "static" = list( # Original EOSD values
                                                "Lowlands" = c(81:83, 100, 213),
                                                "UplandsNonTreed" = c(40, 51, 52),
                                                "UplandConifer" = c(211, 212),
                                                "UplandBroadleaf" = c(221, 222, 231, 232)
                                              )
                                            ), 
                                            # "UplandsConifer" & "UplandsBroadleaf" come from biomass!
                                            thisYearsFires = sim$rstCurrentBurnList,
                                            rstLCC = currRstLCC,
                                            makeAssertions = P(sim)$makeAssertions) # Needs for the 4 types of fires
        # Get the "fixed" layers: 
        sim$fixedLayers <- makeFixedLayers(fireLayers = sim$fireLayers,
                                           rstLCC = currRstLCC,
                                           pathData = dataPath(sim),
                                           makeAssertions = P(sim)$makeAssertions,
                                           rowOfFixedLayers = P(sim)$rowOfFixedLayers,
                                           classTable = sim$classTable)
        
        # Get the simulated layers: 
        sim$simulLayers <- makeSimulatedLayers(fixedLayers = sim$fixedLayers,
                                               classTable = sim$classTable, 
                                               fireLayers = sim$fireLayers,
                                               cohortData = mod$cohortData,
                                               rowOfSimulLayers = P(sim)$rowOfFixedLayers+1,
                                               pixelGroupMap = mod$pixelGroupMap,
                                               simulationProcess = P(sim)$simulationProcess,
                                               rstLCC = currRstLCC,
                                               rasterToMatch = sim$rasterToMatch,
                                               decidousSp = P(sim)$decidousSp,
                                               currentTime = time(sim),
                                               historicalFires = sim$historicalFires,
                                               pathData = dataPath(sim),
                                               species = sim[["sppEquiv"]][[sim[["runName"]]]],
                                               makeAssertions = P(sim)$makeAssertions) #TODO remove "hardcoded" spEquiv
        
        # Put all layers together
        sim$caribouLayers[[paste0("Year", time(sim))]] <- raster::stack(sim$fireLayers, 
                                                                    sim$fixedLayers, 
                                                                    sim$simulLayers,
                                                                    sim$anthropogenicLayers)
        
        # Check all layers have the corresponding names in the model
        testthat::expect_true(all(names(sim$caribouLayers[[paste0("Year", time(sim))]]) %in%
              colnames(sim$coeffTablAndValues$caribouRSF_NT$coeffTable)))
        
        if (time(sim) %in% P(sim)$yearsToSaveCaribouLayers) {
          # Assert all rasters are in memory before saving the stack!
          allInMemory <- checkRasterStackIsInMemory(rasStack = sim$caribouLayers[[paste0("Year", 
                                                                                         time(sim))]])
          if (!all(allInMemory)) {
            notInMem <- which(!allInMemory)
            lapply(notInMem, function(rasNumb){
              sim$caribouLayers[[paste0("Year", time(sim))]][[rasNumb]][] <- 
                sim$caribouLayers[[paste0("Year", time(sim))]][[rasNumb]][]
            })
          }
          # Assert all are in memory, otherwises saving will fail!
          allInMemory <- checkRasterStackIsInMemory(rasStack = sim$caribouLayers[[paste0("Year", 
                                                                                         time(sim))]])
          
          if (!all(allInMemory)) 
            stop("Something went wrong when bringing rasters to memory to save. Please debug.")
          
          ###### End assertion
          
          writeRaster(sim$caribouLayers[[paste0("Year", time(sim))]], 
                      filename = file.path(outputPath(sim), paste0("caribouLayers_year", 
                                                                   time(sim))))
          message(crayon::green(paste0("Caribou layers successfully saved as: ", 
                                       file.path(outputPath(sim), paste0("caribouLayers_year", 
                                                                         time(sim))))))
        }
          
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF_NT", "preparingLayers")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF_NT", "preparingLayers")
      }
    },
    calculatingRSF = {

        sim$predictedPresenceProbability[[paste0("Year", time(sim))]] <- RSFModel(coeffTablAndValues = sim$coeffTablAndValues,
                                                                                  modLayers = sim$caribouLayers[[paste0("Year", time(sim))]],
                                                                                  currentTime = time(sim),
                                                                                  pathData = dataPath(sim),
                                                                                  binningTable = sim$binningTable,
                                                                                  pathOut = outputPath(sim),
                                                                                  shp = caribouArea2,
                                                                                  cropRSFToShp = P(sim)$cropRSFToShp)
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF_NT", "calculatingRSF")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF_NT", "calculatingRSF")
      }
    },
    plot = {
      caribouResourceSelection <- sim$predictedPresenceProbability[[paste0("Year", 
                                                                           time(sim))]][["TaigaPlains"]][["relativeSelection"]]
      Plot(caribouResourceSelection, 
           title = "Caribou resource selection")
      
      # schedule future event(s)
      if (time(sim) != end(sim))
        sim <- scheduleEvent(sim, end(sim), "caribouRSF_NT", "plot", eventPriority = .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  
  if (!suppliedElsewhere("rstCurrentBurnList", sim)){
    warning("rstCurrentBurnList needs to be provided and was not found in the simList. 
Trying to find it in inputPath", immediate. = TRUE)
    rstCurBurnPath <- file.path(Paths$inputPath, 
              "rstCurrentBurnList_year2100.rds")
    if (!file.exists(rstCurBurnPath)){
      if (P(sim)$simulationProcess == "dynamic"){
        stop("rstCurrentBurnList was not found and it is needed when performing simulations. 
        Please provide it as an object to the simulation. 
        rstCurrentBurnList is a list of fires by year (raster format).
             These layers are produced by simulation. Alternatively, if you 
             are updating RSF predictions, make sure you set the parameter 
             simulationProcess  = 'static' or pass rstCurrentBurnList = NULL")
      } else {
        sim$rstCurrentBurnList <- NULL 
      }
    } else {
      sim$rstCurrentBurnList <- readRDS(rstCurBurnPath)
      }
  }
  if (!suppliedElsewhere("caribouCoefTable", sim)){
    sim$caribouCoefTable <- prepInputs(url = extractURL("caribouCoefTable"),
                                          destinationPath = dataPath(sim),
                                          targetFile = "caribouRSF_NT_Table.csv",
                                          fun = "data.table::fread", 
                                          omitArgs = "destinationPath", 
                                          overwrite = TRUE)
  }
  if (!suppliedElsewhere(object = "classTable", sim = sim)){
    sim$classTable <- prepInputs(url = extractURL("classTable"),
                                 targetFile = "landcoverClassesTable.csv",
                                 destinationPath = dataPath(sim), 
                                 fun = "data.table::fread") 
    if (P(sim)$simulationProcess == "static"){
      sim$classTable <- sim$classTable[, c("fixedLayers", "classCodeEOSD")]
      names(sim$classTable)[names(sim$classTable) == "classCodeEOSD"] <- "classCode"
    } else {
      sim$classTable <- sim$classTable[, c("fixedLayers", "classCodeLCC05")]
      names(sim$classTable)[names(sim$classTable) == "classCodeLCC05"] <- "classCode"
    }
  }
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dataPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"), 
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", destinationPath = dataPath(sim), 
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID", "useCloud", 
                                            "overwrite", "filename2"))
  }
  if (!suppliedElsewhere("NT1shapefile", sim)){
    sim$NT1shapefile <- prepInputs(url = "https://drive.google.com/file/d/1AOfJmIzZqQvQWwC7fJWEkmXCj3y6uwWF",
                                   targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                                   destinationPath = Paths$inputPath,
                                   alsoExtract = "similar",
                                   rasterToMatch = sim$rasterToMatch)
  }
  
  if (P(sim)$simulationProcess == "dynamic"){
    if (!suppliedElsewhere("rstLCC", sim)){
      sim$rstLCC <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                         studyArea = sim$studyArea,
                                         rasterToMatch = sim$rasterToMatch)
    }
  } else {
    if (!suppliedElsewhere("caribouLCC", sim)){
    sim$caribouLCC <- Cache(prepInputs, targetFile = "EOSD_covType.tif",
                                      archive = "EOSD_covType.zip",
                                      alsoExtract = "similar",
                                      url = "https://drive.google.com/file/d/1l0Ne1R95xpHJGf0--QY9iFGYmgQZhljn",
                                      studyArea = sim$studyArea,
                                      destinationPath = Paths$inputPath,
                                      filename2 = "EOSD_BCR6",
                                      rasterToMatch = sim$rasterToMatch,
                                      fun = "raster::raster",
                                      userTags = c(stepCacheTag,
                                                   "outFun:Cache", "step:prepEOSD"))
    }
  }

  if (!suppliedElsewhere("anthropogenicLayers", sim)){
    sim$anthropogenicLayers <- Cache(prepInputs, targetFile = "anthropoDistLayers.grd",
                                         archive = "anthropoDistLayers.zip",
                                         alsoExtract = "similar",
                                         url = extractURL("anthropogenicLayers"),
                                         destinationPath = dataPath(sim), 
                                         studyArea = sim$studyArea,
                                         filename2 = "anthropogenicLayers",
                                         rasterToMatch = sim$rasterToMatch,
                                         fun = "raster::stack",
                                         userTags = c("FUN:.inputObjs", 
                                                      "object:anthropogenicLayers"))
    names(sim$anthropogenicLayers)[names(sim$anthropogenicLayers) == "lineDen1000"] <- "lden1000_2015"
    names(sim$anthropogenicLayers)[names(sim$anthropogenicLayers) == "exp_sett"] <- "exp_settle"
  }
  
  if (!suppliedElsewhere("runName", sim)){
    stop("Please provide a runName to your simulation (i.e. 'NWT_NT1_BCR6')")
  }
  
  if (!suppliedElsewhere("historicalFires", sim)){
    fireYears <- 1991:2017
    sim$historicalFires <- Cache(fireSenseUtils::getFirePolygons, 
                           years = fireYears,
                           studyArea = aggregate(sim$studyArea),
                           pathInputs = Paths$inputPath, 
                           userTags = paste0("years:", range(fireYears)))
  }
  
  
  if (!suppliedElsewhere("binningTable", sim)){
    sim$binningTable <- Cache(prepInputs, 
                              targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                              url = extractURL("binningTable"),
                              destinationPath = dataPath(sim), 
                              fun = "data.table::fread",
                              userTags = c("object:binningTable"))
  }
  
  return(invisible(sim))
}