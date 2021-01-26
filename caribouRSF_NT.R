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
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("nBootstrap", "numeric", 100, NA, NA, "How many bootstrap replicates do we want for the coefficients?"),
    defineParameter("plotTime", "numeric", end(sim), NA, NA, "plot time"),
    defineParameter(".plotTimeInterval", "numeric", 10, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set"),
    defineParameter("recoveryTime", "numeric", 60, NA, NA, "Time to recover the forest enough for caribou"),
    defineParameter("predictionInterval", "numeric", 20, NA, NA, "Time between predictions"),
    defineParameter(name = "baseLayer", class = "numeric", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?"),
    defineParameter(name = "decidousSp", class = "character", 
                    default = c("Betu_Pap", "Popu_Tre", "Popu_Bal"), 
                    min = NA, max = NA, desc = "Deciduous species to be considered for caribou"),
    # defineParameter("fireClasses", "list", list(fireClass10y = c(0, 10)), NA, NA, 
    #                 paste0("NOT FUNCTIONAL. Can be added in the future. Currently it is hardcoded ",
    #                        "to match the covariates in the specific model")),
    # defineParameter(name = "oldBurnTime", class = "numeric", default = 40, 
    #                 min = NA, max = NA, desc = "Threshold for oldBurn/newBurn. Max oldburn + 20"),
    defineParameter(".useCache", "character", ".inputObjects", NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
    
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "classTable", objectClass = "data.table",
                 desc = "Classification table for covariate/class",
                 sourceURL = "https://drive.google.com/file/d/1S4WoDlvCy_XKlSEQe8VLtSMr5mvDbCkQ"),
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
    expectsInput(objectName = "anthropogenicLayers", objectClass = "RasterLayer", 
                 desc = "These are: Linear feature density (1-km radius), 
                         Distance to major road, 
                         Distance to polygonal disturbance, 
                         Distance to settlement with the corresponding exponential equations (see
                 DeMars, Hodson, et al 2019 for details)", 
                 sourceURL = "https://drive.google.com/file/d/15RfDLv-EuzYrNZgP-aaE4zuyn_vd8aJu"),
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
    expectsInput(objectName = "reclassLCC05", objectClass = "data.table",
                 desc = "Table converting LCC05 classes to EOSD and back",
                 sourceURL = "https://drive.google.com/file/d/1YUXcx8Gc6dI4vy76l2k_P6tUm6X2m7MG"),
    expectsInput(objectName = "historicalFires", objectClass = "list", 
                 desc = "List for fire by year. This layer was built by James Hodson (GNWT)",
                 sourceURL = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7"),
    expectsInput(objectName = "rstCurrentBurnList", objectClass = "list", 
                 desc = "List of fires by year (raster format). These layers are produced by simulation",
                 sourceURL = "")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "coeffTablAndValues", objectClass = "list", 
                  desc = "List with model equation. Default is DeMars et al., 2019."),
    createsOutput(objectName = "predictedPresenceProbability", objectClass = "list", 
                  desc = "List of rasters per year, indicating the probability of presence of Caribous"),
    createsOutput(objectName = "modLayers", objectClass = "list", 
                  desc = "Stack of all layers for a given year: burns, simulLayers, fixedLayers"),
    createsOutput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to predict for",
                                " Currently the default is 3 shapefiles: Edehzhie, range planning, herds"))
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
        params(sim)$caribouRSF_NT$.useDummyData <- TRUE
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF_NT", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF_NT", "gettingData")
      }
    },
    preparingLayers = {
      if (isTRUE(P(sim)$.useDummyData)){
        stop("This module does not work without data. Please provide the necessary layers")
      } else {
        if (is.null(sim$modLayers)){
          sim$modLayers <- list()
        }
        sim$fireLayers <- composeFireLayers(currentTime = time(sim),
                                            historicalFires = sim$historicalFires,
                                            pathData = dataPath(sim),
                                            species = sim$sppEquiv$NWT_BCR6, #TODO remove "hardcoded"
                                            fireLayers = sim$fireLayers,
                                            cohortData = mod$cohortData,
                                            pixelGroupMap = mod$pixelGroupMap,
                                            decidousSp = P(sim)$decidousSp,
                                            rasterToMatch = sim$rasterToMatch,
                                            landClasses = c("Lowlands", "UplandsNonTreed",
                                                            "UplandConifer", "UplandBroadleaf"),
                                            yearClasses = c(10, 20, 30, 40, 60),
                                            correspondingClassesValues = list("Lowlands" = c(8, 17, 19, 31:32), 
                                                                              "UplandsNonTreed" = c(23, 16, 18, 
                                                                                                    25, 33, 36, 
                                                                                                    39)),
                                            # "UplandsConifer" & "UplandsBroadleaf" come from biomass!
                                            thisYearsFires = sim$rstCurrentBurnList,
                                            rstLCC = sim$rstLCC,
                                            makeAssertions = FALSE) # Needs for the 4 types of fires
        # Get the "fixed" layers: 
        sim$fixedLayers <- makeFixedLayers(fireLayers = sim$fireLayers,
                                           rstLCC = sim$rstLCC,
                                           pathData = dataPath(sim),
                                           makeAssertions = FALSE,
                                           classTable = sim$classTable)
        
        # Get the simulated layers: 
        sim$simulLayers <- makeSimulatedLayers(fixedLayers = sim$fixedLayers,
                                               classTable = sim$classTable, 
                                               fireLayers = sim$fireLayers,
                                               cohortData = mod$cohortData,
                                               pixelGroupMap = mod$pixelGroupMap,
                                               rasterToMatch = sim$rasterToMatch,
                                               rstLCC = sim$rstLCC,
                                               decidousSp = P(sim)$decidousSp,
                                               currentTime = time(sim),
                                               historicalFires = sim$historicalFires,
                                               pathData = dataPath(sim),
                                               species = sim$sppEquiv$NWT_BCR6) #TODO remove "hardcoded"
        
        # Put all layers together
        sim$modLayers[[paste0("Year", time(sim))]] <- raster::stack(sim$fireLayers, 
                                                                    sim$fixedLayers, 
                                                                    sim$simulLayers,
                                                                    sim$anthropogenicLayers)
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
                                                                                  modLayers = sim$modLayers[[paste0("Year", time(sim))]],
                                                                                  currentTime = time(sim),
                                                                                  pathData = dataPath(sim),
                                                                                  pathOut = outputPath(sim),
                                                                                  shp = caribouArea2)
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
    sim$rstCurrentBurnList <- readRDS(file.path(Paths$inputPath, 
                                                "rstCurrentBurnList_year2100.rds"))
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
                           destinationPath = dataPath(sim), 
                           fun = "data.table::fread") #TODO # Pass as argument! (Make sure to harmonize tables!) 
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
                               # useCloud = P(sim)$.useCloud,
                               # cloudFolderID = sim$cloudFolderID, 
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID", "useCloud", 
                                            "overwrite", "filename2"))
  }
  
  if (!suppliedElsewhere("reclassLCC05", sim)){
    sim$reclassLCC05 <- prepInputs(targetFile = "EOSD_LCC05_ConversionTable.csv",
                                   url = extractURL("reclassLCC05"),
                                   destinationPath = dataPath(sim),
                                   # overwrite = TRUE, 
                                   fun = "data.table::fread")
  }
  
  if (!suppliedElsewhere("rstLCC", sim)){
    sim$rstLCC <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                       studyArea = sim$studyArea,
                                       rasterToMatch = sim$rasterToMatch)
  }

  if (!suppliedElsewhere("waterRaster", sim)){
    sim$waterRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                             studyArea = sim$studyArea, lccLayer = P(sim)$baseLayer,
                             rasterToMatch = sim$rasterToMatch,
                             userTags = c("objectName:wetLCC"))
    
    waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[!is.na(waterVals) & waterVals != 1] <- 0
    sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)
  }
  
  if (!suppliedElsewhere("anthropogenicLayers", sim)){
    sim$anthropogenicLayers <- Cache(prepInputs, targetFile = "anthropoDistLayers.grd",
                                         archive = "anthropoDistLayers.zip",
                                         alsoExtract = "similar",
                                         url = extractURL("anthropogenicLayers"),
                                         destinationPath = dataPath(sim), 
                                         studyArea = sim$studyArea,
                                         rasterToMatch = sim$rasterToMatch,
                                         fun = "raster::stack",
                                         userTags = c("FUN:.inputObjs", 
                                                      "object:anthropogenicLayers"))
  }

  if (!suppliedElsewhere("historicalFires", sim)){
    fireYears <- 1991:2017
    sim$historicalFires <- Cache(fireSenseUtils::getFirePolygons, 
                           years = fireYears,
                           studyArea = aggregate(sim$studyArea),
                           pathInputs = Paths$inputPath, 
                           userTags = paste0("years:", range(fireYears)))
  }
  
  if (!suppliedElsewhere(object = "listSACaribou", sim = sim)){
    
    caribouArea2 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV",
                          targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                          alsoExtract = "similar", overwrite = TRUE,
                          rasterToMatch = rasterToMatch,
                          destinationPath = dataPath(sim), 
                          filename2 = "caribouArea2")
    
    Edehzhie <- Cache(prepInputs, targetFile = "Edehzhie.shp",
                      archive = "Edehzhie.zip",
                      alsoExtract = "similar", overwrite = TRUE,
                      url = "https://drive.google.com/open?id=1VP91AyIeGCFwJS9oPSEno4_SbtJQJMh7", 
                      studyArea = sim$studyArea,
                      destinationPath = dataPath(sim), filename2 = NULL,
                      rasterToMatch = sim$rasterToMatch)
    
    caribouArea1 <- Cache(prepInputs, 
                          url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO",
                          targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                          alsoExtract = "similar", overwrite = TRUE,
                          rasterToMatch = rasterToMatch,
                          destinationPath = dataPath(sim), filename2 = "caribouArea1")
    
    sim$listSACaribou = list(sim$caribouArea1, sim$caribouArea2, sim$Edehzhie)
    names(sim$listSACaribou) <- c("caribouArea1", "caribouArea2", "Edehzhie")
  }
  return(invisible(sim))
}