makeSimulatedLayers <- function(fixedLayers,
                                currentTime,
                                historicalFires,
                                fireLayers,
                                cohortData,
                                species,
                                classTable,
                                pixelGroupMap,
                                decidousSp,
                                rasterToMatch,
                                rstLCC,
                                pathData,
                                makeAssertions = TRUE){
  # 1. Define which ones are conifers, which are broadleaf and which are mixed
  # The pixels that have never been burned, are already classified in rstLCC
  #     Define the pixels that have not been burned in the last 60 years and use 
  #     them for this
  # 1A. Remove all fired pixels of the last 60 years
  pixelsToClassify <- rasterToMatch
  allCurrentlyBurnedPixels <- raster::calc(fireLayers, fun = sum, na.rm = TRUE)
  names(allCurrentlyBurnedPixels) <- "fireLayers"
  pixelsToClassify[allCurrentlyBurnedPixels == 1] <- 0
  # 1B. Remove all fixedLayers pixels
  allFixedLayersPixels <- raster::calc(fixedLayers, fun = sum, na.rm = TRUE)
  names(allFixedLayersPixels) <- "fixedLayers"
  pixelsToClassify[allFixedLayersPixels == 1] <- 0
  # Now we are left with NonVegetated and Pixels where we need to classify biomass

  biomassClassified <- rstLCC
  biomassClassified[pixelsToClassify == 0] <- 0
  # # Conifer open = 6, 1*
  # # Conifer sparse = 8
  # # Broadleaf dense = 2,
  # # Broadleaf open = 11,
  # # Mixedwood open = 13, 15
  # # Mixedwood dense = 3
  templateRas <- raster(rstLCC)
  rowOfSimulLayers <- 9 #TODO # Softcode!! Will need to clear cache, no time now
  simulatedLayers <- raster::stack(lapply(rowOfSimulLayers:NROW(classTable), function(index){
    message(paste0("Building ", classTable[["fixedLayers"]][index]," layer..."))
    templateRas[biomassClassified[] %in% eval(parse(text = classTable[["classCode"]][index]))] <- 1
    names(templateRas) <- classTable[["fixedLayers"]][index]
    return(templateRas)
  }))
  
  # Classes we don't have in the models:
  # *Conifer dense = 1 --> Conifer open and sparse have the same coefficient. 
  #                       I will assume caribou selects for conifer dense the same way
  # **Mixedwood Sparse = 15 --> I will assume caribou selects for mixedwood sparse the same way as open

  NonVegetatedClasses <- c(25, 33, 36, 39)
  # Barren Land = 25
  # Snow/Ice = 39
  # Rocks = 33
  # Urban = 36
  nonveg <- raster(rstLCC) 
  nonveg[biomassClassified[] %in% NonVegetatedClasses] <- 1
  names(nonveg) <- "nonveg"
  
  if (makeAssertions){
    # Assertion!!
    binaryLayersStack <- raster::stack(allCurrentlyBurnedPixels, 
                                       allFixedLayersPixels,
                                       simulatedLayers,
                                       nonveg)
    # Each pixel can be only one classification, so the sum of the stack 
    # needs to be 1 or NA for all pixels
    message("Verifying the constructed simulated layers...")
    pixelsSum <- raster::calc(binaryLayersStack, fun = sum, na.rm = TRUE)
    testthat::expect_true(all.equal(sort(unique(pixelsSum[])), c(0, 1)),
                          label = "S layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }
  
  # Proportions: Prepare the biomass maps
  treeSpecies <- data.table(speciesCode = c(decidousSp,
                                            species[!species %in% decidousSp]),
                            treeRSF = c(rep("broadleaf",
                                            times = length(decidousSp)),
                                        rep("conifer",
                                            times = length(species[!species %in% decidousSp]))))
  cohortData <- merge(cohortData, treeSpecies)
  # Pixel Group to Track: "248852"
  cohortData[, treeB := sum(B), by = c("pixelGroup", "treeRSF")]
  # 1. Create the broadleaf table and map
  cohortDataBroad <- unique(cohortData[treeRSF == "broadleaf", c("pixelGroup", "treeB")])
  broadleafMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataBroad,
                                                 fullRaster = pixelGroupMap,
                                                 newRasterCols = "treeB",
                                                 mapcode = "pixelGroup")
  # 2. Create the conifer table
  cohortDataConifer <- unique(cohortData[treeRSF == "conifer", c("pixelGroup", "treeB")])
  coniferMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataConifer,
                                                 fullRaster = pixelGroupMap,
                                                 newRasterCols = "treeB",
                                                 mapcode = "pixelGroup")
  # 3. Create the totalBiomass table
  cohortData[, totalBiomass := sum(B), by = "pixelGroup"]
  cohortDataTotal <- unique(cohortData[, c("pixelGroup", "totalBiomass")])
  totalBiomassMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataTotal,
                                               fullRaster = pixelGroupMap,
                                               newRasterCols = "totalBiomass",
                                               mapcode = "pixelGroup")
  
  focalMatrix <- circularWindow(ras = pixelGroupMap, focalDistance = 1000)
  t_broad <- raster::focal(x = broadleafMap, w = focalMatrix, fun = sum, na.rm = TRUE)
  t_consparse <- raster::focal(x = coniferMap, w = focalMatrix, fun = sum, na.rm = TRUE)
  total <- raster::focal(x = totalBiomassMap, w = focalMatrix, fun = sum, na.rm = TRUE)
  
  p_broad <- t_broad/total
  names(p_broad) <- "p_broad"
  p_consparse <- t_consparse/total
  names(p_consparse) <- "p_consparse"
  
  # Assertions
  if (makeAssertions){
    message("Verifying the constructed proportion layers...")
    pixelsSum <-  calc(raster::stack(p_broad, p_consparse), fun = sum, na.rm = TRUE)
    testthat::expect_true(all.equal(sort(unique(pixelsSum[])), c(0, 1)), 
                          label = "Proportion layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }

  simulLayers <- raster::stack(simulatedLayers, nonveg, p_broad, p_consparse)
  return(simulLayers)
}