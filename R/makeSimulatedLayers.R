makeSimulatedLayers <- function(fixedLayers,
                                currentTime,
                                historicalFires,
                                fireLayers,
                                cohortData,
                                species,
                                classTable,
                                pixelGroupMap,
                                decidousSp,
                                rstLCC,
                                rasterToMatch,
                                simulationProcess,
                                pathData,
                                makeAssertions = TRUE){
  # 1. Define which ones are conifers, which are broadleaf and which are mixed
  # The pixels that have never been burned, are already classified in rstLCC
  #     Define the pixels that have not been burned in the last 60 years and use 
  #     them for this
  # 1A. Remove all fired pixels of the last 60 years
  pixelsToClassify <- rstLCC
  pixelsToClassify[!is.na(pixelsToClassify)] <- 1
  allCurrentlyBurnedPixels <- raster::calc(fireLayers, fun = sum, na.rm = TRUE, 
                                           filename = file.path(Paths$outputPath,
                                                                "tmp_fireCalcLay"),
                                           overwrite = TRUE, format = "GTiff")
  allCurrentlyBurnedPixels[] <- allCurrentlyBurnedPixels[]
  names(allCurrentlyBurnedPixels) <- "fireLayers"
  pixelsToClassify[allCurrentlyBurnedPixels[] == 1] <- 0
  # 1B. Remove all fixedLayers pixels
  allFixedLayersPixels <- raster::calc(fixedLayers, fun = sum, na.rm = TRUE, 
                                       filename = file.path(Paths$outputPath,
                                                            "tmp_fireCalcLay"),
                                       overwrite = TRUE, format = "GTiff")
  allFixedLayersPixels[] <- allFixedLayersPixels[]
  names(allFixedLayersPixels) <- "fixedLayers"
  pixelsToClassify[allFixedLayersPixels[] == 1] <- 0
  # Now we are left with NonVegetated and Pixels where we need to classify biomass
  biomassClassified <- rstLCC
  biomassClassified[pixelsToClassify[] == 0] <- 0

  templateRas <- raster(rstLCC)
  rowOfSimulLayers <- 9 #TODO # Softcode!! Will need to clear cache, no time now
  simulatedLayers <- raster::stack(lapply(rowOfSimulLayers:NROW(classTable), function(index){
    message(paste0("Building ", classTable[["fixedLayers"]][index]," layer..."))
    templateRas[biomassClassified[] %in% eval(parse(text = classTable[["classCode"]][index]))] <- 1
    names(templateRas) <- classTable[["fixedLayers"]][index]
    return(templateRas)
  }))
  if (simulationProcess == "dynamic"){
    NonVegetatedClasses <- c(25, 33, 36, 39)  
  } else {
    NonVegetatedClasses <- c(11:12, 31:33)
  }
  message(crayon::yellow(paste0("non-vegetated classes set to: ", 
                               paste(NonVegetatedClasses, collapse = ", "))))
  # Barren Land = 25, 33 (in EOSD Exposed Land)
  # Snow/Ice = 39, 31 (in EOSD Snow/Ice)
  # Rocks = 33, 32 (in EOSD Rock/Rubble)
  # Urban = 36, NA
  # NoData = NA, 11:12 (in EOSD shadow:cloud)
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
    pixelsSum <- raster::calc(binaryLayersStack, fun = sum, na.rm = TRUE, 
                              filename = file.path(Paths$outputPath,
                                                   "tmp_fireCalcLay"),
                              overwrite = TRUE, format = "GTiff")
    testthat::expect_true(all.equal(sort(unique(pixelsSum[])), c(0, 1)),
                          label = "S layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }
  focalMatrix <- circularWindow(ras = rstLCC, 
                                focalDistance = 1000)
  # I need to calculate the denominator matrix to avoid study area border effects
  # This will return a raster with the number of pixels available in the neighborhood
  # of each pixel
  denominatorRaster <- raster::focal(x = rasterToMatch, w = focalMatrix, 
                                     fun = sum, na.rm = TRUE)
  # Broadleaf 1km proportion
  # ‘broadleaf dense’, ‘broadleaf open’, ‘mixedwood open’, and ‘mixedwood dense’ 
  
  # Proportions: Prepare the biomass maps
    # Classes we don't have in the models (for simulationProcess == "dynamic"):
    # *Conifer dense = 1 --> Conifer dense was ommited from the model to serve as
    #                         reference category. According to DeMars, 2019:
    #                         "For local land-cover type, we created a binary variable 
    #                         for each type and set ‘conifer dense’ as the reference category by
    #                         omitting it from the models."
    # **Mixedwood Sparse = 15 --> As of Jan 11th this class doesn't exist
    # # Conifer open = 6
    # # Conifer sparse = 8 
    # # Broadleaf dense = 2,
    # # Broadleaf open = 11,
    # # Mixedwood open = 13,
    # # Mixedwood dense = 3

  # Broadleaf (and mixed, open and dense) 1km proportion
    broadleafClasses <- as.numeric(classTable[fixedLayers %in% c("mix_open", "mix_dens",
                                                                 "broad_dens", "broad_open"), classCode])
    broadleafMap <- raster(rstLCC)
    broadleafMap[rstLCC[] %in% broadleafClasses] <- 1
    t_broad <- raster::focal(x = broadleafMap, w = focalMatrix, fun = sum, na.rm = TRUE)
    p_broad <- t_broad/denominatorRaster
    names(p_broad) <- "p_broad"

    # Conifer Sparse 1km proportion
    coniferClasses <- as.numeric(classTable[fixedLayers %in% c("con_sp"), classCode]) # Only confer sparse!
    coniferMap <- raster(rstLCC)
    coniferMap[rstLCC[] %in% coniferClasses] <- 1
    t_consparse <- raster::focal(x = coniferMap, w = focalMatrix, fun = sum, na.rm = TRUE)
    p_consparse <- t_consparse/denominatorRaster
    names(p_consparse) <- "p_consparse"

  # Assertions
  if (makeAssertions){
    message("Verifying the constructed proportion layers...")
    pixelsSum <-  calc(raster::stack(p_broad, p_consparse), fun = sum, na.rm = TRUE, 
                       filename = file.path(Paths$outputPath,
                                            "tmp_fireCalcLay"),
                       overwrite = TRUE, format = "GTiff")

    testthat::expect_true(all(minValue(pixelsSum) == 0, maxValue(pixelsSum) == 1), 
                          label = "Proportion layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }

  simulLayers <- raster::stack(simulatedLayers, nonveg, 
                               p_broad, p_consparse)
  return(simulLayers)
}