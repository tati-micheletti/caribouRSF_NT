composeFireLayers <- function(currentTime,
                              pathData, # To compare to current time. First time needs 
                              # to be different as we are creating layers, not updating them
                              historicalFires,
                              species,
                              fireLayers,
                              cohortData = NULL,
                              pixelGroupMap = NULL,
                              decidousSp,
                              makeAssertions = TRUE,
                              simulationProcess,
                              landClasses = c("Lowlands", "UplandsNonTreed",
                                              "UplandsConifer", "UplandsBroadleaf"),
                              yearClasses = c(10, 20, 30, 40, 60),
                              # For correspondingClassesValues see: https://drive.google.com/drive/u/0/folders/1911W_RGwcC36HovCHtpvf806w2GHDiuO
                              # This is how the model was built by GNWT
                              correspondingClassesValues = list(
                                "dynamic" = list(
                                  "Lowlands" = c(8, 17, 19, 31:32),
                                  "UplandsNonTreed" = c(23, 16, 18),
                                  "UplandConifer" = c(1, 6),
                                  "UplandBroadleaf" = c(2, 3, 11, 13) # 15 shouldn't exist (broadleaf sparse)
                                ),
                                # Converted EOSD to LCC05 values
                                "static" = list(
                                  "Lowlands" = c(81:83, 100, 213),
                                  "UplandsNonTreed" = c(40, 51, 52),
                                  "UplandConifer" = c(211, 212),
                                  "UplandBroadleaf" = c(221, 222, 231, 232)
                                )
                              ), # Original EOSD values
                              # "UplandsConifer" & "UplandsBroadleaf" come from biomass!
                              thisYearsFires,
                              rstLCC){
  
  firesFilenameRas <- file.path(pathData, "historicalFireRaster.tif")
  firesFilenameList <- file.path(pathData, "historicalFireList.qs")
  
  # Currently, the function will implement the current fires from the simulation. 
  if (is.null(fireLayers)){ # This is for the first year, to create the fire layers
    if (!file.exists(firesFilenameRas)){
      if (!file.exists(firesFilenameList)){
      fireRas <- rstLCC
      fireRas[!is.na(fireRas)] <- 0
      tsRas <- lapply(unique(historicalFires$fireYear), function(YYYY){
        message(paste0("Fires for year ", YYYY, " being processed..."))
        subst <- historicalFires[historicalFires$fireYear == YYYY, ]
        if (!length(subst) == 0){
          substSF <- sf::st_as_sf(subst)
          yearFire <- suppressWarnings(fasterize::fasterize(sf = substSF, 
                                                            raster = rstLCC, 
                                                            field = "fireYear"))
          fireRas[yearFire == YYYY] <- YYYY
        }
        return(fireRas)
      })
      names(tsRas) <- paste0("Year", unique(historicalFires$fireYear))
      tsRas <- lapply(names(tsRas), function(r){
        ras <- tsRas[[r]]
        names(ras) <- paste0("historicalFires", r)
        return(ras)
      })
      names(tsRas) <- paste0("Year", unique(historicalFires$fireYear))
      qs::qsave(tsRas, file = firesFilenameList)
    } else {
      tsRas <- qs::qread(firesFilenameList)
    }
    # This is the first year. I need to create one historical raster of burns with years as counters
    # Place the maximum year of the list in the pixel
    counterRaster <- raster::calc(raster::stack(tsRas), fun = max, na.rm = TRUE)
    names(counterRaster) <- tools::file_path_sans_ext(basename(firesFilenameRas))
    writeRaster(counterRaster, firesFilenameRas, format = "GTiff")
    } else {
      counterRaster <- raster::raster(firesFilenameRas)
    }
    # Make sure fire layer matches rstLCC
    counterRaster <- tryCatch({
      stk <- raster::stack(counterRaster, rstLCC)
      counterRaster
    }, error = function(e){
      counterRaster <- postProcess(counterRaster, 
                                   rasterToMatch = rstLCC, 
                                   destinationPath = pathData)
      return(counterRaster)
    })
  } else {
    # Move one year from the previous, using thisYearsFires
    counterRaster <- raster::raster(firesFilenameRas)
    # 1. Determine which years are in "thisYearsFires"
    minYear <- 1+(currentTime - max(yearClasses))
    maxYearFromData <- maxValue(counterRaster)
    # 2. Get years we don't have from data from simulation
    subThisYears <- raster::stack(lapply(maxYearFromData:currentTime, function(Y){
      y <- thisYearsFires[[grep(Y, names(thisYearsFires))]]
      y[y > 0] <- Y
      names(y) <- paste0("Year", Y)
      y <- postProcess(y, 
                       rasterToMatch = rstLCC, 
                       destinationPath = pathData)
      return(y)
      }))

    # 3. Add the new years
    counterRaster <- raster::calc(raster::stack(counterRaster, 
                                                subThisYears), 
                                  fun = max, 
                                  na.rm = TRUE, 
                                  filename = file.path(Paths$rasterPath,
                                                       paste0("tmp_fireCalcLay",
                                                              basename(tempfile(pattern = "")))),
                                  overwrite = TRUE, format = "GTiff")
    counterRaster[] <- counterRaster[]
    # 4. Delete years outside of the range
    counterRaster[counterRaster < minYear] <- 0
  }
  # Classify fire layer
  allClasses <- expand.grid(landClasses, yearClasses)
  names(allClasses) <- c("landClasses", "yearClasses")
  burnedLayers <- lapply(1:NROW(allClasses), function(rowIndex){
    message(paste0("Class ", paste0("burned", allClasses[rowIndex, "landClasses"],
                                    allClasses[rowIndex, "yearClasses"], 
                                    "y"), " being processed..."))
    yearClassesSeq <- c(0, yearClasses)
    # 1. Identify which are the min and max
    maxFireYear <- currentTime - yearClassesSeq[which(yearClassesSeq == allClasses[rowIndex, "yearClasses"])-1]
    minFireYear <- 1+(currentTime - yearClassesSeq[which(yearClassesSeq == allClasses[rowIndex, "yearClasses"])])
    burnClass <- allClasses[rowIndex, "landClasses"]
    # 2. Make the landcover classes
    rstLCCclasses <- correspondingClassesValues[[simulationProcess]][[burnClass]]
    currRas <- raster(rstLCC)
    currRas[rstLCC[] %in% rstLCCclasses] <- 1
    
    # 3. Need to identify which pixels of this specific class burned
    counterRasterThisClass <- raster(counterRaster)
    counterRasterThisClass[counterRaster[] %in% minFireYear:maxFireYear] <- 1
    # overlay fire raster (counterRasterThisClass) with vegetation raster (currRas)
    thisClassPixels <- raster(counterRaster)
    thisClassPixels[counterRasterThisClass[] == 1 & currRas[] == 1] <- 1
    names(thisClassPixels) <- paste0("burned", paste0(allClasses$landClasses[rowIndex], 
                                                  allClasses$yearClasses[rowIndex]), "y")
    return(thisClassPixels)
  })
  names(burnedLayers) <- paste0("burned", paste0(allClasses$landClasses,
                                                 allClasses$yearClasses), "y")
  burnedLayers <- raster::stack(burnedLayers)
  
  if (makeAssertions){
    # Assertion!!
    # Each pixel can be only one classification, so the sum of the stack 
    # needs to be 1 or NA for all pixels
    message("Verifying the constructed fire layers...")
    pixelsSum <- raster::calc(burnedLayers, fun = sum, na.rm = TRUE, 
                              filename = file.path(Paths$rasterPath,
                                                   paste0("tmp_fireCalcLay",
                                                          basename(tempfile(pattern = "")))),
                              overwrite = TRUE, format = "GTiff")
    pixelsSum[] <- pixelsSum[]
    testthat::expect_true(all.equal(sort(unique(pixelsSum[])), c(0, 1)), 
                          label = "Fire layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }
  return(burnedLayers)
}
