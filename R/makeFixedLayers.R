makeFixedLayers <- function(fireLayers,
                            pathData,
                            classTable,
                            rstLCC,
                            makeAssertions = TRUE){
  # 1. Make the following layers (once EOSD is directly used, we need to review this):
  templateRas <- raster(rstLCC)
  # In LCC05:
  # water = 37
  # Bryoids = 23
  # Tall shrub = 16
  # Short shrub = 18
  # Treed wetland = 32
  # Shrub wetland = 31
  # Herb wetland = 19
  # Herb = 17
  # 1B. Exclude the ones that were burned in any moment
  anyBurns <- raster::calc(fireLayers, sum, na.rm = TRUE, 
                           filename = file.path(Paths$rasterPath,
                                                paste0("tmp_fireCalcLay",basename(tempfile(pattern = "")))),
                           overwrite = TRUE, format = "GTiff")
  # Bring anyBurns to memory as the operation saves a raster in scratch, 
  # which gets cleaned up soon after
  anyBurns[] <- anyBurns[]
  rowOfFixedLayers <- 8 #TODO # Softcode!! Will need to clear cache, no time now
  fixedLayers <- raster::stack(lapply(1:rowOfFixedLayers, function(index){
    message(paste0("Building ", classTable[["fixedLayers"]][index]," layer..."))
    templateRas[rstLCC[] == classTable[["classCode"]][index]] <- 1
    templateRas[anyBurns[] == 1] <- 0 # Exclude the ones that were burned in any moment
    names(templateRas) <- classTable[["fixedLayers"]][index]
    return(templateRas)
  }))
  # 2. Make sure the layers do not overlap
  if (makeAssertions){
    # Assertion!!
    # Each pixel can be only one classification, so the sum of the stack 
    # needs to be 1 or NA for all pixels
    message("Verifying the constructed fire layers...")
    pixelsSum <- raster::calc(fixedLayers, fun = sum, na.rm = TRUE, 
                              filename = file.path(Paths$rasterPath,
                                                   paste0("tmp_fireCalcLay",basename(tempfile(pattern = "")))),
                              overwrite = TRUE, format = "GTiff")
    pixelsSum[] <- pixelsSum[]
    testthat::expect_true(all.equal(sort(unique(pixelsSum[])), c(0, 1)), 
                          label = "Fixed layers were not correctly built. Please debug. 
                          Sum of layers == 1 ")
    message("Verification complete! Layers were correctly built.")
  }

  return(fixedLayers)
}