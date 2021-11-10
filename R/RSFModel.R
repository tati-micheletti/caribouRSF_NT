RSFModel <- function(coeffTablAndValues,
                     modLayers,
                     currentTime,
                     pathData,
                     runName,
                     binningTable,
                     pathOut,
                     shp,
                     cropRSFToShp = TRUE){

  message("Forecasting caribou habitat from RSF ...")
    modsPred <- lapply(X = names(coeffTablAndValues), FUN = function(modelType) {
      covTable <- data.table::data.table(raster::getValues(modLayers))
    
responseList <- generateRSFPredictions(covTable = covTable,
                    coeffTable = coeffTablAndValues[[modelType]][["coeffTable"]],
                    coeffValues = coeffTablAndValues[[modelType]][["coeffValues"]],
                    modelType = modelType)

predAndUncertain <- generateRSFRas(modelType = modelType, 
                            templateRas = modLayers[[1]], 
                            currentTime = currentTime, 
                            responseTable = responseList,
                            binningTable = binningTable,
                            runName = runName,
                            shp = shp,
                            cropToShp = cropRSFToShp,
                            rasName = c("relativeSelection", 
                                        "relativeSelectionUncertain"),
                            pathOut = pathOut)
return(predAndUncertain)

    })
    names(modsPred) <- names(coeffTablAndValues)
    return(modsPred)
}