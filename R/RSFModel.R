RSFModel <- function(coeffTablAndValues,
                     modLayers,
                     currentTime,
                     pathData,
                     binningTable,
                     runName,
                     pathOut,
                     makeMap = FALSE,
                     shp,
                     cropRSFToShp){

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
                            runName = runName,
                            binningTable = binningTable,
                            shp = shp,
                            makeMap = makeMap,
                            cropToShp = cropRSFToShp,
                            rasName = c("relativeSelection", 
                                        "relativeSelectionUncertain"),
                            pathOut = pathOut)
return(predAndUncertain)

    })
    names(modsPred) <- names(coeffTablAndValues)
    return(modsPred)
}