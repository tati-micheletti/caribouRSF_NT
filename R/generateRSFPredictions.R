generateRSFPredictions <- function(covTable,
                                   coeffTable,
                                   coeffValues,
                                   modelType){
  tic("Elapsed time for caribou prediction:")
  if (length(modelType)>1)
    stop("For now, caribouRSF_NT only has one model (caribouRSF_NT)")
  # Simplifying the covariates table
  covTable[, pixelID := 1:NROW(covTable)]
  library("matrixStats")
  covTable[, isNA := rowMeans(.SD, na.rm = TRUE), 
             .SDcols = names(covTable)[names(covTable) != "pixelID"]]
  # Remove the ones that are NA, while keeping track of pixelID
  covTableRed <- covTable[!is.na(isNA), ]
  pixID <- covTableRed$pixelID
  covTableRed[, c("pixelID", "isNA") := NULL]
  # Fill up zeroes!
  for (j in seq_len(ncol(covTableRed))){
    set(covTableRed, which(is.na(covTableRed[[j]])), j, 0)
  }
  # The matrix multiple -- will result in 100 columns
  # Coefficients (coeffTable); Covariates (covTableRed)
  # This results in a pixel * bootstrap replicate (rows x columns) 
  predictedTableSD <- exp(as.matrix(covTableRed) %*% 
                                   t(coeffTable[,-which(colnames(coeffTable) %in% c("Intercept", 
                                                                                "intercept"))]))
  # My matrix has 7910521 rows (pixels) and 100 columns (replicates)

  # Uncertainty across replicates
  predictedSD <- matrixStats::rowSds(predictedTableSD)

  # Now the RSF calculations
  predictedRSF <- as.numeric(exp(as.matrix(covTableRed) %*%
                        as.matrix(coeffValues)[-which(colnames(coeffValues) %in% c("Intercept",
                                                                           "intercept"))]))
  toc()
  return(list(RSF = predictedRSF, 
              sdRSF = predictedSD, 
              pixelID = pixID))
}
