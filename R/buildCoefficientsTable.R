buildCoefficientsTable <- function(caribouCoefTable, nBootstrap){
  
  # Get bootstrapped coefficients
  allCoeffs <- caribouCoefTable[["Coefficient"]]
  coeffTable <- lapply(X = allCoeffs, function(coeff){
    if (coeff %in% c("Intercept", "intercept")){
      return(rep(as.numeric(caribouCoefTable[Coefficient == coeff, "Value"]), 
                 times = nBootstrap))
    } else {
      vec <- rnorm(n = nBootstrap, 
                   mean = as.numeric(caribouCoefTable[Coefficient == coeff, "Value"]),
                   sd = as.numeric(caribouCoefTable[Coefficient == coeff, "StdErr"]))
      return(vec)
    }
  })
  names(coeffTable) <- allCoeffs

  # put into a matrix
  coeffMatrix <- do.call(cbind, coeffTable)

  coeffValues <- data.table(t(caribouCoefTable[, "Value"]))
  names(coeffValues) <- caribouCoefTable[["Coefficient"]]
  
# Make 2 lists with objects before putting in the caribouRSF_NT list  
# coeffTable
# coeffValues
  modList <- list(caribouRSF_NT = list(coeffTable = coeffMatrix,
                                       coeffValues = coeffValues))
  return(modList)
}