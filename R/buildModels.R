buildModels <- function(caribouCoefTable){
  equation <- createEquation(caribouCoefTable)
  modList <- list(equation)
  names(modList) <- "caribouRSF_NT"
  return(modList)
}