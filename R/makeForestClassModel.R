makeForestClassModel <- function(pixelGroupMap,
                                 cohortData,
                                 rstLCC,
                                 classTable = data.table(standLeading = c("pureCon_dense", "pureCon_open", 
                                                                          "pureCon_sparse", "pureBroad_dense",
                                                                          "pureBroad_open", "mixed_dense", 
                                                                          "mixed_sparse", " mixed_open"), 
                                                         LCCclass = c(1,6,8,2,11,3,13,15))){
  # 1. At initial conditions (i.e., cohortData at year start(sim)), create Oldest StandAge Map and Biomass Map
  ch <- copy(cohortData)
  ch[, totalBiomass := sum(B), by = "pixelGroup"]
  ch[, oldestStandAge := max(age), by = "pixelGroup"]
  # Remove duplicates
  ch <- unique(ch[, c("pixelGroup", "totalBiomass", "oldestStandAge")])
  oldestStandAgeMap <- SpaDES.tools::rasterizeReduced(reduced = ch, 
                                                      fullRaster = pixelGroupMap, 
                                                      newRasterCols = "oldestStandAge", mapcode = "pixelGroup")
  names(oldestStandAgeMap) <- "oldestStandAgeMap"
  totalBiomassMap <- SpaDES.tools::rasterizeReduced(reduced = ch, 
                                                      fullRaster = pixelGroupMap, 
                                                      newRasterCols = "totalBiomass", mapcode = "pixelGroup")
  names(totalBiomassMap) <- "totalBiomassMap"
  # 2. Take LCC classes and divide them into "closed", "sparse", "open" 
  # to create a new "sparseness map"
  sparsenessMap <- rstLCC
  sparsenessMap[!sparsenessMap[] %in% classTable[["LCCclass"]]] <- NA
  sparse <- classTable[["LCCclass"]][grep(pattern = "sparse", x = classTable[["standLeading"]])]
  open <- classTable[["LCCclass"]][grep(pattern = "open", x = classTable[["standLeading"]])]
  dense <- classTable[["LCCclass"]][grep(pattern = "dense", x = classTable[["standLeading"]])]
  # dense = 1; open = 2; sparse =  3
  sparsenessMap[sparsenessMap[] %in% dense] <- -1
  sparsenessMap[sparsenessMap[] %in% open] <- -2
  sparsenessMap[sparsenessMap[] %in% sparse] <- -3
  sparsenessMap <- -sparsenessMap
  
  sparsenessMap <- ratify(sparsenessMap)
  rat <- raster::levels(sparsenessMap)[[1]]
  rat$sparseness <- c("dense", "open", "sparse")
  levels(sparsenessMap) <- rat
  names(sparsenessMap) <- "sparsenessMap"
  # 3. Run a multinomial logit to get relationship between Sparseness ~ Biomass * Age
  # 3.1 Get data
  DT <- data.table(pixelID = 1:ncell(sparsenessMap),
                   raster::getValues(raster::stack(sparsenessMap, 
                                                   totalBiomassMap, 
                                                   oldestStandAgeMap)))
  DT <- DT[complete.cases(DT), ]
  DT[, sparsenessMap := factor(sparsenessMap)]
  if (FALSE){
    library(MASS)
    for (subsetSize in c(50, 80, 100, 150, 200, 300, 450, 800)){
      for (validationSize in c(100, 200, 400, 600, 1000, 3000))  {
        vals1 <- sample(DT[sparsenessMap == 1, pixelID], size = subsetSize, replace = FALSE)
        vals2 <- sample(DT[sparsenessMap == 2, pixelID], size = subsetSize, replace = FALSE)
        vals3 <- sample(DT[sparsenessMap == 3, pixelID], size = subsetSize, replace = FALSE)
        DTsub <- DT[pixelID %in% c(vals1, vals2, vals3), ]
        lccModel <- polr(sparsenessMap ~ totalBiomassMap * oldestStandAgeMap, data = DTsub, Hess = TRUE)
        expModel <- exp(coef(lccModel))
        lccModPred <- predict(lccModel, type = "class")
        summary(lccModPred)
        browser()
        res <- DTsub[1:validationSize, sparsenessMap]
        newData <- data.table(totalBiomassMap = DTsub[1:validationSize, totalBiomassMap],
                              oldestStandAgeMap = DTsub[1:validationSize, oldestStandAgeMap])
        newData <- cbind(newData, predict(lccModel, newdata = newData, type = "class"))
        newData[, predictedResult := colnames(.SD)[max.col(.SD, ties.method="first")],
                .SDcols = c("1", "2", "3")]
        newData[, originalResult := res]
        newData[, Matched := predictedResult == originalResult]
        newData <- na.omit(newData)
        message(paste0(100*sum(newData$Matched)/NROW(newData)), "% accuracy with ",
                subsetSize, " subsetSize and ", validationSize, " validation size")
      }
    } 
    
    browser()
    
  }
}