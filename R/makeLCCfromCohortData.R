makeLCCfromCohortData <- function(cohortData,
                                  pixelGroupMap,
                                  lccClassTable = data.table(
                                    standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                                                     "pureCon_sparse",
                                                     "pureBroad_dense", "pureBroad_open", "pureBroad_sparse",
                                                     "mixed_dense", "mixed_open", "mixed_sparse"),
                                                          LCCclass = c(1,6,8,32,
                                                                       2,11,11,
                                                                       3,13,13)), # HARDCODED TO MATCH LCC05
                                  deciduousCoverDiscount,
                                  leadingSpThreshold,
                                  decidousSp,
                                  rstLCC){

  # NOTE ON SPARSE CLASSES:
  # Even though the input landcover raster does NOT have mixed or broadleadf sparse, these
  # will appear once we start doing simulations from originally pure conifer sparse sites (i.e.
  # broadleaf trees might grow on these sites, and eventually convert these into mixed or
  # broadleaf sparse sites). Once I do the final classification map, though, these broadleaf and
  # mixed 'sparse' sites are converted to 'open' as the caribou model does not have coefficients
  # for broadleaf and mixed 'sparse' classifications.

  # We need to convert cohortData into landcover class
  # First, I need to define the category of the pixel: broadleaf, mixedwood, conifer (STEP 1);
  # Then, I need to define the "openness" of it: dense, open, sparse (STEP 2 -- this step, however,
  # starts earlier on, when I fit the model in the first year)

  # ---- STEP 1
  # 1. Make a table of species
  species <- as.character(unique(cohortData[["speciesCode"]]))
  treeSpecies <- data.table(speciesCode = c(decidousSp,
                                            species[!species %in% decidousSp]),
                            treeRSF = c(rep("broadleaf",
                                            times = length(decidousSp)),
                                        rep("conifer",
                                            times = length(species[!species %in% decidousSp]))))

  cohortData <- merge(cohortData, treeSpecies, by = "speciesCode", all.x = TRUE)
  if (NROW(cohortData) == 0)
    stop(paste0("cohortData became empty after being merged with tree types.",
                "This might happen if the cohortData has species codes that do not match ",
                "the parameter decidousSp (i.e., camel case vs all upper case). Please ",
                "make sure that the parameter decidousSp is provided in the same pattern ",
                "as cohortData"))
  # 2. Calculate species cover based on biomass. NOTE: in itself, this cover makes no sense
  # but as a percentage, we can use to apply the threshold to define the leading sp in the pixel
  # correctly
  cohortData[, coverIndex := fifelse(treeRSF == "broadleaf",
                                    B/deciduousCoverDiscount,
                                    B)]

  # 3. Define which ones are conifers and which are broadleaf (pixelGroup 8929 to check)
  cohortData[, totalCoverIndex := sum(coverIndex), by = c("pixelGroup")]
  # 4. Calculate the sum of treeRSF per pixel group
  cohortData[, treeTypeCoverIndex := sum(coverIndex), by = c("pixelGroup", "treeRSF")]
  # 5. Calculate the percentage of forest biomass that is
  cohortData[, percTree := treeTypeCoverIndex/totalCoverIndex,
             by = c("pixelGroup", "treeRSF") ]
  # 6. Simplify and dcast cohortData to be able to compare the percentages
  cohortDataSim <- unique(cohortData[, c("pixelGroup", "treeRSF", "percTree")])
  cohortDataD <- dcast(data = cohortDataSim, formula = pixelGroup ~ treeRSF,
                       fill = 0)
  # 7. Define first if a stand is pure or mixes:
  cohortDataD[, pureBroad := fifelse(broadleaf >= leadingSpThreshold, 1, 0)]
  cohortDataD[, pureCon := fifelse(conifer >= leadingSpThreshold, 1, 0)]
  cohortDataD[, standLeading := colnames(.SD)[max.col(.SD, ties.method="first")],
              .SDcols = c("pureBroad", "pureCon")]
  cohortDataD[, standLeading := fifelse(pureBroad+pureCon == 0, "mixed", standLeading)]
  # 8. Simplifying and recodeing
  cohortDataSim <- unique(cohortDataD[, c("pixelGroup", "standLeading")])

  # --- STEP 2: define the "openness"
  # 2. Take LCC classes and divide them into "closed", "sparse", "open"
  # to create a new "sparseness map"
  sparsenessMap <- rstLCC
  sparsenessMap[!sparsenessMap[] %in% lccClassTable[["LCCclass"]]] <- NA
  sparse <- lccClassTable[["LCCclass"]][grep(pattern = "sparse", x = lccClassTable[["standLeading"]])]
  open <- lccClassTable[["LCCclass"]][grep(pattern = "open", x = lccClassTable[["standLeading"]])]
  dense <- lccClassTable[["LCCclass"]][grep(pattern = "dense", x = lccClassTable[["standLeading"]])]
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
  sparsenessMapDT <- unique(na.omit(data.table::data.table(getValues(stack(sparsenessMap,
                                                                           pixelGroupMap)))))
  finalDT  <- merge(cohortDataSim, sparsenessMapDT,
                    by.x = "pixelGroup",
                    by.y = names(pixelGroupMap),
                    all.x = TRUE)
  finalDT <- na.omit(merge(finalDT, data.table(sparsenessMap = c(1,2,3),
                                       sparseness = c("dense", "open", "sparse")),
                   by = "sparsenessMap",
                   all.x = TRUE))
  finalDT[, standLeading  := paste(standLeading, sparseness, sep = "_")]
  # Because we have 2 categories of pureCon_sparse (8 and 32 -- which is treed lichen bog or treed wetland),
  # we need to remove one from the lccClassTable
  lccClassTable <- unique(lccClassTable, by = "standLeading")
  finalDT <- merge(finalDT, lccClassTable, by = "standLeading", all.x = TRUE)

  # Get the new classes to the LCC where they are supposed to be
  newLCCClass <- SpaDES.tools::rasterizeReduced(reduced = finalDT,
                                                fullRaster = pixelGroupMap,
                                                newRasterCols = "LCCclass",
                                                mapcode = "pixelGroup")
  DT <- data.table(pixelID = 1:ncell(newLCCClass),
                   getValues(stack(rstLCC, newLCCClass)))
  names(DT) <- c("pixelID", "LCC", "newLCC")
  DT[, updatedLCC := fifelse(!is.na(newLCC), newLCC, LCC)]
  updatedLCCras <- raster::setValues(x = raster(rstLCC),
                                     values = DT[["updatedLCC"]])
  return(updatedLCCras)
}
