makeLCCfromCohortData <- function(cohortData,
                                  pixelGroupMap,
                                  classTable = data.table(standLeading = c("pureCon_dense", "pureCon_open", 
                                                                           "pureCon_sparse", "pureBroad_dense",
                                                                           "pureBroad_open", "mixed_dense", 
                                                                           "mixed_sparse", " mixed_open"), 
                                                          LCCclass = c(1,6,8,2,11,3,13,15)), # HARDCODED TO MATCH LCC05
                                  deciduousCoverDiscount,
                                  species,
                                  leadingSpThreshold,
                                  opennessMod,
                                  decidousSp){
  # We need to convert cohortData into landcover class
  # First, I need to define the category of the pixel: broadleaf, mixedwood, conifer (STEP 1); 
  # Then, I need to define the "openness" of it: dense, open, sparse (STEP 2 -- this step, however, 
  # starts earlier on, when I fit the model in the first year)
  
  # ---- STEP 1
  # 1. Make a table of species
  treeSpecies <- data.table(speciesCode = c(decidousSp,
                                            species[!species %in% decidousSp]),
                            treeRSF = c(rep("broadleaf", 
                                            times = length(decidousSp)),
                                        rep("conifer", 
                                            times = length(species[!species %in% decidousSp]))))
  cohortData <- merge(cohortData, treeSpecies)
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

  browser() 
  
}