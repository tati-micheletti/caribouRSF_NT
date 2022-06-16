#Will need to edit this function to point at the correct seasons seletion rations
#CALVING - Calving_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MVxNKeznzM0o_LV2QIeeJGxrh27r3xkJ/view?usp=sharing
#SUMMER - Summer_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1M64c9Kn4W4--_KNWSiHhqrWgCW_83xHc/view?usp=sharing
#EARLYFALL- EarlyFall_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MVKxby_XsboGMBndhzfLIDF8vE6_IrT8/view?usp=sharing
#LATEFALL- LateFall_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MMcjtskZMF5N7mCtoWBba33z1aLcqoHz/view?usp=sharing
#EARLYWINTER- EarlyWinter_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MS4M_jlgJjymteOj0qoKuhbcoo7IhWg0/view?usp=sharing
#MIDWINTER- MidWinter_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1M7Gb0RopBQmjAROwqVFOWtc5JJghl38Z/view?usp=sharing
#LATEWINTER- LateWinter_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MCGW6O2EGjufXF9m2lT3lz5DtijFeyu7/view?usp=sharing
#ALLYEAR- AllYear_noMac_SelectionRatios_2021010120.csv-https://drive.google.com/file/d/1MhEx2BCdYElMgg1SxDZTMt8yA06EE7rH/view?usp=sharing

binRSFtoDeMars2019 <- function(ras){
  message("Loading binning table...")
  binningTable <- Cache(prepInputs, 
                        targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                        url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                        destinationPath = Paths[["inputPath"]], 
                        fun = "data.table::fread",
                        userTags = c("object:binningTable"))
  reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                                binningTable[["Max.Value"]],
                                binningTable[["RSF.Bin"]]), 
                          ncol = 3)
  # Make sure that the max value is infinite, so it accommodates any bigger value
  # than before
  reclassMatrix[nrow(reclassMatrix), 2] <- Inf
  rasBinned <- raster::reclassify(x = ras, 
                                  rcl = reclassMatrix)
  return(rasBinned)
}