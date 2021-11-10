generateRSFRas <- function(modelType, 
                           templateRas, 
                           currentTime, 
                           responseTable, 
                           rasName, 
                           runName,
                           pathOut,
                           binningTable,
                           shp,
                           cropToShp = TRUE){

  DT <- data.table(RSF = responseTable$RSF,
                   RSFsd = responseTable$sdRSF,
                   pixelID = responseTable$pixelID)

  if (cropToShp){
    shpSF <- sf::st_as_sf(shp)
    shpSF$ID <- 1
    shpRas <- fasterize::fasterize(sf = shpSF, 
                                   raster = raster(templateRas), 
                                   field = "ID")
    DTshp <- data.table(pixelID = 1:ncell(shpRas),
                        inSHP = raster::getValues(shpRas))
    whichPixIDtoKeep <- DTshp[inSHP == 1, pixelID]
    DT <- DT[pixelID %in% whichPixIDtoKeep, ]
    templateRas <- shpRas
  }
  
  # According to DrMars et al., 2019: 10 bins were created, 
  # with each bin containing the same number of points
  # Bin the results before putting in the raster
  # Because the results are exactly the same for several points, 
  # the binning doesn't work to make the same number of points per bin.
  # If we add some noise, however, we can achieve the same number of points 
  # in each bin. But that doesn't make sense as it is completely unreproducible!
  
  # Work back the missing pixels using responseTable$
  fullTable <- data.table(pixelID = 1:ncell(templateRas))
  RSFtable <- merge(fullTable, DT, by = "pixelID", all.x = TRUE)
  setkey(RSFtable, "pixelID")
  RSFtable[, pixelID := NULL]
  # Pallete to match DeMars et al. 2019
  pal <- c("#c2473d",
           "#d96b52",
           "#e1936b",
           "#f4bd81",
           "#f8e8b0",
           "#ecedc2",
           "#bec3c7",
           "#95a3ca",
           "#6c7ed0",
           "#3260c6") # Handmade to match DeMars 2019
  names(RSFtable) <- rasName
  rasList <- lapply(X = seq_len(length(names(RSFtable))), FUN = function(r){
    ras <- raster::setValues(x = raster(templateRas),
                             values = RSFtable[, get(rasName[r])])
    names(ras) <- paste0(rasName[r], modelType, "_Year", currentTime)
    ras[] <- ras[]
    
    # Do the binning using the binning table
    ######### 
    # Old binning way
    # RSFbinned <- ggplot2::cut_number(x = ras[], n = 10, label = FALSE)
    # rasBinned <- raster::setValues(raster(ras), RSFbinned)
    # #####
if (names(ras) == "relativeSelection"){
  reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                                binningTable[["Max.Value"]],
                                binningTable[["RSF.Bin"]]), 
                          ncol = 3)
  # Make sure that the max value is infinite, so it accommodates any bigger value
  # than before
  reclassMatrix[nrow(reclassMatrix), 2] <- Inf
  rasBinned <- raster::reclassify(x = ras, rcl = reclassMatrix)
} else {
  RSFbinned <- ggplot2::cut_number(x = ras[], n = 10, label = FALSE)
  rasBinned <- raster::setValues(raster(ras), RSFbinned)
}
    writeRaster(x = ras, 
                filename = file.path(pathOut, 
                                     paste0(runName, "_", rasName[r], modelType, 
                                            "_Year", currentTime)),
                format = "GTiff", overwrite = TRUE)
    
    pngPath <- file.path(pathOut, paste0(rasName[r], modelType,
                                         "_Year", currentTime, ".png"))
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    png(filename = pngPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    
    pathSHP <- file.path(Paths$inputPath, "RSFshp.shp")
    if (!file.exists(pathSHP)){
      rgdal::writeOGR(obj = shp, dsn = Paths$inputPath, "RSFshp", 
                      driver = "ESRI Shapefile")
    }
    shpLoaded <- maptools::readShapeLines(pathSHP)
    shpProj <- raster::crs(shp)

    # Add shp to levelplot
   if (rasName[r] == "relativeSelection"){
     Pal <- pal
     rasBinned <- ratify(rasBinned) 
     att <- "ID"
   } else {
     Pal <- viridis_pal(option = "D")(10) 
     att <- NULL
   }
   print(rasterVis::levelplot(rasBinned,
              sub = paste0("Caribou RSF in ", currentTime),
              att = att,
              margin = FALSE,
              maxpixels = 6e6,
              colorkey = list(
                space = 'bottom',
                at = 1:10,
                axis.line = list(col = 'black'),
                width = 0.75
              ),
              par.settings = list(
                strip.border = list(col = 'transparent'),
                strip.background = list(col = 'transparent'),
                axis.line = list(col = 'transparent')),
              scales = list(draw = FALSE),
              col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
              par.strip.text = list(cex = 0.8,
                                    lines = 1,
                                    col = "black"),
              panel = function(...){
                lattice::panel.levelplot.raster(...)
                sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
              }))
   
    dev.off()
    
    return(ras)
  })
  names(rasList) <- rasName
  return(rasList)
}