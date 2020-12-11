generateRSFRas <- function(modelType, templateRas, currentTime, 
                           responseTable, rasName, pathOut,
                           shp){

  # According to DrMars et al., 2019: 10 bins were created, with each bin containing the same number 
  # of points
  # Bin the results before putting in the raster
  RSF <- responseTable$RSF
  RSFsd <- responseTable$sdRSF
  RSFbinned <- ggplot2::cut_number(x = RSF, n = 10, label = FALSE)
  RSFSDbinned <- ggplot2::cut_number(x = RSFsd, n = 10, label = FALSE)
  
  # Work back the missing pixels using responseTable$
  fullTable <- data.table(pixelID = 1:ncell(templateRas))
  availablePixTable <- data.table(pixelID = responseTable$pixelID,
                                  RSF = RSFbinned,
                                  RSFsd = RSFSDbinned)
  RSFtable <- merge(fullTable, availablePixTable, all = TRUE)
  RSFtable[, pixelID := NULL]
  names(RSFtable) <- rasName
  
  rasList <- lapply(X = seq_len(length(rasName)), FUN = function(r){
    ras <- raster::setValues(x = raster(templateRas), 
                             values = RSFtable[, get(rasName[r])])
    names(ras) <- paste0(rasName[r], modelType, "_Year", currentTime)
    ras[] <- ras[]
    writeRaster(x = ras, 
                filename = file.path(pathOut, 
                                     paste0(rasName[r], modelType, 
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
    shpLoaded <- readShapeLines(pathSHP)
    shpProj <- raster::crs(shp)

    # Add shp to levelplot
    colPal <- colorspace::diverge_hcl(n = 10, rev = TRUE)
   print(levelplot(ras,
              sub = paste0("Caribou RSF in 2011"),
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
              col.regions = colPal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
              par.strip.text = list(cex = 0.8,
                                    lines = 1,
                                    col = "black"),
              panel = function(...){
                panel.levelplot.raster(...)
                sp.polygons(shpLoaded, fill = 'black', lwd = 1)
              }))
   
    dev.off()
    
    return(ras)
  })
  names(rasList) <- rasName
  return(rasList)
}