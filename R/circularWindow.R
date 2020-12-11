circularWindow <- function(ras, focalDistance){
  ras <- res(ras)
  nx <- 1 + 2 * floor(focalDistance/ras[1])
  ny <- 1 + 2 * floor(focalDistance/ras[2])
  m <- matrix(ncol = nx, nrow = ny)
  m[ceiling(ny/2), ceiling(nx/2)] <- 1
  if (nx == 1 & ny == 1) {
    return(m)
  } else {
    x <- raster(m, xmn = 0, xmx = nx * ras[1], ymn = 0, ymx = ny * 
                  ras[2], crs = "+proj=utm +zone=1 +datum=WGS84")
    focalDistance <- matrix(raster::distance(x), ncol = raster::ncol(x), nrow = raster::nrow(x)) <= focalDistance
    m <- matrix(as.numeric(focalDistance), ncol = base::ncol(focalDistance), nrow = base::nrow(focalDistance))
    return(m)
  }
}
