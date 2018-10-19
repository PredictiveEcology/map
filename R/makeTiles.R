makeTiles <- function(tilePath, object) {
  dirNotExist <- !dir.exists(tilePath)

  if (dirNotExist) { # assume that tilePath is unique for that object, via .robustDigest
    object[] <- object[]
    message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
    message("                   writing to disk")
    objectLflt <- try(projectRaster(object, crs = CRS("+init=epsg:4326")), silent = TRUE)
    tmpFile <- tempfile(fileext = ".tif")
    objectLflt <- try(writeRaster(objectLflt, tmpFile), silent = TRUE)

    toDo <- TRUE
    tryNum <- 1
    while (toDo) {
      print(tryNum)
      out <- try(tiler::tile(tmpFile, tilePath, zoom = "1-10", crs = CRS("+init=epsg:4326"),
                  format = "tms", viewer = FALSE, resume = TRUE), silent = TRUE)
      toDo <- is(out, "try-error")
      files <- dir(tilePath, recursive = TRUE)
      if (length(files) < 5) {
        unlink(tilePath, recursive = TRUE)
        toDo <- TRUE
      }

      tryNum <- tryNum + 1
    }

  } else {
    message("  Tiles - skipping creation - already exist")
  }

}
