makeTiles <- function(tilePath, object) {
  dirNotExist <- !dir.exists(tilePath)

  if (dirNotExist) {
    object[] <- object[]
    objectLflt <- projectRaster(object, crs = CRS("+init=epsg:4326"))
    tmpFile <- tempfile(fileext = ".tif")
    objectLflt <- writeRaster(objectLflt, tmpFile)
    message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
    message("Creating tiles")
    if (isTRUE(getOption("reproducible.useCache", FALSE)) ||
        getOption("reproducible.useCache", FALSE) == "overwrite")
      message("  using Cache. To prevent this, set options('reproducible.useCache' = FALSE)")
    tiler::tile(asPath(tmpFile), tilePath, zoom = "1-10", crs = CRS("+init=epsg:4326"),
                format = "tms", useCache = getOption("reproducible.useCache"))
  } else {
    message("  Tiles - skipping creation - already exist")
  }

}
