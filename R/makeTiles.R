#' Make tiles (pyramids) using `gdal2tiles`
#'
#' @param tilePath A director to write tiles
#'
#' @param obj A raster object with or without file-backing
#'
#' @param overwrite Logical. If `FALSE`, and the director exists,
#'   then it will not overwrite any files.
#'
#' @param ... Arguments passed to [reproducible::projectInputs()] (e.g., `useGDAL`).
#'
#' @export
makeTiles <- function(tilePath, obj, overwrite = FALSE, ...) {
  stopifnot(is(obj) %in% c("Raster", "SpatRaster"))

  if (is(obj, "Raster")) {
    obj <- terra::rast(obj)
  }

  dirNotExist <- !dir.exists(tilePath) | isTRUE(overwrite)

  if (!is.na(tilePath) && dirNotExist) {
    ## assume that tilePath is unique for that obj, via .robustDigest
    message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
    objLflt <- try({
      ## TODO: using projectTo() fails; reproducible#355
      # reproducible::projectTo(obj, projectTo = sf::st_crs("epsg:4326"), ...)
      terra::project(obj, "epsg:4326", ...)
    }, silent = TRUE)
    fname <- reproducible::Filenames(objLflt)

    if (length(fname) == 0 | nchar(fname) == 0) {
      tmpFile <- tempfile(fileext = ".tif")
      message("                   writing to disk")
      objLflt <- try({
        terra::writeRaster(objLflt, tmpFile)
      }, silent = TRUE)
    } else {
      tmpFile <- fname
    }

    toDo <- TRUE
    tryNum <- 1
    while (toDo) {
      print(tryNum)
      isCorrectCRS <- terra::same.crs("epsg:4326", objLflt)
      out <- try({
        tiler::tile(tmpFile, tilePath, zoom = "1-10",
                    crs = as(sf::st_crs("epsg:4326"), "CRS"),
                    format = "tms", viewer = FALSE, resume = TRUE)
      }, silent = TRUE)
      toDo <- is(out, "try-error")
      files <- dir(tilePath, recursive = TRUE)
      if (length(files) < 5) {
        unlink(tilePath, recursive = TRUE)
        toDo <- TRUE
      }

      tryNum <- tryNum + 1
    }
  } else {
    message("  Tiles - skipping creation - directory")
    message(reproducible::normPath(tilePath))
    message(" already exists or is not specified")
  }
}
