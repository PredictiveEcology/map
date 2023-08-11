#' Make tiles (pyramids) using `gdal2tiles`
#'
#' @param tilePath A director to write tiles
#' @param obj A raster object with or without file-backing
#' @param overwrite Logical. If `FALSE`, and the director exists,
#'   then it will not overwrite any files.
#' @param ... Passed to `reproducible::projectInputs` e.g., `useGDAL`
#'
#' @export
makeTiles <- function(tilePath, obj, overwrite = FALSE, ...) {
  dirNotExist <- !dir.exists(tilePath) | isTRUE(overwrite)

  if (!is.na(tilePath) && dirNotExist) {
    ## assume that tilePath is unique for that obj, via .robustDigest
    message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
    objLflt <- try(projectTo(obj, projectTo = crs("epsg:4326"), ...), silent = TRUE)
    browser() ## TODO: above fails with:
    ##> unable to find an inherited method for function ‘res’ for signature ‘"character"’
    if (nchar(filename(objLflt)) == 0) {
      tmpFile <- tempfile(fileext = ".tif")
      message("                   writing to disk")
      objLflt <- try(writeRaster(objLflt, tmpFile), silent = TRUE)
    } else {
      tmpFile <- filename(objLflt)
    }

    toDo <- TRUE
    tryNum <- 1
    while (toDo) {
      print(tryNum)
      isCorrectCRS <- compareCRS(crs("epsg:4326"), objLflt)
      #browser()
      out <- try(tiler::tile(tmpFile, tilePath, zoom = "1-10",
                             crs = as(sf::st_crs("epsg:4326"), "CRS"),
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
    message("  Tiles - skipping creation - directory")
    message(reproducible::normPath(tilePath))
    message(" already exists or is not specified")
  }
}
