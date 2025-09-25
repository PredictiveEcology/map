#' Make raster tiles (pyramids)
#'
#' If python with GDAL support is available, uses `gdal2tiles` via [tiler::tile()]
#' to create tiles for use with e.g. `leaflet`.
#'
#' @note Requires python with GDAL support (see [tiler::tiler_options()]).
#' Windows users should install 'OSGeo4W' (<https://trac.osgeo.org/osgeo4w/>).
#'
#' @param tilePath A directory to write tiles
#'
#' @param obj A raster object with or without file-backing
#'
#' @param overwrite Logical. If `FALSE`, and the director exists,
#'   then it will not overwrite any files.
#'
#' @param ... Arguments passed to [reproducible::projectInputs()] (e.g., `useGDAL`).
#'
#' @return
#' - `makeTiles()` is invoked for it's side-effect of creating tiles in `tilePath`;
#' - `canMakeTiles()` returns a logical indicating the availability of python and GDAL support;
#'
#' @export
#' @rdname makeTiles
makeTiles <- function(tilePath = getOption("map.tilePath", "tiles"), obj, overwrite = FALSE, ...) {
  if (canMakeTiles()) {
    stopifnot(is(obj) %in% c("Raster", "SpatRaster"))

    if (is(obj, "Raster")) {
      obj <- terra::rast(obj)
    }

    dirNotExist <- !dir.exists(tilePath) | isTRUE(overwrite)

    if (!is.na(tilePath) && dirNotExist) {
      ## assume that tilePath is unique for that obj, via .robustDigest
      message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
      objLflt <- try(
        {
          ## TODO: using projectTo() fails; reproducible#355
          # reproducible::projectTo(obj, projectTo = sf::st_crs("epsg:4326"), ...)
          terra::project(obj, "epsg:4326", ...)
        },
        silent = TRUE
      )
      fname <- reproducible::Filenames(objLflt)

      if (length(fname) == 0 || nchar(fname) == 0) {
        tmpFile <- tempfile(fileext = ".tif")
        message("                   writing to disk")
        objLflt <- try(
          {
            terra::writeRaster(objLflt, tmpFile)
          },
          silent = TRUE
        )
      } else {
        tmpFile <- fname
      }

      toDo <- TRUE
      tryNum <- 1
      maxRetries <- getOption("map.tileRetry", 3L)
      while (toDo && tryNum <= maxRetries) {
        print(tryNum)
        isCorrectCRS <- terra::same.crs("epsg:4326", objLflt)
        out <- try(
          {
            tiler::tile(
              file = tmpFile,
              tiles = tilePath,
              zoom = "1-10",
              crs = as(sf::st_crs("epsg:4326"), "CRS"),
              resume = TRUE, viewer = FALSE, georef = TRUE
            )
          },
          silent = TRUE
        )
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
  } else {
    message("  Tiles - skipping creation - python or gdal support unavailable")
  }

  return(invisible(NULL))
}

#' @export
#' @rdname makeTiles
canMakeTiles <- function() {
  all(nzchar(tiler::tiler_options()))
}
