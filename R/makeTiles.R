#' @keywords internal
.isWindows <- getFromNamespace("isWindows", "reproducible")

#' Make tiles (pyramids) using `gdal2tiles`
#'
#' @param tilePath A director to write tiles
#' @param obj A raster objects with or without file-backing
#' @param overwrite Logical. If `FALSE`, and the director exists,
#'   then it will not overwrite any files.
#' @param ... Passed to `reproducible::projectInputs` e.g., `useGDAL`
#'
#' @export
#' @importFrom raster compareCRS filename projectRaster writeRaster
#' @importFrom sp CRS
#' @importFrom tiler tile
makeTiles <- function(tilePath, obj, overwrite = FALSE, ...) {
  dirNotExist <- !dir.exists(tilePath) | isTRUE(overwrite)

  if (dirNotExist) { # assume that tilePath is unique for that obj, via .robustDigest
    .setTilerPythonPath()

    obj[] <- obj[]
    message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
    objLflt <- try(projectInputs(obj, targetCRS = CRS("+init=epsg:4326"), ...), silent = TRUE)
    # objLflt <- try(projectRaster(obj, crs = CRS("+init=epsg:4326")), silent = TRUE)
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
      isCorrectCRS <- compareCRS(CRS("+init=epsg:4326"), objLflt)
      #browser()
      out <- try(tiler::tile(tmpFile, tilePath, zoom = "1-10",
                             crs = CRS("+init=epsg:4326"),
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
    message(" already exists")
  }
}

#' @importFrom reproducible .requireNamespace
findOSGeo4W <- function() {
  if (.isWindows()) {
    if (reproducible::.requireNamespace("gdalUtils")) {
      gdalPath <- NULL
      attemptGDAL <- TRUE

      ## Handle all QGIS possibilities
      a <- dir("C:/", pattern = "Progra", full.names = TRUE)
      a <- grep("Program Files", a, value = TRUE)
      a <- unlist(lapply(a, dir, pattern = "QGIS", full.name = TRUE))
      # a <- unlist(lapply(a, dir, pattern = "bin", full.name = TRUE))

      possibleWindowsPaths <- c(a, "C:/OSGeo4W64/",
                                "C:/GuidosToolbox/QGIS/",
                                "C:/GuidosToolbox/guidos_progs/FWTools_win/",
                                "C:/Program Files (x86)/Quantum GIS Wroclaw/",
                                "C:/Program Files/GDAL",
                                "C:/Program Files (x86)/GDAL")
      message("Searching for OSGeo4W installation")
      paths <- file.path(possibleWindowsPaths, "OSGeo4W.bat")
      OSGeo4WExists <- file.exists(paths)
      if (any(OSGeo4WExists))
        OSGeo4WPath <- paths[OSGeo4WExists]

      tiler::tiler_options(osgeo4w = OSGeo4WPath)

      OSGeo4WPath
    }
  }
}
