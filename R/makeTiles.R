#' Make tiles (pyramids) using gdal2tiles
#'
#' @importFrom raster projectRaster writeRaster
#' @importFrom sp CRS
#' @importFrom tiler tile
#' @param tilePath A director to write tiles
#' @param obj A raster objects with or without file-backing
#' @param overwrite Logical. If \code{FALSE}, and the director exists,
#'   then it will not overwrite any files.
#' @param ... Passed to reproducible::projectInputs e.g., \code{useGDAL}
#' @export
makeTiles <- function(tilePath, obj, overwrite = FALSE, ...) {
  dirNotExist <- !dir.exists(tilePath) | isTRUE(overwrite)

  if (dirNotExist) { # assume that tilePath is unique for that obj, via .robustDigest
    if (reproducible:::isWindows()) {
      out <- reproducible:::findGDAL()
      if (isFALSE(out))
        stop("Need to have gdal installed; see ?tiler")
      pydir <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path)
      possPyBin <- file.path(pydir, "python.exe")
      possPyBin3 <- file.path(pydir, "python3.exe")

      pythonPth <- if (file.exists(possPyBin)) {
        possPyBin
      } else if (file.exists(possPyBin3)) {
        possPyBin3
      } else {
        stop("Can't find python.exe or python3.exe")
      }
      tiler::tiler_options(python = pythonPth)

      out <- findOSGeo4W() # set path in tiler::tiler_options
      if (length(out) == 0)
        stop("Need to have OSGeo4W installed; see ?tiler")
    }
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


findOSGeo4W <- function() {
  if (reproducible:::.requireNamespace("gdalUtils")) {
    gdalPath <- NULL
    attemptGDAL <- TRUE
    if (reproducible:::isWindows()) {
      # Handle all QGIS possibilities
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
    }
    tiler::tiler_options(osgeo4w = OSGeo4WPath)

    OSGeo4WPath

  }
}
