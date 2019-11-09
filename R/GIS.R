#' \code{areaAndPolyValue}
#'
#' Determine the area of each zone in a raster. TODO: improve description
#'
#' @param ras A \code{Raster*} object
#'
#' @return list containing: \code{sizeInHa}, the area; and \code{polyID}, the polygon ID.
#'
#' @export
#' @importFrom sf st_area
areaAndPolyValue <- function(ras) {
  polyIndivSpecies <- gdal_polygonizeR(ras) # 99 seconds with full ras
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}

#' Polygonize with GDAL
#'
#' Based on
#' \url{https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/}.
#'
#' @param x TODO: description needed
#' @param outshape TODO: description needed
#' @param gdalformat TODO: description needed
#' @param pypath TODO: description needed
#' @param readpoly TODO: description needed
#' @param quiet TODO: description needed
#'
#' @importFrom gdalUtils gdal_setInstallation
#' @importFrom raster extent writeRaster
#' @importFrom reproducible assessDataType
#' @importFrom sf st_bbox read_sf
#' @importFrom tools file_path_sans_ext
gdal_polygonizeR <- function(x, outshape = NULL, gdalformat = "ESRI Shapefile", # nolint
                             pypath = NULL, readpoly = TRUE, quiet = TRUE) {
  if (isTRUE(readpoly)) requireNamespace("rgdal", quietly = TRUE)
  if (is.null(pypath)) {
    pypath <- Sys.which("gdal_polygonize.py")
    if (!nzchar(pypath)) {
      browser()
      gdalUtils::gdal_setInstallation()
      o <- options()
      pypath <- file.path(o$gdalUtils_gdalPath[[2]]$path, "gdal_polygonize.py")
      if (!nzchar(pypath)) {
        stop("Need gdal_polygonize.py")
      }
    }
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (is.null(outshape)) {
    outshape <- tempfile(fileext = ".shp")
  } else {
    outshape <- sub("\\.shp$", "", outshape)
    fExists <- file.exists(paste(outshape, c("shp", "shx", "dbf"), sep = "."))
    if (any(fExists))
      stop(sprintf("File already exists: %s",
                   toString(paste(outshape, c("shp", "shx", "dbf"), sep = ".")[fExists])),
           call. = FALSE)
  }
  if (is(x, "Raster")) {
    f <- tempfile(fileext = ".tif")
    rastpath <- normalizePath(f, mustWork = FALSE)
    writeRaster(x, rastpath, datatype = assessDataType(x))

  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop("x must be a file path (character string), or a Raster object.")
  system2("python", args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s"',
                                    pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- sf::read_sf(dsn = dirname(outshape), layer = basename(file_path_sans_ext(outshape)))
    sf::st_bbox(shp, extent(x))
    return(shp)
  } else {
    return(NULL)
  }
}

#' \code{.rasterToMemory}
#'
#' @param x A \code{Raster*} object
#' @param ... Additional arguments passed to \code{raster}
#'
#' @export
#' @importFrom raster getValues raster setValues
#' @rdname rasterToMemory
.rasterToMemory <- function(x, ...) {
  r <- raster(x, ...)
  r <- raster::setValues(r, raster::getValues(r))
  return(r)
}

#' Fasterize with crop & spTransform first
#'
#' @param emptyRaster An empty raster with res, crs, extent all
#'        correct for to pass to \code{fasterize}
#' @param polygonToFasterize passed to \code{fasterize}, but it
#'        will be cropped first if
#'        \code{extent(emptyRaster) < extent(polygonToFasterize)}
#' @param field passed to \code{fasterize}
#'
#' @export
#' @importFrom fasterize fasterize
#' @importFrom raster crs extent raster
#' @importFrom reproducible Cache cropInputs projectInputs
#' @importFrom sf st_as_sf
fasterize2 <- function(emptyRaster, polygonToFasterize, field) {
  ras <- raster(emptyRaster)
  if (extent(polygonToFasterize) > extent(ras)) {
    polygonToFasterize <- Cache(cropInputs, polygonToFasterize, rasterToMatch = ras)
  }
  thePoly <- projectInputs(polygonToFasterize, targetCRS = crs(ras))
  thePoly$polygonNum <- seq_along(thePoly)
  if (!is.factor(thePoly[[field]])) {
    thePoly[[field]] <- factor(thePoly[[field]])
  }
  aa2 <- fasterize::fasterize(sf::st_as_sf(thePoly), ras, field = field)
  #levels(aa2) <- data.frame(ID = seq_len(nlevels(thePoly[[field]])),
  #                          Factor = levels(thePoly[[field]]))
  levels(aa2) <- data.frame(ID = seq_along(thePoly[[field]]),
                            Factor = as.character(thePoly[[field]]),
                            as.data.frame(thePoly))

  aa2
}
