#' Determine the area of each zone in a raster
#'
#' Polygonize a raster and calculate the area of each polygon.
#'
#' @param ras A `Raster` or `SpatRaster` object
#' @param ... Additional arguments (not used)
#'
#' @return list containing: `sizeInHa`, the area; and `polyID`, the polygon ID.
#'
#' @export
#' @rdname areaAndPolyValue
areaAndPolyValue <- function(ras, ...) {
  UseMethod("areaAndPolyValue", ras)
}

#' @export
#' @rdname areaAndPolyValue
areaAndPolyValue.Raster <- function(ras, ...) {
  polyIndivSpecies <- gdal_polygonizeR(ras)
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}

#' @export
#' @rdname areaAndPolyValue
areaAndPolyValue.SpatRaster <- function(ras, ...) {
  polyIndivSpecies <- gdal_polygonizeR(ras)
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}

#' Polygonize a raster
#'
#' @param x a `RasterLayer`, `SpatRaster`, or character giving the filepath to a raster.
#'
#' @param outshape character giving the filepath for the output shapefile.
#'
#' @param gdalformat GDAL driver to use. See `terra::gdal(drivers=TRUE)`.
#'
#' @param readpoly logical indicating whether the polygons object should be returned
#'        (this was previously using `gdal_polygonize` on disk and required reading the file)
#'
#' @param pypath,quiet deprecated. maintained for backwards compatibility only (not used).
#'
#' @return if `readpoly = TRUE` (default), an `sf` polygons object; otherwise, `NULL`.
#'
gdal_polygonizeR <- function(x, outshape = NULL, gdalformat = "ESRI Shapefile", # nolint
                             pypath = NULL, readpoly = TRUE, quiet = TRUE) {
  if (is.null(outshape)) {
    outshape <- tempfile(fileext = ".shp")
  }
  if (is(x, "Raster") || is(x, "character")) {
    x <- terra::rast(x)
  }
  if (terra::is.factor(x)) {
    x <- as.numeric(x)
  }

  p <- terra::as.polygons(x) |>
    terra::disagg()  ## to get POLYGONS from MULTIPOLYGONS
  names(p) <- "DN" ## for backwards compatibility

  terra::writeVector(p, outshape, filetype = gdalformat)

  if (isTRUE(readpoly)) {
    return(p)
  } else {
    return(NULL)
  }
}

#' `.rasterToMemory`
#'
#' @param x A `Raster` or `SpatRaster` object
#'
#' @param ... Additional arguments passed to raster read function
#'
#' @export
#' @rdname rasterToMemory
.rasterToMemory <- function(x, ...) {
  UseMethod(".rasterToMemory", x)
}

#' @export
#' @rdname rasterToMemory
.rasterToMemory.Raster <- function(x, ...) {
  raster::raster(x, ...) |>
    raster::setValues(raster::getValues(x))
}

#' @export
#' @rdname rasterToMemory
.rasterToMemory.SpatRaster <- function(x, ...) {
  terra::rast(x, ...) |>
    terra::setValues(terra::values(x))
}

#' Fasterize with crop & spTransform first
#'
#' @param emptyRaster An empty raster with res, crs, extent all
#'        correct for to pass to `fasterize`
#' @param polygonToFasterize passed to `fasterize`, but it
#'        will be cropped first if
#'        `extent(emptyRaster) < extent(polygonToFasterize)`
#' @param field passed to `fasterize`
#'
#' @export
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
  levels(aa2) <- data.frame(ID = seq_along(thePoly[[field]]),
                            Factor = as.character(thePoly[[field]]),
                            as.data.frame(thePoly))

  aa2
}
