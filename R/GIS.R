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
#' @return if `readpoly = TRUE` (default), a `SpatVector` object; otherwise, `NULL`.
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
    terra::disagg() ## to get POLYGONS from MULTIPOLYGONS
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

#' Rasterize following crop and reproject
#'
#' A simple wrapper around [terra::rasterize()].
#'
#' @param emptyRaster An empty `RasterLayer` or`SpatRaster` to use as a template.
#'
#' @param polygonToFasterize an `sf` or `SpatVector` object, which will be cropped first
#'        if `extent(emptyRaster) < extent(polygonToFasterize)`.
#'
#' @inheritParams terra::rasterize
#'
#' @return an object of the same class as `emptyRaster`
#'
#' @export
#'
#' @examples
#'
#' ## using sf + raster
#' f1 <- system.file("external/lux.shp", package = "raster")
#' v1 <- sf::st_read(f1)
#' r1 <- raster::raster(v1, ncols = 75, nrows = 100)
#' raster::crs(r1) <- "epsg:4326"
#' z1 <- fasterize2(r1, v1, "NAME_2")
#'
#' ## using terra
#' f2 <- system.file("ex/lux.shp", package = "terra")
#' v2 <- terra::vect(f2)
#' r2 <- terra::rast(v2, ncols = 75, nrows = 100)
#' z2 <- fasterize2(r2, v2, "NAME_2")
#'
#' terra::compareGeom(terra::rast(z1), z2)
fasterize2 <- function(emptyRaster, polygonToFasterize, field) {
  asRaster <- ifelse(is(emptyRaster, "RasterLayer"), TRUE, FALSE)

  if (!is(polygonToFasterize, "SpatVector")) {
    polygonToFasterize <- terra::vect(polygonToFasterize)
  }

  ras <- terra::rast(emptyRaster)
  if (terra::ext(polygonToFasterize) > terra::ext(ras)) {
    polygonToFasterize <- reproducible::cropInputs(polygonToFasterize, rasterToMatch = ras) |>
      reproducible::Cache()
  }
  thePoly <- reproducible::projectInputs(polygonToFasterize, targetCRS = raster::crs(ras))
  if (!is(thePoly, "SpatVector")) {
    thePoly <- terra::vect(thePoly)
  }
  thePoly$polygonNum <- seq_along(thePoly)
  if (!is.factor(thePoly[[field]][[1]])) {
    thePoly[[field]][[1]] <- factor(thePoly[[field]][[1]])
  }
  aa2 <- terra::rasterize(thePoly, ras, field = field)
  # levels(aa2) <- data.frame(
  #   ID = seq_along(thePoly[[field]][[1]]),
  #   category = as.character(thePoly[[field]][[1]]),
  #   as.data.frame(thePoly)
  # )

  if (isTRUE(asRaster)) {
    return(raster::raster(aa2))
  } else {
    return(aa2)
  }
}
