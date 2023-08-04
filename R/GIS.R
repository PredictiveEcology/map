#' `areaAndPolyValue`
#'
#' Determine the area of each zone in a raster. TODO: improve description
#'
#' @param ras A `Raster*` object
#'
#' @return list containing: `sizeInHa`, the area; and `polyID`, the polygon ID.
#'
#' @export
areaAndPolyValue <- function(ras) {
  polyIndivSpecies <- gdal_polygonizeR(ras)
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}

#' Polygonize a raster
#'
#' @param x a `RsaterLayer`, `SpatRaster`, or character giving the filepath to a raster.
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
    x <- rast(x)
  }
  shp <- as.polygons(x)
  writeVector(x, outshape, filetype = gdalformat)

  if (isTRUE(readpoly)) {
    return(st_as_sf(shp))
  } else {
    return(NULL)
  }
}

#' `.rasterToMemory`
#'
#' @param x A `Raster*` object
#'
#' @param ... Additional arguments passed to `raster`
#'
#' @export
#' @rdname rasterToMemory
.rasterToMemory <- function(x, ...) {
  r <- raster(x, ...)
  r <- raster::setValues(r, raster::getValues(r))
  return(r)
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
