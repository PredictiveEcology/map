## OLD versions of functions, kept to test re-implementations

.gdal_polygonizeR <- function(x, outshape = NULL, gdalformat = "ESRI Shapefile", # nolint
                               pypath = NULL, readpoly = TRUE, quiet = TRUE) {
  if (is.null(pypath)) {
    pypath <- Sys.which("gdal_polygonize.py")
    if (!nzchar(pypath)) {
      stop("Can't find gdal_polygonize.py on your system.")
    }
  }

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
    raster::writeRaster(x, rastpath, datatype = raster::dataType(x))
  } else if (is(x, "SpatRaster")) {
    f <- tempfile(fileext = ".tif")
    rastpath <- normalizePath(f, mustWork = FALSE)
    terra::writeRaster(x, rastpath, datatype = terra::datatype(x))
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else {
    stop("x must be a file path (character string), Raster, or SpatRaster object.")
  }
  system2(tiler::tiler_options()[["python"]],
          args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s"',
                          pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- sf::st_read(dsn = dirname(outshape),
                       layer = basename(file_path_sans_ext(outshape)),
                       quiet = TRUE)

    return(shp) ## returns an sf POLYGONS object
  } else {
    return(NULL)
  }
}
