#' Build \code{map} obj metadata table
#'
#' @param metadata TODO: description needed
#' @param isStudyArea TODO: description needed
#' @param isRasterToMatch Logical. Is this(these) layer(s) the \code{rasterToMatch} layers.
#'        If \code{TRUE}, then this layer can be accessed by \code{rasterToMatch(map)}
#' @param layerName TODO: description needed
#' @param obj TODO: description needed
#' @param columnNameForLabels TODO: description needed
#' @param objHash TODO: description needed
#' @param leaflet Logical or Character vector of path(s) to write tiles.
#'  If \code{TRUE} or a character vector, then this layer will be added to a leaflet map.
#'  For \code{RasterLayer} object, this will trigger a call to \code{gdal2tiles}, making tiles.
#'  If path is not specified, it will be the current path.
#'  The tile base file path will be \code{paste0(layerName, "_", rndstr(1, 6))}.
#' @param envir TODO: description needed
#' @param ... Additional arguments.
#'
#' @rdname buildMetadata
buildMetadata <- function(metadata, isStudyArea, isRasterToMatch, layerName, obj,
                          columnNameForLabels, objHash, leaflet, envir, ...) {
  b <- copy(.singleMetadataNAEntry)
  dots <- list(...)

  # If it is studyArea
  if (isTRUE(isStudyArea)) {
    area <- rgeos::gArea(obj)
    studyAreaNumber <- 1 + NROW(metadata[compareNA(studyArea, TRUE) |
                                               (is.numeric(studyArea) & studyArea > 0)])
    set(b, NULL, "studyArea", studyAreaNumber)
    set(b, NULL, "area", area)
  }

  if (isTRUE(isRasterToMatch)) {
    rasterToMatchNumber <- 1 + NROW(metadata[compareNA(rasterToMatch, TRUE) |
                                           (is.numeric(rasterToMatch) & rasterToMatch > 0)])
    set(b, NULL, "rasterToMatch", rasterToMatchNumber)
  }

  if (!is.null(dots$url))
    set(b, NULL, "url", dots$url)

  set(b, NULL, "layerName", layerName)
  set(b, NULL, "layerType", class(obj))
  if (length(columnNameForLabels) > 0) {
    if (is(obj, "SpatialPolygonsDataFrame")) {
      set(b, NULL, "columnNameForLabels", columnNameForLabels)
    }
  }
  set(b, NULL, "objectHash", objHash)

  if (!isFALSE(leaflet)) {
    set(b, NULL, "leaflet", leaflet)
    if (is(obj, "Raster")) {
      dig <- .robustDigest(obj)
      tilePath <- ifelse(is.na(leaflet), asPath(NA_character_),
                         asPath(file.path(leaflet, paste0("tiles_", layerName, "_", substr(dig, 1, 6))))) # nolint
      set(b, NULL, "leafletTiles", tilePath)
    }
  }

  set(b, NULL, "envir", list(list(envir)))

  # Add all extra columns to metadata
  if (length(dots)) {
    dots <- dots[!unlist(lapply(dots, is.null))] # remove NULL because that isn't added to data.table anyway
    if (!is.null(dots$filename2)) {
      if (!isFALSE(dots$filename2)) {
        if (inherits(obj, "RasterLayer")) {
          if (endsWith(dots$filename2, suffix = "tif")) {
            if (raster::is.factor(obj)) {
              dots$filename2 <- basename(raster::filename(obj))
            }
          }
        }
      }
    }
  }
  columnsToAdd <- dots

  # Add columns by reference to "b"
  Map(cta = columnsToAdd, nta = names(columnsToAdd),
      function(cta, nta) {
        ## a data.table can't handle all types of objects. need to wrap in a list to stick it there
        ## try first without a list wrapper, then try once with a list.
        needToSet <- TRUE
        tries <- 0
        while (isTRUE(needToSet) && tries < 2) {
          needToSet <- tryCatch(set(b, NULL, nta, cta), silent = TRUE, error = function(x) TRUE)
          tries <- tries + 1
          cta <- list(cta)
        }
      })

  return(b)
}
