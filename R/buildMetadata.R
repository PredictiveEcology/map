#' Build `map` obj metadata table
#'
#' @param metadata TODO: description needed
#' @param isStudyArea TODO: description needed
#' @param isRasterToMatch Logical. Is this(these) layer(s) the `rasterToMatch` layers.
#'        If `TRUE`, then this layer can be accessed by `rasterToMatch(map)`
#' @param layerName TODO: description needed
#' @param obj TODO: description needed
#' @param columnNameForLabels TODO: description needed
#' @param objHash TODO: description needed
#' @param leaflet Logical or Character vector of path(s) to write tiles.
#'  If `TRUE` or a character vector, then this layer will be added to a leaflet map.
#'  For `RasterLayer` or `SpatRaster` object, this will make tiles.
#'  If path is not specified, it will be the current path.
#'  The tile base file path will be `paste0(layerName, "_", rndstr(1, 6))`.
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
    area <- (if (is(obj, "sf")) obj else sf::st_as_sf(obj)) |>
      sf::st_area()
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

  if (!is.null(dots[["url"]]))
    set(b, NULL, "url", dots[["url"]])

  set(b, NULL, "layerName", layerName)
  set(b, NULL, "layerType", class(obj)[1])
  if (length(columnNameForLabels) > 0) {
    if (is(obj, "SpatialPolygonsDataFrame") || is(obj, "sf")) {
      set(b, NULL, "columnNameForLabels", columnNameForLabels)
    }
  }
  set(b, NULL, "objectHash", objHash)

  if (isFALSE(leaflet)) {
    set(b, NULL, "leafletTiles", asPath(NA_character_))
  } else {
    set(b, NULL, "leaflet", leaflet)
    if (is(obj, "Raster") || is(obj, "SpatRaster")) {
      dig <- .robustDigest(obj)
      defaultTilePath <- file.path(leaflet, paste0("tiles_", layerName, "_", substr(dig, 1, 6)))
      tilePath <- ifelse(is.na(leaflet), NA_character_, defaultTilePath)
      set(b, NULL, "leafletTiles", asPath(tilePath))
    } else {
      set(b, NULL, "leafletTiles", asPath(NA_character_))
    }
  }

  set(b, NULL, "envir", list(list(envir)))

  # Add all extra columns to metadata
  if (length(dots)) {
    dots <- dots[!unlist(lapply(dots, is.null))] # remove NULL because that isn't added anyway
    if (!is.null(dots$filename2)) {
      if (!isFALSE(dots$filename2)) {
        if (is(obj) %in% c("RasterLayer", "SpatRaster")) {
          if (endsWith(dots$filename2, suffix = "tif")) {
            if (is.factor(obj)) {
              dots$filename2 <- basename(reproducible::Filenames(obj))
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
      }
  )

  b <- .enforceColumnTypes(b)

  return(b)
}

#' Ensure columns in metadata table are a particular type (class)
#'
#' @param metadata
#'
#' @return metadata `data.table` with corrected column types
#'
#' @keywords internal
#'
.enforceColumnTypes <- function(metadata) {
  ## NOTE (2019-11-08): targetCRS needs to be character, not CRS/crs due to change in data.table
  if (!is.null(metadata[["targetCRS"]]) && !is(metadata[["targetCRS"]], "character"))
    metadata[["targetCRS"]] <- as.character(metadata[["targetCRS"]])

  ## TODO: manual workarounds to deal with column typing for LandWeb
  if (!is.null(metadata[["destinationPath"]]) && !is(metadata[["destinationPath"]], "Path"))
    set(metadata, NULL, "destinationPath", asPath(metadata[["destinationPath"]]))

  if (!is.null(metadata[["leaflet"]]) && !is(metadata[["leaflet"]], "Path"))
    set(metadata, NULL, "leaflet", asPath(metadata[["leaflet"]]))

  if (!is.null(metadata[["leafletTiles"]]) && !is(metadata[["leafletTiles"]], "Path"))
    set(metadata, NULL, "leafletTiles", asPath(metadata[["leafletTiles"]]))

  if (!is.null(metadata[["targetFile"]]) && !is(metadata[["targetFile"]], "Path"))
    set(metadata, NULL, "targetFile", asPath(metadata[["targetFile"]]))

  if (!is.null(metadata[["tsf"]]) && !is(metadata[["tsf"]], "Path"))
    set(metadata, NULL, "tsf", asPath(metadata[["tsf"]]))

  if (!is.null(metadata[["vtm"]]) && !is(metadata[["vtm"]], "Path"))
    set(metadata, NULL, "vtm", asPath(metadata[["vtm"]]))

  return(metadata)
}
