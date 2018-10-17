
buildMetadata <- function(metadata, isStudyArea, layerName,
                          object, columnNameForLabels, objHash, leaflet, envir, ...) {
  b <- copy(.singleMetadataNAEntry)
  dots <- list(...)

  # If it is studyArea
  if (isTRUE(isStudyArea)) {
    studyAreaNumber <- 1 + NROW(metadata[compareNA(studyArea, TRUE) |
                                               (is.numeric(studyArea) & studyArea > 0)])
    set(b, NULL, "studyArea", studyAreaNumber)
  }
  if (!is.null(dots$url))
    set(b, NULL, "url", dots$url)
  set(b, NULL, "layerName", layerName)
  set(b, NULL, "layerType", class(object))
  if (length(columnNameForLabels)>0) {
    if (is(object, "SpatialPolygonsDataFrame")) {
      set(b, NULL, "columnNameForLabels", columnNameForLabels)
    }
  }
  set(b, NULL, "objectHash", objHash)

  if (leaflet) {
    set(b, NULL, "leaflet", leaflet)
    if (is(object, "Raster")) {
      dig <- .robustDigest(object)
      tilePath <- asPath(paste0("tiles_", layerName, "_", substr(dig, 1,6)))
      set(b, NULL, "leafletTiles", tilePath)
    }
  }

  set(b, NULL, "envir", list(list(envir)))

  # Add all extra columns to metadata
  if (length(dots)) {
    dots <- dots[!unlist(lapply(dots, is.null))] # remove NULL because that isn't added to data.table anyway
    if (!is.null(dots$filename2)) {
      if (!isFALSE(dots$filename2)) {
        if (inherits(object, "RasterLayer")) {
          if (endsWith(dots$filename2, suffix = "tif")) {
            if (raster::is.factor(object)) {
              dots$filename2 <- basename(filename(object))
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
        # a data.table can't handle all types of objects ... need to wrap in
        #   a list to stick it there -- try first without a list wrapper, then
        #   try once with a list
        needToSet <- TRUE
        tries <- 0
        while (isTRUE(needToSet) && tries < 2) {
          needToSet <- tryCatch(set(b, NULL, nta, cta), silent = TRUE,
                                error = function(x) TRUE)
          tries <- tries + 1
          cta <- list(cta)
        }
      })

  return(b)

}
