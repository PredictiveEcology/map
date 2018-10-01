#' The \code{map} class
#'
#' Contains a common system for organzing vector and raster
#' layers, principly for use with leaflet and shiny.
#'
#' @slot metadata    List of character names specifying which modules to load.
#'
#' @slot maps Named list of map-type objects (e.g., \code{sf}, \code{Raster*},
#'            \code{Spatial*}.
#'
#' @slot rasterTiles Paths to rasterTiles
#'
#' @slot CRS  The common crs of all layers
#' @slot rasterTemplate The empty raster with all metadata filled
#' @slot analyses    A data.table or data.frame of the types of analyses to perform
#'
#' @slot analysesData A data.table or data.frame of the results of the analyses
#'
#' @aliases map
#' @rdname map-class
#' @exportClass map
#' @importFrom data.table data.table
#' @importFrom raster crs raster
setClass(
  "map",
  slots = list(
    metadata = "data.table",
    maps = "environment",
    rasterTiles = "character",
    CRS = "CRS",
    rasterTemplate = "RasterLayer",
    analyses = "data.table",
    analysesData = "list"
  ),
  prototype = list(
    metadata = data.table(layerName = character(), layerType = character(),
                          sourceURL = character(),
                          columnNameForLabels = character(),
                          leafletVisible = logical(), studyArea = logical()),
    maps = new.env(),
    rasterTiles = character(),
    rasterTemplate = NULL,
    CRS = sp::CRS(),
    analyses = data.table::data.table(),
    analysesData = list()
  ),
  validity = function(object) {
    #if (is.na(object@simtimes$end)) {
    #  stop("simulation end time must be specified.")
    #} else {
    #  if (object@simtimes$start > object@simtimes$end) {
    #    stop("simulation end time cannot be before start time.")
    #  }
    #}
  }
)


#' Append a spatial object to map
#' @export
#' @examples
#' library(sp)
#' library(raster)
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
#'                       59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' ml <- new("map")
#' appendMapList(StudyArea, ml, studyArea = TRUE, layerName = "newPoly")
#'
appendMapList <- function(object, map, layerName, overwrite = FALSE, ...)
  UseMethod("appendMapList")

#' @export
#' @importFrom reproducible prepInputs
appendMapList.default <- function(object = NULL, map = NULL,
                                  layerName = NULL, overwrite = FALSE,
                                  sourceURL = NULL,
                                  columnNameForLabels = character(),
                                  leafletVisible = TRUE, studyArea = FALSE, ...) {
  if (is.null(map)) {
    map <- new("map")
  }
  if (is.null(object)) {
    if (is.null(sourceURL)) {
      stop("Must provide either object or sourceURL")
    } else {
      object <- prepInputs(url = sourceURL)
    }
    map <- appendMapList(object, map = map, layerName = layerName,
                         overwrite = overwrite,
                         sourceURL = sourceURL, columnNameForLabels = columnNameForLabels,
                         leafletVisible = leafletVisible, studyArea = studyArea)
  }

  map
}

#' @export
#' @importFrom reproducible fixErrors projectInputs postProcess
#' @importFrom data.table rbindlist
#' @param ... passed to reproducible::postProcess and reproducible::projectInputs and
#'            reproducible::fixErrors
appendMapList.SpatialPolygons <- function(object, map = NULL, layerName = NULL,
                                          overwrite = FALSE, sourceURL = NULL,
                                          columnNameForLabels = NULL,
                                          leafletVisible = TRUE, studyArea = NULL, ...) {
  mustOverwrite <- if (isTRUE(layerName %in% ls(map@maps))) {
    if (isTRUE(overwrite)) {
      message(layerName, " already in map; overwriting")
    } else {
      stop(layerName, " already in map; stopping. Want overwrite = TRUE?")
    }
    TRUE
  } else {
    FALSE
  }
  if (is.null(studyArea(map))) {
    object <- fixErrors(object, ...)
    if (isFALSE(studyArea)) {
      message("There is no studyArea in map; consider adding one with 'studyArea = TRUE'")
    }
    if (is.na(crs(map))) {
      message("No crs already in map, so no reprojection")
    } else {
      object <- projectInputs(object, targetCRS = crs(map), ...)
    }
  } else {
    if (is.na(crs(map))) {
      message("There is no CRS already in map; using the studyArea CRS and adding that to map")
    } else {
      object <- postProcess(object, studyArea = studyArea(map), ...)
    }
  }

  # Put map into map slot
  assign(layerName, object, envir = map@maps) # this overwrites, if same name
  if (mustOverwrite) {
    ln <- layerName
    map@metadata <- map@metadata[!(layerName %in% ln)]
  }

  b <- .singleMetadataNAEntry
  if (isTRUE(studyArea)) {
    if (!is.null(studyArea(map))) {
      message("map already has a studyArea; adding another one as study area ",
              1 + NROW(map@metadata[studyArea == TRUE]))
    } else {
      message("Setting map CRS to this layer because it is the (first) studyArea inserted")
      map@CRS <- raster::crs(object)
    }
    set(b, , "studyArea", TRUE)
  }
  if (!is.null(sourceURL))
    set(b, , "sourceURL", sourceURL)
  set(b, , "layerName", layerName)
  set(b, , "layerType", class(object))
  if (!is.null(columnNameForLabels)) {
    if (is(object, "SpatialPolygonsDataFrame")) {
      set(b, , "columnNameForLabels", columnNameForLabels)
    }
  }
  if (isFALSE(leafletVisible))
    set(b, , "leafletVisible", leafletVisible)

  map@metadata <- rbindlist(list(map@metadata, b), use.names = TRUE, fill = TRUE)
  return(map)
}



#' @export
removeMap <- function(map, layer, ask = TRUE, ...)
  UseMethod("removeMap")

removeMap.default <- function(map = NULL,
                              layer = NULL, ask = TRUE, ...) {
  if (is.null(map)) {
    stop("Must pass a map")
  }
  if (is.character(layer))
    layer <- map@metadata[, which(layerName %in% layer) ]

  layerName <- unique(map@metadata[ layer , layerName])
  if (length(layer > 1))
    stop("There are more than object in map with that layer name, '",
         layerName,"'. Please indicate layer ",
         "by row number in map@metadata")

  rm(layerName)
  browser()
}

#' @importMethodsFrom raster crs
#' @importFrom raster crs
#' @exportMethod crs
setMethod("crs",
          signature = "map",
          function(x, ...) {
            if (!is.null(x@CRS))
              x@CRS
            else
              NA
          })

#' @export
studyAreaName <- function(map, layer = 1) {
  if (sum(map@metadata$studyArea)) {
    map@metadata[studyArea == TRUE, layerName][layer]
  } else {
    NULL
  }
}

#' @export
studyArea <- function(map, layer = 1) {
  if (sum(map@metadata$studyArea)) {
    get(map@metadata[studyArea == TRUE, layerName][layer], map@maps)
  } else {
    NULL
  }
}

#' @export
rasters <- function(map) {
  lsObjs <- ls(ml@maps)
  logicalRasters <- unlist(lapply(mget(lsObjs, ml@maps), is, "RasterLayer"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], ml@maps)
  } else {
    NULL
  }
}


#' @export
spatialPolygons <- function(map) {
  lsObjs <- ls(ml@maps)
  logicalRasters <- unlist(lapply(mget(lsObjs, ml@maps), is, "SpatialPolygons"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], ml@maps)
  } else {
    NULL
  }
}

#' @export
spatialPoints <- function(map) {
  lsObjs <- ls(ml@maps)
  logicalRasters <- unlist(lapply(mget(lsObjs, ml@maps), is, "SpatialPoints"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], ml@maps)
  } else {
    NULL
  }
}

#' @export
maps <- function(map) {
  lsObjs <- ls(ml@maps)
  mget(lsObjs, ml@maps)
}

.singleMetadataNAEntry <-
  data.table::data.table(layerName = NA_character_, layerType = NA_character_,
                         sourceURL = NA_character_,
                         columnNameForLabels = NA_character_,
                         leafletVisible = TRUE, studyArea = FALSE)


