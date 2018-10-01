#' The \code{map} class
#'
#' Contains a common system for organzing vector and raster
#' layers, principly for use with leaflet and shiny.
#'
#' @slot metadata  \code{data.table} with columns describing metadata of map objects in
#'                 \code{maps} slot.
#'
#' @slot maps Named environment of map-type objects (e.g., \code{sf}, \code{Raster*},
#'            \code{Spatial*}. Each entry may also be simply an environment, which
#'            indicates where to find the object, i.e., via \code{get(layerName, envir = environment)}
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

setMethod("initialize", "map",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object@metadata = data.table(layerName = character(), layerType = character(),
                                          sourceURL = character(),
                                          columnNameForLabels = character(),
                                          leafletVisible = logical(), studyArea = logical())
            .Object@maps = new.env()
            .Object@rasterTiles = character()
            #.Object@rasterTemplate = NULL
            .Object@CRS = sp::CRS()
            .Object@analyses = data.table::data.table()
            .Object@analysesData = list()

            .Object
          })

#' Append a spatial object to map
#' @export
#' @rdname mapAdd
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
#' ml <- mapAdd(StudyArea, ml, studyArea = TRUE, layerName = "newPoly")
#'
#' if (requireNamespace("SpaDES.tools")) {
#'   smallStudyArea <- SpaDES.tools::randomPolygon(studyArea(ml), 1e2)
#'   ml <- mapAdd(smallStudyArea, ml, studyArea = TRUE, filename2 = NULL,
#'                envir = .GlobalEnv) # adds a second studyArea within 1st
#' }
#'
mapAdd <- function(object, map, layerName, overwrite = FALSE, ...)
  UseMethod("mapAdd")

#' @export
#' @rdname mapAdd
#' @importFrom reproducible prepInputs
#' @param ... passed to reproducible::postProcess and reproducible::projectInputs and
#'            reproducible::fixErrors and reproducible::prepInputs
mapAdd.default <- function(object = NULL, map = NULL,
                                  layerName = NULL, overwrite = FALSE,
                                  sourceURL = NULL,
                                  columnNameForLabels = character(),
                                  leafletVisible = TRUE, studyArea = FALSE, ...) {
  browser()
  if (is.null(map)) {
    map <- new("map")
  }
  if (is.null(object)) {
    if (is.null(sourceURL)) {
      stop("Must provide either object or sourceURL")
    } else {
      object <- prepInputs(url = sourceURL, ...)
    }
    map <- mapAdd(object, map = map, layerName = layerName,
                             overwrite = overwrite,
                             sourceURL = sourceURL, columnNameForLabels = columnNameForLabels,
                             leafletVisible = leafletVisible, studyArea = studyArea)
  }

  map
}

#' @export
#' @rdname mapAdd
#' @importFrom reproducible fixErrors projectInputs postProcess
#' @importFrom data.table rbindlist set
#' @param pointToEnvir An optional environment. If supplied, then the object
#'        will not be placed "into" the maps slot, rather the environment label will
#'        be placed into the maps slot. Upon re
mapAdd.SpatialPolygons <- function(object, map = NULL, layerName = NULL,
                                   overwrite = FALSE, sourceURL = NULL,
                                   columnNameForLabels = NULL,
                                   leafletVisible = TRUE, studyArea = NULL,
                                   pointToEnvir = NULL, ...) {
  browser()
  objectName <- deparse(substitute(object))
  objectEnv <- quickPlot::whereInStack(objectName)

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

  if (is.null(layerName)) {
    layerName <- objectName
  }

  if (is.null(pointToEnvir)) {
    # Put map into map slot
    assign(layerName, object, envir = map@maps) # this overwrites, if same name
  } else {

    if (exists(layerName, envir = pointToEnvir)) {
      assign(layerName, pointToEnvir, envir = map@maps)
    } else {
      stop("object named ", layerName, " does not exist in pointToEnvir: ", pointToEnvir)
    }
  }
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

  set(b, , "objectEnvironment", list(list(pointToEnvir)))
  set(b, , "objectName", objectName)

  map@metadata <- rbindlist(list(map@metadata, b), use.names = TRUE, fill = TRUE)
  return(map)
}



#' Remove objects from a \code{map}
#' @export
#' @family mapMethods
#' @inheritParams map-class
#' @rdname mapRm
mapRm <- function(map, layer, ask = TRUE, ...)
  UseMethod("mapRm")

#' @export
#' @aliases mapRm
#' @family mapMethods
#' @rdname mapRm
mapRm.default <- function(map = NULL,
                          layer = NULL, ask = TRUE, ...) {
  if (is.null(map)) {
    stop("Must pass a map")
  }
  if (is.character(layer))
    layer <- map@metadata[, which(layerName %in% layer) ]

  layerName
  browser()
  layerName <- unique(map@metadata[ layer , layerName])
  if (length(layer > 1))
    stop("There are more than object in map with that layer name, '",
         layerName,"'. Please indicate layer ",
         "by row number in map@metadata")

  rm(layerName)

}


if (!isGeneric("crs")) {
  setGeneric("crs", function(x, ...) {
    standardGeneric("crs")
  })
}

#' Extract the crs of a \code{map}
#' @importMethodsFrom raster crs
#' @importFrom raster crs
#' @exportMethod crs
#' @family mapMethods
#' @rdname crs
setMethod("crs",
          signature = "map",
          function(x, ...) {
            if (!is.null(x@CRS))
              x@CRS
            else
              NA
          })

#' Map class methods
#'
#' Tools for getting objects and metadata in and out of a \code{map} class.
#'
#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName <- function(map, layer)
  UseMethod("studyAreaName")


#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName.map <- function(map, layer = 1) {
  if (sum(map@metadata$studyArea)) {
    map@metadata[studyArea == TRUE, layerName][layer]
  } else {
    NULL
  }
}

#' Extract the studyArea(s) from a \code{map}
#' @export
#' @family mapMethods
#' @inheritParams map-class
#' @rdname studyArea
studyArea <- function(map, layer)
  UseMethod("studyArea")

#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea.map <- function(map, layer = 1) {
  if (sum(map@metadata$studyArea)) {
    get(map@metadata[studyArea == TRUE, layerName][layer], map@maps)
  } else {
    NULL
  }
}

#' Extract rasters in the \code{map} object
#' @export
#' @family mapMethods
#' @rdname maps
rasters <- function(map)
  UseMethod("rasters")

#' @export
#' @family mapMethods
#' @rdname maps
rasters.map <- function(map) {
  allObjs <- maps(map, "RasterLayer")
}


#' @export
#' @rdname maps
spatialPolygons <- function(map) {
  allObjs <- maps(map, "SpatialPolygons")
}

#' @export
#' @rdname maps
spatialPoints <- function(map) {
  allObjs <- maps(map, "SpatialPoints")
}

#' Extract maps from a \code{map} object
#'
#' This will extract all objects in or pointed to within the \code{map}.
#'
#' @export
#' @param map A \code{map} class object
#' @param class If supplied, this will be the class of objects returned. Default
#'              is \code{NULL} which is "all", meaning all objects in the \code{map}
#'              object
#' @return
#' A list of maps (i.e., sp, raster, or sf objects) of class \code{class}
maps <- function(map, class = NULL) {
  lsObjs <- ls(ml@maps)
  objs <- mget(lsObjs, ml@maps)
  envirs <- unlist(lapply(objs, is.environment))
  objsInEnvirs <- Map(objs = lsObjs[envirs], envirs = unname(mget(lsObjs[envirs], envir = map@maps)),
      function(objs, envirs) get(objs, envirs))
  objs[envirs] <- objsInEnvirs
  if (!is.null(class)) {
    logicalMaps <- unlist(lapply(objs, is, class))
    objs <- objs[logicalMaps]
  }
  objs
}

.singleMetadataNAEntry <-
  data.table::data.table(layerName = NA_character_, layerType = NA_character_,
                         sourceURL = NA_character_,
                         columnNameForLabels = NA_character_,
                         leafletVisible = TRUE, studyArea = FALSE)



if (!isGeneric("area")) {
  setGeneric("area", function(x, ...) {
    standardGeneric("area")
  })
}

#' Calculate area of (named) objects the \code{map} object
#'
#' @export
#' @importMethodsFrom raster area
#' @importFrom raster area
#' @family mapMethods
#' @rdname area
setMethod("area",
          signature = "map",
          function(x) {
            browser()
  lsObjs <- ls(ml@maps)
  logicalRasters <- unlist(lapply(mget(lsObjs, ml@maps), is, "RasterLayer"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], ml@maps)
  } else {
    NULL
  }
})
