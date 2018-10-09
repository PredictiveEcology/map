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
#' @slot CRS  The common crs of all layers
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
  contains = "environment",
  slots = list(
    metadata = "data.table",
    #.Data = "environment",
    CRS = "CRS",
    analyses = "data.table",
    analysesData = "list"
  ),
  validity = function(object) {
    browser()
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
                                          leaflet = logical(), studyArea = numeric(),
                                          rasterToMatch = logical())
            .Object@CRS = sp::CRS()
            .Object@analyses = data.table::data.table()
            .Object@analysesData = list()

            .Object
          })

#' Append a spatial object to map
#'
#' @details
#' If \code{isStudyArea = TRUE}, then several things will be triggered:
#'
#' 1. This layer will be added to metadata with \code{studyArea} set to
#'    \code{max(\code{studyArea(map)}) + 1}
#' 2. update CRS slot to be the CRS of the study area
#' 2.
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
#' ml <- mapAdd(StudyArea, ml, isStudyArea = TRUE, layerName = "newPoly")
#'
#' if (requireNamespace("SpaDES.tools")) {
#'   smallStudyArea <- SpaDES.tools::randomPolygon(studyArea(ml), 1e2)
#'   ml <- mapAdd(smallStudyArea, ml, isStudyArea = TRUE, filename2 = NULL,
#'                envir = .GlobalEnv) # adds a second studyArea within 1st
#' }
#'
mapAdd <- function(object, map, layerName, overwrite = FALSE, ...)
  UseMethod("mapAdd")

#' @export
#' @rdname mapAdd
#' @importFrom reproducible prepInputs preProcess
#' @param ... passed to reproducible::postProcess and reproducible::projectInputs and
#'            reproducible::fixErrors and reproducible::prepInputs
mapAdd.default <- function(object = NULL, map = NULL,
                                  layerName = NULL, overwrite = FALSE,
                                  sourceURL = NULL,
                                  columnNameForLabels = character(),
                                  leaflet = TRUE, isStudyArea = FALSE, ...) {
  if (is.null(map)) {
    map <- new("map")
    # options("map.current") <- map
    # suppressWarnings(rm("map", envir = as.environment("package:map")))
    # makeActiveBinding(sym = "map",
    #                   fun = ".maps",
    #                   env = as.environment("package:map"))
    # lockBinding("map", as.environment("package:map"))
  }
  if (is.null(object)) {
    dots <- list(...)
    if (is.null(sourceURL)) {
      stop("Must provide either object or sourceURL")
    } else {
      # Don't run preProcess because that will happen in next mapAdd when object is
      #   in hand
      forms <- reproducible:::.formalsNotInCurrentDots(preProcess, ...)
      args <- dots[!(names(dots) %in% forms)]
      object <- do.call(prepInputs, args = append(list(url = sourceURL), args))
    }
    map <- mapAdd(object, map = map, layerName = layerName,
                             overwrite = overwrite,
                             sourceURL = sourceURL, columnNameForLabels = columnNameForLabels,
                             leaflet = leaflet, isStudyArea = isStudyArea, ...)
  }
  map
}

#' @export
#' @rdname mapAdd
#' @importFrom reproducible fixErrors projectInputs postProcess .robustDigest asPath Cache compareNA
#' @importFrom data.table rbindlist set copy
#' @param envir An optional environment. If supplied, then the object
#'        will not be placed "into" the maps slot, rather the environment label will
#'        be placed into the maps slot. Upon re
#'
mapAdd.spatialObjects <- function(object, map = NULL, layerName = NULL,
                                   overwrite = FALSE, sourceURL = NULL,
                                   columnNameForLabels = NULL,
                                   leaflet = TRUE, isStudyArea = NULL,
                                   envir = NULL, ...) {

  dots <- list(...)
  objectName <- deparse(substitute(object))
  objectEnv <- quickPlot::whereInStack(objectName)

  mustOverwrite <- if (isTRUE(layerName %in% ls(map@.xData))) {
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
    if (isFALSE(isStudyArea)) {
      message("There is no studyArea in map; consider adding one with 'studyArea = TRUE'")
    }
    if (is.na(crs(map))) {
      if (is.null(dots$targetCRS)) { # OK ... user did not pass in targetCRS
        message("No crs already in map, so no reprojection")
      }
    } else {
      dots[["targetCRS"]] <- crs(map)
    }
    object <- do.call(projectInputs, append(list(object), dots))
  } else {
    if (is.na(crs(map))) {
      message("There is no CRS already in map; using the studyArea CRS and adding that to map")
    } else {
      dots <- list(...)
      # args <- if (!is.null(dots$studyArea)) {
      #   dots[!names(dots) %in% "studyArea"]
      # } else {
      #   dots
      # }
      object <- do.call(postProcess, append(list(object), dots))
    }
  }

  if (is.null(layerName)) {
    layerName <- objectName
  }

  if (is.null(envir)) {
    envir <- map@.xData
    # Put map into map slot
    assign(layerName, object, envir = map@.xData) # this overwrites, if same name
  } else {

    if (exists(layerName, envir = envir)) {
      assign(layerName, envir, envir = map@.xData)
    } else {
      stop("object named ", layerName, " does not exist in envir: ", envir)
    }
  }
  if (mustOverwrite) {
    ln <- layerName
    map@metadata <- map@metadata[!(layerName %in% ln)]
  }

  b <- copy(.singleMetadataNAEntry)

  # If it is studyArea
  if (isTRUE(isStudyArea)) {
    studyAreaNumber <- 1 + NROW(map@metadata[compareNA(studyArea, TRUE) |
                                               (is.numeric(studyArea) & studyArea > 0)])
    if (!is.null(studyArea(map))) {
      message("map already has a studyArea; adding another one as study area ",
              studyAreaNumber)
    } else {
      message("Setting map CRS to this layer because it is the (first) studyArea inserted")
      map@CRS <- raster::crs(object)
    }
    set(b, , "studyArea", studyAreaNumber)
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
  if (leaflet) {
    set(b, , "leaflet", leaflet)
    if (is(object, "Raster")) {
      object[] <- object[]
      objectLflt <- projectRaster(object, crs = CRS("+init=epsg:4326"))
      tmpFile <- tempfile(fileext = ".tif")
      objectLflt <- writeRaster(objectLflt, tmpFile)
      dig <- .robustDigest(objectLflt)
      tilePath <- asPath(paste0("tiles_", layerName, "_", substr(dig, 1,5)))
      message("Creating tiles")
      if (isTRUE(getOption("reproducible.useCache", FALSE)) ||
          getOption("reproducible.useCache", FALSE) == "overwrite") message("  using Cache. To revent this, set options('reproducible.useCache' = FALSE)")
      Cache(tiler::tile, asPath(tmpFile), tilePath, zoom = "1-10", crs = CRS("+init=epsg:4326"),
                  format = "tms", useCache = getOption("reproducible.useCache"))
      set(b, , "leafletTiles", tilePath)
    }
  }

  set(b, , "envir", list(list(envir)))
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
#'
#' If \code{layer} is not provided and there is more than one \code{studyArea},
#' then this will extract the last one added.
#'
#' @export
#' @family mapMethods
#' @inheritParams map-class
#' @rdname studyArea
studyArea <- function(map, layer)
  UseMethod("studyArea")

#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea.map <- function(map, layer = NA) {
  if (sum(map@metadata$studyArea, na.rm = TRUE)) {
    if (isTRUE(is.na(layer))) {
      layer <- max(map@metadata$studyArea, na.rm = TRUE)
    }
    get(map@metadata[studyArea > 0, layerName][layer], map@.xData)
  } else {
    NULL
  }
}

#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea.default <- function(map, layer = NA) {
  browser()
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
  maps(map, "RasterLayer")
}

#' Extract sp class objects from the \code{map} object
#' @export
#' @family mapMethods
#' @rdname maps
sp <- function(map)
  UseMethod("sp")

#' @export
#' @family mapMethods
#' @rdname maps
sp.map <- function(map) {
  maps(map, "Spatial")
}

#' Extract \code{sf} class objects from the \code{map} object
#' @export
#' @family mapMethods
#' @rdname maps
sf <- function(map)
  UseMethod("sf")

#' @export
#' @family mapMethods
#' @rdname maps
sf.map <- function(map) {
  maps(map, "sf")
}


#' @export
#' @rdname maps
spatialPolygons <- function(map) {
  maps(map, "SpatialPolygons")
}

#' @export
#' @rdname maps
spatialPoints <- function(map) {
  maps(map, "SpatialPoints")
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
  x <- map@metadata$layerName
  names(x) <- x
  envirs <- map@metadata$envir
  names(envirs) <- x
  out <- Map(envir = envirs, x = x,
        get(x, envir = envir, inherits = FALSE)
  )
  if (!is.null(class)) {
    classOnly <- unlist(lapply(out, is, class2 = class))
    out <- out[classOnly]
  }

  out
}

#' @export
.maps <- function() {
  browser()
  maps(getOption("map.current"))
}

.singleMetadataNAEntry <-
  data.table::data.table(layerName = NA_character_, layerType = NA_character_,
                         sourceURL = NA_character_,
                         columnNameForLabels = NA_character_,
                         envir = list(), leaflet = FALSE, studyArea = 0)



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
  lsObjs <- ls(ml@.xData)
  logicalRasters <- unlist(lapply(mget(lsObjs, ml@.xData), is, "RasterLayer"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], ml@.xData)
  } else {
    NULL
  }
})

#' Show method for map class objects
#' @export
#' @rdname show
setMethod(
  "show",
  signature = "map",
  definition = function(object) {
    show(object@metadata)

})
