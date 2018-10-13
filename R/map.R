#' Append a spatial object to map
#'
#' If \code{isStudyArea = TRUE}, then several things will be triggered:
#' \enumerate{
#'   \item This layer will be added to metadata with \code{studyArea} set to
#'         \code{max(studyArea(map)) + 1}.
#'   \item update CRS slot to be the CRS of the study area.
#' }
#'
#' @examples
#' library(sp)
#' library(raster)
#' setwd(tempdir())
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
#'                       59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' StudyArea <- SpatialPolygonsDataFrame(StudyArea,
#'                            data = data.frame(ID = 1, shinyLabel = "zone2"),
#'                            match.ID = FALSE)
#'
#' ml <- mapAdd(StudyArea, isStudyArea = TRUE, layerName = "Small Study Area")
#'
#' if (require("SpaDES.tools")) {
#'   smallStudyArea <- randomPolygon(studyArea(ml), 1e4)
#'   smallStudyArea <- SpatialPolygonsDataFrame(smallStudyArea,
#'                            data = data.frame(ID = 1, shinyLabel = "zone1"),
#'                            match.ID = FALSE)
#'   ml <- mapAdd(smallStudyArea, ml, isStudyArea = TRUE, filename2 = NULL,
#'                envir = .GlobalEnv, layerName = "Smaller Study Area") # adds a second studyArea within 1st
#'
#'   rasTemplate <- raster(extent(studyArea(ml)), res = 0.001)
#'   tsf <- randomPolygons(rasTemplate, numTypes = 3)*80
#'   crs(tsf) <- crs(ml)
#'   vtm <- randomPolygons(tsf, numTypes = 4)
#'   levels(vtm) <- data.frame(ID = sort(unique(vtm[])),
#'                             Factor = c("black spruce", "white spruce", "aspen", "fir"))
#'   crs(vtm) <- crs(ml)
#'   ml <- mapAdd(tsf, ml, filename2 = "tsf1.tif", layerName = "tsf1",
#'                tsf = TRUE,
#'                analysisGroup = "tsf1_vtm1", leaflet = FALSE, overwrite = TRUE)
#'   ml <- mapAdd(vtm, ml, filename2 = "vtm1.tif", layerName = "vtm1",
#'                vtm = TRUE,
#'                analysisGroup = "tsf1_vtm1", leaflet = FALSE, overwrite = TRUE)
#'
#'   ageClasses <- c("Young", "Immature", "Mature", "Old")
#'   ageClassCutOffs <- c(0, 40, 80, 120)
#'   ml <- mapLeadingByStage(ml, ageClasses = ageClasses,
#'                     ageClassCutOffs = ageClassCutOffs)
#'   ml <- mapAnalysis(ml, functionName = "Large patches", ageClasses = ageClasses, id = "1", labelColumn = "shinyLabel",
#'                     ageClassCutOffs = ageClassCutOffs,
#'                     quotedAnalysis = quote(Cache(.largePatchesCalc, tsfFile = tsf,
#'                       vtmFile = vtm, byPoly = poly, ...)))
#'
#'   # Add a second polygon, trigger
#'   smallStudyArea2 <- randomPolygon(studyArea(ml), 1e4)
#'   smallStudyArea2 <- SpatialPolygonsDataFrame(smallStudyArea2,
#'                            data = data.frame(ID = 1, shinyLabel = "zone1"),
#'                            match.ID = FALSE)
#'   ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
#'                envir = .GlobalEnv, layerName = "Smaller Study Area 2") # adds a second studyArea within 1st
#'   ml <- mapLeadingByStage(ml, ageClasses = ageClasses,
#'                     ageClassCutOffs = ageClassCutOffs)
#'
#'
#' }
#' @param object    TODO: document this
#' @param map       TODO: document this
#' @param layerName TODO: document this
#' @param overwrite TODO: document this
#' @param columnNameForLabels TODO: document this
#' @param leaflet TODO: document this
#' @param isStudyArea TODO: document this
#' @include map-class.R
#'
#' @export
#' @rdname mapAdd
#'
mapAdd <- function(object, map, layerName, overwrite = FALSE, ...) {
  UseMethod("mapAdd")
}

#' @export
#' @rdname mapAdd
#' @importFrom reproducible prepInputs preProcess
#' @param ... passed to reproducible::postProcess and reproducible::projectInputs and
#'            reproducible::fixErrors and reproducible::prepInputs
mapAdd.default <- function(object = NULL, map = new("map"),
                           layerName = NULL, overwrite = FALSE,
                           #url = NULL,
                           columnNameForLabels = character(),
                           leaflet = TRUE, isStudyArea = FALSE, ...) {
  if (is.null(object)) {    # with no object, we get it first, then pass to mapAdd

    dots <- list(...)
    # Don't run preProcess because that will happen in next mapAdd when object is
    #   in hand
    forms <- reproducible:::.formalsNotInCurrentDots(preProcess, ...)
    args <- dots[!(names(dots) %in% forms)]
    object <- do.call(prepInputs, args = args)

    map <- mapAdd(object, map = map, layerName = layerName,
                  overwrite = overwrite,
                  #           url = url,
                  columnNameForLabels = columnNameForLabels,
                             leaflet = leaflet, isStudyArea = isStudyArea, ...)
  }
  map
}

#' @param envir An optional environment. If supplied, then the object
#'        will not be placed "into" the maps slot, rather the environment label will
#'        be placed into the maps slot. Upon re
#'
#' @export
#' @importFrom data.table rbindlist set copy
#' @importFrom quickPlot whereInStack
#' @importFrom reproducible fixErrors projectInputs postProcess .robustDigest asPath Cache compareNA
#' @importFrom raster crs projectRaster writeRaster
#' @importFrom sp CRS
#' @importFrom backports isFALSE
#'
#' @rdname mapAdd
mapAdd.spatialObjects <- function(object, map = new("map"), layerName = NULL,
                                   overwrite = FALSE, #url = NULL,
                                   columnNameForLabels = NULL,
                                   leaflet = TRUE, isStudyArea = NULL,
                                   envir = NULL, ...) {

  dots <- list(...)
  objectName <- deparse(substitute(object))
  objectEnv <- whereInStack(objectName)

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
  if (is.null(studyArea(map)) && is.null(rasterToMatch(map))) {
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
      object <- do.call(projectInputs, append(list(object), dots))
    }
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
      if (!is.null(studyArea(map))) {
        dots$studyArea <- studyArea(map)
      }
      if (!is.null(rasterToMatch(map))) {
        dots$rasterToMatch <- rasterToMatch(map)
      }
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
      envir <- map@.xData
      assign(layerName, object, envir = envir)
      message("object named ", layerName, " does not exist in envir provided",
              ". Adding it to map object")
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
  if (leaflet) {
    set(b, NULL, "leaflet", leaflet)
    if (is(object, "Raster")) {
      dig <- .robustDigest(object)
      tilePath <- asPath(paste0("tiles_", layerName, "_", substr(dig, 1,6)))
      dirNotExist <- !dir.exists(tilePath)

      if (dirNotExist) {
        object[] <- object[]
        objectLflt <- projectRaster(object, crs = CRS("+init=epsg:4326"))
        tmpFile <- tempfile(fileext = ".tif")
        objectLflt <- writeRaster(objectLflt, tmpFile)
        message("  Creating tiles - reprojecting to epsg:4326 (leaflet projection)")
        message("Creating tiles")
        if (isTRUE(getOption("reproducible.useCache", FALSE)) ||
            getOption("reproducible.useCache", FALSE) == "overwrite") message("  using Cache. To prevent this, set options('reproducible.useCache' = FALSE)")
        tiler::tile(asPath(tmpFile), tilePath, zoom = "1-10", crs = CRS("+init=epsg:4326"),
                    format = "tms", useCache = getOption("reproducible.useCache"))
      } else {
        message("  Tiles - skipping creation - already exist")
      }
      set(b, NULL, "leafletTiles", tilePath)
    }
  }

  set(b, NULL, "envir", list(list(envir)))
  set(b, NULL, "objectName", objectName)

  # Add all extra columns to metadata
  dots <- list(...)
  if (length(dots)) {
    dots <- dots[!unlist(lapply(dots, is.null))] # remove NULL because that isn't added to data.table anyway
    if (!is.null(dots$filename2)) {
      if (inherits(object, "RasterLayer")) {
        if (endsWith(dots$filename2, suffix = "tif")) {
          if (raster::is.factor(object)) {
            dots$filename2 <- basename(filename(object))
          }
        }
      }
    }
  }
  columnsToAdd <- dots#[!names(dots) %in% .formalsReproducible]
  Map(cta = columnsToAdd, nta = names(columnsToAdd),
      function(cta, nta) set(b, , nta, cta))

  map@metadata <- rbindlist(list(map@metadata, b), use.names = TRUE, fill = TRUE)
  return(map)
}

#' Remove objects from a \code{map}
#'
#' @inheritParams map-class
#'
#' @param map TODO: document this
#'
#' @param layer TODO: document this
#'
#' @param ask TODO: document this
#'
#' @param ... TODO: document this
#'
#' @export
#' @family mapMethods
#' @rdname mapRm
mapRm <- function(map, layer, ask = TRUE, ...) {
  UseMethod("mapRm")
}

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
#'
#' @inheritParams raster crs
#'
#' @exportMethod crs
#' @family mapMethods
#' @importMethodsFrom raster crs
#' @importFrom raster crs
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
#' @param map TODO: document this
#'
#' @param layerName TODO: document this
#'
#' @param layer TODO: document this
#'
#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName <- function(map, layerName, layer) {
  UseMethod("studyAreaName")
}

#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName.map <- function(map, layerName, layer = 1) {
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
#' @param map TODO: document this
#'
#' @param layerName TODO: document this
#'
#' @param layer TODO: document this
#'
#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea <- function(map, layerName, layer) {
  UseMethod("studyArea")
}

#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea.default <- function(map, layerName, layer = NA) {
  browser()
}

#' @export
#' @family mapMethods
#' @rdname studyArea
studyArea.map <- function(map, layerName, layer = NA) {
  if (sum(map@metadata$studyArea, na.rm = TRUE)) {
    if (isTRUE(is.na(layer))) {
      layer <- max(map@metadata$studyArea, na.rm = TRUE)
    }
    get(map@metadata[studyArea == layer]$layerName,
        map@metadata[studyArea == layer]$envir[[1]])
  } else {
    NULL
  }
}

#' Extract the rasterToMatch(s) from a \code{map}
#'
#' If \code{layer} is not provided and there is more than one \code{studyArea},
#' then this will extract the last one added.
#'
#' @param map TODO: describe this
#'
#' @param layerName TODO: describe this
#'
#' @param layer TODO: describe this
#'
#' @export
#' @family mapMethods
#' @rdname rasterToMatch
rasterToMatch <- function(map, layerName, layer) {
  UseMethod("rasterToMatch")
}

#' @export
#' @family mapMethods
#' @rdname rasterToMatch
rasterToMatch.map <- function(map, layerName, layer = NA) {
  if (sum(map@metadata$rasterToMatch, na.rm = TRUE)) {
    if (isTRUE(is.na(layer))) {
      layer <- max(map@metadata$rasterToMatch, na.rm = TRUE)
    }
    get(map@metadata[rasterToMatch == layer, layerName], map@.xData)
  } else {
    NULL
  }
}

#' Extract rasters in the \code{map} object
#' @export
#' @family mapMethods
#' @rdname maps
rasters <- function(map) {
  UseMethod("rasters")
}

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
sp <- function(map) {
  UseMethod("sp")
}

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
sf <- function(map) {
  UseMethod("sf")
}

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

#' Extract leaflet tile paths from a \code{map} object
#'
#' @param map A \code{map} class object
#'
#' @export
#' @return
#' A vector of paths indicating the relative paths. Any layers
#' that don't have leaflet tiles will return NA.
leafletTiles <- function(map) {
  x <- map@metadata$layerName
  tiles <- map@metadata$leafletTiles
  names(tiles) <- x
  tiles
}

#' Extract maps from a \code{map} object
#'
#' This will extract all objects in or pointed to within the \code{map}.
#'
#' @param map A \code{map} class object
#' @param class If supplied, this will be the class of objects returned. Default
#'              is \code{NULL} which is "all", meaning all objects in the \code{map}
#'              object
#' @export
#' @return
#' A list of maps (i.e., sp, raster, or sf objects) of class \code{class}
maps <- function(map, class = NULL) {
  x <- map@metadata$layerName
  names(x) <- x
  envirs <- map@metadata$envir
  names(envirs) <- x
  out <- Map(envir = envirs, x = x,
             function(envir, x)
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

#' @keywords internal
.singleMetadataNAEntry <- data.table::data.table(
  layerName = NA_character_, layerType = NA_character_, #url = NA_character_,
  columnNameForLabels = NA_character_, envir = list(), leaflet = FALSE, studyArea = 0
)

if (!isGeneric("area")) {
  setGeneric("area", function(x, ...) {
    standardGeneric("area")
  })
}

#' Calculate area of (named) objects the \code{map} object
#'
#' @inheritParams raster area
#'
#' @export
#' @family mapMethods
#' @importMethodsFrom raster area
#' @importFrom raster area
#' @rdname area
setMethod("area",
          signature = "map",
          function(x) {
  lsObjs <- ls(x@.xData)
  logicalRasters <- unlist(lapply(mget(lsObjs, x@.xData), is, "RasterLayer"))
  if (any(logicalRasters)) {
    mget(names(logicalRasters)[logicalRasters], x@.xData)
  } else {
    NULL
  }
})

#' Show method for map class objects
#'
#' @param object TODO: describe this
#'
#' @export
#' @rdname show
setMethod(
  "show",
  signature = "map",
  definition = function(object) {
    show(object@metadata)
})

.formalsReproducible <- unique(c(formalArgs(reproducible::preProcess),
                          formalArgs(reproducible::postProcess),
                          formalArgs(reproducible:::determineFilename),
                          formalArgs(reproducible::cropInputs),
                          formalArgs(reproducible::maskInputs),
                          formalArgs(reproducible::projectInputs)))


################################################################################
#' Extract the metadata object
#'
#' Methods for specific classes exist.
#'
#' @export
#' @rdname metadata
metadata <- function(x) UseMethod("metadata")

#' @importFrom raster metadata
#' @export
#' @rdname metadata
metadata.Raster <- function(x) {
  raster::metadata(x)
}

#' @export
#' @rdname metadata
metadata.map <- function(x) {
  x@metadata
}

