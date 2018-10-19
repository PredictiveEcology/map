if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".N", "objectHash"))
}


#' Append a spatial object to map
#'
#' If \code{isStudyArea = TRUE}, then several things will be triggered:
#' \enumerate{ \item This layer will be added to metadata with \code{studyArea}
#' set to \code{max(studyArea(map)) + 1}. \item update CRS slot to be the CRS of
#' the study area. }
#'
#' @param object    Optional spatial object, currently \code{RasterLayer},
#'  \code{SpatialPolygons}
#' @param map       Optional map object. If not provided, then one will be
#'  created. If provided, then the present \code{object} or options passed to
#'  prepInputs e.g., \code{url}, will be appended to this \code{map}
#' @param layerName Required. A label for this map layer. This can be the same as
#'  the object name.
#' @param overwrite Logical. If \code{TRUE} and this \code{layerName} exists in
#'  the \code{map}, then it will replace the existing object.
#' @param columnNameForLabels A character string indicating which column to use
#'  for labels. This is currently only used if the object is a
#'  \code{SpatialPolygonsDataFram}.
#' @param leaflet Logical or Character vector of path(s) to write tiles.
#'  If \code{TRUE} or a character vector, then this layer will be added to a
#'  leaflet map. For \code{RasterLayer} object, this will trigger a call to
#'  \code{gdal2tiles}, making tiles. If path is not specified, it will be
#'  the current path. The tile base file path will be the
#'  \code{paste0(layerName, "_", rndstr(1,6))}
#' @param isStudyArea Logical. If \code{TRUE}, this will be assigned the label,
#'  "StudyArea", and will be passed into \code{prepInputs} for any future layers
#'  added.
#'
#' @export
#' @include map-class.R
#' @rdname mapAdd
#'
#' @examples
#' library(sp)
#' library(raster)
#' cwd <- getwd()
#' setwd(tempdir())
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
#'                       59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- paste("+init=epsg:4326 +proj=longlat +datum=WGS84",
#'                         "+no_defs +ellps=WGS84 +towgs84=0,0,0")
#' StudyArea <- SpatialPolygonsDataFrame(StudyArea,
#'                            data = data.frame(ID = 1, shinyLabel = "zone2"),
#'                            match.ID = FALSE)
#'
#' ml <- mapAdd(StudyArea, isStudyArea = TRUE, layerName = "Small Study Area",
#'                poly = TRUE, analysisGroup2 = "Small Study Area")
#'
#'# if (require("SpaDES.tools")) {
#' require("SpaDES.tools")
#'   smallStudyArea <- randomPolygon(studyArea(ml), 1e5)
#'   smallStudyArea <- SpatialPolygonsDataFrame(smallStudyArea,
#'                            data = data.frame(ID = 1, shinyLabel = "zone1"),
#'                            match.ID = FALSE)
#'   ml <- mapAdd(smallStudyArea, ml, isStudyArea = TRUE, filename2 = NULL,
#'                analysisGroup2 = "Smaller Study Area",
#'                poly = TRUE,
#'                layerName = "Smaller Study Area") # adds a second studyArea within 1st
#'
#'   rasTemplate <- raster(extent(studyArea(ml)), res = 0.001)
#'   tsf <- randomPolygons(rasTemplate, numTypes = 8)*30
#'   crs(tsf) <- crs(ml)
#'   vtm <- randomPolygons(tsf, numTypes = 4)
#'   levels(vtm) <- data.frame(ID = sort(unique(vtm[])),
#'                             Factor = c("black spruce", "white spruce", "aspen", "fir"))
#'   crs(vtm) <- crs(ml)
#'   ml <- mapAdd(tsf, ml, layerName = "tsf1",
#'                filename2 = "tsf1.tif", # to postProcess
#'                # to map object
#'                tsf = "tsf1.tif", # to column in map@metadata
#'                analysisGroup1 = "tsf1_vtm1",  # this is the label for analysisGroup1
#'                leaflet = TRUE, # to column in map@metadata, will be used for visualizing in leaflet
#'                overwrite = TRUE)
#'   ml <- mapAdd(vtm, ml, filename2 = "vtm1.grd",
#'                layerName = "vtm1",
#'                vtm = "vtm1.grd",
#'                analysisGroup1 = "tsf1_vtm1", leaflet = TRUE, overwrite = TRUE)
#'
#'   ageClasses <- c("Young", "Immature", "Mature", "Old")
#'   ageClassCutOffs <- c(0, 40, 80, 120)
#'
#'   # add an analysis -- this will trigger analyses because there are already objects in the map
#'   #    This will trigger 2 analyses:
#'   #    LeadingVegTypeByAgeClass on each raster x polygon combo (only 1 currently)
#'   #    so there is 1 raster group, 2 polygon groups, 1 analyses - Total 2, 2 run now
#'   ml <- mapAddAnalysis(ml, functionName ="LeadingVegTypeByAgeClass",
#'                         ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
#'   # add an analysis -- this will trigger analyses because there are already objects in the map
#'   #    This will trigger 2 more analyses:
#'   #    largePatches on each raster x polygon combo (only 1 currently)
#'   #    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
#'   ml <- mapAddAnalysis(ml, functionName = "LargePatches", ageClasses = ageClasses,
#'                     id = "1", labelColumn = "shinyLabel",
#'                     ageClassCutOffs = ageClassCutOffs)
#'
#'   # Add a second polygon, trigger
#'   smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
#'   smallStudyArea2 <- SpatialPolygonsDataFrame(smallStudyArea2,
#'                            data = data.frame(ID = 1, shinyLabel = "zone1"),
#'                            match.ID = FALSE)
#'   # add a new layer -- this will trigger analyses because there are already analyese in the map
#'   #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
#'   #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
#'   ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
#'                analysisGroup2 = "Smaller Study Area 2",
#'                poly = TRUE,
#'                layerName = "Smaller Study Area 2") # adds a second studyArea within 1st
#'
#'   # Add a *different* second polygon, via overwrite. This should trigger new analyses
#'   smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
#'   smallStudyArea2 <- SpatialPolygonsDataFrame(smallStudyArea2,
#'                            data = data.frame(ID = 1, shinyLabel = "zone1"),
#'                            match.ID = FALSE)
#'   # add a new layer -- this will trigger analyses because there are already analyses in the map
#'   #    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
#'   #    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
#'   ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
#'                analysisGroup2 = "Smaller Study Area 2",
#'                poly = TRUE,
#'                layerName = "Smaller Study Area 2") # adds a second studyArea within 1st
#'
#'   # Add a 2nd pair of rasters
#'   rasTemplate <- raster(extent(studyArea(ml)), res = 0.001)
#'   tsf2 <- randomPolygons(rasTemplate, numTypes = 8)*30
#'   crs(tsf2) <- crs(ml)
#'   vtm2 <- randomPolygons(tsf2, numTypes = 4)
#'   levels(vtm2) <- data.frame(ID = sort(unique(vtm2[])),
#'                             Factor = c("black spruce", "white spruce", "aspen", "fir"))
#'   crs(vtm2) <- crs(ml)
#'   ml <- mapAdd(tsf2, ml, filename2 = "tsf2.tif", layerName = "tsf2",
#'                tsf = "tsf2.tif",
#'                analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)
#'   ml <- mapAdd(vtm2, ml, filename2 = "vtm2.grd", layerName = "vtm2",
#'                vtm = "vtm2.grd",
#'                analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)
#'
#'   # post hoc analysis of data
#'   #  use or create a specialized function that can handle the analysesData slot
#'   ml <- mapAddPostHocAnalysis(map = ml, functionName = "rbindlistAG",
#'               postHocAnalysisGroups = "analysisGroup2",
#'               postHocAnalyses = "all")
#'
#' ## cleanup
#' setwd(cwd)
#' unlink(tempdir(), recursive = TRUE)
#' #}
#'@param obj    Optional spatial object, currently tested with \code{RasterLayer},
#'  \code{SpatialPolygons*}
#'@param map       Optional map object. If not provided, then one will be
#'  created. If provided, then the present \code{obj} or options passed to
#'  prepInputs e.g., \code{url}, will be appended to this \code{map}
#'@param layerName Required. A label for this map layer. This can be the same as
#'  the object name.
#'@param overwrite Logical. If \code{TRUE} and this \code{layerName} exists in
#'  the \code{map}, then it will replace the existing object. Default is
#'  \code{getOption("map.overwrite")}
#'@param columnNameForLabels A character string indicating which column to use
#'  for labels. This is currently only used if the object is a
#'  \code{SpatialPolygonsDataFram}.
#'@param leaflet Logical or Character vector of path(s) to write tiles.
#'  If \code{TRUE} or a character vector, then this layer will be added to a
#'  leaflet map. For \code{RasterLayer} object, this will trigger a call to
#'  \code{gdal2tiles}, making tiles. If path is not specified, it will be
#'  the current path. The tile base file path will be the
#'  \code{paste0(layerName, "_", rndstr(1,6))}
#'@param isStudyArea Logical. If \code{TRUE}, this will be assigned the label,
#'  "StudyArea", and will be passed into \code{prepInputs} for any future layers
#'  added.
#'@include map-class.R
#'
#'@export
#'@rdname mapAdd
#'
mapAdd <- function(obj, map, layerName,
                   overwrite = getOption("map.overwrite", FALSE), ...) {
  UseMethod("mapAdd")
}

#' @export
#' @rdname mapAdd
#' @importFrom reproducible prepInputs preProcess
#' @param ... passed to reproducible::postProcess and reproducible::projectInputs and
#'            reproducible::fixErrors and reproducible::prepInputs
# mapAdd.default <- function(obj = NULL, map = new("map"),
#                            layerName = NULL, overwrite = FALSE,
#                            #url = NULL,
#                            columnNameForLabels = character(),
#                            leaflet = TRUE, isStudyArea = FALSE, ...) {
#   if (is.null(obj)) {    # with no obj, we get it first, then pass to mapAdd
#
#     dots <- list(...)
#     # Don't run postProcess because that will happen in next mapAdd when obj is
#     #   in hand
#     forms <- reproducible:::.formalsNotInCurrentDots(preProcess, ...)
#     args <- dots[!(names(dots) %in% forms)]
#     args <- append(args, mget(ls()[ls() %in% formalArgs(preProcess)], inherits = FALSE))
#     obj <- do.call(prepInputs, args = args)
#
#     map <- mapAdd(obj, map = map, layerName = layerName,
#                   overwrite = overwrite,
#                   #           url = url,
#                   columnNameForLabels = columnNameForLabels,
#                   leaflet = leaflet, isStudyArea = isStudyArea, ...)
#   }
#   map
# }

#' @param envir An optional environment. If supplied, then the obj
#'        will not be placed "into" the maps slot, rather the environment label will
#'        be placed into the maps slot. Upon re
#' @param useCache Logical. If \code{TRUE}, then internal calls to \code{Cache} will
#'        be used. Default is \code{TRUE}
#' @param useParallel Logical. If \code{TRUE}, then if there is more than one
#'        calculation to do at any stage, it will create and use a parallel
#'        cluster via \code{makeOptimalCluster}
#' @param paths TODO: description needed
#'
#' @export
#' @importFrom data.table rbindlist set copy
#' @importFrom quickPlot whereInStack
#' @importFrom reproducible fixErrors projectInputs postProcess .robustDigest asPath Cache compareNA
#' @importFrom raster crs projectRaster writeRaster
#' @importFrom sp CRS
#'
#' @rdname mapAdd
mapAdd.default <- function(obj = NULL, map = new("map"), layerName = NULL,
                           overwrite = getOption("map.overwrite"),
                           columnNameForLabels = 1,
                           leaflet = TRUE, isStudyArea = FALSE,
                           envir = NULL, useCache = TRUE,
                           useParallel = getOption("map.useParallel"),
                           paths = getOption("map.paths"), ...) {

  dots <- list(...)

  # Some of the arguments will need to be passed into Cache
  ###########################################
  # Get obj, if missing, via prepInputs url, or targetFile
  ###########################################
  if (is.null(obj)) {    # with no obj, we get it first, then pass to mapAdd
    dots <- list(...)
    # Don't run postProcess because that will happen in next mapAdd when obj is
    #   in hand
    args1 <- identifyVectorArgs(fn = list(Cache, preProcess), ls(), environment(), dots)
    obj <- MapOrLapply(prepInputs, multiple = args1$argsMulti,
                   single = args1$argsSingle, useCache = useCache)
    if (is(obj, "list")) # note is.list returns TRUE for data.frames ... BAD
      names(obj) <- layerName
  }

  layerNameExistsInMetadata <- if (isTRUE(layerName %in% ls(map@.xData))) {
    if (isTRUE(overwrite)) {
      message(layerName, " already in map; overwriting")
    } else {
      stop(layerName, " already in map; stopping. Want overwrite = TRUE?")
    }
    TRUE
  } else {
    FALSE
  }

  ####################################################
  # postProcess -- determine studyArea and rasterToMatch from map
  ####################################################
  if (is.null(studyArea(map)) && is.null(rasterToMatch(map))) {
    argsFixErrors <- getLocalArgsFor(list(Cache, fixErrors), dots = dots)
    obj <- do.call(Cache, append(list(fixErrors, obj), argsFixErrors))
    if (isFALSE(isStudyArea)) {
      message("There is no studyArea in map; consider adding one with 'studyArea = TRUE'")
    }
    if (is.na(crs(map))) {
      if (is.null(dots$targetCRS)) { # OK ... user did not pass in targetCRS
        message("No crs already in map, so no reprojection")
      } else {
        argsProjectInputs <- getLocalArgsFor(list(Cache, projectInputs), dots = dots)
        obj <- do.call(Cache, append(list(projectInputs, obj), argsProjectInputs))
      }

    } else {
      dots[["targetCRS"]] <- crs(map)
      args <- dots
      args <- append(args, mget(ls()[ls() %in% formalArgs(projectInputs)], inherits = FALSE))

      obj <- do.call(projectInputs, append(list(obj), args))
    }
  } else {
    if (is.na(crs(map))) {
      message("There is no CRS already in map; using the studyArea CRS and adding that to map")
    } else {
      dots <- list(...)
      if (!is.null(studyArea(map))) {
        studyArea <- studyArea(map)
      }
      if (!is.null(rasterToMatch(map))) {
        rasterToMatch <- rasterToMatch(map)
      }

      list2env(dots, envir = environment()) # put any arguments from the ... into this local env
      x <- obj # put it into memory so identifyVectorArgs finds it
      args1 <- identifyVectorArgs(fn = list(Cache, getS3method("postProcess", "spatialObjects"),
                                            projectInputs, cropInputs, writeOutputs),
                                  ls(), environment(), dots = dots)
      obj <- MapOrLapply(postProcess, multiple = args1$argsMulti,
                     single = args1$argsSingle, useCache = useCache)
    }
  }

  ## TODO: remove the block below
  if (FALSE) {
    aaa <<- 1
    on.exit(rm(aaa, envir = .GlobalEnv))
    browser(expr = exists("aaa"))
  }

  ###############################################
  # Purge obj(s) from metadata, if overwrite is TRUE
  ###############################################
  objHash <- .robustDigest(obj)
  purgeAnalyses <- NULL # Set default as NULL
  if (layerNameExistsInMetadata) {
    ln <- layerName
    purge <- isFALSE(map@metadata[(layerName %in% ln), objectHash] == objHash)
    if (isTRUE(purge)) {
      if (any(startsWith(colnames(map@metadata), "analysisGroup")))
        purgeAnalyses <- map@metadata[layerName %in% ln, get(colnames(map@metadata)[
          startsWith(colnames(map@metadata), "analysisGroup")])]

    }
    map@metadata <- map@metadata[!(layerName %in% ln)]
  }

  ###################################################
  # Add "shinyLabel" column if it is a SpatialPolygonsDataFrame
  ###################################################
  args1 <- identifyVectorArgs(fn = addColumnNameForLabels,
                              c(x = "obj", columnNameForLabels = "columnNameForLabels"),
                              environment(), dots = dots)
  obj <- MapOrLapply(addColumnNameForLabels, multiple = args1$argsMulti,
              single = args1$argsSingle, useCache = useCache)

  ####################################################
  # Assign obj to map@.xData
  ####################################################
  if (is.null(envir)) {
    envir <- map@.xData # keep envir for later
    # Put map into map slot
    a <- list()
    objTmp <- if (is(obj, "list")) obj else list(obj)
    a[layerName] <- objTmp
    list2env(a, envir = envir)
  } else {

    if (exists(layerName, envir = envir)) {
      a <- list()
      envir1 <- if (is(envir, "list")) obj else list(envir)
      a[layerName] <- list(envir1)
      list2env(a, envir = map@.xData)
    } else {
      envir <- map@.xData
      a <- list()
      objTmp <- if (is(obj, "list")) obj else list(obj)
      a[layerName] <- objTmp
      list2env(a, envir = envir)
      message("obj named ", paste(layerName, collapse = ", "), " does not exist in envir provided",
              ". Adding it to map obj")
    }
  }

  ####################################################
  # Metadata -- build new entries in data.table -- vectorized
  ####################################################
  args1 <- identifyVectorArgs(fn = list(buildMetadata, prepInputs), ls(), environment(),
                              dots = dots)
  if (length(dots)) {
    howLong <- unlist(lapply(dots, length))
    args1$argsSingle[names(dots)[howLong<=1]] <- dots[howLong<=1]
    args1$argsMulti[names(dots)[howLong>1]] <- dots[howLong>1]
  }
  MoreArgs = append(args1$argsSingle, list(metadata = map@metadata))
  if (length(args1$argsMulti)==0) {
    dts <- do.call(buildMetadata, MoreArgs)
  } else {
    dtsList <- do.call(Map, args = append(args1$argsMulti,
                                          list(f = buildMetadata, MoreArgs = MoreArgs)))
    dts <- rbindlist(dtsList, use.names = TRUE, fill = TRUE)
  }


  ########################################################
  # make tiles, if it is leaflet
  ########################################################
  if (any(!isFALSE(leaflet)) && !is.null(dts$leafletTiles)) {
    if (is.logical(leaflet)) {
      leaflet <- getwd()
    }
    MBadjustment <- 4000 # some approximate, empirically derived number. Likely only good in some cases
    MBper <- if (is(obj, "RasterLayer")) {
      ncell(obj) / MBadjustment
    } else if ( tryCatch(is(obj[[1]], "RasterLayer"), error = function(x) FALSE)) {
      ncell(obj[[1]]) / MBadjustment
    } else {
      1 # i.e., default to detectClusters()
    }
    if (isTRUE(all(dir.exists(dts$leafletTiles)))) {
      useParallel <- FALSE
    } else {
      if (missing(useParallel)) {
        useParallel <- getOption("map.useParallel",
                                 !identical("windows", .Platform$OS.type))
      }
    }
    cl <- makeOptimalCluster(useParallel = useParallel,
                             MBper = MBper,
                             maxNumClusters = length(obj))
    on.exit(try(stopCluster(cl), silent = TRUE))
    tilePath <- dts$leafletTiles
    args1 <- identifyVectorArgs(fn = makeTiles, ls(), environment(), dots = dots)
    out <- MapOrLapply(makeTiles, multiple = args1$argsMulti,
                       single = args1$argsSingle, useCache = FALSE, cl = cl)
                # If the rasters are identical, then there may be
                              # errors

  }

  ###################################
  # set CRS
  ####################################
  if (isTRUE(isStudyArea)) {
    if ((!is.null(studyArea(map))) && isStudyArea) {
      message("map already has a studyArea; adding another one as study area ",
              dts$studyArea)
    } else {
      message("Setting map CRS to this layer because it is the (first) studyArea inserted")
      map@CRS <- raster::crs(obj)
    }
  }

  ######################################
  # rbindlist new metadata with existing metadata
  ########################################
  map@metadata <- rbindlist(list(map@metadata, dts), use.names = TRUE, fill = TRUE)

  ######################################
  # run map analyses
  ########################################
  map <- runMapAnalyses(map = map, purgeAnalyses = purgeAnalyses, useParallel = useParallel)

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
#' @examples
#' if (require("SpaDES.tools")) {
#'   library(sp)
#'   longLatCRS <- CRS(paste("+init=epsg:4326 +proj=longlat +datum=WGS84",
#'                           "+no_defs +ellps=WGS84 +towgs84=0,0,0"))
#'   p <- randomPolygon(SpatialPoints(cbind(-120, 60), proj4string = longLatCRS),
#'        area = 1e5)
#'   m <- mapAdd(p, layerName = "p")
#'   mapRm(m, "p")
#' }
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

  layerName <- unique(map@metadata[ layer , layerName])
  if (length(layer) > 1)
    stop("There are more than obj in map with that layer name, '",
         layerName,"'. Please indicate layer by row number in map@metadata.")

  rm(list = layerName, envir = map@metadata[ layer , envir][[1]])
  map@metadata <- map@metadata[ -layer , ]

  if (NROW(map@analyses))
    message("Layer ", layerName, " has been removed, but not any analysis that ",
            "was previously run using this layer")

  map
}

if (!isGeneric("crs")) {
  setGeneric("crs", function(x, ...) {
    standardGeneric("crs")
  })
}

#' Extract the crs of a \code{map}
#'
#' @inheritParams raster::crs
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
  NULL
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

#' Extract rasters in the \code{map} obj
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

#' Extract sp class objects from the \code{map} obj
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

#' Extract \code{sf} class objects from the \code{map} obj
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

#' Extract leaflet tile paths from a \code{map} obj
#'
#' @param map A \code{map} class obj
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

#' Extract maps from a \code{map} obj
#'
#' This will extract all objects in or pointed to within the \code{map}.
#'
#' @param map A \code{map} class obj
#' @param class If supplied, this will be the class of objects returned. Default
#'              is \code{NULL} which is "all", meaning all objects in the \code{map}
#'              obj
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

#' Calculate area of (named) objects the \code{map} obj
#'
#' @inheritParams raster::area
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
#' @param obj TODO: describe this
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
#' Extract the metadata obj
#'
#' Methods for specific classes exist.
#'
#' @param x TODO: description needed
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


#' Find sources for arguments in arbitrary function(s)
#'
#' Search among local objects (which will often be arguments passed
#' into a function) as well as dot objects to match the
#' formals needed by \code{fn}. If \code{localFormalArgs} is named,
#' then it will match the formal (name of localFormaArgs) with the
#' local object, e.g., localFormalArgs = c(x = "obj") will find
#' the object in the local environment called "obj", and this will
#' be found because it matches the \code{x} argument in \code{fn}.
#'
#' @param localFormalArgs A (named) character vector or arguments to
#' @return List of named objects. The names are the formals in fn, and
#' the objects are the values for those formals. This can easily
#' be passed to do.call(fn, args1)
getLocalArgsFor <- function(fn, localFormalArgs, envir, dots) {
  if (missing(envir))
    envir <- parent.frame()
  if (missing(localFormalArgs))
    localFormalArgs <- ls(envir = envir)

  if (is.null(names(localFormalArgs)))
    names(localFormalArgs) <- localFormalArgs

  #dots <- list(...)
  if (length(fn) > 1) {
    forms <- unlist(lapply(fn, reproducible::.formalsNotInCurrentDots, dots = dots))
    forms <- forms[duplicated(forms)]
  } else {
    forms <- reproducible:::.formalsNotInCurrentDots(fn, dots = dots)
  }
  args <- dots[!(names(dots) %in% forms)]
  localFormals <- if (length(fn) > 1) {
    a <- lapply(fn, function(f)
      localFormalArgs[names(localFormalArgs) %in% formalArgs(f)]
    )
    unlist(a)[!duplicated(unlist(a))] # unique strips names
  } else {
    localFormalArgs[names(localFormalArgs) %in% formalArgs(fn)]
  }
  objsFromLocal <- mget(localFormals, envir = envir)
  names(objsFromLocal) <- names(localFormals)
  args <- append(args, objsFromLocal)

}


identifyVectorArgs <- function(fn, localFormalArgs, envir, dots) {

  allArgs <- getLocalArgsFor(fn, localFormalArgs, envir = envir, dots = dots)

  specialTypes = c("environment", "SpatialPolygons")
  lengthOne <- unlist(lapply(allArgs, is.null)) | unlist(lapply(allArgs, function(x) {
    if (any(unlist(lapply(specialTypes, is, obj = x))) | length(x)==1) {
      TRUE
    } else {
      FALSE
    }}))
  if (sum(lengthOne)) {
    argsSingle <- allArgs[lengthOne]
    argsMulti <- allArgs[!lengthOne]
  } else {
    argsSingle <- list()
    argsMulti <- allArgs
  }

  list(argsSingle = argsSingle, argsMulti = argsMulti)

}

#' \code{Map}/\code{lapply} all in one
#'
#' Usually run after \code{identifyVectorArgs}
#'
#' @param multiple This a list the arguments that Map will cycle over.
#' @param single Passed to \code{MoreArgs} in the \code{mapply} function.
#' @param fn The function that will be run via \code{Map}/\code{clusterMap}.
#' @param useCache Logical indicating whether to use the cache.
#' @param cl A cluster object or \code{NULL}.
#'
#' @seealso \code{identifyVectorArgs}
MapOrLapply <- function(fn, multiple, single, useCache, cl = NULL) {
  if (length(multiple)) {
    obj <- do.call(Cache, args = append(multiple, list(Map2, fn,
                                               MoreArgs = single,
                                               cl = cl, useCache = useCache)))
  } else {
    if (!missing(useCache))
      single[["useCache"]] <- useCache
    obj <- do.call(Cache, args = append(list(fn),
                                           single))
  }
  obj

}

addColumnNameForLabels <- function(x, columnNameForLabels) {
  if (is(x, "SpatialPolygonsDataFrame")) {
    x$shinyLabel <- x[[columnNameForLabels]]
  }
  x
}
