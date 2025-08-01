utils::globalVariables(c(
  ".", ":=", "..pathCols1", "..pathCols2", ".I", ".N", ".SD",
  "envir", "layerName", "objectHash"
))

#' Append a spatial object to map
#'
#' If `isStudyArea = TRUE`, then several things will be triggered:
#' 1. This layer will be added to metadata with `studyArea`
#'   set to `max(studyArea(map)) + 1`.
#' 2. update CRS slot to be the CRS of the study area.
#'
#' @param obj    Optional spatial object, currently `RasterLayer`, `SpatialPolygons`.
#'
#' @param map       Optional map object. If not provided, then one will be created.
#'  If provided, then the present `obj` or options passed to
#'  [reproducible::prepInputs()] (e.g., `url`), will be appended to this `map`.
#'
#' @param layerName Required. A label for this map layer. This can be the same as
#'  the object name.
#'
#' @param overwrite Logical. If `TRUE` and this `layerName` exists in
#'  the `map`, then it will replace the existing object. Default is `getOption("map.overwrite")`.
#'
#' @param columnNameForLabels A character string indicating which column to use for labels.
#'  This is currently only used if the object is a [sp::SpatialPolygonsDataFrame-class].
#'
#' @param leaflet Logical or Character vector of path(s) to write tiles.
#'  If `TRUE` or a character vector, then this layer will be added to a leaflet map.
#'  For `RasterLayer` object, this will trigger a call to `gdal2tiles`, making tiles.
#'  If path is not specified, it will be the current path.
#'  The tile base file path will be `paste0(layerName, "_", rndstr(1, 6))`.
#'
#' @param isStudyArea Logical. If `TRUE`, this will be assigned the label "StudyArea",
#'   and will be passed into [reproducible::prepInputs()] for any future layers added.
#'
#' @export
#' @include map-class.R
#' @rdname mapAdd
#'
#' @examples
#' withr::local_tempdir("example_mapAdd_") |>
#'   withr::local_dir()
#'
#' StudyArea <- list(cbind(
#'   x = c(-122.98, -116.1, -99.2, -106, -122.98),
#'   y = c(59.9, 65.73, 63.58, 54.79, 59.9)
#' )) |>
#'   sf::st_polygon() |>
#'   sf::st_sfc() |>
#'   sf::st_sf(geometry = _, ID = 1L, shinyLabel = "zone2", crs = "epsg:4326")
#'
#' ml <- mapAdd(
#'   StudyArea,
#'   isStudyArea = TRUE,
#'   layerName = "Small Study Area",
#'   poly = TRUE,
#'   analysisGroup2 = "Small Study Area"
#' )
#'
#' if (require("SpaDES.tools", quietly = TRUE)) {
#'   withr::local_options(list(
#'     map.tilePath = withr::local_tempdir("tiles_"),
#'     map.useParallel = FALSE
#'   ))
#'   smallStudyArea <- SpaDES.tools::randomPolygon(studyArea(ml), 1e5)
#'   smallStudyArea$ID <- 1L
#'   smallStudyArea$shinyLabel <- "zone2"
#'
#'   ml <- mapAdd(
#'     smallStudyArea,
#'     ml,
#'     isStudyArea = TRUE,
#'     filename2 = NULL,
#'     analysisGroup2 = "Smaller Study Area",
#'     poly = TRUE,
#'     layerName = "Smaller Study Area"
#'   ) ## adds a second studyArea within 1st
#'
#'   rasTemplate <- terra::rast(terra::ext(studyArea(ml)), resolution = 0.001)
#'   tsf <- SpaDES.tools::randomPolygons(rasTemplate, numTypes = 8) * 30
#'   vtm <- SpaDES.tools::randomPolygons(tsf, numTypes = 4)
#'   levels(vtm) <- data.frame(
#'     ID = sort(unique(vtm[])),
#'     Factor = c("black spruce", "white spruce", "aspen", "fir")
#'   )
#'
#'   ml <- mapAdd(
#'     tsf,
#'     ml,
#'     layerName = "tsf1",
#'     filename2 = "tsf1.tif", ## to postProcess
#'     ## to map object
#'     tsf = "tsf1.tif", ## to column in map@metadata
#'     analysisGroup1 = "tsf1_vtm1", ## this is the label for analysisGroup1
#'     leaflet = TRUE, ## to column in map@metadata; used for visualizing in leaflet
#'     overwrite = TRUE
#'   )
#'   ml <- mapAdd(
#'     vtm,
#'     ml,
#'     filename2 = "vtm1.grd",
#'     layerName = "vtm1",
#'     vtm = "vtm1.grd",
#'     analysisGroup1 = "tsf1_vtm1",
#'     leaflet = TRUE,
#'     overwrite = TRUE
#'   )
#'
#'   ## these map analyses are in `LandWebUtils` package, which is reverse dependency of this one
#'   ageClasses <- c("Young", "Immature", "Mature", "Old")
#'   ageClassCutOffs <- c(0, 40, 80, 120)
#'
#'   ## add an analysis -- this will trigger analyses because there are already objects in the map
#'   ##    This will trigger 2 analyses:
#'   ##    LeadingVegTypeByAgeClass on each raster x polygon combo (only 1 currently)
#'   ##    so there is 1 raster group, 2 polygon groups, 1 analyses - Total 2, 2 run now
#'   # ml <- mapAddAnalysis(ml, functionName = "LeadingVegTypeByAgeClass",
#'   #                      ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
#'
#'   ## add an analysis -- this will trigger analyses because there are already objects in the map
#'   ##    This will trigger 2 more analyses:
#'   ##    largePatches on each raster x polygon combo (only 1 currently)
#'   ##    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
#'   # ml <- mapAddAnalysis(ml, functionName = "LargePatches", ageClasses = ageClasses,
#'   #                      id = "1", labelColumn = "shinyLabel",
#'   #                      ageClassCutOffs = ageClassCutOffs)
#'
#'   ## Add a second polygon, trigger
#'   # smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
#'   # smallStudyArea2$ID <- 1L
#'   # smallStudyArea2$shinyLabel <- "zone2"
#'
#'   ## add a new layer -- this will trigger analyses because there are already analyses in the map
#'   ##    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
#'   ##    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
#'   # ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
#'   #              analysisGroup2 = "Smaller Study Area 2",
#'   #              poly = TRUE,
#'   #              layerName = "Smaller Study Area 2") # adds a second studyArea within 1st
#'
#'   ## Add a *different* second polygon, via overwrite. This should trigger new analyses.
#'   # smallStudyArea2 <- randomPolygon(studyArea(ml), 1e5)
#'   # smallStudyArea2$ID <- 1
#'   # smallStudyArea2$shinyLabel = "zone1"
#'
#'   ## add a new layer -- this will trigger analyses because there are already analyses in the map
#'   ##    This will trigger 2 more analyses ... largePatches on each *new* raster x polygon combo
#'   ##    (now there are 2) -- so there is 1 raster group, 3 polygon groups, 2 analyses - Total 6
#'   # ml <- mapAdd(smallStudyArea2, ml, isStudyArea = FALSE, filename2 = NULL, overwrite = TRUE,
#'   #              analysisGroup2 = "Smaller Study Area 2",
#'   #              poly = TRUE,
#'   #              layerName = "Smaller Study Area 2") # adds a second studyArea within 1st
#'
#'   ## Add a 2nd pair of rasters
#'   # rasTemplate <- rast(ext(studyArea(ml)), res = 0.001)
#'   # tsf2 <- randomPolygons(rasTemplate, numTypes = 8)*30
#'   # vtm2 <- randomPolygons(tsf2, numTypes = 4)
#'   # levels(vtm2) <- data.frame(
#'   #   ID = sort(unique(vtm2[])),
#'   #   Factor = c("black spruce", "white spruce", "aspen", "fir")
#'   # )
#'
#'   # ml <- mapAdd(tsf2, ml, filename2 = "tsf2.tif", layerName = "tsf2",
#'   #              tsf = "tsf2.tif",
#'   #              analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)
#'   # ml <- mapAdd(vtm2, ml, filename2 = "vtm2.grd", layerName = "vtm2",
#'   #              vtm = "vtm2.grd",
#'   #              analysisGroup1 = "tsf2_vtm2", leaflet = TRUE, overwrite = TRUE)
#'
#'   ## post hoc analysis of data
#'   ##  use or create a specialized function that can handle the analysesData slot
#'   # ml <- mapAddPostHocAnalysis(map = ml, functionName = "rbindlistAG",
#'   #                             postHocAnalysisGroups = "analysisGroup2",
#'   #                             postHocAnalyses = "all")
#' }
#'
#' ## cleanup
#' withr::deferred_run()
#'
mapAdd <- function(obj, map, layerName,
                   overwrite = getOption("map.overwrite", FALSE), ...) {
  UseMethod("mapAdd", obj)
}

#' @param ... Additional arguments passed to:
#'            [reproducible::postProcess()],
#'            [reproducible::projectInputs()],
#'            [reproducible::fixErrors()],
#'            and [reproducible::prepInputs()].
#'
#' @param isRasterToMatch  Logical indicating whether the object to be added should be considered
#'        a `rasterToMatch`.
#'
#' @param envir An optional environment. If supplied, then the obj
#'        will not be placed "into" the maps slot, rather the environment label will
#'        be placed into the maps slot.
#'
#' @param useCache Logical. If `TRUE`, internal calls to [reproducible::Cache()] will be used.
#'
#' @param useParallel Logical. If `TRUE`, then if there is more than one calculation to do at
#'        any stage, it will create and use a parallel cluster via [pemisc::makeOptimalCluster()].
#'        If running analyses in parallel, it may be useful to pass a function (via `.clInit`)
#'        to be run on each of the nodes immediately upon cluster creation (e.g., to set options).
#'
#' @export
#' @rdname mapAdd
mapAdd.default <- function(obj = NULL, map = new("map"), layerName = NULL,
                           overwrite = getOption("map.overwrite"),
                           columnNameForLabels = 1, leaflet = FALSE, isStudyArea = FALSE,
                           isRasterToMatch = FALSE, envir = NULL,
                           useCache = getOption("reproducible.useCache", TRUE),
                           useParallel = getOption("map.useParallel", FALSE), ...) {
  dots <- list(...)
  .outfile <- dots$outfile
  .clInit <- dots$.clInit
  dots$.clInit <- NULL

  map@metadata <- .enforceColumnTypes(map@metadata) ## update previously-created map objects

  if (is.null(layerName)) {
    stop("layerName is required and cannot be NULL")
  }

  if (is.logical(leaflet)) {
    leaflet <- asPath(ifelse(leaflet, getwd(), NA_character_))
  }

  ## Some of the arguments will need to be passed into Cache
  ## Get obj, if missing, via prepInputs url, or targetFile
  if (is.null(obj)) {
    ## with no obj, we get it first, then pass to mapAdd
    ## don't run postProcess because that will happen in next mapAdd when obj is in hand
    args1 <- identifyVectorArgs(fn = list(Cache, preProcess), ls(), environment(), dots)
    maxNumClus <- if (length(args1$argsMulti)) {
      max(unlist(lapply(args1$argsMulti, NROW)), na.rm = TRUE)
    } else {
      0
    }
    maxNumClus <- min(maxNumClus, getOption("map.maxNumCores"))

    message(
      "  Running prepInputs for:\n",
      paste(capture.output(data.table(file = layerName)), collapse = "\n")
    )
    cl <- makeOptimalCluster(
      maxNumClusters = maxNumClus,
      useParallel = useParallel,
      outfile = .outfile
    )
    if (!is.null(cl) && !is.null(.clInit)) {
      parallel::clusterExport(cl, c(".clInit"), envir = environment())
      parallel::clusterEvalQ(cl, .clInit())
    }
    on.exit(try(stopCluster(cl), silent = TRUE))

    obj <- MapOrDoCall(prepInputs,
      multiple = args1$argsMulti, cl = cl,
      single = args1$argsSingle, useCache = useCache
    )

    tryCatch(
      {
        stopCluster(cl)
        rm(cl)
      },
      error = function(x) invisible()
    )
    if (is(obj, "list")) { ## NOTE: is.list returns TRUE for data.frames ... BAD
      names(obj) <- layerName
    } else if (is(obj, "sf")) { ## NOTE: Aug 2022 workaround #7 by forcing use of sp objects
      obj <- as_Spatial(st_zm(obj))
    }
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

  ## postProcess -- determine studyArea and rasterToMatch from map
  if (is.null(studyArea(map)) && is.null(rasterToMatch(map))) {
    argsFixErrors <- getLocalArgsFor(list(Cache, fixErrors), dots = dots)
    theList <- append(list(x = obj), argsFixErrors)
    obj <- Cache(do.call, fixErrors, theList)
    if (isFALSE(isStudyArea)) {
      message("There is no studyArea in map; consider adding one with 'isStudyArea = TRUE'")
    }
    if (is.na(crs(map))) {
      if (is.null(dots$targetCRS)) { # OK ... user did not pass in targetCRS
        message("No crs already in map, so no reprojection")
      } else {
        argsProjectInputs <- getLocalArgsFor(list(Cache, projectInputs), dots = dots)
        obj <- Cache(do.call, projectInputs, append(x = obj, argsProjectInputs))
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
      .outfile <- dots$outfile
      .clInit <- dots$.clInit
      dots$.clInit <- NULL

      if (!is.null(studyArea(map))) {
        studyArea <- studyArea(map)
      }
      if (!is.null(rasterToMatch(map))) {
        rasterToMatch <- rasterToMatch(map)
      }

      list2env(dots, envir = environment()) ## put arguments from `...` into this local env
      x <- obj ## put it into memory so identifyVectorArgs finds it
      args1 <- identifyVectorArgs(
        fn = list(
          reproducible::Cache,
          getS3method("postProcess", "default"),
          reproducible::maskTo,
          reproducible::projectTo,
          reproducible::cropTo,
          raster::projectRaster, ## TODO use terra
          reproducible::writeOutputs
        ),
        ls(),
        environment(),
        dots = dots
      )

      maxNumClus <- if (length(args1$argsMulti)) {
        max(unlist(lapply(args1$argsMulti, NROW)), na.rm = TRUE)
      } else {
        0
      }
      maxNumClus <- min(maxNumClus, getOption("map.maxNumCores"))

      message("  Fixing, cropping, reprojecting, masking: ", paste(layerName, collapse = ", "))
      cl <- makeOptimalCluster(
        maxNumClusters = maxNumClus,
        useParallel = useParallel,
        outfile = .outfile
      )
      if (!is.null(cl) && !is.null(.clInit)) {
        parallel::clusterExport(cl, c(".clInit"), envir = environment())
        parallel::clusterEvalQ(cl, .clInit())
      }
      on.exit(try(stopCluster(cl), silent = TRUE))

      obj <- MapOrDoCall(postProcess,
        multiple = args1$argsMulti, cl = cl,
        single = args1$argsSingle, useCache = useCache
      )
      tryCatch(
        {
          stopCluster(cl)
          rm(cl)
        },
        error = function(x) invisible()
      )
    }
  }

  ## Purge obj(s) from metadata, if overwrite is TRUE
  objHash <- .robustDigest(obj)
  purgeAnalyses <- NULL ## set default as NULL
  if (layerNameExistsInMetadata) {
    ln <- layerName
    purge <- isFALSE(map@metadata[(layerName %in% ln), objectHash] == objHash)
    if (isTRUE(purge)) {
      if (any(startsWith(colnames(map@metadata), "analysisGroup"))) {
        purgeAnalyses <- map@metadata[layerName %in% ln, get(colnames(map@metadata)[
          startsWith(colnames(map@metadata), "analysisGroup")
        ])]
      }
    }
    map@metadata <- map@metadata[!(layerName %in% ln)]
  }

  ## Add "shinyLabel" column if it is a SpatialPolygonsDataFrame
  args1 <- identifyVectorArgs(
    fn = addColumnNameForLabels,
    c(x = "obj", columnNameForLabels = "columnNameForLabels"),
    environment(), dots = dots
  )
  obj <- MapOrDoCall(addColumnNameForLabels,
    multiple = args1$argsMulti,
    single = args1$argsSingle, useCache = FALSE, cl = NULL
  )

  ## Assign obj to map@.xData
  if (is.null(envir)) {
    envir <- map@.xData ## keep envir for later

    ## Put map into map slot
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
      message(
        "obj named ", paste(layerName, collapse = ", "), " does not exist in envir provided",
        ". Adding it to map obj"
      )
    }
  }

  ## Metadata -- build new entries in data.table -- vectorized
  args1 <- identifyVectorArgs(fn = list(buildMetadata, prepInputs), ls(), environment(), dots = dots) # nolint
  if (length(dots)) {
    howLong <- unlist(lapply(dots, length))
    args1$argsSingle[names(dots)[howLong <= 1]] <- dots[howLong <= 1]
    args1$argsMulti[names(dots)[howLong > 1]] <- dots[howLong > 1]
  }
  moreArgs <- append(args1$argsSingle, alist(metadata = map@metadata))
  if (length(args1$argsMulti) == 0) {
    dts <- do.call(buildMetadata, moreArgs)
  } else {
    dtsList <- do.call(Map, args = append(
      args1$argsMulti,
      list(f = buildMetadata, MoreArgs = moreArgs)
    ))
    dts <- rbindlist(dtsList, use.names = TRUE, fill = TRUE) ## TODO: fails here provMB postprocess
  }

  ## make tiles, if it is leaflet
  if (any(!is.na(leaflet)) && !is.null(dts[["leafletTiles"]])) {
    MBadjustment <- 4000 ## some approx, empirically derived number. Likely only good in some cases.
    MBper <- if (is(obj, "RasterLayer")) { # nolint
      ncell(obj) / MBadjustment
    } else if (tryCatch(is(obj[[1]], "RasterLayer"), error = function(x) FALSE)) {
      ncell(obj[[1]]) / MBadjustment
    } else {
      4600 ## seems to be approx mem use for a prov
    }
    if (isTRUE(all(dir.exists(dts[["leafletTiles"]])))) {
      useParallel <- FALSE
    } else {
      if (missing(useParallel)) {
        useParallel <- getOption("map.useParallel", !identical("windows", .Platform$OS.type))
      }
    }

    cl <- makeOptimalCluster(
      useParallel = useParallel, MBper = MBper,
      maxNumClusters = min(length(obj), getOption("map.maxNumCores")),
      outfile = .outfile
    )
    if (!is.null(cl) && !is.null(.clInit)) {
      parallel::clusterExport(cl, c(".clInit"), envir = environment())
      parallel::clusterEvalQ(cl, .clInit())
    }
    on.exit(try(stopCluster(cl), silent = TRUE))

    tilePath <- dts[["leafletTiles"]]
    args1 <- identifyVectorArgs(fn = makeTiles, ls(), environment(), dots = dots)
    out <- MapOrDoCall(makeTiles,
      multiple = args1$argsMulti,
      single = args1$argsSingle, useCache = FALSE, cl = cl
    )
    ## if the rasters are identical, then there may be errors
    tryCatch(
      {
        stopCluster(cl)
        rm(cl)
      },
      error = function(x) invisible()
    )
  }

  ## set CRS
  if (isTRUE(isStudyArea)) {
    if ((!is.null(studyArea(map))) && isStudyArea) {
      message("map already has a studyArea; adding another one as study area ", dts[["studyArea"]])
    } else {
      message("Setting map CRS to this layer because it is the (first) studyArea inserted")
      map@CRS <- raster::crs(obj) |> sf::st_crs()
    }
  }

  ## rbindlist new metadata with existing metadata

  ## 2022-10-04: workaround path columns having same class() but sorted slightly differently, e.g.,
  ## Browse[4]> class(map@metadata$leaflet) == class(dts$leaflet)
  ## [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
  ## Browse[4]> sort(class(map@metadata$leaflet)) == sort(class(dts$leaflet))
  ## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

  pathCols1 <- names(which(sapply(dts, is, class2 = "Path")))
  pathCols2 <- names(which(sapply(map@metadata, is, class2 = "Path")))

  classes1 <- lapply(dts[, ..pathCols1], class)
  allClasses1Identical <- all(vapply(seq_along(classes1[-1]), function(i) {
    identical(classes1[[i]], classes1[[i + 1]])
  }, logical(1)))
  if (isFALSE(allClasses1Identical)) {
    warning("Some columns in dts corresponding to Paths do not have identical class.")
  }
  class1 <- classes1[[1]]

  classes2 <- lapply(map@metadata[, ..pathCols2], class)
  allClasses2Identical <- all(vapply(seq_along(classes2[-1]), function(i) {
    identical(classes2[[i]], classes2[[i + 1]])
  }, logical(1)))
  if (isFALSE(allClasses2Identical)) {
    warning("Some columns in map@metadata corresponding to Paths do not have identical class.")
  }
  class2 <- classes2[[1]]

  if (!identical(class1, class2)) {
    if (!is.null(dts[["destinationPath"]])) class(dts[["destinationPath"]]) <- class2
    if (!is.null(dts[["filename2"]])) class(dts[["filename2"]]) <- class2
    if (!is.null(dts[["leaflet"]])) class(dts[["leaflet"]]) <- class2
    if (!is.null(dts[["leafletTiles"]])) class(dts[["leafletTiles"]]) <- class2
    if (!is.null(dts[["targetFile"]])) class(dts[["targetFile"]]) <- class2
    if (!is.null(dts[["tsf"]])) class(dts[["tsf"]]) <- class2
    if (!is.null(dts[["vtm"]])) class(dts[["vtm"]]) <- class2
  }

  map@metadata <- rbindlist(list(map@metadata, dts), use.names = TRUE, fill = TRUE)

  ## run map analyses
  map <- runMapAnalyses(map = map, purgeAnalyses = purgeAnalyses, useParallel = useParallel)

  return(map)
}

#' Remove objects from a `map`
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
#' if (require("SpaDES.tools", quietly = TRUE)) {
#'   p <- terra::vect(cbind(-120, 60), crs = "epsg:4326") |>
#'     SpaDES.tools::randomPolygon(area = 1e5) |>
#'     sf::st_as_sf() |>
#'     sf::as_Spatial()
#'   m <- mapAdd(p, layerName = "p")
#'   m
#'
#'   m <- mapRm(m, "p")
#'   m
#' }
mapRm <- function(map, layer, ask = TRUE, ...) {
  UseMethod("mapRm", map)
}

#' @export
#' @aliases mapRm
#' @family mapMethods
#' @rdname mapRm
mapRm.map <- function(map, layer = NULL, ask = TRUE, ...) {
  if (is.character(layer)) {
    layer <- map@metadata[, which(layerName %in% layer)]
  }

  layerName <- unique(map@metadata[layer, layerName])
  if (length(layer) > 1) {
    stop(
      "There are more than obj in map with that layer name, '", layerName, "'.",
      " Please indicate layer by row number in map@metadata."
    )
  }

  rm(list = layerName, envir = map@metadata[layer, envir][[1]])
  map@metadata <- map@metadata[-layer, ]

  if (NROW(map@analyses)) {
    message(
      "Layer ", layerName, " has been removed, but not any analysis that ",
      "was previously run using this layer."
    )
  }

  map
}

if (!isGeneric("crs")) {
  setGeneric("crs", function(x, ...) {
    standardGeneric("crs")
  })
}

#' Extract the CRS of a `map`
#'
#' @inheritParams raster::crs
#'
#' @export
#' @family mapMethods
#' @importMethodsFrom raster crs
#' @rdname crs
setMethod("crs",
  signature = "map",
  function(x, ...) {
    if (!is.null(x@CRS)) {
      x@CRS
    } else {
      NA_character_
    }
  }
)

#' Map class methods
#'
#' Tools for getting objects and metadata in and out of a `map` class.
#'
#' @param x TODO: document this
#'
#' @param layer TODO: document this
#'
#' @param ... Additional arguments passed to other methods (not used)
#'
#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName <- function(x, layer, ...) {
  UseMethod("studyAreaName", x)
}

#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName.map <- function(x, layer = 1, ...) {
  if (sum(x@metadata$studyArea, na.rm = TRUE)) {
    if (isTRUE(is.na(layer))) {
      layer <- max(x@metadata$studyArea, na.rm = TRUE)
    }
    x@metadata[studyArea == layer]$layerName
  } else {
    NULL
  }
}

#' @export
#' @family mapMethods
#' @rdname studyAreaName
studyAreaName.data.table <- function(x, layer = 1, ...) {
  if (sum(x$studyArea, na.rm = TRUE)) {
    if (isTRUE(is.na(layer))) {
      layer <- max(x$studyArea, na.rm = TRUE)
    }
    x[studyArea == layer]$layerName
  } else {
    NULL
  }
}

#' Extract the studyArea(s) from a `map`
#'
#' If `layer` is not provided and there is more than one `studyArea`,
#' then this will extract the last one added.
#'
#' @param map TODO: document this
#'
#' @param sorted Logical. Should the numeric `layer` be referring to
#'               geographic area of the `area` or the order that
#'               the `studyArea` were placed into map object
#'
#' @param layer TODO: document this
#'
#' @export
#' @family mapMethods
#' @rdname studyArea
setGeneric("studyArea", function(map, layer = NA, sorted = FALSE) {
  standardGeneric("studyArea")
})

#' @export
#' @family mapMethods
#' @rdname studyArea
setMethod("studyArea", "ANY",
  definition = function(map, layer = NA, sorted = FALSE) {
    NULL
  }
)

#' @export
#' @family mapMethods
#' @rdname studyArea
setMethod("studyArea", "map",
  definition = function(map, layer = NA, sorted = FALSE) {
    if (isTRUE(sorted)) {
      studyAreas <- map@metadata[!is.na(map@metadata$studyArea), ]
      mapSorted <- studyAreas[order(area, decreasing = FALSE), ][, studyArea := as.numeric(.I)] # nolint
      san <- studyAreaName(mapSorted, layer = layer)
    } else {
      san <- studyAreaName(map, layer = layer)
    }
    if (length(san) > 0) {
      get(san, map@metadata[layerName == san]$envir[[1]])
    } else {
      NULL
    }
  }
)

#' @param value The value to assign to the object.
#'
#' @export
#' @rdname studyArea
setGeneric("studyArea<-", function(map, layer = NA, value) {
  standardGeneric("studyArea<-")
})

#' @export
#' @family mapMethods
#' @rdname studyArea
setReplaceMethod("studyArea",
  signature = "map",
  definition = function(map, layer = NA, value) {
    ln <- studyAreaName(map, layer = layer)
    map[[ln]] <- value
    map
  }
)

if (!isGeneric("rasterToMatch")) {
  setGeneric(
    "rasterToMatch",
    function(x, ...) {
      standardGeneric("rasterToMatch")
    }
  )
}

#' Extract the `rasterToMatch`(s) from a `map` object
#'
#' If `layer` is not provided and there is more than one `studyArea`,
#' then this will extract the last one added.
#'
#' @param x a `map` object
#'
#' @param layer an integer identifying the index of the `rasterToMatch` to extract
#'
#' @export
#' @exportMethod rasterToMatch
#' @family mapMethods
#' @rdname rasterToMatch
#' @importMethodsFrom pemisc rasterToMatch
setMethod("rasterToMatch",
  signature = "map",
  definition = function(x, layer = 1) {
    rtms <- x@metadata$rasterToMatch
    if (sum(rtms, na.rm = TRUE)) {
      if (isTRUE(is.na(layer))) {
        layer <- max(x@metadata$rasterToMatch, na.rm = TRUE)
      }
      if (!layer %in% rtms) layer <- min(rtms, na.rm = TRUE)
      get(x@metadata[rasterToMatch == layer, ]$layerName, x@.xData)
    } else {
      NULL
    }
  }
)

################################################################################
#' Extract objects of specific classes from a `map` object
#'
#' - `rasters()` extracts `RasterLayer` objects;
#' - `sf()` extracts `sf` objects;
#' - `sp()` extracts `Spatial` objects;
#' - `spatialPolygons()` extracts `SpatialPolygons` objects;
#' - `spatialPoints()` extracts `SpatialPoints` objects;
#' - `spatRasters()` extracts `SpatRaster` objects;
#' - `spatVectors()` extracts `SpatVector` objects;
#'
#' @export
#' @family mapMethods
#' @rdname maps
rasters <- function(map, ...) {
  UseMethod("rasters", map)
}

#' @export
#' @family mapMethods
#' @rdname maps
rasters.map <- function(map, ...) {
  maps(map, "RasterLayer")
}

#' @export
#' @family mapMethods
#' @rdname maps
sp <- function(map, ...) {
  UseMethod("sp")
}

#' @export
#' @family mapMethods
#' @rdname maps
sp.map <- function(map, ...) {
  maps(map, "Spatial")
}

#' @export
#' @family mapMethods
#' @rdname maps
sf <- function(map, ...) {
  UseMethod("sf", map)
}

#' @export
#' @family mapMethods
#' @rdname maps
sf.map <- function(map, ...) {
  maps(map, "sf")
}

#' @export
#' @family mapMethods
#' @rdname maps
spatialPolygons <- function(map, ...) {
  UseMethod("spatialPolygons", map)
}

#' @export
#' @rdname maps
spatialPolygons.map <- function(map, ...) {
  maps(map, "SpatialPolygons")
}

#' @export
#' @family mapMethods
#' @rdname maps
spatialPoints <- function(map, ...) {
  UseMethod("spatialPoints", map)
}

#' @export
#' @rdname maps
spatialPoints.map <- function(map, ...) {
  maps(map, "SpatialPoints")
}

#' @export
#' @family mapMethods
#' @rdname maps
spatRasters <- function(map, ...) {
  UseMethod("spatRasters", map)
}

#' @export
#' @family mapMethods
#' @rdname maps
spatRasters.map <- function(map, ...) {
  maps(map, "SpatRasters")
}

#' @export
#' @family mapMethods
#' @rdname maps
spatVectors <- function(map, ...) {
  UseMethod("spatVectors", map)
}

#' @export
#' @family mapMethods
#' @rdname maps
spatVectors.map <- function(map, ...) {
  maps(map, "SpatVectors")
}

#' Extract leaflet tile paths from a `map` obj
#'
#' @param map A `map` class obj
#'
#' @export
#' @return
#' A vector of paths indicating the relative paths. Any layers
#' that don't have leaflet tiles will return NA.
leafletTiles <- function(map) {
  x <- map@metadata[["layerName"]]
  tiles <- map@metadata[["leafletTiles"]]
  names(tiles) <- x
  tiles
}

#' Extract maps from a `map` object
#'
#' This will extract all objects in or pointed to within the `map`.
#'
#' @param map A `map` object
#'
#' @param class If supplied, this will be the class of objects returned.
#'              Default is `NULL` which is "all", meaning all objects in the `map` object.
#'
#' @param layerName character string giving the name(s) of layer(s) to extract.
#'
#' @param ... Additional arguments passed to other methods (not used)
#'
#' @return A list of geospatial objects of class `class`
#'
#' @export
maps <- function(map, class = NULL, layerName = NULL) {
  if (!is.null(layerName)) {
    layers <- layerName
    meta <- map@metadata[layerName %in% layers]
  } else {
    meta <- map@metadata
  }
  x <- meta$layerName
  names(x) <- x
  envirs <- meta$envir
  names(envirs) <- x
  out <- Map(
    envir = envirs, x = x,
    function(envir, x) {
      get(x, envir = envir, inherits = FALSE)
    }
  )
  if (!is.null(class)) {
    classOnly <- unlist(lapply(out, is, class2 = class))
    out <- out[classOnly]
  }

  return(out)
}

#' @keywords internal
.singleMetadataNAEntry <- data.table::data.table(
  layerName = NA_character_,
  layerType = NA_character_,
  columnNameForLabels = NA_character_,
  envir = list(),
  leaflet = FALSE,
  studyArea = 0
)

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
  }
)

.formalsReproducible <- c(
  formalArgs(reproducible::preProcess),
  formalArgs(reproducible::postProcess),
  formalArgs(reproducible:::determineFilename),
  formalArgs(reproducible::cropInputs),
  formalArgs(reproducible::maskInputs),
  formalArgs(reproducible::projectInputs)
) |>
  unique()

################################################################################
#' Extract metadata
#'
#'
#'
#' @param x A `map`, `Raster`, or `SpatRaster` object
#'
#' @param ... Additional arguments passed to other methods (not used)
#'
#' @export
#' @rdname metadata
metadata <- function(x, ...) {
  UseMethod("metadata", x)
}

#' @export
#' @rdname metadata
metadata.map <- function(x, ...) {
  x@metadata
}

#' @export
#' @rdname metadata
metadata.Map <- function(x, ...) {
  x$metadata
}

#' @export
#' @rdname metadata
metadata.Raster <- function(x, ...) {
  raster::metadata(x)
}

#' @export
#' @rdname metadata
metadata.SpatRaster <- function(x, ...) {
  terra::meta(x)
}

################################################################################
#' Add `shinyLabel` column (attribute) to spatial vectors
#'
#' @param x a spatial vector object (`sf`, `SpatialPolygons`, `SpatVector`)
#'
#' @param columnNameForLabels character or integer identifying an existing column (attribute)
#'        to use as the `shinyLabel`.
#'
#' @param ... Additional arguments passed to other methods (not used)
#'
#' @return a modified object with the same class as `x`
#'
#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels <- function(x, columnNameForLabels, ...) {
  UseMethod("addColumnNameForLabels", x)
}

#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels.default <- function(x, columnNameForLabels, ...) {
  return(x)
}

#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels.list <- function(x, columnNameForLabels, ...) {
  lapply(x, addColumnNameForLabels, columnNameForLabels = columnNameForLabels)
}

#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels.sf <- function(x, columnNameForLabels, ...) {
  if (ncol(x) > 0) {
    x[["shinyLabel"]] <- x[[columnNameForLabels]]
  }

  return(x)
}

#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels.SpatialPolygonsDataFrame <- function(x, columnNameForLabels, ...) {
  if (ncol(x) > 0) {
    x[["shinyLabel"]] <- x[[columnNameForLabels]]
  }

  return(x)
}

#' @export
#' @rdname addColumnNameForLabels
addColumnNameForLabels.SpatVector <- function(x, columnNameForLabels, ...) {
  if (ncol(x) > 0) {
    x[["shinyLabel"]] <- x[[columnNameForLabels]]
  }

  return(x)
}
