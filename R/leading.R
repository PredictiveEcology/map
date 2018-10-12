#' Calculate proportion of landscape occupied by each vegetation class
#'
#' This function is recursive.
#' If \code{polygonToSummarizeBy} is a \code{SpatialPolygon}, then the function
#' will enter once, and convert this to a fasterized version, and pass that into
#' the function replacing \code{polygonToSummarizeBy}.
#' It is also recursive of passed a vector of filenames for \code{tsf} and \code{vtm}.
#'
#' @param tsf TODO
#' @param vtm TODO
#' @param polygonToSummarizeBy TODO
#' @param ageClassCutOffs TODO
#' @param ageClasses TODO
#' @param objName
#' @param ... TODO
#'
#' @return A \code{data.table} with proportion of the pixels in each vegetation class,
#'         for each given age class within each polygon.
#'
leadingByStage <- function(tsf, vtm, polygonToSummarizeBy,
                           ageClassCutOffs,  ageClasses, objName = NULL, ...) {
  if (!is.null(tsf)) {
    if (is.list(polygonToSummarizeBy)) {
      if (!is.null(objName)) {
        message("    ", objName, ":  Calculating leading species by stage")
      }
      out <- Map(
        polygonToSummarizeBy = polygonToSummarizeBy,
        objName = names(polygonToSummarizeBy),
        leadingByStage,
        MoreArgs = list(
          tsf = tsf,
          vtm = vtm,
          ageClassCutOffs = ageClassCutOffs,
          ageClasses = ageClasses,
          ...
        )
      )
      return(out)
    } else {
      if (!is(polygonToSummarizeBy, "Raster")) {
        # if a polygon, fasterize it then rerun
        if (is.list(tsf)) {
          singleTsf <- unlist(tsf)[1]
        } else {
          singleTsf <- tsf
        }
        if (!is.null(singleTsf)) {
          if (!is.null(objName)) message("      ", objName, ":  Fasterizing")
          aa2 <- Cache(fasterize2, raster(raster(singleTsf)), polygonToSummarizeBy,
                       field = "polygonNum")
          out <- leadingByStage(
            tsf = tsf,
            vtm = vtm,
            polygonToSummarizeBy = aa2,
            ageClassCutOffs = ageClassCutOffs,
            ageClasses = ageClasses,
            ...
          )
          return(out)
        } else {
          return(NULL)
        }
      } else {
        if (is.list(tsf)) {
          # if tsf is a list
          out <- Map(
            tsf = tsf,
            vtm = vtm,
            leadingByStage,
            MoreArgs = list(
              polygonToSummarizeBy = polygonToSummarizeBy,
              ageClassCutOffs = ageClassCutOffs,
              ageClasses = ageClasses,
              ...
            )
          )
          if ((is.list(out) && length(out) == 1)) {
            out <- out[[1]]
          }
          return(out)
        } else {
          if (length(tsf) > 1) {
            # recursive call to this same function, but one tsf and vtm at a time
            numClust <- optimalClusterNum(16000, maxNumClusters = length(tsf))
            out <- if (numClust > 1) {
              cl <- makeForkCluster(numClust, type = "FORK")
              on.exit(stopCluster(cl), add = TRUE)
              clusterMap(cl = cl,
                         leadingByStage,
                         tsf = tsf,
                         vtm = vtm,
                         objName = basename(tsf),
                         MoreArgs = list(
                           polygonToSummarizeBy = polygonToSummarizeBy,
                           ageClassCutOffs = ageClassCutOffs,
                           ageClasses = ageClasses,
                           ...
                         ),
                         .scheduling = "dynamic")
            } else {
              Map(
                leadingByStage,
                tsf = tsf,
                vtm = vtm,
                objName = basename(filename(tsf)),
                MoreArgs = list(
                  polygonToSummarizeBy = polygonToSummarizeBy,
                  ageClassCutOffs = ageClassCutOffs,
                  ageClasses = ageClasses,
                  ...
                )
              )
            }

            names(out) <- basename(names(out))
            out <- rbindlist(out)
            amc::.gc()
            return(out)
          } else {
            startTime <- Sys.time()
            if (is.null(objName)) {
              objName = basename(tsf)
            }
            message("        ", objName, ":  Calculating leading species by stage")

            # main function code
            if (tail(ageClassCutOffs, 1) != Inf)
              ageClassCutOffs <- c(ageClassCutOffs, Inf)

            timeSinceFireFilesRast <- raster(tsf[1])
            timeSinceFireFilesRast[] <- timeSinceFireFilesRast[]

            # Use this when NOT in parallel
            #timeSinceFireFilesRast <- Cache(rasterToMemory, tsf[1])

            rasTsf <- reclassify(
              timeSinceFireFilesRast,
              cbind(
                from = ageClassCutOffs[-length(ageClassCutOffs)] -
                  0.1,
                to = ageClassCutOffs[-1],
                seq_along(ageClasses)
              )
            )

            levels(rasTsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

            rasVeg <- raster(vtm[1])
            rasVeg[] <- rasVeg[] # 3 seconds

            splitVal <- paste0("_", 75757575, "_") # unlikely to occur for any other reason

            # Individual species
            nas3 <- is.na(rasVeg[]) | rasVeg[] == 0
            nas1 <- is.na(rasTsf[]) | rasTsf[] == 0
            nas <- nas3 | nas1
            name1 <- as.character(factorValues(rasTsf, rasTsf[][!nas])[, 1])
            #as.character(raster::levels(rasTsf)[[1]]$Factor)[rasTsf[][!nas]]
            name3 <- as.character(factorValues(rasVeg, rasVeg[][!nas])[, 1])
            #as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]
            ff <- paste(name1, name3, sep = splitVal) # 4 seconds

            ras <- raster(rasVeg)
            ffFactor <- factor(ff)
            ras[!nas] <- ffFactor # 2 seconds

            eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
            types <- strsplit(as.character(eTable$VALUE), split = splitVal)
            types <- do.call(rbind, types)

            levels(ras) <- data.frame(eTable, ageClass = types[, 1], vegCover = types[, 2])

            levs <- raster::levels(polygonToSummarizeBy)[[1]]

            # this is same, if all values present: e.g., 1, 2, 3, 4, 5 ...,
            # but not if missing: e.g., 1, 2, 3, 5
            levs <- factorValues(polygonToSummarizeBy, levs$ID)
            facVals <- factorValues(
              polygonToSummarizeBy,
              polygonToSummarizeBy[],
              att = c("shinyLabel", "polygonNum") ## TODO: these don't exist in the raster; causes error
            )

            bb <- data.table(
              zone = facVals$shinyLabel,
              polygonID = facVals$polygonNum,
              cell = seq_len(ncell(ras))
            )
            #bb <- na.omit(bb)

            # add age and vegCover by reference
            bb[, c("ageClass", "vegCover") := factorValues(ras, ras[][bb$cell], att = c("ageClass", "vegCover"))]
            bb <- na.omit(bb)

            #set(bb, , "zone", polygonToSummarizeBy$shinyLabel[as.numeric(bb$polygonNum)])
            tabulated <- bb[, list(N1 = .N), by = c("zone", "polygonID", "ageClass", "vegCover")]
            tabulated[, proportion := round(N1 / sum(N1), 4), by = c("zone", "ageClass")]

            allCombos <- expand.grid(
              ageClass = ageClasses,
              stringsAsFactors = FALSE,
              vegCover = raster::levels(rasVeg)[[1]]$Factor,
              zone = levs$shinyLabel# factorValues(polygonToSummarizeBy, 1:5)$shinyLabel
            )
            allCombos$polygonID <- match(allCombos$zone, levs$shinyLabel)
            #allCombos$proportion <- 0
            data.table::setDT(allCombos)

            #allCombos[tabulated, on = c("zone", "vegCover", "ageClass")]

            tabulated <- merge(
              tabulated,
              allCombos,
              by = c("zone", "vegCover", "ageClass", "polygonID"),
              all.y = TRUE
            )
            tabulated[is.na(proportion), proportion := 0]
            set(tabulated, , "N1", NULL)
            set(tabulated,
                ,
                "label",
                paste(
                  tabulated$ageClass,
                  paste(basename(dirname(tsf)), basename(tsf), sep = "_"),
                  sep = "."
                ))

            endTime <- Sys.time()
            message("    Leading cover calculation took ",
                    format(endTime - startTime, digits = 2))

            return(tabulated)
          }
        }
      }
    }
  }
}

mapLeadingByStage <- function(map, ...) {
  # a data.table
  m <- map@metadata
  listEntry <- "Leading by stage"

  if (is.null(m$analysisGroup)) {
    stop("Expecting analysisGroup column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  ags <- sort(na.omit(unique(m$analysisGroup)))
  polys <- maps(map, "SpatialPolygons")
  combos <- map@analysesData[[listEntry]]$.LeadingDone
  if (is.null(combos))
    combos <- character()

  out <- Map(poly = polys, polyName = names(polys), function(poly, polyName) {
    out2 <- lapply(ags, function(ag) {
      comboNew <- paste(ag, polyName, sep = "_")
      if (!comboNew %in% combos) {
        tsf <- m[tsf==TRUE & analysisGroup==ag, filename2]
        vtm <- m[vtm==TRUE & analysisGroup==ag, filename2]
        message("  Calculating leading by stage for ", comboNew)
        out3 <- Cache(leadingByStage2, asPath(tsf), asPath(vtm), poly, ...)
        combos <<- c(combos, comboNew)
        list(dt = out3)
      }
    })
  })
  map@analysesData[[listEntry]]$.LeadingDone <- combos
  map@analysesData[[listEntry]] <- out
  map
}

leadingByStage2 <- function(tsf, vtm, polygonToSummarizeBy,
                            ageClassCutOffs,  ageClasses, ...) {
  # main function code
  startTime <- Sys.time()
  if (tail(ageClassCutOffs, 1) != Inf)
    ageClassCutOffs <- c(ageClassCutOffs, Inf)

  # prepare tsf rasters
  timeSinceFireFilesRast <- raster(tsf[1])
  timeSinceFireFilesRast[] <- timeSinceFireFilesRast[]

  # Use this when NOT in parallel
  #timeSinceFireFilesRast <- Cache(rasterToMemory, tsf[1])

  rasTsf <- reclassify(
    timeSinceFireFilesRast,
    cbind(
      from = ageClassCutOffs[-length(ageClassCutOffs)] -
        0.1,
      to = ageClassCutOffs[-1],
      seq_along(ageClasses)
    )
  )

  levels(rasTsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

  # prepare vtm rasters
  rasVeg <- raster(vtm[1])
  rasVeg[] <- rasVeg[] # 3 seconds

  splitVal <- paste0("_", 75757575, "_") # unlikely to occur for any other reason

  # Individual species
  nas3 <- is.na(rasVeg[]) | rasVeg[] == 0
  nas1 <- is.na(rasTsf[]) | rasTsf[] == 0
  nas <- nas3 | nas1
  name1 <- as.character(factorValues(rasTsf, rasTsf[][!nas])[, 1])
  #as.character(raster::levels(rasTsf)[[1]]$Factor)[rasTsf[][!nas]]
  name3 <- as.character(factorValues(rasVeg, rasVeg[][!nas])[, 1])
  #as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]
  ff <- paste(name1, name3, sep = splitVal) # 4 seconds

  ras <- raster(rasVeg)
  ffFactor <- factor(ff)
  ras[!nas] <- ffFactor # 2 seconds

  eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
  types <- strsplit(as.character(eTable$VALUE), split = splitVal)
  types <- do.call(rbind, types)

  levels(ras) <- data.frame(eTable, ageClass = types[, 1], vegCover = types[, 2])

  # prepare polygonToSummarizeBy factor raster
  if (is(polygonToSummarizeBy, "SpatialPolygons")) {
    if (!"shinyLabel" %in% colnames(polygonToSummarizeBy@data))
      stop("polygonToSummarizeBy must have a column 'shinyLabel'")
    polygonToSummarizeBy <- Cache(fasterize2, rasTsf, polygonToSummarizeBy,
                 field = "polygonNum")
  }
  levs <- raster::levels(polygonToSummarizeBy)[[1]]

  # this is same, if all values present: e.g., 1, 2, 3, 4, 5 ...,
  # but not if missing: e.g., 1, 2, 3, 5
  levs <- factorValues(polygonToSummarizeBy, levs$ID)
  facVals <- factorValues(
    polygonToSummarizeBy,
    polygonToSummarizeBy[],
    att = c("shinyLabel", "polygonNum")
  )

  bb <- data.table(
    zone = facVals$shinyLabel,
    polygonID = facVals$polygonNum,
    cell = seq_len(ncell(ras))
  )

  # add age and vegCover by reference
  bb[, c("ageClass", "vegCover") := factorValues(ras, ras[][bb$cell], att = c("ageClass", "vegCover"))]
  bb <- na.omit(bb)

  # One species at a time
  tabulated <- bb[, list(NPixels = .N), by = c("zone", "polygonID", "ageClass", "vegCover")]
  tabulated[, proportion := round(NPixels / sum(NPixels), 4), by = c("zone", "vegCover")]

  # All species
  tabulated2 <- bb[, list(NPixels = .N), by = c("zone", "polygonID", "ageClass")]
  tabulated2[, proportion := round(NPixels / sum(NPixels), 4), by = c("zone")]
  set(tabulated2, , "vegCover", "All species")

  tabulated <- rbindlist(list(tabulated, tabulated2), use.names = TRUE, fill = TRUE)

  allCombos <- expand.grid(
    ageClass = ageClasses,
    stringsAsFactors = FALSE,
    vegCover = raster::levels(rasVeg)[[1]]$Factor,
    zone = levs$shinyLabel
  )
  allCombos$polygonID <- match(allCombos$zone, levs$shinyLabel)
  data.table::setDT(allCombos)

  tabulated <- merge(
    tabulated,
    allCombos,
    by = c("zone", "vegCover", "ageClass", "polygonID"),
    all.y = TRUE
  )
  # fill in zeros where there is no value
  tabulated[is.na(proportion), proportion := 0]
  set(tabulated,
      ,
      "label",
      paste(
        tabulated$ageClass,
        paste(gsub(basename(dirname(tsf)), pattern = "\\.", replacement = ""),
              basename(tsf), sep = "_"),
        sep = "."
      ))

  endTime <- Sys.time()
  message("    Leading cover calculation took ",
          format(endTime - startTime, digits = 2))

  return(tabulated)

}

#' Fasterize with crop & spTransform first
#'
#' @param emptyRaster An empty raster with res, crs, extent all
#'        correct for to pass to \code{fasterize}
#' @param polygonToFasterize passed to \code{fasterize}, but it
#'        will be cropped first if
#'        \code{extent(emptyRaster) < extent(polygonToFasterize)}
#' @param field passed to fasterize
#' @importFrom reproducible cropInputs projectInputs
fasterize2 <- function(emptyRaster, polygonToFasterize, field) {
  ras <- raster(emptyRaster)
  if (extent(polygonToFasterize) > extent(ras)) {
    polygonToFasterize <- Cache(cropInputs, polygonToFasterize, rasterToMatch = ras)
  }
  thePoly <- projectInputs(polygonToFasterize, targetCRS = crs(ras))
  thePoly$polygonNum <- seq_along(thePoly)
  if (!is.factor(thePoly[[field]])) {
    thePoly[[field]] <- factor(thePoly[[field]])
  }
  aa2 <- fasterize::fasterize(sf::st_as_sf(thePoly), ras, field = field)
  #levels(aa2) <- data.frame(ID = seq_len(nlevels(thePoly[[field]])),
  #                          Factor = levels(thePoly[[field]]))
  levels(aa2) <- data.frame(ID = seq_along(thePoly[[field]]),
                            Factor = as.character(thePoly[[field]]),
                            as.data.frame(thePoly))

  aa2
}

optimalClusterNum <- function(memRequiredMB = 5000, maxNumClusters = 1) {
  if (Sys.info()["sysname"] == "Linux") {
    detectedNumCores <- parallel::detectCores()
    shouldUseCluster <- (maxNumClusters > 0)

    if (shouldUseCluster) {
      # try to scale to available RAM
      try(aa <- system("free -lm", intern = TRUE))
      if (!is(aa, "try-error")) {
        bb <- strsplit(aa[2], split = " ") # 'Mem:' row
        availMem <- as.numeric(bb[[1]][nzchar(bb[[1]])][7]) # 'available' column
        numClusters <- floor(min(detectedNumCores, availMem / memRequiredMB))
      } else {
        message("The OS function, 'free' is not available. Returning 1 cluster")
        numClusters <- 1
      }
      numClusters <- min(maxNumClusters, numClusters, detectedNumCores)
    } else {
      numClusters <- 1
    }
  } else {
    message("This function returns 1 cluster on Windows and MacOS.")
    numClusters <- 1
  }
  return(numClusters)
}

#' Create a parallel Fork cluster, if useful
#'
#' Given the size of a problem, it may not be useful to create a cluster.
#' This will make a Fork cluster (so Linux only)
#' @param useParallel Logical. If \code{FALSE}, returns NULL
#' @param MBper Numeric. Passed to \code{memRequiredMB} in
#'              \code{\link{optimalClusterNum}}
#' @param maxNumClusters Numeric or Integer. The theoretical upper limit
#'        for number of clusters to create (e.g., because there are only
#'        3 problems to solve, not \code{parallel::detectCores})
#' @param ... Passed to \code{makeForkClusterRandom}.
#'            Only relevant for \code{iseed}.
makeOptimalCluster <- function(useParallel = FALSE, MBper = 5e3,
                               maxNumClusters = parallel::detectCores(), ...) {
  if (isTRUE(useParallel) && tolower(Sys.info()[["sysname"]]) != "windows") {
    numClus <- optimalClusterNum(MBper, maxNumClusters = maxNumClusters)
    if (numClus <= 1) {
      NULL
    } else {
      makeForkClusterRandom(numClus, ...)
    }
  } else {
    NULL
  }
}

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

