
#' Generic analysis for map objects
#'
#'
mapAnalysis <- function(map, functionName = NULL, ...) {
  m <- map@metadata

  if (is.null(functionName)) {
    stop("Each analysis must have a functionName")
  }
  if (is.null(m$analysisGroup1)) {
    stop("Expecting analysisGroup1 column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  AGs <- sort(unique(colnames(m)[startsWith(colnames(m), "analysisGroup")]))
  names(AGs) <- AGs
  ags <- lapply(AGs, function(AG) sort(na.omit(unique(m[[AG]]))))
  combosCompleted <- map@analysesData[[functionName]]$.Completed

  #if (is.null(combosCompleted)) {
    if (any(unlist(lapply(ags, function(x) length(x>0))))) {
      combosAll <- do.call(expand.grid, args = append(list(stringsAsFactors = FALSE),
                                                      lapply(ags, function(x) x)))
      combosAll$all <- apply(combosAll, 1, paste, collapse = "._.")
    }
  #}
  combosToDo <- combosAll[!combosAll$all %in% combosCompleted,]
  formalsInFunction <- formalArgs(functionName)[formalArgs(functionName) %in% colnames(m)]
  names(formalsInFunction) <- formalsInFunction

  if (NROW(combosToDo)) {
    out <- by(combosToDo, combosToDo$all, simplify = FALSE,
              function(combo) {

                # Cycle through for each analysisGroup, get each argument
                args <- lapply(AGs, function(AG) {
                  args <- lapply(formalsInFunction, function(arg) {
                    val <- na.omit(m[get(AG) == combo[[AG]], ][[arg]])
                    if (isTRUE(val)) {
                      val <- get(m[get(AG) == combo[[AG]], layerName],
                                 envir = m[get(AG) == combo[[AG]], envir][[1]])
                    }
                    if (length(val)>0)
                      assign(arg, val)
                    else
                      NULL
                  })
                  args[!sapply(args, is.null)]
                })
                args <- unlist(unname(args), recursive = FALSE)
                message("  Calculating ", functionName, " for ", combo$all)
                fnOut <- do.call(Cache, args = c(list(get(functionName)), args, list(...)))
                combosCompleted <<- c(combosCompleted, combo$all)
                list(dt = fnOut)
              })

    map@analysesData[[functionName]][names(out)] <- out
    map@analysesData[[functionName]]$.Completed <- combosCompleted
  }
  map
}


mapLargePatches <- function(map, ...) {
  m <- map@metadata

  listEntry <- "Large patches"
  if (is.null(m$analysisGroup)) {
    stop("Expecting analysisGroup column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  ags <- sort(na.omit(unique(m$analysisGroup)))
  polys <- maps(map, "SpatialPolygons")
  combos <- map@analysesData[[listEntry]]$.Completed
  if (is.null(combos))
    combos <- character()

  out <- Map(poly = polys, polyName = names(polys), function(poly, polyName) {
    out2 <- lapply(ags, function(ag) {
      comboNew <- paste(ag, polyName, sep = "_")
      if (!comboNew %in% combos) {
        tsf <- m[tsf==TRUE & analysisGroup==ag, filename2]
        vtm <- m[vtm==TRUE & analysisGroup==ag, filename2]
        message("  Calculating Large Patches for ", comboNew)
        out3 <- Cache(LargePatches, tsf = tsf,
                      vtm = vtm,
                      byPoly = poly, ...)
        combos <<- c(combos, comboNew)
        list(dt = out3)
      }
    })
  })
  browser()
  map@analysesData[[listEntry]][names(out)] <- out
  map@analysesData[[listEntry]]$.Completed <- combos
  map
}

#' @importFrom SpaDES.core rasterToMemory
LargePatches <- function(tsf, vtm, poly, labelColumn,
                              id, ageClassCutOffs, ageClasses) {
  timeSinceFireFilesRast <- Cache(rasterToMemory, tsf)

  tsf <- reclassify(timeSinceFireFilesRast,
                    cbind(from = ageClassCutOffs - 0.1,
                          to = c(ageClassCutOffs[-1], Inf),
                          seq_along(ageClasses)))
  levels(tsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

  poly$tmp <- factor(poly[[labelColumn]])
  rasRepPoly <- Cache(
    fasterize2,
    poly,
    emptyRaster = raster(timeSinceFireFilesRast), # doesn't need to the data -- makes Caching more effective
    field = "tmp"
  )
  # rasRepPoly2 <- fasterize::fasterize(sf::st_as_sf(poly),
  #                                    raster = timeSinceFireFilesRast, field = "tmp")
  # levels(rasRepPoly2) <-
  #   data.frame(ID = seq_len(nlevels(poly$tmp)),
  #              Factor = levels(poly$tmp))

  # 3rd raster
  rasVeg <- Cache(rasterToMemory, vtm)#,

  splitVal <- paste0("_", 75757575, "_") # unlikely to occur for any other reason

  # Individual species
  nas3 <- is.na(rasRepPoly[])
  nas2 <- is.na(rasVeg[]) | rasVeg[] == 0
  nas1 <- is.na(tsf[])
  nas <- nas3 | nas2 | nas1

  if (!isTRUE(all(nas))) {
    name1 <- as.character(raster::levels(tsf)[[1]]$Factor)[tsf[][!nas]]
    name2 <- as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]

    # rasRepPoly will have the numeric values of the *factor* in poly$tmp, NOT
    #   the raster::levels(rasRepPoly)[[1]])
    name3 <- as.character(poly$tmp)[rasRepPoly[][!nas]]

    if (!identical(length(name1), length(name2)) || !identical(length(name1), length(name3)))
      stop("There is something wrong with tsf or rasVeg or rasRepPoly inside LargePatches")

    ff <- paste(name1, name2, name3, sep = splitVal) # 4 seconds
    ras <- raster(rasVeg)
    ffFactor <- factor(ff)
    ras[!nas] <- ffFactor # 2 seconds

    areaAndPolyOut <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge
    eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
    types <- strsplit(as.character(eTable$VALUE), split = splitVal)
    types <- do.call(rbind, types)

    facPolygonID <- factor(types[areaAndPolyOut$polyID,3])

    outBySpecies <- data.table(polygonID = as.numeric(facPolygonID),
                               sizeInHa = areaAndPolyOut$sizeInHa,
                               vegCover = types[areaAndPolyOut$polyID, 2],
                               rep = id,
                               ageClass = types[areaAndPolyOut$polyID, 1],
                               polygonName = as.character(facPolygonID))

    # All species combined # remove name2
    ff <- paste(name1, name3, sep = splitVal)
    ff[grepl("NA", ff)] <- NA
    ras <- raster(rasVeg)
    ffFactor <- factor(ff)
    ras[!nas] <- ffFactor

    rm(areaAndPolyOut)
    areaAndPolyOut2 <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge

    eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
    types <- strsplit(as.character(eTable$VALUE), split = splitVal)
    types <- do.call(rbind, types)

    facPolygonID <- factor(types[areaAndPolyOut2$polyID, 2])

    outAllSpecies <- data.table(polygonID = as.numeric(facPolygonID),
                                sizeInHa = areaAndPolyOut2$sizeInHa,
                                vegCover = "All species",
                                rep = id,
                                ageClass = types[areaAndPolyOut2$polyID, 1],
                                polygonName = as.character(facPolygonID))

    out <- rbindlist(list(outBySpecies, outAllSpecies))
    out <- out[sizeInHa >= 100] # never will need patches smaller than 100 ha
  } else {
    out <- data.table(polygonID = character(), sizeInHa = numeric(), vegCover = character(),
                      rep = numeric(), ageClass = numeric(), polygonName = numeric())
  }
  out
}


areaAndPolyValue <- function(ras) {
  polyIndivSpecies <- gdal_polygonizeR(ras) # 99 seconds with full ras
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}

#' Polygonize with gdal
#'
#' Copied from https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
#'
#' @importFrom reproducible assessDataType
#' @importFrom tools file_path_sans_ext
#' @importFrom sf st_bbox read_sf
gdal_polygonizeR <- function(x, outshape = NULL, gdalformat = "ESRI Shapefile",
                             pypath = NULL, readpoly = TRUE, quiet = TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which("gdal_polygonize.py")
    if (!nzchar(pypath)) {
      browser()
      gdalUtils::gdal_setInstallation()
      o <- options()
      pypath <- file.path(o$gdalUtils_gdalPath[[2]]$path, "gdal_polygonize.py")
      if (!nzchar(pypath)) {
        stop("Need gdal_polygonize.py")
      }
    }
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (is.null(outshape)) {
    outshape <- tempfile(fileext = ".shp")
  } else {
    outshape <- sub("\\.shp$", "", outshape)
    f.exists <- file.exists(paste(outshape, c("shp", "shx", "dbf"), sep = "."))
    if (any(f.exists))
      stop(sprintf("File already exists: %s",
                   toString(paste(outshape, c("shp", "shx", "dbf"), sep = ".")[f.exists])),
           call. = FALSE)
  }
  if (is(x, "Raster")) {
    require(raster)
    f <- tempfile(fileext = ".tif")
    rastpath <- normalizePath(f, mustWork = FALSE)
    writeRaster(x, rastpath, datatype = assessDataType(x))

  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop("x must be a file path (character string), or a Raster object.")
  system2("python", args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s"',
                                    pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- sf::read_sf(dsn = dirname(outshape), layer = basename(file_path_sans_ext(outshape)))
    sf::st_bbox(shp, extent(x))
    return(shp)
  } else {
    return(NULL)
  }
}

flattenNames <- function(l, currentLevel = 1) {
  theCol <- paste0("names", currentLevel)
  if (is.list(l)) {
    dt2 <- lapply(l, function(fn) {
      if (is.list(fn)) {
        flattenNames(fn, currentLevel = currentLevel + 1)
      } else {
        dt <- data.table(a = 1)
        dt <- dt[, empty := NA]
        set(dt, NULL, "a", NULL)
      }
    })
    dt <- rbindlist(dt2, idcol = theCol, fill = TRUE)
    if (isTRUE(any(grepl("empty", colnames(dt)))))
      set(dt, NULL, "empty", NULL)
  } else {
    dt <- NA
  }
  return(dt)
}
