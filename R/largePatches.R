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
  combos <- map@analysesData[[listEntry]]$.LargePatchesDone
  if (is.null(combos))
    combos <- character()

  out <- Map(poly = polys, polyName = names(polys), function(poly, polyName) {
    out2 <- lapply(ags, function(ag) {
      comboNew <- paste(ag, polyName, sep = "_")
      if (!comboNew %in% combos) {
        tsf <- m[tsf==TRUE & analysisGroup==ag, filename2]
        vtm <- m[vtm==TRUE & analysisGroup==ag, filename2]
        message("  Calculating Large Patches for ", comboNew)
        out3 <- Cache(.largePatchesCalc, tsfFile = asPath(tsf),
                      vtmFile = asPath(vtm),
                      byPoly = poly, ...)
        combos <<- c(combos, comboNew)
        list(dt = out3)
      }
    })
  })
  map@analysesData[[listEntry]] <- out
  map@analysesData[[listEntry]]$.LargePatchesDone <- combos
  map
}

#' @importFrom SpaDES.core rasterToMemory
.largePatchesCalc <- function(tsfFile, vtmFile, byPoly, labelColumn,
                              id, ageClassCutOffs, ageClasses) {
  timeSinceFireFilesRast <- Cache(rasterToMemory, tsfFile)

  tsf <- reclassify(timeSinceFireFilesRast,
                    cbind(from = ageClassCutOffs - 0.1,
                          to = c(ageClassCutOffs[-1], Inf),
                          seq_along(ageClasses)))
  levels(tsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

  byPoly$tmp <- factor(byPoly[[labelColumn]])
  rasRepPoly <- Cache(
    fasterize2,
    byPoly,
    emptyRaster = raster(timeSinceFireFilesRast), # doesn't need to the data -- makes Caching more effective
    field = "tmp"
  )
  # rasRepPoly2 <- fasterize::fasterize(sf::st_as_sf(byPoly),
  #                                    raster = timeSinceFireFilesRast, field = "tmp")
  # levels(rasRepPoly2) <-
  #   data.frame(ID = seq_len(nlevels(byPoly$tmp)),
  #              Factor = levels(byPoly$tmp))

  # 3rd raster
  rasVeg <- Cache(rasterToMemory, vtmFile)#,

  splitVal <- paste0("_", 75757575, "_") # unlikely to occur for any other reason

  # Individual species
  nas3 <- is.na(rasRepPoly[])
  nas2 <- is.na(rasVeg[]) | rasVeg[] == 0
  nas1 <- is.na(tsf[])
  nas <- nas3 | nas2 | nas1

  if (!isTRUE(all(nas))) {
    name1 <- as.character(raster::levels(tsf)[[1]]$Factor)[tsf[][!nas]]
    name2 <- as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]

    # rasRepPoly will have the numeric values of the *factor* in byPoly$tmp, NOT
    #   the raster::levels(rasRepPoly)[[1]])
    name3 <- as.character(byPoly$tmp)[rasRepPoly[][!nas]]

    if (!identical(length(name1), length(name2)) || !identical(length(name1), length(name3)))
      stop("There is something wrong with tsf or rasVeg or rasRepPoly inside .largePatchesCalc")

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
