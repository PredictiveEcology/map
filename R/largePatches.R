mapLargePatches <- function(map, ...) {
  m <- metadata(map)

  if (is.null(m$analysisGroup)) {
    stop("Expecting analysisGroup column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  ags <- sort(na.omit(unique(m$analysisGroup)))
  polys <- maps(map, "SpatialPolygons")
  out <- lapply(polys, function(poly) {
    browser()
    out2 <- lapply(au, function(ag) {
      tsf <- m[tsf==TRUE & analysisGroup==ag, filename2]
      vtm <- m[vtm==TRUE & analysisGroup==ag, filename2]
      out3 <- leadingByStage2(tsf, vtm, poly, ...)
      browse()
      m[analysisGroup==ag, LeadingDone := TRUE]
      out3
    })
  })
  map@analysesData[["Leading by stage"]] <- out
  map
}

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


