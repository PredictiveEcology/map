#' Calculate proportion of landscape occupied by each vegetation class
#'
#' This function is recursive.
#' If \code{poly} is a \code{SpatialPolygon}, then the function
#' will enter once, and convert this to a fasterized version, and pass that into
#' the function replacing \code{poly}.
#' It is also recursive of passed a vector of filenames for \code{tsf} and \code{vtm}.
#'
#' @param tsf A single filename, relative or absolute, pointing to a Time Since Fire raster.
#'            Can be any format that \code{raster} can use.
#' @param vtm A single filename, relative or absolute, pointing to a Vegetation Type Map raster.
#'            Can be any format that \code{raster} can use.
#' @param poly A single \code{SpatialPolygonsDataFrame} object or a factor \code{RasterLayer}.
#'             This layer MUST have a column labelled \code{shinyLabel}
#' @param ageClasses A character vector with labels for age classes to bin the \code{tsf} times,
#'                   e.g., \code{c("Young", "Immature", "Mature", "Old")}
#' @param ageClassCutOffs A numeric vector with the endpoints for the \code{ageClasses}.
#'                        Should be \code{length(ageClasses) + 1}
#'
#' @return A \code{data.table} with proportion of the pixels in each vegetation class,
#'         for each given age class within each polygon.
#'
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @importFrom data.table setDT
#' @export
LeadingVegTypeByAgeClass <- function(tsf, vtm, poly, ageClassCutOffs,  ageClasses) {
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

  # prepare poly factor raster
  if (is(poly, "SpatialPolygons")) {
    if (!"shinyLabel" %in% colnames(poly@data))
      stop("poly must have a column 'shinyLabel'")
    poly <- Cache(fasterize2, rasTsf, poly,
                 field = "polygonNum")
  }
  levs <- raster::levels(poly)[[1]]

  # this is same, if all values present: e.g., 1, 2, 3, 4, 5 ...,
  # but not if missing: e.g., 1, 2, 3, 5
  levs <- factorValues(poly, levs$ID)
  facVals <- factorValues(
    poly,
    poly[],
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
  set(tabulated2, NULL, "vegCover", "All species")

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
      NULL,
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
#' @param field passed to \code{fasterize}
#'
#' @importFrom fasterize fasterize
#' @importFrom raster extent
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
