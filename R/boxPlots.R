if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("ageClass", "group", "proportionCC", "totalPixels", "vegCover", "zone"))
}

#' @export
#' @importFrom graphics boxplot points
#' @importFrom grDevices dev.off png
.doPlotBoxplot <- function(data, authStatus, fname = NULL, ageClasses, ...) {
  if (!is.null(fname)) png(fname, height = 600, width = 800, units = "px")
  boxplot(proportion ~ as.factor(ageClass), data, ...)

  if (isTRUE(authStatus)) {
    ids <- match(factor(data$ageClass[1:4]), data[1:4, ]$ageClass)
    points(data$proportionCC[ids], factor(data$ageClass[1:4]), col = "red", pch = 20, cex = 3)
  }
  if (!is.null(fname)) dev.off()
}

#' Generate box and whisker plots for leading vegetation cover
#'
#' TODO: description needed
#'
#' @param map A \code{map} object.
#' @param functionName TODO: description needed
#' @param analysisGroups TODO: description needed
#' @param dPath Destination path for the resulting png files.
# @param ageClasses Character vector of vegetation class names.
#'
#' @export
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom raster res
#' @importFrom reproducible checkPath
#' @importFrom tools toTitleCase
#' @importFrom utils write.csv
runBoxPlotsVegCover <- function(map, functionName, analysisGroups, dPath) {
                                #ageClasses = c("Young", "Immature", "Mature", "Old")
  allRepPolys <- na.omit(map@metadata[[analysisGroups]])
  names(allRepPolys) <- allRepPolys

  lapply(allRepPolys, function(poly) {
    allData <- map@analysesData[[functionName]][["LeadingVegTypeByAgeClass"]][[poly]]
    if (is.null(allData))
      allData <- map@analysesData[[functionName]][[poly]] ## TODO: fix upstream
    allData <- unique(allData) ## remove duplicates; with LandWeb#89
    allData$vegCover <- gsub(" leading", "", allData$vegCover) %>%
      tools::toTitleCase() %>%
      as.factor() ## match CC raster names
    allData$ageClass <- factor(allData$ageClass, ageClasses)

    data <- allData[!grepl("CC", group)]

    dataCC <- allData[grepl("CC", group)]
    setnames(dataCC, "proportion", "proportionCC") ## rename the column to proportionCC
    dataCC <- dataCC[, c("group", "label", "NPixels") := list(NULL, NULL, NULL)]

    data2 <- dataCC[data, on = .(zone, vegCover, ageClass)]
    data2[is.na(NPixels), NPixels := 0]

    ## sum = all species + each indiv species = 2 * totalPixels
    ## NOTE: this is number of TREED pixels, which is likely smaller than the polygon area
    data2[, totalPixels := as.double(base::sum(NPixels, na.rm = TRUE)),
          by = c("group", "vegCover", "zone")]
    data2[, totalPixels2 := as.double(base::mean(totalPixels, na.rm = TRUE)),
          by = c("vegCover", "zone")] ## use mean for plot labels below

    try(write.csv(data2, file.path(dPath, paste0("leading_", gsub(" ", "_", poly), ".csv"))))
    saveDir <- checkPath(file.path(dPath, poly), create = TRUE)
    savePng <- quote(file.path(saveDir, paste0(unique(paste(zone, vegCover, collapse = " ")), ".png")))
    slices <- c("zone", "vegCover")
    data2[, tryCatch(.doPlotBoxplot(data = .SD,
                                    authStatus = TRUE,
                                    col = "limegreen",
                                    fname = eval(savePng),
                                    ageClasses = ageClasses,
                                    horizontal = TRUE,
                                    main = unique(paste(zone, vegCover, collapse = "_")),
                                    xlab = paste0("Proportion of forest area (total ",
                                                  format(unique(totalPixels2) *
                                                    prod(res(rasterToMatch(map))) / 1e4,
                                                    big.mark = ","),
                                                  " ha)"),
                                    ylab = "Age class",
                                    ylim = c(0, 1)),
      error = function(e) warning(e)),
      .SDcols = c("ageClass", "proportion", "proportionCC", "NPixels"), by = slices]
    data2[, list(filename = eval(savePng)), by = slices]
  })
}

.doPlotHistogram <- function(data, fname = NULL, ...) {
  minNumBars <- 6
  maxNumBars <- 30
  rangeNClusters <- range(c(0, data$NCC, data$N, minNumBars)) ## TODO: verify
  attemptedNumBars <- max(minNumBars, min(maxNumBars, diff(rangeNClusters)))
  breaksRaw <- seq(rangeNClusters[1], rangeNClusters[2], length.out = attemptedNumBars)
  prettyBreaks <- pretty(breaksRaw, n = attemptedNumBars, min.n = min(attemptedNumBars, minNumBars))
  dataForBreaks <- hist(data$N, plot = FALSE, breaks = prettyBreaks)
  breaksLabels <- dataForBreaks$breaks
  breaksInterval <- diff(breaksLabels)[1]
  dataForHistogram <- if (NROW(data) == 0) {
    # add a bar at zero if there are no patches
    hist(0, plot = FALSE, breaks = prettyBreaks)
  } else {
    hist(data$N, plot = FALSE, breaks = prettyBreaks)
  }
  histogramData <- dataForHistogram$counts / sum(dataForHistogram$counts)
  histogramData[is.na(histogramData)] <- 0 # NA means that there were no large patches in dt

  breaks <- breaksLabels - breaksInterval / 2
  barplotBreaks <- seq_along(breaksLabels) - 0.5 # (breaksLabels + breaksInterval / 2)
  ticksAt <- barplotBreaks - min(breaksLabels)
  browser()
  xlim <- range(ticksAt) - 0.5 # breaksInterval / 2
  addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks - min(breaksLabels))
  verticalLineAtX <- unique(data$NCC)[1] + 0.5# breaksInterval / 2 # The barplot xaxis is 1/2 a barwidth off

  if (!is.null(fname)) png(fname, width = 400, height = 400, units = "px")
  barplot(histogramData, ...)
  if (!is.null(addAxisParams)) do.call(axis, addAxisParams)
  if (!is.null(verticalLineAtX)) abline(v = verticalLineAtX, col = "red", lwd = 3)
  if (!is.null(fname)) dev.off()
}

#' Generate histograms for large patches
#'
#' TODO: description needed
#'
#' @param map A \code{map} object.
#' @param functionName TODO: description needed
#' @param analysisGroups TODO: description needed
#' @param dPath Destination path for the resulting png files.
# @param ageClasses Character vector of vegetation class names.
#'
#' @export
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom raster res
#' @importFrom reproducible checkPath
#' @importFrom tools toTitleCase
#' @importFrom utils write.csv
runHistsLargePatches <- function(map, functionName, analysisGroups, dPath) {
                                 #ageClasses = c("Young", "Immature", "Mature", "Old")
  allRepPolys <- na.omit(map@metadata[[analysisGroups]])
  names(allRepPolys) <- allRepPolys

  lapply(allRepPolys, function(poly) {
    allData <- map@analysesData[[functionName]][["LargePatches"]][[poly]]
    if (is.null(allData))
      allData <- map@analysesData[[functionName]][[poly]] ## TODO: fix upstream
    #allData <- unique(allData) ## remove duplicates; with LandWeb#89
    #allData$vegCover <- gsub(" leading", "", allData$vegCover) %>%
    #  tools::toTitleCase() %>%
    #  as.factor() ## match CC raster names
    #allData$ageClass <- factor(allData$ageClass, ageClasses)

    data <- allData[!grepl("CC", group)]
    dataCC <- allData[grepl("CC", group)]

    slices <- c("ageClass", "polygonName", "vegCover", "rep")
    slicesNoRep <- slices[slices != "rep"]
    #out <- data[, .N, by = c(slices, "rep")]

    data <- data[!ageClass == "NA" | !vegCover == "NA"]
    emptyDT <- data.table(expand.grid(ageClass = unique(data$ageClass),
                          vegCover = unique(data$vegCover),
                          polygonName = unique(data$polygonName),
                          rep = unique(data$rep))) ## move to internal

    patchSizes <- c(100, 500, 1000, 5000) ## minPatchSize <- 100

    lapply(patchSizes, function(minPatchSize) {
      nClustersDT <- data[sizeInHa >= minPatchSize, .N, by = slices]
      nClustersDT <- nClustersDT[emptyDT, on = slices, nomatch = NA]
      nClustersDT[is.na(N), N := 0]

      # nClusters <- nClustersDT$N ## move to internal

      nClustersDT_CC <- dataCC[sizeInHa >= minPatchSize, .N, by = slices]
      setnames(nClustersDT_CC, "N", "NCC")
      nClustersDT_CC <- nClustersDT_CC[emptyDT, on = slices, nomatch = NA]
      nClustersDT_CC[is.na(NCC), NCC := 0]

      # verticalLineAtX <- max(0, nClustersDT_CC$N) ## move to internal

      nClustersDT <- nClustersDT[nClustersDT_CC, on = slices]

      try(write.csv(nClustersDT, file.path(dPath, paste0("largePatches_",
                                                         gsub(" ", "_", poly),
                                                         "_", minPatchSize, ".csv"))))

      saveDir <- checkPath(file.path(dPath, poly, "largePatches", minPatchSize), create = TRUE)
      savePng <- quote(file.path(saveDir, paste0(unique(paste(polygonName, ageClass, vegCover, collapse = " ")), ".png")))

      browser()
      xlim <- c(0,max(nClustersDT$N))
      nClustersDT[, tryCatch(.doPlotHistogram(data = .SD,
                                              fname = eval(savePng),
                                              # ccLine = NCC,
                                              border = "grey",
                                              col = "darkgrey",
                                              main = paste(polygonName, ageClass, vegCover, collapse = " "),
                                              space = 0,
                                              xlab = paste0("Number of patches greater than ",
                                                            minPatchSize, " ha"),
                                              ylab = "Proportion in NRV",
                                              xlim = xlim,
                                              ylim = c(0, 1)),
                             error = function(e) warning(e)),
                  .SDcols = c(slices, "N", "NCC"), by = slicesNoRep]

    })



    ###

    data2[, list(filename = eval(savePng)), by = slices]
  })
}
