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
#' @param ageClasses Character vector of vegetation class names.
#'
#' @export
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom raster res
#' @importFrom reproducible checkPath
#' @importFrom tools toTitleCase
#' @importFrom utils write.csv
runBoxPlotsVegCover <- function(map, functionName, analysisGroups, dPath,
                                ageClasses = c("Young", "Immature", "Mature", "Old")) {
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

#' Generate histograms for large patches
#'
#' TODO: description needed
#'
#' @param map A \code{map} object.
#' @param functionName TODO: description needed
#' @param analysisGroups TODO: description needed
#' @param dPath Destination path for the resulting png files.
#' @param ageClasses Character vector of vegetation class names.
#'
#' @export
#' @importFrom data.table setnames
#' @importFrom magrittr %>%
#' @importFrom raster res
#' @importFrom reproducible checkPath
#' @importFrom tools toTitleCase
#' @importFrom utils write.csv
runHistsLargePatches <- function(map, functionName, analysisGroups, dPath,
                                 ageClasses = c("Young", "Immature", "Mature", "Old")) {
  browser()
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
    #setnames(dataCC, "proportion", "proportionCC") ## rename the column to proportionCC
    #dataCC <- dataCC[, c("group", "label", "NPixels") := list(NULL, NULL, NULL)]

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
                                    CCpnts = proportionCC,
                                    col = "limegreen",
                                    fname = eval(savePng),
                                    ageClasses = ageClasses,
                                    horizontal = TRUE,
                                    main = unique(paste(zone, vegCover, collapse = "_")),
                                    xlab = paste0("Proportion of of forest area (total ",
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
