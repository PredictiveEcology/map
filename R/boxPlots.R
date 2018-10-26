#' @export
.doPlotBoxplot <- function(data, CCpnts = NULL, authStatus, fname = NULL, ...) {
  if (!is.null(fname)) png(fname, height = 600, width = 800, units = "px")
  boxplot(proportion~as.factor(ageClass), data, ...)

  if (isTRUE(authStatus)) {
    if (length(CCpnts) == 4) {
      points(CCpnts, factor(ageClasses), col = "red", pch = 20, cex = 3)
    } else {
      message(CCpnts)
    }
  }
  if (!is.null(fname)) dev.off()
}

#' @export
#' @importFrom data.table setnames
#' @importFrom tools toTitleCase
runBoxPlotsVegCover <- function(map, functionName, analysisGroups, dPath) {

  ageClasses <- c("Young", "Immature", "Mature", "Old")
  allRepPolys <- na.omit(map@metadata[[analysisGroups]])
  names(allRepPolys) <- allRepPolys

  lapply(allRepPolys, function(poly) {
    allData <- map@analysesData[[functionName]][["LeadingVegTypeByAgeClass"]][[poly]]
    allData$vegCover <- gsub(" leading", "", allData$vegCover) %>%
      tools::toTitleCase() %>%
      as.factor() ## match CC raster names
    allData$ageClass <- factor(allData$ageClass, ageClasses)

    data <- allData[!grepl("CC", group)]

    dataCC <- allData[grepl("CC", group)]
    setnames(dataCC, "proportion", "proportionCC") ## rename the column to proportionCC
    dataCC <- dataCC[, c("group", "polygonID", "label", "NPixels") := list(NULL, NULL, NULL, NULL)]

    data2 <- dataCC[data, on = .(zone, vegCover, ageClass)]
    try(write.csv(data2, file.path(Paths$outputPath, paste0("leading_", poly, ".csv"))))

    saveDir <- checkPath(file.path(dPath, poly), create = TRUE)
    savePng <- quote(file.path(saveDir, paste0(unique(paste(zone, vegCover, collapse = " ")), ".png")))
    slices <- c("zone", "vegCover")
    out <- data2[, tryCatch(.doPlotBoxplot(data = .SD,
                                      authStatus = TRUE,
                                      CCpnts = unique(proportionCC),
                                      col = "limegreen",
                                      fname = eval(savePng),
                                      horizontal = TRUE,
                                      main = unique(paste(zone, vegCover, collapse = "_")),
                                      xlab = paste0("Proportion of of forest area (total ",
                                                    sum(NPixels, na.rm = TRUE) *
                                                      res(rasterToMatch(ml))[1]^2/1e4,
                                                    " ha)"),
                                      ylab = "Age class",
                                      ylim = c(0, 1)),
      error = function(e) warning(e)),
      .SDcols = c("ageClass", "proportion", "proportionCC", "NPixels"), by = slices]
    data[, list(filename = eval(savePng)), by = slices]
  })
}
