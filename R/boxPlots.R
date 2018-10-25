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
runBoxPlotsVegCover <- function(map, dt, analysisGroups, dPath) {
  browser()

  allRepPolys <- na.omit(map@metadata[[analysisGroups]])
  names(allRepPolys) <- allRepPolys

  CCpnts <- "" ## TODO: points from CC

  lapply(allRepPolys, function(poly) {
    data <- map@analysesData[[dt]][["LeadingVegTypeByAgeClass"]][[poly]]
    #par(mfrow = c(3,4))
    saveDir <- checkPath(file.path(dPath, poly), create = TRUE)
    out <- data[, .doPlotBoxplot(data = .SD,
                                 authStatus = TRUE,
                                 CCpnts = CCpnts,
                                 col = "limegreen",
                                 fname = file.path(saveDir, paste0(unique(paste(zone, vegCover, collapse = " "))), ".png"),
                                 horizontal = TRUE,
                                 main = unique(paste(zone, vegCover, collapse = "_")),
                                 xlab = paste0("Proportion of of forest area (total ", sum(NPixels, na.rm = TRUE) * res(rasterToMatch(ml))[1]^2/1e4 , " ha)"),
                                 ylab = "Age class",
                                 ylim = c(0, 1)),
                .SDcols = c("ageClass", "proportion", "NPixels"), by = c("zone", "vegCover")]
    out
  })
}
