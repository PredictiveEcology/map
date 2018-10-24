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

runBoxPlotsVegCover <- function(map) {
  browser()
  map
  data[, .doPlotBoxplot(data = .SD,  authStatus = TRUE, col = "limegreen",
                        horizontal = TRUE,
                        main = unique(paste(zone, vegCover, collapse = " ")), ## TODO: add total area to title (#59)
                        xlab = paste0("Proportion of of forest area (total ", sum(NPixels, na.rm= TRUE) * res(rasterToMatch(ml))[1]^2/1e4 , " ha)"),
                        ylab = "Age class",
                        ylim = c(0, 1)), .SDcols = c("ageClass", "proportion", "NPixels"), by = c("zone", "vegCover")]
}
