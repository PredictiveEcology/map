
#' Utility functions for grouping analyses in a \code{map} object
#'
#' @inheritParams mapAdd
#'
#' @param analysisGroups A character (length 1 currently), indicating which
#'   analysis group (e.g., "analysisGroup1") should be used to \code{rbindlist}.
#'   Can also specify \code{"all"} which will \code{rbindlist} all outputs.
#' @param functionName TODO: description needed
#'
#' @return A list of \code{data.table}s.
#'
#' @aliases rbindlist-analysisGroups
#' @aliases rbindlistAG
#' @export
#' @rdname map-utilities
rbindlistAG <- function(map, functionName, analysisGroups) {
  lpNames <- names(map@analysesData[[functionName]])
  if (length(analysisGroups) > 1)
    stop("rbindlistAG is not tested for more than 1 analysisGroups")
  names(analysisGroups) <- analysisGroups
  polys <- lapply(analysisGroups, function(ag) {
    map@metadata$layerName[!is.na(map@metadata[[ag]])]
  })
  polys <- unlist(polys)
  names(polys) <- polys
  out <- lapply(polys, function(poly) {
    postGroup <- lpNames[grep(paste0("\\._\\.", poly, "$"), lpNames)]
    data.table::rbindlist(unlist(map@analysesData[[functionName]][postGroup], recursive = FALSE),
                          idcol = "group")
  })
}
