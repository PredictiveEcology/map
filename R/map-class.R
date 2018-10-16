#' The \code{map} class
#'
#' Contains a common system for organzing vector and raster layers,
#' principally for use with \pkg{leaflet} and \pkg{shiny}.
#'
#' @slot metadata  \code{data.table} with columns describing metadata of map objects in
#'                 \code{maps} slot.
#'
#' @slot .xData Named environment of map-type objects (e.g., \code{sf}, \code{Raster*},
#'            \code{Spatial*}. Each entry may also be simply an environment, which
#'            indicates where to find the object, i.e., via \code{get(layerName, envir = environment)}
#'
#' @slot CRS  The common crs of all layers
#'
#' @slot analyses    A \code{data.table} or \code{data.frame} of the types of analyses to perform.
#'
#' @slot analysesData A \code{data.table} or \code{data.frame} of the results of the analyses.
#'
#' @aliases map
#' @exportClass map
#' @importFrom data.table data.table
#' @importFrom raster crs raster
#' @rdname map-class
setClass(
  "map",
  contains = "environment",
  slots = list(
    metadata = "data.table",
    #.Data = "environment",
    CRS = "CRS",
    analyses = "data.table",
    analysesData = "list"
  ),
  validity = function(object) {
    browser()
    #if (is.na(object@simtimes$end)) {
    #  stop("simulation end time must be specified.")
    #} else {
    #  if (object@simtimes$start > object@simtimes$end) {
    #    stop("simulation end time cannot be before start time.")
    #  }
    #}
  }
)

setMethod("initialize", "map",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object@metadata = data.table(layerName = character(), layerType = character(),
                                          #url = character(),
                                          columnNameForLabels = character(),
                                          leaflet = logical(), studyArea = numeric(),
                                          rasterToMatch = logical())
            .Object@CRS = sp::CRS()
            .Object@analyses = data.table::data.table(functionName = character())#, quotedFn = character())
            .Object@analysesData = list()

            .Object
          })

