#' The `map` class
#'
#' Contains a common system for organzing vector and raster layers,
#' principally for use with \pkg{leaflet} and \pkg{shiny}.
#'
#' @slot metadata  `data.table` with columns describing metadata of map objects in
#'                 `maps` slot.
#'
#' @slot .xData Named environment of map-type objects (e.g., `sf`, `Raster*`,
#'              `Spatial*`. Each entry may also be simply an environment, which indicates
#'              where to find the object, i.e., via `get(layerName, envir = environment)`.
#'
#' @slot CRS  The common crs of all layers
#'
#' @slot paths File paths. A named list of paths. The default is a list of length 2,
#'             `dataPath` and `tilePath`
#'
#' @slot analyses    A `data.table` or `data.frame` of the types of analyses to perform.
#'
#' @slot analysesData A `data.table` or `data.frame` of the results of the analyses.
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
    paths = "list",
    analyses = "data.table",
    analysesData = "list"
  )#,
  # validity = function(object) {
  #   ## TODO: add additional checks!
  #   identical(vapply(object@metadata, class, character(1)),
  #             c(layerName = "character", layerType = "character",
  #               columnNameForLabels = "character", leaflet = "character",
  #               studyArea = "numeric", rasterToMatch = "logical"))
  # }
)

setMethod("initialize", "map",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object@metadata <- data.table(layerName = character(), layerType = character(),
                                           columnNameForLabels = character(),
                                           leaflet = asPath(character()),
                                           studyArea = numeric(), rasterToMatch = logical())
            .Object@CRS <- sp::CRS()
            .Object@analyses <- data.table::data.table(functionName = character())
            .Object@analysesData <- list()
            .Object@paths <- list(dataPath = getOption("map.dataPath", "."),
                                  tilePath = getOption("map.tilePath", "."))

            .Object
})
