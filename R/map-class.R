#' The `map` class
#'
#' Contains a common system for organizing geospatial vector and raster data,
#' principally for use with \pkg{leaflet} and \pkg{shiny}.
#'
#' @slot metadata  `data.table` with columns describing metadata of objects in the `map`.
#'
#' @slot .xData Named environment of geospatial data objects (e.g., `sf`, `Raster*`, `Spatial*`).
#'              Each entry may also be simply an environment, which indicates
#'              where to find the object, i.e., via `get(layerName, envir = environment)`.
#'
#' @slot CRS  The common CRS of all layers.
#'
#' @slot paths A named list of file paths. The default is a list of length 2 specifying
#'             `dataPath` and `tilePath`.
#'
#' @slot analyses    A `data.table` or `data.frame` of the types of analyses to perform.
#'
#' @slot analysesData A `data.table` or `data.frame` of the results of the analyses.
#'
#' @exportClass map
#' @rdname map-class
setClass(
  "map",
  contains = "environment",
  slots = list(
    metadata = "data.table",
    # .xData = "environment",
    CRS = "crs",
    paths = "list",
    analyses = "data.table",
    analysesData = "list"
  )
)

setMethod(
  "initialize", "map",
  function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@metadata <- data.table(
      layerName = character(0),
      layerType = character(0),
      columnNameForLabels = character(0),
      leaflet = asPath(character(0)),
      studyArea = numeric(0),
      rasterToMatch = logical(0)
    )
    .Object@CRS <- sf::st_crs()
    .Object@paths <- list(
      dataPath = getOption("map.dataPath", "data"),
      tilePath = getOption("map.tilePath", "tiles")
      ## TODO : scratch/raster/terra path?
    )
    .Object@analyses <- data.table::data.table(
      functionName = character(0)
    )
    .Object@analysesData <- list()

    return(.Object)
  }
)
