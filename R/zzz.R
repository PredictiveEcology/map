.NCONNECTIONS <- utils::getFromNamespace(".NCONNECTIONS", "pemisc")

#' Set python path for `tiler`
#'
#' By default, \pkg{tiler} is configured to use python 2, which may not be available on
#' recent Linux distributions (e.g., Ubuntu 20.04+).
#' Thus, explicitly set tiler options to find the correct python path on their system.
#'
#' @keywords internal
.setTilerPythonPath <- function() {
  os <- strsplit(utils::osVersion, " ")[[1]][1]
  osVersion <- numeric_version(strsplit(utils::osVersion, " ")[[1]][2])
  if (isTRUE(os == "Ubuntu") && isTRUE(osVersion >= "20.04")) {
    tiler::tiler_options(python = Sys.which("python3"))
    message("Setting tiler option `python = python3`.")
  } else if (.isWindows()) {
    # out <- reproducible:::findGDAL()
    # if (isFALSE(out))
    #   stop("Need to have gdal installed; see ?tiler")
    pydir <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path)
    possPyBin <- file.path(pydir, "python.exe")
    possPyBin3 <- file.path(pydir, "python3.exe")

    pythonPth <- if (file.exists(possPyBin)) {
      possPyBin
    } else if (file.exists(possPyBin3)) {
      possPyBin3
    } else {
      stop("Can't find python.exe or python3.exe")
    }
    tiler::tiler_options(python = pythonPth)

    out <- findOSGeo4W() # set path in tiler::tiler_options
    if (length(out) == 0)
      stop("Need to have OSGeo4W installed; see ?tiler")
  }
}

#' @importFrom parallel detectCores
.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.map <- list( # nolint
    map.overwrite = FALSE,
    map.tilePath = file.path("tiles"),
    map.dataPath = file.path("data"),
    map.maxNumCores = min(getOption("Ncpus"), parallel::detectCores(), .NCONNECTIONS),
    map.useParallel = !identical("windows", .Platform$OS.type)
  )
  toset <- !(names(opts.map) %in% names(opts))
  if (any(toset)) options(opts.map[toset])

  .setTilerPythonPath()

  ## import functions using backports:
  backports::import(pkgname, "isFALSE")
}
