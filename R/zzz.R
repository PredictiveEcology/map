##
.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.map <- list( # nolint
    map.overwrite = FALSE,
    map.tilePath = file.path("tiles"),
    map.dataPath = file.path("data"),
    map.useParallel = !identical("windows", .Platform$OS.type)
  )
  toset <- !(names(opts.map) %in% names(opts))
  if (any(toset)) options(opts.map[toset])

  ## import functions using backports:
  backports::import(pkgname, "isFALSE")
}
