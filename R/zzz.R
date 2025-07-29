.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.map <- list( # nolint
    map.dataPath = "data",
    map.maxNumCores = min(
      getOption("Ncpus"),
      parallelly::availableCores(constraints = "connections")
    ),
    map.overwrite = FALSE,
    map.tilePath = "tiles",
    map.tileRetry = 3L,
    map.useParallel = !identical("windows", .Platform$OS.type)
  )
  toset <- !(names(opts.map) %in% names(opts))
  if (any(toset)) options(opts.map[toset])
}
