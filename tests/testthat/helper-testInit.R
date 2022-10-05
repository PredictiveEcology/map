testInit <- function(libraries, ask = FALSE, verbose = FALSE, tmpFileExt = "",
                     opts = NULL, needGoogle = FALSE) {
  optsAsk <- if (!ask)
    options("reproducible.ask" = ask)
  else
    list()
  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  os <- strsplit(utils::osVersion, " ")[[1]][1]
  osVersion <- numeric_version(strsplit(utils::osVersion, " ")[[1]][2])
  if (isTRUE(os == "Ubuntu") && isTRUE(osVersion >= "20.04")) {
    tiler::tiler_options(python = Sys.which("python3"))
    message("Additonally setting tiler option `python = python3`.")
  }

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- reproducible::normPath(file.path(tempdir(), reproducible:::rndstr(1, 6)))

  ## TODO: reproducible#119 changes use of .httr-oauth (i.e., no longer used)
  ## instead, uses ~/.R/gargle/gargle-oauth
  if (interactive() && isTRUE(needGoogle)) {
    if (file.exists("~/.httr-oauth")) {
      reproducible::linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
    } else {
      googledrive::drive_auth()
      file.copy(".httr-oauth", "~/.httr-oauth")
    }
  }
  reproducible::checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- reproducible::normPath(file.path(tmpdir, "testCache"))
  reproducible::checkPath(tmpCache, create = TRUE)

  if (!is.null(opts)) {
    opts <- options(opts)
  }
  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(reproducible:::rndstr(1, 7), ".", x)))
    tmpfile <- file.path(tmpdir, ranfiles)
    tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
    file.create(tmpfile)
    tmpfile <- reproducible::normPath(tmpfile)
  }

  try(reproducible::clearCache(tmpdir, ask = FALSE), silent = TRUE)
  try(reproducible::clearCache(tmpCache, ask = FALSE), silent = TRUE)

  outList <- list(tmpdir = tmpdir, origDir = origDir, libs = libraries,
                  tmpCache = tmpCache, optsAsk = optsAsk,
                  optsVerbose = optsVerbose, tmpfile = tmpfile,
                  opts = opts)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  lapply(testInitOut$libs, function(lib) {
    detach(paste0("package:", lib), character.only = TRUE)
  })
}

urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif" # nolint
urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing" # nolint
urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing" # nolint
