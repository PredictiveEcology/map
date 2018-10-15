##
.onLoad <- function(libname, pkgname) {

  ## import functions using backports:
  backports::import(pkgname, "isFALSE")
}
