#' Determine the number of nodes to use in a new cluster
#'
#' TODO: DESCRIPTION NEEDED
#'
#' @param memRequiredMB The amount of memory needed in MB
#' @param maxNumClusters The maximum number of nodes to use
#'
optimalClusterNum <- function(memRequiredMB = 500, maxNumClusters = 1) {
  if (Sys.info()["sysname"] == "Linux") {
    detectedNumCores <- parallel::detectCores()
    shouldUseCluster <- (maxNumClusters > 0)

    if (shouldUseCluster) {
      # try to scale to available RAM
      try(aa <- system("free -lm", intern = TRUE))
      if (!is(aa, "try-error")) {
        bb <- strsplit(aa[2], split = " ") # 'Mem:' row
        availMem <- as.numeric(bb[[1]][nzchar(bb[[1]])][7]) # 'available' column
        numClusters <- floor(min(detectedNumCores, availMem / memRequiredMB))
      } else {
        message("The OS function, 'free' is not available. Returning 1 cluster")
        numClusters <- 1
      }
      numClusters <- min(maxNumClusters, numClusters, detectedNumCores)
    } else {
      numClusters <- 1
    }
  } else {
    message("This function returns 1 cluster on Windows and MacOS.")
    numClusters <- 1
  }
  return(numClusters)
}

#' Create a parallel Fork cluster, if useful
#'
#' Given the size of a problem, it may not be useful to create a cluster.
#' This will make a Fork cluster (so Linux only)
#' @param useParallel Logical or numeric. If \code{FALSE}, returns NULL. If
#'        \code{numeric}, then will return a cluster object with this
#'        many cores, up to \code{maxNumClusters}
#' @param MBper Numeric. Passed to \code{memRequiredMB} in
#'              \code{\link{optimalClusterNum}}
#' @param maxNumClusters Numeric or Integer. The theoretical upper limit
#'        for number of clusters to create (e.g., because there are only
#'        3 problems to solve, not \code{parallel::detectCores})
#' @param ... Passed to \code{makeForkClusterRandom}.
#'            Only relevant for \code{iseed}.
#' @export
makeOptimalCluster <- function(useParallel = getOption("map.useParallel", FALSE),
                               MBper = 5e2,
                               maxNumClusters = parallel::detectCores(), ...) {
  cl <- NULL
  if (is.null(maxNumClusters)) maxNumClusters = parallel::detectCores()

  if (!identical("windows", .Platform$OS.type)) {
    numClus <- if (isTRUE(useParallel)) {
      numClus <- optimalClusterNum(MBper, maxNumClusters = maxNumClusters)
      if (numClus <= 1) {
        numClus <- NULL
      }
      numClus
    } else if (is.numeric(useParallel)) {
      min(useParallel, maxNumClusters)
    }

    if (!is.null(numClus)) {
      cl <- makeForkClusterRandom(numClus, ...)
    }
  }
  return(cl)
}

#' \code{makeForkCluster} with random seed set
#'
#' This will set different random seeds on the clusters (not the default)
#' with \code{makeForkCluster}.
#' It also defaults to creating a logfile with message of where it is.
#'
#' @param ... passed to \code{makeForkCluster}, e.g.,
#' @param iseed passed to \code{clusterSetRNGStream}
#'
#' @importFrom reproducible checkPath
makeForkClusterRandom <- function(..., iseed = NULL) {
  require(parallel)
  dots <- list(...)
  if (!("outfile" %in% names(dots))) {
    dots$outfile <- file.path("outputs", ".log.txt")
  }
  checkPath(dirname(dots$outfile), create = TRUE)
  for (i in 1:4)
    cat(file = dots$outfile, "------------------------------------------------------------")
  cl <- do.call(makeForkCluster, args = dots)
  message("  Starting a cluster with ", length(cl)," threads")
  message("    Log file is ", dots$outfile, ". To prevent log file, pass outfile = ''")
  clusterSetRNGStream(cl, iseed = iseed)
  cl
}

#' \code{Map} and \code{parallel::clusterMap} together
#'
#' This will send to Map or clusterMap, depending on whether cl is provided.
#'
#' @importFrom reproducible .formalsNotInCurrentDots
#' @importFrom parallel clusterMap
#' @param ... passed to \code{Map} or \code{clusterMap}
#' @param cl A cluster object, passed to \code{clusterMap}
Map2 <- function(..., cl = NULL) {
  formsMap <- reproducible::.formalsNotInCurrentDots(mapply, ...)
  formsClusterMap <- reproducible::.formalsNotInCurrentDots(clusterMap, ...)
  if (is.null(cl)) {
    argList <- list(...)
    wrongFun1 <- "fun" %in% names(argList)
    if (wrongFun1) {
      fun <- argList$fun
    }
    wrongFun2 <- "FUN" %in% names(argList)
    if (wrongFun2) {
      fun <- argList$FUN
    }
    argList[setdiff(formsMap, formsClusterMap)] <- NULL
    if (wrongFun1 || wrongFun2) {
      argList$f <- fun
    }
    do.call(Map, args = argList)
  } else {
    argList <- list(...)
    wrongFun1 <- "f" %in% names(argList)
    if (wrongFun1) {
      fun <- argList$f
    }
    wrongFun2 <- "FUN" %in% names(argList)
    if (wrongFun2) {
      fun <- argList$FUN
    }
    argList[setdiff(formsClusterMap, formsMap)] <- NULL
    if (wrongFun1 || wrongFun2) {
      argList$fun <- fun
    }
    do.call(clusterMap, append(list(cl = cl), argList))
  }
}
