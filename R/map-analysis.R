#' Generic analysis for map objects
#'
#' This is the workhorse function that runs any analyses described in
#' \code{map@analyses}. It uses hashing, and will not rerun any analysis that
#' already ran on identical inputs.
#'
#' @inheritParams mapAdd
#' @param functionName A function name that will be run on combinations of
#'   inputs in the map object. See details.
#' @param purgeAnalyses A character string indicating which analysis group
#'   combination or part thereof (e.g., the name entered into the row under
#'   \code{analysisGroup2} column of the \code{map@metadata} or a \code{functionName}.
#'
#' @details
#' This function will do a sequence of things. First, it will run \code{expand.grid}
#' on any columns whose names start with \code{analysisGroup}, creating a factorial
#' set of analyses as described by these columns. Then it will assess if any of
#' these has already been run. For those that have not been run, it will then
#' run the \code{functionName} on arguments that it finds in the \code{metadata}
#' slot of the map object, as well as any arguments passed in here in the \code{...}.
#' In general, the arguments being passed in here should be fixed across all analyses,
#' while any that vary by analysis should be entered into the metadata table at the
#' time of adding the layer to the map, via \code{mapAdd}.
#'
#' @importFrom stats na.omit
mapAnalysis <- function(map, functionName = NULL, purgeAnalyses = NULL, ...) {
  m <- map@metadata
  dots <- list(...)

  if (is.null(functionName)) {
    stop("Each analysis must have a functionName")
  }
  if (!isTRUE(any(grepl("analysisGroup", colnames(m))))) {
    stop("Expecting analysisGroup1 column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  browser()
  AGs <- sort(unique(colnames(m)[startsWith(colnames(m), "analysisGroup")]))
  names(AGs) <- AGs
  ags <- lapply(AGs, function(AG) sort(na.omit(unique(m[[AG]]))))
  combosCompleted <- map@analysesData[[functionName]]$.Completed

  # Purge if purgeAnalyses is non NULL
  if (!is.null(purgeAnalyses)) {
    message("Purging previous analyses with ", purgeAnalyses)
    combosCompleted <- if (isTRUE(purgeAnalyses == functionName))
      character()
    else
      grep(purgeAnalyses, combosCompleted, value = TRUE, invert = TRUE)
  }

  #if (is.null(combosCompleted)) {
  if (any(unlist(lapply(ags, function(x) length(x > 0))))) {
    combosAll <- do.call(expand.grid, args = append(list(stringsAsFactors = FALSE),
                                                    lapply(ags, function(x) x)))
    combosAll$all <- apply(combosAll, 1, paste, collapse = "._.")
  }
  #}
  combosToDo <- combosAll[!combosAll$all %in% combosCompleted,]

  if (NROW(combosToDo)) {
    # Get the expand.grid arguments
    formalsInFunction <- formalArgs(functionName)[formalArgs(functionName) %in% colnames(m)]
    names(formalsInFunction) <- formalsInFunction

    # Get the fixed arguments
    otherFormalsInFunction <- formalArgs(functionName)[formalArgs(functionName) %in% colnames(map@analyses)]
    if (length(otherFormalsInFunction)) {
      names(otherFormalsInFunction) <- otherFormalsInFunction

      # Override dots from this function call
      dots <- lapply(otherFormalsInFunction, function(form) {
        fn <- functionName
        assign(form, map@analyses[functionName == fn, get(form)][[1]])
      })
    }

    out <- by(combosToDo, combosToDo$all, simplify = FALSE,
              function(combo) {

                # Cycle through for each analysisGroup, get each argument
                args <- lapply(AGs, function(AG) {
                  browser()
                  args <- lapply(formalsInFunction, function(arg) {
                    browser()
                    val <- na.omit(m[get(AG) == combo[[AG]], ][[arg]])
                    if (isTRUE(val)) {
                      val <- get(m[get(AG) == combo[[AG]], layerName],
                                 envir = m[get(AG) == combo[[AG]], envir][[1]])
                    }
                    if (length(val) > 0)
                      assign(arg, val)
                    else
                      NULL
                  })
                  args[!sapply(args, is.null)]
                })
                browser()
                args <- unlist(unname(args), recursive = FALSE)
                message("  Calculating ", functionName, " for ", combo$all)
                fnOut <- do.call(Cache, args = c(list(get(functionName)), args, dots))
                combosCompleted <<- c(combosCompleted, combo$all)
                list(dt = fnOut)
              })

    map@analysesData[[functionName]][names(out)] <- unname(lapply(out, function(x) x))
    map@analysesData[[functionName]]$.Completed <- combosCompleted
  } else {
    message("  ", functionName, " already run on all layers")
  }
  map
}

#' @export
#' @importFrom data.table data.table
#' @importFrom fastdigest fastdigest
mapAddAnalysis <- function(map, functionName, ...) {
  dots <- list(...)
  b <- data.table(functionName = functionName, t(dots))
  prevEntry <- map@analyses$functionName == functionName
  purgeAnalyses <- NULL # Set default as NULL
  newDigest <- fastdigest::fastdigest(
    c(.robustDigest(get(b[, functionName])),
      .robustDigest(b[, !"functionName"]))
  )
  set(b, NULL, "argHash", newDigest)
  doRbindlist <- TRUE
  if (sum(prevEntry)) {
    if (!isTRUE(newDigest %in% map@analyses[prevEntry, ]$argHash)) {
      message("An analysis called ", functionName, " already added to map object; ",
              " Overwriting it")
      purgeAnalyses <- functionName
      map@analyses <- map@analyses[!prevEntry]
    } else {
      doRbindlist <- FALSE
      message("An analysis called, ", functionName, " with identical function and ",
              "arguments already added and run. Skipping reruns.")
    }
  }

  if (doRbindlist)
    map@analyses <- rbindlist(list(map@analyses, b), fill = TRUE, use.names = TRUE)

  map <- runMapAnalyses(map, purgeAnalyses)

  map

}


#' Add a post hoc analysis function to a map object
#'
#' @inheritParams mapAdd
#'
#' @param functionName A function that is designed for post hoc analysis of
#'   map class objects, e.g., \code{rbindlistAG}
#'
#' @param postHocAnalysisGroups Character string with one
#'   \code{analysisGroups} i.e., \code{"analysisGroup1"} or \code{"analysisGroup2"}
#'
#' @param postHocAnalyses Character vector with \code{"all"},
#'   (which will do all analysisGroups) the default,
#'   or 1 or more of the the \code{functionName}s that are in the analyses slot.
#'
#' @param ... Optional arguments to pass into \code{functionName}
#'
#'
#' @aliases mapAddPostHocAnalysis
#' @importFrom fastdigest fastdigest
#' @rdname postHoc
mapAddPostHocAnalysis <- function(map, functionName, postHocAnalysisGroups = NULL,
                                  postHocAnalyses = "all", ...) {
  dots <- list(...)

  if (is.null(postHocAnalysisGroups))
    stop("postHocAnalysisGroups cannot be NULL. It should be one of the column names ",
         "in metadata(map) that starts with 'analysisGroup'")
  b <- if (length(dots)) {
    data.table(functionName = functionName, t(dots))
  } else {
    data.table(functionName = functionName, postHocAnalysisGroups = postHocAnalysisGroups,
               postHocAnalyses = postHocAnalyses, postHoc = TRUE)
  }
  prevEntry <- map@analyses$functionName == functionName
  purgeAnalyses <- NULL # Set default as NULL
  newDigest <- fastdigest::fastdigest(
    c(.robustDigest(get(b[, functionName])),
      .robustDigest(b[, !"functionName"]))
  )
  set(b, NULL, "argHash", newDigest)
  doRbindlist <- TRUE
  if (sum(prevEntry)) {
    if (!isTRUE(newDigest %in% map@analyses[prevEntry, ]$argHash)) {
      message("An analysis called ", functionName, " already added to map object; ",
              " Overwriting it")
      purgeAnalyses <- functionName
      map@analyses <- map@analyses[!prevEntry]
    } else {
      doRbindlist <- FALSE
      message("An analysis called, ", functionName, " with identical function and ",
              "arguments already added and run. Skipping reruns.")
    }
  }

  if (doRbindlist)
    map@analyses <- rbindlist(list(map@analyses, b), fill = TRUE, use.names = TRUE)

  map <- runMapAnalyses(map, purgeAnalyses)
  map

}

## TODO: needs documentation?
runMapAnalyses <- function(map, purgeAnalyses = NULL) {
  browser(expr = exists("aaa"))
  isPostHoc <- if (is.null(map@analyses$postHoc)) {
    rep(FALSE, NROW(map@analyses))
  } else {
    compareNA(map@analyses$postHoc, TRUE)
  }
  if (NROW(map@analyses[!isPostHoc])) {
    out <- tryCatch(
      by(map@analyses[!isPostHoc], map@analyses$functionName[!isPostHoc],
         function(x) {
           ma <- mapAnalysis(map = map, functionName = x$functionName,
                             purgeAnalyses = purgeAnalyses)
           ma@analysesData[[x$functionName]]
         }),
      error = function(x) NULL)
    if (!is.null(out)) {
      map@analysesData[names(out)] <- lapply(out, function(x) x)
    } else {
      warning("One of the analyses failed")
    }
  }

  # run postHoc analyses
  if (NROW(map@analyses[isPostHoc])) {
    out <- tryCatch(
      by(map@analyses[isPostHoc], map@analyses$functionName[isPostHoc],
         function(x) {
           phas <- if (identical(x$postHocAnalyses, "all")) {
             map@analyses[!isPostHoc]$functionName
           } else {
             x$postHocAnalyses
           }
           names(phas) <- phas
           out2 <- lapply(phas, function(pha) {
             message("    Running ", x$functionName, " on ", pha)
             ma <- get(x$functionName)(map = map, pha,
                                       analysisGroups = x$postHocAnalysisGroups)
           })
           out2
         }),
      error = function(x) NULL)
    if (!is.null(out)) {
      map@analysesData[names(out)] <- lapply(out, function(x) x)
    } else {
      warning("One of the analyses failed")
    }
  }
  map
}
