#' Generic analysis for map objects
#'
#' This is the workhorse function that runs any analyses described in
#' \code{map@analyses}. It uses hashing, and will not rerun any analysis that
#' already ran on identical inputs.
#'
#' @inheritParams mapAdd
#' @importFrom data.table setDT
#' @param functionName A function name that will be run on combinations of
#'   inputs in the map obj. See details.
#' @param purgeAnalyses A character string indicating which analysis group
#'   combination or part thereof (e.g., the name entered into the row under
#'   \code{analysisGroup2} column of the \code{map@metadata} or a \code{functionName}.
#'
#' @details
#' This function will do a sequence of things. First, it will run \code{expand.grid}
#' on any columns whose names start with \code{analysisGroup}, creating a factorial
#' set of analyses as described by these columns. It will assess the combinations
#' against the arguments used by the functionName. For any analysisGroup that
#' does not provide the correct arguments for the functionName, these analysisGroups
#' will be omitted for that particular function. For efficiency, the function will
#' then assess if any of
#' these has already been run. For those that have not been run, it will then
#' run the \code{functionName} on arguments that it finds in the \code{metadata}
#' slot of the map obj, as well as any arguments passed in here in the \code{...}.
#' In general, the arguments being passed in here should be fixed across all analyses,
#' while any that vary by analysis should be entered into the metadata table at the
#' time of adding the layer to the map, via \code{mapAdd}.
#'
#' @importFrom stats na.omit
mapAnalysis <- function(map, functionName = NULL, purgeAnalyses = NULL,
                        useParallel = getOption("map.useParallel"),
                        ...) {
  m <- map@metadata
  dots <- list(...)

  if (is.null(functionName)) {
    stop("Each analysis must have a functionName")
  }
  if (is.null(names(functionName)))
    names(functionName) <- functionName
  if (!isTRUE(any(grepl("analysisGroup", colnames(m))))) {
    stop("Expecting analysisGroup1 column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  AGs <- sort(unique(colnames(m)[startsWith(colnames(m), "analysisGroup")]))
  names(AGs) <- AGs
  ags <- lapply(AGs, function(AG) sort(na.omit(unique(m[[AG]]))))

  combosCompleted <- lapply(functionName, function(fn)
    map@analysesData[[fn]]$.Completed)
  #combosCompleted <- map@analysesData[[functionName]]$.Completed

  # Purge if purgeAnalyses is non NULL
  if (!is.null(purgeAnalyses)) {
    message("Purging previous analyses with ", purgeAnalyses)
    purge <- names(combosCompleted) %in% purgeAnalyses
    if (sum(purge) > 0)
      combosCompleted[purge] <- list(rep(NULL, sum(purge)))
  }

  #if (is.null(combosCompleted)) {
  combosAll <- expandAnalysisGroups(ags)

  # Cycle through for each analysisGroup, get each argument
  # This is only for the first one to do, as it is just finding the columns required
  # This will be run again inside the combosToDo section below
  args1 <- lapply(functionName, function(funName) {
    args <- getFormalsFromMetadata(metadata = m, combo = combosAll[1,],
                                   AGs = AGs, funName = funName)
    keepArgs <- unlist(lapply(args, function(arg) length(arg) > 0))
    args[keepArgs]

  })

  AGsByFunName <- lapply(functionName, function(funName) {
    names(args1[[funName]])
  })

  # Corrected for analysis groups that are relevant to each functionName
  combosAll <-Map(agsByFunName = AGsByFunName,
      MoreArgs = list(ags = ags),
      function(agsByFunName, ags) {
    expandAnalysisGroups(ags[agsByFunName])
  })


  combosToDo <- Map(cc = combosCompleted, ca = combosAll, function(cc, ca) {
    if (!is.null(cc))
      setDT(ca[!ca$all %in% cc,])
    else
      setDT(ca)
  })

  combosToDo <- Map(ctd = combosToDo, fn = functionName,
      function(ctd, fn) ctd[, functionName:=fn])
  combosToDoDT <- rbindlist(combosToDo)
  # clear out empty ones
  combosToDo <- combosToDo[!unlist(lapply(combosToDo, function(ctd) NROW(ctd)==0))]

  if (NROW(combosToDoDT)) {
    funNames <- unique(combosToDoDT$functionName)
    names(funNames) <- funNames
    # Get the fixed arguments
    otherFormalsInFunction <- lapply(funNames, function(funName) {
      otherFormalsInFunction <- formalArgs(funName)[formalArgs(funName) %in%
                                                      colnames(map@analyses)]
      if (length(otherFormalsInFunction)) {
        names(otherFormalsInFunction) <- otherFormalsInFunction

        # Override dots from this function call
        dots <- lapply(otherFormalsInFunction, function(form) {
          fn <- funName
          assign(form, map@analyses[functionName == fn, get(form)][[1]])
        })
      }
    })

    cl <- makeOptimalCluster(useParallel, maxNumClusters = NROW(combosToDoDT))
    on.exit(try(stopCluster(cl), silent = TRUE))

    combosToDoList <- split(combosToDoDT, combosToDoDT$all)
    out3 <- Map2(cl = cl,
      combo = combosToDoList, function(combo) {
        funName <- combo$functionName
        args1 <- getFormalsFromMetadata(metadata = map@metadata,
                                        combo = combo, AGs = AGs, funName = funName)
        args <- unlist(unname(args1), recursive = FALSE)
        message("  Calculating ", funName, " for ", combo$all)
        fnOut <- do.call(Cache,
                         args = append(list(get(funName)),
                                       append(args,
                                              otherFormalsInFunction[[funName]])))
        fnOut
      })

    for (funName in funNames) {
      fromFunName <- combosToDoDT$functionName==funName
      map@analysesData[[funName]][names(out3)[fromFunName]] <- out3[fromFunName]
      map@analysesData[[funName]]$.Completed <- names(out3[fromFunName])
    }
  } else {
    message("  ", functionName, " already run on all layers")
  }
  map
}

#' @export
#' @importFrom data.table data.table
#' @importFrom fastdigest fastdigest
mapAddAnalysis <- function(map, functionName,
                           useParallel = getOption("map.useParallel"), ...) {
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
      message("An analysis called ", functionName, " already added to map obj; ",
              " Overwriting it")
      purgeAnalyses <- functionName
      map@analyses <- map@analyses[!prevEntry]
    } else {
      doRbindlist <- FALSE
      message("An analysis called ", functionName, " with identical function and ",
              "arguments already added and run. Skipping reruns.")
    }
  }

  if (doRbindlist)
    map@analyses <- rbindlist(list(map@analyses, b), fill = TRUE, use.names = TRUE)

  map <- runMapAnalyses(map = map, purgeAnalyses = purgeAnalyses, useParallel = useParallel)

  map

}


#' Add a post hoc analysis function to a map obj
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
                                  postHocAnalyses = "all",
                                  useParallel = getOption("map.useParallel"),
                                  ...) {
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
      message("An analysis called ", functionName, " already added to map obj; ",
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

  map <- runMapAnalyses(map = map, purgeAnalyses = purgeAnalyses, useParallel = useParallel)
  map

}

## TODO: needs documentation?
runMapAnalyses <- function(map, purgeAnalyses = NULL,
                           useParallel = getOption("map.useParallel")) {
  isPostHoc <- if (is.null(map@analyses$postHoc)) {
    rep(FALSE, NROW(map@analyses))
  } else {
    compareNA(map@analyses$postHoc, TRUE)
  }
  if (NROW(map@analyses[!isPostHoc])) {
    funName <- map@analyses$functionName[!isPostHoc]
    map <- mapAnalysis(map, funName, purgeAnalyses = purgeAnalyses)
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

getFormalsFromMetadata <- function(metadata, combo, AGs, funName) {
  formalsInFunction <- formalArgs(funName)[formalArgs(funName) %in% colnames(metadata)]
  names(formalsInFunction) <- formalsInFunction
  args <- lapply(AGs, function(AG) {
    args <- lapply(formalsInFunction, function(arg) {
      val <- na.omit(metadata[get(AG) == combo[[AG]], ][[arg]])
      if (isTRUE(val)) {
        val <- get(metadata[get(AG) == combo[[AG]], layerName],
                   envir = metadata[get(AG) == combo[[AG]], envir][[1]])
      }
      if (length(val) > 0)
        assign(arg, val)
      else
        NULL
    })
    args[!sapply(args, is.null)]
  })
  #args <- unlist(unname(args), recursive = FALSE)

  args
}

expandAnalysisGroups <- function(ags) {
   if (any(unlist(lapply(ags, function(x) length(x > 0))))) {
     combosAll <- do.call(expand.grid, args = append(list(stringsAsFactors = FALSE),
                                                     lapply(ags, function(x) x)))
     combosAll$all <- apply(combosAll, 1, paste, collapse = "._.")
   }
   combosAll
}
