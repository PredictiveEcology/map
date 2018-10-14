#' Generic analysis for map objects
#'
#'
mapAnalysis <- function(map, functionName = NULL, ...) {
  m <- map@metadata
  dots <- list(...)

  if (is.null(functionName)) {
    stop("Each analysis must have a functionName")
  }
  if (is.null(m$analysisGroup1)) {
    stop("Expecting analysisGroup1 column in map metadata. ",
         "Please pass in a unique name representing the analysis group, ",
         "i.e., which tsf is associated with which vtm")
  }
  AGs <- sort(unique(colnames(m)[startsWith(colnames(m), "analysisGroup")]))
  names(AGs) <- AGs
  ags <- lapply(AGs, function(AG) sort(na.omit(unique(m[[AG]]))))
  combosCompleted <- map@analysesData[[functionName]]$.Completed

  #if (is.null(combosCompleted)) {
  if (any(unlist(lapply(ags, function(x) length(x>0))))) {
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
                  args <- lapply(formalsInFunction, function(arg) {
                    val <- na.omit(m[get(AG) == combo[[AG]], ][[arg]])
                    if (isTRUE(val)) {
                      val <- get(m[get(AG) == combo[[AG]], layerName],
                                 envir = m[get(AG) == combo[[AG]], envir][[1]])
                    }
                    if (length(val)>0)
                      assign(arg, val)
                    else
                      NULL
                  })
                  args[!sapply(args, is.null)]
                })
                args <- unlist(unname(args), recursive = FALSE)
                message("  Calculating ", functionName, " for ", combo$all)
                fnOut <- do.call(Cache, args = c(list(get(functionName)), args, dots))
                combosCompleted <<- c(combosCompleted, combo$all)
                list(dt = fnOut)
              })

    map@analysesData[[functionName]][names(out)] <- unname(lapply(out, function(x) x))
    map@analysesData[[functionName]]$.Completed <- combosCompleted
  }
  map
}

#' @export
mapAddAnalysis <- function(map, functionName, ...) {
  dots <- list(...)
  b <- data.table(functionName = functionName,
                  quotedFn = "mapAnalysis(map, functionName = functionName, ...)",
                  t(dots))
  prevEntry <- map@analyses$functionName==functionName
  if (sum(prevEntry)){
    message("An analysis called ", functionName, " already added to map object; ",
            " Overwriting it")
    map@analyses <- map@analyses[!prevEntry]
  }

  map@analyses <- rbindlist(list(map@analyses, b), fill = TRUE, use.names = TRUE)

  if (NROW(map@analyses)) {
    #browser()
    #map@analysesData <- .runMapAnalysis(map@analyses)

    out <- by(map@analyses, map@analyses$functionName,
              function(x) {
                ma <- mapAnalysis(map = map, functionName = x$functionName)
                ma@analysesData[[x$functionName]]
              })
    map@analysesData[names(out)] <- lapply(out, function(x) x)
    #map@analysesData <- lapply(out, function(x) x)
  }

  map

}


.runMapAnalysis <- function(analyses) {
  out <- by(analyses, analyses$functionName,
            function(x) {
              ma <- mapAnalysis(map = map, functionName = x$functionName)
              ma@analysesData[[x$functionName]]
            })
  lapply(out, function(x) x)
}
