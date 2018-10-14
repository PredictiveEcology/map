#' Generic analysis for map objects
#'
#'
mapAnalysis <- function(map, functionName = NULL, ...) {
  m <- map@metadata

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
  formalsInFunction <- formalArgs(functionName)[formalArgs(functionName) %in% colnames(m)]
  names(formalsInFunction) <- formalsInFunction

  if (NROW(combosToDo)) {
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
                fnOut <- do.call(Cache, args = c(list(get(functionName)), args, list(...)))
                combosCompleted <<- c(combosCompleted, combo$all)
                list(dt = fnOut)
              })

    map@analysesData[[functionName]][names(out)] <- out
    map@analysesData[[functionName]]$.Completed <- combosCompleted
  }
  map
}

mapAddAnalysis <- function(functionName)
