### start helper functions ----

checkCharacter <- function(...) {
  args <- list(...)
  if (!all(sapply(args, is.character))) {
    stop("The provided arguments must be of class character.")
  }
}

#' Helper function for row binding data.frames with different columns.
#'
#' @param dfList A list of data.frames.
#'
#' @noRd
#' @keywords internal
dfBind <- function(dfList) {
  ## Filter empty data.frames from dfList
  dfList <- Filter(f = function(x) nrow(x) > 0, x = dfList)
  if (length(dfList) == 0) {
    return(data.frame())
  }
  ## Get variable names from all data.frames.
  allNms <- unique(unlist(lapply(dfList, names)))
  ## rbind all data.frames setting values for missing columns to NA.
  do.call(rbind,
          c(lapply(X = dfList, FUN = function(x) {
            nwDat <- sapply(X = setdiff(allNms, names(x)), FUN = function(y) {
              NA
            })
            data.frame(c(x, nwDat), check.names = FALSE,
                       stringsAsFactors = FALSE)
          }), make.row.names = FALSE)
  )
}

#' Helper function for creating data frames for slots in ISA object for
#' JSON input format.
#'
#' @noRd
#' @keywords internal
createISASlotDataFrameJson <- function(file,
                                       name,
                                       cols) {
  tempdf <- lapply(X = file[[name]], FUN = as.data.frame,
                   stringsAsFactors = FALSE)
  tempdf <- do.call(what = dfBind, args = list(tempdf))
  rownames(tempdf) <- NULL
  if (any(rowSums(!is.na(tempdf)) == 0)) {
    tempdf <- tempdf[-c(which(rowSums(!is.na(tempdf)) == 0)), ]
  }
  if (nrow(tempdf) == 0) {
    tempdf <- data.frame(matrix(NA_character_, nrow = 0, ncol = length(cols)))
  }
  colnames(tempdf) <- cols
  return(tempdf)
}




### end helper functions
