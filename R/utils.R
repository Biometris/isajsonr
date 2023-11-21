### Minimum required columns for data.frames.
oSRCols <- c("description",
             "file",
             "name",
             "version")
investCols <- c("identifier",
                "title",
                "description",
                "submissionDate",
                "publicReleaseDate")
pubsCols <- c("pubMedID",
              "doi",
              "authorList",
              "title")
contactCols <- c("lastName",
                 "firstName",
                 "midInitials",
                 "email",
                 "phone",
                 "fax",
                 "address",
                 "affiliation")
studyCols <- c("filename",
               "identifier",
               "title",
               "description",
               "submissionDate",
               "publicReleaseDate")
sDDCols <- c("annotationValue",
             "termSource",
             "termAccession")
sFactCols <- c("@id",
               "factorName")
sAssaysCols <- c("filename",
                 "technologyPlatform")
sProtsCols <- c("Study Protocol Name",
                "Study Protocol Type",
                "Study Protocol Type Term Accession Number",
                "Study Protocol Type Term Source REF",
                "Study Protocol Description",
                "Study Protocol URI",
                "Study Protocol Version",
                "Study Protocol Parameters Name",
                "Study Protocol Parameters Name Term Accession Number",
                "Study Protocol Parameters Name Term Source REF",
                "Study Protocol Components Name",
                "Study Protocol Components Type",
                "Study Protocol Components Type Term Accession Number",
                "Study Protocol Components Type Term Source REF")

sProtsCols <- c("id",
                "name",
                "description",
                "uri",
                "version")
ontologyAnnotationCols <- c("@id",
                            "annotationValue",
                            "termSource",
                            "termAccession")

# sFilesCols <- c("Source Name",
#                 "Term Source REF",
#                 "Sample Name")
# aFilesCols <- c()

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
  if (all(sapply(X = dfList, FUN = function(x) {!isTRUE(nrow(x) > 0)}))) {
    return(data.frame())
  }
  ## Get variable names from all data.frames.
  allNms <- unique(unlist(lapply(dfList, colnames)))
  ## rbind all data.frames setting values for missing columns to NA.
  do.call(rbind,
          c(lapply(X = dfList, FUN = function(x) {
            if (!isTRUE(nrow(x) > 0)) {
              data.frame(matrix(NA_character_, nrow = 1, ncol = length(allNms),
                                dimnames = list(NULL, allNms)),
                         check.names = FALSE)
            } else {
              nwCols <- setdiff(allNms, colnames(x))
              nwDat <- as.data.frame(matrix(nrow = nrow(x), ncol = length(nwCols),
                                            dimnames = list(NULL, nwCols)))
              data.frame(cbind(x, nwDat), check.names = FALSE,
                         stringsAsFactors = FALSE)
            }
          }), make.row.names = FALSE)
  )
}

#' Helper function for creating data.frames for slots in ISA object for
#' JSON input format.
#'
#' @noRd
#' @keywords internal
createISAJsonDataFrame <- function(content,
                                   name,
                                   cols) {
  tempdf <- lapply(X = content[[name]], FUN = as.data.frame,
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

#' Helper function for converting data.frames to json like format.
#'
#' @noRd
#' @keywords internal
jsonDataFrameToList <- function(df) {
  lapply (X = 1:nrow(df), FUN = function(i) {
    as.list(df[i, ])
  })
}


#' Helper function for creating empty data.frame with column names.
#'
#' @noRd
#' @keywords internal
createEmptyDat <- function(cols) {
  colMat <- matrix(nrow = 0, ncol = length(cols), dimnames = list(NULL, cols))
  colDat <- as.data.frame(colMat)
  return(colDat)
}


#' Helper function for parsing comments section.
#'
#' @noRd
#' @keywords internal
parseComments <- function(comments) {
  if (is.data.frame(comments)) {
    comments <- list(comments)
  }
  commentDatLst <- lapply(X = comments, FUN = function(commentDat) {
    if (length(commentDat) > 0) {
      matrix(data = commentDat$value, nrow = 1,
             dimnames = list(NULL, paste0("Comment[", commentDat$name, "]")))
    }
  })
  dfBind(commentDatLst)
}

#' Helper function for deparsing comments section.
#'
#' @noRd
#' @keywords internal
deparseComments <- function(dat) {
  if (length(dat) > 0) {
    commentCols <- colnames(dat)[startsWith(colnames(dat), "Comment[") &
                                   endsWith(colnames(dat), "]")]
    if (length(commentCols) > 0) {
      commentNames <- gsub(pattern = "Comment[", replacement =  "",
                           x = commentCols, fixed = TRUE)
      commentNames <- substring(commentNames, first = 1,
                                last = nchar(commentNames) - 1)
    }
    lapply(X = 1:nrow(dat), FUN = function(i) {
      if (length(commentCols) > 0) {
        return(data.frame(name = commentNames,
                          value = unlist(dat[i, commentCols]),
                          row.names = NULL))
      } else {
        return(list())
      }
    })
  } else {
    return(list())
  }
}

#' Helper function for parsing ontology annotation.
#'
#' @noRd
#' @keywords internal
parseOntologySource <- function(dat,
                                name) {
  if (length(dat) == 0) {
    ontAnnotDat <- createEmptyDat(ontologyAnnotationCols)
  } else {
    ontAnnotDat <- dat[ontologyAnnotationCols]
    ontAnnotComments <- parseComments(ontAnnotDat$comments)
    if (nrow(ontAnnotComments) > 0) {
      ontAnnotDat <- cbind(ontAnnotDat, ontAnnotComments)
    }
  }
  colnames(ontAnnotDat) <- paste0(name, colnames(ontAnnotDat))
  return(ontAnnotDat)
}

#' Helper function for deparsing ontology annotation.
#'
#' @noRd
#' @keywords internal
deparseOntologySource <- function(dat,
                                  name) {
  if (nrow(dat) > 0) {
    ontAnnotDat <- dat[paste0(name, ontologyAnnotationCols)]
    ontAnnotDat[[paste0(name, "comments")]] <- deparseComments(ontAnnotDat)
    colnames(ontAnnotDat) <- substring(colnames(ontAnnotDat),
                                       first = nchar(name) + 1)
  } else {
    ontAnnotDat <- list()
  }
  return(ontAnnotDat)
}


#' Helper function for parsing ontology list annotation.
#'
#' @noRd
#' @keywords internal
parseOntologySourceLst <- function(dat,
                                   name) {
  ontAnnotLst <- lapply(X = dat, FUN = function(d) {
    ## Parse data.
    ontDat <- parseOntologySource(d, name = name)
    ## Remove empty columns.
    ontDat <- ontDat[, sapply(X = ontDat, FUN = function(od) {any(nzchar(od))})]
    colnames(ontDat) <- substring(colnames(ontDat), first = nchar(name) + 1)
    ontDat[[name]] <- ontDat[["annotationValue"]]
    ontDat <- ontDat[, c(name, setdiff(colnames(ontDat), c(name, "annotationValue")))]
  })
  ontAnnotDat <- do.call(cbind, ontAnnotLst)
  return(ontAnnotDat)
}

#' Helper function for deparsing ontology annotation.
#'
#' @noRd
#' @keywords internal
deparseOntologySourceLst <- function(dat,
                                     name) {
  nameColPos <- which(colnames(dat) == name)
  if (length(nameColPos) > 0) {
    ontDatLst <- lapply(X = nameColPos, FUN = function(pos) {
      ontDat <- dat[, pos, drop = FALSE]
      colnames(ontDat) <- "annotationValue"
      i <- 1
      while (pos + i <= ncol(dat) &&
             (colnames(dat)[pos + i] %in% ontologyAnnotationCols ||
             startsWith(x = colnames(dat)[pos + i], prefix = "Comment"))) {
        ontCol <- colnames(dat)[pos + i]
        ontDat[[ontCol]] <- dat[pos + i]
        i <- i + 1
      }
      missCols <- ontologyAnnotationCols[!ontologyAnnotationCols %in% colnames(ontDat)]
      if (length(missCols) > 0) ontDat[, missCols] <- ""
      colnames(ontDat) <- paste0(name, colnames(ontDat))
      ontDat <- deparseOntologySource(ontDat, name = name)
      return(ontDat)
    })
  } else {
    ontDatLst <- list()
  }
  return(ontDatLst)
}



#' Helper function for parsing protocol parameters.
#'
#' @noRd
#' @keywords internal
parseProtocolParams <- function(dat) {
  if (length(dat) == 0) {
    protParamDat <- createEmptyDat("id")
  } else {
    protParamDat <- dat["id"]
    protParamOntSource <- parseOntologySource(dat, name = "parameterName")
    protParamDat <- cbind(protParamDat, protParamOntSource)
  }
  colnames(protParamDat)[1] <- "parameterid"
  return(protParamDat)
}

#' Helper function for deparsing protocol parameters.
#'
#' @noRd
#' @keywords internal
deparseProtocolParams <- function(dat,
                                  name) {
  if (nrow(dat) > 0) {
    protParamDat <- dat["parameterid"]
    colnames(protParamDat) <- "id"
    protParamOntSource <- deparseOntologySource(dat, name = "parameterName")
    protParamDat <- cbind(protParamDat, protParamOntSource)
  } else {
    protParamDat <- list()
  }
  return(protParamDat)
}


### end helper functions
