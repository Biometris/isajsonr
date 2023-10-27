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
contactCols <- c("id",
                 "lastName",
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
sFactCols <- c("id",
               "factorName")
sAssaysCols <- c("measurementType",
                 "technologyType",
                 "technologyPlatform",
                 "filename")
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
sContactsCols <- c("Study Person Last Name",
                   "Study Person First Name",
                   "Study Person Mid Initials",
                   "Study Person Email",
                   "Study Person Phone",
                   "Study Person Fax",
                   "Study Person Address",
                   "Study Person Affiliation",
                   "Study Person Roles",
                   "Study Person Roles Term Accession Number",
                   "Study Person Roles Term Source REF")
ontologyAnnotationCols <- c("annotationValue",
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
  dfList <- Filter(f = function(x) nrow(x) > 0, x = dfList)
  if (length(dfList) == 0) {
    return(data.frame())
  }
  ## Get variable names from all data.frames.
  allNms <- unique(unlist(lapply(dfList, colnames)))
  ## rbind all data.frames setting values for missing columns to NA.
  do.call(rbind,
          c(lapply(X = dfList, FUN = function(x) {
            nwCols <- setdiff(allNms, colnames(x))
            nwDat <- as.data.frame(matrix(nrow = nrow(x), ncol = length(nwCols),
                                          dimnames = list(NULL, nwCols)))
            data.frame(cbind(x, nwDat), check.names = FALSE,
                       stringsAsFactors = FALSE)
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
    ontAnnotComments <- parseComments(dat[[paste0(name, "comments")]])
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
    ontAnnotCols <- colnames(dat)[startsWith(colnames(dat), prefix = name)]
    ontAnnotDat <- dat[ontAnnotCols]
    colnames(ontAnnotDat) <- substring(text = colnames(ontAnnotDat),
                                       first = nchar(name) + 1)
    ontAnnotDat$comments <- deparseComments(ontAnnotDat)
  }
  return(ontAnnotDat)
}




### end helper functions
