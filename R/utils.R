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
              "title",
              "annotationValue",
              "termSource",
              "termAccession")
iContactCols <- c("id",
                  "lastName",
                  "firstName",
                  "midInitials",
                  "email",
                  "phone",
                  "fax",
                  "address",
                  "affiliation",
                  "annotationValue",
                  "termSource",
                  "termAccession")
studyCols <- c("filename",
               "identifier",
               "title",
               "description",
               "submissionDate",
               "publicReleaseDate")
sDDCols <- c("annotationValue",
             "termSource",
             "termAccession")
sFactCols <- c("factorName",
               "annotationValue",
               "termSource",
               "termAccession")



# sAssaysCols <- c("Study Assay Measurement Type",
#                  "Study Assay Measurement Type Term Accession Number",
#                  "Study Assay Measurement Type Term Source REF",
#                  "Study Assay Technology Type",
#                  "Study Assay Technology Type Term Accession Number",
#                  "Study Assay Technology Type Term Source REF",
#                  "Study Assay Technology Platform",
#                  "Study Assay File Name")
# sProtsCols <- c("Study Protocol Name",
#                 "Study Protocol Type",
#                 "Study Protocol Type Term Accession Number",
#                 "Study Protocol Type Term Source REF",
#                 "Study Protocol Description",
#                 "Study Protocol URI",
#                 "Study Protocol Version",
#                 "Study Protocol Parameters Name",
#                 "Study Protocol Parameters Name Term Accession Number",
#                 "Study Protocol Parameters Name Term Source REF",
#                 "Study Protocol Components Name",
#                 "Study Protocol Components Type",
#                 "Study Protocol Components Type Term Accession Number",
#                 "Study Protocol Components Type Term Source REF")
# sContactsCols <- c("Study Person Last Name",
#                    "Study Person First Name",
#                    "Study Person Mid Initials",
#                    "Study Person Email",
#                    "Study Person Phone",
#                    "Study Person Fax",
#                    "Study Person Address",
#                    "Study Person Affiliation",
#                    "Study Person Roles",
#                    "Study Person Roles Term Accession Number",
#                    "Study Person Roles Term Source REF")
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



### end helper functions
