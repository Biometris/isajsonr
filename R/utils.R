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
sCharCatsCols <- c("@id")
sAssaysCols <- c("filename",
                 "technologyPlatform")
sProtsCols <- c("@id",
                "name",
                "description",
                "uri",
                "version")
sProcSeqCols <- c("@id",
                  "name",
                  "date",
                  "performer")
ontologyAnnotationCols <- c("@id",
                            "annotationValue",
                            "termSource",
                            "termAccession")
dataCols <- c("@id",
              "name",
              "type")


## Raw Data File Column Names - depend on technology type.
rawDataFileCols = c(base = "Raw Data File",
                    microarray = "Array Data File",
                    ms = "Raw Spectral Data File",
                    "Free Induction Decay Data File")

## Derived Data File Column Names - depend on technology type.
derivedDataFileCols = c(base = "Derived Data File",
                        microarray = "Derived Array Data File",
                        ms = "Derived Spectral Data File")

### start technologyTypes list ----
## Technology types is a free field.
## Some technologies may have different/additional defined columns.
## Those technologies are specified here
technologyTypes <- list(
  microarray = "DNA microarray",
  gelelecto  = "Gel electrophoresis",
  fc         = "flow cytometry",
  ms         = "mass spectrometry",
  NMR        = "NMR spectroscopy",
  seq        = "nucleotide sequencing"
)
### end technologyTypes list ----



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
  if (nrow(dat) > 0) {
    commentCols <- colnames(dat)[startsWith(colnames(dat), "Comment[") &
                                   endsWith(colnames(dat), "]")]
    if (length(commentCols) > 0) {
      commentNames <- gsub(pattern = "Comment[", replacement =  "",
                           x = commentCols, fixed = TRUE)
      commentNames <- substring(commentNames, first = 1,
                                last = nchar(commentNames) - 1)
      lapply(X = 1:nrow(dat), FUN = function(i) {
        return(data.frame(name = commentNames,
                          value = unlist(dat[i, commentCols]),
                          row.names = NULL))
      })
    } else {
      return(data.frame(matrix(nrow = nrow(dat), ncol = 0)))
    }
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
    if (ncol(ontDat) > 0) {
      colnames(ontDat) <- substring(colnames(ontDat), first = nchar(name) + 1)
      ontDat[[name]] <- ontDat[["annotationValue"]]
      ontDat <- ontDat[, c(name, setdiff(colnames(ontDat),
                                         c(name, "annotationValue")))]
    }
    return(ontDat)
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


#' Helper function for parsing data list.
#'
#' @noRd
#' @keywords internal
parseDataLst <- function(dat,
                         name) {
  parsedDatLst <- lapply(X = dat, FUN = function(d) {
    if (length(d) == 0) {
      parsedDat <- createEmptyDat(dataCols)
    } else {
      parsedDat <- d[dataCols]
      parsedDatComments <- parseComments(d$comments)
      if (nrow(parsedDatComments) > 0) {
        parsedDat <- cbind(parsedDat, parsedDatComments)
      }
    }
    ## Remove empty columns.
    parsedDat <- parsedDat[, sapply(X = parsedDat, FUN = function(pd) {
      any(nzchar(pd))
    })]
    if (ncol(parsedDat) > 0) {
      parsedLst <- lapply(X = 1:nrow(parsedDat), FUN = function(i) {
        datI <- parsedDat[i, ]
        datI[["dataFiles"]] <- datI[["type"]]
        datI <- datI[, c("dataFiles", setdiff(colnames(datI),
                                              c("dataFiles", "type")))]
      })

    }
    return(parsedDat)
  })
  parsedDat <- do.call(cbind, parsedDatLst)
  return(parsedDat)
}


#' Helper function for parsing source data.
#'
#' @noRd
#' @keywords internal
parseSourceLst <- function(dat) {
  parsedLst <- lapply(X = dat, FUN = function(d) {
    if (length(d) == 0) {
      sourceDat <- createEmptyDat(c("@id", "name"))
    } else {
      sourceDat <- d[c("@id", "name")]
      sourceCharDat <- parseCharacteristicsLst(d$characteristics)
      if (nrow(sourceCharDat) > 0) {
        sourceDat <- cbind(sourceDat, sourceCharDat)
      }
      if (nrow(sourceDat) > 0) {
        colnames(sourceDat) <- paste0("sources", colnames(sourceDat))
      }
    }
    return(sourceDat)
  })
  return(parsedLst)
}


#' Helper function for parsing samples data.
#'
#' @noRd
#' @keywords internal
parseSamplesLst <- function(dat) {
  parsedLst <- lapply(X = dat, FUN = function(d) {
    if (length(d) == 0) {
      sampleDat <- createEmptyDat(c("@id", "name"))
    } else {
      sampleDat <- d[, colnames(d) %in% c("@id", "name"), drop = FALSE]
      sampleCharDat <- parseCharacteristicsLst(d$characteristics)
      if (nrow(sampleCharDat) > 0) {
        sampleDat <- cbind(sampleDat, sampleCharDat)
      }
      if (nrow(sampleDat) > 0) {
        colnames(sampleDat) <- paste0("samples", colnames(sampleDat))
      }
    }
    return(sampleDat)
  })
  return(parsedLst)
}


#' Helper function for parsing samples data.
#'
#' @noRd
#' @keywords internal
parseOtherLst <- function(dat) {
  parsedLst <- lapply(X = dat, FUN = function(d) {
    if (length(d) == 0) {
      otherDat <- createEmptyDat(c("@id", "name", "type"))
    } else {
      otherDat <- d[c("@id", "name", "type")]
      otherCharDat <- parseCharacteristicsLst(d$characteristics)
      if (nrow(otherCharDat) > 0) {
        otherDat <- cbind(otherDat, otherCharDat)
      }
      if (nrow(otherDat) > 0) {
        colnames(otherDat) <- paste0("other", colnames(otherDat))
      }
    }
    return(otherDat)
  })
  return(parsedLst)
}





#' Helper function for parsing material attribute.
#'
#' @noRd
#' @keywords internal
parseMaterialAttribute <- function(dat,
                                   name) {
  if (length(dat) == 0) {
    matAttrDat <- createEmptyDat("@id")
  } else {
    matAttrDat <- dat["@id"]
    matAttrOnt <- parseOntologySource(dat$characteristicsType, "characteristicsType")
    if (nrow(matAttrOnt) > 0) {
      matAttrDat <- cbind(matAttrDat, matAttrOnt)
    }
  }
  colnames(matAttrDat) <- paste0(name, colnames(matAttrDat))
  return(matAttrDat)
}


#' Helper function for parsing characteristics.
#'
#' @noRd
#' @keywords internal
parseCharacteristicsLst <- function(dat) {
  parsedLst <- lapply(X = dat, FUN = function(d) {
    if (length(d) == 0) {
      characteristicsDat <- createEmptyDat(c("@id", "category", "value", "unit"))
    } else {
      characteristicsDat <- parseMaterialAttribute(d$category, "category")
      if (is.data.frame(d$value)) {
        valueDat <- parseOntologySource(d$value, "value")
        characteristicsDat <- cbind(characteristicsDat, valueDat)
      } else {
        characteristicsDat$value <- d$value
      }
      unitDat <- parseOntologySource(d$unit, "unit")
      if (nrow(unitDat) > 0) {
        characteristicsDat <- cbind(characteristicsDat, unitDat)
      }
    }
    return(characteristicsDat)
  })
  return(dfBind(parsedLst))
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
