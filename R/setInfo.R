### Get and set information for an ISA-class object

### Path.

#' @rdname isaPath
setMethod("isaPath", "ISAjson", function(x) x@path)

#' @rdname isaPath
setMethod("isaPath<-", "ISAjson", function(x, value) {
  ## nomalizePath is required to protect against system dependent paths,
  ## e.g. extra or missing /.
  x@path <- normalizePath(value)
  #validISAJSONObject(x)
  return(x)
})


### invest

#' @rdname invest
setMethod("invest", "ISAjson", function(x) {
  investDat <- data.frame(x@content[investCols])
  if (length(x@content$comments) > 0) {
    investComments <- parseComments(x@content$comments)
    investDat <- cbind(investDat, investComments)
  }
  return(investDat)
})

#' @rdname invest
setMethod("invest<-", "ISAjson", function(x, value) {
  for (investCol in investCols) {
    x@content[[investCol]] <- value[[investCol]]
  }
  x@content$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})


### oSR

#' @rdname oSR
setMethod("oSR", "ISAjson", function(x) {
  osrDat <- x@content$ontologySourceReferences[oSRCols]
  oSRComments <- parseComments(x@content$ontologySourceReferences$comments)
  if (nrow(oSRComments) > 0) {
    osrDat <- cbind(osrDat, oSRComments)
  }
  return(osrDat)
})

#' @rdname oSR
setMethod("oSR<-", "ISAjson", function(x, value) {
  x@content$ontologySourceReferences <- value[oSRCols]
  x@content$ontologySourceReferences$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})


### iPubs

#' @rdname iPubs
setMethod("iPubs", "ISAjson", function(x) {
  if (length(x@content$publications) == 0) {
    iPubsDat <- createEmptyDat(pubsCols)
  } else {
    iPubsDat <- x@content$publications[pubsCols]
    iPubsComments <- parseComments(x@content$publications$comments)
    if (nrow(iPubsComments) > 0) {
      iPubsDat <- cbind(iPubsDat, iPubsComments)
    }
  }
  iPubsOntology <- parseOntologySource(x@content$publications$status,
                                       name = "status")
  iPubsDat <- cbind(iPubsDat, iPubsOntology)
  return(iPubsDat)
})

#' @rdname iPubs
setMethod("iPubs<-", "ISAjson", function(x, value) {
  iPubsDat <- value[pubsCols]
  iPubsOntology <- deparseOntologySource(value, name = "status")
  x@content$publications <- iPubsDat
  x@content$publications$status <- iPubsOntology
  x@content$publications$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})


### iContacts

#' @rdname iContacts
setMethod("iContacts", "ISAjson", function(x) {
  if (length(x@content$people) == 0) {
    iContactsDat <- createEmptyDat(contactCols)
  } else {
    iContactsDat <- x@content$people[contactCols]
    iContactsComments <- parseComments(x@content$people$comments)
    if (nrow(iContactsComments) > 0) {
      iContactsDat <- cbind(iContactsDat, iContactsComments)
    }
  }
  iContactsOntology <- parseOntologySourceLst(x@content$people$roles,
                                              name = "roles")
  iContactsDat <- cbind(iContactsDat, iContactsOntology)
  return(iContactsDat)
})

#' @rdname iContacts
setMethod("iContacts<-", "ISAjson", function(x, value) {
  iContactsDat <- value[contactCols]
  iContactsOntology <- deparseOntologySource(value, name = "roles")
  x@content$people <- iContactsDat
  x@content$people$roles <- iContactsOntology
  x@content$people$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})



### study

#' @rdname study
setMethod("study", "ISAjson", function(x) {
  studyDat <- data.frame(x@content$studies[studyCols])
  studyComments <- parseComments(x@content$studies$comments)
  if (nrow(studyComments) > 0) {
    studyDat <- cbind(studyDat, studyComments)
  }
  return(studyDat)
})

#' @rdname study
setMethod("study<-", "ISAjson", function(x, value) {
  for (studyCol in studyCols) {
    x@content$studies[[studyCol]] <- value[[studyCol]]
  }
  x@content$studies$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})


### sDD

#' @rdname sDD
setMethod("sDD", "ISAjson", function(x) {
  sDDLst <- lapply(X = x@content$studies$studyDesignDescriptors,
                   FUN = parseOntologySource, name = "")
  return(sDDLst)
})

#' @rdname sDD
setMethod("sDD<-", "ISAjson", function(x, value) {
  sDDLst <- lapply(X = value, FUN = deparseOntologySource, name = "")
  x@content$studies$studyDesignDescriptors <- sDDLst
  #validISAJSONObject(x)
  return(x)
})


### sPubs

#' @rdname sPubs
setMethod("sPubs", "ISAjson", function(x) {
  sPubsLst <- lapply(X = x@content$studies$publications,
                     FUN = function(dat) {
                       if (length(dat) == 0) {
                         sPubsDat <- createEmptyDat(pubsCols)
                       } else {
                         sPubsDat <- dat[pubsCols]
                         sPubsComments <- parseComments(dat$comments)
                         if (nrow(sPubsComments) > 0) {
                           sPubsDat <- cbind(sPubsDat, sPubsComments)
                         }
                       }
                       sPubsOntology <- parseOntologySource(dat$status,
                                                            name = "status")
                       sPubsDat <- cbind(sPubsDat, sPubsOntology)
                       return(sPubsDat)
                     })
  return(sPubsLst)
})

#' @rdname sPubs
setMethod("sPubs<-", "ISAjson", function(x, value) {
  sPubsLst <- lapply(X = value, FUN = function(dat) {
    sPubsDat <- dat[pubsCols]
  })
  x@content$studies$publications <- sPubsLst
  for (i in seq_along(value)) {
    sPubsCommentDat <- deparseComments(value[[i]])
    x@content$studies$publications[[i]]$comments <- sPubsCommentDat
    sPubsOntology <- deparseOntologySource(value[[i]], name = "status")
    x@content$studies$publications[[i]]$status <- sPubsOntology
  }
  #validISAJSONObject(x)
  return(x)
})


### sContacts

#' @rdname sContacts
setMethod("sContacts", "ISAjson", function(x) {
  sContactsLst <- lapply(X = x@content$studies$people,
                         FUN = function(dat) {
                           if (length(dat) == 0) {
                             sContactsDat <- createEmptyDat(contactCols)
                           } else {
                             sContactsDat <- dat[contactCols]
                             sContactsComments <- parseComments(dat$comments)
                             if (nrow(sContactsComments) > 0) {
                               sContactsDat <- cbind(sContactsDat, sContactsComments)
                             }
                           }
                           sContactsOntology <- parseOntologySourceLst(dat$roles,
                                                                       name = "roles")
                           sContactsDat <- cbind(sContactsDat, sContactsOntology)
                           return(sContactsDat)
                         })
  return(sContactsLst)
})

#' @rdname sContacts
setMethod("sContacts<-", "ISAjson", function(x, value) {
  sContactsLst <- lapply(X = value, FUN = function(dat) {
    sContactsDat <- dat[contactCols]
  })
  x@content$studies$people <- sContactsLst
  for (i in seq_along(value)) {
    sContactsCommentDat <- deparseComments(value[[i]])
    x@content$studies$people[[i]]$comments <- sContactsCommentDat
    sContactsOntology <- deparseOntologySourceLst(value[[i]], name = "roles")
    x@content$studies$people[[i]]$roles <- sContactsOntology

  }
  #validISAJSONObject(x)
  return(x)
})


#' @rdname sFacts
setMethod("sFacts", "ISAjson", function(x) {
  sFactsLst <- lapply(X = x@content$studies$factors,
                      FUN = function(dat) {
                        if (length(dat) == 0) {
                          sFactsDat <- createEmptyDat(sFactCols)
                        } else {
                          sFactsDat <- dat[sFactCols]
                          sFactsComments <- parseComments(dat$comments)
                          if (nrow(sFactsComments) > 0) {
                            sFactsDat <- cbind(sFactsDat, sFactsComments)
                          }
                        }
                        sFactsOntology <- parseOntologySource(dat$factorType,
                                                              name = "factorType")
                        sFactsDat <- cbind(sFactsDat, sFactsOntology)
                        return(sFactsDat)
                      })
  return(sFactsLst)
})

#' @rdname sFacts
setMethod("sFacts<-", "ISAjson", function(x, value) {
  sFactsLst <- lapply(X = value, FUN = function(dat) {
    sFactsDat <- dat[sFactCols]
  })
  x@content$studies$factors <- sFactsLst
  for (i in seq_along(value)) {
    sFactsCommentDat <- deparseComments(value[[i]])
    x@content$studies$factors[[i]]$comments <- sFactsCommentDat
    sFactsOntology <- deparseOntologySource(value[[i]], name = "factorType")
    x@content$studies$factors[[i]]$factorType <- sFactsOntology

  }
  #validISAJSONObject(x)
  return(x)
})



### sAssays

#' @rdname sAssays
setMethod("sAssays", "ISAjson", function(x) {
  sAssaysLst <- lapply(X = x@content$studies$assays,
                       FUN = function(dat) {
                         if (length(dat) == 0) {
                           sAssaysDat <- createEmptyDat(sAssaysCols)
                         } else {
                           sAssaysDat <- dat[sAssaysCols]
                           sAssaysComments <- parseComments(dat$comments)
                           if (nrow(sAssaysComments) > 0) {
                             sAssaysDat <- cbind(sAssaysDat, sAssaysComments)
                           }
                         }
                         sAssaysMeasOnt <- parseOntologySource(dat$measurementType,
                                                               name = "measurementType")
                         sAssaysDat <- cbind(sAssaysDat, sAssaysMeasOnt)
                         sAssaysTechOnt <- parseOntologySource(dat$technologyType,
                                                               name = "technologyType")
                         sAssaysDat <- cbind(sAssaysDat, sAssaysTechOnt)
                         return(sAssaysDat)
                       })
  return(sAssaysLst)
})

#' @rdname sAssays
setMethod("sAssays<-", "ISAjson", function(x, value) {
  sAssaysLst <- lapply(X = value, FUN = function(dat) {
    sAssaysDat <- dat[sAssaysCols]
  })
  x@content$studies$assays <- sAssaysLst
  for (i in seq_along(value)) {
    sAssaysCommentDat <- deparseComments(value[[i]])
    x@content$studies$assays[[i]]$comments <- sAssaysCommentDat
    sAssaysMeasOntology <- deparseOntologySource(value[[i]], name = "measurementType")
    x@content$studies$assays[[i]]$measurementType <- sAssaysMeasOntology
    sAssaysTechOntology <- deparseOntologySource(value[[i]], name = "technologyType")
    x@content$studies$assays[[i]]$technologyType <- sAssaysTechOntology
  }
  #validISAJSONObject(x)
  return(x)
})







#' ### sProts
#'
#' #' @rdname sProts
#' setMethod("sProts", "ISAjson", function(x) {
#'   sProtsLst <- lapply(X = x@content$studies$protocols,
#'                       FUN = function(dat) {
#'                         if (length(dat) == 0) {
#'                           sProtsDat <- createEmptyDat(sProtsCols)
#'                         } else {
#'                           sProtsDat <- dat[sProtsCols]
#'                           sProtsComments <- parseComments(dat$comments)
#'                           if (nrow(sProtsComments) > 0) {
#'                             sProtsDat <- cbind(sProtsDat, sProtsComments)
#'                           }
#'                         }
#'                         sProtsOntology <- parseOntologySource(dat$protocolType,
#'                                                               name = "protocolType")
#'                         sProtsDat$protocolType <- sProtsOntology
#'                         sProtsParamsDat <- parseProtocolParams(dat$parameters)
#'                         sProtsDat <- cbind(sProtsDat, sProtsParamsDat)
#'                         return(sProtsDat)
#'                       })
#'   return(sProtsLst)
#' })
#'
#' #' @rdname sProts
#' setMethod("sProts<-", "ISAjson", function(x, value) {
#'   sProtsLst <- lapply(X = value, FUN = function(dat) {
#'     sProtsDat <- cbind(dat[sProtsCols],
#'                        deparseOntologySource(dat, name = "protocolType"))
#'   })
#'   x@content$studies$protocols <- sProtsLst
#'   for (i in seq_along(value)) {
#'     sProtsCommentDat <- deparseComments(value[[i]])
#'     x@content$studies$protocols[[i]]$comments <- sProtsCommentDat
#'     sProtsParamsDat <- deparseProtocolParams(value[[i]])
#'     x@content$studies$protocols[[i]]$parameters <- sProtsParamsDat
#'   }
#'   #validISAJSONObject(x)
#'   return(x)
#' })






### sAssays
#'
#' #' @rdname sAssays
#' setMethod("sAssays", "ISAjson", function(x) {
#'   data.frame(x@content$studies$assays[[1]][sAssaysCols])
#' })
#'
#' #' @rdname sAssays
#' setMethod("sAssays<-", "ISAjson", function(x, value) {
#'   for (sAssaysCol in sAssaysCols) {
#'     x@content$studies$assays[[1]][[sAssaysCol]] <- value[[sAssaysCol]]
#'   }
#'   #validISAJSONObject(x)
#'   return(x)
#' })

#'
#' ### sFiles
#'
#' #' @rdname sFiles
#' setMethod("sFiles", "ISA", function(x) x@sFiles)
#'
#' #' @rdname sFiles
#' setMethod("sFiles<-", "ISA", function(x, value) {
#'   x@sFiles <- value
#'   validISAObject(x)
#'   return(x)
#' })
#'
#' ### aFiles
#'
#' #' @rdname aFiles
#' setMethod("aFiles", "ISA", function(x) x@aFiles)
#'
#' #' @rdname aFiles
#' setMethod("aFiles<-", "ISA", function(x, value) {
#'   x@aFiles <- value
#'   validISAObject(x)
#'   return(x)
#' })
#'
#'
#' ### Convenience functions
#'
#' #' Retrieve the Study Identifier(s) and Study File Name(s) from an ISA object.
#' #'
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study
#' #' Identifier(s) and Study File Name(s) as contained in the Investigation.
#' #' To directly access the Study Identifier(s) use the names() function, e.g.
#' #' \code{names(getStudyFileNames(isaObject))}.
#' #'
#' #' @inheritParams writeISAtab
#' #'
#' #' @return A named character vector containing the Study File Name(s) and the
#' #' name(s) representing the Study Identifier(s).
#' #'
#' #' @examples
#' #' ## Read example Atwell data set.
#' #' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#' #'                                           package = "isatabr")))
#' #'
#' #' ## Extract study identifiers and file names.
#' #' getStudyFileNames(isaObject1)
#' #'
#' #' @export
#' getStudyFileNames <- function(isaObject) {
#'   sapply(X = study(isaObject), FUN = `[[`, ISASyntax$sFileName)
#' }
#'
#' #' Retrieve the Assay File Name(s) per Study from an ISA object.
#' #'
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay File Name(s)
#' #' linked to the Study Identifier(s) per Study.
#' #'
#' #' @inheritParams writeISAtab
#' #'
#' #' @return A named list of character vectors containing the Assay File Name(s)
#' #' for each Study Identifier. The name of the character vector or names of the
#' #' list elements represent(s) the Study Identifier(s).
#' #'
#' #' #' @examples
#' #' ## Read example Atwell data set.
#' #' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#' #'                                           package = "isatabr")))
#' #'
#' #' ## Extract assay file names per study.
#' #' getAssayFileNames(isaObject1)
#' #'
#' #' @export
#' getAssayFileNames <- function(isaObject) {
#'   lapply(X = sAssays(isaObject), FUN = `[[`, ISASyntax$aFileName)
#' }
#'
#'
#' #' Retrieve Factor Values per Study File from an ISA object.
#' #'
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Factor Values for
#' #' each Study File.
#' #'
#' #' @inheritParams writeISAtab
#' #'
#' #' @return A list of factor lists, where each list element, named by the Study
#' #' Identifier, contains a list of factors specifying the Factor Values used
#' #' in a specific Study File linked to the Study Identifier.
#' #'
#' #' @noRd
#' #' @keywords internal
#' getFactors <- function(isaObject) {
#'   tmplist <- lapply(X = isaObject@sFiles, FUN = function(df) {
#'     tmplist <- lapply(X = grep(pattern = ISASyntax$fctrValue,
#'                                x = colnames(df),
#'                                value = TRUE),
#'                       FUN = function(x) {
#'                         factor(df[[x]])
#'                       })
#'     names(tmplist) <- grep(pattern = ISASyntax$fctrValue,
#'                            x = colnames(df),
#'                            value = TRUE)
#'     return(tmplist)
#'   })
#'   names(tmplist) <- getStudyFileNames(isaObject)
#'   return(tmplist)
#' }
