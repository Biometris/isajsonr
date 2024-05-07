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
  names(sDDLst) <- getStudyFileNames(x)
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
  names(sPubsLst) <- getStudyFileNames(x)
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
  names(sContactsLst) <- getStudyFileNames(x)
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
  names(sFactsLst) <- getStudyFileNames(x)
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


#' @rdname sMaterials
setMethod("sMaterials", "ISAjson", function(x) {
  sMaterialsRaw <- x@content$studies$materials
  if (nrow(sMaterialsRaw) == 0) {
    sMaterialsDat <- createEmptyDat(c("sources", "samples", "otherMaterials"))
  } else {
    sMaterialsSources <- parseSourceLst(sMaterialsRaw$sources)
    sMaterialsSamples <- parseSamplesLst(sMaterialsRaw$samples)
    sMaterialsOther <- parseOtherLst(sMaterialsRaw$otherMaterials)
  }
  sMaterialsLst <- list(list(sources = sMaterialsSources,
                             samples = sMaterialsSamples,
                             other = sMaterialsOther))
  names(sMaterialsLst) <- getStudyFileNames(x)
  return(sMaterialsLst)
})



#' @rdname sUnitCats
setMethod("sUnitCats", "ISAjson", function(x) {
  sUnitCatsLst <- lapply(X = x@content$studies$unitCategories,
                         FUN = parseOntologySource, name = "")
  names(sUnitCatsLst) <- getStudyFileNames(x)
  return(sUnitCatsLst)
})

#' @rdname sUnitCats
setMethod("sUnitCats<-", "ISAjson", function(x, value) {
  sUnitCatsLst <- lapply(X = value, FUN = deparseOntologySource, name = "")
  x@content$studies$unitCategories <- sUnitCatsLst
  #validISAJSONObject(x)
  return(x)
})


#' @rdname sCharCats
setMethod("sCharCats", "ISAjson", function(x) {
  sCharCatsLst <- lapply(X = x@content$studies$characteristicCategories,
                         FUN = function(dat) {
                           if (length(dat) == 0) {
                             sCharCatsDat <- createEmptyDat(sCharCatsCols)
                           } else {
                             sCharCatsDat <- dat[sCharCatsCols]
                           }
                           sCharCatsOntology <- parseOntologySource(dat$characteristicType,
                                                                    name = "characteristicType")
                           sCharCatsDat <- cbind(sCharCatsDat, sCharCatsOntology)
                           return(sCharCatsDat)
                         })
  names(sCharCatsLst) <- getStudyFileNames(x)
  return(sCharCatsLst)
})

#' @rdname sCharCats
setMethod("sCharCats<-", "ISAjson", function(x, value) {
  sCharCatsLst <- lapply(X = value, FUN = function(dat) {
    sCharCatsDat <- dat[sCharCatsCols]
  })
  x@content$studies$characteristicCategories <- sCharCatsLst
  for (i in seq_along(value)) {
    sCharCatsOntology <- deparseOntologySource(value[[i]], name = "characteristicType")
    x@content$studies$characteristicCategories[[i]]$characteristicType <- sCharCatsOntology
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
                         sAssaysUnitOnt <- parseOntologySourceLst(dat$unitCategories,
                                                                  name = "unitCategories")
                         if (ncol(sAssaysUnitOnt) > 0) {
                           sAssaysDat <- cbind(sAssaysDat, sAssaysUnitOnt)
                         }
                         return(sAssaysDat)
                       })
  names(sAssaysLst) <- getStudyFileNames(x)
  return(sAssaysLst)
})

#' @rdname sAssays
setMethod("sAssays<-", "ISAjson", function(x, value) {
  for (i in seq_along(value)) {
    for (sAssayCol in sAssaysCols) {
      x@content$studies$assays[[i]][[sAssayCol]] <- value[[i]][[sAssayCol]]
    }
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


### sProts

#' @rdname sProts
setMethod("sProts", "ISAjson", function(x) {
  sProtsLst <- lapply(X = x@content$studies$protocols,
                      FUN = function(dat) {
                        if (length(dat) == 0) {
                          sProtsDat <- createEmptyDat(sProtsCols)
                        } else {
                          sProtsDat <- dat[sProtsCols]
                          sProtsComments <- parseComments(dat$comments)
                          if (nrow(sProtsComments) > 0) {
                            sProtsDat <- cbind(sProtsDat, sProtsComments)
                          }
                        }
                        sProtsOntology <- parseOntologySource(dat$protocolType,
                                                              name = "protocolType")
                        sProtsDat <- cbind(sProtsDat, sProtsOntology)
                        # sProtsParamOntology <- parseOntologySourceLst(dat$parameters,
                        #                                               name = "parameters")
                        # if (ncol(sProtsParamOntology) > 0) {
                        #   sProtsDat <- cbind(sProtsDat, sProtsParamOntology)
                        # }
                        return(sProtsDat)
                      })
  names(sProtsLst) <- getStudyFileNames(x)
  return(sProtsLst)
})

#' @rdname sProts
setMethod("sProts<-", "ISAjson", function(x, value) {
  sProtsLst <- lapply(X = value, FUN = function(dat) {
    sProtsDat <- dat[sProtsCols]
  })
  x@content$studies$protocols <- sProtsLst
  for (i in seq_along(value)) {
    sProtsCommentDat <- deparseComments(value[[i]])
    x@content$studies$protocols[[i]]$comments <- sProtsCommentDat
    sProtsTypeOntology <- deparseOntologySource(value[[i]], name = "protocolType")
    x@content$studies$protocols[[i]]$protocolType <- sProtsTypeOntology
    # sProtsParamsDat <- deparseProtocolParams(value[[i]])
    # x@content$studies$protocols[[i]]$parameters <- sProtsParamsDat
  }
  #validISAJSONObject(x)
  return(x)
})


### sProcSeq

#' @rdname sProcSeq
setMethod("sProcSeq", "ISAjson", function(x) {
  sProcSeqLst <- lapply(X = x@content$studies$processSequence,
                        FUN = function(dat) {
                          if (length(dat) == 0) {
                            sProcSeqDat <- createEmptyDat(sProcSeqCols)
                          } else {
                            sProcSeqDat <- dat[sProcSeqCols]
                            sProcSeqComments <- parseComments(dat$comments)
                            if (nrow(sProcSeqComments) > 0) {
                              sProcSeqDat <- cbind(sProcSeqDat, sProcSeqComments)
                            }
                            if (!is.null(dat$executesProtocol)) {
                              sProcSeqDat$executesProtocol <- dat$executesProtocol$`@id`
                            }
                            if (!is.null(dat$previousProcess)) {
                              sProcSeqDat$previousProcess <- dat$previousProcess$`@id`
                            }
                            if (!is.null(dat$nextProcess)) {
                              sProcSeqDat$nextProcess <- dat$nextProcess$`@id`
                            }
                            sProcSeqDat$inputs <- sapply(X = dat$inputs,
                                                         FUN = function(input) {
                                                           paste(unlist(input),
                                                                 collapse = ", ")
                                                         })
                            sProcSeqDat$outputs <- sapply(X = dat$outputs,
                                                          FUN = function(output) {
                                                            paste(unlist(output),
                                                                  collapse = ", ")
                                                          })
                          }
                          return(sProcSeqDat)
                        })
  names(sProcSeqLst) <- getStudyFileNames(x)
  return(sProcSeqLst)
})

#' @rdname sProcSeq
setMethod("sProcSeq<-", "ISAjson", function(x, value) {
  sProcSeqLst <- lapply(X = value, FUN = function(dat) {
    sProcSeqDat <- dat[sProcSeqCols]
    if (!is.null(dat$executesProtocol)) {
      sProcSeqDat$executesProtocol <- data.frame("@id" = dat$executesProtocol,
                                                 check.names = FALSE)
    }
    if (!is.null(dat$previousProcess)) {
      sProcSeqDat$previousProcess <- data.frame("@id" = dat$previousProcess,
                                                check.names = FALSE)
    }
    if (!is.null(dat$nextProcess)) {
      sProcSeqDat$nextProcess <- data.frame("@id" = dat$nextProcess,
                                            check.names = FALSE)
    }
    if (!is.null(dat$inputs)) {
      inputsLst <- strsplit(x = dat$inputs, split = ", ")
      inputsLst <- lapply(X = inputsLst, FUN = function(input) {
        data.frame("@id" = input, check.names = FALSE)
      })
      sProcSeqDat$inputs <- inputsLst
    }
    if (!is.null(dat$outputs)) {
      outputsLst <- strsplit(x = dat$outputs, split = ", ")
      outputsLst <- lapply(X = outputsLst, FUN = function(output) {
        data.frame("@id" = output, check.names = FALSE)
      })
      sProcSeqDat$outputs <- outputsLst
    }
    if (nrow(sProcSeqDat) == 0) {
      return(list())
    } else {
      return(sProcSeqDat)
    }
  })
  x@content$studies$processSequence <- sProcSeqLst
  for (i in seq_along(value)) {
    sProcSeqCommentDat <- deparseComments(value[[i]])
    if (length(sProcSeqCommentDat) > 0) {
      x@content$studies$processSequence[[i]]$comments <- sProcSeqCommentDat
    }
  }
  #validISAJSONObject(x)
  return(x)
})


### aFiles

#' @rdname aFiles
setMethod("aFiles", "ISAjson", function(x) {
  studyFileNames <- getStudyFileNames(x)
  aFiles <- lapply(X = seq_along(studyFileNames), FUN = function(i) {
    studyAssays <- x@content$studies$assays[[i]]
    dataFileAssayLst <- lapply(X = studyAssays$dataFiles, FUN = function(dataFile) {
      dataFileDat <- dataFile[dataCols]
      dataFileComments <- parseComments(dataFile$comments)
      if (nrow(dataFileComments) > 0) {
        dataFileDat <- cbind(dataFileDat, dataFileComments)
      }
      colnames(dataFileDat) <- paste0("dataFile", colnames(dataFileDat))
      return(dataFileDat)
    })
    names(dataFileAssayLst) <- getAssayFileNames(x)[[i]]
    return(dataFileAssayLst)
  })
  names(aFiles) <- studyFileNames
  return(aFiles)
})

#' @rdname aFiles
setMethod("aFiles<-", "ISAjson", function(x, value) {
  studyFiles <- names(value)
  for (i in seq_along(studyFiles)) {
    assayFiles <- names(value[[i]])
    for (j in seq_along(assayFiles)) {
      dataFileDat <- value[[i]][[j]]
      colnames(dataFileDat) <- gsub("dataFile", "", colnames(dataFileDat))
      x@content$studies$assays[[i]]$dataFiles[[j]] <- dataFileDat[dataCols]
      dataFileCommentDat <- deparseComments(dataFileDat)
      if (length(dataFileCommentDat) > 0) {
        x@content$studies$assays[[i]]$dataFiles[[j]]$comments <- dataFileCommentDat
      }
    }
  }
  #validISAObject(x)
  return(x)
})




### aMaterials


#' @rdname aMaterials
setMethod("aMaterials", "ISAjson", function(x) {
  assayDat <- x@content$studies$assays
  aMaterialsLst <- lapply(X = assayDat, FUN = function(assay) {
    aMaterialsRaw <- assay$materials
    if (nrow(aMaterialsRaw) == 0) {
      aMaterialsDat <- createEmptyDat(c("samples", "otherMaterials"))
    } else {
      aMaterialsSamples <- parseSamplesLst(aMaterialsRaw$samples)
      aMaterialsOther <- parseOtherLst(aMaterialsRaw$otherMaterials)
    }
    return(list(samples = aMaterialsSamples,
                other = aMaterialsOther))
  })
  names(aMaterialsLst) <- getStudyFileNames(x)
  return(aMaterialsLst)
})



#' @rdname aUnitCats
setMethod("aUnitCats", "ISAjson", function(x) {
  assayDat <- x@content$studies$assays
  aUnitCatsLst <- lapply(X = seq_along(assayDat), FUN = function(i) {
    aUnitCatsDat <- lapply(X = assayDat[[i]]$unitCategories,
                           FUN = parseOntologySource, name = "")
    names(aUnitCatsDat) <- getAssayFileNames(x)[[i]]
    return(aUnitCatsDat)
  })
  names(aUnitCatsLst) <- getStudyFileNames(x)
  return(aUnitCatsLst)
})

#' @rdname aUnitCats
setMethod("aUnitCats<-", "ISAjson", function(x, value) {
  studyFiles <- names(value)
  for (i in seq_along(studyFiles)) {
    assayFiles <- names(value[[i]])
    aUnitCatsLst <- lapply(X = value[[i]], FUN = deparseOntologySource, name = "")
    for (j in seq_along(aUnitCatsLst)) {
      x@content$studies$assays[[i]]$unitCategories[[j]] <- aUnitCatsLst[[j]]
    }
  }
  #validISAJSONObject(x)
  return(x)
})



#' @rdname aCharCats
setMethod("aCharCats", "ISAjson", function(x) {
  assayDat <- x@content$studies$assays
  aCharCatsLst <- lapply(X = seq_along(assayDat), FUN = function(i) {
    aCharCatsLst <- lapply(X = assayDat[[i]]$characteristicCategories,
                           FUN = function(dat) {
                             if (length(dat) == 0) {
                               aCharCatsDat <- createEmptyDat(sCharCatsCols)
                             } else {
                               aCharCatsDat <- dat[sCharCatsCols]
                             }
                             aCharCatsOntology <- parseOntologySource(dat$characteristicType,
                                                                      name = "characteristicType")
                             aCharCatsDat <- cbind(aCharCatsDat, aCharCatsOntology)
                             return(aCharCatsDat)
                           })
    names(aCharCatsLst) <- getAssayFileNames(x)[[i]]
    return(aCharCatsLst)
  })
  names(aCharCatsLst) <- getStudyFileNames(x)
  return(aCharCatsLst)
})

#' @rdname aCharCats
setMethod("aCharCats<-", "ISAjson", function(x, value) {
  studyFiles <- names(value)
  for (i in seq_along(studyFiles)) {
    assayFiles <- names(value[[i]])
    aCharCatsLst <- lapply(X = value[[i]], FUN = function(dat) {
      aCharCatsDat <- dat[sCharCatsCols]
      if (nrow(aCharCatsDat) > 0) {
        return(aCharCatsDat)
      } else {
        return(list())
      }
    })
    for (j in seq_along(aCharCatsLst)) {
      x@content$studies$assays[[i]]$characteristicCategories[[j]] <-
        aCharCatsLst[[j]]
      if (length(aCharCatsLst[[j]]) > 0) {
        aCharCatsOntology <- deparseOntologySource(value[[i]][[j]],
                                                   name = "characteristicType")
        x@content$studies$assays[[i]]$characteristicCategories[[j]]$characteristicType <-
          aCharCatsOntology
      }
    }
  }
  #validISAJSONObject(x)
  return(x)
})



### aProcSeq

#' @rdname aProcSeq
setMethod("aProcSeq", "ISAjson", function(x) {
  assayDat <- x@content$studies$assays
  aProcSeqLst <- lapply(X = seq_along(assayDat), FUN = function(i) {
    aProcSeqLst <- lapply(X = assayDat[[i]]$processSequence,
                          FUN = function(dat) {
                            if (length(dat) == 0) {
                              aProcSeqDat <- createEmptyDat(sProcSeqCols)
                            } else {
                              aProcSeqDat <- dat[sProcSeqCols]
                              aProcSeqComments <- parseComments(dat$comments)
                              if (nrow(aProcSeqComments) > 0) {
                                aProcSeqDat <- cbind(aProcSeqDat, aProcSeqComments)
                              }
                              if (!is.null(dat$executesProtocol)) {
                                aProcSeqDat$executesProtocol <- dat$executesProtocol$`@id`
                              }
                              if (!is.null(dat$previousProcess)) {
                                aProcSeqDat$previousProcess <- dat$previousProcess$`@id`
                              }
                              if (!is.null(dat$nextProcess)) {
                                aProcSeqDat$nextProcess <- dat$nextProcess$`@id`
                              }
                              aProcSeqDat$inputs <- sapply(X = dat$inputs,
                                                           FUN = function(input) {
                                                             paste(unlist(input),
                                                                   collapse = ", ")
                                                           })
                              aProcSeqDat$outputs <- sapply(X = dat$outputs,
                                                            FUN = function(output) {
                                                              paste(unlist(output),
                                                                    collapse = ", ")
                                                            })
                            }
                            return(aProcSeqDat)
                          })
    names(aProcSeqLst) <- getAssayFileNames(x)[[i]]
    return(aProcSeqLst)
  })
  names(aProcSeqLst) <- getStudyFileNames(x)
  return(aProcSeqLst)
})

#' @rdname aProcSeq
setMethod("aProcSeq<-", "ISAjson", function(x, value) {
  studyFiles <- names(value)
  for (i in seq_along(studyFiles)) {
    assayFiles <- names(value[[i]])
    aProcSeqLst <- lapply(X = value[[i]], FUN = function(dat) {
      aProcSeqDat <- dat[sProcSeqCols]
      if (!is.null(dat$executesProtocol)) {
        aProcSeqDat$executesProtocol <- data.frame("@id" = dat$executesProtocol,
                                                   check.names = FALSE)
      }
      if (!is.null(dat$previousProcess)) {
        aProcSeqDat$previousProcess <- data.frame("@id" = dat$previousProcess,
                                                  check.names = FALSE)
      }
      if (!is.null(dat$nextProcess)) {
        aProcSeqDat$nextProcess <- data.frame("@id" = dat$nextProcess,
                                              check.names = FALSE)
      }
      if (!is.null(dat$inputs)) {
        inputsLst <- strsplit(x = dat$inputs, split = ", ")
        inputsLst <- lapply(X = inputsLst, FUN = function(input) {
          data.frame("@id" = input, check.names = FALSE)
        })
        aProcSeqDat$inputs <- inputsLst
      }
      if (!is.null(dat$outputs)) {
        outputsLst <- strsplit(x = dat$outputs, split = ", ")
        outputsLst <- lapply(X = outputsLst, FUN = function(output) {
          data.frame("@id" = output, check.names = FALSE)
        })
        aProcSeqDat$outputs <- outputsLst
      }
      if (nrow(aProcSeqDat) == 0) {
        return(list())
      } else {
        return(aProcSeqDat)
      }
    })
    for (j in seq_along(assayFiles)) {
      x@content$studies$assays[[i]]$processSequence[[j]] <- aProcSeqLst[[j]]
      if (length(aProcSeqLst[[j]]) > 0) {
        aProcSeqCommentDat <- deparseComments(value[[i]][[j]])
        if (length(aProcSeqCommentDat) > 0) {
          x@content$studies$assays[[i]]$processSequence[[j]]$comments <-
            aProcSeqCommentDat
        }
      }
    }
  }
  #validISAJSONObject(x)
  return(x)
})














### Convenience functions

#' Retrieve the Study File Name(s) and Study File Identifier(s)from an ISAjson
#' object.
#'
#' Retrieve from an object of the \linkS4class{ISAjson} the Study
#' Identifier(s) and Study File Name(s) as contained in the Investigation.
#' To directly access the Study Identifier(s) use the names() function, e.g.
#' \code{names(getStudyFileNames(isaObject))}.
#'
#' @inheritParams writeISAjson
#'
#' @return A named character vector containing the Study File Name(s) and the
#' name(s) representing the Study Identifier(s).
#'
#' @export
getStudyFileNames <- function(isaObject) {
  studies <- study(isaObject)
  studyNames <- sapply(X = 1:nrow(studies), FUN = function(i) {
    studyName <- studies[i, "filename"]
    if (!nzchar(studyName)) {
      paste0("study", i)
    } else {
      studyName
    }
  })
  studyIds <- sapply(X = 1:nrow(studies), FUN = function(i) {
    studyId <- studies[i, "identifier"]
    if (!nzchar(studyId)) {
      paste0("study", i)
    } else {
      studyId
    }
  })
  names(studyNames) <- studyIds
  return(studyNames)
}


#' Retrieve the Assay File Name(s) per Study from an ISA object.
#'
#' Retrieve from an object of the \linkS4class{ISAjson} the Assay File Name(s)
#' linked to the Study Identifier(s) per Study.
#'
#' @inheritParams writeISAjson
#'
#' @return A named list of character vectors containing the Assay File Name(s)
#' for each Study Identifier. The name of the character vector or names of the
#' list elements represent(s) the Study Identifier(s).
#'
#' @export
getAssayFileNames <- function(isaObject) {
  studies <- getStudyFileNames(isaObject)
  assays <- sAssays(isaObject)
  assayNames <- lapply(X = seq_along(studies), FUN = function(i) {
    lapply(X = seq_along(assays[[i]][["filename"]]), FUN = function(j) {
      assayName <- assays[[i]][["filename"]][j]
      if (!nzchar(assayName)) {
        paste0("assay", i)
      } else {
        assayName
      }
    })
  })
  names(assayNames) <- studies
  return(assayNames)
}


#' Retrieve Factor Values per Study File from an ISA object.
#'
#' Retrieve from an object of the \linkS4class{ISAjson} the Factor Values for
#' each Study File.
#'
#' @inheritParams writeISAtab
#'
#' @return A list of factor lists, where each list element, named by the Study
#' Identifier, contains a list of factors specifying the Factor Values used
#' in a specific Study File linked to the Study Identifier.
#'
#' @noRd
#' @keywords internal
getFactors <- function(isaObject) {
  tmplist <- lapply(sFacts(isaObject), `[[` , "factorTypeannotationValue")
  #names(tmplist) <- getStudyFileNames(isaObject)
  return(tmplist)
}


