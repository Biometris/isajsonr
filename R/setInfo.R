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
  iPubDat <- x@content$publications[pubsCols]
  iPubComments <- parseComments(x@content$publications$comments)
  if (nrow(iPubComments) > 0) {
    iPubDat <- cbind(iPubDat, iPubComments)
  }
  return(iPubDat)
})

#' @rdname iPubs
setMethod("iPubs<-", "ISAjson", function(x, value) {
  x@content$publications <- value[pubsCols]
  x@content$publications$comments <- deparseComments(value)
  #validISAJSONObject(x)
  return(x)
})


### iContacts

#' @rdname iContacts
setMethod("iContacts", "ISAjson", function(x) {
  x@content$people
})

#' @rdname iContacts
setMethod("iContacts<-", "ISAjson", function(x, value) {
  x@content$people <- value
  #validISAJSONObject(x)
  return(x)
})


### study

#' @rdname study
setMethod("study", "ISAjson", function(x) {
  data.frame(x@content$studies[studyCols])
})

#' @rdname study
setMethod("study<-", "ISAjson", function(x, value) {
  for (studyCol in studyCols) {
    x@content$studies[[studyCol]] <- value[[studyCol]]
  }
  #validISAJSONObject(x)
  return(x)
})


### sDD

#' @rdname sDD
setMethod("sDD", "ISAjson", function(x) {
  x@content$studies$studyDesignDescriptors
})

#' @rdname sDD
setMethod("sDD<-", "ISAjson", function(x, value) {
  x@content$studies$studyDesignDescriptors <- value
  #validISAJSONObject(x)
  return(x)
})


### sPubs

#' @rdname sPubs
setMethod("sPubs", "ISAjson", function(x) {
  x@content$studies$publications
})

#' @rdname sPubs
setMethod("sPubs<-", "ISAjson", function(x, value) {
  x@content$studies$publications <- value
  #validISAJSONObject(x)
  return(x)
})


### sFacts

#' @rdname sFacts
setMethod("sFacts", "ISAjson", function(x) {
  x@content$studies$factors
})

#' @rdname sFacts
setMethod("sFacts<-", "ISAjson", function(x, value) {
  x@content$studies$factors <- value
  #validISAJSONObject(x)
  return(x)
})


### sAssays

#' @rdname sAssays
setMethod("sAssays", "ISAjson", function(x) {
  data.frame(x@content$studies$assays[[1]][sAssaysCols])
})

#' @rdname sAssays
setMethod("sAssays<-", "ISAjson", function(x, value) {
  for (sAssaysCol in sAssaysCols) {
    x@content$studies$assays[[1]][[sAssaysCol]] <- value[[sAssaysCol]]
  }
  #validISAJSONObject(x)
  return(x)
})


### sProts

#' @rdname sProts
setMethod("sProts", "ISAjson", function(x) {
  x@content$studies$protocols
})

#' @rdname sProts
setMethod("sProts<-", "ISAjson", function(x, value) {
  x@content$studies$protocols <- value
  #validISAObject(x)
  return(x)
})


### sContacts

#' @rdname sContacts
setMethod("sContacts", "ISAjson", function(x) {
  x@content$studies$people
})

#' @rdname sContacts
setMethod("sContacts<-", "ISAjson", function(x, value) {
  x@content$studies$people <- value
  #validISAJSONObject(x)
  return(x)
})


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
