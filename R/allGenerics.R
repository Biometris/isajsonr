##' Constructor method of ISAjson-class.
#'
#' When creating a new object of class \linkS4class{ISAjson} via:
#' \code{object <- new(Class = "ISAjson", path)}, the function
#' \code{initialize(.Object, path)} is called to initialize and create the
#' actual object. The \code{initialize-method} is seldom used as a function
#' itself.
#'
#' @param .Object character, name of the object of class \linkS4class{ISAjson}
#' to be initialized
#' @param path length-one character vector containing the path to the ISA-JSON
#' file.
#'
#' @rdname ISAjson-class
#' @aliases ISAjson-initialize
#' @export
setMethod(
  f = "initialize",
  signature = "ISAjson",
  definition = function(.Object, path) {
    ## Assignment of the "path" slot
    .Object@path <- path
    ## Read content.
    .Object@content <- jsonlite::read_json(path, simplifyVector = TRUE)
    return(.Object)
  }
)


### Path standard generics.

#' Get and set path
#'
#' Get and set the file path for an object of class \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return The path to the folder for the object of class \linkS4class{ISAjson}.
#'
#' @rdname isaPath
#' @export
setGeneric("isaPath", function(x) standardGeneric("isaPath"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A length-one character vector indicating the isaPath to an
#' accessible directory on the system.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname isaPath
#' @export
setGeneric("isaPath<-", function(x, value) standardGeneric("isaPath<-"))


### oSR standard generics.

#' Get and set oSR
#'
#' Get and set the ontology Source Reference (oSR) data.frame in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A data.frame containing the ontology Source Reference information.
#'
#' @rdname oSR
#' @export
setGeneric("oSR", function(x) standardGeneric("oSR"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A data.frame containing the ontology Source Reference
#' information. data.frame in which at least the following columns are present:
#' `r paste0("'", oSRCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname oSR
#' @export
setGeneric("oSR<-", function(x, value) standardGeneric("oSR<-"))


### invest standard generics.

#' Get and set invest.
#'
#' Get and set the investigation data.frame in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A data.frame containing the investigation information.
#'
#' @rdname invest
#' @export
setGeneric("invest", function(x) standardGeneric("invest"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A data.frame containing the investigation information. A
#' data.frame in which at least the following columns are present:
#' `r paste0("'", investCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname invest
#' @export
setGeneric("invest<-", function(x, value) standardGeneric("invest<-"))


### iPubs standard generics.

#' Get and set iPubs.
#'
#' Get and set the iPubs data.frame in an object of \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A data.frame containing the investigation publications information.
#'
#' @rdname iPubs
#' @export
setGeneric("iPubs", function(x) standardGeneric("iPubs"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A data.frame containing the investigation publications
#' information. A data.frame in which at least the following columns are
#' present:
#' `r paste0("'", pubsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname iPubs
#' @export
setGeneric("iPubs<-", function(x, value) standardGeneric("iPubs<-"))


### iContacts standard generics.

#' Get and set iContacts.
#'
#' Get and set the iContacts data.frame in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A data.frame containing the investigation contacts information.
#'
#' @rdname iContacts
#' @export
setGeneric("iContacts", function(x) standardGeneric("iContacts"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A data.frame containing the investigation contacts
#' information. A data.frame in which at least the following columns are
#' present:
#' `r paste0("'", iContactsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname iContacts
#' @export
setGeneric("iContacts<-", function(x, value) standardGeneric("iContacts<-"))


### study standard generics.

#' Get and set study.
#'
#' Get and set the list of study data.frame in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study information.
#'
#' @rdname study
#' @export
setGeneric("study", function(x) standardGeneric("study"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study information.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", studyCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname study
#' @export
setGeneric("study<-", function(x, value) standardGeneric("study<-"))


### sDD standard generics.

#' Get and set sDD.
#'
#' Get and set the list of sDD data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study design descriptors.
#'
#' @rdname sDD
#' @export
setGeneric("sDD", function(x) standardGeneric("sDD"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study design descriptors.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sDDCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sDD
#' @export
setGeneric("sDD<-", function(x, value) standardGeneric("sDD<-"))


### sPubs standard generics.

#' Get and set sPubs.
#'
#' Get and set the list of sPubs data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study publications.
#'
#' @rdname sPubs
#' @export
setGeneric("sPubs", function(x) standardGeneric("sPubs"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study publications.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", pubsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sPubs
#' @export
setGeneric("sPubs<-", function(x, value) standardGeneric("sPubs<-"))


### sFacts standard generics.

#' Get and set sFacts.
#'
#' Get and set the list of sFacts data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study factors.
#'
#' @rdname sFacts
#' @export
setGeneric("sFacts", function(x) standardGeneric("sFacts"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study factors.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sFactsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sFacts
#' @export
setGeneric("sFacts<-", function(x, value) standardGeneric("sFacts<-"))


### sUnitCats standard generics.

#' Get and set sUnitCats.
#'
#' Get and set the list of sUnitCats data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study unit categories.
#'
#' @rdname sUnitCats
#' @export
setGeneric("sUnitCats", function(x) standardGeneric("sUnitCats"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study unit categories.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sUnitCats
#' @export
setGeneric("sUnitCats<-", function(x, value) standardGeneric("sUnitCats<-"))


### sAssays standard generics.

#' Get and set sAssays.
#'
#' Get and set the list of sAssays data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study assays.
#'
#' @rdname sAssays
#' @export
setGeneric("sAssays", function(x) standardGeneric("sAssays"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study assays.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sAssaysCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sAssays
#' @export
setGeneric("sAssays<-", function(x, value) standardGeneric("sAssays<-"))


### sProts standard generics.

#' Get and set sProts.
#'
#' Get and set the list of sProts data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study protocols.
#'
#' @rdname sProts
#' @export
setGeneric("sProts", function(x) standardGeneric("sProts"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study protocols.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sProtsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#'
#' @rdname sProts
#' @export
setGeneric("sProts<-", function(x, value) standardGeneric("sProts<-"))


### sMaterials standard generics.

#' Get and set sMaterials
#'
#' Get and set the list of sMaterials data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study protocols.
#'
#' @rdname sMaterials
#' @export
setGeneric("sMaterials", function(x) standardGeneric("sMaterials"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study materials.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#'
#' @rdname sMaterials
#' @export
setGeneric("sMaterials<-", function(x, value) standardGeneric("sMaterials<-"))


### sContacts standard generics.

#' Get and set sContacts.
#'
#' Get and set the list of sContacts data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study contacts.
#'
#' @rdname sContacts
#' @export
setGeneric("sContacts", function(x) standardGeneric("sContacts"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study contacts.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sContactsCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sContacts
#' @export
setGeneric("sContacts<-", function(x, value) standardGeneric("sContacts<-"))


### sFiles standard generics.

#' Get and set sFiles.
#'
#' Get and set the list of sFiles data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the study files.
#'
#' @rdname sFiles
#' @export
setGeneric("sFiles", function(x) standardGeneric("sFiles"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the study files.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", sFilesCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname sFiles
#' @export
setGeneric("sFiles<-", function(x, value) standardGeneric("sFiles<-"))


### aFiles standard generics.

#' Get and set aFiles.
#'
#' Get and set the list of aFiles data.frames in an object of
#' \linkS4class{ISAjson}.
#'
#' @param x An object of class \linkS4class{ISAjson}.
#'
#' @return A list of data.frames containing the assay files.
#'
#' @rdname aFiles
#' @export
setGeneric("aFiles", function(x) standardGeneric("aFiles"))

#' @param x An object of class \linkS4class{ISAjson}.
#' @param value A list of data.frames containing the assay files.
#' In each data.frame at least the following columns are present:
#' `r paste0("'", aFilesCols, "'", collapse = ", ")`.
#'
#' @return The updated object of class \linkS4class{ISAjson}.
#' @rdname aFiles
#' @export
setGeneric("aFiles<-", function(x, value) standardGeneric("aFiles<-"))

