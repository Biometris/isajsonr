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
#' `r paste0("'", iPubsCols, "'", collapse = ", ")`.
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


