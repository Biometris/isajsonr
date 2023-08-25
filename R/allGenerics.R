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
    .Object@content <- jsonlite::read_json(path)
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



