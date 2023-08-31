#' Write ISA-JSON object.
#'
#' Write ISA-JSON object to file. The file is written to the folder specified in
#' \code{path}.
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param path A character vector with the name of the directory to which the
#' file(s) should be written. The default value is the current working
#' directory.
#'
#' @return No return value, files are written to path.
#'
#' @export
writeISAjson <- function(isaObject,
                         path = getwd()) {
  jsonlite::write_json(x = isaObject@content,
                       path = path,
                       pretty = TRUE)
}
