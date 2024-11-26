#' Write ISA-JSON object.
#'
#' Write ISA-JSON object to file.
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param file A character string with the name of the file that should be
#' written.
#'
#' @returns No return value, file is written.
#'
#' @export
writeISAjson <- function(isaObject,
                         file) {
  jsonlite::write_json(x = isaObject@content,
                       path = file,
                       pretty = TRUE)
}
