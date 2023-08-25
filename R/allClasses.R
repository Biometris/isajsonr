### ISAjson Class ----
#' S4 Class ISAjson, initialization method
#'
#' An S4 class to store information from an ISA-JSON file.
#'
#' @slot path A length-one character vector containing the path to the ISA-JSON
#' file.
#' @slot content The file content.
#'
#' @keywords classes
#' @rdname ISA-class
#' @exportClass ISAjson
ISAjson <- setClass(Class = "ISAjson",
                    slots = c(
                      path = "character",
                      content = "list"
                    )
)
