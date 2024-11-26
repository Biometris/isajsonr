#' Check the validity of an object of class ISAjson.
#'
#' The \code{validISAObject} function checks whether an object of class
#' \linkS4class{ISAjson} is a valid object. An object of the
#' \linkS4class{ISAjson} is considered valid when:
#' * The structure of the json file matches the schema for a valid ISA-JSON
#' file that can be found at
#' https://github.com/ISA-tools/isa-api/tree/master/isatools/resources/schemas/isa_model_version_1_0_schemas/core
#' *
#'
#' @param object An object of class \linkS4class{ISAjson}.
#'
#' @returns TRUE or an error message.
#'
#' @seealso \linkS4class{ISAjson}
#'
#' @export
validISAJSONObject <- function(object) {
  ## Check that path points to an existing folder.
  objPath <- isaPath(object)
  if (!file.exists(objPath)) {
    stop(objPath, " is not an existing folder on this system.\n")
  } else {
    objPath <- normalizePath(objPath)
  }
  jsonvalidate::json_validate(json = objPath,
                              schema = system.file("extdata/schema/investigation_schema.json",
                                                   package = "isajsonr"),
                              engine = "ajv",
                              verbose = TRUE)
}
setValidity(Class = "ISAjson",
            method = validISAJSONObject)
