#' isajsonr package overview.
#'
#' The isajsonr package is an implementation of the ISA-JSON format for R. It
#' builds an R object from ISA-JSON format files.\cr\cr
#' ISA is a metadata framework to manage an increasingly diverse set of life
#' science, environmental and biomedical experiments that employ one or a
#' combination of technologies. Built around the **Investigation** (the
#' project context), **Study** (a unit of research) and **Assay**
#' (analytical measurements) concepts, ISA helps you to provide rich
#' descriptions of experimental metadata (i.e. sample characteristics,
#' technology and measurement types, sample-to-data relationships) so that the
#' resulting data and discoveries are reproducible and reusable.
#'
#' The ISA Abstract Model has been implemented in two format specifications,
#' ISA-Tab and ISA-JSON. In this package the latter, ISA-JSON, is being used.
#' ISA-JSON files are json files stored with a **.json** extension.
#'
#' @references
#' [ISA framework](https://isa-tools.org/)
#' [ISA Model and Serialization Specifications](https://isa-specs.readthedocs.io/en/stable/)
#' [ISA users community](https://www.isacommons.org)
#'
#' @import methods
#' @importFrom jsonlite read_json
#'
#' @keywords internal
"_PACKAGE"
