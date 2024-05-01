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


### assayTab Class ----
#' S4 Class assayTab, initialization methods
#'
#' An S4 class to store information from an assay files.
#'
#' @slot path A length-one character vector containing the path to the ISA-Tab
#' data set.
#' @slot sFileName A length-one character vector containing the study
#' file name (by definition starting with **s_** and ending at **.txt**).
#' @slot sIdentifier A length-one character vector containing the study
#' identifier.
#' @slot aFileName A length-one character vector containing the assay
#' file name (by definition starting with **a_** and ending at **.txt**).
#' @slot aFile A data.frame containing the contents of the Assay Table file.
#' @slot aTechType A length-one character vector containing the assay
#' technology type.
#' @slot aMeasType A length-one character vector containing the assay
#' measurement type.
#'
#' @keywords classes
#' @rdname assayTab-class
#' @exportClass assayTab
assayTab <- setClass(Class = "assayTab",
                     slots = c(
                       path = "character",
                       sFilename = "character",
                       sIdentifier = "character",
                       aFilename = "character",
                       aFile = "data.frame",
                       aTechType = "character",
                       aMeasType = "character"
                     ),
                     prototype = list(
                       path = NA_character_,
                       sFilename = NA_character_,
                       sIdentifier = NA_character_,
                       aFilename = NA_character_,
                       aFile = data.frame(),
                       aTechType = NA_character_,
                       aMeasType = NA_character_
                     )
)

### Derived assayTab Classes ----
## Special classes for different technology types to enable different methods
## for each technology type.

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass msAssayTab
msAssayTab <- setClass(Class = "msAssayTab",
                       contains = "assayTab",
                       prototype = prototype(aTechType = "mass spectrometry"))

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass microarrayAssayTab
microarrayAssayTab <- setClass(Class = "microarrayAssayTab",
                               contains = "assayTab",
                               prototype = prototype(aTechType = "DNA microarray"))


#' @keywords classes
#' @rdname assayTab-class
#' @exportClass seqAssayTab
seqAssayTab <- setClass(Class = "seqAssayTab",
                        contains = "assayTab",
                        prototype = prototype(aTechType = "nucleotide sequencing"))

