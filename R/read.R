#' Read an ISA-JSON file into an R object.
#'
#' Reads an ISA-JSON file, given as a zip file or as a json file in a
#' specific folder, and builds an object of the \linkS4class{ISAjson} class.
#'
#' @param file A character string with the name of the ISA-JSON file is located
#' (if the parameter zipfile is not provided or if it
#' is equal to \code{NULL}), or the name of the directory where the zip archive
#' containing the ISA-JSON file is located (if the parameter zipfile is not
#' \code{NULL}).
#' @param zipfile A character vector with the name of the zip archive containing
#' ISA-JSON file itself (without a directory name in the zip archive). The
#' default value is \code{NULL} (specifying that the ISA-JSON file has not
#' been archived in a zipped file).
#' @param verbose Should the messages for the different reading steps be shown?
#'
#' @returns An object of the \linkS4class{ISAjson} class.
#'
#' @export
readISAJSON <- function(file,
                        zipfile = NULL,
                        verbose = FALSE) {
  checkCharacter(file)
  ## Run file.path to assure trailing / are removed/added when needed.
  ## This is required to assure specifying path with/without trailing /
  ## works on all operating systems.
  file <- file.path(file)
  if (!file.exists(file)) {
    stop(file, " is not an existing file on this system.\n")
  }
  if (!is.null(zipfile)) {
    checkCharacter(zipfile)
    readZippedISAJSONFiles(file = file, zipfile = zipfile, verbose = verbose)
  } else {
    readISAJSONFiles(file = file, verbose = verbose)
  }
}

#' Helper function for reading zipped isajson files.
#'
#' This function only works if the zip file does not contain a directory name
#' (but the ISA-JSON files itselve)
#'
#' @noRd
#' @keywords internal
readZippedISAJSONFiles <- function(file,
                                   zipfile,
                                   verbose = FALSE) {
  ## Create temporary directory for unzipping.
  tmpdir <- normalizePath(tempdir())
  if (verbose) {
    message("Unzipping file in temporary directory: ", tmpdir, "")
  }
  ## Unzip file.
  unzippedISAJSONFiles <-
    utils::unzip(zipfile = normalizePath(file.path(file, zipfile)),
                 exdir = tmpdir)
  if (verbose) {
    message("Unzipped files: ",
            paste(basename(unzippedISAJSONFiles), collapse = ", "))
  }
  ## Read from temp directory.
  isaObj <- readISAJSONFiles(file = unzippedISAJSONFiles, verbose = verbose)
  ## Clean up and remove temp directory.
  unlink(tmpdir)
  return(isaObj)
}

#' Helper function for reading isajson files from a specified directory.
#'
#' @noRd
#'
#' @keywords internal
readISAJSONFiles <- function(file,
                             verbose = FALSE) {
  if (verbose) {
    message("Converting ISA-JSON dataset at ",
            normalizePath(file),
            " into an R object...")
  }
  isaobject <- new(Class = "ISAjson", path = normalizePath(file))
  if (verbose) {
    message("... done.")
  }
  return(isaobject)
}
