#' Read an ISA-JSON file into an R object.
#'
#' Reads an ISA-JSON file, given as a zip file or as a json file in a
#' specific folder, and builds an object of the \linkS4class{ISAjson} class.
#'
#' @param path A character vector with the name of the directory in which the
#' ISA-JSON file is located (if the parameter zipfile is not provided or if it
#' is equal to \code{NULL}), or the name of the directory where the zip archive
#' containing the ISA-JSON file is located (if the parameter zipfile is not
#' \code{NULL}). The default value is the current working directory.
#' @param zipfile A character vector with the name of the zip archive containing
#' ISA-JSON file itselve (without a directory name in the zip archive). The
#' default value is \code{NULL} (specifying that the ISA-JSON file has not
#' been archived in a zipped file).
#' @param verbose Should the messages for the different reading steps be shown?
#'
#' @return An object of the \linkS4class{ISAjson} class.
#'
#' @export
readISAJSON <- function(path = getwd(),
                        zipfile = NULL,
                        verbose = FALSE) {
  checkCharacter(path)
  ## Run file.path to assure trailing / are removed/added when needed.
  ## This is required to assure specifying path with/without trailing /
  ## works on all operating systems.
  path <- file.path(path)
  if (!file.exists(path)) {
    stop(path, " is not an existing folder on this system.\n")
  }
  if (!is.null(zipfile)) {
    checkCharacter(zipfile)
    readZippedISAJSONFiles(path = path, zipfile = zipfile, verbose = verbose)
  } else {
    readISAJSONFiles(path = path, verbose = verbose)
  }
}

#' Helper function for reading zipped isajson files.
#'
#' This function only works if the zip file does not contain a directory name
#' (but the ISA-JSON files itselve)
#'
#' @noRd
#' @keywords internal
readZippedISAJSONFiles <- function(path = getwd(),
                                   zipfile,
                                   verbose = FALSE) {
  ## Create temporary directory for unzipping.
  tmpdir <- normalizePath(tempdir())
  if (verbose) {
    message("Unzipping file in temporary directory: ", tmpdir, "")
  }
  ## Unzip file.
  unzippedISAJSONFiles <-
    utils::unzip(zipfile = normalizePath(file.path(path, zipfile)),
                 exdir = tmpdir)
  if (verbose) {
    message("Unzipped files: ",
            paste(basename(unzippedISAJSONFiles), collapse = ", "))
  }
  ## Read from temp directory.
  isaObj <- readISAJSONFiles(path = tmpdir, verbose = verbose)
  ## Clean up and remove temp directory.
  unlink(tmpdir)
  return(isaObj)
}

#' Helper function for reading isajson files from a specified directory.
#'
#' @noRd
#'
#' @keywords internal
readISAJSONFiles <- function(path = getwd(),
                            verbose = FALSE) {
  if (verbose) {
    message("Converting ISA-JSON dataset at ",
            normalizePath(path),
            " into an R object...")
  }
  isaobject <- new(Class = "ISAjson", path = normalizePath(path))
  if (verbose) {
    message("... done.")
  }
  return(isaobject)
}
