#' Retrieve Assay Tables from an ISA object.
#'
#' Retrieve from an object of the \code{\link{ISAjson-class}} the Assay Tables.
#'
#' @inheritParams writeISAjson
#'
#' @return A list of lists of objects of class \code{\link{assayTab}}, where
#' each list element, named by the Study Identifier, contains a list of
#' objects of class \code{\link{assayTab}}.
#'
#' @export
getAssayTabs <- function(isaObject) {
  ## Get info from isaObject.
  studyFileNames <- getStudyFileNames(isaObject)
  assayInfo <- sAssays(isaObject)
  assayFiles <- aFiles(isaObject)
  assayTabs <- lapply(X = seq_along(assayInfo), FUN = function(i) {
    studyId <- names(assayInfo)[i]
    studyAssay <- assayInfo[[i]]
    assayTabsStudy <- lapply(X = 1:nrow(studyAssay), FUN = function(j) {
      ## Class is dependent of technology type.
      ## Returns empty character for 'non-existing' technology.
      assayTechType <- studyAssay[j, "technologyTypeannotationValue"]
      assayTechName <- names(technologyTypes)[technologyTypes == assayTechType]
      assayClass <- if (isTRUE(nzchar(assayTechName)))
        paste0(assayTechName, "AssayTab") else "assayTab"
      assayMeasType <- studyAssay[j, "measurementTypeannotationValue"]
      new(assayClass,
          path = isaPath(isaObject),
          sFilename = "",
          sIdentifier = studyId,
          aFilename = studyAssay[j, "filename"],
          aFile = assayFiles[[studyFileNames[i]]][[j]],
          aTechType = assayTechType,
          aMeasType = assayMeasType
      )
    })
    #names(assayTabsStudy) <- studyAssay[[ISASyntax$aFileName]]
    return(assayTabsStudy)
  })
  names(assayTabs) <- studyFileNames
  return(assayTabs)
}

#' Process all assay tab data
#'
#' Process data from all assay tab files present in an object of
#' \linkS4class{ISAjson}.
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param type A character string indicating which data files should be
#' processed, either "raw" for raw data files, or "derived" for derived data
#' files. The file names are taken from the corresponding column in the
#' \code{aTabObject}s.
#'
#' @docType methods
#' @rdname processAllAssays-methods
#' @exportMethod processAllAssays
setGeneric("processAllAssays",
           function(isaObject,
                    study,
                    type = c("raw", "derived")) standardGeneric("processAllAssays"))


#' @rdname processAssay-methods
#' @aliases processAllAssays,ISA,processAllAssays-method
setMethod(f = "processAllAssays",
          signature = c(isaObject = "ISAjson",
                        study = "character",
                        type = "character"),
          definition = function(isaObject, study, type) {
            type <- match.arg(type)
            assayTabs <- getAssayTabs(isaObject)
            assayTabsStudy <- assayTabs[[study]]
            assayCont <- lapply(X = assayTabsStudy,
                                FUN = function(assayTabStudy) {
                                  processAssay(isaObject, assayTabStudy, type)
                                })
            assayContUnlist <- unlist(assayCont, recursive = FALSE)
            assayContNames <- names(assayContUnlist)
            allNames <- unique(assayContNames)
            tst <- sapply(X = allNames, FUN = function(name) {
              data.table::rbindlist(assayContUnlist[assayContNames == name])
            }, simplify = FALSE)
          }
)

#' Process assay tab data
#'
#' Process data from assay tab file
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param aTabObject An object of the \linkS4class{assayTab}.
#' @param type A character string indicating which data files should be
#' processed, either "raw" for raw data files, or "derived" for derived data
#' files. The file names are taken from the corresponding column in the
#' \code{aTabObject}.
#'
#' @docType methods
#' @rdname processAssay-methods
#'
#' @importFrom utils hasName
#' @exportMethod processAssay
setGeneric("processAssay",
           function(isaObject,
                    aTabObject,
                    type = c("raw", "derived")) standardGeneric("processAssay"))

#' @rdname processAssay-methods
#' @aliases processAssay,ISA,assayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISAjson",
                        aTabObject = "assayTab",
                        type = "character"),
          definition = function(isaObject, aTabObject, type) {
            type <- match.arg(type)
            assayDat <- slot(aTabObject, "aFile")
            if (type == "derived") {
              assayDat <- assayDat[assayDat$dataFiletype %in% derivedDataFileCols, ]
            } else {
              assayDat <- assayDat[assayDat$dataFiletype %in% rawDataFileCols, ]
            }
            ## Get file names.
            if (hasName(x = assayDat, "dataFileComment[fullPath]")) {
              datFiles <- file.path(normalizePath(
                dirname(slot(aTabObject, "path")), winslash = .Platform$file.sep),
                unique(assayDat[["dataFileComment[fullPath]"]]))
            } else {
              datFiles <- file.path(normalizePath(slot(aTabObject, "path"),
                                                  winslash = .Platform$file.sep),
                                    unique(assayDat[["dataFilename"]]))
            }
            ## Check that files exist.
            missFiles <- datFiles[!file.exists(datFiles)]
            if (length(missFiles) > 0) {
              datFiles <- setdiff(datFiles, missFiles)
              if (length(datFiles) > 0) {
                warning("The following files are not found and skipped:\n",
                        paste(missFiles, collapse = ", "))
              } else {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
            }
            ## Read file contents.
            assayContLst <- lapply(X = datFiles, FUN = function(datFile) {
              tempdf <- data.table::fread(file.path(datFile), fill = TRUE,
                                          blank.lines.skip = TRUE)
              ## Remove empty columns.
              # Save and re-add colnames to assure duplicate names are not removed.
              colNames <- colnames(tempdf)
              tempdf <- tempdf[, nzchar(colnames(tempdf))]
              colnames(tempdf) <- colNames[nzchar(colnames(tempdf))]
              return(tempdf)
            })
            names(assayContLst) <- basename(datFiles)
            return(assayContLst)
          }
)

#' Process assay tab data for mass spectrometry
#'
#' Process data from assay tab files with technology type mass spectrometry
#' (ms). Processing those files requires the xcms package to be installed.
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param aTabObject An object of the \linkS4class{msAssayTab}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISAjson",
                        aTabObject = "msAssayTab",
                        type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("xcms", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              spectralDatFiles <-
                file.path(normalizePath(slot(aTabObject, "path"),
                                        winslash = .Platform$file.sep),
                          unique(assayDat[["dataFilename"]]))
              ## Check that files exist.
              missFiles <- spectralDatFiles[!file.exists(spectralDatFiles)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct vector of factors in assay.
              isaFactors <- getFactors(isaObject = isaObject)
              assayFactors <- names(isaFactors[[slot(aTabObject, "sIdentifier")]])
              if (length(assayFactors) > 0) {
                ## Construct data.frame with assay factors only.
                sClass <- assayDat[, assayFactors, drop = FALSE]
                for (colName in colnames(sClass)) {
                  if (!is.factor(sClass[[colName]])) {
                    ## Convert to factor if not a factor already.
                    sClass[[colName]] <- as.factor(sClass[[colName]])
                  }
                }
                xset <- xcms::xcmsSet(files = spectralDatFiles,
                                      sclass = sClass)
              } else {
                xset = try(xcms::xcmsSet(files = spectralDatFiles,
                                         phenoData = assayDat))
              }
              return(xset)
            } else {
              stop("For reading mass spectrometry data the xcms package ",
                   "should be installed.\n")
            }
          }
)

#' Process assay tab data for DNA microarray
#'
#' Process data from assay tab files with technology type DNA microarray
#' (ms). Processing those files requires the Biobase and affy packages to be
#' installed.
#'
#' @param isaObject An object of the \linkS4class{ISAjson}.
#' @param aTabObject An object of the \linkS4class{microarrayAssayTab}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISAjson",
                        aTabObject = "microarrayAssayTab",
                        type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("affy", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              ## Get microarray files for assay.
              microarrayDatFiles <- unique(assayDat[["dataFilename"]])
              microarrayDatFilesFull <-
                file.path(normalizePath(slot(aTabObject, "path"),
                                        winslash = .Platform$file.sep),
                          microarrayDatFiles)
              ## Check that files exist.
              missFiles <- microarrayDatFilesFull[!file.exists(microarrayDatFilesFull)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct meta data.
              aTabMIAME <- constructMIAMEMetadata(isaObject = isaObject,
                                                  aTabObject = aTabObject)
              ## Get expression set.
              ## justRMA only reads files from working directory.
              ## So change to that and reset when exiting function.
              wd <- getwd()
              on.exit(setwd(wd), add = TRUE)
              setwd(slot(aTabObject, "path"))
              xset <- affy::justRMA(filenames = microarrayDatFiles,
                                    #phenoData = assayDat,
                                    description = aTabMIAME)
              return(xset)
            } else {
              stop("For reading DNA microarray data the affy package ",
                   "should be installed.\n")
            }
          }
)

#' Helper function for construction MIAME meta data
#'
#' Helper function for construction of MIAME meta data for DNA microarray
#' assay tabs objects. MIAME is a class in the Biobase package for storing
#' MicroArray Experiment Information.
#'
#' @return An object of class \code{MIAME}.
#'
#' @noRd
#' @keywords internal
#' @importFrom utils person
constructMIAMEMetadata <- function(isaObject,
                                   aTabObject) {
  if (requireNamespace("Biobase")) {
    if (is(aTabObject, "microarrayAssayTab")) {
      assayDat <- slot(aTabObject, "aFile")
      sIdentifier <- names(slot(aTabObject, "sFilename"))
      sInfo <- slot(isaObject, "study")[[sIdentifier]]
      sContacts <- isaObject@sContacts[[sIdentifier]]
      ## Get corresponding author details.
      sCorr <- sContacts[sContacts[["roles"]] == "corresponding author", ]
      sCorrPers <- as.character(person(given = paste(sCorr[["fistName"]],
                                                     sCorr[["midInitials"]]),
                                       family = sCorr[["lastName"]]))
      aTabMIAME <- Biobase::MIAME(name = sIdentifier,
                                  lab = sCorr[["affiliation"]],
                                  contact = sCorrPers,
                                  title = "",
                                  abstract = sInfo[["Study Description"]],
                                  samples = as.list(assayDat["Sample Name"])
      )
      return(aTabMIAME)
    } else {
      stop("MIAME meta data can only be constructed for assays of type ",
           "DNA microarray.\n")
    }
  }
  stop("For constructing MIAME meta data the Biobase package ",
       "should be installed.\n")
}

