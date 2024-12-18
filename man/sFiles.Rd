% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R
\name{sFiles}
\alias{sFiles}
\alias{sFiles<-}
\title{Get and set sFiles.}
\usage{
sFiles(x)

sFiles(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISAjson}.}

\item{value}{A list of data.frames containing the study files.
In each data.frame at least the following columns are present:
`r paste0("'", sFilesCols, "'", collapse = ", ")`.}
}
\value{
A list of data.frames containing the study files.

The updated object of class \linkS4class{ISAjson}.
}
\description{
Get and set the list of sFiles data.frames in an object of
\linkS4class{ISAjson}.
}
