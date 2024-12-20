% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{aProcSeq}
\alias{aProcSeq}
\alias{aProcSeq<-}
\alias{aProcSeq,ISAjson-method}
\alias{aProcSeq<-,ISAjson-method}
\title{Get and set aProcSeq.}
\usage{
aProcSeq(x)

aProcSeq(x) <- value

\S4method{aProcSeq}{ISAjson}(x)

\S4method{aProcSeq}{ISAjson}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISAjson}.}

\item{value}{A list of data.frames containing the assay process sequences.
In each data.frame at least the following columns are present:
`r paste0("'", sProcSeqCols, "'", collapse = ", ")`.}
}
\value{
A list of data.frames containing the assay process sequences

The updated object of class \linkS4class{ISAjson}.
}
\description{
Get and set the list of aProcSeq data.frames in an object of
\linkS4class{ISAjson}.
}
