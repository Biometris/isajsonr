% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{study}
\alias{study}
\alias{study<-}
\alias{study,ISAjson-method}
\alias{study<-,ISAjson-method}
\title{Get and set study.}
\usage{
study(x)

study(x) <- value

\S4method{study}{ISAjson}(x)

\S4method{study}{ISAjson}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISAjson}.}

\item{value}{A list of data.frames containing the study information.
In each data.frame at least the following columns are present:
`r paste0("'", studyCols, "'", collapse = ", ")`.}
}
\value{
A list of data.frames containing the study information.

The updated object of class \linkS4class{ISAjson}.
}
\description{
Get and set the list of study data.frame in an object of
\linkS4class{ISAjson}.
}
