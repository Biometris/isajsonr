% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{sContacts}
\alias{sContacts}
\alias{sContacts<-}
\alias{sContacts,ISAjson-method}
\alias{sContacts<-,ISAjson-method}
\title{Get and set sContacts.}
\usage{
sContacts(x)

sContacts(x) <- value

\S4method{sContacts}{ISAjson}(x)

\S4method{sContacts}{ISAjson}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISAjson}.}

\item{value}{A list of data.frames containing the study contacts.
In each data.frame at least the following columns are present:
`r paste0("'", sContactsCols, "'", collapse = ", ")`.}
}
\value{
A list of data.frames containing the study contacts.

The updated object of class \linkS4class{ISAjson}.
}
\description{
Get and set the list of sContacts data.frames in an object of
\linkS4class{ISAjson}.
}
