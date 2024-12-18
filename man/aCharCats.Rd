% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{aCharCats}
\alias{aCharCats}
\alias{aCharCats<-}
\alias{aCharCats,ISAjson-method}
\alias{aCharCats<-,ISAjson-method}
\title{Get and set aCharCats.}
\usage{
aCharCats(x)

aCharCats(x) <- value

\S4method{aCharCats}{ISAjson}(x)

\S4method{aCharCats}{ISAjson}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISAjson}.}

\item{value}{A list of data.frames containing the assay characteristic
categories. In each data.frame at least the following columns are present:
`r paste0("'", sCharCatsCols, "'", collapse = ", ")`.}
}
\value{
A list of data.frames containing the assay characteristic categories.

The updated object of class \linkS4class{ISAjson}.
}
\description{
Get and set the list of aCharCats data.frames in an object of
\linkS4class{ISAjson}.
}
