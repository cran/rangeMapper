\name{rangeFiles-class}
\alias{rangeFiles-class}
\alias{rangeFiles-methods}
\alias{rangeFiles}
\alias{rangeFiles,rangeFiles-method}

\docType{class}


\title{Class "rangeFiles"}
\description{
A class used to handle polygon shp files.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeFiles", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dir}:}{
		Object of class \code{"character"}; directory containing the range files; one for each taxon (e.g. species) }
	\item{\code{ogr}:}{
		Object of class \code{"logical"} if \code{TRUE} a \code{data.frame} with two columns is returned
		(see \code{\link{selectShpFiles} for details)  } }
    \item{\code{polygons.only}:}{Object of class \code{"logical"} If \code{TRUE} only polygon file paths are returned, else the location to all shape files regardless their type is returned. }
  }
}
\section{Methods}{
  \describe{
    \item{rangeFiles}{\code{signature(object = "rangeFiles")}: initiate a new \code{rangeFiles} object }
	 }
}

\author{Mihai Valcu, \email{valcu@orn.mpg.de}}

\seealso{
  \code{\link{selectShpFiles}}
}

\keyword{classes}









