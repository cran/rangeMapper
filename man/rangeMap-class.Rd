\name{rangeMap-class}
\docType{class}
\alias{rangeMap-class}
\alias{print.summary.rangeMap}
\alias{summary,rangeMap-method}
\alias{summary.rangeMap}


\title{Class "rangeMap"}
\description{
A class to formally define a rangeMapper project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMap", ...)}.
}

\section{Slots}{
  \describe{
	\item{\code{CON}:}{Object of class \code{"SQLiteConnection"} pointing to a \code{rangeMapper} project}
	\item{\code{VERSION}:}{Object of class \code{"character"} corresponding to the \code{rangeMapper} version}
	\item{\code{ID}:}{Object of class \code{"character"}; the ID column corresponding to the names of the imported range files. }
    \item{\code{BIOID}:}{Object of class \code{"character"} the ID column corresponding to the names of imported 	\sQuote{BIO} tables. }
	\item{\code{PROJ4STRING}:}{Object of class \code{"character"} the proj4 string }
	\item{\code{GRIDSIZE}:}{Object of class \code{"character"} grid size (in map units)}
	\item{\code{BBOX}:}{Object of class \code{"character"} bounding box}
    \item{\code{METADATA_RANGES}:}{Object of class \code{"character"} The name of a predefined table optionally constructed at the range import stage}
    \item{\code{CANVAS}:}{Object of class \code{"character"} The name of the table hosting the project's regular grid (the canvas) }
    \item{\code{RANGES}:}{Object of class \code{"character"} The name of the table hosting the ranges}
    \item{\code{BIO}:}{Object of class \code{"character"} The prefix for \sQuote{BIO} tables }
    \item{\code{MAP}:}{Object of class \code{"character"} The prefix for \sQuote{MAP} tables  }
  }
}



\section{Methods}{
  \describe{
    \item{canvasFetch}{\code{signature(object = "rangeMap")}:  }
    \item{canvasSave}{\code{signature(object = "rangeMap")}:  }
    \item{gridSizeFetch}{\code{signature(object = "rangeMap")}:  }
    \item{rangeMapBboxFetch}{\code{signature(object = "rangeMap")}:  }
	 }
}



\author{ Mihai Valcu, \email{valcu@orn.mpg.de}
}


\seealso{
  \code{\link{rangeMapStart-class}},
  \code{\link{gridSize-class}},
  \code{\link{rangeMapProcess-class}},
  \code{\link{rangeMapSave-class}},
  \code{\link{rangeMapFetch-class}},
  \code{\link{rangeMapRemove-class}},
  \code{\link{MapImport-class}}
}


\keyword{classes}




