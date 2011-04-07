\name{gridSize}
\alias{gridSize}
\alias{gridSize-class}
\alias{gridSize-methods}
\alias{gridSize,gridSize-method}

\alias{canvasSave}
\alias{canvasSave-methods}
\alias{canvasSave,canvasSave-method}
\alias{canvasSave,rangeMap-method}

\alias{canvasFetch}
\alias{canvasFetch-methods}
\alias{canvasFetch,canvasFetch-method}
\alias{canvasFetch,rangeMap-method}

\alias{gridSizeFetch}
\alias{gridSizeFetch-methods}
\alias{gridSizeFetch,gridSizeFetch-method}
\alias{gridSizeFetch,rangeMap-method}

\alias{gridSizeSave}
\alias{gridSizeSave-methods}
\alias{gridSizeSave,gridSizeSave-method}
\alias{gridSizeSave,gridSize-method}



\Rdversion{1.1}
\docType{class}

\title{Class "gridSize"}
\description{
Objects of class gridSize are used to set the pixel size and thus the canvas size of a \code{rangeMapper} project. If gridSize is not given a sensible default is set.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gridSize", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{gridSize}:}{Object of class \code{"numeric"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{gridSizeSave}{\code{signature(object = "gridSize")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}

\seealso{
	\code{\link[rangeMapper]{global.bbox}}
}

\keyword{classes}












