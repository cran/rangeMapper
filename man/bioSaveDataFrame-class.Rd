\name{bioSaveDataFrame-class}
\alias{bioSave-methods}
\alias{bioSave}
\alias{bioSave,bioSaveDataFrame-method}
\alias{bioSaveDataFrame-class}
\alias{bioSaveDataFrame-method}

\Rdversion{1.1}
\docType{class}

\title{Class "bioSaveDataFrame"}
\description{
The methods of this class allows importing \code{\link{data.frame}}s to a  \code{rangeMapper} project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bioSaveDataFrame", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{Object of class \code{"data.frame"}  }

  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeMapSave}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, by class "rangeMapSave", distance 2.
}
\section{Methods}{
  \describe{
    \item{bioSave}{\code{signature(object = "bioSaveDataFrame")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link{bio.save}}
}

\keyword{classes}
