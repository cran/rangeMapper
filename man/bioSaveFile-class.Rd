\name{bioSaveFile-class}
\alias{bioSaveFile-class}
\alias{bioSave,bioSaveFile-method}

\Rdversion{1.1}
\docType{class}

\title{Class "bioSaveFile"}
\description{
The methods of this class allows importing external tables to a  \code{rangeMapper} project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bioSaveFile", ...)}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bioSaveDataFrame", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{Object of class \code{"data.frame"}  }
    \item{\code{sep}:}{Object of class \code{"character"}  }

  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeMapSave}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, by class "rangeMapSave", distance 2.
}
\section{Methods}{
  \describe{
	\item{bioSave}{\code{signature(object = "bioSaveDataFrame")}  }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link{bio.save}}
}


\keyword{classes}
