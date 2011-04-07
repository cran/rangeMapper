\name{rangeMapProcess}
\alias{rangeMapProcess}
\alias{rangeMapProcess-class}
\alias{rangeMapProcess-methods}
\alias{rangeMapProcess,rangeMapProcess-method}


\Rdversion{1.1}
\docType{class}

\title{Class "rangeMapProcess"}
\description{
The methods of this class allows importing vector (shp) range files to a \code{rangeMapper} project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapProcess", ...)}.
}
\section{Slots}{
  	\describe{
	\item{\code{metadata}:}{Object of class \code{"logical"}  Should the range structure parameters be extracted?}
	\item{\code{parallel}:}{Object of class \code{"character"} Not yet implemented. }

  }
}
\section{Extends}{
Class \code{"\linkS4class{rangeFiles}"}, directly.
Class \code{"\linkS4class{rangeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{rangeMapProcess}{\code{signature(object = "rangeMapProcess")}: ... }
	 }
}

\author{
Mihai Valcu \email{valcu@orn.mpg.de}
}


\seealso{
\code{\link[rangeMapper]{rangeMapper}} for a complete example. 
\code{\link[rangeMapper]{rangeMapProcess}}

}


\keyword{classes}
