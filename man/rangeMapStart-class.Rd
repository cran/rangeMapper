\name{rangeMapStart}
\alias{rangeMapStart}
\alias{rangeMapStart-class}
\alias{rangeMapStart-methods}
\alias{rangeMapStart,rangeMapStart-method}

\Rdversion{1.1}
\docType{class}

\title{Class "rangeMapStart"}
\description{
A class used to initiate a \code{rangeMapper} project.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rangeMapStart", ...)}.
}
\section{Slots}{
  \describe{
	\item{\code{dir}:}{Object of class \code{"character"}; the directory hosting the \code{rangeMapper} project }
	\item{\code{file}:}{Object of class \code{"character"} Name of the \code{rangeMapper} project; an *.sqlite file}
    \item{\code{skeleton}:}{Object of class \code{"list"} containing the SQL calls to initiate a rangeMapper project }
	\item{\code{overwrite}:}{Object of class \code{"logical"}; if \code{TRUE}  all the project's table are removed}
  }
}
\section{Methods}{
  \describe{
    \item{rangeMapStart}{\code{signature(object = "rangeMapStart")}: starts a new rangeMapper project }
	 }
}

\author{
Mihai Valcu, \email{valcu@orn.mpg.de}
}


\seealso{
  \code{\link{rangeMapper}} for a complete example.
 
  }

\keyword{classes}
