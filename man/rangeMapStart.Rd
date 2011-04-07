
\name{rangeMap.start}
\alias{rangeMap.start}
\alias{rangeMap.open}

\title{Initiate/open a new rangeMapper project}

\description{
Initiate/open a new rangeMapper project using an object of class \code{\link{rangeMapStart-class}} 
}

\usage{
rangeMap.start(...)
rangeMap.open(path, verbose)
}

\arguments{
	  \item{path}{Character vector; a path to a valid rangeMapper project}
	  \item{verbose}{Character vector; if \code{TRUE} the project's summary is printed }
	  \item{...}{Arguments to be passed to \code{\link{rangeMapStart-class}} }
}

\value{
An sqlite connection is returned.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{rangeMapStart-class}} 
}


\keyword{spatial}

