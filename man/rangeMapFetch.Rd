
\name{rangeMap.fetch }
\alias{rangeMap.fetch }



\title{Fetch \sQuote{MAP} tables}
\description{
Fetch \sQuote{MAP}s from a \code{rangeMapper} project.
}
	
\usage{
rangeMap.fetch(dbcon, maps)
}



\arguments{
	\item{dbcon}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	\item{maps}{A character vector containing  \sQuote{MAP} names. }

}

\value{
A \code{\link{SpatialPixelsRangeMap-class}} is returned. 

}


\seealso{
\code{\link{rangeMapper}} for a complete example.
\code{\link{SpatialPixelsRangeMap-class}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}


\author{ 
	Mihai Valcu \email{valcu@orn.mpg.de} }

\keyword{fetch}














