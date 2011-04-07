
\name{global.bbox}
\alias{global.bbox}
\alias{global.bbox.save}
\alias{global.bbox.fetch}




\title{Global bounding box}




\description{
Computes, sets or retrieves the global spatial bounding box.
}

\details{
	\link{global.bbox.save}{
		saves the global bounding box and the proj4 string to the \code{rangeMapper}'s \sQuote{metadata} table.}
	\link{global.bbox.fetch}{
		retrieves the global bounding box from a \code{rangeMapper}'project as as a \code{\link{SpatialPolygonsDataFrame}}. }
}


\arguments{
	  \item{con}{An \code{SQLiteConnection} object pointing to a \code{rangeMapper} project}
	  \item{...}{Other arguments to pass to the corresponding methods}
	}	

\usage{
	global.bbox.save(con, ...)
	global.bbox.fetch(con)
	}

	
\seealso{
	\code{\link{rangeMapper}} for a complete example.
	\code{\link{proj4string}}
	\code{\link{bbox}} 
	}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}
	
	
\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{spatial}

