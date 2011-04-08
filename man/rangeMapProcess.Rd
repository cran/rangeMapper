\name{processRanges}
\alias{processRanges}


\title{Process ranges}
\description{
Each polygon range is overlayed on the \code{\link[rangeMapper]{canvas}} and the results are saved to the active project file.
}

\details{The overlay is performed using \code{\link{overlay}}. If the overlay returns no results (i.e. the polygon is smaller than a grid cell) then the centroid of the range will snap to the nearest point and only one grid cell will be returned for that range.
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project.}
	  \item{dir}{ranges file directory}
	  \item{metadata}{A logical value. If \sQuote{TRUE} ranges metadata: range area, median range centroid and range extents (min, max) are saved to \sQuote{metadata_ranges} table}
	  \item{...}{One or several user defined functions applied on each range. The output of these functions will be appended to metadata_ranges table}
	}

\usage{
processRanges(dir,con,metadata = TRUE, ...)
}


 \section{Warning}{
If thousands of range map polygons are processed, their geometries are complex and/or the canvas resolution is relatively high this step can be time consuming. However, once this step has been performed and the results saved to the database, the other operations are very fast.
 }


\seealso{
	\code{\link{rangeMapper}} for a complete example.
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}

 
\keyword{spatial}






















