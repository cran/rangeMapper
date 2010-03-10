\name{processRanges}
\alias{processRanges}
\title{processRanges}
\description{
Each range (.shp file) is imported using \code{\link{readOGR}}, overlayed on the previously created \code{\link{canvas}} and the results are saved to the active project file.
}

\details{The overlay is performed using \code{\link{overlay}}. If the overlay returns no results (the polygon is smaller than a grid cell) then the centroid of the range will snap to the nearest point and only one grid cell will be returned for that range.
}
\arguments{
	  \item{con}{A valid sqlite connection.}
	  \item{Files}{range file location on disk as retrieved by \code{\link{selectShpFiles}}}
	  \item{metadata}{A logical value. If \sQuote{TRUE} ranges metadata: average range centroid and range extents are saved to \sQuote{metadata_ranges} table}
	}

\usage{
processRanges(Files, con, metadata)
}

 \section{Warning}{
If there are thousands of range map polygons with complex geometries to be processed and/or the canvas resolution is relatively high this step is going to be time consuming. For example processing all amphibians of the world (5816 ranges, total 252 MB) using a grid of 0.25 deg (\emph{unnecessary high}) resolution takes 544 minutes on a 64-bit machine with 4 GB of RAM running Windows Vista and R 2.10.0. However, once this step has been performed and the results saved to the database, the other operations are very fast.
 }


\seealso{
	\code{\link{rangeMapper}} for a complete example.
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

 \references{
Hurlbert, AH; Jetz, W (2007) Species richness, hotspots, and the scale dependence of range maps in ecology and conservation, PNAS, 13384-13389
}
 
\keyword{spatial}
\keyword{sqlite}





















