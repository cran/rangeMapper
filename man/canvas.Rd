\name{canvas.save}
\alias{canvas.save}
\alias{canvas.fetch}

\title{Project's canvas}

\description{
	The canvas is a  regular grid of a given resolution. Each range map is overlayed onto the canvas and the results saved to project.}

\usage{
canvas.save(con)
canvas.fetch(con)
}

\arguments{
	  \item{con}{An sqlite connection pointing to a valid \code{rangeMapper} project. }

}  

\value{
	\code{canvas.fetch} Returns a \code{\link[sp]{SpatialPixelsDataFrame}} object.
}

\note{
The method canvasSave() fails if \code{grid.size} was not set.
The method canvasSave() fails if the canvas was already constructed for the given project.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{gridSize.save}} 
	}
	
\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}
	

\keyword{spatial}
\keyword{sqlite}

