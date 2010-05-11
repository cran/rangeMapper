\name{canvas}
\alias{canvas}
\alias{canvas.make}
\alias{canvas.save}
\alias{canvas.fetch}


\title{Project's canvas}

\description{
	The canvas is a  regular grid of a given resolution used further by \code{\link{rangeMap}} function to perform grid interpolation.}

\usage{
canvas.make(con)
canvas.save(con)
canvas.fetch(con)
}

\arguments{
	  \item{con}{A valid sqlite connection.}
	}

\value{
\item{\code{canvas.make}}{returns a \code{\link{SpatialPoints}} object}  
\item{\code{canvas.save}}{saves the a \code{\link{SpatialPoints}} object returned by \code{canvas.make} to the project's data base}  
\item{\code{canvas.fetch}}{retrieves the canvas previously saved by \code{canvas.save} from the project's data base as a \code{\link{SpatialPoints}} object}  
}	
	

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	\code{\link{rangeMapper}} for a complete example.\cr
	\code{\link{gridSize}} 
	}

\keyword{spatial}
\keyword{sqlite}

