\name{gridSize}
\alias{gridSize}
\alias{gridSize.save}
\alias{gridSize.fetch}

\title{Save or retrieve the grid size from an sqlite database.}
\description{
Save or retrieve the grid size from the active sqlite database.
}
\usage{
gridSize.save(con, gridSize) 
gridSize.fetch(con)
}

\arguments{
	  \item{con}{A valid sqlite connection.}
	  \item{gridSize}{A numeric vector of one unit length.}
	}

\value{
	\item{\code{gridSize.fetch}}{Returns a numeric vector of one unit length containing the grid size previously saved by \code{gridSize.save}}

 
}	
	

\seealso{\code{\link{rangeMapper}} for a complete example.}
\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}