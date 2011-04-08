\name{gridSize.save}
\alias{gridSize.save}
\alias{gridSize.fetch}

\title{Save or retrieve the grid size from an \code{rangeMapper} project.}
\description{
Save or retrieve the grid size from the active sqlite database.
}
\usage{
gridSize.save(con, ...) 
gridSize.fetch(con)
}

\arguments{
	  \item{con}{A connection pointing to a valid \code{rangeMapper} project.}
	  \item{...}{Other arguments to pass to methods of class \code{\link{gridSize-class}} }
	  \item{gridSize}{A numeric vector of one unit length.}
	}

\value{
	\item{\code{gridSize.fetch}}{Returns a numeric vector of one unit length containing the grid size previously saved by \code{gridSize.save}}
}	


\seealso{
\code{\link[rangeMapper]{rangeMapper}} for a complete example.
\code{\link[rangeMapper]{global.bbox}}
}

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}









