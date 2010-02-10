\name{brewer.pal.get}
\alias{brewer.pal.get}
\title{
	Fetch all non-qualitative RColorBrewer's palettes.
	}
\description{
	This is a simple wrapper around \code{\link{brewer.pal.info}} returning a named list with all the available color palettes.
	}
\usage{
  brewer.pal.get(palette = NULL)
	}
\arguments{
  \item{palette}{
    If \code{palette} is \code{NULL}  the function returns a named list of palettes otherwise returns the given \code{palette} as a vector.
	}
}	

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
	See Also as \code{\link{brewer.pal.info}}
}
\examples{
	brewer.pal.get()
	brewer.pal.get("YlOrRd")
}
