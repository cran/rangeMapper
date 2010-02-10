
\name{db.ini}
\alias{db.ini}
\alias{db.scheleton}


\title{Initiate a new project}
\description{
\code{db.ini} Initiates a new project by creating an empty database.\cr
\code{db.scheleton} Returns a list containng the sql code used by \code{db.ini} to generate the empty tables.
}

\arguments{
	  \item{con}{A valid sqlite connection.}
}

\usage{
db.ini(con)
db.scheleton()
}

\seealso{\code{\link{rangeMapper}} for a complete example.}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{misc}

