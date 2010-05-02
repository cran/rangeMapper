
\name{rm.rangeMapper}
\alias{rm.rangeMapper}
\alias{rm.rangeMapper}


\title{Remove tables from a give project}
\description{Remove tables given prefix attribute or by name}

\arguments{
	  \item{con}{A valid sqlite connection.}
	  \item{table.nam}{quoted table name(without the MAP_ or BIO_ prefix); if missing all the tables with that prefix will be removed}	  
	  \item{table.type}{quoted table class as indicated by its prefix. Class MAP and BIO are available for the moment}
	  
	  
}

\usage{
rm.rangeMapper(con, table.nam, table.type) 
}

\seealso{\code{\link{rangeMapper}} for a complete example.}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{data base}

