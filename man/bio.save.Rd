
\name{bio.save}
\alias{bio.save}
\title{Uploads 'bio' (e.g. life history data) to the active rangeMapper project.}
\description{
  Imports data to an sqlite database previosly formatted by \code{\link{db.ini}} as a rangeMaper project.
	}
\usage{
	bio.save(con, table_name = NULL, dat, common_id)
	}
\arguments{
	\item{con}{A valid sqlite connection.}
	\item{table_name}{Table to be added to the active project. If \code{table_name} is \code{NULL} then the name of \code{dat} is used.
					The prefix 	\sQuote{BIO} will be appended to \code{table_name} prior to saving.}
	\item{dat}{\code{dat} is a \code{data.frame} containing the \code{common_id} column.}
	\item{common_id}{This is the column corresponding to the names of the range files. This column will also get indexed in the sqlite database.}
}

\seealso{\code{\link{rangeMapper}} for a complete example.}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{import}

