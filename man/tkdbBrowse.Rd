\name{tkdbBrowse}
\alias{tkdbBrowse}

\title{
Browse an sqlite database.
}
\description{
Browse an sqlite database using a tcltk interface.
}
\usage{
tkdbBrowse(con, prefix = NULL, tables.name.only = FALSE, info)
}

\arguments{
  \item{con}{A valid sqlite connection.}
  \item{prefix}{Shows only tables with the given \code{prefix}}
  \item{tables.name.only}{If \sQuote{FALSE}, the default, shows only table names.}
   \item{info}{A character string containing a short text to be shown left to the OK button.}
	}
	
\author{Mihai Valcu \email{valcu@orn.mpg.de}}


\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session 
setwd(tempdir())
dbcon = dbConnect(dbDriver("SQLite"), dbname= "test.sqlite")
db.ini(dbcon)

# select one table and then press OK
tkdbBrowse(dbcon)
}
}

\keyword{database}
\keyword{tcltk}


