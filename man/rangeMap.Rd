
\name{rangeMap}
\alias{rangeMap}
\alias{rangeMap.save}
\alias{rangeMap.fetch}
\title{Compute a chosen statistic at each canvas cell }
\description{
Compute a chosen statistic (richness, average, median or any user-defined statistic) at each grid cell and save/retrieve the data from the database.
}

\usage{
rangeMap.save(dbcon, FUN = NULL, biotab, biotrait, table.nam)
rangeMap.fetch(dbcon, map)
}

\arguments{
	  \item{dbcon}{A valid sqlite connection.}
	  \item{FUN}{the function to be applied to each pixel. If \code{FUN} is \code{NULL} then species richness (species count) is computed.}
	  \item{biotab}{quoted name of the \sQuote{BIO} table previously saved by \code{\link{bio.save}} (without the \sQuote{BIO} preffix.)}
	  \item{biotrait}{column name in the \sQuote{BIO} table identifying the variable to be mapped. The column name must be quoted. }
	  \item{table.nam}{Name of the table (quoted) to be added to the sqlite database. 
			The prefix \sQuote{MAP} will be appended to \code{table.nam} prior to saving.}	  
	  \item{map}{Quoted name of the map table (without the \sQuote{MAP} prefix) to fetch from the sqlite database.} 
	  
	}

\value{
\item{\code{rangeMap.fetch}}{returns a \code{\link{SpatialPixelsDataFrame}} object.}  
 
}	
	
\seealso{\code{\link{rangeMapper}} for a complete example.}
\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}

