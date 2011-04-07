
\name{rangeMap.save}
\alias{rangeMap.save}



\title{Save a rangeMap}
\description{
Compute a chosen statistic (e.g. richness (count), mean, median or any user-defined statistic) at each grid cell.}

\arguments{
	  \item{CON}{An sqlite connection pointing to a valid \code{rangeMapper} project. }
	  \item{tableName}{Name of the table (quoted) to be added to the sqlite database. The prefix \sQuote{MAP} will be appended to \code{tableName} prior to saving.}
	  \item{FUN}{the function to be applied to each pixel. If \code{FUN} is missing then species richness (species count) is computed.}
	  \item{biotab}{character string identifying the \sQuote{BIO} table to use.}
	  \item{biotrait}{character string identifying the ID of the \sQuote{BIO} table. see \code{\link{bio.save}} }
	 \item{subset}{A named \code{\link{list}}. See details} 
	  \item{path}{Path to the raster file(quoted) to be imported to the existing project. \code{raster package} is required at this step.}			
	  \item{overwrite}{If \code{TRUE} then the table is removed}			
		
	\item{...}{When R is an R function, ... denotes any extra arguments to be passed to it.}
}


\usage{
rangeMap.save(CON, tableName, FUN, biotab, biotrait, subset, path , overwrite, ...)
}
	

\value{
Returns TRUE when the MAP was created successfully. 

}	

\details{
The subset argument accepts a named list. Names refers to \sQuote{BIO}, \sQuote{MAP} and \sQuote{metadata_rages} table names 
while the strings in the list are character strings containing the SQL  \code{WHERE} clause.
The subset can point to only one table type 
(e.g. \code{list(MAP_species_richness = "species_richness > 500")} )
or can point to several table types 
(e.g. \code{list(BIO_lifeHistory = "clutch_size > 4", MAP_meanAltitude = "meanAltitude < 1000",  metadata_ranges = "Area < 1000")} )

Any valid SQL expression can be used. See \url{http://www.sqlite.org/lang_expr.html }
}


\note{
SQL aggregate functions are bound to be faster then R functions. For simple aggregate functions like mean, median, sd, count it is advisable to use SQL functions rather then R functions. 

}



\seealso{
\code{\link{rangeMapper}} for a complete example.
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}



















