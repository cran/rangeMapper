
\name{rangeMap.save}
\alias{rangeMap}
\alias{rangeMap.save}
\alias{rangeMap.fetch}
\alias{rangeMap.plot}
\title{Compute a chosen statistic at each canvas cell }
\description{
Compute a chosen statistic (richness, average, median or any user-defined statistic) at each grid cell and save/retrieve the data from the database.
\code{rangeMap.save} is basically an sql constructor; the sql query can be shown by changing \code{show.sql} argument to \code{FALSE}. In the case of simple functions like species richness (count) or average, \code{FUN} is computed by the database engine, otherwise \code{rangeMap.save} constructs the query and uses \code{\link{lapply}} to compute \code{FUN}. 
}

\usage{
rangeMap.save(dbcon, FUN = richness, biotab = NA, biotrait,table.nam, subset = NULL, show.sql = FALSE)
rangeMap.fetch(dbcon, map)
rangeMap.plot(map, colorpalette, ncols, style, scales = FALSE)
}

\arguments{
	  \item{dbcon}{A valid sqlite connection.}
	  \item{FUN}{the function to be applied to each pixel. If \code{FUN} is missing then species richness (species count) is computed.}
	  \item{biotab}{quoted name of the \sQuote{BIO} table previously saved by \code{\link{bio.save}} (without the \sQuote{BIO} preffix.)}
	  \item{biotrait}{A two sided formula to be passed to \code{\link{assemblage.stat}}. 
					The left side of the formula identifies the \sQuote{y} \sQuote{BIO} table variable  to be mapped . 
					The right side of the formula contains other variables in the \sQuote{BIO} table to be used by  \code{FUN} in the modelling process 
					or the constant 1 in which case FUN is applied directy on \sQuote{y}. 
					}
	  \item{table.nam}{Name of the table (quoted) to be added to the sqlite database. 
			The prefix \sQuote{MAP} will be appended to \code{table.nam} prior to saving.}
	 
	 \item{subset}{A named \code{\link{list}}. Names reffers to \sQuote{BIO}, \sQuote{MAP} and \sQuote{metadata_rages} table names while the objects in the list are character strings containing the SQL  \code{WHERE} or \code{HAVING} clause; see \code{\link{rangeMapper}} for a few examples.} 

	  	  
	  \item{show.sql}{A logical value. 
		If \sQuote{TRUE} then the sql code to be executed in the project's database is printed to the screeen. If \sQuote{FALSE}, the default, the MAP is saved to the project's database.
	   }
	  	  
	  \item{map}{Quoted name of the map table (without the \sQuote{MAP} prefix) to fetch from the sqlite database.} 
	  
	  \item{colorpalette}{A vector of colors; see also \code{\link{tkColorPalette}} which allows for interactive choice of color palettes}
	  
	  \item{ncols}{	number of color classes required; argument to be passed to \code{\link{classIntervals}} }
	  \item{style}{class interval style; see \code{\link{classIntervals}} for more details}
	  \item{scales}{If \sQuote{TRUE}, axes scale are drawn. This is an argument to be passed to \code{\link{spplot}} }
	  
	   
	  
	}
	

\value{
\item{\code{rangeMap.save}}{if not the \sQuote{MAP} table was not saved to the database then the SQL string is returned for inspection.}  

\item{\code{rangeMap.fetch}}{returns a \code{\link{SpatialPixelsDataFrame}} object.}  
 
}	
	
\seealso{\code{\link{rangeMapper}} for a complete example.}
\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}

