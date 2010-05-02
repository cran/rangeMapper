

\name{selectShpFiles}
\alias{selectShpFiles}


\title{Select (recursively) .shp files}
\description{
Returns the file path to all .shp files in a directory.
}
\usage{
selectShpFiles(dir, ogr = TRUE, polygons.only = TRUE)
}

\arguments{
	  \item{dir}{character string specifying the directory containing .shp files.}	
	  \item{ogr}{A logical value. 
	  If \sQuote{TRUE} a  \code{\link{data.frame}} with two columns (\code{dsn} and \code{layer}) is returned. 
	  If \sQuote{FALSE} a character vector containing the names of the files in the specified directories  will be returned.
	   }
	\item{polygons.only}{A logical value.
	  If \sQuote{TRUE} only polygon file paths are returned. If \sQuote{FALSE} the location of all shape files is returned.
	}}

\examples{
f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
selectShpFiles(f, ogr = FALSE, polygons.only = TRUE)
}
\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{misc}

